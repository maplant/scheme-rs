use crate::{
    cps::TopLevelExpr,
    env::Local,
    expand,
    gc::{init_gc, Gc, GcInner, Trace},
    lists::list_to_vec,
    proc::{
        deep_clone_value, Application, Closure, FuncPtr, SyncFuncPtr, SyncFuncWithContinuationPtr,
    },
    value::Value,
};
use indexmap::IndexMap;
use inkwell::{
    builder::BuilderError,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    targets::{InitializationConfig, Target},
    AddressSpace, OptimizationLevel,
};
use std::{collections::HashMap, mem::ManuallyDrop};
use tokio::sync::{mpsc, oneshot};

/// Scheme-rs Runtime
///
/// # Safety:
///
/// The runtime contains the only live references to the LLVM Context and therefore
/// modules and allocated functions in the form a Sender of compilation tasks.
///
/// When that sender's ref count is zero, it will cause the receiver to fail and the
/// compilation task will exit, allowing for a graceful shutdown.
///
/// However, this is dropping a lifetime. If we clone a closure and drop the runtime
/// from whence it was cleaved, we're left with a dangling pointer.
///
/// In order to remedy this it is vitally important the closure has a back pointer to
/// the runtime. Probably also want to make it immutable
#[derive(Trace, Clone, Debug)]
pub struct Runtime {
    compilation_buffer_tx: mpsc::Sender<CompilationTask>,
}

const MAX_COMPILATION_TASKS: usize = 5; // Shrug

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Runtime {
    pub fn new() -> Self {
        init_gc();
        let (compilation_buffer_tx, compilation_buffer_rx) = mpsc::channel(MAX_COMPILATION_TASKS);
        // According the inkwell (and therefore LLVM docs), one LlvmContext may be
        // present per thread. Thus, we spawn a new thread and a  new compilation
        // task for every Runtime.
        std::thread::spawn(move || compilation_task(compilation_buffer_rx));
        Runtime {
            compilation_buffer_tx,
        }
    }
}

impl Gc<Runtime> {
    pub async fn compile_expr(&self, expr: TopLevelExpr) -> Result<Closure, BuilderError> {
        self.compile_expr_with_env(expr, IndexMap::default()).await
    }

    pub async fn compile_expr_with_env(
        &self,
        expr: TopLevelExpr,
        env: IndexMap<Local, Gc<Value>>,
    ) -> Result<Closure, BuilderError> {
        let (completion_tx, completion_rx) = oneshot::channel();
        let task = CompilationTask {
            env,
            completion_tx,
            compilation_unit: expr,
            runtime: self.clone(),
        };
        let sender = { self.read().compilation_buffer_tx.clone() };
        sender.send(task).await.unwrap();
        // Wait for the compilation task to complete:
        completion_rx.await.unwrap()
    }
}

struct CompilationTask {
    env: IndexMap<Local, Gc<Value>>,
    compilation_unit: TopLevelExpr,
    completion_tx: oneshot::Sender<CompilationResult>,
    /// Since Contexts are per-thread, we will only ever see the same Runtime. However,
    /// we can't cache the Runtime, as that would cause a live cycle that would prevent
    /// the last compilation buffer sender to drop. Therefore, its lifetime is that of
    /// the compilation task
    runtime: Gc<Runtime>,
}

type CompilationResult = Result<Closure, BuilderError>;

fn compilation_task(mut compilation_queue_rx: mpsc::Receiver<CompilationTask>) {
    Target::initialize_native(&InitializationConfig::default()).unwrap();

    // Create an LLVM context, module and execution engine. All of these should live for
    // the lifetime of the program.
    let context = Context::create();

    let mut modules = Vec::new();

    while let Some(task) = compilation_queue_rx.blocking_recv() {
        let CompilationTask {
            env,
            completion_tx,
            compilation_unit,
            runtime,
        } = task;

        // I don't really know a way to do this beyond just creating a new module every time.

        let module = context.create_module("scheme_rs");
        ExecutionEngine::link_in_mc_jit();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::default())
            .unwrap();
        let builder = context.create_builder();

        install_runtime(&context, &module, &execution_engine);

        let closure = compilation_unit.into_closure(
            runtime,
            env,
            &context,
            &module,
            &execution_engine,
            &builder,
        );

        modules.push(module);

        let _ = completion_tx.send(closure);
    }
}

fn install_runtime<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>, ee: &ExecutionEngine<'ctx>) {
    let i32_type = ctx.i32_type();
    let bool_type = ctx.bool_type();
    let void_type = ctx.void_type();
    let ptr_type = ctx.ptr_type(AddressSpace::default());

    // fn alloc_undef_val() -> *Value
    //
    let sig = ptr_type.fn_type(&[], false);
    let f = module.add_function("alloc_undef_val", sig, None);
    ee.add_global_mapping(&f, alloc_undef_val as usize);

    // fn drop_values(values: **Value, num_values: u32)
    //
    let sig = void_type.fn_type(&[ptr_type.into(), i32_type.into()], false);
    let f = module.add_function("drop_values", sig, None);
    ee.add_global_mapping(&f, drop_values as usize);

    // fn make_application(op: *Value, args: **Value, num_args: u32) -> *Application
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into(), i32_type.into()], false);
    let f = module.add_function("make_application", sig, None);
    ee.add_global_mapping(&f, make_application as usize);

    // fn make_return_values(args: *Value) -> *Application
    //
    let sig = ptr_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("make_return_values", sig, None);
    ee.add_global_mapping(&f, make_return_values as usize);

    // fn truthy(val: *Value) -> bool
    //
    let sig = bool_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("truthy", sig, None);
    ee.add_global_mapping(&f, truthy as usize);

    // fn store(from: *Value, to: *Value);
    //
    let sig = void_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
    let f = module.add_function("store", sig, None);
    ee.add_global_mapping(&f, store as usize);

    // fn make_closure(
    //         runtime: *Runtime,
    //         fn_ptr: SyncFuncPtr
    //         env: **Value,
    //         num_envs: u32,
    //         globals: **Value,
    //         num_globals: u32,
    //         num_required_args: u32,
    //         variadic: bool,
    // ) -> *Value
    //
    let sig = ptr_type.fn_type(
        &[
            ptr_type.into(),
            ptr_type.into(),
            ptr_type.into(),
            i32_type.into(),
            ptr_type.into(),
            i32_type.into(),
            i32_type.into(),
            bool_type.into(),
        ],
        false,
    );
    let f = module.add_function("make_continuation", sig, None);
    ee.add_global_mapping(&f, make_continuation as usize);

    // fn make_closure_with_continuation(
    //         runtime: *Runtime,
    //         fn_ptr: SyncFuncPtr
    //         env: **Value,
    //         num_envs: u32,
    //         globals: **Value,
    //         num_globals: u32,
    //         num_required_args: u32,
    //         variadic: bool,
    // ) -> *Value
    //
    let sig = ptr_type.fn_type(
        &[
            ptr_type.into(),
            ptr_type.into(),
            ptr_type.into(),
            i32_type.into(),
            ptr_type.into(),
            i32_type.into(),
            i32_type.into(),
            bool_type.into(),
        ],
        false,
    );
    let f = module.add_function("make_closure", sig, None);
    ee.add_global_mapping(&f, make_closure as usize);

    // fn get_call_transformer(runtime: *Runtime) -> *Value
    //
    let sig = ptr_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("get_call_transformer_fn", sig, None);
    ee.add_global_mapping(&f, get_call_transformer_fn as usize);

    // fn clone_environment(closure: *Value) -> ()
    //
    let sig = ptr_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("clone_closure", sig, None);
    ee.add_global_mapping(&f, clone_closure as usize);
}

/// Allocate a new Gc with a value of undefined
unsafe extern "C" fn alloc_undef_val() -> *mut GcInner<Value> {
    ManuallyDrop::new(Gc::new(Value::Undefined)).as_ptr()
}

/// Decrement the reference count of all of the values
unsafe extern "C" fn drop_values(vals: *const *mut GcInner<Value>, num_vals: u32) {
    for i in 0..num_vals {
        Gc::drop_raw(vals.add(i as usize).read())
    }
}

/// Create a boxed application
/// TODO: Take error handler as argument, return application with error handler
/// if operator is not a closure.
unsafe extern "C" fn make_application(
    op: *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    num_args: u32,
) -> *mut Application {
    let mut gc_args = Vec::new();
    for i in 0..num_args {
        gc_args.push(Gc::from_ptr(args.add(i as usize).read()));
    }

    let op = Gc::from_ptr(op);
    let op_read = op.read();
    let op: &Closure = op_read.as_ref().try_into().unwrap();
    let app = Application::new(op.clone(), gc_args);

    Box::into_raw(Box::new(app))
}

/// Create a boxed application that simply returns its arguments
pub(crate) unsafe extern "C" fn make_return_values(args: *mut GcInner<Value>) -> *mut Application {
    let args = Gc::from_ptr(args);
    let mut flattened = Vec::new();
    list_to_vec(&args, &mut flattened);

    let app = Application::new_empty(flattened);

    Box::into_raw(Box::new(app))
}

/// Evaluate a Gc<Value> as "truthy" or not, as in whether it triggers a conditional.
unsafe extern "C" fn truthy(val: *mut GcInner<Value>) -> bool {
    Gc::from_ptr(val).read().is_true()
}

/// Replace the value pointed to at to with the value contained in from.
unsafe extern "C" fn store(from: *mut GcInner<Value>, to: *mut GcInner<Value>) {
    let from = Gc::from_ptr(from);
    let to = Gc::from_ptr(to);
    let new_val = from.read().clone();
    *to.write() = new_val;
}

/// Allocate a closure
unsafe extern "C" fn make_continuation(
    runtime: *mut GcInner<Runtime>,
    fn_ptr: SyncFuncPtr,
    env: *const *mut GcInner<Value>,
    num_envs: u32,
    globals: *const *mut GcInner<Value>,
    num_globals: u32,
    num_required_args: u32,
    variadic: bool,
) -> *mut GcInner<Value> {
    // Collect the environment:
    let env: Vec<_> = (0..num_envs)
        .map(|i| Gc::from_ptr(env.add(i as usize).read()))
        .collect();

    // Collect the globals:
    let globals: Vec<_> = (0..num_globals)
        .map(|i| {
            let raw = globals.add(i as usize).read();
            Gc::from_ptr(raw)
        })
        .collect();

    let closure = Closure::new(
        Gc::from_ptr(runtime),
        env,
        globals,
        FuncPtr::SyncFunc(fn_ptr),
        num_required_args as usize,
        variadic,
        true,
    );
    ManuallyDrop::new(Gc::new(Value::Closure(closure))).as_ptr()
}

/// Allocate a closure for a function that takes a continuation
unsafe extern "C" fn make_closure(
    runtime: *mut GcInner<Runtime>,
    fn_ptr: SyncFuncWithContinuationPtr,
    env: *const *mut GcInner<Value>,
    num_envs: u32,
    globals: *const *mut GcInner<Value>,
    num_globals: u32,
    num_required_args: u32,
    variadic: bool,
) -> *mut GcInner<Value> {
    // Collect the environment:
    let env: Vec<_> = (0..num_envs)
        .map(|i| Gc::from_ptr(env.add(i as usize).read()))
        .collect();

    // Collect the globals:
    let globals: Vec<_> = (0..num_globals)
        .map(|i| {
            let raw = globals.add(i as usize).read();
            Gc::from_ptr(raw)
        })
        .collect();

    let closure = Closure::new(
        Gc::from_ptr(runtime),
        env,
        globals,
        FuncPtr::SyncFuncWithContinuation(fn_ptr),
        num_required_args as usize,
        variadic,
        false,
    );
    ManuallyDrop::new(Gc::new(Value::Closure(closure))).as_ptr()
}

/// Call a transformer with the given argument and return the expansion
unsafe extern "C" fn get_call_transformer_fn(
    runtime: *mut GcInner<Runtime>,
) -> *mut GcInner<Value> {
    let closure = Closure::new(
        Gc::from_ptr(runtime),
        Vec::new(),
        Vec::new(),
        FuncPtr::AsyncFunc(expand::call_transformer),
        3,
        true,
        false,
    );
    ManuallyDrop::new(Gc::new(Value::Closure(closure))).as_ptr()
}

/// Clone the values in a closure's environment.
///
/// This is done so poorly and is extremely slow. Need to fix!!
unsafe extern "C" fn clone_closure(closure: *mut GcInner<Value>) -> *mut GcInner<Value> {
    let closure = Gc::from_ptr(closure);
    let mut cloned = HashMap::new();
    ManuallyDrop::new(deep_clone_value(&closure, &mut cloned)).as_ptr()
}
