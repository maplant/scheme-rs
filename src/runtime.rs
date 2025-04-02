use crate::{
    cps::Cps,
    env::Local,
    exception::{Condition, ExceptionHandler},
    expand,
    gc::{init_gc, Gc, GcInner, Trace},
    lists::list_to_vec,
    num,
    proc::{
        deep_clone_value, Application, Closure, ClosurePtr, ContinuationPtr, DynamicWind, FuncPtr,
        FunctionDebugInfo,
    },
    syntax::Span,
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
use std::{collections::HashMap, mem::ManuallyDrop, ptr::null_mut};
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
    pub(crate) debug_info: DebugInfo,
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
            debug_info: DebugInfo::default(),
        }
    }
}

impl Gc<Runtime> {
    pub async fn compile_expr(&self, expr: Cps) -> Result<Closure, BuilderError> {
        self.compile_expr_with_env(expr, IndexMap::default()).await
    }

    pub async fn compile_expr_with_env(
        &self,
        expr: Cps,
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

#[derive(Trace, Clone, Debug, Default)]
pub struct DebugInfo {
    /// Location of function applications
    pub(crate) call_sites: Vec<Span>,
    /// Functions and their debug information
    pub(crate) function_debug_info: Vec<FunctionDebugInfo>,
}

pub type CallSiteId = u32;
pub type FunctionDebugInfoId = u32;

pub const IGNORE_CALL_SITE: CallSiteId = u32::MAX;
pub const IGNORE_FUNCTION: FunctionDebugInfoId = u32::MAX;

impl DebugInfo {
    pub fn new_call_site(&mut self, span: Span) -> CallSiteId {
        let id = self.call_sites.len();
        self.call_sites.push(span);
        id as CallSiteId
    }

    pub fn new_function_debug_info(
        &mut self,
        function_debug_info: FunctionDebugInfo,
    ) -> FunctionDebugInfoId {
        let id = self.function_debug_info.len();
        self.function_debug_info.push(function_debug_info);
        id as FunctionDebugInfoId
    }

    pub fn get_function_debug_info(
        &self,
        function_debug_info_id: FunctionDebugInfoId,
    ) -> Option<&FunctionDebugInfo> {
        if function_debug_info_id == IGNORE_FUNCTION {
            None
        } else {
            self.function_debug_info
                .get(function_debug_info_id as usize)
        }
    }
}

struct CompilationTask {
    env: IndexMap<Local, Gc<Value>>,
    compilation_unit: Cps,
    completion_tx: oneshot::Sender<CompilationResult>,
    /// Since Contexts are per-thread, we will only ever see the same Runtime. However,
    /// we can't cache the Runtime, as that would cause a ref cycle that would prevent
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

    //
    // fn alloc_undef_val:
    //
    let sig = ptr_type.fn_type(&[], false);
    let f = module.add_function("alloc_undef_val", sig, None);
    ee.add_global_mapping(&f, alloc_undef_val as usize);

    //
    // drop_values:
    //
    let sig = void_type.fn_type(&[ptr_type.into(), i32_type.into()], false);
    let f = module.add_function("drop_values", sig, None);
    ee.add_global_mapping(&f, drop_values as usize);

    //
    // apply:
    //
    let sig = ptr_type.fn_type(
        &[
            ptr_type.into(),
            ptr_type.into(),
            ptr_type.into(),
            i32_type.into(),
            ptr_type.into(),
            i32_type.into(),
        ],
        false,
    );
    let f = module.add_function("apply", sig, None);
    ee.add_global_mapping(&f, apply as usize);

    //
    // forward:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into(), ptr_type.into()], false);
    let f = module.add_function("forward", sig, None);
    ee.add_global_mapping(&f, forward as usize);

    //
    // halt:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("halt", sig, None);
    ee.add_global_mapping(&f, halt as usize);

    //
    // truthy:
    //
    let sig = bool_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("truthy", sig, None);
    ee.add_global_mapping(&f, truthy as usize);

    //
    // store:
    //
    let sig = void_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
    let f = module.add_function("store", sig, None);
    ee.add_global_mapping(&f, store as usize);

    //
    // make_continuation
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

    //
    // make_closure:
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
            i32_type.into(),
        ],
        false,
    );
    let f = module.add_function("make_closure", sig, None);
    ee.add_global_mapping(&f, make_closure as usize);

    //
    // get_call_transformer_fn:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("get_call_transformer_fn", sig, None);
    ee.add_global_mapping(&f, get_call_transformer_fn as usize);

    //
    // clone_environment:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("clone_closure", sig, None);
    ee.add_global_mapping(&f, clone_closure as usize);

    //
    // add:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("add", sig, None);
    ee.add_global_mapping(&f, add as usize);

    //
    // sub:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("sub", sig, None);
    ee.add_global_mapping(&f, sub as usize);

    //
    // mul:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("mul", sig, None);
    ee.add_global_mapping(&f, mul as usize);

    //
    // div:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("div", sig, None);
    ee.add_global_mapping(&f, div as usize);

    //
    // equal:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("equal", sig, None);
    ee.add_global_mapping(&f, equal as usize);

    //
    // greater:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("greater", sig, None);
    ee.add_global_mapping(&f, greater as usize);

    //
    // greater_equal:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("greater_equal", sig, None);
    ee.add_global_mapping(&f, greater_equal as usize);

    //
    // lesser:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("lesser", sig, None);
    ee.add_global_mapping(&f, lesser as usize);

    //
    // lesser_equal:
    //
    let sig = ptr_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("lesser_equal", sig, None);
    ee.add_global_mapping(&f, lesser_equal as usize);
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
unsafe extern "C" fn apply(
    runtime: *mut GcInner<Runtime>,
    op: *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    num_args: u32,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
    call_site_id: u32,
) -> *mut Result<Application, Condition> {
    let mut gc_args = Vec::new();
    for i in 0..num_args {
        gc_args.push(Gc::from_ptr(args.add(i as usize).read()));
    }

    let op = Gc::from_ptr(op);
    let op_ref = op.read();
    let op: &Closure = if let Ok(op) = op_ref.as_ref().try_into() {
        op
    } else {
        return Box::into_raw(Box::new(Err(Condition::invalid_operator_type(
            op_ref.type_name(),
        ))));
    };

    let exception_handler = if exception_handler.is_null() {
        None
    } else {
        Some(Gc::from_ptr(exception_handler))
    };

    let call_site = if call_site_id == u32::MAX {
        None
    } else {
        let runtime = Gc::from_ptr(runtime);
        let runtime_ref = runtime.read();
        Some(runtime_ref.debug_info.call_sites[call_site_id as usize].clone())
    };

    let app = Application::new(op.clone(), gc_args, exception_handler, dynamic_wind.as_ref().unwrap().clone(), call_site);

    Box::into_raw(Box::new(Ok(app)))
}

/// Create a boxed application that forwards a list of values to the operator
unsafe extern "C" fn forward(
    op: *mut GcInner<Value>,
    to_forward: *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    let op = Gc::from_ptr(op);
    let to_forward = Gc::from_ptr(to_forward);
    let mut args = Vec::new();
    list_to_vec(&to_forward, &mut args);
    let op_ref = op.read();
    let op: &Closure = if let Ok(op) = op_ref.as_ref().try_into() {
        op
    } else {
        return Box::into_raw(Box::new(Err(Condition::invalid_operator_type(
            op_ref.type_name(),
        ))));
    };

    let exception_handler = if exception_handler.is_null() {
        None
    } else {
        Some(Gc::from_ptr(exception_handler))
    };

    let app = Application::new(op.clone(), args, exception_handler, dynamic_wind.as_ref().unwrap().clone(), None);

    Box::into_raw(Box::new(Ok(app)))
}

/// Create a boxed application that simply returns its arguments
pub(crate) unsafe extern "C" fn halt(
    args: *mut GcInner<Value>,
) -> *mut Result<Application, Condition> {
    let args = Gc::from_ptr(args);
    let mut flattened = Vec::new();
    list_to_vec(&args, &mut flattened);

    let app = Application::halt(flattened);

    Box::into_raw(Box::new(Ok(app)))
}

/// Evaluate a `Gc<Value>` as "truthy" or not, as in whether it triggers a
/// conditional.
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
    fn_ptr: ContinuationPtr,
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
        FuncPtr::Continuation(fn_ptr),
        num_required_args as usize,
        variadic,
        None,
    );
    ManuallyDrop::new(Gc::new(Value::Closure(closure))).as_ptr()
}

/// Allocate a closure for a function that takes a continuation
unsafe extern "C" fn make_closure(
    runtime: *mut GcInner<Runtime>,
    fn_ptr: ClosurePtr,
    env: *const *mut GcInner<Value>,
    num_envs: u32,
    globals: *const *mut GcInner<Value>,
    num_globals: u32,
    num_required_args: u32,
    variadic: bool,
    debug_info_id: u32,
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
        FuncPtr::Closure(fn_ptr),
        num_required_args as usize,
        variadic,
        Some(debug_info_id),
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
        FuncPtr::Bridge(expand::call_transformer),
        3,
        true,
        Some(IGNORE_FUNCTION),
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

unsafe extern "C" fn add(
    vals: *const *mut GcInner<Value>,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> *mut GcInner<Value> {
    let vals: Vec<_> = (0..num_vals)
        .map(|i| Gc::from_ptr(vals.add(i as usize).read()))
        .collect();
    match num::add(&vals) {
        Ok(num) => ManuallyDrop::new(Gc::new(Value::Number(num))).as_ptr(),
        Err(condition) => {
            error.write(Box::into_raw(Box::new(Err(condition))));
            null_mut()
        }
    }
}

unsafe extern "C" fn sub(
    vals: *const *mut GcInner<Value>,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> *mut GcInner<Value> {
    let vals: Vec<_> = (0..num_vals)
        .map(|i| Gc::from_ptr(vals.add(i as usize).read()))
        .collect();
    match num::sub(&vals[0], &vals[1..]) {
        Ok(num) => ManuallyDrop::new(Gc::new(Value::Number(num))).as_ptr(),
        Err(condition) => {
            error.write(Box::into_raw(Box::new(Err(condition))));
            null_mut()
        }
    }
}

unsafe extern "C" fn mul(
    vals: *const *mut GcInner<Value>,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> *mut GcInner<Value> {
    let vals: Vec<_> = (0..num_vals)
        .map(|i| Gc::from_ptr(vals.add(i as usize).read()))
        .collect();
    match num::mul(&vals) {
        Ok(num) => ManuallyDrop::new(Gc::new(Value::Number(num))).as_ptr(),
        Err(condition) => {
            error.write(Box::into_raw(Box::new(Err(condition))));
            null_mut()
        }
    }
}

unsafe extern "C" fn div(
    vals: *const *mut GcInner<Value>,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> *mut GcInner<Value> {
    let vals: Vec<_> = (0..num_vals)
        .map(|i| Gc::from_ptr(vals.add(i as usize).read()))
        .collect();
    match num::div(&vals[0], &vals[1..]) {
        Ok(num) => ManuallyDrop::new(Gc::new(Value::Number(num))).as_ptr(),
        Err(condition) => {
            error.write(Box::into_raw(Box::new(Err(condition))));
            null_mut()
        }
    }
}

macro_rules! define_comparison_fn {
    ( $name:ident ) => {
        unsafe extern "C" fn $name(
            vals: *const *mut GcInner<Value>,
            num_vals: u32,
            error: *mut *mut Result<Application, Condition>,
        ) -> *mut GcInner<Value> {
            let vals: Vec<_> = (0..num_vals)
                .map(|i| Gc::from_ptr(vals.add(i as usize).read()))
                .collect();
            match num::$name(&vals) {
                Ok(res) => ManuallyDrop::new(Gc::new(Value::Boolean(res))).as_ptr(),
                Err(condition) => {
                    error.write(Box::into_raw(Box::new(Err(condition))));
                    null_mut()
                }
            }
        }
    };
}

define_comparison_fn!(equal);
define_comparison_fn!(greater);
define_comparison_fn!(greater_equal);
define_comparison_fn!(lesser);
define_comparison_fn!(lesser_equal);
