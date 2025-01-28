use std::{
    mem::ManuallyDrop,
    sync::{Mutex, OnceLock},
};

use crate::{
    cps::Cps, gc::{Gc, GcInner}, lists::list_to_vec, num::Number, proc::{Application, Closure, FuncPtr, SyncFuncPtr, SyncFuncWithContinuationPtr}, value::Value
};
use inkwell::{
    builder::BuilderError,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    targets::{InitializationConfig, Target},
    AddressSpace, OptimizationLevel,
};
use tokio::{
    sync::{mpsc, oneshot},
    task::JoinHandle,
};

struct CompilationBuffer {
    compilation_buffer_tx: mpsc::Sender<CompilationTask>,
    compilation_buffer_rx: Mutex<Option<mpsc::Receiver<CompilationTask>>>,
}

pub const MAX_COMPILATION_TASKS: usize = 5; // Idk

impl Default for CompilationBuffer {
    fn default() -> Self {
        let (compilation_buffer_tx, compilation_buffer_rx) = mpsc::channel(MAX_COMPILATION_TASKS);
        CompilationBuffer {
            compilation_buffer_tx,
            compilation_buffer_rx: Mutex::new(Some(compilation_buffer_rx)),
        }
    }
}

struct CompilationTask {
    completion_tx: oneshot::Sender<CompilationResult>,
    compilation_unit: Cps,
}

type CompilationResult = Result<Closure, BuilderError>;

static COMPILATION_QUEUE: OnceLock<CompilationBuffer> = OnceLock::new();
static COMPILATION_TASK: OnceLock<JoinHandle<()>> = OnceLock::new();

pub fn init_compiler() {
    let _ = COMPILATION_TASK.get_or_init(|| tokio::task::spawn_blocking(compilation_task));
}

pub async fn compile_cps(cps: Cps) -> Result<Closure, BuilderError> {
    let (completion_tx, completion_rx) = oneshot::channel();
    let task = CompilationTask {
        completion_tx,
        compilation_unit: cps,
    };
    COMPILATION_QUEUE
        .get_or_init(CompilationBuffer::default)
        .compilation_buffer_tx
        .send(task)
        .await
        .unwrap();
    // Wait for the compilation task to complete:
    completion_rx.await.unwrap()
}

fn compilation_task() {
    let mut compilation_queue_rx = COMPILATION_QUEUE
        .get_or_init(CompilationBuffer::default)
        .compilation_buffer_rx
        .lock()
        .unwrap()
        .take()
        .unwrap();

    Target::initialize_native(&InitializationConfig::default()).unwrap();

    // Create an LLVM context, module and execution engine. All of these should live for
    // the lifetime of the program.
    let context = Context::create();

    let mut modules = Vec::new();

    while let Some(task) = compilation_queue_rx.blocking_recv() {
        let CompilationTask {
            completion_tx,
            compilation_unit,
        } = task;

        // I don't really know a way to do this beyond just creating a new module every time.

        let module = context.create_module("scheme_rs");
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::default())
            .unwrap();
        let builder = context.create_builder();

        install_runtime(&context, &module, &execution_engine);

        let closure = compilation_unit.into_closure(&context, &module, &execution_engine, &builder);

        modules.push(module);

        let _ = completion_tx.send(closure);
    }
}

fn install_runtime<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>, ee: &ExecutionEngine<'ctx>) {
    let i64_type = ctx.i64_type();
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

    // fn i64_to_number(i64) -> *Value
    //
    let sig = ptr_type.fn_type(&[i64_type.into()], false);
    let f = module.add_function("i64_to_number", sig, None);
    ee.add_global_mapping(&f, i64_to_number as usize);

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

    // fn make_closure_with_continuation(
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
            i32_type.into(),
            ptr_type.into(),
            i32_type.into(),
            i32_type.into(),
            bool_type.into(),
        ],
        false,
    );
    let f = module.add_function("make_closure_with_continuation", sig, None);
    ee.add_global_mapping(&f, make_closure_with_continuation as usize);

    let sig = void_type.fn_type(&[ptr_type.into(), ptr_type.into(), ptr_type.into()], false);
    let f = module.add_function("dbg_args", sig, None);
    ee.add_global_mapping(&f, dbg_args as usize);
}

/// Allocate a new Gc with a value of undefined
unsafe extern "C" fn alloc_undef_val() -> *mut GcInner<Value> {
    let raw = ManuallyDrop::new(Gc::new(Value::Undefined)).as_ptr();
    raw
}

/// Decrement the reference count of all of the values
unsafe extern "C" fn drop_values(vals: *const *mut GcInner<Value>, num_vals: u32) {
    for i in 0..num_vals {
        Gc::drop_raw(vals.add(i as usize).read())
    }
}

/// Convert the i64 value into a Number and return it boxed
unsafe extern "C" fn i64_to_number(val: i64) -> *mut GcInner<Value> {
    ManuallyDrop::new(Gc::new(Value::Number(Number::from(val)))).as_ptr()
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
    let op: &Gc<Closure> = op_read.as_ref().try_into().unwrap();
    let app = Application::new(op.clone(), gc_args);

    Box::into_raw(Box::new(app))
}

/// Create a boxed application that simply returns its arguments
unsafe extern "C" fn make_return_values(
    args: *mut GcInner<Value>,
) -> *mut Application {
    let args = Gc::from_ptr(args);
    let mut flattened = Vec::new();
    list_to_vec(&args, &mut flattened);

    let app = Application::new_empty(flattened);

    Box::into_raw(Box::new(app))
}

/// Evaluate a Gc<Value> as "truthy" or not, as in whether it triggers a conditional.
unsafe extern "C" fn truthy(val: *mut GcInner<Value>) -> bool {
    dbg!(dbg!(Gc::from_ptr(val)).read().is_true())
}

/// Replace the value pointed to at to with the value contained in from.
unsafe extern "C" fn store(from: *mut GcInner<Value>, to: *mut GcInner<Value>) {
    let from = Gc::from_ptr(from);
    let to = Gc::from_ptr(to);
    let new_val = from.read().clone();
    *to.write() = new_val;
}

unsafe extern "C" fn make_closure(
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
        env,
        globals,
        FuncPtr::SyncFunc(fn_ptr),
        num_required_args as usize,
        variadic,
    );
    ManuallyDrop::new(Gc::new(Value::Closure(Gc::new(closure)))).as_ptr()
}

unsafe extern "C" fn make_closure_with_continuation(
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
        env,
        globals,
        FuncPtr::SyncFuncWithContinuation(fn_ptr),
        num_required_args as usize,
        variadic,
    );
    ManuallyDrop::new(Gc::new(Value::Closure(Gc::new(closure)))).as_ptr()
}

unsafe extern "C" fn dbg_args(
    env: *const *mut GcInner<Value>,
    globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
) {
    println!("env: {env:p}");
    println!("globals: {globals:p}");
    println!("args: {args:p}");
}
