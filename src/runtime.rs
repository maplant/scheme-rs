use crate::{
    cps::Cps,
    env::Local,
    exception::{Condition, ExceptionHandler},
    expand,
    gc::{Gc, GcInner, Trace, init_gc},
    lists::{self, list_to_vec},
    num,
    proc::{
        Application, Closure, ContinuationPtr, DynamicWind, FuncDebugInfo, FuncPtr, UserPtr,
        clone_continuation_env,
    },
    symbols::Symbol,
    syntax::Span,
    value::{ReflexiveValue, UnpackedValue, Value},
    vectors,
};
use indexmap::IndexMap;
use inkwell::{
    OptimizationLevel,
    builder::BuilderError,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    targets::{InitializationConfig, Target},
};
use scheme_rs_macros::runtime_fn;
use std::{
    collections::{HashMap, HashSet},
    mem::ManuallyDrop,
    sync::Arc,
};
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
    // TODO: Make this something better than just a vec
    pub(crate) constants_pool: HashSet<ReflexiveValue>,
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
        // Ensure the GC is initialized:
        init_gc();
        let (compilation_buffer_tx, compilation_buffer_rx) = mpsc::channel(MAX_COMPILATION_TASKS);
        // According the inkwell (and therefore LLVM docs), one LlvmContext may
        // be present per thread. Thus, we spawn a new thread and a new
        // compilation task for every Runtime:
        std::thread::spawn(move || compilation_task(compilation_buffer_rx));
        Runtime {
            compilation_buffer_tx,
            constants_pool: HashSet::new(),
            debug_info: DebugInfo::default(),
        }
    }
}

impl Gc<Runtime> {
    pub async fn compile_expr(&self, expr: Cps) -> Result<Gc<Closure>, BuilderError> {
        self.compile_expr_with_env(expr, IndexMap::default()).await
    }

    pub async fn compile_expr_with_env(
        &self,
        expr: Cps,
        env: IndexMap<Local, Gc<Value>>,
    ) -> Result<Gc<Closure>, BuilderError> {
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
    /// Stored locations:
    stored_spans: Vec<Arc<Span>>,
    /// Stored user function debug information:
    stored_func_info: Vec<Arc<FuncDebugInfo>>,
}

impl DebugInfo {
    pub fn store_span(&mut self, span: Arc<Span>) {
        self.stored_spans.push(span);
    }

    pub fn store_func_info(&mut self, debug_info: Arc<FuncDebugInfo>) {
        self.stored_func_info.push(debug_info);
    }
}

struct CompilationTask {
    env: IndexMap<Local, Gc<Value>>,
    compilation_unit: Cps,
    completion_tx: oneshot::Sender<CompilationResult>,
    /// Since Contexts are per-thread, we will only ever see the same Runtime.
    /// However, we can't cache the Runtime, as that would cause a ref cycle
    /// that would prevent the last compilation buffer sender to drop.
    /// Therefore, its lifetime is that of the compilation task
    runtime: Gc<Runtime>,
}

type CompilationResult = Result<Gc<Closure>, BuilderError>;

fn compilation_task(mut compilation_queue_rx: mpsc::Receiver<CompilationTask>) {
    Target::initialize_native(&InitializationConfig::default()).unwrap();

    // Create an LLVM context, module and execution engine. All of these should
    // live for the maximum lifetime of the generated functions. This contains
    // all of the allocated memory for the functions.
    let context = Context::create();

    // By storing all of the debug information in the same lifetime as the
    // Context, we can directly put pointers referencing the debug information
    // in our JIT compiled functions:
    let mut debug_info = DebugInfo::default();

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

        let closure = compilation_unit
            .into_closure(
                runtime,
                env,
                &context,
                &module,
                &execution_engine,
                &builder,
                &mut debug_info,
            )
            .map(Gc::new);

        modules.push(module);

        let _ = completion_tx.send(closure);
    }
}

struct RuntimeFn {
    install: for<'ctx> fn(&'ctx Context, module: &Module<'ctx>, ee: &ExecutionEngine<'ctx>),
}

impl RuntimeFn {
    const fn new(
        install: for<'ctx> fn(&'ctx Context, module: &Module<'ctx>, ee: &ExecutionEngine<'ctx>),
    ) -> Self {
        Self { install }
    }
}

inventory::collect!(RuntimeFn);

fn install_runtime<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>, ee: &ExecutionEngine<'ctx>) {
    for runtime_fn in inventory::iter::<RuntimeFn> {
        (runtime_fn.install)(ctx, module, ee);
    }
}

unsafe fn arc_from_ptr<T>(ptr: *const T) -> Option<Arc<T>> {
    unsafe {
        if ptr.is_null() {
            return None;
        }
        Arc::increment_strong_count(ptr);
        Some(Arc::from_raw(ptr))
    }
}

/// Allocate a new Gc with a value of undefined
#[runtime_fn]
unsafe extern "C" fn alloc_cell() -> *mut GcInner<Value> {
    Gc::into_raw(Gc::new(Value::undefined()))
}

/// Read the value of a Cell
#[runtime_fn]
unsafe extern "C" fn read_cell(cell: *mut GcInner<Value>) -> i64 {
    unsafe {
        // We do not need to increment the reference count of the cell, it is going to
        // be decremented at the end of this function.
        let cell = ManuallyDrop::new(Gc::from_raw(cell));
        let cell_read = cell.read();
        let raw = Value::as_raw(&cell_read);
        raw as i64
    }
}

/// Decrement the reference count of a cell
#[runtime_fn]
unsafe extern "C" fn dropc(cell: *mut GcInner<Value>) {
    unsafe { Gc::decrement_reference_count(cell) }
}

/// Decrement the reference count of a value
#[runtime_fn]
unsafe extern "C" fn dropv(val: i64) {
    unsafe { drop(Value::from_raw(val as u64)) }
}

/// Create a boxed application
/// TODO: Take error handler as argument, return application with error handler
/// if operator is not a closure.
#[runtime_fn]
unsafe extern "C" fn apply(
    op: i64,
    args: *const i64,
    num_args: u32,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
    span: *const Span,
) -> *mut Result<Application, Condition> {
    unsafe {
        let args: Vec<_> = (0..num_args)
            .map(|i| Value::from_raw_inc_rc(args.add(i as usize).read() as u64))
            .collect();

        let op = match Value::from_raw_inc_rc(op as u64).unpack() {
            UnpackedValue::Closure(op) => op,
            x => {
                return Box::into_raw(Box::new(Err(Condition::invalid_operator_type(
                    x.type_name(),
                ))));
            }
        };

        let app = Application::new(
            op,
            args,
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            arc_from_ptr(span),
        );

        Box::into_raw(Box::new(Ok(app)))
    }
}

/// Create a boxed application that forwards a list of values to the operator
#[runtime_fn]
unsafe extern "C" fn forward(
    op: i64,
    args: i64,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    unsafe {
        let op = match Value::from_raw_inc_rc(op as u64).unpack() {
            UnpackedValue::Closure(op) => op,
            x => {
                return Box::into_raw(Box::new(Err(Condition::invalid_operator_type(
                    x.type_name(),
                ))));
            }
        };

        // We do not need to increment to forward here, for the same reason as in
        // halt.
        let args = ManuallyDrop::new(Value::from_raw(args as u64));
        let mut flattened = Vec::new();
        list_to_vec(&args, &mut flattened);

        let app = Application::new(
            op,
            flattened,
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            None,
        );

        Box::into_raw(Box::new(Ok(app)))
    }
}

/// Create a boxed application that simply returns its arguments
#[runtime_fn]
pub(crate) unsafe extern "C" fn halt(args: i64) -> *mut Result<Application, Condition> {
    unsafe {
        // We do not need to increment the rc here, it will be incremented in list_to_vec
        let args = ManuallyDrop::new(Value::from_raw(args as u64));
        let mut flattened = Vec::new();
        list_to_vec(&args, &mut flattened);
        let app = Application::halt(flattened);
        Box::into_raw(Box::new(Ok(app)))
    }
}

/// Evaluate a `Gc<Value>` as "truthy" or not, as in whether it triggers a
/// conditional.
#[runtime_fn]
unsafe extern "C" fn truthy(val: i64) -> bool {
    unsafe {
        // No need to increment the reference count here:
        ManuallyDrop::new(Value::from_raw(val as u64)).is_true()
    }
}

/// Replace the value pointed to at to with the value contained in from.
#[runtime_fn]
unsafe extern "C" fn store(from: i64, to: *mut GcInner<Value>) {
    unsafe {
        // We do not need to increment the ref count for to, it is dropped
        // immediately.
        let from = Value::from_raw_inc_rc(from as u64);
        let to = ManuallyDrop::new(Gc::from_raw(to));
        *to.write() = from;
    }
}

/// Allocate a closure
#[runtime_fn]
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
    unsafe {
        // Collect the environment:
        let env: Vec<_> = (0..num_envs)
            .map(|i| Gc::from_raw_inc_rc(env.add(i as usize).read()))
            .collect();

        // Collect the globals:
        let globals: Vec<_> = (0..num_globals)
            .map(|i| {
                let raw = globals.add(i as usize).read();
                Gc::from_raw_inc_rc(raw)
            })
            .collect();

        let closure = Closure::new(
            Gc::from_raw_inc_rc(runtime),
            env,
            globals,
            FuncPtr::Continuation(fn_ptr),
            num_required_args as usize,
            variadic,
            None,
        );

        Gc::into_raw(Gc::new(Value::from(closure)))
    }
}

/// Allocate a closure for a function that takes a continuation
#[runtime_fn]
unsafe extern "C" fn make_user(
    runtime: *mut GcInner<Runtime>,
    fn_ptr: UserPtr,
    env: *const *mut GcInner<Value>,
    num_envs: u32,
    globals: *const *mut GcInner<Value>,
    num_globals: u32,
    num_required_args: u32,
    variadic: bool,
    debug_info: *const FuncDebugInfo,
) -> *mut GcInner<Value> {
    unsafe {
        // Collect the environment:
        let env: Vec<_> = (0..num_envs)
            .map(|i| Gc::from_raw_inc_rc(env.add(i as usize).read()))
            .collect();

        // Collect the globals:
        let globals: Vec<_> = (0..num_globals)
            .map(|i| {
                let raw = globals.add(i as usize).read();
                Gc::from_raw_inc_rc(raw)
            })
            .collect();

        let closure = Closure::new(
            Gc::from_raw_inc_rc(runtime),
            env,
            globals,
            FuncPtr::User(fn_ptr),
            num_required_args as usize,
            variadic,
            arc_from_ptr(debug_info),
        );

        Gc::into_raw(Gc::new(Value::from(closure)))
    }
}

/// Call a transformer with the given argument and return the expansion
#[runtime_fn]
unsafe extern "C" fn get_call_transformer_fn(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    num_envs: u32,
) -> *mut GcInner<Value> {
    unsafe {
        // Collect the environment:
        let env: Vec<_> = (0..num_envs)
            .map(|i| Gc::from_raw_inc_rc(env.add(i as usize).read()))
            .collect();

        let closure = Closure::new(
            Gc::from_raw_inc_rc(runtime),
            env,
            Vec::new(),
            FuncPtr::Bridge(expand::call_transformer),
            3,
            false,
            None,
        );

        Gc::into_raw(Gc::new(Value::from(closure)))
    }
}

/// Return an error in the case that a value is undefined
#[runtime_fn]
unsafe extern "C" fn unbound_variable(symbol: u32) -> *mut Result<Application, Condition> {
    let sym = Symbol(symbol);
    Box::into_raw(Box::new(Err(Condition::error(format!("{sym} is unbound")))))
}

/*
/// Create a pair of the two provided values.
unsafe extern "C" fn cons(
    head: *mut GcInner<Value>,
    tail: *mut GcInner<Value>,
) -> *mut GcInner<Value> {
    let head = Gc::from_ptr(head);
    let tail = Gc::from_ptr(tail);
    ManuallyDrop::new(Gc::new(Value::Pair(head, tail))).as_ptr()
}
*/

/// Extract the current winders from the environment and return them as a vec.
/// This has to return a Gc since this is added to the environment of the continuation.
#[runtime_fn]
unsafe extern "C" fn extract_winders(dynamic_wind: *const DynamicWind) -> *mut GcInner<Value> {
    unsafe {
        let dynamic_wind = dynamic_wind.as_ref().unwrap();
        let winders: Vec<_> = dynamic_wind
            .winders
            .iter()
            .cloned()
            .map(|(in_winder, out_winder)| {
                Value::from((Value::from(in_winder), Value::from(out_winder)))
            })
            .collect();
        Gc::into_raw(Gc::new(Value::from(winders)))
    }
}

/// Prepare the continuation for call/cc. Clones the continuation environment
/// and creates a closure that calls the appropriate winders.
///
/// Expects that the continuation and winders will be provided in the form of a
/// pair of the continuation and vector of pairs of in/out winders.
#[runtime_fn]
unsafe extern "C" fn prepare_continuation(
    cont: i64,
    winders: i64,
    from_dynamic_extent: *const DynamicWind,
) -> i64 {
    unsafe {
        // Determine which winders we will need to call. This is determined as the
        // winders provided in cont_and_winders with the prefix of curr_dynamic_wind
        // removed.
        let cont = Value::from_raw_inc_rc(cont as u64);
        let winders = Value::from_raw_inc_rc(winders as u64);
        let to_winders: Gc<vectors::AlignedVector<Value>> = winders.try_into().unwrap();
        let from_winders = from_dynamic_extent.as_ref().unwrap();

        let thunks = compute_winders(from_winders, to_winders.read().as_ref());

        // Clone the continuation
        let cont = clone_continuation_env(&cont, &mut HashMap::default());
        let cont: Gc<Closure> = cont.try_into().unwrap();

        let (runtime, req_args, variadic) = {
            let cont_read = cont.read();
            (
                cont_read.runtime.clone(),
                cont_read.num_required_args,
                cont_read.variadic,
            )
        };

        Value::into_raw(Value::from(Closure::new(
            //     ManuallyDrop::new(Gc::new(Value::Closure(Closure::new(
            runtime,
            vec![Gc::new(thunks), Gc::new(Value::from(cont))],
            Vec::new(),
            FuncPtr::Continuation(call_thunks),
            req_args,
            variadic,
            None,
        ))) as i64
    }
}

fn compute_winders(from_extent: &DynamicWind, to_extent: &[Value]) -> Value {
    let len = from_extent.winders.len().min(to_extent.len());

    let mut split_point = 0;
    #[allow(clippy::needless_range_loop)]
    for i in 0..len {
        let UnpackedValue::Pair(pair /* ref to_in, _*/) = &*to_extent[i].unpacked_ref() else {
            unreachable!()
        };
        let pair_read = pair.read();
        let lists::Pair(to_in, _) = pair_read.as_ref();
        let to_in: Gc<Closure> = to_in.clone().try_into().unwrap();
        if Gc::ptr_eq(&from_extent.winders[i].0, &to_in) {
            split_point = i + 1;
        } else {
            break;
        }
    }

    let (_, to_extent) = to_extent.split_at(split_point);
    let (_, from_extent) = from_extent
        .winders
        .split_at_checked(split_point)
        .unwrap_or((&[], &[]));

    let mut thunks = Value::null();
    for thunk in from_extent
        .iter()
        .map(|(_, out)| Value::from(out.clone()))
        .chain(
            to_extent
                .iter()
                .map(|to_extent| {
                    let pair: Gc<lists::Pair> = to_extent.clone().try_into().unwrap();
                    let pair_read = pair.read();
                    pair_read.0.clone()
                })
                .rev(),
        )
    {
        thunks = Value::from((thunk, thunks));
        // Gc::new(Value::Pair(thunk, thunks));
    }

    thunks
}

unsafe extern "C" fn call_thunks(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    unsafe {
        // env[0] are the thunks:
        let thunks = Gc::from_raw_inc_rc(env.read());
        // env[1] is the continuation:
        let k: Gc<Closure> = Gc::from_raw_inc_rc(env.add(1).read())
            .read()
            .clone()
            .try_into()
            .unwrap();

        // k determines the number of arguments:
        let collected_args = {
            let k_read = k.read();
            let num_args = k_read.num_required_args;

            let mut collected_args = if k_read.variadic {
                args.add(num_args).as_ref().unwrap().clone()
            } else {
                Value::null()
            };

            for i in (0..num_args).rev() {
                let arg = args.add(i).as_ref().unwrap().clone();
                collected_args = Value::from((arg, collected_args));
            }

            collected_args
        };

        let thunks = Closure::new(
            Gc::from_raw_inc_rc(runtime),
            vec![thunks, Gc::new(collected_args), Gc::new(Value::from(k))],
            Vec::new(),
            FuncPtr::Continuation(call_thunks_pass_args),
            0,
            false,
            None,
        );

        let app = Application::new(
            Gc::new(thunks),
            Vec::new(),
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            None,
        );

        Box::into_raw(Box::new(Ok(app)))
    }
}

unsafe extern "C" fn call_thunks_pass_args(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    unsafe {
        // env[0] are the thunks:
        let thunks = Gc::from_raw_inc_rc(env.read());
        // env[1] are the collected arguments
        let args = Gc::from_raw_inc_rc(env.add(1).read());
        // env[2] is k1, the current continuation
        let k = Gc::from_raw_inc_rc(env.add(2).read());

        let thunks = thunks.read();
        let app = match &*thunks.unpacked_ref() {
            // UnpackedValue::Pair(head_thunk, tail) => {
            UnpackedValue::Pair(pair) => {
                let lists::Pair(head_thunk, tail) = &*pair.read();
                let head_thunk: Gc<Closure> = head_thunk.clone().try_into().unwrap();
                let cont = Closure::new(
                    Gc::from_raw_inc_rc(runtime),
                    vec![Gc::new(tail.clone()), args, k],
                    Vec::new(),
                    FuncPtr::Continuation(call_thunks_pass_args),
                    0,
                    false,
                    None,
                );
                Application::new(
                    head_thunk.clone(),
                    vec![Value::from(cont)],
                    ExceptionHandler::from_ptr(exception_handler),
                    dynamic_wind.as_ref().unwrap().clone(),
                    None,
                )
            }
            UnpackedValue::Null => {
                let mut collected_args = Vec::new();
                let args = args.read();
                list_to_vec(&args, &mut collected_args);
                // collected_args.push(Gc::new(Value::Null));
                Application::new(
                    k.read().clone().try_into().unwrap(),
                    collected_args,
                    ExceptionHandler::from_ptr(exception_handler),
                    dynamic_wind.as_ref().unwrap().clone(),
                    None,
                )
            }
            _ => unreachable!(),
        };

        Box::into_raw(Box::new(Ok(app)))
    }
}

#[runtime_fn]
unsafe extern "C" fn add(
    vals: *const i64,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> i64 {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            // Can't easily wrap these in a ManuallyDrop, so we dec the rc.
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
            .collect();
        match num::add(&vals) {
            Ok(num) => Value::into_raw(Value::from(num)) as i64,
            Err(condition) => {
                error.write(Box::into_raw(Box::new(Err(condition))));
                Value::into_raw(Value::undefined()) as i64
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn sub(
    vals: *const i64,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> i64 {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
            .collect();
        match num::sub(&vals[0], &vals[1..]) {
            Ok(num) => Value::into_raw(Value::from(num)) as i64,
            Err(condition) => {
                error.write(Box::into_raw(Box::new(Err(condition))));
                Value::into_raw(Value::undefined()) as i64
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn mul(
    vals: *const i64,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> i64 {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
            .collect();
        match num::mul(&vals) {
            Ok(num) => Value::into_raw(Value::from(num)) as i64,
            Err(condition) => {
                error.write(Box::into_raw(Box::new(Err(condition))));
                Value::into_raw(Value::undefined()) as i64
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn div(
    vals: *const i64,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> i64 {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
            .collect();
        match num::div(&vals[0], &vals[1..]) {
            Ok(num) => Value::into_raw(Value::from(num)) as i64,
            Err(condition) => {
                error.write(Box::into_raw(Box::new(Err(condition))));
                Value::into_raw(Value::undefined()) as i64
            }
        }
    }
}

macro_rules! define_comparison_fn {
    ( $name:ident ) => {
        #[runtime_fn]
        unsafe extern "C" fn $name(
            vals: *const i64,
            num_vals: u32,
            error: *mut *mut Result<Application, Condition>,
        ) -> i64 {
            unsafe {
                let vals: Vec<_> = (0..num_vals)
                    .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
                    .collect();
                match num::$name(&vals) {
                    Ok(res) => Value::into_raw(Value::from(res)) as i64,
                    Err(condition) => {
                        error.write(Box::into_raw(Box::new(Err(condition))));
                        Value::into_raw(Value::undefined()) as i64
                    }
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
