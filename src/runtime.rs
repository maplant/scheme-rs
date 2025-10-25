use crate::{
    ast::DefinitionBody,
    cps::{Compile, Cps, codegen::RuntimeFunctionsBuilder},
    env::{Environment, Global},
    exceptions::{Condition, Exception, ExceptionHandler, ExceptionHandlerInner, raise},
    gc::{Gc, GcInner, Trace, init_gc},
    lists::{self, list_to_vec},
    num,
    ports::Port,
    proc::{Application, ContinuationPtr, DynamicWind, FuncDebugInfo, FuncPtr, Procedure, UserPtr},
    registry::{ImportError, Library, Registry},
    symbols::Symbol,
    syntax::{Span, parse::Parser},
    value::{Cell, ReflexiveValue, UnpackedValue, Value},
};
use scheme_rs_macros::runtime_fn;
use std::{
    any::Any,
    collections::HashSet,
    mem::ManuallyDrop,
    path::Path,
    pin::Pin,
    sync::{Arc, mpsc},
};

#[cfg(not(feature = "async"))]
use std::{fs::File, io::BufReader};

#[cfg(feature = "tokio")]
use tokio::{fs::File, io::BufReader};

/// Scheme-rs Runtime
///
/// # Safety:
///
/// The runtime contains the only live references to the LLVM Context and
/// therefore modules and allocated functions in the form a Sender of compilation
/// tasks.
///
/// When that sender's ref count is zero, it will cause the receiver to fail and
/// the compilation task will exit, allowing for a graceful shutdown.
///
/// However, this is dropping a lifetime. If we clone a procedure and drop the
/// runtime from whence it was cleaved, we're left with a dangling pointer.
///
/// In order to remedy this it is vitally important the closure has a back
/// pointer to the runtime.
#[derive(Trace, Clone)]
pub struct Runtime(pub(crate) Gc<RuntimeInner>);

/*
impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}
*/

impl Runtime {
    /// Creates a new runtime. Also initializes the garbage collector and
    /// creates a default registry with the bridge functions populated.
    pub fn new(
        #[cfg(feature = "async")]
        async_runtime: Arc<impl AsyncRuntime>
    ) -> Self {
        let this = Self(Gc::new(RuntimeInner::new(
            #[cfg(feature = "async")]
            async_runtime
        )));
        let new_registry = Registry::new(&this);
        this.0.write().registry = new_registry;
        this
    }

    #[cfg(feature = "async")]
    pub fn async_runtime(&self) -> Arc<dyn AsyncRuntime> {
        self.0.read().async_runtime.clone()
    }

    #[cfg(feature = "tokio")]
    pub async fn run_program(&self, path: &Path) -> Result<Vec<Value>, Exception> {
        let progm = Library::new_program(self, path);
        let env = Environment::Top(progm);
        let file_name = path.file_name().unwrap().to_string_lossy();
        let reader = BufReader::new(File::open(path).await.unwrap());
        let port = Port::from_reader(reader);
        let mut input_port = port.try_lock_input_port().await.unwrap();
        let mut parser = Parser::new(&file_name, &mut input_port);
        let sexprs = parser
            .all_datums()
            .await
            .map_err(|err| ImportError::ParseSyntaxError(format!("{err:?}")))
            .unwrap();
        let body = DefinitionBody::parse_lib_body(self, &sexprs, &env, sexprs[0].span()).unwrap();
        let compiled = body.compile_top_level();
        let closure = self.compile_expr(compiled);
        Application::new(
            closure,
            Vec::new(),
            ExceptionHandler::default(),
            DynamicWind::default(),
            None,
        )
        .eval()
        .await
    }

    #[cfg(not(feature = "async"))]
    pub fn run_program(&self, path: &Path) -> Result<Vec<Value>, Exception> {
        let progm = Library::new_program(self, path);
        let env = Environment::Top(progm);
        let file_name = path.file_name().unwrap().to_string_lossy();
        let reader = BufReader::new(File::open(path).unwrap());
        let port = Port::from_reader(&file_name, reader);
        let mut parser = Parser::new(&port);
        let sexprs = parser
            .all_datums()
            .map_err(|err| ImportError::ParseSyntaxError(format!("{err:?}")))
            .unwrap();
        let body = DefinitionBody::parse_lib_body(self, &sexprs, &env, sexprs[0].span()).unwrap();
        let compiled = body.compile_top_level();
        let closure = self.compile_expr(compiled);
        Application::new(
            closure,
            Vec::new(),
            ExceptionHandler::default(),
            DynamicWind::default(),
            None,
        )
        .eval()
    }

    #[cfg(feature = "async")]
    pub fn eval_blocking(&self, app: Application) -> Result<Vec<Value>, Exception> {
        let async_runtime = self.async_runtime();
        let Ok(result) = std::thread::spawn(move || {
            async_runtime.block_on(
                Box::pin(async move { Box::new(app.eval().await) as Box<dyn Any + Send> }).as_mut(),
            )
        })
        .join()
        .unwrap()
        .downcast::<Result<Vec<Value>, Exception>>() else {
            unreachable!()
        };
        *result
    }

    pub fn get_registry(&self) -> Registry {
        self.0.read().registry.clone()
    }

    pub fn compile_expr(&self, expr: Cps) -> Procedure {
        let (completion_tx, completion_rx) = mpsc::sync_channel(1);
        let task = CompilationTask {
            completion_tx,
            compilation_unit: expr,
            runtime: self.clone(),
        };
        let sender = { self.0.read().compilation_buffer_tx.clone() };
        sender.send(task).unwrap();
        // Wait for the compilation task to complete:
        completion_rx.recv().unwrap()
    }

    pub(crate) unsafe fn from_raw_inc_rc(rt: *mut GcInner<RuntimeInner>) -> Self {
        unsafe { Self(Gc::from_raw_inc_rc(rt)) }
    }
}

#[derive(Trace)]
pub(crate) struct RuntimeInner {
    /// Package registry
    pub(crate) registry: Registry,
    /// Channel to compilation task
    compilation_buffer_tx: mpsc::SyncSender<CompilationTask>,
    pub(crate) constants_pool: HashSet<ReflexiveValue>,
    pub(crate) globals_pool: HashSet<Global>,
    pub(crate) debug_info: DebugInfo,
    #[cfg(feature = "async")]
    pub(crate) async_runtime: Arc<dyn AsyncRuntime>,
}

/*
impl Default for RuntimeInner {
    fn default() -> Self {
        Self::new()
    }
}
*/

const MAX_COMPILATION_TASKS: usize = 5; // Shrug

impl RuntimeInner {
    fn new(
        #[cfg(feature = "async")]
        async_runtime: Arc<impl AsyncRuntime>
    ) -> Self {
        // Ensure the GC is initialized:
        init_gc();
        let (compilation_buffer_tx, compilation_buffer_rx) =
            mpsc::sync_channel(MAX_COMPILATION_TASKS);
        // According the inkwell (and therefore LLVM docs), one LlvmContext may
        // be present per thread. Thus, we spawn a new thread and a new
        // compilation task for every Runtime:
        std::thread::spawn(move || compilation_task(compilation_buffer_rx));
        RuntimeInner {
            registry: Registry::empty(),
            compilation_buffer_tx,
            constants_pool: HashSet::new(),
            globals_pool: HashSet::new(),
            debug_info: DebugInfo::default(),
            #[cfg(feature = "async")]
            async_runtime,
        }
    }
}

#[cfg(feature = "async")]
pub trait AsyncRuntime: Any + Send + Sync + 'static {
    fn block_on(
        &self,
        future: Pin<&mut dyn Future<Output = Box<dyn Any + Send + 'static>>>,
    ) -> Box<dyn Any + Send + 'static>;
}

#[cfg(feature = "tokio")]
impl AsyncRuntime for tokio::runtime::Runtime {
    fn block_on(
        &self,
        future: Pin<&mut dyn Future<Output = Box<dyn Any + Send + 'static>>>,
    ) -> Box<dyn Any + Send + 'static> {
        self.block_on(future)
    }
}

/*
impl AsyncRuntime for tokio::runtime::Handle {
    fn block_on<F: Future>(&self, future: F) -> F::Output {
        self.block_on(future)
    }
}
*/

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
    compilation_unit: Cps,
    completion_tx: mpsc::SyncSender<Procedure>,
    /// Since Contexts are per-thread, we will only ever see the same Runtime.
    /// However, we can't cache the Runtime, as that would cause a ref cycle
    /// that would prevent the last compilation buffer sender to drop.
    /// Therefore, its lifetime is that of the compilation task
    runtime: Runtime,
}

fn compilation_task(compilation_queue_rx: mpsc::Receiver<CompilationTask>) {
    use cranelift::prelude::*;
    use cranelift_jit::{JITBuilder, JITModule};

    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    // FIXME set back to true once the x64 backend supports it.
    flag_builder.set("is_pic", "false").unwrap();
    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {msg}");
    });
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap();

    let mut jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

    for runtime_fn in inventory::iter::<RuntimeFn> {
        (runtime_fn.install_symbol)(&mut jit_builder);
    }

    let mut module = JITModule::new(jit_builder);
    let mut runtime_funcs_builder = RuntimeFunctionsBuilder::default();

    for runtime_fn in inventory::iter::<RuntimeFn> {
        (runtime_fn.install_decl)(&mut runtime_funcs_builder, &mut module);
    }

    let runtime_funcs = runtime_funcs_builder.build().unwrap();

    // By storing all of the debug information in the same lifetime as the
    // Context, we can directly put pointers referencing the debug information
    // in our JIT compiled functions:
    let mut debug_info = DebugInfo::default();

    while let Ok(task) = compilation_queue_rx.recv() {
        let CompilationTask {
            completion_tx,
            compilation_unit,
            runtime,
        } = task;

        let proc =
            compilation_unit.into_procedure(runtime, &runtime_funcs, &mut module, &mut debug_info);

        let _ = completion_tx.send(proc);
    }
}

pub(crate) struct RuntimeFn {
    install_decl:
        for<'a> fn(&'a mut RuntimeFunctionsBuilder, module: &'a mut cranelift_jit::JITModule),
    install_symbol: for<'a> fn(&'a mut cranelift_jit::JITBuilder),
}

impl RuntimeFn {
    pub(crate) const fn new(
        install_decl: for<'a> fn(
            &'a mut RuntimeFunctionsBuilder,
            module: &'a mut cranelift_jit::JITModule,
        ),
        install_symbol: for<'a> fn(&'a mut cranelift_jit::JITBuilder),
    ) -> Self {
        Self {
            install_decl,
            install_symbol,
        }
    }
}

inventory::collect!(RuntimeFn);

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
unsafe extern "C" fn alloc_cell() -> i64 {
    Value::into_raw(Value::from(Cell(Gc::new(Value::undefined())))) as i64
}

/// Read the value of a Cell
#[runtime_fn]
unsafe extern "C" fn read_cell(cell: i64) -> i64 {
    unsafe {
        let cell = Value::from_raw(cell as u64);
        let cell: Cell = cell.try_into().unwrap();
        // We do not need to increment the reference count of the cell, it is going to
        // be decremented at the end of this function.
        let cell = ManuallyDrop::new(cell);
        let cell_read = cell.0.read();
        let raw = Value::as_raw(&cell_read);
        raw as i64
    }
}

/// Decrement the reference count of a value
#[runtime_fn]
unsafe extern "C" fn dropv(val: *const i64, num_drops: u32) {
    unsafe {
        for i in 0..num_drops {
            drop(Value::from_raw(val.add(i as usize).read() as u64));
        }
    }
}

/// Create a boxed application
#[runtime_fn]
unsafe extern "C" fn apply(
    runtime: *mut GcInner<RuntimeInner>,
    op: i64,
    args: *const i64,
    num_args: u32,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
    span: *const Span,
) -> *mut Application {
    unsafe {
        let args: Vec<_> = (0..num_args)
            .map(|i| Value::from_raw_inc_rc(args.add(i as usize).read() as u64))
            .collect();

        let op = match Value::from_raw_inc_rc(op as u64).unpack() {
            UnpackedValue::Procedure(op) => op,
            x => {
                let raised = raise(
                    Runtime::from_raw_inc_rc(runtime),
                    Condition::invalid_operator(x.type_name()).into(),
                    ExceptionHandler::from_ptr(exception_handler),
                    dynamic_wind.as_ref().unwrap(),
                );
                return Box::into_raw(Box::new(raised));
            }
        };

        let app = Application::new(
            op,
            args,
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            arc_from_ptr(span),
        );

        Box::into_raw(Box::new(app))
    }
}

/// Create a boxed application that forwards a list of values to the operator
#[runtime_fn]
unsafe extern "C" fn forward(
    runtime: *mut GcInner<RuntimeInner>,
    op: i64,
    args: i64,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        let op = match Value::from_raw_inc_rc(op as u64).unpack() {
            UnpackedValue::Procedure(op) => op,
            x => {
                let raised = raise(
                    Runtime::from_raw_inc_rc(runtime),
                    Condition::invalid_operator(x.type_name()).into(),
                    ExceptionHandler::from_ptr(exception_handler),
                    dynamic_wind.as_ref().unwrap(),
                );
                return Box::into_raw(Box::new(raised));
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

        Box::into_raw(Box::new(app))
    }
}

/// Create a boxed application that simply returns its arguments
#[runtime_fn]
pub(crate) unsafe extern "C" fn halt(args: i64) -> *mut Application {
    unsafe {
        // We do not need to increment the rc here, it will be incremented in list_to_vec
        let args = ManuallyDrop::new(Value::from_raw(args as u64));
        let mut flattened = Vec::new();
        list_to_vec(&args, &mut flattened);
        let app = Application::halt(flattened);
        Box::into_raw(Box::new(app))
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
unsafe extern "C" fn store(from: i64, to: i64) {
    unsafe {
        // We do not need to increment the ref count for to, it is dropped
        // immediately.
        let from = Value::from_raw_inc_rc(from as u64);
        let to: ManuallyDrop<Cell> =
            ManuallyDrop::new(Value::from_raw(to as u64).try_into().unwrap());
        *to.0.write() = from;
    }
}

/// Return the cons of the two arguments
#[runtime_fn]
unsafe extern "C" fn cons(vals: *const i64, num_vals: u32, error: *mut Value) -> i64 {
    unsafe {
        if num_vals != 2 {
            error.write(Condition::wrong_num_of_args(2, num_vals as usize).into());
            return Value::into_raw(Value::undefined()) as i64;
        }
        let car = Value::from_raw_inc_rc(vals.read() as u64);
        let cdr = Value::from_raw_inc_rc(vals.add(1).read() as u64);
        let raw = Value::into_raw(Value::from(Gc::new(lists::Pair(car, cdr))));
        raw as i64
    }
}

/// Return the proper list of the arguments
#[runtime_fn]
unsafe extern "C" fn list(vals: *const i64, num_vals: u32, _error: *mut Value) -> i64 {
    let mut list = Value::null();
    unsafe {
        for i in (0..num_vals).rev() {
            list = Value::from(Gc::new(lists::Pair(
                Value::from_raw_inc_rc(vals.add(i as usize).read() as u64),
                list,
            )));
        }
    }
    Value::into_raw(list) as i64
}

/// Allocate a continuation
#[runtime_fn]
unsafe extern "C" fn make_continuation(
    runtime: *mut GcInner<RuntimeInner>,
    fn_ptr: ContinuationPtr,
    env: *const i64,
    num_envs: u32,
    num_required_args: u32,
    variadic: bool,
) -> i64 {
    unsafe {
        // Collect the environment:
        let env: Vec<_> = (0..num_envs)
            .map(|i| Value::from_raw_inc_rc(env.add(i as usize).read() as u64))
            .collect();

        let proc = Procedure::new(
            Runtime::from_raw_inc_rc(runtime),
            env,
            FuncPtr::Continuation(fn_ptr),
            num_required_args as usize,
            variadic,
            None,
        );

        Value::into_raw(Value::from(proc)) as i64
    }
}

/// Allocate a user function
#[runtime_fn]
unsafe extern "C" fn make_user(
    runtime: *mut GcInner<RuntimeInner>,
    fn_ptr: UserPtr,
    env: *const i64,
    num_envs: u32,
    num_required_args: u32,
    variadic: bool,
    debug_info: *const FuncDebugInfo,
) -> i64 {
    unsafe {
        // Collect the environment:
        let env: Vec<_> = (0..num_envs)
            .map(|i| Value::from_raw_inc_rc(env.add(i as usize).read() as u64))
            .collect();

        let proc = Procedure::new(
            Runtime::from_raw_inc_rc(runtime),
            env,
            FuncPtr::User(fn_ptr),
            num_required_args as usize,
            variadic,
            arc_from_ptr(debug_info),
        );

        Value::into_raw(Value::from(proc)) as i64
    }
}

/// Return an error in the case that a value is undefined
#[runtime_fn]
unsafe extern "C" fn error_unbound_variable(symbol: u32) -> i64 {
    let sym = Symbol(symbol);
    let condition = Condition::error(format!("{sym} is unbound"));
    Value::into_raw(Value::from(condition)) as i64
}

#[runtime_fn]
unsafe extern "C" fn add(vals: *const i64, num_vals: u32, error: *mut Value) -> i64 {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            // Can't easily wrap these in a ManuallyDrop, so we dec the rc.
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
            .collect();
        match num::add(&vals) {
            Ok(num) => Value::into_raw(Value::from(num)) as i64,
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined()) as i64
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn sub(vals: *const i64, num_vals: u32, error: *mut Value) -> i64 {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
            .collect();
        match num::sub(&vals[0], &vals[1..]) {
            Ok(num) => Value::into_raw(Value::from(num)) as i64,
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined()) as i64
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn mul(vals: *const i64, num_vals: u32, error: *mut Value) -> i64 {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
            .collect();
        match num::mul(&vals) {
            Ok(num) => Value::into_raw(Value::from(num)) as i64,
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined()) as i64
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn div(vals: *const i64, num_vals: u32, error: *mut Value) -> i64 {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
            .collect();
        match num::div(&vals[0], &vals[1..]) {
            Ok(num) => Value::into_raw(Value::from(num)) as i64,
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined()) as i64
            }
        }
    }
}

macro_rules! define_comparison_fn {
    ( $name:ident ) => {
        #[runtime_fn]
        unsafe extern "C" fn $name(vals: *const i64, num_vals: u32, error: *mut Value) -> i64 {
            unsafe {
                let vals: Vec<_> = (0..num_vals)
                    .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
                    .collect();
                match num::$name(&vals) {
                    Ok(res) => Value::into_raw(Value::from(res)) as i64,
                    Err(condition) => {
                        error.write(condition.into());
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
