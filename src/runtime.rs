//! Scheme-rs core runtime.
//!
//! The [`Runtime`] struct initializes and stores the core runtime for
//! scheme-rs. It contains a registry of libraries and the memory associated
//! with the JIT compiled [`Procedures`](Procedure).

use crate::{
    ast::DefinitionBody,
    cps::{Compile, Cps, codegen::RuntimeFunctionsBuilder},
    env::{Environment, Global, TopLevelEnvironment},
    exceptions::{Exception, raise},
    gc::{Gc, GcInner, Trace, init_gc},
    hashtables::EqualHashSet,
    lists::{Pair, list_to_vec},
    num,
    ports::{BufferMode, Port, Transcoder},
    proc::{
        Application, ContinuationPtr, DynamicState, FuncPtr, ProcDebugInfo, Procedure, UserPtr,
    },
    registry::Registry,
    symbols::Symbol,
    syntax::{Identifier, Span, Syntax},
    value::{Cell, UnpackedValue, Value},
};
use parking_lot::RwLock;
use scheme_rs_macros::{maybe_async, maybe_await, runtime_fn};
use std::{
    collections::{BTreeSet, HashSet},
    mem::ManuallyDrop,
    path::Path,
    sync::Arc,
};

/// Scheme-rs core runtime
///
/// Practically, the runtime is the core entry point for running Scheme programs
/// with scheme-rs. It initializes the garbage collector and JIT compiler tasks
/// and creates a new library registry.
///
/// There is not much you can do with a Runtime beyond creating it and using it
/// to [run programs](Runtime::run_program), however a lot of functions require
/// it as an arguments, such as [TopLevelEnvironment::new_repl].
///
/// You can also use the runtime to [define libraries](Runtime::def_lib) from
/// Rust code.
///
/// Runtime is automatically reference counted, so if you have all of the
/// procedures you need you can drop it without any issue.
///
/// # Safety:
///
/// The runtime contains the only live references to the Cranelift Context and
/// therefore modules and allocated functions in the form a Sender of
/// compilation tasks.
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
pub struct Runtime(pub(crate) Gc<RwLock<RuntimeInner>>);

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Runtime {
    /// Creates a new runtime. Also initializes the garbage collector and
    /// creates a default registry with the bridge functions populated.
    pub fn new() -> Self {
        let this = Self(Gc::new(RwLock::new(RuntimeInner::new())));
        let new_registry = Registry::new(&this);
        this.0.write().registry = new_registry;
        this
    }

    /// Run a program at the given location and return the values.
    #[maybe_async]
    pub fn run_program(&self, path: &Path) -> Result<Vec<Value>, Exception> {
        #[cfg(not(feature = "async"))]
        use std::fs::File;

        #[cfg(feature = "tokio")]
        use tokio::fs::File;

        let progm = TopLevelEnvironment::new_program(self, path);
        let env = Environment::Top(progm.clone());
        let mut form = {
            let port = Port::new(
                path.display(),
                maybe_await!(File::open(path)).unwrap(),
                BufferMode::Block,
                Some(Transcoder::native()),
            );
            let file_name = path.file_name().unwrap().to_str().unwrap_or("<unknown>");
            let span = Span::new(file_name);
            maybe_await!(port.all_sexprs(span)).map_err(Exception::from)?
        };

        form.add_scope(progm.scope());
        let body = maybe_await!(DefinitionBody::parse_lib_body(self, &form, &env))?;
        let compiled = body.compile_top_level();
        let closure = maybe_await!(self.compile_expr(compiled));

        maybe_await!(Application::new(closure, Vec::new()).eval(&mut DynamicState::default()))
    }

    /// Define a library from Rust code. Useful if file system access is disabled.
    #[cfg(not(feature = "async"))]
    #[track_caller]
    pub fn def_lib(&self, lib: &str) -> Result<(), Exception> {
        use std::panic::Location;

        self.get_registry()
            .def_lib(self, lib, Location::caller().file())
    }

    /// Define a library from Rust code. Useful if file system access is disabled.
    #[cfg(feature = "async")]
    pub async fn def_lib(&self, lib: &str) -> Result<(), Exception> {
        use std::panic::Location;

        self.get_registry()
            .def_lib(self, lib, Location::caller().file())
            .await
    }

    pub(crate) fn get_registry(&self) -> Registry {
        self.0.read().registry.clone()
    }

    #[maybe_async]
    pub(crate) fn compile_expr(&self, expr: Cps) -> Procedure {
        let (completion_tx, completion_rx) = completion();
        let task = CompilationTask {
            completion_tx,
            compilation_unit: expr,
            runtime: self.clone(),
        };
        let sender = { self.0.read().compilation_buffer_tx.clone() };
        let _ = maybe_await!(sender.send(task));
        // Wait for the compilation task to complete:
        maybe_await!(recv_procedure(completion_rx))
    }

    pub(crate) unsafe fn from_raw_inc_rc(rt: *mut GcInner<RwLock<RuntimeInner>>) -> Self {
        unsafe { Self(Gc::from_raw_inc_rc(rt)) }
    }
}

#[cfg(not(feature = "async"))]
type CompilationBufferTx = std::sync::mpsc::SyncSender<CompilationTask>;
#[cfg(not(feature = "async"))]
type CompilationBufferRx = std::sync::mpsc::Receiver<CompilationTask>;

#[cfg(feature = "async")]
type CompilationBufferTx = tokio::sync::mpsc::Sender<CompilationTask>;
#[cfg(feature = "async")]
type CompilationBufferRx = tokio::sync::mpsc::Receiver<CompilationTask>;

#[derive(Trace)]
pub(crate) struct RuntimeInner {
    /// Package registry
    pub(crate) registry: Registry,
    /// Channel to compilation task
    compilation_buffer_tx: CompilationBufferTx,
    pub(crate) constants_pool: EqualHashSet,
    pub(crate) globals_pool: HashSet<Global>,
    pub(crate) debug_info: DebugInfo,
}

impl Default for RuntimeInner {
    fn default() -> Self {
        Self::new()
    }
}

const MAX_COMPILATION_TASKS: usize = 5; // Shrug

#[cfg(not(feature = "async"))]
fn compilation_buffer() -> (CompilationBufferTx, CompilationBufferRx) {
    std::sync::mpsc::sync_channel(MAX_COMPILATION_TASKS)
}

#[cfg(feature = "async")]
fn compilation_buffer() -> (CompilationBufferTx, CompilationBufferRx) {
    tokio::sync::mpsc::channel(MAX_COMPILATION_TASKS)
}

impl RuntimeInner {
    fn new() -> Self {
        // Ensure the GC is initialized:
        // init_gc();
        let (compilation_buffer_tx, compilation_buffer_rx) = compilation_buffer();
        // According the inkwell (and therefore LLVM docs), one LlvmContext may
        // be present per thread. Thus, we spawn a new thread and a new
        // compilation task for every Runtime:
        std::thread::spawn(move || compilation_task(compilation_buffer_rx));
        RuntimeInner {
            registry: Registry::empty(),
            compilation_buffer_tx,
            constants_pool: EqualHashSet::new(),
            globals_pool: HashSet::new(),
            debug_info: DebugInfo::default(),
        }
    }
}

#[derive(Trace, Clone, Debug, Default)]
pub(crate) struct DebugInfo {
    /// Stored user function debug information:
    stored_func_info: Vec<Arc<ProcDebugInfo>>,
}

impl DebugInfo {
    pub fn store_func_info(&mut self, debug_info: Arc<ProcDebugInfo>) {
        self.stored_func_info.push(debug_info);
    }
}

#[cfg(not(feature = "async"))]
type CompletionTx = std::sync::mpsc::SyncSender<Procedure>;
#[cfg(not(feature = "async"))]
type CompletionRx = std::sync::mpsc::Receiver<Procedure>;

#[cfg(feature = "async")]
type CompletionTx = tokio::sync::oneshot::Sender<Procedure>;
#[cfg(feature = "async")]
type CompletionRx = tokio::sync::oneshot::Receiver<Procedure>;

#[cfg(not(feature = "async"))]
fn completion() -> (CompletionTx, CompletionRx) {
    std::sync::mpsc::sync_channel(1)
}

#[cfg(feature = "async")]
fn completion() -> (CompletionTx, CompletionRx) {
    tokio::sync::oneshot::channel()
}

#[cfg(not(feature = "async"))]
fn recv_procedure(rx: CompletionRx) -> Procedure {
    rx.recv().unwrap()
}

#[cfg(feature = "async")]
async fn recv_procedure(rx: CompletionRx) -> Procedure {
    rx.await.unwrap()
}

struct CompilationTask {
    compilation_unit: Cps,
    completion_tx: CompletionTx,
    /// Since Contexts are per-thread, we will only ever see the same Runtime.
    /// However, we can't cache the Runtime, as that would cause a ref cycle
    /// that would prevent the last compilation buffer sender to drop.
    /// Therefore, its lifetime is that of the compilation task
    runtime: Runtime,
}

#[cfg(not(feature = "async"))]
fn recv_compilation_task(rx: &mut CompilationBufferRx) -> Option<CompilationTask> {
    rx.recv().ok()
}

#[cfg(feature = "async")]
fn recv_compilation_task(rx: &mut CompilationBufferRx) -> Option<CompilationTask> {
    rx.blocking_recv()
}

fn compilation_task(mut compilation_queue_rx: CompilationBufferRx) {
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

    while let Some(task) = recv_compilation_task(&mut compilation_queue_rx) {
        let CompilationTask {
            completion_tx,
            compilation_unit,
            runtime,
        } = task;

        let proc =
            compilation_unit.into_procedure(runtime, &runtime_funcs, &mut module, &mut debug_info);

        let _ = completion_tx.send(proc);
    }

    // Free the JITed memory
    unsafe {
        module.free_memory();
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
unsafe extern "C" fn alloc_cell() -> *const () {
    Value::into_raw(Value::from(Cell(Gc::new(RwLock::new(Value::undefined())))))
}

/// Read the value of a Cell
#[runtime_fn]
unsafe extern "C" fn read_cell(cell: *const ()) -> *const () {
    unsafe {
        let cell = Value::from_raw(cell);
        let cell: Cell = cell.try_into().unwrap();
        // We do not need to increment the reference count of the cell, it is going to
        // be decremented at the end of this function.
        let cell = ManuallyDrop::new(cell);
        let cell_read = cell.0.read();
        Value::as_raw(&cell_read)
    }
}

/// Decrement the reference count of a value
#[runtime_fn]
unsafe extern "C" fn dropv(val: *const *const (), num_drops: u32) {
    unsafe {
        for i in 0..num_drops {
            drop(Value::from_raw(val.add(i as usize).read()));
        }
    }
}

/// Create a boxed application
#[runtime_fn]
unsafe extern "C" fn apply(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    op: *const (),
    args: *const *const (),
    num_args: u32,
    dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        let args: Vec<_> = (0..num_args)
            .map(|i| Value::from_raw_inc_rc(args.add(i as usize).read()))
            .collect();

        let op = match Value::from_raw_inc_rc(op).unpack() {
            UnpackedValue::Procedure(op) => op,
            x => {
                println!("x = {:?}", x.clone().into_value());
                let raised = raise(
                    Runtime::from_raw_inc_rc(runtime),
                    Exception::invalid_operator(x.type_name()).into(),
                    dyn_state.as_mut().unwrap_unchecked(),
                );
                return Box::into_raw(Box::new(raised));
            }
        };

        let app = Application::new(op, args);

        Box::into_raw(Box::new(app))
    }
}

/// Get a frame from a procedure and a span
#[runtime_fn]
unsafe extern "C" fn get_frame(op: *const (), span: *const ()) -> *const () {
    unsafe {
        let op = Value::from_raw_inc_rc(op);
        let Some(op) = op.cast_to_scheme_type::<Procedure>() else {
            return Value::into_raw(Value::null());
        };
        let span = Value::from_raw_inc_rc(span);
        let span = span.cast_to_rust_type::<Span>().unwrap();
        let frame = Syntax::Identifier {
            ident: Identifier {
                sym: op
                    .get_debug_info()
                    .map_or_else(|| Symbol::intern("<lambda>"), |dbg| dbg.name),
                scopes: BTreeSet::new(),
            },
            span: span.as_ref().clone(),
        };
        Value::into_raw(Value::from(frame))
    }
}

/// Set the value for continuation mark
#[runtime_fn]
unsafe extern "C" fn set_continuation_mark(
    tag: *const (),
    val: *const (),
    dyn_state: *mut DynamicState,
) {
    unsafe {
        let tag = Value::from_raw_inc_rc(tag);
        let val = Value::from_raw_inc_rc(val);
        dyn_state
            .as_mut()
            .unwrap()
            .set_continuation_mark(tag.cast_to_scheme_type().unwrap(), val);
    }
}

/// Create a boxed application that simply returns its arguments
#[runtime_fn]
pub(crate) unsafe extern "C" fn halt(args: *const ()) -> *mut Application {
    unsafe {
        // We do not need to increment the rc here, it will be incremented in list_to_vec
        let args = ManuallyDrop::new(Value::from_raw(args));
        let mut flattened = Vec::new();
        list_to_vec(&args, &mut flattened);
        let app = Application::halt_ok(flattened);
        Box::into_raw(Box::new(app))
    }
}

/// Evaluate a `Gc<Value>` as "truthy" or not, as in whether it triggers a
/// conditional.
#[runtime_fn]
unsafe extern "C" fn truthy(val: *const ()) -> bool {
    unsafe {
        // No need to increment the reference count here:
        ManuallyDrop::new(Value::from_raw(val)).is_true()
    }
}

/// Replace the value pointed to at to with the value contained in from.
#[runtime_fn]
unsafe extern "C" fn store(from: *const (), to: *const ()) {
    unsafe {
        // We do not need to increment the ref count for to, it is dropped
        // immediately.
        let from = Value::from_raw_inc_rc(from);
        let to: ManuallyDrop<Cell> = ManuallyDrop::new(Value::from_raw(to).try_into().unwrap());
        *to.0.write() = from;
    }
}

/// Return the cons of the two arguments
#[runtime_fn]
unsafe extern "C" fn cons(vals: *const *const (), num_vals: u32, error: *mut Value) -> *const () {
    unsafe {
        if num_vals != 2 {
            error.write(Exception::wrong_num_of_args(2, num_vals as usize).into());
            return Value::into_raw(Value::undefined());
        }
        let car = Value::from_raw_inc_rc(vals.read());
        let cdr = Value::from_raw_inc_rc(vals.add(1).read());
        Value::into_raw(Value::from(Pair::new(car, cdr, true)))
    }
}

/// Return the proper list of the arguments
#[runtime_fn]
unsafe extern "C" fn list(vals: *const *const (), num_vals: u32, _error: *mut Value) -> *const () {
    let mut list = Value::null();
    unsafe {
        for i in (0..num_vals).rev() {
            list = Value::from(Pair::new(
                Value::from_raw_inc_rc(vals.add(i as usize).read()),
                list,
                true,
            ));
        }
    }
    Value::into_raw(list)
}

/// Allocate a continuation
#[runtime_fn]
unsafe extern "C" fn make_continuation(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    fn_ptr: ContinuationPtr,
    env: *const *const (),
    num_envs: u32,
    num_required_args: u32,
    variadic: bool,
    dyn_state: *mut DynamicState,
) -> *const () {
    unsafe {
        // Collect the environment:
        let env: Vec<_> = (0..num_envs)
            .map(|i| Value::from_raw_inc_rc(env.add(i as usize).read()))
            .collect();

        let proc = dyn_state.as_mut().unwrap().new_k(
            Runtime::from_raw_inc_rc(runtime),
            env,
            fn_ptr,
            num_required_args as usize,
            variadic,
        );

        Value::into_raw(Value::from(proc))
    }
}

/// Allocate a user function
#[runtime_fn]
unsafe extern "C" fn make_user(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    fn_ptr: UserPtr,
    env: *const *const (),
    num_envs: u32,
    num_required_args: u32,
    variadic: bool,
    debug_info: *const ProcDebugInfo,
) -> *const () {
    unsafe {
        // Collect the environment:
        let env: Vec<_> = (0..num_envs)
            .map(|i| Value::from_raw_inc_rc(env.add(i as usize).read()))
            .collect();

        let proc = Procedure::with_debug_info(
            Runtime::from_raw_inc_rc(runtime),
            env,
            FuncPtr::User(fn_ptr),
            num_required_args as usize,
            variadic,
            arc_from_ptr(debug_info),
        );

        Value::into_raw(Value::from(proc))
    }
}

/// Return an error in the case that a value is undefined
#[runtime_fn]
unsafe extern "C" fn error_unbound_variable(symbol: u32) -> *const () {
    let sym = Symbol(symbol);
    let condition = Exception::error(format!("{sym} is unbound"));
    Value::into_raw(Value::from(condition))
}

#[runtime_fn]
unsafe extern "C" fn add(vals: *const *const (), num_vals: u32, error: *mut Value) -> *const () {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            // Can't easily wrap these in a ManuallyDrop, so we dec the rc.
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read()))
            .collect();
        match num::add_prim(&vals) {
            Ok(num) => Value::into_raw(Value::from(num)),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn sub(vals: *const *const (), num_vals: u32, error: *mut Value) -> *const () {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read()))
            .collect();
        match num::sub_prim(&vals[0], &vals[1..]) {
            Ok(num) => Value::into_raw(Value::from(num)),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn mul(vals: *const *const (), num_vals: u32, error: *mut Value) -> *const () {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read()))
            .collect();
        match num::mul_prim(&vals) {
            Ok(num) => Value::into_raw(Value::from(num)),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn div(vals: *const *const (), num_vals: u32, error: *mut Value) -> *const () {
    unsafe {
        let vals: Vec<_> = (0..num_vals)
            .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read()))
            .collect();
        match num::div_prim(&vals[0], &vals[1..]) {
            Ok(num) => Value::into_raw(Value::from(num)),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

macro_rules! define_comparison_fn {
    ( $name:ident, $prim:ident ) => {
        #[runtime_fn]
        unsafe extern "C" fn $name(
            vals: *const *const (),
            num_vals: u32,
            error: *mut Value,
        ) -> *const () {
            unsafe {
                let vals: Vec<_> = (0..num_vals)
                    .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read()))
                    .collect();
                match num::$prim(&vals) {
                    Ok(res) => Value::into_raw(Value::from(res)),
                    Err(condition) => {
                        error.write(condition.into());
                        Value::into_raw(Value::undefined())
                    }
                }
            }
        }
    };
}

define_comparison_fn!(equal, equal_prim);
define_comparison_fn!(greater, greater_prim);
define_comparison_fn!(greater_equal, greater_equal_prim);
define_comparison_fn!(lesser, lesser_prim);
define_comparison_fn!(lesser_equal, lesser_equal_prim);
