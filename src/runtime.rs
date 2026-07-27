//! Scheme-rs core runtime.
//!
//! The [`Runtime`] struct initializes and stores the core runtime for
//! scheme-rs. It contains a registry of libraries and the memory associated
//! with the JIT compiled [`Procedures`](Procedure).

use crate::{
    ast::{Definitions, Primitive},
    cps::{Cps, codegen::RuntimeFunctionsBuilder, compile::Compiler},
    env::{Environment, Global, TopLevelEnvironment},
    exceptions::{Exception, SourceCache, raise},
    gc::{Gc, GcInner, Trace, init_gc},
    hashtables::EqualHashSet,
    lists::{Pair, list_to_vec},
    num,
    parameters::Parameter,
    ports::{BufferMode, Port, Transcoder},
    proc::{
        Application, ContBarrier, ContPtr, ContinuationPtr, DynState, FuncPtr, ProcDebugInfo,
        Procedure, ProcedureInner, UserPtr,
    },
    registry::Registry,
    symbols::Symbol,
    syntax::{Span, Syntax},
    value::{Cell, TAG, UnpackedValue, Value},
};
use parking_lot::{Mutex, MutexGuard, RwLock};
use scheme_rs_macros::{maybe_async, maybe_await, runtime_fn};
use std::{
    collections::HashSet,
    mem::{ManuallyDrop, MaybeUninit},
    path::Path,
    sync::{Arc, LazyLock},
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
/// There is only one Runtime per running program.
///
/// # Safety
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
/// In order to remedy this we keep a Runtime alive for the lifetime of a
/// program.
#[derive(Trace, Copy, Clone)]
pub struct Runtime(#[trace(skip)] pub(crate) &'static RuntimeInner);

impl Default for Runtime {
    fn default() -> Self {
        Self::handle()
    }
}

impl Runtime {
    /// Creates a handle to the application's runtime, initializing the garbage
    /// collector and creating a default registry with the bridge functions
    /// populated.
    pub fn handle() -> Self {
        static APP_RUNTIME: LazyLock<RuntimeInner> = LazyLock::new(RuntimeInner::new);
        Self(&APP_RUNTIME)
    }

    /// Run a program at the given location and return the values.
    #[maybe_async]
    pub fn run_program(self, path: &Path) -> Result<Vec<Value>, Exception> {
        #[cfg(not(feature = "async"))]
        use std::fs::File;

        #[cfg(feature = "tokio")]
        use tokio::fs::File;

        let progm = TopLevelEnvironment::new_program(path);
        let env = Environment::Top(progm.clone());

        let mut form = {
            let port = Port::new(
                path.display(),
                maybe_await!(File::open(path)).map_err(Exception::io_error)?,
                BufferMode::Block,
                Some(Transcoder::native()),
            );
            let file_name = path.file_name().unwrap().to_str().unwrap_or("<unknown>");
            let span = Span::new(file_name);
            maybe_await!(port.all_sexprs(span)).map_err(Exception::from)?
        };

        form.add_scope(progm.scope());

        // Check if the first form is an import
        let add_rnrs_import = if let Some(first_form) = form.car()
            && let Some(Syntax::Identifier { ident, .. }) = first_form.car()
            && let Some(binding) = ident.resolve()
        {
            env.lookup_primitive(binding) != Some(Primitive::Import)
        } else {
            true
        };

        // If the first form is not an import, import (rnrs)
        if add_rnrs_import {
            maybe_await!(env.import("(library (rnrs))".parse()?))?;
        }

        let mut mutable_vars = HashSet::default();
        let body = maybe_await!(Definitions::parse_lib_body(&form, &env, &mut mutable_vars))?;
        maybe_await!(Compiler::new(mutable_vars).compile(self, &body))
    }

    // TODO: Move to registry:

    /// Define a library from Rust code. Useful if file system access is disabled.
    #[cfg(not(feature = "async"))]
    #[track_caller]
    pub fn def_lib(&self, lib: &str) -> Result<(), Exception> {
        use std::panic::Location;

        self.get_registry().def_lib(lib, Location::caller().file())
    }

    /// Define a library from Rust code. Useful if file system access is disabled.
    #[cfg(feature = "async")]
    pub async fn def_lib(&self, lib: &str) -> Result<(), Exception> {
        use std::panic::Location;

        self.get_registry()
            .def_lib(lib, Location::caller().file())
            .await
    }

    /// # Safety
    ///
    /// See [`Registry::load_plugin`].
    #[cfg(feature = "plugins")]
    #[maybe_async]
    pub unsafe fn load_plugin(&self, library: libloading::Library) -> Result<(), Exception> {
        unsafe { maybe_await!(self.get_registry().load_plugin(library)) }
    }

    pub(crate) fn get_registry(&self) -> Registry {
        self.0.registry.clone()
    }

    #[maybe_async]
    pub(crate) fn compile_expr(&self, expr: Cps) -> Result<Vec<Value>, Exception> {
        let (completion_tx, completion_rx) = completion();
        let task = CompilationTask {
            completion_tx,
            compilation_unit: expr,
        };
        let sender = { self.0.compilation_buffer_tx.clone() };
        let _ = maybe_await!(sender.send(task));
        let entry_cont = maybe_await!(recv_continuation(completion_rx));
        let mut barrier = ContBarrier::new();
        let app = unsafe {
            let mut app = std::mem::MaybeUninit::<Application>::uninit();
            entry_cont(
                std::ptr::null(),
                std::ptr::null(),
                &mut barrier as *mut ContBarrier<'_>,
                &mut app,
            );
            app.assume_init()
        };
        maybe_await!(app.eval(&mut barrier))
    }

    pub fn source_cache(&self) -> MutexGuard<'_, SourceCache> {
        self.0.source_cache.lock()
    }

    /// Takes the runtime's persistent root dynamic state, leaving a fresh
    /// one behind. Paired with [`Runtime::restore_dyn_state`] to publish the
    /// root state for a top-level evaluation without holding the runtime
    /// lock for the evaluation's duration.
    pub(crate) fn checkout_dyn_state(&self) -> DynState {
        std::mem::take(&mut self.0.dyn_state.lock())
    }

    /// Checks a dynamic state back in as the runtime's root state.
    pub(crate) fn restore_dyn_state(&self, state: DynState) {
        *self.0.dyn_state.lock() = state;
    }
}

#[allow(unused)]
#[cfg(not(feature = "async"))]
fn read_to_string(path: &Path) -> std::io::Result<String> {
    std::fs::read_to_string(path)
}

#[allow(unused)]
#[cfg(feature = "tokio")]
async fn read_to_string(path: &Path) -> std::io::Result<String> {
    tokio::fs::read_to_string(path).await
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
    pub(crate) constants_pool: Mutex<EqualHashSet>,
    pub(crate) globals_pool: Mutex<HashSet<Global>>,
    pub(crate) debug_info: DebugInfo,
    pub(crate) source_cache: Mutex<SourceCache>,
    /// The root dynamic state, persistent for the runtime's lifetime.
    /// Checked out for the duration of each top-level evaluation (see
    /// [`Runtime::checkout_dyn_state`]) so parameter roots, current ports,
    /// etc. outlive any single eval call.
    dyn_state: Mutex<DynState>,
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
        init_gc();
        let (compilation_buffer_tx, compilation_buffer_rx) = compilation_buffer();
        std::thread::spawn(move || compilation_task(compilation_buffer_rx));
        RuntimeInner {
            registry: Registry::new(),
            compilation_buffer_tx,
            constants_pool: Mutex::new(EqualHashSet::new()),
            globals_pool: Mutex::new(HashSet::new()),
            debug_info: DebugInfo::default(),
            source_cache: Mutex::new(SourceCache::default()),
            dyn_state: Mutex::new(DynState::default()),
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
type CompletionTx = std::sync::mpsc::SyncSender<ContinuationPtr>;
#[cfg(not(feature = "async"))]
type CompletionRx = std::sync::mpsc::Receiver<ContinuationPtr>;

#[cfg(feature = "async")]
type CompletionTx = tokio::sync::oneshot::Sender<ContinuationPtr>;
#[cfg(feature = "async")]
type CompletionRx = tokio::sync::oneshot::Receiver<ContinuationPtr>;

#[cfg(not(feature = "async"))]
fn completion() -> (CompletionTx, CompletionRx) {
    std::sync::mpsc::sync_channel(1)
}

#[cfg(feature = "async")]
fn completion() -> (CompletionTx, CompletionRx) {
    tokio::sync::oneshot::channel()
}

#[cfg(not(feature = "async"))]
fn recv_continuation(rx: CompletionRx) -> ContinuationPtr {
    rx.recv().unwrap()
}

#[cfg(feature = "async")]
async fn recv_continuation(rx: CompletionRx) -> ContinuationPtr {
    rx.await.unwrap()
}

struct CompilationTask {
    compilation_unit: Cps,
    completion_tx: CompletionTx,
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
        } = task;

        let proc = compilation_unit.compile(&runtime_funcs, &mut module, &mut debug_info);

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

/// Increment the reference count of a value, returning it
#[runtime_fn]
unsafe extern "C" fn clonev(val: *const ()) -> *const () {
    unsafe { Value::into_raw(Value::from_raw_inc_rc(val)) }
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

/// Create a boxed application.
#[runtime_fn]
unsafe extern "C" fn apply(
    op: *const (),
    args: *const *const (),
    num_args: u32,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        let op_val = Value::from_raw_inc_rc(op);
        let op = match op_val.unpack() {
            UnpackedValue::Procedure(op) => op,
            UnpackedValue::Record(ref record) if record.cast::<Parameter>().is_some() => {
                record.cast::<Parameter>().unwrap().companion().clone()
            }
            x => {
                let raised = raise(
                    Exception::invalid_operator(&x.type_name()).into(),
                    barrier.as_mut().unwrap_unchecked(),
                );
                (*out).write(raised);
                return;
            }
        };

        (*out).write(Application::new(
            op,
            (0..num_args)
                .map(|i| Value::from_raw_inc_rc(args.add(i as usize).read()))
                .collect(),
        ));
    }
}

/// Get a frame from a procedure and a span
#[cfg(feature = "continuation-marks")]
#[runtime_fn]
unsafe extern "C" fn get_frame(op: *const (), span: *const ()) -> *const () {
    unsafe {
        use std::collections::BTreeSet;

        use crate::{records::Embedded, syntax::Identifier};

        let op = Value::from_raw_inc_rc(op);
        let Some(op) = op.cast::<Procedure>() else {
            return Value::into_raw(Value::null());
        };
        let span = Value::from_raw_inc_rc(span);
        let span = span.cast::<Embedded<Span>>().unwrap();
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
#[cfg(feature = "continuation-marks")]
#[runtime_fn]
unsafe extern "C" fn set_continuation_mark(
    tag: *const (),
    val: *const (),
    barrier: *mut ContBarrier,
) {
    unsafe {
        let tag = Value::from_raw_inc_rc(tag);
        let val = Value::from_raw_inc_rc(val);
        let barrier = barrier.as_mut().unwrap_unchecked();
        barrier.set_continuation_mark(tag.cast().unwrap(), val);
    }
}

/// Create a boxed application that simply returns its arguments
#[runtime_fn]
pub(crate) unsafe extern "C" fn halt(args: *const (), out: *mut MaybeUninit<Application>) {
    unsafe {
        // We do not need to increment the rc here, it will be incremented in list_to_vec
        let args = ManuallyDrop::new(Value::from_raw(args));
        let mut flattened = Vec::new();
        list_to_vec(&args, &mut flattened);
        (*out).write(Application::halt_ok(flattened));
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

/// Return the car of the argument
#[runtime_fn]
unsafe extern "C" fn car(val: *const (), error: *mut Value) -> *const () {
    unsafe {
        let val = ManuallyDrop::new(Value::from_raw(val));
        if let Some(car) = val.pair_car() {
            return Value::into_raw(car);
        }
        match val.try_to::<Pair>() {
            Ok(pair) => Value::into_raw(pair.car()),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

/// Return the cdr of the argument
#[runtime_fn]
unsafe extern "C" fn cdr(val: *const (), error: *mut Value) -> *const () {
    unsafe {
        let val = ManuallyDrop::new(Value::from_raw(val));
        if let Some(cdr) = val.pair_cdr() {
            return Value::into_raw(cdr);
        }
        match val.try_to::<Pair>() {
            Ok(pair) => Value::into_raw(pair.cdr()),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

/// Return the cons of the two arguments
#[runtime_fn]
unsafe extern "C" fn cons(car: *const (), cdr: *const ()) -> *const () {
    unsafe {
        let car = Value::from_raw_inc_rc(car);
        let cdr = Value::from_raw_inc_rc(cdr);
        Value::into_raw(Value::from(Pair::mutable(car, cdr)))
    }
}

/// Return the proper list of the arguments
#[runtime_fn]
unsafe extern "C" fn list(vals: *const *const (), num_vals: u32) -> *const () {
    let mut list = Value::null();
    unsafe {
        for i in (0..num_vals).rev() {
            list = Value::from(Pair::mutable(
                Value::from_raw_inc_rc(vals.add(i as usize).read()),
                list,
            ));
        }
    }
    Value::into_raw(list)
}

/// Allocate a continuation
#[runtime_fn]
unsafe extern "C" fn push_continuation(
    fn_ptr: ContinuationPtr,
    env: *const *const (),
    num_envs: u32,
    num_required_args: u32,
    variadic: bool,
    barrier: *mut ContBarrier,
) {
    unsafe {
        let barrier = barrier.as_mut().unwrap();
        barrier.push_cont(
            (0..num_envs).map(|i| Value::from_raw_inc_rc(env.add(i as usize).read())),
            ContPtr::Continuation(fn_ptr),
            num_required_args as usize,
            variadic,
        );
    }
}

/// Call the current continuation
#[runtime_fn]
unsafe extern "C" fn call_continuation(
    args: *const *const (),
    num_args: u32,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        (*out).write(
            barrier.as_mut().unwrap().call_cont(
                (0..num_args)
                    .map(|i| Value::from_raw_inc_rc(args.add(i as usize).read()))
                    .collect(),
            ),
        );
    }
}

/// Allocate a user function
#[runtime_fn]
unsafe extern "C" fn make_user(
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

        let proc = Procedure(Gc::rooted(ProcedureInner::new(
            env,
            FuncPtr::User(fn_ptr),
            num_required_args as usize,
            variadic,
            arc_from_ptr(debug_info),
        )));

        Value::into_raw(Value::from(proc))
    }
}

/// Path in a value into the env array for a procedure post-allocation. This
/// isn't normally possible without Mutices, but procedures are allocated rooted
/// and are unrooted after.
#[runtime_fn]
unsafe extern "C" fn patch_env_slot(proc: *const (), slot_idx: u32, value: *const ()) {
    unsafe {
        let proc_gc = ManuallyDrop::new(Gc::from_raw(
            proc.map_addr(|raw| raw & !TAG) as *mut GcInner<ProcedureInner>
        ));
        (*Gc::as_ptr(&proc_gc)).data_mut().env[slot_idx as usize] = Value::from_raw_inc_rc(value);
    }
}

/// Unroot a procedure, giving up mutable access
#[runtime_fn]
unsafe extern "C" fn unroot_proc(proc: *const ()) {
    unsafe {
        let proc_gc = ManuallyDrop::new(Gc::from_raw(
            proc.map_addr(|raw| raw & !TAG) as *mut GcInner<ProcedureInner>
        ));
        Gc::unroot(&proc_gc);
    }
}

/// Return an error in the case that a value is undefined
#[runtime_fn]
unsafe extern "C" fn error_unbound_variable(symbol: u32) -> *const () {
    let sym = Symbol(symbol);
    let condition = Exception::error(format!("undefined variable {sym}"));
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
unsafe extern "C" fn i64_to_number(value: i64) -> *const () {
    Value::into_raw(Value::from(num::Number::from(value)))
}

#[runtime_fn]
unsafe extern "C" fn i128_to_number(lo: i64, hi: i64) -> *const () {
    let value = ((hi as i128) << 64) | ((lo as u64) as i128);
    Value::into_raw(Value::from(num::Number::from(value)))
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

#[runtime_fn]
unsafe extern "C" fn call_known_1x0(func: usize, arg1: *const (), error: *mut Value) -> *const () {
    unsafe {
        let func: fn(&Value) -> Result<(), Exception> = std::mem::transmute(func);
        let arg1 = ManuallyDrop::new(Value::from_raw(arg1));
        match (func)(&arg1) {
            Ok(()) => Value::into_raw(Value::from(true)),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn call_known_2x0(
    func: usize,
    arg1: *const (),
    arg2: *const (),
    error: *mut Value,
) -> *const () {
    unsafe {
        let func: fn(&Value, &Value) -> Result<(), Exception> = std::mem::transmute(func);
        let arg1 = ManuallyDrop::new(Value::from_raw(arg1));
        let arg2 = ManuallyDrop::new(Value::from_raw(arg2));
        match (func)(&arg1, &arg2) {
            Ok(()) => Value::into_raw(Value::from(true)),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn call_known_3x0(
    func: usize,
    arg1: *const (),
    arg2: *const (),
    arg3: *const (),
    error: *mut Value,
) -> *const () {
    unsafe {
        let func: fn(&Value, &Value, &Value) -> Result<(), Exception> = std::mem::transmute(func);
        let arg1 = ManuallyDrop::new(Value::from_raw(arg1));
        let arg2 = ManuallyDrop::new(Value::from_raw(arg2));
        let arg3 = ManuallyDrop::new(Value::from_raw(arg3));
        match (func)(&arg1, &arg2, &arg3) {
            Ok(()) => Value::into_raw(Value::from(true)),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn call_known_0x1(func: usize, error: *mut Value) -> *const () {
    unsafe {
        let func: fn() -> Result<Value, Exception> = std::mem::transmute(func);
        match (func)() {
            Ok(res) => Value::into_raw(res),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn call_known_1x1(func: usize, arg1: *const (), error: *mut Value) -> *const () {
    unsafe {
        let func: fn(&Value) -> Result<Value, Exception> = std::mem::transmute(func);
        let arg1 = ManuallyDrop::new(Value::from_raw(arg1));
        match (func)(&arg1) {
            Ok(res) => Value::into_raw(res),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn call_known_2x1(
    func: usize,
    arg1: *const (),
    arg2: *const (),
    error: *mut Value,
) -> *const () {
    unsafe {
        let func: fn(&Value, &Value) -> Result<Value, Exception> = std::mem::transmute(func);
        let arg1 = ManuallyDrop::new(Value::from_raw(arg1));
        let arg2 = ManuallyDrop::new(Value::from_raw(arg2));
        match (func)(&arg1, &arg2) {
            Ok(res) => Value::into_raw(res),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn call_known_3x1(
    func: usize,
    arg1: *const (),
    arg2: *const (),
    arg3: *const (),
    error: *mut Value,
) -> *const () {
    unsafe {
        let func: fn(&Value, &Value, &Value) -> Result<Value, Exception> =
            std::mem::transmute(func);
        let arg1 = ManuallyDrop::new(Value::from_raw(arg1));
        let arg2 = ManuallyDrop::new(Value::from_raw(arg2));
        let arg3 = ManuallyDrop::new(Value::from_raw(arg3));
        match (func)(&arg1, &arg2, &arg3) {
            Ok(res) => Value::into_raw(res),
            Err(condition) => {
                error.write(condition.into());
                Value::into_raw(Value::undefined())
            }
        }
    }
}
