//! Procedures, continuation and user, and applying values to those procedures.

use crate::{
    env::Local,
    exceptions::{Exception, raise},
    gc::{Gc, GcInner, Trace},
    lists::{self, Pair, list_to_vec},
    num::Number,
    ports::{BufferMode, Port, Transcoder},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::BridgeFnDebugInfo,
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    syntax::Span,
    value::Value,
    vectors::Vector,
};
use parking_lot::RwLock;
use scheme_rs_macros::{cps_bridge, maybe_async, maybe_await};
use std::{
    collections::HashMap,
    fmt,
    sync::{
        Arc, OnceLock,
        atomic::{AtomicUsize, Ordering},
    },
};

/// A function pointer to a generated continuation.
pub(crate) type ContinuationPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    dyn_state: *mut DynamicState,
) -> *mut Application;

/// A function pointer to a generated user function.
pub(crate) type UserPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    dyn_state: *mut DynamicState,
    k: Value,
) -> *mut Application;

/// A function pointer to a sync Rust bridge function.
pub type BridgePtr = for<'a> fn(
    runtime: &'a Runtime,
    env: &'a [Value],
    // TODO: Make this a Vec
    args: &'a [Value],
    rest_args: &'a [Value],
    dyn_state: &mut DynamicState,
    k: Value,
) -> Application;

/// A function pointer to an async Rust bridge function.
#[cfg(feature = "async")]
pub type AsyncBridgePtr = for<'a> fn(
    runtime: &'a Runtime,
    env: &'a [Value],
    args: &'a [Value],
    rest_args: &'a [Value],
    dyn_state: &'a mut DynamicState,
    k: Value,
) -> futures::future::BoxFuture<'a, Application>;

#[derive(Copy, Clone, Debug)]
pub(crate) enum FuncPtr {
    /// A function defined in Rust
    Bridge(BridgePtr),
    #[cfg(feature = "async")]
    /// An async function defined in Rust
    AsyncBridge(AsyncBridgePtr),
    /// A JIT compiled user function
    User(UserPtr),
    /// A JIT compiled (or occasionally defined in Rust) continuation
    Continuation(ContinuationPtr),
    /// A continuation that exits a prompt. Can be dynamically replaced
    PromptBarrier {
        barrier_id: usize,
        k: ContinuationPtr,
    },
}

impl From<BridgePtr> for FuncPtr {
    fn from(ptr: BridgePtr) -> Self {
        Self::Bridge(ptr)
    }
}

#[cfg(feature = "async")]
impl From<AsyncBridgePtr> for FuncPtr {
    fn from(ptr: AsyncBridgePtr) -> Self {
        Self::AsyncBridge(ptr)
    }
}

impl From<UserPtr> for FuncPtr {
    fn from(ptr: UserPtr) -> Self {
        Self::User(ptr)
    }
}

enum JitFuncPtr {
    Continuation(ContinuationPtr),
    User(UserPtr),
}

#[derive(Clone, Trace)]
#[repr(align(16))]
pub(crate) struct ProcedureInner {
    /// The runtime the Procedure is defined in. This is necessary to ensure that
    /// dropping the runtime does not de-allocate the function pointer for this
    /// procedure.
    // TODO: Do we make this optional in the case of bridge functions?
    pub(crate) runtime: Runtime,
    /// Environmental variables used by the procedure.
    pub(crate) env: Vec<Value>,
    /// Fuction pointer to the body of the procecure.
    #[trace(skip)]
    pub(crate) func: FuncPtr,
    /// Number of required arguments to this procedure.
    pub(crate) num_required_args: usize,
    /// Whether or not this is a variadic function.
    pub(crate) variadic: bool,
    /// Whether or not this function is a variable transformer.
    pub(crate) is_variable_transformer: bool,
    /// Debug information for this function. Only applicable if the function is
    /// a user function, i.e. not a continuation.
    pub(crate) debug_info: Option<Arc<ProcDebugInfo>>,
}

impl ProcedureInner {
    pub(crate) fn new(
        runtime: Runtime,
        env: Vec<Value>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<ProcDebugInfo>>,
    ) -> Self {
        Self {
            runtime,
            env,
            func,
            num_required_args,
            variadic,
            is_variable_transformer: false,
            debug_info,
        }
    }

    pub fn is_continuation(&self) -> bool {
        matches!(
            self.func,
            FuncPtr::Continuation(_) | FuncPtr::PromptBarrier { .. }
        )
    }

    pub(crate) fn prepare_args(
        &self,
        mut args: Vec<Value>,
        dyn_state: &mut DynamicState,
    ) -> Result<(Vec<Value>, Option<Value>), Application> {
        // Extract the continuation, if it is required
        let cont = (!self.is_continuation()).then(|| args.pop().unwrap());

        // Error if the number of arguments provided is incorrect
        if args.len() < self.num_required_args {
            return Err(raise(
                self.runtime.clone(),
                Exception::wrong_num_of_args(self.num_required_args, args.len()).into(),
                dyn_state,
            ));
        }

        if !self.variadic && args.len() > self.num_required_args {
            return Err(raise(
                self.runtime.clone(),
                Exception::wrong_num_of_args(self.num_required_args, args.len()).into(),
                dyn_state,
            ));
        }

        Ok((args, cont))
    }

    #[cfg(feature = "async")]
    async fn apply_async_bridge(
        &self,
        func: AsyncBridgePtr,
        args: &[Value],
        dyn_state: &mut DynamicState,
        k: Value,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(&self.runtime, &self.env, args, rest_args, dyn_state, k).await
    }

    fn apply_sync_bridge(
        &self,
        func: BridgePtr,
        args: &[Value],
        dyn_state: &mut DynamicState,
        k: Value,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(&self.runtime, &self.env, args, rest_args, dyn_state, k)
    }

    fn apply_jit(
        &self,
        func: JitFuncPtr,
        mut args: Vec<Value>,
        dyn_state: &mut DynamicState,
        k: Option<Value>,
    ) -> Application {
        if self.variadic {
            let mut rest_args = Value::null();
            let extra_args = args.len() - self.num_required_args;
            for _ in 0..extra_args {
                rest_args = Value::from(Pair::new(args.pop().unwrap(), rest_args, false));
            }
            args.push(rest_args);
        }

        let app = match func {
            JitFuncPtr::Continuation(sync_fn) => unsafe {
                (sync_fn)(
                    Gc::as_ptr(&self.runtime.0),
                    self.env.as_ptr(),
                    args.as_ptr(),
                    dyn_state as *mut DynamicState,
                )
            },
            JitFuncPtr::User(sync_fn) => unsafe {
                (sync_fn)(
                    Gc::as_ptr(&self.runtime.0),
                    self.env.as_ptr(),
                    args.as_ptr(),
                    dyn_state as *mut DynamicState,
                    Value::from_raw(Value::as_raw(k.as_ref().unwrap())),
                )
            },
        };

        unsafe { *Box::from_raw(app) }
    }

    #[maybe_async]
    pub fn apply(&self, args: Vec<Value>, dyn_state: &mut DynamicState) -> Application {
        let (args, k) = match self.prepare_args(args, dyn_state) {
            Ok(args) => args,
            Err(raised) => return raised,
        };

        match self.func {
            FuncPtr::Bridge(sbridge) => {
                self.apply_sync_bridge(sbridge, &args, dyn_state, k.unwrap())
            }
            #[cfg(feature = "async")]
            FuncPtr::AsyncBridge(abridge) => {
                self.apply_async_bridge(abridge, &args, dyn_state, k.unwrap())
                    .await
            }
            FuncPtr::User(user) => self.apply_jit(JitFuncPtr::User(user), args, dyn_state, k),
            FuncPtr::Continuation(k) => {
                dyn_state.pop_marks();
                self.apply_jit(JitFuncPtr::Continuation(k), args, dyn_state, None)
            }
            FuncPtr::PromptBarrier { barrier_id: id, k } => {
                dyn_state.pop_marks();
                match dyn_state.pop_dyn_stack() {
                    Some(DynStackElem::PromptBarrier(PromptBarrier {
                        barrier_id,
                        replaced_k,
                    })) if barrier_id == id => {
                        return Application::new(replaced_k, args);
                    }
                    Some(other) => dyn_state.push_dyn_stack(other),
                    _ => (),
                }
                self.apply_jit(JitFuncPtr::Continuation(k), args, dyn_state, None)
            }
        }
    }

    #[cfg(feature = "async")]
    /// Attempt to call the function, and throw an error if is async
    pub fn apply_sync(&self, args: Vec<Value>, dyn_state: &mut DynamicState) -> Application {
        let (args, k) = match self.prepare_args(args, dyn_state) {
            Ok(args) => args,
            Err(raised) => return raised,
        };

        match self.func {
            FuncPtr::Bridge(sbridge) => {
                self.apply_sync_bridge(sbridge, &args, dyn_state, k.unwrap())
            }
            FuncPtr::AsyncBridge(_) => raise(
                self.runtime.clone(),
                Exception::error("attempt to apply async function in a sync-only context").into(),
                dyn_state,
            ),
            FuncPtr::User(user) => self.apply_jit(JitFuncPtr::User(user), args, dyn_state, k),
            FuncPtr::Continuation(k) => {
                dyn_state.pop_marks();
                self.apply_jit(JitFuncPtr::Continuation(k), args, dyn_state, None)
            }
            FuncPtr::PromptBarrier { barrier_id: id, k } => {
                dyn_state.pop_marks();
                match dyn_state.pop_dyn_stack() {
                    Some(DynStackElem::PromptBarrier(PromptBarrier {
                        barrier_id,
                        replaced_k,
                    })) if barrier_id == id => {
                        return Application::new(replaced_k, args);
                    }
                    Some(other) => dyn_state.push_dyn_stack(other),
                    _ => (),
                }
                self.apply_jit(JitFuncPtr::Continuation(k), args, dyn_state, None)
            }
        }
    }
}

impl fmt::Debug for ProcedureInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_continuation() {
            return write!(f, "continuation");
        }

        let Some(ref debug_info) = self.debug_info else {
            write!(f, "(<lambda>")?;
            for i in 0..self.num_required_args {
                write!(f, " ${i}")?;
            }
            if self.variadic {
                write!(f, " . ${}", self.num_required_args)?;
            }
            return write!(f, ")");
        };

        write!(f, "({}", debug_info.name)?;

        if let Some((last, args)) = debug_info.args.split_last() {
            for arg in args {
                write!(f, " {arg}")?;
            }
            if self.variadic {
                write!(f, " .")?;
            }
            write!(f, " {last}")?;
        }

        write!(f, ") at {}", debug_info.location)
    }
}

/// The runtime representation of a Procedure, which can be either a user
/// function or a continuation. Contains a reference to all of the environmental
/// variables used in the body, along with a function pointer to the body of the
/// procedure.
#[derive(Clone, Trace)]
pub struct Procedure(pub(crate) Gc<ProcedureInner>);

impl Procedure {
    #[allow(private_bounds)]
    /// Creates a new procedure. `func` must be a [`BridgePtr`] or an
    /// `AsyncBridgePtr` if `async` is enabled.
    pub fn new(
        runtime: Runtime,
        env: Vec<Value>,
        func: impl Into<FuncPtr>,
        num_required_args: usize,
        variadic: bool,
    ) -> Self {
        Self::with_debug_info(runtime, env, func.into(), num_required_args, variadic, None)
    }

    pub(crate) fn with_debug_info(
        runtime: Runtime,
        env: Vec<Value>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<ProcDebugInfo>>,
    ) -> Self {
        Self(Gc::new(ProcedureInner {
            runtime,
            env,
            func,
            num_required_args,
            variadic,
            is_variable_transformer: false,
            debug_info,
        }))
    }

    pub fn get_runtime(&self) -> Runtime {
        self.0.runtime.clone()
    }

    pub fn get_formals(&self) -> (usize, bool) {
        (self.0.num_required_args, self.0.variadic)
    }

    pub fn get_debug_info(&self) -> Option<Arc<ProcDebugInfo>> {
        self.0.debug_info.clone()
    }

    /// # Safety
    /// `args` must be a valid pointer and contain num_required_args + variadic entries.
    pub(crate) unsafe fn collect_args(&self, args: *const Value) -> Vec<Value> {
        // I don't really like this, but what are you gonna do?
        let (num_required_args, variadic) = self.get_formals();

        unsafe {
            let mut collected_args: Vec<_> = (0..num_required_args)
                .map(|i| args.add(i).as_ref().unwrap().clone())
                .collect();

            if variadic {
                let rest_args = args.add(num_required_args).as_ref().unwrap().clone();
                let mut vec = Vec::new();
                lists::list_to_vec(&rest_args, &mut vec);
                collected_args.extend(vec);
            }

            collected_args
        }
    }

    pub fn is_variable_transformer(&self) -> bool {
        self.0.is_variable_transformer
    }

    pub fn is_continuation(&self) -> bool {
        matches!(self.0.func, FuncPtr::Continuation(_))
    }

    /// Applies `args` to the procedure and returns the values it evaluates to.
    #[maybe_async]
    pub fn call(&self, args: &[Value]) -> Result<Vec<Value>, Exception> {
        let mut args = args.to_vec();

        args.push(halt_continuation(self.get_runtime()));

        maybe_await!(Application::new(self.clone(), args).eval(&mut DynamicState::default()))
    }

    #[cfg(feature = "async")]
    pub fn call_sync(&self, args: &[Value]) -> Result<Vec<Value>, Exception> {
        let mut args = args.to_vec();

        args.push(halt_continuation(self.get_runtime()));

        Application::new(self.clone(), args).eval_sync(&mut DynamicState::default())
    }
}

static HALT_CONTINUATION: OnceLock<Value> = OnceLock::new();

/// Return a continuation that returns its expressions.
pub fn halt_continuation(runtime: Runtime) -> Value {
    unsafe extern "C" fn halt(
        _runtime: *mut GcInner<RwLock<RuntimeInner>>,
        _env: *const Value,
        args: *const Value,
        _dyn_state: *mut DynamicState,
    ) -> *mut Application {
        unsafe { crate::runtime::halt(Value::into_raw(args.read())) }
    }

    HALT_CONTINUATION
        .get_or_init(move || {
            Value::from(Procedure(Gc::new(ProcedureInner::new(
                runtime,
                Vec::new(),
                FuncPtr::Continuation(halt),
                0,
                true,
                None,
            ))))
        })
        .clone()
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PartialEq for Procedure {
    fn eq(&self, rhs: &Procedure) -> bool {
        Gc::ptr_eq(&self.0, &rhs.0)
    }
}

pub(crate) enum OpType {
    Proc(Procedure),
    HaltOk,
    HaltErr,
}

/// An application of a function to a given set of values.
pub struct Application {
    /// The operator being applied to.
    op: OpType,
    /// The arguments being applied to the operator.
    args: Vec<Value>,
}

impl Application {
    pub fn new(op: Procedure, args: Vec<Value>) -> Self {
        Self {
            op: OpType::Proc(op),
            args,
        }
    }

    pub fn halt_ok(args: Vec<Value>) -> Self {
        Self {
            op: OpType::HaltOk,
            args,
        }
    }

    pub fn halt_err(arg: Value) -> Self {
        Self {
            op: OpType::HaltErr,
            args: vec![arg],
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    #[maybe_async]
    pub fn eval(mut self, dyn_state: &mut DynamicState) -> Result<Vec<Value>, Exception> {
        loop {
            let op = match self.op {
                OpType::Proc(proc) => proc,
                OpType::HaltOk => return Ok(self.args),
                OpType::HaltErr => {
                    return Err(Exception(self.args.pop().unwrap()));
                }
            };
            self = maybe_await!(op.0.apply(self.args, dyn_state));
        }
    }

    #[cfg(feature = "async")]
    /// Just like [eval] but throws an error if we encounter an async function.
    pub fn eval_sync(mut self, dyn_state: &mut DynamicState) -> Result<Vec<Value>, Exception> {
        loop {
            let op = match self.op {
                OpType::Proc(proc) => proc,
                OpType::HaltOk => return Ok(self.args),
                OpType::HaltErr => {
                    return Err(Exception(self.args.pop().unwrap()));
                }
            };
            self = op.0.apply_sync(self.args, dyn_state);
        }
    }
}

/// Debug information associated with a procedure, including its name, argument
/// names, and source location.
#[derive(Debug)]
pub struct ProcDebugInfo {
    /// The name of the function.
    pub name: Symbol,
    /// Named arguments for the function.
    pub args: Vec<Local>,
    /// Location of the function definition
    pub location: Span,
}

impl ProcDebugInfo {
    pub fn new(name: Option<Symbol>, args: Vec<Local>, location: Span) -> Self {
        Self {
            name: name.unwrap_or_else(|| Symbol::intern("<lambda>")),
            args,
            location,
        }
    }

    pub fn from_bridge_fn(name: &'static str, debug_info: BridgeFnDebugInfo) -> Self {
        Self {
            name: Symbol::intern(name),
            args: debug_info
                .args
                .iter()
                .map(|arg| Local::gensym_with_name(Symbol::intern(arg)))
                .collect(),
            location: Span {
                line: debug_info.line,
                column: debug_info.column as usize,
                offset: debug_info.offset,
                file: std::sync::Arc::new(debug_info.file.to_string()),
            },
        }
    }
}

#[cps_bridge(def = "apply arg1 . args", lib = "(rnrs base builtins (6))")]
pub fn apply(
    _runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    if rest_args.is_empty() {
        return Err(Exception::wrong_num_of_args(2, args.len()));
    }
    let op: Procedure = args[0].clone().try_into()?;
    let (last, args) = rest_args.split_last().unwrap();
    let mut args = args.to_vec();
    list_to_vec(last, &mut args);
    args.push(k);
    Ok(Application::new(op.clone(), args))
}

////////////////////////////////////////////////////////////////////////////////
//
// Dynamic state
//

/// The dynamic state of the running program, including winders, exception
/// handlers, and continuation marks.
#[derive(Clone, Debug, Trace)]
pub struct DynamicState {
    dyn_stack: Vec<DynStackElem>,
    cont_marks: Vec<HashMap<Symbol, Value>>,
}

impl DynamicState {
    pub fn new() -> Self {
        Self {
            dyn_stack: Vec::new(),
            // Procedures returned by the JIT compiler are delimited
            // continuations (of sorts), and therefore we need to preallocate
            // the initial marks for them since there's no mechanism to allocate
            // for them when they're run.
            cont_marks: vec![HashMap::new()],
        }
    }

    /// This is the only method you can use to create continuations, in order to
    /// ensure that a continuation isn't allocated without a corresponding push
    /// to cont_marks
    pub(crate) fn new_k(
        &mut self,
        runtime: Runtime,
        env: Vec<Value>,
        k: ContinuationPtr,
        num_required_args: usize,
        variadic: bool,
    ) -> Procedure {
        self.push_marks();
        Procedure::with_debug_info(
            runtime,
            env,
            FuncPtr::Continuation(k),
            num_required_args,
            variadic,
            None,
        )
    }

    pub(crate) fn push_marks(&mut self) {
        self.cont_marks.push(HashMap::new());
    }

    pub(crate) fn pop_marks(&mut self) {
        self.cont_marks.pop();
    }

    pub(crate) fn current_marks(&self, tag: Symbol) -> Vec<Value> {
        self.cont_marks
            .iter()
            .rev()
            .flat_map(|marks| marks.get(&tag).cloned())
            .collect()
    }

    pub(crate) fn set_continuation_mark(&mut self, tag: Symbol, val: Value) {
        self.cont_marks.last_mut().unwrap().insert(tag, val);
    }

    // TODO: We should certainly try to optimize these functions. Linear
    // searching isn't _great_, although in practice I can't imagine this stack
    // will ever get very large.

    pub fn current_exception_handler(&self) -> Option<Procedure> {
        self.dyn_stack.iter().rev().find_map(|elem| match elem {
            DynStackElem::ExceptionHandler(proc) => Some(proc.clone()),
            _ => None,
        })
    }

    pub fn current_input_port(&self) -> Port {
        self.dyn_stack
            .iter()
            .rev()
            .find_map(|elem| match elem {
                DynStackElem::CurrentInputPort(port) => Some(port.clone()),
                _ => None,
            })
            .unwrap_or_else(|| {
                Port::new(
                    "<stdin>",
                    #[cfg(not(feature = "async"))]
                    std::io::stdin(),
                    #[cfg(feature = "tokio")]
                    tokio::io::stdin(),
                    BufferMode::Line,
                    Some(Transcoder::native()),
                )
            })
    }

    pub fn current_output_port(&self) -> Port {
        self.dyn_stack
            .iter()
            .rev()
            .find_map(|elem| match elem {
                DynStackElem::CurrentOutputPort(port) => Some(port.clone()),
                _ => None,
            })
            .unwrap_or_else(|| {
                Port::new(
                    "<stdout>",
                    #[cfg(not(feature = "async"))]
                    std::io::stdout(),
                    #[cfg(feature = "tokio")]
                    tokio::io::stdout(),
                    // TODO: Probably should change this to line, but that
                    // doesn't play nicely with rustyline
                    BufferMode::None,
                    Some(Transcoder::native()),
                )
            })
    }

    pub(crate) fn push_dyn_stack(&mut self, elem: DynStackElem) {
        self.dyn_stack.push(elem);
    }

    pub(crate) fn pop_dyn_stack(&mut self) -> Option<DynStackElem> {
        self.dyn_stack.pop()
    }

    pub(crate) fn dyn_stack_get(&self, idx: usize) -> Option<&DynStackElem> {
        self.dyn_stack.get(idx)
    }

    pub(crate) fn dyn_stack_last(&self) -> Option<&DynStackElem> {
        self.dyn_stack.last()
    }

    pub(crate) fn dyn_stack_len(&self) -> usize {
        self.dyn_stack.len()
    }

    pub(crate) fn dyn_stack_is_empty(&self) -> bool {
        self.dyn_stack.is_empty()
    }
}

impl Default for DynamicState {
    fn default() -> Self {
        Self::new()
    }
}

impl SchemeCompatible for DynamicState {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "$dyn-stack", sealed: true, opaque: true)
    }
}

#[derive(Clone, Debug, PartialEq, Trace)]
pub(crate) enum DynStackElem {
    Prompt(Prompt),
    PromptBarrier(PromptBarrier),
    Winder(Winder),
    ExceptionHandler(Procedure),
    CurrentInputPort(Port),
    CurrentOutputPort(Port),
}

pub(crate) unsafe extern "C" fn pop_dyn_stack(
    _runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        // env[0] is the continuation
        let k: Procedure = env.as_ref().unwrap().clone().try_into().unwrap();

        dyn_state.as_mut().unwrap_unchecked().pop_dyn_stack();

        let args = k.collect_args(args);
        let app = Application::new(k, args);

        Box::into_raw(Box::new(app))
    }
}

#[cps_bridge(def = "print-trace", lib = "(rnrs base builtins (6))")]
pub fn print_trace(
    _runtime: &Runtime,
    _env: &[Value],
    _args: &[Value],
    _rest_args: &[Value],
    dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    println!(
        "trace: {:#?}",
        dyn_state.current_marks(Symbol::intern("trace"))
    );
    Ok(Application::new(k.try_into()?, vec![]))
}

////////////////////////////////////////////////////////////////////////////////
//
// Call with current continuation
//

#[cps_bridge(
    def = "call-with-current-continuation proc",
    lib = "(rnrs base builtins (6))"
)]
pub fn call_with_current_continuation(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let [proc] = args else { unreachable!() };
    let proc: Procedure = proc.clone().try_into()?;

    let (req_args, variadic) = {
        let k: Procedure = k.clone().try_into()?;
        k.get_formals()
    };

    let dyn_state = Value::from(Record::from_rust_type(dyn_state.clone()));

    let escape_procedure = Procedure::new(
        runtime.clone(),
        vec![k.clone(), dyn_state],
        FuncPtr::Bridge(escape_procedure),
        req_args,
        variadic,
    );

    let app = Application::new(proc, vec![Value::from(escape_procedure), k]);

    Ok(app)
}

/// Prepare the continuation for call/cc. Clones the continuation environment
/// and creates a closure that calls the appropriate winders.
#[cps_bridge]
fn escape_procedure(
    runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    dyn_state: &mut DynamicState,
    _k: Value,
) -> Result<Application, Exception> {
    // env[0] is the continuation
    let k = env[0].clone();

    // env[1] is the dyn stack of the continuation
    let saved_dyn_state_val = env[1].clone();
    let saved_dyn_state = saved_dyn_state_val
        .try_to_rust_type::<DynamicState>()
        .unwrap();
    let saved_dyn_state_read = saved_dyn_state.as_ref();
    dyn_state.cont_marks = saved_dyn_state_read.cont_marks.clone();

    // Clone the continuation
    let k: Procedure = k.try_into().unwrap();

    let args = args.iter().chain(rest_args).cloned().collect::<Vec<_>>();

    // Simple optimization: check if we're in the same dyn stack
    if dyn_state.dyn_stack_len() == saved_dyn_state_read.dyn_stack_len()
        && dyn_state.dyn_stack_last() == saved_dyn_state_read.dyn_stack_last()
    {
        Ok(Application::new(k, args))
    } else {
        let args = Value::from(args);
        let k = dyn_state.new_k(
            runtime.clone(),
            vec![Value::from(k), args, saved_dyn_state_val],
            unwind,
            0,
            false,
        );
        Ok(Application::new(k, Vec::new()))
    }
}

unsafe extern "C" fn unwind(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        // env[0] is the ultimate continuation
        let k = env.as_ref().unwrap().clone();

        // env[1] are the arguments to pass to k
        let args = env.add(1).as_ref().unwrap().clone();

        // env[2] is the stack we are trying to reach
        let dest_stack_val = env.add(2).as_ref().unwrap().clone();
        let dest_stack = dest_stack_val
            .clone()
            .try_to_rust_type::<DynamicState>()
            .unwrap();
        let dest_stack_read = dest_stack.as_ref();

        let dyn_state = dyn_state.as_mut().unwrap_unchecked();

        while !dyn_state.dyn_stack_is_empty()
            && (dyn_state.dyn_stack_len() > dest_stack_read.dyn_stack_len()
                || dyn_state.dyn_stack_last()
                    != dest_stack_read.dyn_stack_get(dyn_state.dyn_stack_len() - 1))
        {
            match dyn_state.pop_dyn_stack() {
                None => {
                    break;
                }
                Some(DynStackElem::Winder(winder)) => {
                    // Call the out winder while unwinding
                    let app = Application::new(
                        winder.out_thunk,
                        vec![Value::from(dyn_state.new_k(
                            Runtime::from_raw_inc_rc(runtime),
                            vec![k, args, dest_stack_val],
                            unwind,
                            0,
                            false,
                        ))],
                    );
                    return Box::into_raw(Box::new(app));
                }
                _ => (),
            };
        }

        // Begin winding
        let app = Application::new(
            dyn_state.new_k(
                Runtime::from_raw_inc_rc(runtime),
                vec![k, args, dest_stack_val, Value::from(false)],
                wind,
                0,
                false,
            ),
            Vec::new(),
        );

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn wind(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        // env[0] is the ultimate continuation
        let k = env.as_ref().unwrap().clone();

        // env[1] are the arguments to pass to k
        let args = env.add(1).as_ref().unwrap().clone();

        // env[2] is the stack we are trying to reach
        let dest_stack_val = env.add(2).as_ref().unwrap().clone();
        let dest_stack = dest_stack_val.try_to_rust_type::<DynamicState>().unwrap();
        let dest_stack_read = dest_stack.as_ref();

        let dyn_state = dyn_state.as_mut().unwrap_unchecked();

        // env[3] is potentially a winder that we should push onto the dyn stack
        let winder = env.add(3).as_ref().unwrap().clone();
        if winder.is_true() {
            let winder = winder.try_to_rust_type::<Winder>().unwrap();
            dyn_state.push_dyn_stack(DynStackElem::Winder(winder.as_ref().clone()));
        }

        while dyn_state.dyn_stack_len() < dest_stack_read.dyn_stack_len() {
            match dest_stack_read
                .dyn_stack_get(dyn_state.dyn_stack_len())
                .cloned()
            {
                None => {
                    break;
                }
                Some(DynStackElem::Winder(winder)) => {
                    // Call the in winder while winding
                    let app = Application::new(
                        winder.in_thunk.clone(),
                        vec![Value::from(dyn_state.new_k(
                            Runtime::from_raw_inc_rc(runtime),
                            vec![
                                k,
                                args,
                                dest_stack_val,
                                Value::from(Record::from_rust_type(winder)),
                            ],
                            wind,
                            0,
                            false,
                        ))],
                    );
                    return Box::into_raw(Box::new(app));
                }
                Some(elem) => dyn_state.push_dyn_stack(elem),
            }
        }

        let args: Vector = args.try_into().unwrap();
        let args = args.0.vec.read().to_vec();

        Box::into_raw(Box::new(Application::new(k.try_into().unwrap(), args)))
    }
}

unsafe extern "C" fn call_consumer_with_values(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        // env[0] is the consumer
        let consumer = env.as_ref().unwrap().clone();
        let type_name = consumer.type_name();

        let consumer: Procedure = match consumer.try_into() {
            Ok(consumer) => consumer,
            _ => {
                let raised = raise(
                    Runtime::from_raw_inc_rc(runtime),
                    Exception::invalid_operator(type_name).into(),
                    dyn_state.as_mut().unwrap_unchecked(),
                );
                return Box::into_raw(Box::new(raised));
            }
        };

        // env[1] is the continuation
        let k = env.add(1).as_ref().unwrap().clone();

        let mut collected_args: Vec<_> = (0..consumer.0.num_required_args)
            .map(|i| args.add(i).as_ref().unwrap().clone())
            .collect();

        // I hate this constant going back and forth from variadic to list. I have
        // to figure out a way to make it consistent
        if consumer.0.variadic {
            let rest_args = args
                .add(consumer.0.num_required_args)
                .as_ref()
                .unwrap()
                .clone();
            let mut vec = Vec::new();
            list_to_vec(&rest_args, &mut vec);
            collected_args.extend(vec);
        }

        collected_args.push(k);

        Box::into_raw(Box::new(Application::new(consumer.clone(), collected_args)))
    }
}

#[cps_bridge(
    def = "call-with-values producer consumer",
    lib = "(rnrs base builtins (6))"
)]
pub fn call_with_values(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let [producer, consumer] = args else {
        return Err(Exception::wrong_num_of_args(2, args.len()));
    };

    let producer: Procedure = producer.clone().try_into()?;
    let consumer: Procedure = consumer.clone().try_into()?;

    // Get the details of the consumer:
    let (num_required_args, variadic) = { (consumer.0.num_required_args, consumer.0.variadic) };

    let call_consumer_closure = dyn_state.new_k(
        runtime.clone(),
        vec![Value::from(consumer), k],
        call_consumer_with_values,
        num_required_args,
        variadic,
    );

    Ok(Application::new(
        producer,
        vec![Value::from(call_consumer_closure)],
    ))
}

////////////////////////////////////////////////////////////////////////////////
//
// Dynamic wind
//

#[derive(Clone, Debug, Trace, PartialEq)]
pub(crate) struct Winder {
    pub(crate) in_thunk: Procedure,
    pub(crate) out_thunk: Procedure,
}

impl SchemeCompatible for Winder {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "$winder", sealed: true, opaque: true)
    }
}

#[cps_bridge(def = "dynamic-wind in body out", lib = "(rnrs base builtins (6))")]
pub fn dynamic_wind(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let [in_thunk_val, body_thunk_val, out_thunk_val] = args else {
        return Err(Exception::wrong_num_of_args(3, args.len()));
    };

    let in_thunk: Procedure = in_thunk_val.clone().try_into()?;
    let _: Procedure = body_thunk_val.clone().try_into()?;

    let call_body_thunk_cont = dyn_state.new_k(
        runtime.clone(),
        vec![
            in_thunk_val.clone(),
            body_thunk_val.clone(),
            out_thunk_val.clone(),
            k,
        ],
        call_body_thunk,
        0,
        true,
    );

    Ok(Application::new(
        in_thunk,
        vec![Value::from(call_body_thunk_cont)],
    ))
}

pub(crate) unsafe extern "C" fn call_body_thunk(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        // env[0] is the in thunk
        let in_thunk = env.as_ref().unwrap().clone();

        // env[1] is the body thunk
        let body_thunk: Procedure = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        // env[2] is the out thunk
        let out_thunk = env.add(2).as_ref().unwrap().clone();

        // env[3] is k, the continuation
        let k = env.add(3).as_ref().unwrap().clone();

        let dyn_state = dyn_state.as_mut().unwrap_unchecked();

        dyn_state.push_dyn_stack(DynStackElem::Winder(Winder {
            in_thunk: in_thunk.clone().try_into().unwrap(),
            out_thunk: out_thunk.clone().try_into().unwrap(),
        }));

        let k = dyn_state.new_k(
            Runtime::from_raw_inc_rc(runtime),
            vec![out_thunk, k],
            call_out_thunks,
            0,
            true,
        );

        let app = Application::new(body_thunk, vec![Value::from(k)]);

        Box::into_raw(Box::new(app))
    }
}

pub(crate) unsafe extern "C" fn call_out_thunks(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        // env[0] is the out thunk
        let out_thunk: Procedure = env.as_ref().unwrap().clone().try_into().unwrap();

        // env[1] is k, the remaining continuation
        let k = env.add(1).as_ref().unwrap().clone();

        // args[0] is the result of the body thunk
        let body_thunk_res = args.as_ref().unwrap().clone();

        let dyn_state = dyn_state.as_mut().unwrap_unchecked();
        dyn_state.pop_dyn_stack();

        let cont = dyn_state.new_k(
            Runtime::from_raw_inc_rc(runtime),
            vec![body_thunk_res, k],
            forward_body_thunk_result,
            0,
            true,
        );

        let app = Application::new(out_thunk, vec![Value::from(cont)]);

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn forward_body_thunk_result(
    _runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    _dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        // env[0] is the result of the body thunk
        let body_thunk_res = env.as_ref().unwrap().clone();
        // env[1] is k, the continuation.
        let k: Procedure = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        let mut args = Vec::new();
        list_to_vec(&body_thunk_res, &mut args);

        Box::into_raw(Box::new(Application::new(k, args)))
    }
}

////////////////////////////////////////////////////////////////////////////////
//
// Prompts and delimited continuations
//

#[derive(Clone, Debug, PartialEq, Trace)]
pub(crate) struct Prompt {
    tag: Symbol,
    barrier_id: usize,
    handler: Procedure,
    handler_k: Procedure,
}

#[derive(Clone, Debug, PartialEq, Trace)]
pub(crate) struct PromptBarrier {
    barrier_id: usize,
    replaced_k: Procedure,
}

static BARRIER_ID: AtomicUsize = AtomicUsize::new(0);

#[cps_bridge(
    def = "call-with-prompt tag thunk handler",
    lib = "(rnrs base builtins (6))"
)]
pub fn call_with_prompt(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let [tag, thunk, handler] = args else {
        unreachable!()
    };

    let k_proc: Procedure = k.clone().try_into().unwrap();
    let (req_args, variadic) = k_proc.get_formals();
    let tag: Symbol = tag.clone().try_into().unwrap();

    let barrier_id = BARRIER_ID.fetch_add(1, Ordering::Relaxed);

    dyn_state.push_dyn_stack(DynStackElem::Prompt(Prompt {
        tag,
        handler: handler.clone().try_into().unwrap(),
        barrier_id,
        handler_k: k.clone().try_into()?,
    }));

    dyn_state.push_marks();

    let prompt_barrier = Procedure::new(
        runtime.clone(),
        vec![k],
        FuncPtr::PromptBarrier {
            barrier_id,
            k: pop_dyn_stack,
        },
        req_args,
        variadic,
    );

    Ok(Application::new(
        thunk.clone().try_into().unwrap(),
        vec![Value::from(prompt_barrier)],
    ))
}

#[cps_bridge(def = "abort-to-prompt tag", lib = "(rnrs base builtins (6))")]
pub fn abort_to_prompt(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let [tag] = args else { unreachable!() };

    let unwind_to_prompt = dyn_state.new_k(
        runtime.clone(),
        vec![
            k,
            tag.clone(),
            Value::from(Record::from_rust_type(dyn_state.clone())),
        ],
        unwind_to_prompt,
        0,
        false,
    );

    Ok(Application::new(unwind_to_prompt, Vec::new()))
}

unsafe extern "C" fn unwind_to_prompt(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        // env[0] is continuation
        let k = env.as_ref().unwrap().clone();
        // env[1] is the prompt tag
        let tag: Symbol = env.add(1).as_ref().unwrap().clone().try_into().unwrap();
        // env[2] is the saved dyn stack
        let saved_dyn_state = env.add(2).as_ref().unwrap().clone();

        let dyn_state = dyn_state.as_mut().unwrap_unchecked();

        loop {
            let app = match dyn_state.pop_dyn_stack() {
                None => {
                    // If the stack is empty, we should return the error
                    Application::halt_err(Value::from(Exception::error(format!(
                        "No prompt tag {tag} found"
                    ))))
                }
                Some(DynStackElem::Prompt(Prompt {
                    tag: prompt_tag,
                    barrier_id,
                    handler,
                    handler_k,
                })) if prompt_tag == tag => {
                    let saved_dyn_state =
                        saved_dyn_state.try_to_rust_type::<DynamicState>().unwrap();
                    let prompt_delimited_dyn_state = DynamicState {
                        dyn_stack: saved_dyn_state.as_ref().dyn_stack
                            [dyn_state.dyn_stack_len() + 1..]
                            .to_vec(),
                        cont_marks: saved_dyn_state.cont_marks.clone(),
                    };
                    let (req_args, var) = {
                        let k_proc: Procedure = k.clone().try_into().unwrap();
                        k_proc.get_formals()
                    };
                    Application::new(
                        handler,
                        vec![
                            Value::from(Procedure::new(
                                Runtime::from_raw_inc_rc(runtime),
                                vec![
                                    k,
                                    Value::from(Number::from(barrier_id)),
                                    Value::from(Record::from_rust_type(prompt_delimited_dyn_state)),
                                ],
                                FuncPtr::Bridge(delimited_continuation),
                                req_args,
                                var,
                            )),
                            Value::from(handler_k),
                        ],
                    )
                }
                Some(DynStackElem::Winder(winder)) => {
                    // If this is a winder, we should call the out winder while unwinding
                    Application::new(
                        winder.out_thunk,
                        vec![Value::from(dyn_state.new_k(
                            Runtime::from_raw_inc_rc(runtime),
                            vec![k, Value::from(tag), saved_dyn_state],
                            unwind_to_prompt,
                            0,
                            false,
                        ))],
                    )
                }
                _ => continue,
            };
            return Box::into_raw(Box::new(app));
        }
    }
}

#[cps_bridge]
fn delimited_continuation(
    runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    // env[0] is the delimited continuation
    let dk = env[0].clone();

    // env[1] is the barrier Id
    let barrier_id: Arc<Number> = env[1].clone().try_into()?;
    let barrier_id: usize = barrier_id.as_ref().try_into()?;

    // env[2] is the dyn stack of the continuation
    let saved_dyn_state_val = env[2].clone();
    let saved_dyn_state = saved_dyn_state_val
        .try_to_rust_type::<DynamicState>()
        .unwrap();
    let saved_dyn_state_read = saved_dyn_state.as_ref();
    // Restore continuation marks
    dyn_state.cont_marks = saved_dyn_state_read.cont_marks.clone();

    let args = args.iter().chain(rest_args).cloned().collect::<Vec<_>>();

    dyn_state.push_dyn_stack(DynStackElem::PromptBarrier(PromptBarrier {
        barrier_id,
        replaced_k: k.try_into()?,
    }));

    // Simple optimization: if the saved dyn stack is empty, we
    // can just call the delimited continuation
    if saved_dyn_state_read.dyn_stack_is_empty() {
        Ok(Application::new(dk.try_into()?, args))
    } else {
        let args = Value::from(args);
        let k = dyn_state.new_k(
            runtime.clone(),
            vec![
                dk,
                args,
                saved_dyn_state_val,
                Value::from(Number::from(0)),
                Value::from(false),
            ],
            wind_delim,
            0,
            false,
        );
        Ok(Application::new(k, Vec::new()))
    }
}

unsafe extern "C" fn wind_delim(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        // env[0] is the ultimate continuation
        let k = env.as_ref().unwrap().clone();

        // env[1] are the arguments to pass to k
        let args = env.add(1).as_ref().unwrap().clone();

        // env[2] is the stack we are trying to reach
        let dest_stack_val = env.add(2).as_ref().unwrap().clone();
        let dest_stack = dest_stack_val.try_to_rust_type::<DynamicState>().unwrap();
        let dest_stack_read = dest_stack.as_ref();

        // env[3] is the index into the dest stack we're at
        let idx: Arc<Number> = env.add(3).as_ref().unwrap().clone().try_into().unwrap();
        let mut idx: usize = idx.as_ref().try_into().unwrap();

        let dyn_state = dyn_state.as_mut().unwrap_unchecked();

        // env[4] is potentially a winder that we should push onto the dyn stack
        let winder = env.add(4).as_ref().unwrap().clone();
        if winder.is_true() {
            let winder = winder.try_to_rust_type::<Winder>().unwrap();
            dyn_state.push_dyn_stack(DynStackElem::Winder(winder.as_ref().clone()));
        }

        while let Some(elem) = dest_stack_read.dyn_stack_get(idx) {
            idx += 1;

            if let DynStackElem::Winder(winder) = elem {
                // Call the in winder while winding
                let app = Application::new(
                    winder.in_thunk.clone(),
                    vec![Value::from(dyn_state.new_k(
                        Runtime::from_raw_inc_rc(runtime),
                        vec![
                            k,
                            args,
                            dest_stack_val,
                            Value::from(Record::from_rust_type(winder.clone())),
                        ],
                        wind,
                        0,
                        false,
                    ))],
                );
                return Box::into_raw(Box::new(app));
            }
            dyn_state.push_dyn_stack(elem.clone());
        }

        let args: Vector = args.try_into().unwrap();
        let args = args.0.vec.read().to_vec();

        Box::into_raw(Box::new(Application::new(k.try_into().unwrap(), args)))
    }
}
