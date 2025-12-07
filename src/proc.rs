//! Procedures, continuation and user, and applying values to those procedures.
//! Contains the main evaluation trampoline.

use crate::{
    env::Local,
    exceptions::{Condition, Exception, Frame, raise},
    gc::{Gc, GcInner, Trace},
    lists::{self, Pair, list_to_vec},
    num::Number,
    ports::Port,
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::BridgeFnDebugInfo,
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    syntax::Span,
    value::Value,
    vectors,
};
use parking_lot::RwLock;
use scheme_rs_macros::{cps_bridge, maybe_async, maybe_await};
use std::{
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
    dyn_stack: *mut DynStack,
) -> *mut Application;

/// A function pointer to a generated user function.
pub(crate) type UserPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    dyn_stack: *mut DynStack,
    k: Value,
) -> *mut Application;

/// A function pointer to a sync Rust bridge function.
pub type BridgePtr = for<'a> fn(
    runtime: &'a Runtime,
    env: &'a [Value],
    // TODO: Make this a Vec
    args: &'a [Value],
    rest_args: &'a [Value],
    dyn_stack: &mut DynStack,
    k: Value,
) -> Application;

/// A function pointer to an async Rust bridge function.
#[cfg(feature = "async")]
pub type AsyncBridgePtr = for<'a> fn(
    runtime: &'a Runtime,
    env: &'a [Value],
    args: &'a [Value],
    rest_args: &'a [Value],
    dyn_stack: &'a mut DynStack,
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
    pub(crate) debug_info: Option<Arc<FuncDebugInfo>>,
}

impl ProcedureInner {
    pub(crate) fn new(
        runtime: Runtime,
        env: Vec<Value>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<FuncDebugInfo>>,
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
    ) -> Result<(Vec<Value>, Option<Value>), Application> {
        // Extract the continuation, if it is required
        let cont = (!self.is_continuation()).then(|| args.pop().unwrap());

        // Error if the number of arguments provided is incorrect
        if args.len() < self.num_required_args {
            return Err(raise(
                self.runtime.clone(),
                Condition::wrong_num_of_args(self.num_required_args, args.len()).into(),
            ));
        }

        if !self.variadic && args.len() > self.num_required_args {
            return Err(raise(
                self.runtime.clone(),
                Condition::wrong_num_of_args(self.num_required_args, args.len()).into(),
            ));
        }

        Ok((args, cont))
    }

    #[cfg(feature = "async")]
    async fn apply_async_bridge(
        &self,
        func: AsyncBridgePtr,
        args: &[Value],
        dyn_stack: &mut DynStack,
        k: Value,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(&self.runtime, &self.env, args, rest_args, dyn_stack, k).await
    }

    fn apply_sync_bridge(
        &self,
        func: BridgePtr,
        args: &[Value],
        dyn_stack: &mut DynStack,
        k: Value,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(&self.runtime, &self.env, args, rest_args, dyn_stack, k)
    }

    fn apply_jit(
        &self,
        func: JitFuncPtr,
        mut args: Vec<Value>,
        dyn_stack: &mut DynStack,
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
                    dyn_stack as *mut DynStack,
                )
            },
            JitFuncPtr::User(sync_fn) => unsafe {
                (sync_fn)(
                    Gc::as_ptr(&self.runtime.0),
                    self.env.as_ptr(),
                    args.as_ptr(),
                    dyn_stack as *mut DynStack,
                    Value::from_raw(Value::as_raw(k.as_ref().unwrap())),
                )
            },
        };

        unsafe { *Box::from_raw(app) }
    }

    #[maybe_async]
    pub fn apply(&self, args: Vec<Value>, dyn_stack: &mut DynStack) -> Application {
        let (args, k) = match self.prepare_args(args) {
            Ok(args) => args,
            Err(raised) => return raised,
        };

        match self.func {
            FuncPtr::Bridge(sbridge) => {
                self.apply_sync_bridge(sbridge, &args, dyn_stack, k.unwrap())
            }
            #[cfg(feature = "async")]
            FuncPtr::AsyncBridge(abridge) => {
                self.apply_async_bridge(abridge, &args, dyn_stack, k.unwrap())
                    .await
            }
            FuncPtr::User(user) => self.apply_jit(JitFuncPtr::User(user), args, dyn_stack, k),
            FuncPtr::Continuation(k) => {
                self.apply_jit(JitFuncPtr::Continuation(k), args, dyn_stack, None)
            }
            FuncPtr::PromptBarrier { barrier_id: id, k } => {
                match dyn_stack.pop() {
                    Some(DynStackElem::PromptBarrier(PromptBarrier {
                        barrier_id,
                        replaced_k,
                    })) if barrier_id == id => {
                        return Application::new(replaced_k, args, None);
                    }
                    Some(other) => dyn_stack.push(other),
                    _ => (),
                }
                self.apply_jit(JitFuncPtr::Continuation(k), args, dyn_stack, None)
            }
        }
    }

    #[cfg(feature = "async")]
    /// Attempt to call the function, and throw an error if is async
    pub fn apply_sync(&self, args: Vec<Value>, dyn_stack: &mut DynStack) -> Application {
        let (args, k) = match self.prepare_args(args) {
            Ok(args) => args,
            Err(raised) => return raised,
        };

        match self.func {
            FuncPtr::Bridge(sbridge) => {
                self.apply_sync_bridge(sbridge, &args, dyn_stack, k.unwrap())
            }
            FuncPtr::AsyncBridge(_) => raise(
                self.runtime.clone(),
                Condition::error("attempt to apply async function in a sync-only context").into(),
            ),
            FuncPtr::User(user) => self.apply_jit(JitFuncPtr::User(user), args, dyn_stack, k),
            FuncPtr::Continuation(k) => {
                self.apply_jit(JitFuncPtr::Continuation(k), args, dyn_stack, None)
            }
            FuncPtr::PromptBarrier { barrier_id: id, k } => {
                match dyn_stack.pop() {
                    Some(DynStackElem::PromptBarrier(PromptBarrier {
                        barrier_id,
                        replaced_k,
                    })) if barrier_id == id => {
                        return Application::new(replaced_k, args, None);
                    }
                    Some(other) => dyn_stack.push(other),
                    _ => (),
                }
                self.apply_jit(JitFuncPtr::Continuation(k), args, dyn_stack, None)
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
            write!(f, "(unknown-function")?;
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
    pub(crate) fn new(
        runtime: Runtime,
        env: Vec<Value>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<FuncDebugInfo>>,
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

    /// # Safety
    /// `args` must be a valid pointer and contain num_required_args + variadic entries.
    pub unsafe fn collect_args(&self, args: *const Value) -> Vec<Value> {
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

    #[maybe_async]
    pub fn call(&self, args: &[Value]) -> Result<Vec<Value>, Exception> {
        let mut args = args.to_vec();

        args.push(halt_continuation(self.get_runtime()));

        maybe_await!(Application::new(self.clone(), args, None,).eval(&mut DynStack::default()))
    }

    #[cfg(feature = "async")]
    pub fn call_sync(&self, args: &[Value]) -> Result<Vec<Value>, Exception> {
        let mut args = args.to_vec();

        args.push(halt_continuation(self.get_runtime()));

        Application::new(self.clone(), args, None).eval_sync(&mut DynStack::default())
    }
}

static HALT_CONTINUATION: OnceLock<Value> = OnceLock::new();

/// Return a continuation that returns its expressions.
pub fn halt_continuation(runtime: Runtime) -> Value {
    unsafe extern "C" fn halt(
        _runtime: *mut GcInner<RwLock<RuntimeInner>>,
        _env: *const Value,
        args: *const Value,
        _dyn_stack: *mut DynStack,
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

pub enum OpType {
    Proc(Procedure),
    HaltOk,
    HaltErr,
}

/// An application of a function to a given set of values.
pub struct Application {
    /// The operator being applied to.
    pub op: OpType,
    /// The arguments being applied to the operator.
    pub args: Vec<Value>,
    /// The call site of this application, if it exists.
    pub call_site: Option<Arc<Span>>,
}

impl Application {
    pub fn new(op: Procedure, args: Vec<Value>, call_site: Option<Arc<Span>>) -> Self {
        Self {
            // We really gotta figure out how to deal with this better
            op: OpType::Proc(op),
            args,
            call_site,
        }
    }

    pub fn halt_ok(args: Vec<Value>) -> Self {
        Self {
            op: OpType::HaltOk,
            args,
            call_site: None,
        }
    }

    pub fn halt_err(arg: Value) -> Self {
        Self {
            op: OpType::HaltErr,
            args: vec![arg],
            call_site: None,
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    #[maybe_async]
    pub fn eval(mut self, dyn_stack: &mut DynStack) -> Result<Vec<Value>, Exception> {
        let mut stack_trace = StackTraceCollector::new();

        loop {
            let op = match self.op {
                OpType::Proc(proc) => proc,
                OpType::HaltOk => return Ok(self.args),
                OpType::HaltErr => {
                    return Err(Exception::new(
                        stack_trace.into_frames(),
                        self.args.pop().unwrap(),
                    ));
                }
            };
            stack_trace.collect_application(op.0.debug_info.clone(), self.call_site);
            self = maybe_await!(op.0.apply(self.args, dyn_stack));
        }
    }

    #[cfg(feature = "async")]
    /// Just like [eval] but throws an error if we encounter an async function.
    pub fn eval_sync(mut self, dyn_stack: &mut DynStack) -> Result<Vec<Value>, Exception> {
        let mut stack_trace = StackTraceCollector::new();

        loop {
            let op = match self.op {
                OpType::Proc(proc) => proc,
                OpType::HaltOk => return Ok(self.args),
                OpType::HaltErr => {
                    return Err(Exception::new(
                        stack_trace.into_frames(),
                        self.args.pop().unwrap(),
                    ));
                }
            };
            stack_trace.collect_application(op.0.debug_info.clone(), self.call_site);
            self = op.0.apply_sync(self.args, dyn_stack);
        }
    }
}

#[derive(Default)]
struct StackTraceCollector {
    stack_trace: Vec<StackTrace>,
}

impl StackTraceCollector {
    fn new() -> Self {
        Self::default()
    }

    fn into_frames(self) -> Vec<Frame> {
        self.stack_trace
            .into_iter()
            .map(|trace| Frame::new(trace.debug_info.name, trace.call_site))
            .collect()
    }

    fn collect_application(
        &mut self,
        debug_info: Option<Arc<FuncDebugInfo>>,
        call_site: Option<Arc<Span>>,
    ) {
        if let Some(debug_info) = debug_info {
            // This is a user func, and therefore we should push to the
            // current stack trace.
            self.stack_trace.push(StackTrace {
                debug_info,
                call_site,
            });
        } else {
            // If this is not user func, we are returning from one via a
            // continuation and should pop the stack frame:
            self.stack_trace.pop();
        }
    }
}

#[derive(Debug)]
pub struct FuncDebugInfo {
    /// The name of the function.
    name: Symbol,
    /// Named arguments for the function.
    args: Vec<Local>,
    /// Location of the function definition
    location: Span,
}

#[derive(Debug)]
struct StackTrace {
    debug_info: Arc<FuncDebugInfo>,
    call_site: Option<Arc<Span>>,
}

impl FuncDebugInfo {
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

#[cps_bridge(name = "apply", lib = "(rnrs base builtins (6))", args = "arg1 . args")]
pub fn apply(
    _runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    _dyn_stack: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
    if rest_args.is_empty() {
        return Err(Condition::wrong_num_of_args(2, args.len()));
    }
    let op: Procedure = args[0].clone().try_into()?;
    let (last, args) = rest_args.split_last().unwrap();
    let mut args = args.to_vec();
    list_to_vec(last, &mut args);
    args.push(k);
    Ok(Application::new(op.clone(), args, None))
}

////////////////////////////////////////////////////////////////////////////////
//
// Dynamic stack
//

/// A dynamic stack, loosely modeled after Guile's
#[derive(Clone, Default, Debug, Trace)]
pub struct DynStack {
    dyn_stack: Vec<DynStackElem>,
}

impl DynStack {
    pub fn current_exception_handler(&self) -> Option<Procedure> {
        self.dyn_stack.iter().rev().find_map(|elem| match elem {
            DynStackElem::ExceptionHandler(proc) => Some(proc.clone()),
            _ => None,
        })
    }

    pub fn current_output_port(&self) -> Option<Port> {
        self.dyn_stack.iter().rev().find_map(|elem| match elem {
            DynStackElem::CurrentOutputPort(port) => Some(port.clone()),
            _ => None,
        })
    }

    pub fn current_input_port(&self) -> Option<Port> {
        self.dyn_stack.iter().rev().find_map(|elem| match elem {
            DynStackElem::CurrentInputPort(port) => Some(port.clone()),
            _ => None,
        })
    }

    pub fn push(&mut self, elem: DynStackElem) {
        self.dyn_stack.push(elem);
    }

    pub fn pop(&mut self) -> Option<DynStackElem> {
        self.dyn_stack.pop()
    }

    pub fn get(&self, idx: usize) -> Option<&DynStackElem> {
        self.dyn_stack.get(idx)
    }

    pub fn last(&self) -> Option<&DynStackElem> {
        self.dyn_stack.last()
    }

    pub fn len(&self) -> usize {
        self.dyn_stack.len()
    }

    pub fn is_empty(&self) -> bool {
        self.dyn_stack.is_empty()
    }
}

impl SchemeCompatible for DynStack {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "$dyn-stack", sealed: true, opaque: true)
    }
}

#[derive(Clone, Debug, PartialEq, Trace)]
pub enum DynStackElem {
    Prompt(Prompt),
    PromptBarrier(PromptBarrier),
    Winder(Winder),
    ExceptionHandler(Procedure),
    CurrentOutputPort(Port),
    CurrentInputPort(Port),
}

pub(crate) unsafe extern "C" fn pop_dyn_stack(
    _runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    dyn_stack: *mut DynStack,
) -> *mut Application {
    unsafe {
        // env[0] is the continuation
        let k: Procedure = env.as_ref().unwrap().clone().try_into().unwrap();

        dyn_stack.as_mut().unwrap_unchecked().pop();

        let args = k.collect_args(args);
        let app = Application::new(k, args, None);

        Box::into_raw(Box::new(app))
    }
}

////////////////////////////////////////////////////////////////////////////////
//
// Call with current continuation
//

#[cps_bridge(
    name = "call-with-current-continuation",
    lib = "(rnrs base builtins (6))",
    args = "proc"
)]
pub fn call_with_current_continuation(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_stack: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
    let [proc] = args else { unreachable!() };
    let proc: Procedure = proc.clone().try_into()?;

    let (req_args, variadic) = {
        let k: Procedure = k.clone().try_into()?;
        k.get_formals()
    };

    let dyn_stack = Value::from(Record::from_rust_type(dyn_stack.clone()));

    let escape_procedure = Procedure::new(
        runtime.clone(),
        vec![k.clone(), dyn_stack],
        FuncPtr::Bridge(escape_procedure),
        req_args,
        variadic,
        None,
    );

    let app = Application::new(proc, vec![Value::from(escape_procedure), k], None);

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
    dyn_stack: &mut DynStack,
    _k: Value,
) -> Result<Application, Condition> {
    // env[0] is the continuation
    let k = env[0].clone();

    // env[1] is the dyn stack of the continuation
    let saved_dyn_stack_val = env[1].clone();
    let saved_dyn_stack = saved_dyn_stack_val
        .clone()
        .try_into_rust_type::<DynStack>()
        .unwrap();
    let saved_dyn_stack_read = saved_dyn_stack.as_ref();

    // Clone the continuation
    let k: Procedure = k.try_into().unwrap();

    let args = args.iter().chain(rest_args).cloned().collect::<Vec<_>>();

    // Simple optimization: check if we're in the same dyn stack
    if dyn_stack.len() == saved_dyn_stack_read.len()
        && dyn_stack.last() == saved_dyn_stack_read.last()
    {
        Ok(Application::new(k, args, None))
    } else {
        let args = Value::from(args);
        let k = Procedure::new(
            runtime.clone(),
            vec![Value::from(k), args, saved_dyn_stack_val],
            FuncPtr::Continuation(unwind),
            0,
            false,
            None,
        );
        Ok(Application::new(k, Vec::new(), None))
    }
}

unsafe extern "C" fn unwind(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    dyn_stack: *mut DynStack,
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
            .try_into_rust_type::<DynStack>()
            .unwrap();
        let dest_stack_read = dest_stack.as_ref();

        let dyn_stack = dyn_stack.as_mut().unwrap_unchecked();

        while !dyn_stack.is_empty()
            && (dyn_stack.len() > dest_stack_read.len()
                || dyn_stack.last() != dest_stack_read.get(dyn_stack.len() - 1))
        {
            match dyn_stack.pop() {
                None => {
                    break;
                }
                Some(DynStackElem::Winder(winder)) => {
                    // Call the out winder while unwinding
                    let app = Application::new(
                        winder.out_thunk,
                        vec![Value::from(Procedure::new(
                            Runtime::from_raw_inc_rc(runtime),
                            vec![k, args, dest_stack_val],
                            FuncPtr::Continuation(unwind),
                            0,
                            false,
                            None,
                        ))],
                        None,
                    );
                    return Box::into_raw(Box::new(app));
                }
                _ => (),
            };
        }

        // Begin winding
        let app = Application::new(
            Procedure::new(
                Runtime::from_raw_inc_rc(runtime),
                vec![k, args, dest_stack_val, Value::from(false)],
                FuncPtr::Continuation(wind),
                0,
                false,
                None,
            ),
            Vec::new(),
            None,
        );

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn wind(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    dyn_stack: *mut DynStack,
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
            .try_into_rust_type::<DynStack>()
            .unwrap();
        let dest_stack_read = dest_stack.as_ref();

        let dyn_stack = dyn_stack.as_mut().unwrap_unchecked();

        // env[3] is potentially a winder that we should push onto the dyn stack
        let winder = env.add(3).as_ref().unwrap().clone();
        if winder.is_true() {
            let winder = winder.try_into_rust_type::<Winder>().unwrap();
            dyn_stack.push(DynStackElem::Winder(winder.as_ref().clone()));
        }

        while dyn_stack.len() < dest_stack_read.len() {
            match dest_stack_read.get(dyn_stack.len()).cloned() {
                None => {
                    break;
                }
                Some(DynStackElem::Winder(winder)) => {
                    // Call the in winder while winding
                    let app = Application::new(
                        winder.in_thunk.clone(),
                        vec![Value::from(Procedure::new(
                            Runtime::from_raw_inc_rc(runtime),
                            vec![
                                k,
                                args,
                                dest_stack_val,
                                Value::from(Record::from_rust_type(winder)),
                            ],
                            FuncPtr::Continuation(wind),
                            0,
                            false,
                            None,
                        ))],
                        None,
                    );
                    return Box::into_raw(Box::new(app));
                }
                Some(elem) => dyn_stack.push(elem),
            }
        }

        let args: Gc<RwLock<vectors::AlignedVector<Value>>> = args.try_into().unwrap();
        let args = args.read().0.to_vec();

        let app = Application::new(k.try_into().unwrap(), args, None);
        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn call_consumer_with_values(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    _dyn_stack: *mut DynStack,
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
                    Condition::invalid_operator(type_name).into(),
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

        Box::into_raw(Box::new(Application::new(
            consumer.clone(),
            collected_args,
            None,
        )))
    }
}

#[cps_bridge(
    name = "call-with-values",
    lib = "(rnrs base builtins (6))",
    args = "producer consumer"
)]
pub fn call_with_values(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_stack: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
    let [producer, consumer] = args else {
        return Err(Condition::wrong_num_of_args(2, args.len()));
    };

    let producer: Procedure = producer.clone().try_into()?;
    let consumer: Procedure = consumer.clone().try_into()?;

    // Get the details of the consumer:
    let (num_required_args, variadic) = {
        (consumer.0.num_required_args, consumer.0.variadic)
    };

    let call_consumer_closure = Procedure::new(
        runtime.clone(),
        vec![Value::from(consumer), k],
        FuncPtr::Continuation(call_consumer_with_values),
        num_required_args,
        variadic,
        None,
    );

    Ok(Application::new(
        producer,
        vec![Value::from(call_consumer_closure)],
        None,
    ))
}

////////////////////////////////////////////////////////////////////////////////
//
// Dynamic wind
//

#[derive(Clone, Debug, Trace, PartialEq)]
pub struct Winder {
    pub(crate) in_thunk: Procedure,
    pub(crate) out_thunk: Procedure,
}

impl SchemeCompatible for Winder {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "$winder", sealed: true, opaque: true)
    }
}

#[cps_bridge(
    name = "dynamic-wind",
    lib = "(rnrs base builtins (6))",
    args = "in body out"
)]
pub fn dynamic_wind(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_stack: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
    let [in_thunk_val, body_thunk_val, out_thunk_val] = args else {
        return Err(Condition::wrong_num_of_args(3, args.len()));
    };

    let in_thunk: Procedure = in_thunk_val.clone().try_into()?;
    let _: Procedure = body_thunk_val.clone().try_into()?;

    let call_body_thunk_cont = Procedure::new(
        runtime.clone(),
        vec![
            in_thunk_val.clone(),
            body_thunk_val.clone(),
            out_thunk_val.clone(),
            k,
        ],
        FuncPtr::Continuation(call_body_thunk),
        0,
        true,
        None,
    );

    Ok(Application::new(
        in_thunk,
        vec![Value::from(call_body_thunk_cont)],
        None,
    ))
}

pub(crate) unsafe extern "C" fn call_body_thunk(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    dyn_stack: *mut DynStack,
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

        let dyn_stack = dyn_stack.as_mut().unwrap_unchecked();

        dyn_stack.push(DynStackElem::Winder(Winder {
            in_thunk: in_thunk.clone().try_into().unwrap(),
            out_thunk: out_thunk.clone().try_into().unwrap(),
        }));

        let k = Procedure::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![out_thunk, k],
            FuncPtr::Continuation(call_out_thunks),
            0,
            true,
            None,
        );

        let app = Application::new(body_thunk, vec![Value::from(k)], None);

        Box::into_raw(Box::new(app))
    }
}

pub(crate) unsafe extern "C" fn call_out_thunks(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    dyn_stack: *mut DynStack,
) -> *mut Application {
    unsafe {
        // env[0] is the out thunk
        let out_thunk: Procedure = env.as_ref().unwrap().clone().try_into().unwrap();

        // env[1] is k, the remaining continuation
        let k = env.add(1).as_ref().unwrap().clone();

        // args[0] is the result of the body thunk
        let body_thunk_res = args.as_ref().unwrap().clone();

        let dyn_stack = dyn_stack.as_mut().unwrap_unchecked();
        dyn_stack.pop();

        let cont = Procedure(Gc::new(ProcedureInner::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![body_thunk_res, k],
            FuncPtr::Continuation(forward_body_thunk_result),
            0,
            true,
            None,
        )));

        let app = Application::new(out_thunk, vec![Value::from(cont)], None);

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn forward_body_thunk_result(
    _runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    _dyn_stack: *mut DynStack,
) -> *mut Application {
    unsafe {
        // env[0] is the result of the body thunk
        let body_thunk_res = env.as_ref().unwrap().clone();
        // env[1] is k, the continuation.
        let k: Procedure = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        let mut args = Vec::new();
        list_to_vec(&body_thunk_res, &mut args);

        let app = Application::new(k, args, None);

        Box::into_raw(Box::new(app))
    }
}

////////////////////////////////////////////////////////////////////////////////
//
// Prompts and delimited continuations
//

#[derive(Clone, Debug, PartialEq, Trace)]
pub struct Prompt {
    tag: Symbol,
    barrier_id: usize,
    handler: Procedure,
    handler_k: Procedure,
}

#[derive(Clone, Debug, PartialEq, Trace)]
pub struct PromptBarrier {
    barrier_id: usize,
    replaced_k: Procedure,
}

static BARRIER_ID: AtomicUsize = AtomicUsize::new(0);

#[cps_bridge(
    name = "call-with-prompt",
    lib = "(rnrs base builtins (6))",
    args = "tag thunk handler"
)]
pub fn call_with_prompt(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_stack: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
    let [tag, thunk, handler] = args else {
        unreachable!()
    };

    let k_proc: Procedure = k.clone().try_into().unwrap();
    let (req_args, variadic) = k_proc.get_formals();
    let tag: Symbol = tag.clone().try_into().unwrap();

    let barrier_id = BARRIER_ID.fetch_add(1, Ordering::Relaxed);

    dyn_stack.push(DynStackElem::Prompt(Prompt {
        tag,
        handler: handler.clone().try_into().unwrap(),
        barrier_id,
        handler_k: k.clone().try_into()?,
    }));

    let prompt_barrier = Procedure::new(
        runtime.clone(),
        vec![k],
        FuncPtr::PromptBarrier {
            barrier_id,
            k: pop_dyn_stack,
        },
        req_args,
        variadic,
        None,
    );

    Ok(Application::new(
        thunk.clone().try_into().unwrap(),
        vec![Value::from(prompt_barrier)],
        None,
    ))
}

#[cps_bridge(
    name = "abort-to-prompt",
    lib = "(rnrs base builtins (6))",
    args = "tag"
)]
pub fn abort_to_prompt(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_stack: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
    let [tag] = args else { unreachable!() };

    let unwind_to_prompt = Procedure::new(
        runtime.clone(),
        vec![
            k,
            tag.clone(),
            Value::from(Record::from_rust_type(dyn_stack.clone())),
        ],
        FuncPtr::Continuation(unwind_to_prompt),
        0,
        false,
        None,
    );

    Ok(Application::new(unwind_to_prompt, Vec::new(), None))
}

unsafe extern "C" fn unwind_to_prompt(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    dyn_stack: *mut DynStack,
) -> *mut Application {
    unsafe {
        // env[0] is continuation
        let k = env.as_ref().unwrap().clone();
        // env[1] is the prompt tag
        let tag: Symbol = env.add(1).as_ref().unwrap().clone().try_into().unwrap();
        // env[2] is the saved dyn stack
        let saved_dyn_stack = env.add(2).as_ref().unwrap().clone();

        let dyn_stack = dyn_stack.as_mut().unwrap_unchecked();

        loop {
            let app = match dyn_stack.pop() {
                None => {
                    // If the stack is empty, we should return the error
                    Application::halt_err(Value::from(Condition::error(format!(
                        "No prompt tag {tag} found"
                    ))))
                }
                Some(DynStackElem::Prompt(Prompt {
                    tag: prompt_tag,
                    barrier_id,
                    handler,
                    handler_k,
                })) if prompt_tag == tag => {
                    let saved_dyn_stack = saved_dyn_stack.try_into_rust_type::<DynStack>().unwrap();
                    let prompt_delimited_dyn_stack = DynStack {
                        dyn_stack: saved_dyn_stack.as_ref().dyn_stack[dyn_stack.len() + 1..].to_vec(),
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
                                    Value::from(Record::from_rust_type(prompt_delimited_dyn_stack)),
                                ],
                                FuncPtr::Bridge(delimited_continuation),
                                req_args,
                                var,
                                None,
                            )),
                            Value::from(handler_k),
                        ],
                        None,
                    )
                }
                Some(DynStackElem::Winder(winder)) => {
                    // If this is a winder, we should call the out winder while unwinding
                    Application::new(
                        winder.out_thunk,
                        vec![Value::from(Procedure::new(
                            Runtime::from_raw_inc_rc(runtime),
                            vec![k, Value::from(tag), saved_dyn_stack],
                            FuncPtr::Continuation(unwind_to_prompt),
                            0,
                            false,
                            None,
                        ))],
                        None,
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
    dyn_stack: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
    // env[0] is the delimited continuation
    let dk = env[0].clone();

    // env[1] is the barrier Id
    let barrier_id: Arc<Number> = env[1].clone().try_into()?;
    let barrier_id: usize = barrier_id.as_ref().try_into()?;

    // env[2] is the dyn stack of the continuation
    let saved_dyn_stack_val = env[2].clone();
    let saved_dyn_stack = saved_dyn_stack_val
        .clone()
        .try_into_rust_type::<DynStack>()
        .unwrap();
    let saved_dyn_stack_read = saved_dyn_stack.as_ref();

    let args = args.iter().chain(rest_args).cloned().collect::<Vec<_>>();

    dyn_stack.push(DynStackElem::PromptBarrier(PromptBarrier {
        barrier_id,
        replaced_k: k.try_into()?,
    }));

    // Simple optimization: if the saved dyn stack is empty, we
    // can just call the delimited continuation
    if saved_dyn_stack_read.is_empty() {
        Ok(Application::new(dk.try_into()?, args, None))
    } else {
        let args = Value::from(args);
        let k = Procedure::new(
            runtime.clone(),
            vec![
                dk,
                args,
                saved_dyn_stack_val,
                Value::from(Number::from(0)),
                Value::from(false),
            ],
            FuncPtr::Continuation(wind_delim),
            0,
            false,
            None,
        );
        Ok(Application::new(k, Vec::new(), None))
    }
}

unsafe extern "C" fn wind_delim(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    dyn_stack: *mut DynStack,
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
            .try_into_rust_type::<DynStack>()
            .unwrap();
        let dest_stack_read = dest_stack.as_ref();

        // env[3] is the index into the dest stack we're at
        let idx: Arc<Number> = env.add(3).as_ref().unwrap().clone().try_into().unwrap();
        let mut idx: usize = idx.as_ref().try_into().unwrap();

        let dyn_stack = dyn_stack.as_mut().unwrap_unchecked();

        // env[4] is potentially a winder that we should push onto the dyn stack
        let winder = env.add(4).as_ref().unwrap().clone();
        if winder.is_true() {
            let winder = winder.try_into_rust_type::<Winder>().unwrap();
            dyn_stack.push(DynStackElem::Winder(winder.as_ref().clone()));
        }

        while let Some(elem) = dest_stack_read.get(idx) {
            idx += 1;

            if let DynStackElem::Winder(winder) = elem {
                // Call the in winder while winding
                let app = Application::new(
                    winder.in_thunk.clone(),
                    vec![Value::from(Procedure::new(
                        Runtime::from_raw_inc_rc(runtime),
                        vec![
                            k,
                            args,
                            dest_stack_val,
                            Value::from(Record::from_rust_type(winder.clone())),
                        ],
                        FuncPtr::Continuation(wind),
                        0,
                        false,
                        None,
                    ))],
                    None,
                );
                return Box::into_raw(Box::new(app));
            }
            dyn_stack.push(elem.clone());
        }

        let args: Gc<RwLock<vectors::AlignedVector<Value>>> = args.try_into().unwrap();
        let args = args.read().0.to_vec();

        let app = Application::new(k.try_into().unwrap(), args, None);
        Box::into_raw(Box::new(app))
    }
}
