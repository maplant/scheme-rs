//! Scheme Procedures.
//!
//! Scheme procedures, more commonly known as [`closures`](https://en.wikipedia.org/wiki/Closure_(computer_programming))
//! as they capture their environment, are the fundamental and only way to
//! transfer control from a Rust context to a Scheme context.
//!
//! # Calling procedures from Rust
//!
//! # Manually creating closures
//!
//! Generally procedures are created in Scheme contexts. However, it is
//! occasionally desirable to create a closure in Rust contexts. This can be
//! done with a [`cps_bridge`] function and a call to [`Procedure::new`]. The
//! `env` argument to the CPS function is a reference to the vector passed to
//! the `new` function:
//!
//! ```
//! # use scheme_rs::{proc::{Procedure, BridgePtr, Application, ContBarrier},
//! # registry::cps_bridge, value::Value, exceptions::Exception};
//! #[cps_bridge]
//! fn closure(
//!     env: &[Value],
//!     _args: &[Value],
//!     _rest_args: &[Value],
//!     barrier: &mut ContBarrier,
//! ) -> Result<Application, Exception> {
//!     Ok(barrier.call_cont(vec![ env[0].clone() ]))
//! }
//!
//! # fn main() {
//! let closure = Procedure::new(
//!     vec![ Value::from(3.1415) ],
//!     closure as BridgePtr,
//!     0,
//!     false,
//! );
//! # }
//! ```
//!
//! By default the environment is immutable. If the environment needs to be
//! modified, a [`Cell`](scheme_rs::value::Cell) can be used:
//!
//! ```
//! # use scheme_rs::{
//! #     proc::{Procedure, BridgePtr, Application, ContBarrier},
//! #     registry::cps_bridge, value::{Value, Cell},
//! #     exceptions::Exception,
//! #     num::Number,
//! # };
//! #[cps_bridge]
//! fn next_num(
//!     env: &[Value],
//!     _args: &[Value],
//!     _rest_args: &[Value],
//!     barrier: &mut ContBarrier,
//! ) -> Result<Application, Exception> {
//!     // Fetch the cell from the environment:
//!     let cell: Cell = env[0].try_to()?;
//!     let curr: Number = cell.get().try_into()?;
//!
//!     // Increment the cell
//!     cell.set(Value::from(curr.clone() + Number::from(1)));
//!
//!     // Return the previous value:
//!     Ok(barrier.call_cont(vec![ Value::from(curr) ]))
//! }
//!
//! # fn main() {
//! let next_num = Procedure::new(
//!     // Cells must be converted to values:
//!     vec![ Value::from(Cell::new(Value::from(3.1415))) ],
//!     next_num as BridgePtr,
//!     0,
//!     false,
//! );
//! # }
//! ```
//!
//! # Categories of procedures
//!
//! In scheme-rs, procedures can be placed into a few different categories, the
//! most obvious is that procedures are either _user_ functions or
//! [_continuations_](https://en.wikipedia.org/wiki/Continuation). This
//! categorization is mostly transparent to the user.

use crate::{
    cps::PrimOp,
    env::Local,
    exceptions::{Exception, raise},
    gc::{Gc, Trace},
    lists::{Pair, list_to_vec},
    ports::{BufferMode, Port, Transcoder},
    records::{Embeddable, Embedded, RecordTypeDescriptor, rtd},
    registry::BridgeFnDebugInfo,
    runtime::Runtime,
    symbols::Symbol,
    syntax::Span,
    value::Value,
    vectors::Vector,
};
use scheme_rs_macros::{cps_bridge, maybe_async, maybe_await};
use smallvec::SmallVec;
#[cfg(feature = "async")]
use std::future::Future;
use std::{
    any::Any,
    cell::RefCell,
    collections::HashMap,
    fmt,
    mem::MaybeUninit,
    ops::DerefMut,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

/// A function pointer to a generated continuation.
pub(crate) type ContinuationPtr = unsafe extern "C" fn(
    env: *const Value,
    args: *const Value,
    barrier: *mut ContBarrier<'_>,
    out: *mut MaybeUninit<Application>,
);

/// A function pointer to a generated user function.
pub(crate) type UserPtr = unsafe extern "C" fn(
    env: *const Value,
    args: *const Value,
    barrier: *mut ContBarrier<'_>,
    out: *mut MaybeUninit<Application>,
);

/// A function pointer to a sync Rust bridge function.
pub type BridgePtr = fn(
    env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    barrier: &mut ContBarrier<'_>,
) -> Application;

/// A function pointer to an async Rust bridge function.
#[cfg(feature = "async")]
pub type AsyncBridgePtr = for<'a> fn(
    env: &'a [Value],
    args: &'a [Value],
    rest_args: &'a [Value],
    barrier: &'a mut ContBarrier<'_>,
) -> futures::future::BoxFuture<'a, Application>;

#[derive(Copy, Clone, Debug)]
pub enum KnownFunc {
    Known0x1(fn() -> Result<Value, Exception>),
    Known1x0(fn(&Value) -> Result<(), Exception>),
    Known1x1(fn(&Value) -> Result<Value, Exception>),
    Known2x0(fn(&Value, &Value) -> Result<(), Exception>),
    Known2x1(fn(&Value, &Value) -> Result<Value, Exception>),
    Known3x0(fn(&Value, &Value, &Value) -> Result<(), Exception>),
    Known3x1(fn(&Value, &Value, &Value) -> Result<Value, Exception>),
}

impl KnownFunc {
    fn call(self, args: &[Value]) -> Result<Vec<Value>, Exception> {
        match self {
            Self::Known0x1(func) => Ok(vec![(func)()?]),
            Self::Known1x0(func) => {
                (func)(&args[0])?;
                Ok(Vec::new())
            }
            Self::Known1x1(func) => Ok(vec![(func)(&args[0])?]),
            Self::Known2x0(func) => {
                (func)(&args[0], &args[1])?;
                Ok(Vec::new())
            }
            Self::Known2x1(func) => Ok(vec![(func)(&args[0], &args[1])?]),
            Self::Known3x0(func) => {
                (func)(&args[0], &args[1], &args[2])?;
                Ok(Vec::new())
            }
            Self::Known3x1(func) => Ok(vec![(func)(&args[0], &args[1], &args[2])?]),
        }
    }

    fn apply(self, args: &[Value], barrier: &mut ContBarrier<'_>) -> Application {
        match self.call(args) {
            Ok(result) => barrier.call_cont(result),
            Err(err) => raise(err.into(), barrier),
        }
    }

    pub fn return_values(&self) -> usize {
        match self {
            Self::Known1x0(_) | Self::Known2x0(_) | Self::Known3x0(_) => 0,
            Self::Known0x1(_) | Self::Known1x1(_) | Self::Known2x1(_) | Self::Known3x1(_) => 1,
        }
    }

    pub(crate) fn cast_to_usize(&self) -> usize {
        match self {
            Self::Known0x1(ptr) => *ptr as usize,
            Self::Known1x0(ptr) => *ptr as usize,
            Self::Known1x1(ptr) => *ptr as usize,
            Self::Known2x0(ptr) => *ptr as usize,
            Self::Known2x1(ptr) => *ptr as usize,
            Self::Known3x0(ptr) => *ptr as usize,
            Self::Known3x1(ptr) => *ptr as usize,
        }
    }

    pub(crate) fn matches_args(&self, num: usize) -> bool {
        match self {
            Self::Known0x1(_) => num == 0,
            Self::Known1x0(_) | Self::Known1x1(_) => num == 1,
            Self::Known2x0(_) | Self::Known2x1(_) => num == 2,
            Self::Known3x0(_) | Self::Known3x1(_) => num == 3,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum FuncPtr {
    /// A function defined in Rust
    Bridge(BridgePtr),
    #[cfg(feature = "async")]
    /// An async function defined in Rust
    AsyncBridge(AsyncBridgePtr),
    /// A JIT compiled user function
    User(UserPtr),
    /// A known function
    Known(KnownFunc),
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

#[derive(Copy, Clone)]
pub(crate) enum ContPtr {
    /// A JIT compiled (or occasionally defined in Rust) continuation
    Continuation(ContinuationPtr),
    /// A continuation that exits a prompt. Can be dynamically replaced.
    /// The continuation of a prompt barrier will always be pop_dyn_stack.
    PromptBarrier { barrier_id: usize },
}

impl From<ContinuationPtr> for ContPtr {
    fn from(value: ContinuationPtr) -> Self {
        Self::Continuation(value)
    }
}

#[derive(Clone, Trace)]
#[repr(align(16))]
pub(crate) struct ProcedureInner {
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
        env: Vec<Value>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<ProcDebugInfo>>,
    ) -> Self {
        Self {
            env,
            func,
            num_required_args,
            variadic,
            is_variable_transformer: false,
            debug_info,
        }
    }

    #[cfg(feature = "async")]
    async fn apply_async_bridge(
        &self,
        func: AsyncBridgePtr,
        args: &[Value],
        barrier: &mut ContBarrier<'_>,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(&self.env, args, rest_args, barrier).await
    }

    fn apply_sync_bridge(
        &self,
        func: BridgePtr,
        args: &[Value],
        barrier: &mut ContBarrier,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(&self.env, args, rest_args, barrier)
    }

    fn apply_jit(
        &self,
        func: UserPtr,
        mut args: Vec<Value>,
        barrier: &mut ContBarrier,
    ) -> Application {
        if self.variadic {
            let mut rest_args = Value::null();
            let extra_args = args.len() - self.num_required_args;
            for _ in 0..extra_args {
                // TBD: Is pop or clone faster?
                rest_args = Value::from(Pair::immutable(args.pop().unwrap(), rest_args));
            }
            args.push(rest_args);
        }

        unsafe {
            let mut app = std::mem::MaybeUninit::<Application>::uninit();
            (func)(
                self.env.as_ptr(),
                args.as_ptr(),
                barrier as *mut ContBarrier<'_>,
                &mut app,
            );
            app.assume_init()
        }
    }

    /// Apply the arguments to the function, returning the next application.
    #[maybe_async]
    pub fn apply(&self, args: Vec<Value>, barrier: &mut ContBarrier<'_>) -> Application {
        if let Err(raised) = check_args(self.num_required_args, self.variadic, &args, barrier) {
            return raised;
        }

        match self.func {
            FuncPtr::Bridge(sbridge) => self.apply_sync_bridge(sbridge, &args, barrier),
            #[cfg(feature = "async")]
            FuncPtr::AsyncBridge(abridge) => self.apply_async_bridge(abridge, &args, barrier).await,
            FuncPtr::User(user) => self.apply_jit(user, args, barrier),
            FuncPtr::Known(known) => known.apply(&args, barrier),
        }
    }

    #[cfg(feature = "async")]
    /// Attempt to call the function, and throw an error if is async
    pub fn apply_sync(&self, args: Vec<Value>, barrier: &mut ContBarrier) -> Application {
        if let Err(raised) = check_args(self.num_required_args, self.variadic, &args, barrier) {
            return raised;
        }

        match self.func {
            FuncPtr::Bridge(sbridge) => self.apply_sync_bridge(sbridge, &args, barrier),
            FuncPtr::AsyncBridge(_) => raise(
                Exception::error("attempt to apply async function in a sync-only context").into(),
                barrier,
            ),
            FuncPtr::User(user) => self.apply_jit(user, args, barrier),
            FuncPtr::Known(known) => known.apply(&args, barrier),
        }
    }
}

impl fmt::Debug for ProcedureInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
#[repr(transparent)]
pub struct Procedure(pub(crate) Gc<ProcedureInner>);

impl Procedure {
    #[allow(private_bounds)]
    /// Creates a new procedure. `func` must be a [`BridgePtr`] or an
    /// `AsyncBridgePtr` if `async` is enabled.
    pub fn new(
        env: Vec<Value>,
        func: impl Into<FuncPtr>,
        num_required_args: usize,
        variadic: bool,
    ) -> Self {
        Self::with_debug_info(env, func.into(), num_required_args, variadic, None)
    }

    pub(crate) fn with_debug_info(
        env: Vec<Value>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<ProcDebugInfo>>,
    ) -> Self {
        Self(Gc::new(ProcedureInner::new(
            env,
            func,
            num_required_args,
            variadic,
            debug_info,
        )))
    }

    /// Return the number of required arguments and whether or not this function
    /// is variadic
    pub fn get_formals(&self) -> (usize, bool) {
        (self.0.num_required_args, self.0.variadic)
    }

    /// Return the debug information associated with procedure, if it exists.
    pub fn get_debug_info(&self) -> Option<Arc<ProcDebugInfo>> {
        self.0.debug_info.clone()
    }

    pub fn is_variable_transformer(&self) -> bool {
        self.0.is_variable_transformer
    }

    /// Applies `args` to the procedure and returns the values it evaluates to.
    #[maybe_async]
    pub fn call(&self, args: &[Value]) -> Result<Vec<Value>, Exception> {
        maybe_await!(self.call_with_barrier(args, &mut ContBarrier::new()))
    }

    #[cfg(feature = "async")]
    pub fn call_sync(&self, args: &[Value]) -> Result<Vec<Value>, Exception> {
        self.call_sync_with_barrier(args, &mut ContBarrier::new())
    }

    #[maybe_async]
    pub fn call_with_barrier(
        &self,
        args: &[Value],
        barrier: &mut ContBarrier<'_>,
    ) -> Result<Vec<Value>, Exception> {
        maybe_await!(Application::new(self.clone(), args.to_vec()).eval(barrier))
    }

    #[cfg(feature = "async")]
    pub fn call_sync_with_barrier(
        &self,
        args: &[Value],
        barrier: &mut ContBarrier<'_>,
    ) -> Result<Vec<Value>, Exception> {
        Application::new(self.clone(), args.to_vec()).eval_sync(barrier)
    }

    pub(crate) fn to_primop(&self) -> Option<PrimOp> {
        use crate::{
            lists::{car, cdr, cons, list},
            num::{add, div, equal, greater, greater_equal, lesser, lesser_equal, mul, sub},
            proc::{BridgePtr, FuncPtr::Bridge},
            value::{not, null_pred, pair_pred},
        };
        use std::ptr::fn_addr_eq;

        const PRIMOP_TAB: &[(BridgePtr, PrimOp)] = &[
            (add, PrimOp::Add),
            (sub, PrimOp::Sub),
            (mul, PrimOp::Mul),
            (div, PrimOp::Div),
            (equal, PrimOp::Equal),
            (greater, PrimOp::Greater),
            (greater_equal, PrimOp::GreaterEqual),
            (lesser, PrimOp::Lesser),
            (lesser_equal, PrimOp::LesserEqual),
            (cons, PrimOp::Cons),
            (list, PrimOp::List),
            (car, PrimOp::Car),
            (cdr, PrimOp::Cdr),
            (not, PrimOp::Not),
            (null_pred, PrimOp::IsNull),
            (pair_pred, PrimOp::IsPair),
        ];

        let Bridge(ptr) = self.0.func else {
            return None;
        };

        for (builtin, primop) in PRIMOP_TAB.iter().copied() {
            // These function pointer comparisons are guaranteed to be meaningful since
            // they are returned from a store.
            if fn_addr_eq(ptr, builtin) {
                return Some(primop);
            }
        }

        None
    }

    pub(crate) fn to_known(&self) -> Option<KnownFunc> {
        match self.0.func {
            FuncPtr::Known(known) => Some(known),
            _ => None,
        }
    }
}

unsafe extern "C" fn halt(
    _env: *const Value,
    args: *const Value,
    _barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe { crate::runtime::halt(Value::into_raw(args.read()), out) }
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
    /// Call a procedure, passing it the continuation `k`.
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

    /// The main trampoline loop.
    #[maybe_async]
    fn eval_inner(mut self, barrier: &mut ContBarrier<'_>) -> Result<Vec<Value>, Exception> {
        loop {
            let Application { op, args } = self;
            self = match op {
                OpType::Proc(proc) => maybe_await!(proc.0.apply(args, barrier)),
                OpType::HaltOk => return Ok(args),
                OpType::HaltErr => {
                    let mut args = args;
                    return Err(Exception(args.pop().unwrap()));
                }
            };
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    ///
    /// Publishes the runtime's root dynamic state if none is already active;
    /// reuses whatever's published otherwise.
    #[cfg(feature = "async")]
    pub async fn eval(self, barrier: &mut ContBarrier<'_>) -> Result<Vec<Value>, Exception> {
        if !DYN_STATE.is_published() {
            with_root_dyn_state(Runtime::handle(), self.eval_inner(barrier)).await
        } else {
            self.eval_inner(barrier).await
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    #[cfg(not(feature = "async"))]
    pub fn eval(self, barrier: &mut ContBarrier<'_>) -> Result<Vec<Value>, Exception> {
        if !DYN_STATE.is_published() {
            with_root_dyn_state_sync(Runtime::handle(), move || self.eval_inner(barrier))
        } else {
            self.eval_inner(barrier)
        }
    }

    #[cfg(feature = "async")]
    fn eval_sync_inner(mut self, barrier: &mut ContBarrier) -> Result<Vec<Value>, Exception> {
        loop {
            let Application { op, args } = self;
            self = match op {
                OpType::Proc(proc) => proc.0.apply_sync(args, barrier),
                OpType::HaltOk => return Ok(args),
                OpType::HaltErr => {
                    let mut args = args;
                    return Err(Exception(args.pop().unwrap()));
                }
            };
        }
    }

    #[cfg(feature = "async")]
    /// Just like [eval] but throws an error if we encounter an async function.
    pub fn eval_sync(self, barrier: &mut ContBarrier) -> Result<Vec<Value>, Exception> {
        if !DYN_STATE.is_published() {
            with_root_dyn_state_sync(Runtime::handle(), move || self.eval_sync_inner(barrier))
        } else {
            self.eval_sync_inner(barrier)
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
    /// Documentation captured from the function definition.
    pub docs: Option<String>,
}

impl ProcDebugInfo {
    pub fn new(name: Option<Symbol>, args: Vec<Local>, location: Span) -> Self {
        Self {
            name: name.unwrap_or_else(|| Symbol::intern("<lambda>")),
            args,
            location,
            docs: None,
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
                file: std::sync::Arc::from(debug_info.file.to_string()),
            },
            docs: (!debug_info.docs.is_empty()).then(|| debug_info.docs.to_string()),
        }
    }
}

#[cps_bridge(def = "apply arg1 . args", lib = "(rnrs base builtins (6))")]
pub fn apply(
    _env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    if rest_args.is_empty() {
        return Err(Exception::wrong_num_of_args(2, args.len()));
    }
    let op: Procedure = args[0].clone().try_into()?;
    let (last, args) = rest_args.split_last().unwrap();
    let mut args = args.to_vec();
    list_to_vec(last, &mut args);
    Ok(Application::new(op.clone(), args))
}

////////////////////////////////////////////////////////////////////////////////
//
// Continuation barriers
//

#[cfg(feature = "async")]
type Param<'a> = &'a mut (dyn Any + Send + Sync);

#[cfg(not(feature = "async"))]
type Param<'a> = &'a mut dyn Any;

/// The dynamic state of a running program, owned by the trampoline as a
/// local and published as the current dynamic state for an evaluation.
#[derive(Trace)]
pub struct DynState {
    /// The active dynamic stack
    dyn_stack: Vec<DynStackElem>,
    /// Whether this is the state currently being evaluated. False for a
    /// dormant/idle slot; [`DynStateSlot::is_published`] is just this flag.
    published: bool,
}

impl DynState {
    fn new() -> Self {
        Self {
            dyn_stack: Vec::new(),
            published: false,
        }
    }

    /// Current ports cross a spawn boundary; winders, handlers, and prompts
    /// do not.
    fn spawn_snapshot(&self) -> DynState {
        DynState {
            dyn_stack: self
                .dyn_stack
                .iter()
                .filter(|elem| elem.crosses_spawn())
                .cloned()
                .collect(),
            published: false,
        }
    }
}

impl Default for DynState {
    fn default() -> Self {
        Self::new()
    }
}

impl DynStackElem {
    /// Only current ports survive a spawn.
    fn crosses_spawn(&self) -> bool {
        matches!(
            self,
            DynStackElem::CurrentInputPort(_) | DynStackElem::CurrentOutputPort(_)
        )
    }
}

std::thread_local! {
    /// Sync-trampoline dynamic state. Always holds a `DynState`; dormant
    /// (not currently evaluating) is `published == false`, not absence.
    static CURRENT_DYN_STATE: RefCell<DynState> = RefCell::new(DynState::new());
}

#[cfg(feature = "async")]
tokio::task_local! {
    /// Async-trampoline dynamic state (survives work-stealing).
    static TASK_DYN_STATE: RefCell<DynState>;
}

pub(crate) struct DynStateGuard(DynState);

impl Drop for DynStateGuard {
    fn drop(&mut self) {
        CURRENT_DYN_STATE.with(|c| *c.borrow_mut() = std::mem::take(&mut self.0));
    }
}

/// Unified access to the thread-local and task-local dynamic state.
struct DynStateSlot;

static DYN_STATE: DynStateSlot = DynStateSlot;

impl DynStateSlot {
    fn is_published(&self) -> bool {
        #[cfg(feature = "async")]
        if let Ok(published) = TASK_DYN_STATE.try_with(|c| c.borrow().published) {
            return published;
        }
        CURRENT_DYN_STATE.with(|c| c.borrow().published)
    }

    /// Short borrow of the current dynamic state; never hold across
    /// re-entry.
    fn with<R>(&self, f: impl FnOnce(&mut DynState) -> R) -> R {
        #[cfg(feature = "async")]
        {
            let task_active = TASK_DYN_STATE.try_with(|_| ()).is_ok();
            if task_active {
                return TASK_DYN_STATE.with(|c| f(&mut c.borrow_mut()));
            }
        }
        CURRENT_DYN_STATE.with(|c| f(&mut c.borrow_mut()))
    }

    /// Publish state for a synchronous evaluation.
    fn enter_sync(&self, mut state: DynState) -> DynStateGuard {
        state.published = true;
        let prev = CURRENT_DYN_STATE.with(|c| std::mem::replace(&mut *c.borrow_mut(), state));
        DynStateGuard(prev)
    }

    /// Publish state for an async evaluation.
    #[cfg(feature = "async")]
    async fn enter_async<F: Future>(&self, mut state: DynState, fut: F) -> F::Output {
        state.published = true;
        TASK_DYN_STATE.scope(RefCell::new(state), fut).await
    }
}

pub(crate) fn with_dyn_state_sync<R>(state: DynState, f: impl FnOnce() -> R) -> R {
    let _guard = DYN_STATE.enter_sync(state);
    f()
}

#[cfg(feature = "async")]
pub(crate) async fn with_dyn_state<F: Future>(state: DynState, fut: F) -> F::Output {
    DYN_STATE.enter_async(state, fut).await
}

/// On drop, takes whatever's published and checks it back into `runtime`,
/// marked no longer published. Runs before the enclosing [`DynStateGuard`]
/// (declared first, so dropped last) restores the previous (dormant) slot,
/// so it still observes the finished state.
struct RootDynStateCheckin(Runtime);

impl Drop for RootDynStateCheckin {
    fn drop(&mut self) {
        let mut state = DYN_STATE.with(std::mem::take);
        state.published = false;
        self.0.restore_dyn_state(state);
    }
}

/// Checks out `runtime`'s root dynamic state and publishes it for the
/// duration of `f`, then checks the (possibly mutated) state back in so
/// later top-level entries into `runtime` observe the same parameter
/// roots, current ports, etc.
pub(crate) fn with_root_dyn_state_sync<R>(runtime: Runtime, f: impl FnOnce() -> R) -> R {
    let state = runtime.checkout_dyn_state();
    let _guard = DYN_STATE.enter_sync(state);
    let _checkin = RootDynStateCheckin(runtime);
    f()
}

/// Async counterpart to [`with_root_dyn_state_sync`]. The checkin runs
/// inside the scoped future so it can still read the task-local dynamic
/// state once `fut` completes (or is dropped without completing).
///
/// Boxed so this call's stack frame stays a fixed, small size regardless of
/// what `fut` contains: macro transformers re-enter `eval` (and thus this
/// function) once per level of macro nesting in the source being expanded,
/// so an unboxed frame here grows with source-level macro nesting depth and
/// can overflow the stack on deeply macro-heavy code.
#[cfg(feature = "async")]
pub(crate) async fn with_root_dyn_state<F: Future + Send>(runtime: Runtime, fut: F) -> F::Output
where
    F::Output: Send,
{
    let state = runtime.checkout_dyn_state();
    let boxed: std::pin::Pin<Box<dyn Future<Output = F::Output> + Send>> = Box::pin(async move {
        let _checkin = RootDynStateCheckin(runtime);
        fut.await
    });
    DYN_STATE.enter_async(state, boxed).await
}

// TODO: We should certainly try to optimize these functions. Linear
// searching isn't _great_, although in practice I can't imagine this stack
// will ever get very large.

pub fn current_exception_handler() -> Option<Procedure> {
    DYN_STATE.with(|s| {
        s.dyn_stack.iter().rev().find_map(|elem| match elem {
            DynStackElem::ExceptionHandler(proc) => Some(proc.clone()),
            _ => None,
        })
    })
}

pub fn current_input_port() -> Port {
    DYN_STATE.with(|s| {
        s.dyn_stack
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
    })
}

pub fn current_output_port() -> Port {
    DYN_STATE.with(|s| {
        s.dyn_stack
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
    })
}

pub(crate) fn push_dyn_stack(elem: DynStackElem) {
    DYN_STATE.with(|s| s.dyn_stack.push(elem));
}

pub(crate) fn pop_dyn_stack() -> Option<DynStackElem> {
    DYN_STATE.with(|s| s.dyn_stack.pop())
}

pub(crate) fn dyn_stack_last() -> Option<DynStackElem> {
    DYN_STATE.with(|s| s.dyn_stack.last().cloned())
}

pub(crate) fn dyn_stack_len() -> usize {
    DYN_STATE.with(|s| s.dyn_stack.len())
}

pub(crate) fn dyn_stack_is_empty() -> bool {
    DYN_STATE.with(|s| s.dyn_stack.is_empty())
}

pub(crate) fn dyn_state_snapshot() -> DynState {
    if !DYN_STATE.is_published() {
        return DynState::new();
    }
    DYN_STATE.with(|s| s.spawn_snapshot())
}

/// A continuation barrier. Escape procedures created within a continuation
/// barrier cannot be called within another barrier.
pub struct ContBarrier<'a> {
    /// The id of the barrier. Checked when calling an escape procedure
    id: usize,
    /// The current live continuations for the program. Effectively the call
    /// stack. Includes active [continuation marks](https://srfi.schemers.org/srfi-157/srfi-157.html).
    cont_stack: ContStack,
    /// The active installed mutable parameters
    params: HashMap<Symbol, Param<'a>>,
}

impl<'a> ContBarrier<'a> {
    pub fn new() -> Self {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

        let mut this = Self {
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            cont_stack: ContStack::default(),
            params: HashMap::new(),
        };

        // The call stack always contains a top-level halt continuation:
        this.push_cont([], ContPtr::Continuation(halt), 0, true);

        this
    }

    /// Captures the barrier id and a copy of the current dyn_stack/cont_stack,
    /// for restoring later (call/cc, prompts).
    pub fn save(&self) -> SavedDynamicState {
        DYN_STATE.with(|s| SavedDynamicState {
            id: self.id,
            dyn_stack: s.dyn_stack.clone(),
            cont_stack: self.cont_stack.clone(),
        })
    }

    pub fn add_param(
        &mut self,
        key: impl Into<Symbol>,
        #[cfg(feature = "async")] val: &'a mut (impl Any + Send + Sync),
        #[cfg(not(feature = "async"))] val: &'a mut impl Any,
    ) {
        self.params.insert(key.into(), val);
    }

    pub fn get_param<'b>(&'b mut self, key: impl Into<Symbol>) -> Option<Param<'b>> {
        self.params.get_mut(&key.into()).map(|v| v.deref_mut())
    }

    pub fn get_params_disjoint<'b, const N: usize>(
        &'b mut self,
        keys: [&Symbol; N],
    ) -> [Option<Param<'b>>; N] {
        self.params
            .get_disjoint_mut(keys)
            .map(|v| v.map(|v| v.deref_mut()))
    }

    pub fn iter_params<'b>(&'b mut self) -> impl Iterator<Item = (Symbol, Param<'b>)> {
        self.params.iter_mut().map(|(k, v)| (*k, v.deref_mut()))
    }

    /// Constructs a child barrier from the current barrier, extracting an array
    /// of parameters that are not automatically passed onto the child.
    /// dyn_stack isn't copied here (it's ambient, already shared); cont_stack
    /// is, via `save`/`From<SavedDynamicState>`.
    pub fn child_barrier<'b, 'c, const N: usize>(
        &'b mut self,
        params: [impl Into<Symbol>; N],
    ) -> ([Option<Param<'b>>; N], ContBarrier<'c>)
    where
        'b: 'c,
    {
        let param_to_index = params
            .into_iter()
            .enumerate()
            .map(|(idx, param)| (param.into(), idx))
            .collect::<HashMap<_, _>>();
        let mut params = [const { None }; N];
        let mut child_barrier = ContBarrier::from(self.save());
        for (key, value) in self.params.iter_mut() {
            let value = value.deref_mut();
            if let Some(idx) = param_to_index.get(key) {
                params[*idx] = Some(value);
            } else {
                child_barrier.params.insert(*key, value);
            }
        }
        (params, child_barrier)
    }

    #[cfg(feature = "continuation-marks")]
    pub(crate) fn current_marks(&self, tag: Symbol) -> Vec<Value> {
        self.cont_stack
            .frames
            .iter()
            .rev()
            .map(|frame| &frame.marks)
            .flat_map(|marks| marks.get(&tag).cloned())
            .collect()
    }

    #[cfg(feature = "continuation-marks")]
    pub(crate) fn set_continuation_mark(&mut self, tag: Symbol, val: Value) {
        self.cont_stack
            .frames
            .last_mut()
            .unwrap()
            .marks
            .insert(tag, val);
    }

    /// Push a continuation onto the current call stack.
    #[allow(private_bounds)]
    pub fn push_cont(
        &mut self,
        env: impl IntoIterator<Item = Value>,
        func_ptr: impl Into<ContPtr>,
        num_required_args: usize,
        variadic: bool,
    ) {
        self.cont_stack
            .push_cont(func_ptr.into(), env, num_required_args, variadic);
    }

    pub fn call_cont(&mut self, mut args: Vec<Value>) -> Application {
        let curr_frame = self.cont_stack.frames.pop().unwrap();
        let env: SmallVec<[Value; 10]> =
            self.cont_stack.envs.drain(curr_frame.env_start..).collect();

        if let Err(raised) = check_args(
            curr_frame.num_required_args,
            curr_frame.variadic,
            &args,
            self,
        ) {
            return raised;
        }

        if curr_frame.variadic {
            let mut rest_args = Value::null();
            let extra_args = args.len() - curr_frame.num_required_args;
            for _ in 0..extra_args {
                rest_args = Value::from(Pair::immutable(args.pop().unwrap(), rest_args));
            }
            args.push(rest_args);
        }

        match curr_frame.func_ptr {
            ContPtr::Continuation(func) => unsafe {
                let mut app = std::mem::MaybeUninit::<Application>::uninit();
                (func)(
                    env.as_ptr(),
                    args.as_ptr(),
                    self as *mut ContBarrier<'_>,
                    &mut app,
                );
                app.assume_init()
            },
            ContPtr::PromptBarrier { .. } => {
                pop_dyn_stack();
                let mut values: Vec<Value> = args[..curr_frame.num_required_args].to_vec();
                if curr_frame.variadic {
                    list_to_vec(&args[curr_frame.num_required_args], &mut values);
                }
                self.call_cont(values)
            }
        }
    }

    pub fn cont_formals(&self) -> (usize, bool) {
        let curr_frame = self.cont_stack.frames.last().unwrap();
        (curr_frame.num_required_args, curr_frame.variadic)
    }
}

impl Default for ContBarrier<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, 'b, 'c> From<&'b mut ContBarrier<'a>> for ContBarrier<'c>
where
    'b: 'c,
{
    fn from(value: &'b mut ContBarrier<'a>) -> Self {
        let mut new_barrier = ContBarrier::from(value.save());
        for (key, value) in value.params.iter_mut() {
            new_barrier.params.insert(*key, value.deref_mut());
        }
        new_barrier
    }
}

/// Independent of the current dynamic state: a plain value, safe to embed
/// and pass around Scheme code.
#[derive(Clone, Trace)]
pub struct SavedDynamicState {
    id: usize,
    dyn_stack: Vec<DynStackElem>,
    cont_stack: ContStack,
}

impl SavedDynamicState {
    pub(crate) fn dyn_stack_get(&self, idx: usize) -> Option<&DynStackElem> {
        self.dyn_stack.get(idx)
    }

    pub(crate) fn dyn_stack_len(&self) -> usize {
        self.dyn_stack.len()
    }
}

impl From<SavedDynamicState> for ContBarrier<'_> {
    fn from(value: SavedDynamicState) -> Self {
        // dyn_stack isn't restored here: it's ambient (shared with whatever
        // published it), not barrier-local, so a sibling/child barrier
        // already observes it without copying.
        ContBarrier {
            cont_stack: value.cont_stack,
            ..Default::default()
        }
    }
}

unsafe impl Embeddable for SavedDynamicState {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(ty: SavedDynamicState, name: "%dynamic-state", sealed: true, opaque: true)
    }
}

#[derive(Clone, Debug, PartialEq, Trace)]
pub(crate) enum DynStackElem {
    Prompt(Prompt),
    Winder(Winder),
    ExceptionHandler(Procedure),
    CurrentInputPort(Port),
    CurrentOutputPort(Port),
}

/// Named distinctly from the free function `pop_dyn_stack` to avoid a clash.
pub(crate) unsafe extern "C" fn pop_dyn_stack_cont(
    _env: *const Value,
    args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        let barrier = barrier.as_mut().unwrap_unchecked();
        pop_dyn_stack();

        let (num_required_args, variadic) = barrier.cont_formals();
        let mut collected_args: Vec<_> = (0..num_required_args)
            .map(|i| args.add(i).as_ref().unwrap().clone())
            .collect();
        if variadic {
            let rest_args = args.add(num_required_args).as_ref().unwrap().clone();
            let mut vec = Vec::new();
            crate::lists::list_to_vec(&rest_args, &mut vec);
            collected_args.extend(vec);
        }

        (*out).write(barrier.call_cont(collected_args));
    }
}

#[derive(Default, Clone, Trace)]
pub(crate) struct ContStack {
    frames: Vec<ContFrame>,
    envs: Vec<Value>,
}

impl ContStack {
    pub(crate) fn push_cont(
        &mut self,
        func_ptr: ContPtr,
        env: impl IntoIterator<Item = Value>,
        num_required_args: usize,
        variadic: bool,
    ) {
        let env_start = self.envs.len();
        self.envs.extend(env);
        self.frames.push(ContFrame {
            func_ptr,
            env_start,
            num_required_args,
            variadic,
            #[cfg(feature = "continuation-marks")]
            marks: HashMap::default(),
        });
    }
}

#[derive(Clone, Trace)]
pub(crate) struct ContFrame {
    #[trace(skip)]
    func_ptr: ContPtr,
    env_start: usize,
    num_required_args: usize,
    variadic: bool,
    #[cfg(feature = "continuation-marks")]
    marks: HashMap<Symbol, Value>,
}

fn check_args(
    num_required_args: usize,
    variadic: bool,
    args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<(), Application> {
    // Error if the number of arguments provided is incorrect.
    if args.len() < num_required_args || (!variadic && args.len() > num_required_args) {
        return Err(raise(
            Exception::wrong_num_of_args(num_required_args, args.len()).into(),
            barrier,
        ));
    }

    Ok(())
}

#[cfg(feature = "continuation-marks")]
#[cps_bridge(def = "print-trace", lib = "(rnrs base builtins (6))")]
pub fn print_trace(
    _env: &[Value],
    _args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    println!(
        "trace: {:#?}",
        barrier.current_marks(Symbol::intern("trace"))
    );
    Ok(barrier.call_cont(Vec::new()))
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
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let proc: Procedure = args[0].clone().try_into()?;
    let (req_args, variaidic) = barrier.cont_formals();

    let escape = Procedure::new(
        vec![Value::from(barrier.save())],
        FuncPtr::Bridge(escape_procedure),
        req_args,
        variaidic,
    );

    Ok(Application::new(proc, vec![Value::from(escape)]))
}

/// Prepare the continuation for call/cc. Clones the continuation environment
/// and creates a closure that calls the appropriate winders.
#[cps_bridge]
fn escape_procedure(
    env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    // env[0] is the continuation environment:
    let saved_barrier = env[0]
        .clone()
        .cast::<Embedded<SavedDynamicState>>()
        .unwrap();

    // Cross-barrier escape must halt this trampoline (halt_err) instead of
    // raising through the exception-handler search. With shared dynamic state,
    // raising would find a handler belonging to an ancestor evaluation (e.g.
    // guard's), whose own escape continuation also crosses this barrier,
    // causing a second rejection with no handler left to catch it.
    if saved_barrier.id != barrier.id {
        return Ok(Application::halt_err(Value::from(Exception::error(
            "attempt to cross continuation barrier",
        ))));
    }

    let args = args.iter().chain(rest_args).cloned().collect::<Vec<_>>();

    barrier.cont_stack = saved_barrier.cont_stack.clone();
    barrier.push_cont(
        vec![Value::from(args), env[0].clone()],
        ContPtr::Continuation(unwind),
        0,
        false,
    );
    Ok(barrier.call_cont(Vec::new()))
}

unsafe extern "C" fn unwind(
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        // env[0] are the arguments to pass to k
        let args = env.as_ref().unwrap().clone();

        // env[1] is the stack we are trying to reach
        let dest_stack_val = env.add(1).as_ref().unwrap().clone();
        let dest_stack = dest_stack_val
            .clone()
            .try_to::<Embedded<SavedDynamicState>>()
            .unwrap();
        let dest_stack_read = dest_stack.as_ref();

        let barrier = barrier.as_mut().unwrap_unchecked();

        while !dyn_stack_is_empty()
            && (dyn_stack_len() > dest_stack_read.dyn_stack_len()
                || dyn_stack_last().as_ref() != dest_stack_read.dyn_stack_get(dyn_stack_len() - 1))
        {
            match pop_dyn_stack() {
                None => {
                    break;
                }
                Some(DynStackElem::Winder(winder)) => {
                    // Call the out winder while unwinding
                    barrier.push_cont(
                        [args, dest_stack_val],
                        ContPtr::Continuation(unwind),
                        0,
                        false,
                    );
                    let app = Application::new(winder.out_thunk, Vec::new());
                    (*out).write(app);
                    return;
                }
                _ => (),
            };
        }

        // Begin winding
        barrier.push_cont(
            [args, dest_stack_val, Value::from(false)],
            ContPtr::Continuation(wind),
            0,
            false,
        );
        (*out).write(barrier.call_cont(Vec::new()));
    }
}

unsafe extern "C" fn wind(
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        // env[0] are the arguments to pass to k
        let args = env.as_ref().unwrap().clone();

        // env[0] is the stack we are trying to reach
        let dest_stack_val = env.add(1).as_ref().unwrap().clone();
        let dest_stack = dest_stack_val
            .try_to::<Embedded<SavedDynamicState>>()
            .unwrap();
        let dest_stack_read = dest_stack.as_ref();

        let barrier = barrier.as_mut().unwrap_unchecked();

        // env[2] is potentially a winder that we should push onto the dyn stack
        let winder = env.add(2).as_ref().unwrap().clone();
        if winder.is_true() {
            let winder = winder.try_to::<Embedded<Winder>>().unwrap();
            push_dyn_stack(DynStackElem::Winder(winder.as_ref().clone()));
        }

        while dyn_stack_len() < dest_stack_read.dyn_stack_len() {
            match dest_stack_read.dyn_stack_get(dyn_stack_len()).cloned() {
                None => {
                    break;
                }
                Some(DynStackElem::Winder(winder)) => {
                    // Call the in winder while winding
                    let in_thunk = winder.in_thunk.clone();
                    barrier.push_cont(
                        [args, dest_stack_val, Value::from(winder)],
                        ContPtr::Continuation(wind),
                        0,
                        false,
                    );
                    let app = Application::new(in_thunk, Vec::new());
                    (*out).write(app);
                    return;
                }
                Some(elem) => push_dyn_stack(elem),
            }
        }

        let args: Vector = args.try_into().unwrap();
        let args = args.0.vec.read().to_vec();

        (*out).write(barrier.call_cont(args));
    }
}

unsafe extern "C" fn call_consumer_with_values(
    env: *const Value,
    args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        // env[0] is the consumer
        let consumer = env.as_ref().unwrap().clone();
        let type_name = consumer.type_name();

        let consumer: Procedure = match consumer.try_into() {
            Ok(consumer) => consumer,
            _ => {
                let raised = raise(
                    Exception::invalid_operator(&type_name).into(),
                    barrier.as_mut().unwrap_unchecked(),
                );
                (*out).write(raised);
                return;
            }
        };

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

        (*out).write(Application::new(consumer.clone(), collected_args));
    }
}

#[cps_bridge(
    def = "call-with-values producer consumer",
    lib = "(rnrs base builtins (6))"
)]
pub fn call_with_values(
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [producer, consumer] = args else {
        return Err(Exception::wrong_num_of_args(2, args.len()));
    };

    let producer: Procedure = producer.clone().try_into()?;
    let consumer: Procedure = consumer.clone().try_into()?;

    // Get the details of the consumer:
    let (num_required_args, variadic) = { (consumer.0.num_required_args, consumer.0.variadic) };

    barrier.push_cont(
        [Value::from(consumer)],
        ContPtr::Continuation(call_consumer_with_values),
        num_required_args,
        variadic,
    );

    Ok(Application::new(producer, Vec::new()))
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

unsafe impl Embeddable for Winder {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(ty: Winder, name: "%winder", sealed: true, opaque: true)
    }
}

#[cps_bridge(def = "dynamic-wind in body out", lib = "(rnrs base builtins (6))")]
pub fn dynamic_wind(
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [in_thunk_val, body_thunk_val, out_thunk_val] = args else {
        return Err(Exception::wrong_num_of_args(3, args.len()));
    };

    let in_thunk: Procedure = in_thunk_val.clone().try_into()?;
    let _: Procedure = body_thunk_val.clone().try_into()?;

    barrier.push_cont(
        [
            in_thunk_val.clone(),
            body_thunk_val.clone(),
            out_thunk_val.clone(),
        ],
        ContPtr::Continuation(call_body_thunk),
        0,
        true,
    );

    Ok(Application::new(in_thunk, Vec::new()))
}

pub(crate) unsafe extern "C" fn call_body_thunk(
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        // env[0] is the in thunk
        let in_thunk = env.as_ref().unwrap().clone();

        // env[1] is the body thunk
        let body_thunk: Procedure = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        // env[2] is the out thunk
        let out_thunk = env.add(2).as_ref().unwrap().clone();

        let barrier = barrier.as_mut().unwrap_unchecked();

        push_dyn_stack(DynStackElem::Winder(Winder {
            in_thunk: in_thunk.clone().try_into().unwrap(),
            out_thunk: out_thunk.clone().try_into().unwrap(),
        }));

        barrier.push_cont([out_thunk], ContPtr::Continuation(call_out_thunks), 0, true);

        (*out).write(Application::new(body_thunk, Vec::new()));
    }
}

pub(crate) unsafe extern "C" fn call_out_thunks(
    env: *const Value,
    args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        // env[0] is the out thunk
        let out_thunk: Procedure = env.as_ref().unwrap().clone().try_into().unwrap();

        // args[0] is the result of the body thunk
        let body_thunk_res = args.as_ref().unwrap().clone();

        let barrier = barrier.as_mut().unwrap_unchecked();
        pop_dyn_stack();

        barrier.push_cont(
            vec![body_thunk_res],
            ContPtr::Continuation(forward_body_thunk_result),
            0,
            true,
        );

        (*out).write(Application::new(out_thunk, Vec::new()));
    }
}

unsafe extern "C" fn forward_body_thunk_result(
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        // env[0] is the result of the body thunk
        let body_thunk_res = env.as_ref().unwrap().clone();

        let mut args = Vec::new();
        list_to_vec(&body_thunk_res, &mut args);

        (*out).write(barrier.as_mut().unwrap().call_cont(args));
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
}

#[cps_bridge(def = "call-with-prompt tag thunk handler", lib = "(prompts)")]
pub fn call_with_prompt(
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    static BARRIER_ID: AtomicUsize = AtomicUsize::new(0);

    let [tag, thunk, handler] = args else {
        unreachable!()
    };

    let (req_args, variadic) = barrier.cont_formals();
    let tag: Symbol = tag.clone().try_into().unwrap();

    let barrier_id = BARRIER_ID.fetch_add(1, Ordering::Relaxed);

    push_dyn_stack(DynStackElem::Prompt(Prompt {
        tag,
        handler: handler.clone().try_into().unwrap(),
        barrier_id,
    }));

    barrier.push_cont(
        Vec::new(),
        ContPtr::PromptBarrier { barrier_id },
        req_args,
        variadic,
    );

    Ok(Application::new(
        thunk.clone().try_into().unwrap(),
        Vec::new(),
    ))
}

#[cps_bridge(def = "abort-to-prompt tag . values", lib = "(prompts)")]
pub fn abort_to_prompt(
    _env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [tag] = args else { unreachable!() };
    barrier.push_cont(
        vec![
            Value::from(rest_args.to_vec()),
            tag.clone(),
            Value::from(barrier.save()),
        ],
        ContPtr::Continuation(unwind_to_prompt),
        0,
        false,
    );
    Ok(barrier.call_cont(Vec::new()))
}

unsafe extern "C" fn unwind_to_prompt(
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        // env[0] is the arguments passed to abort-to-prompt:
        let args = env.as_ref().unwrap().clone();
        // env[1] is the prompt tag
        let tag: Symbol = env.add(1).as_ref().unwrap().clone().try_into().unwrap();
        // env[2] is the saved dyn stack
        let saved_barrier = env.add(2).as_ref().unwrap().clone();

        let barrier = barrier.as_mut().unwrap_unchecked();

        loop {
            let app = match pop_dyn_stack() {
                None => {
                    // If the stack is empty, we should return the error
                    Application::halt_err(Value::from(Exception::error(format!(
                        "no prompt tag {tag} found"
                    ))))
                }
                Some(DynStackElem::Prompt(Prompt {
                    tag: prompt_tag,
                    barrier_id,
                    handler,
                })) if prompt_tag == tag => {
                    // Split the continuation at the barrier:
                    let barrier_idx = barrier
                        .cont_stack
                        .frames
                        .iter()
                        .position(|frame| {
                            matches!(frame.func_ptr, ContPtr::PromptBarrier { barrier_id: b } if b == barrier_id)
                        })
                        .unwrap();
                    let env_base = barrier
                        .cont_stack
                        .frames
                        .get(barrier_idx + 1)
                        .map_or(barrier.cont_stack.envs.len(), |frame| frame.env_start);
                    let mut delimited_frames =
                        barrier.cont_stack.frames[barrier_idx + 1..].to_vec();
                    for frame in &mut delimited_frames {
                        frame.env_start -= env_base;
                    }
                    let delimited_cont = ContStack {
                        frames: delimited_frames,
                        envs: barrier.cont_stack.envs[env_base..].to_vec(),
                    };

                    let barrier_env_base = barrier.cont_stack.frames[barrier_idx].env_start;
                    barrier.cont_stack.frames.truncate(barrier_idx);
                    barrier.cont_stack.envs.truncate(barrier_env_base);

                    let saved_barrier = saved_barrier
                        .try_to::<Embedded<SavedDynamicState>>()
                        .unwrap();
                    let prompt_delimited_barrier = SavedDynamicState {
                        id: saved_barrier.id,
                        dyn_stack: saved_barrier.as_ref().dyn_stack[dyn_stack_len() + 1..].to_vec(),
                        cont_stack: delimited_cont,
                    };

                    let mut handler_args = vec![Value::from(Procedure::new(
                        vec![Value::from(prompt_delimited_barrier)],
                        FuncPtr::Bridge(delimited_continuation),
                        0,
                        true,
                    ))];
                    handler_args.extend(args.cast::<Vector>().unwrap().iter());
                    Application::new(handler, handler_args)
                }
                Some(DynStackElem::Winder(winder)) => {
                    barrier.push_cont(
                        vec![args, Value::from(tag), saved_barrier],
                        ContPtr::Continuation(unwind_to_prompt),
                        0,
                        false,
                    );
                    Application::new(winder.out_thunk, Vec::new())
                }
                _ => continue,
            };
            (*out).write(app);
            return;
        }
    }
}

#[cps_bridge]
fn delimited_continuation(
    env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    // env[0] is the captured delimited continuation.
    let saved_barrier_val = env[0].clone();
    let saved_barrier = saved_barrier_val.try_to::<Embedded<SavedDynamicState>>()?;

    // Splice the captured frames onto the current continuation.
    let base = barrier.cont_stack.envs.len();
    barrier
        .cont_stack
        .envs
        .extend(saved_barrier.as_ref().cont_stack.envs.iter().cloned());
    for frame in &saved_barrier.as_ref().cont_stack.frames {
        let mut frame = frame.clone();
        frame.env_start += base;
        barrier.cont_stack.frames.push(frame);
    }

    // Restore the captured dynamic stack entries and rewind
    let values = Value::from(args.iter().chain(rest_args).cloned().collect::<Vec<_>>());
    barrier.push_cont(
        [
            values,
            saved_barrier_val,
            Value::from(0),
            Value::from(false),
        ],
        ContPtr::Continuation(wind_delim),
        0,
        false,
    );
    Ok(barrier.call_cont(Vec::new()))
}

unsafe extern "C" fn wind_delim(
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        let barrier = barrier.as_mut().unwrap_unchecked();

        // env[0] are the values to resume the delimited continuation with
        let args = env.as_ref().unwrap().clone();

        // env[1] is the saved state whose dynamic stack we are re-establishing
        let dest_stack_val = env.add(1).as_ref().unwrap().clone();
        let dest_stack = dest_stack_val
            .try_to::<Embedded<SavedDynamicState>>()
            .unwrap();

        // env[2] is how far into that dynamic stack we've wound
        let mut idx: usize = env.add(2).as_ref().unwrap().cast().unwrap();

        // env[3] is the winder whose in thunk just ran.
        let winder = env.add(3).as_ref().unwrap().clone();
        if winder.is_true() {
            let winder = winder.try_to::<Embedded<Winder>>().unwrap();
            push_dyn_stack(DynStackElem::Winder(winder.as_ref().clone()));
        }

        while let Some(elem) = dest_stack.as_ref().dyn_stack_get(idx) {
            idx += 1;

            if let DynStackElem::Winder(winder) = elem {
                barrier.push_cont(
                    vec![
                        args,
                        dest_stack_val,
                        Value::from(idx),
                        Value::from(winder.clone()),
                    ],
                    ContPtr::Continuation(wind_delim),
                    0,
                    false,
                );
                (*out).write(Application::new(winder.in_thunk.clone(), Vec::new()));
                return;
            }
            push_dyn_stack(elem.clone());
        }

        let args: Vector = args.try_into().unwrap();
        let args = args.0.vec.read().to_vec();
        (*out).write(barrier.call_cont(args));
    }
}
