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
//! done with a `bridge` function taking `#[env]` parameters and a call to
//! [`Procedure::new`]. The `#[env]` parameters are converted from the vector
//! passed to the `new` function:
//!
//! ```
//! # use scheme_rs::{proc::{Procedure, BridgePtr, Application, ContBarrier, Args},
//! # registry::bridge, value::Value, exceptions::Exception};
//! #[bridge]
//! fn closure(
//!     #[env] captured: Value,
//!     barrier: &mut ContBarrier,
//! ) -> Result<Application, Exception> {
//!     Ok(barrier.call_cont(Args::pack([captured])))
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
//! #     proc::{Procedure, BridgePtr, Application, ContBarrier, Args},
//! #     registry::bridge, value::{Value, Cell},
//! #     exceptions::Exception,
//! #     num::Number,
//! # };
//! #[bridge]
//! fn next_num(
//!     #[env] cell: Cell,
//!     barrier: &mut ContBarrier,
//! ) -> Result<Application, Exception> {
//!     let curr: Number = cell.get().try_into()?;
//!
//!     // Increment the cell
//!     cell.set(Value::from(curr.clone() + Number::from(1)));
//!
//!     // Return the previous value:
//!     Ok(barrier.call_cont(Args::pack([Value::from(curr)])))
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
    lists::{Pair, list_len, list_to_vec},
    ports::{BufferMode, Port, Transcoder},
    records::{Embeddable, Embedded, RecordTypeDescriptor, rtd},
    registry::BridgeFnDebugInfo,
    symbols::Symbol,
    syntax::Span,
    value::Value,
};
use scheme_rs_macros::{bridge, maybe_async, maybe_await};
use std::{
    any::Any,
    collections::HashMap,
    fmt,
    mem::MaybeUninit,
    ops::DerefMut,
    ptr::NonNull,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

/// An opaque pointer to the tail calling convention body of a JIT compiled
/// function.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) struct JitPtr(pub(crate) *const u8);

unsafe impl Send for JitPtr {}
unsafe impl Sync for JitPtr {}

/// A function pointer to a generated continuation.
pub(crate) type ContinuationPtr = extern "C" fn(
    arg1: Value,
    arg2: Value,
    arg3: Value,
    arg4: Value,
    argn: Value,
    barrier: &mut ContBarrier<'_>,
    out: &mut MaybeUninit<Application>,
);

/// A function pointer to a generated user function.
pub(crate) type UserPtr = extern "C" fn(
    proc: Procedure,
    arg1: Value,
    arg2: Value,
    arg3: Value,
    arg4: Value,
    argn: Value,
    barrier: &mut ContBarrier<'_>,
    out: &mut MaybeUninit<Application>,
);

/// A function pointer to a sync Rust bridge function.
pub type BridgePtr = extern "C" fn(
    proc: Procedure,
    arg1: Value,
    arg2: Value,
    arg3: Value,
    arg4: Value,
    argn: Value,
    barrier: &mut ContBarrier<'_>,
    out: &mut MaybeUninit<Application>,
);

/// A function pointer to an async Rust bridge function.
#[cfg(feature = "async")]
pub type AsyncBridgePtr = for<'a> fn(
    proc: Procedure,
    arg1: Value,
    arg2: Value,
    arg3: Value,
    arg4: Value,
    argn: Value,
    barrier: &'a mut ContBarrier<'_>,
) -> futures::future::BoxFuture<'a, Application>;

pub const MAX_DIRECT_ARGS: usize = 4;

#[repr(transparent)]
pub struct Args(pub [Value; MAX_DIRECT_ARGS + 1]);

impl Args {
    pub fn pack(args: impl IntoIterator<IntoIter: DoubleEndedIterator<Item = Value>>) -> Self {
        let mut slots = Self::empty();
        let mut args = args.into_iter();
        for slot in slots.0.iter_mut().take(MAX_DIRECT_ARGS) {
            let Some(arg) = args.next() else {
                return slots;
            };
            *slot = arg;
        }
        let mut argn = Value::null();
        while let Some(arg) = args.next_back() {
            argn = Value::from(Pair::immutable(arg, argn));
        }
        slots.0[MAX_DIRECT_ARGS] = argn;
        slots
    }

    pub fn from_slice(args: &[Value]) -> Self {
        Self::pack(args.iter().cloned())
    }

    pub fn empty() -> Self {
        Self([
            Value::undefined(),
            Value::undefined(),
            Value::undefined(),
            Value::undefined(),
            Value::null(),
        ])
    }

    pub fn from_list(mut list: Value) -> Self {
        let mut slots = Self::empty();
        for slot in slots.0.iter_mut().take(MAX_DIRECT_ARGS) {
            let Some(pair) = list.cast::<Pair>() else {
                return slots;
            };
            *slot = pair.car();
            list = pair.cdr();
        }
        slots.0[MAX_DIRECT_ARGS] = list;
        slots
    }

    pub fn into_list(self) -> Value {
        let Self([arg1, arg2, arg3, arg4, argn]) = self;
        cons_direct_args([arg1, arg2, arg3, arg4], argn)
    }

    pub fn into_vec(self) -> Vec<Value> {
        let Self([arg1, arg2, arg3, arg4, argn]) = self;
        let mut out = Vec::new();
        for arg in [arg1, arg2, arg3, arg4] {
            if arg.is_undefined() {
                return out;
            }
            out.push(arg);
        }
        list_to_vec(&argn, &mut out);
        out
    }

    pub fn len(&self) -> usize {
        let mut count = 0;
        for arg in &self.0[..MAX_DIRECT_ARGS] {
            if arg.is_undefined() {
                return count;
            }
            count += 1;
        }
        count + list_len(&self.0[MAX_DIRECT_ARGS])
    }

    pub fn is_empty(&self) -> bool {
        self.0[0].is_undefined()
    }
}

/// Cons the provided direct argument slots onto `tail`, skipping any that
/// are undefined.
fn cons_direct_args<const N: usize>(direct: [Value; N], tail: Value) -> Value {
    let mut list = tail;
    for arg in direct.into_iter().rev() {
        if !arg.is_undefined() {
            list = Value::from(Pair::immutable(arg, list));
        }
    }
    list
}

impl Default for Args {
    fn default() -> Self {
        Self::empty()
    }
}

pub type KnownFnPtr0 = extern "C" fn(error: &mut Value) -> Value;
pub type KnownFnPtr1 = extern "C" fn(Value, error: &mut Value) -> Value;
pub type KnownFnPtr2 = extern "C" fn(Value, Value, error: &mut Value) -> Value;
pub type KnownFnPtr3 = extern "C" fn(Value, Value, Value, error: &mut Value) -> Value;

#[derive(Copy, Clone, Debug)]
pub enum KnownFunc {
    Known0x1(KnownFnPtr0),
    Known1x0(KnownFnPtr1),
    Known1x1(KnownFnPtr1),
    Known2x0(KnownFnPtr2),
    Known2x1(KnownFnPtr2),
    Known3x0(KnownFnPtr3),
    Known3x1(KnownFnPtr3),
}

impl KnownFunc {
    fn apply(self, args: Args, barrier: &mut ContBarrier<'_>) -> Application {
        let expected = self.num_args();
        if args.0[..expected].iter().any(Value::is_undefined) || !args.0[expected].is_undefined() {
            return raise(
                Exception::wrong_num_of_args(expected, args.len()).into(),
                barrier,
            );
        }
        let Args([arg1, arg2, arg3, _, _]) = args;
        let mut error = Value::undefined();
        let res = match self {
            Self::Known0x1(func) => func(&mut error),
            Self::Known1x0(func) | Self::Known1x1(func) => func(arg1, &mut error),
            Self::Known2x0(func) | Self::Known2x1(func) => func(arg1, arg2, &mut error),
            Self::Known3x0(func) | Self::Known3x1(func) => func(arg1, arg2, arg3, &mut error),
        };
        if res.is_undefined() {
            raise(error, barrier)
        } else if self.returns_value() {
            barrier.call_cont(Args::pack([res]))
        } else {
            barrier.call_cont(Args::empty())
        }
    }

    pub(crate) fn returns_value(&self) -> bool {
        matches!(
            self,
            Self::Known0x1(_) | Self::Known1x1(_) | Self::Known2x1(_) | Self::Known3x1(_)
        )
    }

    /// The number of arguments the function takes.
    pub fn num_args(&self) -> usize {
        match self {
            Self::Known0x1(_) => 0,
            Self::Known1x0(_) | Self::Known1x1(_) => 1,
            Self::Known2x0(_) | Self::Known2x1(_) => 2,
            Self::Known3x0(_) | Self::Known3x1(_) => 3,
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
    /// A JIT compiled user function: its tail callable body and its native
    /// entry point.
    User(JitPtr, UserPtr),
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

#[derive(Clone)]
pub(crate) enum ContPtr {
    /// A generated continuation: its tail callable body and its native
    /// entry point.
    JitCont(JitPtr, ContinuationPtr),
    /// A boxed Rust closure continuation.
    RustCont(RustContinuation),
    /// A continuation that exits a prompt. Can be dynamically replaced.
    /// The continuation of a prompt barrier will always be pop_dyn_stack.
    PromptBarrier { barrier_id: usize },
}

impl ContPtr {
    fn is_rust_cont(&self) -> bool {
        matches!(self, ContPtr::RustCont(_))
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
}

#[cfg(feature = "async")]
async fn apply_async_bridge(
    proc: Procedure,
    func: AsyncBridgePtr,
    args: Args,
    barrier: &mut ContBarrier<'_>,
) -> Application {
    let Args([arg1, arg2, arg3, arg4, argn]) = args;
    (func)(proc, arg1, arg2, arg3, arg4, argn, barrier).await
}

fn apply_bridge(
    proc: Procedure,
    func: BridgePtr,
    args: Args,
    barrier: &mut ContBarrier,
) -> Application {
    let Args([arg1, arg2, arg3, arg4, argn]) = args;
    let mut app = std::mem::MaybeUninit::<Application>::uninit();
    (func)(proc, arg1, arg2, arg3, arg4, argn, barrier, &mut app);
    unsafe { app.assume_init() }
}

fn apply_jit(
    proc: Procedure,
    entry: UserPtr,
    args: Args,
    barrier: &mut ContBarrier,
) -> Application {
    let Args([arg1, arg2, arg3, arg4, argn]) = args;
    let mut app = std::mem::MaybeUninit::<Application>::uninit();
    entry(proc, arg1, arg2, arg3, arg4, argn, barrier, &mut app);
    unsafe { app.assume_init() }
}

impl Procedure {
    /// Apply the arguments to the procedure, returning the next application.
    #[maybe_async]
    pub(crate) fn apply(self, args: Args, barrier: &mut ContBarrier<'_>) -> Application {
        match self.0.func {
            FuncPtr::Bridge(sbridge) => apply_bridge(self, sbridge, args, barrier),
            #[cfg(feature = "async")]
            FuncPtr::AsyncBridge(abridge) => apply_async_bridge(self, abridge, args, barrier).await,
            FuncPtr::User(_, entry) => apply_jit(self, entry, args, barrier),
            FuncPtr::Known(known) => known.apply(args, barrier),
        }
    }

    #[cfg(feature = "async")]
    /// Attempt to call the function, and throw an error if is async
    pub(crate) fn apply_sync(self, args: Args, barrier: &mut ContBarrier) -> Application {
        match self.0.func {
            FuncPtr::Bridge(sbridge) => apply_bridge(self, sbridge, args, barrier),
            FuncPtr::AsyncBridge(_) => raise(
                Exception::error("attempt to apply async function in a sync-only context").into(),
                barrier,
            ),
            FuncPtr::User(_, entry) => apply_jit(self, entry, args, barrier),
            FuncPtr::Known(known) => known.apply(args, barrier),
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

    /// Borrow the environment slice of this procedure.
    pub fn env(&self) -> &[Value] {
        &self.0.env
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

    #[allow(private_bounds)]
    pub fn call_with_cont<A, const N: usize>(
        &self,
        args: Args,
        cont_env: [Value; N],
        cont: impl IntoRustContinuation<A, N>,
        barrier: &mut ContBarrier<'_>,
    ) -> Application {
        let (req_args, variadic) = cont.formals();
        barrier.cont_stack.push(
            ContPtr::RustCont(cont.into_rust_cont()),
            cont_env,
            req_args,
            variadic,
        );
        Application::new(self.clone(), args)
    }

    /// Applies `args` to the procedure and returns the values it evaluates to.
    #[maybe_async]
    pub fn call(
        &self,
        args: &[Value],
        barrier: &mut ContBarrier<'_>,
    ) -> Result<Vec<Value>, Exception> {
        maybe_await!(Application::new(self.clone(), Args::from_slice(args)).eval(barrier))
    }

    #[cfg(feature = "async")]
    pub fn call_sync(
        &self,
        args: &[Value],
        barrier: &mut ContBarrier<'_>,
    ) -> Result<Vec<Value>, Exception> {
        Application::new(self.clone(), Args::from_slice(args)).eval_sync(barrier)
    }

    pub(crate) fn to_primop(&self) -> Option<PrimOp> {
        use crate::{
            lists::{append, car, cdr, cons, list},
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
            (list, PrimOp::List),
            (append, PrimOp::Append),
            (cons, PrimOp::Cons),
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

fn halt(_env: [Value; 0], args: Rest, _barrier: &mut ContBarrier) -> Application {
    Application::halt_ok(Args::from_list(args.0))
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

#[repr(C, u8)]
pub(crate) enum OpType {
    JitCont = 0,
    Proc(Procedure),
    HaltOk,
    HaltErr,
}

/// An application of a function to a given set of values.
#[repr(C)]
pub struct Application {
    pub(crate) op: OpType,
    pub(crate) args: Args,
}

impl Application {
    pub fn new(op: Procedure, args: Args) -> Self {
        Self {
            op: OpType::Proc(op),
            args,
        }
    }

    pub fn halt_ok(args: Args) -> Self {
        Self {
            op: OpType::HaltOk,
            args,
        }
    }

    pub fn halt_err(arg: Value) -> Self {
        Self {
            op: OpType::HaltErr,
            args: Args::pack([arg]),
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    #[maybe_async]
    pub fn eval(mut self, barrier: &mut ContBarrier<'_>) -> Result<Vec<Value>, Exception> {
        loop {
            let Application { op, args } = self;
            self = match op {
                OpType::Proc(proc) => maybe_await!(proc.apply(args, barrier)),
                OpType::JitCont => call_jit_cont(args, barrier),
                OpType::HaltOk => return Ok(args.into_vec()),
                OpType::HaltErr => {
                    let Args([err, _, _, _, _]) = args;
                    return Err(Exception(err));
                }
            };
        }
    }

    #[cfg(feature = "async")]
    /// Just like [eval] but throws an error if we encounter an async function.
    pub fn eval_sync(mut self, barrier: &mut ContBarrier) -> Result<Vec<Value>, Exception> {
        loop {
            let Application { op, args } = self;
            self = match op {
                OpType::Proc(proc) => proc.apply_sync(args, barrier),
                OpType::JitCont => call_jit_cont(args, barrier),
                OpType::HaltOk => return Ok(args.into_vec()),
                OpType::HaltErr => {
                    let Args([err, _, _, _, _]) = args;
                    return Err(Exception(err));
                }
            };
        }
    }
}

fn call_jit_cont(args: Args, barrier: &mut ContBarrier<'_>) -> Application {
    let frame = barrier.cont_stack.frames.pop().unwrap();
    let ContPtr::JitCont(_, entry) = frame.func_ptr else {
        unreachable!("proc isn't not a jit continuation");
    };
    let Args([arg1, arg2, arg3, arg4, argn]) = args;
    let mut app = std::mem::MaybeUninit::<Application>::uninit();
    entry(arg1, arg2, arg3, arg4, argn, barrier, &mut app);
    unsafe { app.assume_init() }
}

pub trait IntoApplication {
    fn into_application(self, barrier: &mut ContBarrier) -> Application;
}

impl IntoApplication for Application {
    fn into_application(self, _barrier: &mut ContBarrier) -> Application {
        self
    }
}

impl IntoApplication for () {
    fn into_application(self, barrier: &mut ContBarrier) -> Application {
        barrier.call_cont(Args::pack([]))
    }
}

impl<A, B> IntoApplication for (A, B)
where
    Value: From<A>,
    Value: From<B>,
{
    fn into_application(self, barrier: &mut ContBarrier) -> Application {
        barrier.call_cont(Args::pack([Value::from(self.0), Value::from(self.1)]))
    }
}

impl<A, B, C> IntoApplication for (A, B, C)
where
    Value: From<A>,
    Value: From<B>,
    Value: From<C>,
{
    fn into_application(self, barrier: &mut ContBarrier) -> Application {
        barrier.call_cont(Args::pack([
            Value::from(self.0),
            Value::from(self.1),
            Value::from(self.2),
        ]))
    }
}

impl<T, E> IntoApplication for Result<T, E>
where
    T: IntoApplication,
    Value: From<E>,
{
    fn into_application(self, barrier: &mut ContBarrier) -> Application {
        match self {
            Ok(val) => val.into_application(barrier),
            Err(err) => raise(err.into(), barrier),
        }
    }
}

impl<T> IntoApplication for T
where
    Value: From<T>,
{
    fn into_application(self, barrier: &mut ContBarrier) -> Application {
        barrier.call_cont(Args::pack([Value::from(self)]))
    }
}

pub(crate) trait IntoRustContinuation<A, const N: usize> {
    fn formals(&self) -> (usize, bool);

    fn into_rust_cont(self) -> RustContinuation;
}

/// Type to declare that a Rust continuation is variadic
pub struct Rest(pub Value);

/// A continuation derived from a Rust clossure.
#[derive(Copy, Clone)]
pub(crate) struct RustContinuation(fn(Args, &mut ContBarrier<'_>) -> Application);

const fn assert_non_capturing<F>() {
    assert!(
        size_of::<F>() == 0,
        "a Rust continuation must be convertible to a function pointer"
    );
}

/// Create a function from a type. Has the effect of converting a `impl Fn`
/// into a callable function. This code is taken from the rust stdlib nightly
/// feature `conjure_zst`.
///
/// # Safety
///
/// Generally incredibly unsafe and should only be used in this particular
/// context.
#[allow(clippy::uninit_assumed_init)]
unsafe fn conjure<F>() -> F {
    const { assert_non_capturing::<F>() };
    unsafe { std::mem::MaybeUninit::<F>::uninit().assume_init() }
}

impl<F, R, const N: usize> IntoRustContinuation<(), N> for F
where
    F: Fn([Value; N], &mut ContBarrier) -> R + Send + Sync + 'static,
    R: IntoApplication + 'static,
{
    fn formals(&self) -> (usize, bool) {
        (0, false)
    }

    fn into_rust_cont(self) -> RustContinuation {
        RustContinuation(|args, barrier: &mut ContBarrier<'_>| {
            let env = barrier.pop_env_n::<N>();
            if !args.0[0].is_undefined() {
                return raise(Exception::wrong_num_of_args(0, args.len()).into(), barrier);
            }
            (unsafe { conjure::<F>() })(env, barrier).into_application(barrier)
        })
    }
}

impl<F, R, const N: usize> IntoRustContinuation<(Rest,), N> for F
where
    F: Fn([Value; N], Rest, &mut ContBarrier) -> R + Send + Sync + 'static,
    R: IntoApplication + 'static,
{
    fn formals(&self) -> (usize, bool) {
        (0, true)
    }

    fn into_rust_cont(self) -> RustContinuation {
        RustContinuation(|args, barrier: &mut ContBarrier<'_>| {
            let env = barrier.pop_env_n::<N>();
            (unsafe { conjure::<F>() })(env, Rest(args.into_list()), barrier)
                .into_application(barrier)
        })
    }
}

macro_rules! count {
    () => {
        0usize
    };

    ($head:ident, $( $tail:ident, )*) => {
        1 + count!($($tail,)*)
    };
}

macro_rules! direct_args {
    ($args:ident, $barrier:ident; $( $t:ident )*) => {{
        let expected = count!($( $t, )*);
        let direct = expected.min(MAX_DIRECT_ARGS);
        let required_present = $args.0[..direct].iter().all(|arg| !arg.is_undefined());
        let no_extra = if expected < MAX_DIRECT_ARGS {
            $args.0[expected].is_undefined()
        } else {
            list_len(&$args.0[MAX_DIRECT_ARGS]) == expected - MAX_DIRECT_ARGS
        };
        if !required_present || !no_extra {
            return raise(
                Exception::wrong_num_of_args(expected, $args.len()).into(),
                $barrier,
            );
        }
        direct_args!(@extract $args; $( $t )*)
    }};
    (@extract $args:ident; $t1:ident) => {{
        let Args([arg1, ..]) = $args;
        [arg1]
    }};
    (@extract $args:ident; $t1:ident $t2:ident) => {{
        let Args([arg1, arg2, ..]) = $args;
        [arg1, arg2]
    }};
    (@extract $args:ident; $t1:ident $t2:ident $t3:ident) => {{
        let Args([arg1, arg2, arg3, ..]) = $args;
        [arg1, arg2, arg3]
    }};
    (@extract $args:ident; $t1:ident $t2:ident $t3:ident $t4:ident) => {{
        let Args([arg1, arg2, arg3, arg4, _]) = $args;
        [arg1, arg2, arg3, arg4]
    }};
    (@extract $args:ident; $t1:ident $t2:ident $t3:ident $t4:ident $t5:ident) => {{
        let Args([arg1, arg2, arg3, arg4, argn]) = $args;
        let arg5 = argn.cast::<Pair>().unwrap().car();
        [arg1, arg2, arg3, arg4, arg5]
    }};
}

macro_rules! direct_args_rest {
    ($args:ident, $barrier:ident; $( $t:ident )*) => {{
        let expected = count!($( $t, )*);
        let direct = expected.min(MAX_DIRECT_ARGS);
        let required_present = $args.0[..direct].iter().all(|arg| !arg.is_undefined())
            && (expected <= MAX_DIRECT_ARGS
                || list_len(&$args.0[MAX_DIRECT_ARGS]) >= expected - MAX_DIRECT_ARGS);
        if !required_present {
            return raise(
                Exception::wrong_num_of_args(expected, $args.len()).into(),
                $barrier,
            );
        }
        direct_args_rest!(@extract $args; $( $t )*)
    }};
    (@extract $args:ident; $t1:ident) => {{
        let Args([arg1, arg2, arg3, arg4, argn]) = $args;
        ([arg1], cons_direct_args([arg2, arg3, arg4], argn))
    }};
    (@extract $args:ident; $t1:ident $t2:ident) => {{
        let Args([arg1, arg2, arg3, arg4, argn]) = $args;
        ([arg1, arg2], cons_direct_args([arg3, arg4], argn))
    }};
    (@extract $args:ident; $t1:ident $t2:ident $t3:ident) => {{
        let Args([arg1, arg2, arg3, arg4, argn]) = $args;
        ([arg1, arg2, arg3], cons_direct_args([arg4], argn))
    }};
    (@extract $args:ident; $t1:ident $t2:ident $t3:ident $t4:ident) => {{
        let Args([arg1, arg2, arg3, arg4, argn]) = $args;
        ([arg1, arg2, arg3, arg4], argn)
    }};
    (@extract $args:ident; $t1:ident $t2:ident $t3:ident $t4:ident $t5:ident) => {{
        let Args([arg1, arg2, arg3, arg4, argn]) = $args;
        let pair = argn.cast::<Pair>().unwrap();
        ([arg1, arg2, arg3, arg4, pair.car()], pair.cdr())
    }};
}

macro_rules! impl_rust_cont {
    ( $( $arg:ident ),* ) => {
        impl<F, R, $( $arg, )* const N: usize> IntoRustContinuation<($($arg,)*), N> for F
        where
            F: Fn([Value; N], $( $arg, )* &mut ContBarrier) -> R + Send + Sync + 'static,
            R: IntoApplication + 'static,
        $(
            Value: TryInto<$arg>,
            <Value as TryInto<$arg>>::Error: Into<Value>,
        )*
        {
            fn formals(&self) -> (usize, bool) {
                (count!($( $arg, )*), false)
            }

            fn into_rust_cont(self) -> RustContinuation {
                RustContinuation(|args, barrier: &mut ContBarrier<'_>| {
                    let env = barrier.pop_env_n::<N>();
                    let values = direct_args!(args, barrier; $( $arg )*);
                    let mut values = values.into_iter();
                    (unsafe { conjure::<F>() })(
                        env,
                        $(
                            match <Value as TryInto<$arg>>::try_into(values.next().unwrap()) {
                                Ok(val) => val,
                                Err(err) => return raise(err.into(), barrier),
                            },
                        )*
                        barrier
                    ).into_application(barrier)
                })
            }
        }

        impl<F, R, $( $arg, )* const N: usize> IntoRustContinuation<($($arg,)* Rest), N> for F
        where
            F: Fn([Value; N], $( $arg, )* Rest) -> R + Send + Sync + 'static,
            R: IntoApplication + 'static,
        $(
            Value: TryInto<$arg>,
            <Value as TryInto<$arg>>::Error: Into<Value>,
        )*
        {
            fn formals(&self) -> (usize, bool) {
                (count!($( $arg, )*), true)
            }

            fn into_rust_cont(self) -> RustContinuation {
                RustContinuation(|args, barrier: &mut ContBarrier<'_>| {
                    let env = barrier.pop_env_n::<N>();
                    let (values, rest) = direct_args_rest!(args, barrier; $( $arg )*);
                    let mut values = values.into_iter();
                    (unsafe { conjure::<F>() })(
                        env,
                        $(
                            match <Value as TryInto<$arg>>::try_into(values.next().unwrap()) {
                                Ok(val) => val,
                                Err(err) => return raise(err.into(), barrier),
                            },
                        )*
                        Rest(rest)
                    ).into_application(barrier)
                })
            }
        }
    }
}

impl_rust_cont!(T1);
impl_rust_cont!(T1, T2);
impl_rust_cont!(T1, T2, T3);
impl_rust_cont!(T1, T2, T3, T4);
impl_rust_cont!(T1, T2, T3, T4, T5);

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

#[bridge(name = "apply", lib = "(rnrs base builtins (6))")]
pub fn apply(proc: Procedure, #[rest_args] args: Value) -> Result<Application, Exception> {
    Ok(Application::new(proc, Args::from_list(splice_last(args)?)))
}

/// Flatten the last element of a list with its own elements, i.e.
/// `(a b (c d))` becomes `(a b c d)`.
fn splice_last(args: Value) -> Result<Value, Exception> {
    let Some(pair) = args.cast::<Pair>() else {
        return Err(Exception::wrong_num_of_args(2, 1));
    };
    let cdr = pair.cdr();
    if cdr.is_null() {
        Ok(pair.car())
    } else {
        Ok(Value::from(Pair::immutable(pair.car(), splice_last(cdr)?)))
    }
}

////////////////////////////////////////////////////////////////////////////////
//
// Continuation barriers
//

#[cfg(feature = "async")]
type Param<'a> = &'a mut (dyn Any + Send + Sync);

#[cfg(not(feature = "async"))]
type Param<'a> = &'a mut dyn Any;

/// A continuation barrier. Escape procedures created within a continuation
/// barrier cannot be called within another barrier.
///
/// This structure also contains the dynamic state of the running program
/// including winders, exception handlers, continuation marks, and parameters.
pub struct ContBarrier<'a> {
    /// The id of the barrier. Checked when calling an escape procedure
    id: usize,
    /// The active dynamic state
    state: DynState,
    /// The current live continuations for the program. Effectively the call
    /// stack. Includes active [continuation marks](https://srfi.schemers.org/srfi-157/srfi-157.html).
    pub(crate) cont_stack: ContStack,
    /// The active installed mutable parameters
    params: HashMap<Symbol, Param<'a>>,
}

impl<'a> ContBarrier<'a> {
    pub fn new() -> Self {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

        let mut this = Self {
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            state: DynState::default(),
            cont_stack: ContStack::default(),
            params: HashMap::new(),
        };

        // The call stack always contains a top-level halt continuation:
        this.push_cont([], halt);

        this
    }

    pub fn save(&self) -> SavedDynamicState {
        SavedDynamicState {
            id: self.id,
            state: self.state.clone(),
            cont_stack: self.cont_stack.clone(),
        }
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

    // TODO: We should certainly try to optimize these functions. Linear
    // searching isn't _great_, although in practice I can't imagine this stack
    // will ever get very large.

    pub fn current_exception_handler(&self) -> Option<Procedure> {
        self.state
            .dyn_stack
            .iter()
            .rev()
            .find_map(|elem| match elem {
                DynStackElem::ExceptionHandler(proc) => Some(proc.clone()),
                _ => None,
            })
    }

    pub fn current_input_port(&self) -> Port {
        self.state
            .dyn_stack
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
        self.state
            .dyn_stack
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
        self.state.dyn_stack.push(elem);
    }

    pub(crate) fn pop_dyn_stack(&mut self) -> Option<DynStackElem> {
        self.state.dyn_stack.pop()
    }

    pub(crate) fn dyn_stack_last(&self) -> Option<&DynStackElem> {
        self.state.dyn_stack.last()
    }

    pub(crate) fn dyn_stack_len(&self) -> usize {
        self.state.dyn_stack.len()
    }

    pub(crate) fn dyn_stack_is_empty(&self) -> bool {
        self.state.dyn_stack.is_empty()
    }

    /// Push a Rust continuation onto the current call stack.
    #[allow(private_bounds)]
    pub fn push_cont<A, const N: usize>(
        &mut self,
        env: [Value; N],
        cont: impl IntoRustContinuation<A, N>,
    ) {
        let (num_required_args, variadic) = cont.formals();
        self.cont_stack.push(
            ContPtr::RustCont(cont.into_rust_cont()),
            env,
            num_required_args,
            variadic,
        );
    }

    pub fn call_cont(&mut self, args: Args) -> Application {
        loop {
            let curr_frame = self.cont_stack.frames.pop().unwrap();
            match curr_frame.func_ptr {
                ContPtr::JitCont(..) => {
                    // Return the JIT continuation to the trampoline. When
                    // `become` is stabalized this will no longer be necessary.
                    self.cont_stack.frames.push(curr_frame);
                    return Application {
                        op: OpType::JitCont,
                        args,
                    };
                }
                ContPtr::RustCont(rust_cont) => {
                    return (rust_cont.0)(args, self);
                }
                ContPtr::PromptBarrier { .. } => {
                    self.pop_dyn_stack();
                }
            }
        }
    }

    pub(crate) fn pop_env_n<const N: usize>(&mut self) -> [Value; N] {
        let mut env = std::array::from_fn(|_| self.pop_env());
        env.reverse();
        env
    }

    pub(crate) fn pop_env(&mut self) -> Value {
        self.cont_stack.envs.pop().unwrap()
    }

    pub(crate) fn pop_jit_cont(&mut self) -> Option<NonNull<u8>> {
        loop {
            if self
                .cont_stack
                .frames
                .last()
                .is_some_and(|frame| frame.func_ptr.is_rust_cont())
            {
                return None;
            } else {
                match self.cont_stack.frames.pop().unwrap().func_ptr {
                    ContPtr::JitCont(func, _) => return NonNull::new(func.0 as *mut u8),
                    ContPtr::PromptBarrier { .. } => {
                        self.pop_dyn_stack();
                    }
                    _ => unreachable!(),
                }
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

/// A copy of [`ContBarrier`] without mutable parameters
#[derive(Clone, Trace)]
pub struct SavedDynamicState {
    id: usize,
    state: DynState,
    cont_stack: ContStack,
}

impl SavedDynamicState {
    pub(crate) fn dyn_stack_get(&self, idx: usize) -> Option<&DynStackElem> {
        self.state.dyn_stack.get(idx)
    }

    pub(crate) fn dyn_stack_len(&self) -> usize {
        self.state.dyn_stack.len()
    }
}

impl From<SavedDynamicState> for ContBarrier<'_> {
    fn from(value: SavedDynamicState) -> Self {
        ContBarrier {
            state: value.state,
            cont_stack: value.cont_stack,
            ..Default::default()
        }
    }
}

#[derive(Clone, Default, Trace)]
pub(crate) struct DynState {
    dyn_stack: Vec<DynStackElem>,
}

impl DynState {
    /// Ports cross a spawn boundary; winders, handlers, and prompts do not.
    #[allow(dead_code)]
    fn spawn_snapshot(&self) -> DynState {
        DynState {
            dyn_stack: self
                .dyn_stack
                .iter()
                .filter(|elem| elem.crosses_spawn())
                .cloned()
                .collect(),
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

impl DynStackElem {
    #[allow(dead_code)]
    fn crosses_spawn(&self) -> bool {
        matches!(
            self,
            DynStackElem::CurrentInputPort(_) | DynStackElem::CurrentOutputPort(_)
        )
    }
}

pub(crate) fn pop_dyn_stack(
    _env: [Value; 0],
    args: Rest,
    barrier: &mut ContBarrier,
) -> Application {
    barrier.pop_dyn_stack();
    barrier.call_cont(Args::from_list(args.0))
}

#[derive(Default, Clone, Trace)]
pub(crate) struct ContStack {
    frames: Vec<ContFrame>,
    envs: Vec<Value>,
}

impl ContStack {
    pub(crate) fn push(
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

#[cfg(feature = "continuation-marks")]
#[bridge(name = "print-trace", lib = "(scheme-rs tracing (6))")]
pub fn print_trace(barrier: &mut ContBarrier) {
    println!(
        "trace: {:#?}",
        barrier.current_marks(Symbol::intern("trace"))
    );
}

////////////////////////////////////////////////////////////////////////////////
//
// Call with current continuation
//

#[bridge(
    name = "call-with-current-continuation",
    lib = "(rnrs base builtins (6))"
)]
pub fn call_with_current_continuation(proc: Procedure, barrier: &mut ContBarrier) -> Application {
    let (req_args, variaidic) = barrier.cont_formals();

    let escape_proc = Procedure::new(
        vec![Value::from(barrier.save())],
        FuncPtr::Bridge(escape_proc),
        req_args,
        variaidic,
    );

    Application::new(proc, Args::pack([Value::from(escape_proc)]))
}

/// Prepare the continuation for call/cc. Clones the continuation environment
/// and creates a closure that calls the appropriate winders.
#[bridge]
fn escape_proc(
    #[env] saved_barrier: Embedded<SavedDynamicState>,
    #[rest_args] args: Value,
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    if saved_barrier.id != barrier.id {
        return Err(Exception::error("attempt to cross continuation barrier"));
    }

    barrier.cont_stack = saved_barrier.cont_stack.clone();
    barrier.push_cont([args, Value::from(saved_barrier)], unwind);

    Ok(barrier.call_cont(Args::pack([])))
}

fn unwind(env: [Value; 2], _args: Rest, barrier: &mut ContBarrier) -> Application {
    let [args, dest_stack_val] = env;
    let dest_stack = dest_stack_val
        .clone()
        .try_to::<Embedded<SavedDynamicState>>()
        .unwrap();
    let dest_stack_read = dest_stack.as_ref();

    while !barrier.dyn_stack_is_empty()
        && (barrier.dyn_stack_len() > dest_stack_read.dyn_stack_len()
            || barrier.dyn_stack_last()
                != dest_stack_read.dyn_stack_get(barrier.dyn_stack_len() - 1))
    {
        match barrier.pop_dyn_stack() {
            None => {
                break;
            }
            Some(DynStackElem::Winder(winder)) => {
                // Call the out winder while unwinding
                barrier.push_cont([args, dest_stack_val], unwind);
                return Application::new(winder.out_thunk, Args::pack([]));
            }
            _ => (),
        };
    }

    // Begin winding
    barrier.push_cont([args, dest_stack_val, Value::from(false)], wind);
    barrier.call_cont(Args::pack([]))
}

fn wind(env: [Value; 3], _args: Rest, barrier: &mut ContBarrier) -> Application {
    let [args, dest_stack_val, winder] = env;
    let dest_stack = dest_stack_val
        .clone()
        .try_to::<Embedded<SavedDynamicState>>()
        .unwrap();
    let dest_stack_read = dest_stack.as_ref();

    if winder.is_true() {
        let winder = winder.try_to::<Embedded<Winder>>().unwrap();
        barrier.push_dyn_stack(DynStackElem::Winder(winder.as_ref().clone()));
    }

    while barrier.dyn_stack_len() < dest_stack_read.dyn_stack_len() {
        match dest_stack_read
            .dyn_stack_get(barrier.dyn_stack_len())
            .cloned()
        {
            None => {
                break;
            }
            Some(DynStackElem::Winder(winder)) => {
                // Call the in winder while winding
                let in_thunk = winder.in_thunk.clone();
                barrier.push_cont([args, dest_stack_val, Value::from(winder)], wind);
                return Application::new(in_thunk, Args::pack([]));
            }
            Some(elem) => barrier.push_dyn_stack(elem),
        }
    }

    barrier.call_cont(Args::from_list(args))
}

#[bridge(name = "call-with-values", lib = "(rnrs base builtins (6))")]
pub fn call_with_values(
    producer: Procedure,
    consumer: Procedure,
    barrier: &mut ContBarrier,
) -> Application {
    producer.call_with_cont(
        Args::pack([]),
        [Value::from(consumer)],
        |[consumer]: [Value; 1], args: Rest, _: &mut ContBarrier| {
            Application::new(
                consumer.cast::<Procedure>().unwrap(),
                Args::from_list(args.0),
            )
        },
        barrier,
    )
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

#[bridge(name = "dynamic-wind", lib = "(rnrs base builtins (6))")]
pub fn dynamic_wind(
    in_thunk: Procedure,
    body_thunk: Procedure,
    out_thunk: Procedure,
    barrier: &mut ContBarrier,
) -> Application {
    // Call the in thunk:
    in_thunk.call_with_cont(
        Args::pack([]),
        [
            Value::from(in_thunk.clone()),
            Value::from(body_thunk),
            Value::from(out_thunk),
        ],
        |[in_thunk, body_thunk, out_thunk]: [Value; 3], _: Rest, barrier: &mut ContBarrier| {
            barrier.push_dyn_stack(DynStackElem::Winder(Winder {
                in_thunk: in_thunk.cast().unwrap(),
                out_thunk: out_thunk.cast().unwrap(),
            }));
            // Call the body thunk:
            body_thunk.cast::<Procedure>().unwrap().call_with_cont(
                Args::pack([]),
                [out_thunk.clone()],
                |[out_thunk]: [Value; 1], args: Rest, barrier: &mut ContBarrier| {
                    // Pop the dyn stack:
                    barrier.pop_dyn_stack();
                    // Save the arguments and call the out thunk:
                    out_thunk.cast::<Procedure>().unwrap().call_with_cont(
                        Args::pack([]),
                        [args.0],
                        |[body_thunk_res]: [Value; 1], _: Rest, barrier: &mut ContBarrier| {
                            barrier.call_cont(Args::from_list(body_thunk_res.clone()))
                        },
                        barrier,
                    )
                },
                barrier,
            )
        },
        barrier,
    )
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

#[bridge(name = "call-with-prompt", lib = "(prompts)")]
pub fn call_with_prompt(
    tag: Symbol,
    thunk: Procedure,
    handler: Procedure,
    barrier: &mut ContBarrier,
) -> Application {
    static BARRIER_ID: AtomicUsize = AtomicUsize::new(0);

    let barrier_id = BARRIER_ID.fetch_add(1, Ordering::Relaxed);

    let (req_args, variadic) = barrier.cont_formals();

    barrier.push_dyn_stack(DynStackElem::Prompt(Prompt {
        tag,
        handler,
        barrier_id,
    }));

    barrier.cont_stack.push(
        ContPtr::PromptBarrier { barrier_id },
        Vec::new(),
        req_args,
        variadic,
    );

    Application::new(thunk, Args::pack([]))
}

#[bridge(name = "abort-to-prompt", lib = "(prompts)")]
pub fn abort_to_prompt(
    tag: Symbol,
    #[rest_args] rest_args: Value,
    barrier: &mut ContBarrier,
) -> Application {
    let saved = Value::from(barrier.save());
    barrier.push_cont([rest_args, Value::from(tag), saved], unwind_to_prompt);
    barrier.call_cont(Args::pack([]))
}

fn unwind_to_prompt(env: [Value; 3], _args: Rest, barrier: &mut ContBarrier) -> Application {
    let [args, tag_val, saved_barrier] = env;
    let tag: Symbol = tag_val.clone().try_into().unwrap();

    loop {
        return match barrier.pop_dyn_stack() {
            None => Application::halt_err(Value::from(Exception::error(format!(
                "no prompt tag {tag} found"
            )))),
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
                let mut delimited_frames = barrier.cont_stack.frames[barrier_idx + 1..].to_vec();
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
                    state: DynState {
                        dyn_stack: saved_barrier.as_ref().state.dyn_stack
                            [barrier.dyn_stack_len() + 1..]
                            .to_vec(),
                    },
                    cont_stack: delimited_cont,
                };

                let delimited_cont_proc = Value::from(Procedure::new(
                    vec![Value::from(prompt_delimited_barrier)],
                    FuncPtr::Bridge(delimited_continuation),
                    0,
                    true,
                ));
                let handler_args = Value::from(Pair::immutable(delimited_cont_proc, args));
                Application::new(handler, Args::from_list(handler_args))
            }
            Some(DynStackElem::Winder(winder)) => {
                barrier.push_cont([args, tag_val, saved_barrier], unwind_to_prompt);
                Application::new(winder.out_thunk, Args::pack([]))
            }
            _ => continue,
        };
    }
}

#[bridge]
fn delimited_continuation(
    #[env] saved_barrier: Embedded<SavedDynamicState>,
    #[rest_args] rest_args: Value,
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
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
    barrier.push_cont(
        [
            rest_args,
            Value::from(saved_barrier),
            Value::from(0),
            Value::from(false),
        ],
        wind_delim,
    );
    Ok(barrier.call_cont(Args::pack([])))
}

fn wind_delim(env: [Value; 4], _args: Rest, barrier: &mut ContBarrier) -> Application {
    let [args, dest_stack_val, idx, winder] = env;
    let dest_stack = dest_stack_val
        .clone()
        .try_to::<Embedded<SavedDynamicState>>()
        .unwrap();
    let mut idx: usize = idx.cast().unwrap();

    if winder.is_true() {
        let winder = winder.try_to::<Embedded<Winder>>().unwrap();
        barrier.push_dyn_stack(DynStackElem::Winder(winder.as_ref().clone()));
    }

    while let Some(elem) = dest_stack.as_ref().dyn_stack_get(idx) {
        idx += 1;

        if let DynStackElem::Winder(winder) = elem {
            barrier.push_cont(
                [
                    args,
                    dest_stack_val,
                    Value::from(idx),
                    Value::from(winder.clone()),
                ],
                wind_delim,
            );
            return Application::new(winder.in_thunk.clone(), Args::pack([]));
        }
        barrier.push_dyn_stack(elem.clone());
    }

    barrier.call_cont(Args::from_list(args))
}
