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
//! # registry::cps_bridge, value::Value, runtime::Runtime, exceptions::Exception};
//! #[cps_bridge]
//! fn closure(
//!     _runtime: &Runtime,
//!     env: &[Value],
//!     k: Procedure,
//!     _args: &[Value],
//!     _rest_args: &[Value],
//!     _barrier: &mut ContBarrier,
//! ) -> Result<Application, Exception> {
//!     Ok(Application::new(k, None, vec![ env[0].clone() ]))
//! }
//!
//! # fn main() {
//! # let runtime = Runtime::new();
//! let closure = Procedure::new(
//!     runtime,
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
//! #     registry::cps_bridge, value::{Value, Cell}, runtime::Runtime,
//! #     exceptions::Exception,
//! #     num::Number,
//! # };
//! #[cps_bridge]
//! fn next_num(
//!     _runtime: &Runtime,
//!     env: &[Value],
//!     k: Procedure,
//!     _args: &[Value],
//!     _rest_args: &[Value],
//!     _barrier: &mut ContBarrier,
//! ) -> Result<Application, Exception> {
//!     // Fetch the cell from the environment:
//!     let cell: Cell = env[0].try_to_scheme_type()?;
//!     let curr: Number = cell.get().try_into()?;
//!
//!     // Increment the cell
//!     cell.set(Value::from(curr.clone() + Number::from(1)));
//!
//!     // Return the previous value:
//!     Ok(Application::new(k, None, vec![ Value::from(curr) ]))
//! }
//!
//! # fn main() {
//! # let runtime = Runtime::new();
//! let next_num = Procedure::new(
//!     runtime,
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
    gc::{Gc, GcInner, Trace},
    lists::{self, Pair, list_to_vec},
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
use scheme_rs_macros::{cps_bridge, maybe_async, maybe_await, maybe_await_boxed};
use std::{
    any::Any,
    collections::HashMap,
    fmt,
    ops::DerefMut,
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
    barrier: *mut ContBarrier<'_>,
) -> *mut Application;

/// A function pointer to a generated user function.
pub(crate) type UserPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    barrier: *mut ContBarrier<'_>,
    k: *mut GcInner<ProcedureInner>,
) -> *mut Application;

/// A function pointer to a sync Rust bridge function.
pub type BridgePtr = fn(
    runtime: &Runtime,
    env: &[Value],
    k: Procedure,
    args: &[Value],
    rest_args: &[Value],
    barrier: &mut ContBarrier<'_>,
) -> Application;

/// A function pointer to an async Rust bridge function.
#[cfg(feature = "async")]
pub type AsyncBridgePtr = for<'a> fn(
    runtime: &'a Runtime,
    env: &'a [Value],
    k: Procedure,
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

    #[maybe_async]
    fn apply(
        self,
        runtime: &Runtime,
        k: Procedure,
        args: &[Value],
        barrier: &mut ContBarrier<'_>,
    ) -> Application {
        match self.call(args) {
            Ok(result) => maybe_await!(k.0.apply(None, result, barrier)),
            Err(err) => raise(runtime.clone(), err.into(), barrier),
        }
    }

    #[cfg(feature = "async")]
    fn apply_sync(
        self,
        runtime: &Runtime,
        k: Procedure,
        args: &[Value],
        barrier: &mut ContBarrier<'_>,
    ) -> Application {
        match self.call(args) {
            Ok(result) => k.0.apply_sync(None, result, barrier),
            Err(err) => raise(runtime.clone(), err.into(), barrier),
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
    /// A JIT compiled (or occasionally defined in Rust) continuation
    Continuation(ContinuationPtr),
    /// A continuation that exits a prompt. Can be dynamically replaced
    PromptBarrier {
        barrier_id: usize,
        k: ContinuationPtr,
    },
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

    pub(crate) fn check_args(
        &self,
        k: &Option<Procedure>,
        args: &[Value],
        barrier: &mut ContBarrier,
    ) -> Result<(), Application> {
        // Error if this is not a continuation and this function is not passed
        // a continuation
        if self.is_continuation() == k.is_some() {
            return Err(raise(
                self.runtime.clone(),
                Exception::no_cont().into(),
                barrier,
            ));
        }

        // Error if the number of arguments provided is incorrect
        if args.len() < self.num_required_args {
            return Err(raise(
                self.runtime.clone(),
                Exception::wrong_num_of_args(self.num_required_args, args.len()).into(),
                barrier,
            ));
        }

        if !self.variadic && args.len() > self.num_required_args {
            return Err(raise(
                self.runtime.clone(),
                Exception::wrong_num_of_args(self.num_required_args, args.len()).into(),
                barrier,
            ));
        }

        Ok(())
    }

    #[cfg(feature = "async")]
    async fn apply_async_bridge(
        &self,
        func: AsyncBridgePtr,
        k: Procedure,
        args: &[Value],
        barrier: &mut ContBarrier<'_>,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(&self.runtime, &self.env, k, args, rest_args, barrier).await
    }

    fn apply_sync_bridge(
        &self,
        func: BridgePtr,
        k: Procedure,
        args: &[Value],
        barrier: &mut ContBarrier,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(&self.runtime, &self.env, k, args, rest_args, barrier)
    }

    fn apply_jit(
        &self,
        func: JitFuncPtr,
        k: Option<Procedure>,
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

        let app = match func {
            JitFuncPtr::Continuation(sync_fn) => unsafe {
                (sync_fn)(
                    Gc::as_ptr(&self.runtime.0),
                    self.env.as_ptr(),
                    args.as_ptr(),
                    barrier as *mut ContBarrier<'_>,
                )
            },
            JitFuncPtr::User(sync_fn) => unsafe {
                (sync_fn)(
                    Gc::as_ptr(&self.runtime.0),
                    self.env.as_ptr(),
                    args.as_ptr(),
                    barrier as *mut ContBarrier<'_>,
                    Gc::as_ptr(&k.as_ref().unwrap().0),
                )
            },
        };

        unsafe { *Box::from_raw(app) }
    }

    /// Apply the arguments to the function, returning the next application.
    #[maybe_async]
    pub fn apply(
        &self,
        k: Option<Procedure>,
        args: Vec<Value>,
        barrier: &mut ContBarrier<'_>,
    ) -> Application {
        if let FuncPtr::PromptBarrier { barrier_id: id, .. } = self.func {
            barrier.pop_marks();
            match barrier.pop_dyn_stack() {
                Some(DynStackElem::PromptBarrier(PromptBarrier {
                    barrier_id,
                    replaced_k,
                })) if barrier_id == id => {
                    return if let Err(raised) =
                        replaced_k.0.check_args(&k, args.as_slice(), barrier)
                    {
                        raised
                    } else {
                        Application::new(replaced_k, None, args)
                    };
                }
                Some(other) => barrier.push_dyn_stack(other),
                _ => (),
            }
        }

        if let Err(raised) = self.check_args(&k, &args, barrier) {
            return raised;
        }

        match self.func {
            FuncPtr::Bridge(sbridge) => self.apply_sync_bridge(sbridge, k.unwrap(), &args, barrier),
            #[cfg(feature = "async")]
            FuncPtr::AsyncBridge(abridge) => {
                self.apply_async_bridge(abridge, k.unwrap(), &args, barrier)
                    .await
            }
            FuncPtr::User(user) => self.apply_jit(JitFuncPtr::User(user), k, args, barrier),
            FuncPtr::Continuation(k) => {
                barrier.pop_marks();
                self.apply_jit(JitFuncPtr::Continuation(k), None, args, barrier)
            }
            FuncPtr::PromptBarrier { k, .. } => {
                self.apply_jit(JitFuncPtr::Continuation(k), None, args, barrier)
            }
            FuncPtr::Known(known) => {
                maybe_await_boxed!(known.apply(&self.runtime, k.unwrap(), &args, barrier))
            }
        }
    }

    #[cfg(feature = "async")]
    /// Attempt to call the function, and throw an error if is async
    pub fn apply_sync(
        &self,
        k: Option<Procedure>,
        args: Vec<Value>,
        barrier: &mut ContBarrier,
    ) -> Application {
        if let FuncPtr::PromptBarrier { barrier_id: id, .. } = self.func {
            barrier.pop_marks();
            match barrier.pop_dyn_stack() {
                Some(DynStackElem::PromptBarrier(PromptBarrier {
                    barrier_id,
                    replaced_k,
                })) if barrier_id == id => {
                    return if let Err(raised) =
                        replaced_k.0.check_args(&k, args.as_slice(), barrier)
                    {
                        raised
                    } else {
                        Application::new(replaced_k, None, args)
                    };
                }
                Some(other) => barrier.push_dyn_stack(other),
                _ => (),
            }
        }

        if let Err(raised) = self.check_args(&k, &args, barrier) {
            return raised;
        }

        match self.func {
            FuncPtr::Bridge(sbridge) => self.apply_sync_bridge(sbridge, k.unwrap(), &args, barrier),
            FuncPtr::AsyncBridge(_) => raise(
                self.runtime.clone(),
                Exception::error("attempt to apply async function in a sync-only context").into(),
                barrier,
            ),
            FuncPtr::User(user) => self.apply_jit(JitFuncPtr::User(user), k, args, barrier),
            FuncPtr::Continuation(k) => {
                barrier.pop_marks();
                self.apply_jit(JitFuncPtr::Continuation(k), None, args, barrier)
            }
            FuncPtr::PromptBarrier { k, .. } => {
                self.apply_jit(JitFuncPtr::Continuation(k), None, args, barrier)
            }
            FuncPtr::Known(known) => known.apply_sync(&self.runtime, k.unwrap(), &args, barrier),
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
#[repr(transparent)]
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

    pub(crate) fn new_cont(
        runtime: Runtime,
        env: Vec<Value>,
        k: ContinuationPtr,
        num_required_args: usize,
        variadic: bool,
        cont_barrier: &mut ContBarrier,
    ) -> Self {
        cont_barrier.push_marks();
        Procedure::with_debug_info(
            runtime,
            env,
            FuncPtr::Continuation(k),
            num_required_args,
            variadic,
            None,
        )
    }

    pub(crate) fn with_debug_info(
        runtime: Runtime,
        env: Vec<Value>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<ProcDebugInfo>>,
    ) -> Self {
        Self(Gc::new(ProcedureInner::new(
            runtime,
            env,
            func,
            num_required_args,
            variadic,
            debug_info,
        )))
    }

    /// Get the runtime associated with the procedure
    pub fn get_runtime(&self) -> Runtime {
        self.0.runtime.clone()
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

    /// Return whether or not the procedure is a continuation
    pub fn is_continuation(&self) -> bool {
        self.0.is_continuation()
    }

    /// Applies `args` to the procedure and returns the values it evaluates to.
    #[maybe_async]
    pub fn call(
        &self,
        args: &[Value],
        barrier: &mut ContBarrier<'_>,
    ) -> Result<Vec<Value>, Exception> {
        let k = (!self.is_continuation()).then(|| halt_continuation(self.get_runtime()));
        maybe_await!(Application::new(self.clone(), k, args.to_vec()).eval(barrier))
    }

    #[cfg(feature = "async")]
    pub fn call_sync(
        &self,
        args: &[Value],
        barrier: &mut ContBarrier<'_>,
    ) -> Result<Vec<Value>, Exception> {
        let k = (!self.is_continuation()).then(|| halt_continuation(self.get_runtime()));
        Application::new(self.clone(), k, args.to_vec()).eval_sync(barrier)
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

static HALT_CONTINUATION: OnceLock<Procedure> = OnceLock::new();

/// Return a continuation that returns its expressions to the Rust program.
pub fn halt_continuation(runtime: Runtime) -> Procedure {
    unsafe extern "C" fn halt(
        _runtime: *mut GcInner<RwLock<RuntimeInner>>,
        _env: *const Value,
        args: *const Value,
        _barrier: *mut ContBarrier,
    ) -> *mut Application {
        unsafe { crate::runtime::halt(Value::into_raw(args.read())) }
    }

    HALT_CONTINUATION
        .get_or_init(move || {
            Procedure(Gc::new(ProcedureInner::new(
                runtime,
                Vec::new(),
                FuncPtr::Continuation(halt),
                0,
                true,
                None,
            )))
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
    /// The continuation being applied, if it exists.
    k: Option<Procedure>,
    /// The arguments being applied to the operator.
    // TODO: Maybe make this a Cow<[Value]>?
    args: Vec<Value>,
}

impl Application {
    pub fn new(op: Procedure, k: Option<Procedure>, args: Vec<Value>) -> Self {
        Self {
            op: OpType::Proc(op),
            k,
            args,
        }
    }

    pub fn halt_ok(args: Vec<Value>) -> Self {
        Self {
            op: OpType::HaltOk,
            k: None,
            args,
        }
    }

    pub fn halt_err(arg: Value) -> Self {
        Self {
            op: OpType::HaltErr,
            k: None,
            args: vec![arg],
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    #[maybe_async]
    pub fn eval(mut self, barrier: &mut ContBarrier<'_>) -> Result<Vec<Value>, Exception> {
        loop {
            let op = match self.op {
                OpType::Proc(proc) => proc,
                OpType::HaltOk => return Ok(self.args),
                OpType::HaltErr => {
                    return Err(Exception(self.args.pop().unwrap()));
                }
            };
            self = maybe_await!(op.0.apply(self.k, self.args, barrier));
        }
    }

    #[cfg(feature = "async")]
    /// Just like [eval] but throws an error if we encounter an async function.
    pub fn eval_sync(mut self, barrier: &mut ContBarrier) -> Result<Vec<Value>, Exception> {
        loop {
            let op = match self.op {
                OpType::Proc(proc) => proc,
                OpType::HaltOk => return Ok(self.args),
                OpType::HaltErr => {
                    return Err(Exception(self.args.pop().unwrap()));
                }
            };
            self = op.0.apply_sync(self.k, self.args, barrier);
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
                file: std::sync::Arc::from(debug_info.file.to_string()),
            },
        }
    }
}

#[cps_bridge(def = "apply arg1 . args", lib = "(rnrs base builtins (6))")]
pub fn apply(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
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
    Ok(Application::new(op.clone(), Some(k), args))
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
    /// The active dynamic stack
    dyn_stack: Vec<DynStackElem>,
    /// The active [continuation marks](https://srfi.schemers.org/srfi-157/srfi-157.html)
    cont_marks: Vec<HashMap<Symbol, Value>>,
    /// The active installed mutable parameters
    params: HashMap<Symbol, Param<'a>>,
}

impl<'a> ContBarrier<'a> {
    pub fn new() -> Self {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

        Self {
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            dyn_stack: Vec::new(),
            // Procedures returned by the JIT compiler are delimited
            // continuations (of sorts), and therefore we need to preallocate
            // the initial marks for them since there's no mechanism to allocate
            // for them when they're run.
            cont_marks: vec![HashMap::new()],
            params: HashMap::new(),
        }
    }

    pub fn save(&self) -> SavedDynamicState {
        SavedDynamicState {
            id: self.id,
            dyn_stack: self.dyn_stack.clone(),
            cont_marks: self.cont_marks.clone(),
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

/// A copy of [`DynamicState`] without mutable parameters
#[derive(Clone, Debug, Trace)]
pub struct SavedDynamicState {
    id: usize,
    dyn_stack: Vec<DynStackElem>,
    cont_marks: Vec<HashMap<Symbol, Value>>,
}

impl SavedDynamicState {
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

impl From<SavedDynamicState> for ContBarrier<'_> {
    fn from(value: SavedDynamicState) -> Self {
        ContBarrier {
            dyn_stack: value.dyn_stack,
            cont_marks: value.cont_marks,
            ..Default::default()
        }
    }
}

impl SchemeCompatible for SavedDynamicState {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "$dynamic-state", sealed: true, opaque: true)
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
    barrier: *mut ContBarrier,
) -> *mut Application {
    unsafe {
        // env[0] is the continuation
        let k: Procedure = env.as_ref().unwrap().clone().try_into().unwrap();

        barrier.as_mut().unwrap_unchecked().pop_dyn_stack();

        let args = k.collect_args(args);
        let app = Application::new(k, None, args);

        Box::into_raw(Box::new(app))
    }
}

#[cps_bridge(def = "print-trace", lib = "(rnrs base builtins (6))")]
pub fn print_trace(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    _args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    println!(
        "trace: {:#?}",
        barrier.current_marks(Symbol::intern("trace"))
    );
    Ok(Application::new(k, None, Vec::new()))
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
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [proc] = args else { unreachable!() };
    let proc: Procedure = proc.clone().try_into()?;

    let (req_args, variadic) = k.get_formals();

    let barrier = Value::from_rust_type(barrier.save());

    let escape_procedure = Procedure::new(
        runtime.clone(),
        vec![Value::from(k.clone()), barrier],
        FuncPtr::Bridge(escape_procedure),
        req_args,
        variadic,
    );

    let app = Application::new(proc, Some(k), vec![Value::from(escape_procedure)]);

    Ok(app)
}

/// Prepare the continuation for call/cc. Clones the continuation environment
/// and creates a closure that calls the appropriate winders.
#[cps_bridge]
fn escape_procedure(
    runtime: &Runtime,
    env: &[Value],
    _k: Procedure,
    args: &[Value],
    rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    // env[0] is the continuation
    let k = env[0].clone();

    // env[1] is the dyn stack of the continuation
    let saved_barrier_val = env[1].clone();
    let saved_barrier = saved_barrier_val
        .try_to_rust_type::<SavedDynamicState>()
        .unwrap();
    let saved_barrier_read = saved_barrier.as_ref();

    if saved_barrier_read.id != barrier.id {
        return Err(Exception::error("attempt to cross continuation barrier"));
    }

    barrier.cont_marks = saved_barrier_read.cont_marks.clone();

    // Clone the continuation
    let k: Procedure = k.try_into().unwrap();

    let args = args.iter().chain(rest_args).cloned().collect::<Vec<_>>();

    // Simple optimization: check if we're in the same dyn stack
    if barrier.dyn_stack_len() == saved_barrier_read.dyn_stack_len()
        && barrier.dyn_stack_last() == saved_barrier_read.dyn_stack_last()
    {
        Ok(Application::new(k, None, args))
    } else {
        let args = Value::from(args);
        let k = Procedure::new_cont(
            runtime.clone(),
            vec![Value::from(k), args, saved_barrier_val],
            unwind,
            0,
            false,
            barrier,
        );
        Ok(Application::new(k, None, Vec::new()))
    }
}

unsafe extern "C" fn unwind(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
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
            .try_to_rust_type::<SavedDynamicState>()
            .unwrap();
        let dest_stack_read = dest_stack.as_ref();

        let barrier = barrier.as_mut().unwrap_unchecked();

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
                    let app = Application::new(
                        winder.out_thunk,
                        Some(Procedure::new_cont(
                            Runtime::from_raw_inc_rc(runtime),
                            vec![k, args, dest_stack_val],
                            unwind,
                            0,
                            false,
                            barrier,
                        )),
                        Vec::new(),
                    );
                    return Box::into_raw(Box::new(app));
                }
                _ => (),
            };
        }

        // Begin winding
        let app = Application::new(
            Procedure::new_cont(
                Runtime::from_raw_inc_rc(runtime),
                vec![k, args, dest_stack_val, Value::from(false)],
                wind,
                0,
                false,
                barrier,
            ),
            None,
            Vec::new(),
        );

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn wind(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
) -> *mut Application {
    unsafe {
        // env[0] is the ultimate continuation
        let k = env.as_ref().unwrap().clone();

        // env[1] are the arguments to pass to k
        let args = env.add(1).as_ref().unwrap().clone();

        // env[2] is the stack we are trying to reach
        let dest_stack_val = env.add(2).as_ref().unwrap().clone();
        let dest_stack = dest_stack_val
            .try_to_rust_type::<SavedDynamicState>()
            .unwrap();
        let dest_stack_read = dest_stack.as_ref();

        let barrier = barrier.as_mut().unwrap_unchecked();

        // env[3] is potentially a winder that we should push onto the dyn stack
        let winder = env.add(3).as_ref().unwrap().clone();
        if winder.is_true() {
            let winder = winder.try_to_rust_type::<Winder>().unwrap();
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
                    let app = Application::new(
                        winder.in_thunk.clone(),
                        Some(Procedure::new_cont(
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
                            barrier,
                        )),
                        Vec::new(),
                    );
                    return Box::into_raw(Box::new(app));
                }
                Some(elem) => barrier.push_dyn_stack(elem),
            }
        }

        let args: Vector = args.try_into().unwrap();
        let args = args.0.vec.read().to_vec();

        Box::into_raw(Box::new(Application::new(
            k.try_into().unwrap(),
            None,
            args,
        )))
    }
}

unsafe extern "C" fn call_consumer_with_values(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    barrier: *mut ContBarrier,
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
                    barrier.as_mut().unwrap_unchecked(),
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

        Box::into_raw(Box::new(Application::new(
            consumer.clone(),
            k.cast_to_scheme_type(),
            collected_args,
        )))
    }
}

#[cps_bridge(
    def = "call-with-values producer consumer",
    lib = "(rnrs base builtins (6))"
)]
pub fn call_with_values(
    runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
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

    let call_consumer_closure = Procedure::new_cont(
        runtime.clone(),
        vec![Value::from(consumer), Value::from(k)],
        call_consumer_with_values,
        num_required_args,
        variadic,
        barrier,
    );

    Ok(Application::new(
        producer,
        Some(call_consumer_closure),
        Vec::new(),
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
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [in_thunk_val, body_thunk_val, out_thunk_val] = args else {
        return Err(Exception::wrong_num_of_args(3, args.len()));
    };

    let in_thunk: Procedure = in_thunk_val.clone().try_into()?;
    let _: Procedure = body_thunk_val.clone().try_into()?;

    let call_body_thunk_cont = Procedure::new_cont(
        runtime.clone(),
        vec![
            in_thunk_val.clone(),
            body_thunk_val.clone(),
            out_thunk_val.clone(),
            Value::from(k),
        ],
        call_body_thunk,
        0,
        true,
        barrier,
    );

    Ok(Application::new(
        in_thunk,
        Some(call_body_thunk_cont),
        Vec::new(),
    ))
}

pub(crate) unsafe extern "C" fn call_body_thunk(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
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

        let barrier = barrier.as_mut().unwrap_unchecked();

        barrier.push_dyn_stack(DynStackElem::Winder(Winder {
            in_thunk: in_thunk.clone().try_into().unwrap(),
            out_thunk: out_thunk.clone().try_into().unwrap(),
        }));

        let k = Procedure::new_cont(
            Runtime::from_raw_inc_rc(runtime),
            vec![out_thunk, k],
            call_out_thunks,
            0,
            true,
            barrier,
        );

        let app = Application::new(body_thunk, Some(k), Vec::new());

        Box::into_raw(Box::new(app))
    }
}

pub(crate) unsafe extern "C" fn call_out_thunks(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    barrier: *mut ContBarrier,
) -> *mut Application {
    unsafe {
        // env[0] is the out thunk
        let out_thunk: Procedure = env.as_ref().unwrap().clone().try_into().unwrap();

        // env[1] is k, the remaining continuation
        let k = env.add(1).as_ref().unwrap().clone();

        // args[0] is the result of the body thunk
        let body_thunk_res = args.as_ref().unwrap().clone();

        let barrier = barrier.as_mut().unwrap_unchecked();
        barrier.pop_dyn_stack();

        let k = Procedure::new_cont(
            Runtime::from_raw_inc_rc(runtime),
            vec![body_thunk_res, k],
            forward_body_thunk_result,
            0,
            true,
            barrier,
        );

        let app = Application::new(out_thunk, Some(k), Vec::new());

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn forward_body_thunk_result(
    _runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    _barrier: *mut ContBarrier,
) -> *mut Application {
    unsafe {
        // env[0] is the result of the body thunk
        let body_thunk_res = env.as_ref().unwrap().clone();
        // env[1] is k, the continuation.
        let k: Procedure = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        let mut args = Vec::new();
        list_to_vec(&body_thunk_res, &mut args);

        Box::into_raw(Box::new(Application::new(k, None, args)))
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

#[cps_bridge(def = "call-with-prompt tag thunk handler", lib = "(prompts)")]
pub fn call_with_prompt(
    runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [tag, thunk, handler] = args else {
        unreachable!()
    };

    let (req_args, variadic) = k.get_formals();
    let tag: Symbol = tag.clone().try_into().unwrap();

    let barrier_id = BARRIER_ID.fetch_add(1, Ordering::Relaxed);

    barrier.push_dyn_stack(DynStackElem::Prompt(Prompt {
        tag,
        handler: handler.clone().try_into().unwrap(),
        barrier_id,
        handler_k: k.clone(),
    }));

    barrier.push_marks();

    let prompt_barrier = Procedure::new(
        runtime.clone(),
        vec![Value::from(k)],
        FuncPtr::PromptBarrier {
            barrier_id,
            k: pop_dyn_stack,
        },
        req_args,
        variadic,
    );

    Ok(Application::new(
        thunk.clone().try_into().unwrap(),
        Some(prompt_barrier),
        Vec::new(),
    ))
}

#[cps_bridge(def = "abort-to-prompt tag . values", lib = "(prompts)")]
pub fn abort_to_prompt(
    runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [tag] = args else { unreachable!() };

    let unwind_to_prompt = Procedure::new_cont(
        runtime.clone(),
        vec![
            Value::from(k),
            Value::from(rest_args.to_vec()),
            tag.clone(),
            Value::from_rust_type(barrier.save()),
        ],
        unwind_to_prompt,
        0,
        false,
        barrier,
    );

    Ok(Application::new(unwind_to_prompt, None, Vec::new()))
}

unsafe extern "C" fn unwind_to_prompt(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
) -> *mut Application {
    unsafe {
        // env[0] is continuation
        let k = env.as_ref().unwrap().clone();
        // env[1] is the arguments passed to abort-to-prompt:
        let args = env.add(1).as_ref().unwrap().clone();
        // env[2] is the prompt tag
        let tag: Symbol = env.add(2).as_ref().unwrap().clone().try_into().unwrap();
        // env[3] is the saved dyn stack
        let saved_barrier = env.add(3).as_ref().unwrap().clone();

        let barrier = barrier.as_mut().unwrap_unchecked();

        loop {
            let app = match barrier.pop_dyn_stack() {
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
                    handler_k,
                })) if prompt_tag == tag => {
                    let saved_barrier = saved_barrier
                        .try_to_rust_type::<SavedDynamicState>()
                        .unwrap();
                    let prompt_delimited_barrier = SavedDynamicState {
                        id: saved_barrier.id,
                        dyn_stack: saved_barrier.as_ref().dyn_stack[barrier.dyn_stack_len() + 1..]
                            .to_vec(),
                        cont_marks: saved_barrier.cont_marks.clone(),
                    };
                    let (req_args, var) = {
                        let k_proc: Procedure = k.clone().try_into().unwrap();
                        k_proc.get_formals()
                    };
                    // Construct the arguments. The handler's continuation comes
                    // first, followed by the resume procedure and the values.
                    let mut handler_args = vec![Value::from(Procedure::new(
                        Runtime::from_raw_inc_rc(runtime),
                        vec![
                            k,
                            Value::from(barrier_id),
                            Value::from_rust_type(prompt_delimited_barrier),
                        ],
                        FuncPtr::Bridge(delimited_continuation),
                        req_args,
                        var,
                    ))];
                    handler_args.extend(args.cast_to_scheme_type::<Vector>().unwrap().iter());
                    Application::new(handler, Some(handler_k), handler_args)
                }
                Some(DynStackElem::Winder(winder)) => {
                    // If this is a winder, we should call the out winder while unwinding
                    Application::new(
                        winder.out_thunk,
                        Some(Procedure::new_cont(
                            Runtime::from_raw_inc_rc(runtime),
                            vec![k, args, Value::from(tag), saved_barrier],
                            unwind_to_prompt,
                            0,
                            false,
                            barrier,
                        )),
                        Vec::new(),
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
    k: Procedure,
    args: &[Value],
    rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    // env[0] is the delimited continuation
    let dk = env[0].clone();

    // env[1] is the barrier Id
    let barrier_id: usize = env[1].try_to_scheme_type()?;

    // env[2] is the dyn stack of the continuation
    let saved_barrier_val = env[2].clone();
    let saved_barrier = saved_barrier_val
        .try_to_rust_type::<SavedDynamicState>()
        .unwrap();
    let saved_barrier_read = saved_barrier.as_ref();
    // Restore continuation marks
    barrier.cont_marks = saved_barrier_read.cont_marks.clone();

    let args = args.iter().chain(rest_args).cloned().collect::<Vec<_>>();

    barrier.push_dyn_stack(DynStackElem::PromptBarrier(PromptBarrier {
        barrier_id,
        replaced_k: k,
    }));

    // Simple optimization: if the saved dyn stack is empty, we
    // can just call the delimited continuation
    if saved_barrier_read.dyn_stack_is_empty() {
        Ok(Application::new(dk.try_into()?, None, args))
    } else {
        let args = Value::from(args);
        let k = Procedure::new_cont(
            runtime.clone(),
            vec![
                dk,
                args,
                saved_barrier_val,
                Value::from(0),
                Value::from(false),
            ],
            wind_delim,
            0,
            false,
            barrier,
        );
        Ok(Application::new(k, None, Vec::new()))
    }
}

unsafe extern "C" fn wind_delim(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    _args: *const Value,
    barrier: *mut ContBarrier,
) -> *mut Application {
    unsafe {
        // env[0] is the ultimate continuation
        let k = env.as_ref().unwrap().clone();

        // env[1] are the arguments to pass to k
        let args = env.add(1).as_ref().unwrap().clone();

        // env[2] is the stack we are trying to reach
        let dest_stack_val = env.add(2).as_ref().unwrap().clone();
        let dest_stack = dest_stack_val
            .try_to_rust_type::<SavedDynamicState>()
            .unwrap();
        let dest_stack_read = dest_stack.as_ref();

        // env[3] is the index into the dest stack we're at
        let mut idx: usize = env.add(3).as_ref().unwrap().cast_to_scheme_type().unwrap();

        let barrier = barrier.as_mut().unwrap_unchecked();

        // env[4] is potentially a winder that we should push onto the dyn stack
        let winder = env.add(4).as_ref().unwrap().clone();
        if winder.is_true() {
            let winder = winder.try_to_rust_type::<Winder>().unwrap();
            barrier.push_dyn_stack(DynStackElem::Winder(winder.as_ref().clone()));
        }

        while let Some(elem) = dest_stack_read.dyn_stack_get(idx) {
            idx += 1;

            if let DynStackElem::Winder(winder) = elem {
                // Call the in winder while winding
                let app = Application::new(
                    winder.in_thunk.clone(),
                    Some(Procedure::new_cont(
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
                        barrier,
                    )),
                    Vec::new(),
                );
                return Box::into_raw(Box::new(app));
            }
            barrier.push_dyn_stack(elem.clone());
        }

        let args: Vector = args.try_into().unwrap();
        let args = args.0.vec.read().to_vec();

        Box::into_raw(Box::new(Application::new(
            k.try_into().unwrap(),
            None,
            args,
        )))
    }
}
