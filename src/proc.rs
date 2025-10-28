//! Procedures, continuation and user, and applying values to those procedures.
//! Contains the main evaluation trampoline.

use crate::{
    env::Local,
    exceptions::{Condition, Exception, ExceptionHandler, ExceptionHandlerInner, Frame, raise},
    gc::{Gc, GcInner, Trace},
    lists::{self, list_to_vec, slice_to_list},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::BridgeFnDebugInfo,
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    syntax::Span,
    value::{Cell, UnpackedValue, UnpackedValueRef, Value},
};
use scheme_rs_macros::{cps_bridge, maybe_async, maybe_await};
use std::{borrow::Cow, collections::HashMap, fmt, hash::Hash, mem::MaybeUninit, sync::Arc};

/// A function pointer to a generated continuation.
pub(crate) type ContinuationPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application;

/// A function pointer to a generated user function.
pub(crate) type UserPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
    cont: Value,
) -> *mut Application;

/// A function pointer to a sync Rust bridge function.
pub type SyncBridgePtr = for<'a> fn(
    runtime: &'a Runtime,
    env: &'a [Value],
    args: &'a [Value],
    rest_args: &'a [Value],
    cont: &'a Value,
    exception_handler: &'a ExceptionHandler,
    dynamic_wind: &'a DynamicWind,
) -> Application;

/// A function pointer to an async Rust bridge function.
#[cfg(feature = "async")]
pub type AsyncBridgePtr = for<'a> fn(
    runtime: &'a Runtime,
    env: &'a [Value],
    args: &'a [Value],
    rest_args: &'a [Value],
    cont: &'a Value,
    exception_handler: &'a ExceptionHandler,
    dynamic_wind: &'a DynamicWind,
) -> futures::future::BoxFuture<'a, Application>;

#[derive(Copy, Clone, Debug)]
pub(crate) enum FuncPtr {
    Continuation(ContinuationPtr),
    User(UserPtr),
    Bridge(SyncBridgePtr),
    #[cfg(feature = "async")]
    AsyncBridge(AsyncBridgePtr),
    /// Special type to indicate that we should return the argument as an Err.
    HaltError,
}

enum JitFuncPtr {
    Continuation(ContinuationPtr),
    User(UserPtr),
}

unsafe impl Trace for FuncPtr {
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(crate::gc::OpaqueGcPtr)) {}

    unsafe fn finalize(&mut self) {}
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
        matches!(self.func, FuncPtr::Continuation(_))
    }

    pub fn is_user_func(&self) -> bool {
        !self.is_continuation()
    }

    pub(crate) fn prepare_args<'a>(
        &self,
        args: &'a [Value],
        exception_handler: &ExceptionHandler,
        dynamic_wind: &DynamicWind,
    ) -> Result<(&'a [Value], Option<Value>), Application> {
        // Extract the continuation, if it is required
        let requires_cont = !matches!(self.func, FuncPtr::Continuation(_));
        let (cont, args) = if requires_cont {
            let (cont, args) = args.split_last().unwrap();
            (Some(cont.clone()), args)
        } else {
            (None, args)
        };

        // Error if the number of arguments provided is incorrect
        if args.len() < self.num_required_args {
            return Err(raise(
                self.runtime.clone(),
                Condition::wrong_num_of_args(self.num_required_args, args.len()).into(),
                exception_handler.clone(),
                dynamic_wind,
            ));
        }

        if !self.variadic && args.len() > self.num_required_args {
            return Err(raise(
                self.runtime.clone(),
                Condition::wrong_num_of_args(self.num_required_args, args.len()).into(),
                exception_handler.clone(),
                dynamic_wind,
            ));
        }

        Ok((args, cont))
    }

    #[cfg(feature = "async")]
    async fn apply_async_bridge(
        &self,
        func: AsyncBridgePtr,
        args: &[Value],
        cont: Value,
        exception_handler: &ExceptionHandler,
        dynamic_wind: &DynamicWind,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(
            &self.runtime,
            &self.env,
            args,
            rest_args,
            &cont,
            exception_handler,
            dynamic_wind,
        )
        .await
    }

    fn apply_sync_bridge(
        &self,
        func: SyncBridgePtr,
        args: &[Value],
        cont: Value,
        exception_handler: &ExceptionHandler,
        dynamic_wind: &DynamicWind,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(
            &self.runtime,
            &self.env,
            args,
            rest_args,
            &cont,
            exception_handler,
            dynamic_wind,
        )
    }

    fn apply_jit(
        &self,
        func: JitFuncPtr,
        args: &[Value],
        cont: Option<Value>,
        exception_handler: &ExceptionHandler,
        dynamic_wind: &DynamicWind,
    ) -> Application {
        let args = if self.variadic {
            let (args, rest_args) = args.split_at(self.num_required_args);
            let mut args = args.to_owned();
            args.push(slice_to_list(rest_args));
            Cow::Owned(args)
        } else {
            Cow::Borrowed(args)
        };

        let app = match func {
            JitFuncPtr::Continuation(sync_fn) => unsafe {
                (sync_fn)(
                    Gc::as_ptr(&self.runtime.0),
                    self.env.as_ptr(),
                    args.as_ptr(),
                    exception_handler.as_ptr(),
                    dynamic_wind as *const DynamicWind,
                )
            },
            JitFuncPtr::User(sync_fn) => unsafe {
                (sync_fn)(
                    Gc::as_ptr(&self.runtime.0),
                    self.env.as_ptr(),
                    args.as_ptr(),
                    exception_handler.as_ptr(),
                    dynamic_wind as *const DynamicWind,
                    Value::from_raw(Value::as_raw(cont.as_ref().unwrap())),
                )
            },
        };

        unsafe { *Box::from_raw(app) }
    }

    #[maybe_async]
    pub fn apply(
        &self,
        args: &[Value],
        exception_handler: &ExceptionHandler,
        dynamic_wind: &DynamicWind,
    ) -> Result<Application, Value> {
        if let FuncPtr::HaltError = self.func {
            return Err(args[0].clone());
        }

        let (args, k) = match self.prepare_args(args, exception_handler, dynamic_wind) {
            Ok(args) => args,
            Err(raised) => return Ok(raised),
        };

        let app = match self.func {
            FuncPtr::Continuation(cont) => self.apply_jit(
                JitFuncPtr::Continuation(cont),
                args,
                k,
                exception_handler,
                dynamic_wind,
            ),
            FuncPtr::User(user) => self.apply_jit(
                JitFuncPtr::User(user),
                args,
                k,
                exception_handler,
                dynamic_wind,
            ),
            FuncPtr::Bridge(sbridge) => {
                self.apply_sync_bridge(sbridge, args, k.unwrap(), exception_handler, dynamic_wind)
            }
            #[cfg(feature = "async")]
            FuncPtr::AsyncBridge(abridge) => {
                self.apply_async_bridge(abridge, args, k.unwrap(), exception_handler, dynamic_wind)
                    .await
            }
            FuncPtr::HaltError => unreachable!(),
        };

        Ok(app)
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
        self.0.read().runtime.clone()
    }

    pub fn is_variable_transformer(&self) -> bool {
        self.0.read().is_variable_transformer
    }

    #[maybe_async]
    pub fn call(&self, args: &[Value]) -> Result<Vec<Value>, Exception> {
        unsafe extern "C" fn halt(
            _runtime: *mut GcInner<RuntimeInner>,
            _env: *const Value,
            args: *const Value,
            _exception_handler: *mut GcInner<ExceptionHandlerInner>,
            _dynamic_wind: *const DynamicWind,
        ) -> *mut Application {
            unsafe { crate::runtime::halt(Value::into_raw(args.read()) as i64) }
        }

        let mut args = args.to_vec();

        // TODO: We don't need to create a new one of these every time, we should just have
        // one
        args.push(Value::from(Self(Gc::new(ProcedureInner::new(
            self.0.read().runtime.clone(),
            Vec::new(),
            FuncPtr::Continuation(halt),
            0,
            true,
            None,
        )))));

        maybe_await!(
            Application::new(
                self.clone(),
                args,
                ExceptionHandler::default(),
                DynamicWind::default(),
                None,
            )
            .eval()
        )
    }
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.read().fmt(f)
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

/// An application of a function to a given set of values.
pub struct Application {
    /// The operator being applied to. If None, we return the values to the Rust
    /// caller.
    op: Option<Procedure>,
    /// The arguments being applied to the operator.
    args: Vec<Value>,
    /// The current exception handler to be passed to the operator.
    exception_handler: ExceptionHandler,
    /// The dynamic extend of the application.
    dynamic_wind: DynamicWind,
    /// The call site of this application, if it exists.
    call_site: Option<Arc<Span>>,
}

impl Application {
    pub fn new(
        op: Procedure,
        args: Vec<Value>,
        exception_handler: ExceptionHandler,
        dynamic_wind: DynamicWind,
        call_site: Option<Arc<Span>>,
    ) -> Self {
        Self {
            // We really gotta figure out how to deal with this better
            op: Some(op),
            args,
            exception_handler,
            dynamic_wind,
            call_site,
        }
    }

    pub fn halt(args: Vec<Value>) -> Self {
        Self {
            op: None,
            args,
            exception_handler: ExceptionHandler::default(),
            dynamic_wind: DynamicWind::default(),
            call_site: None,
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    #[maybe_async]
    pub fn eval(mut self) -> Result<Vec<Value>, Exception> {
        let mut stack_trace = StackTraceCollector::new();

        while let Application {
            op: Some(op),
            args,
            exception_handler,
            dynamic_wind,
            call_site,
        } = self
        {
            let op = { op.0.read().as_ref().clone() };
            stack_trace.collect_application(op.debug_info.clone(), call_site);
            self = match maybe_await!(op.apply(&args, &exception_handler, &dynamic_wind)) {
                Err(exception) => {
                    return Err(Exception::new(stack_trace.into_frames(), exception));
                }
                Ok(app) => app,
            };
        }

        // If we have no operator left, return the arguments as the final values:
        Ok(self.args)
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
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    if rest_args.is_empty() {
        return Err(Condition::wrong_num_of_args(2, args.len()));
    }
    let op: Procedure = args[0].clone().try_into()?;
    let (last, args) = rest_args.split_last().unwrap();
    let mut args = args.to_vec();
    list_to_vec(last, &mut args);
    args.push(cont.clone());
    Ok(Application::new(
        op.clone(),
        args,
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    ))
}

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
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let [proc] = args else { unreachable!() };
    let proc: Procedure = proc.clone().try_into()?;

    let (req_args, variadic) = {
        let cont: Procedure = cont.clone().try_into()?;
        let cont_read = cont.0.read();
        (cont_read.num_required_args, cont_read.variadic)
    };

    let dynamic_wind_value = Value::from(Record::from_rust_type(dynamic_wind.clone()));

    let escape_procedure = Procedure::new(
        runtime.clone(),
        vec![cont.clone(), dynamic_wind_value],
        FuncPtr::Bridge(escape_procedure),
        req_args,
        variadic,
        None,
    );

    let app = Application::new(
        proc,
        vec![Value::from(escape_procedure), cont.clone()],
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    );

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
    _cont: &Value,
    exception_handler: &ExceptionHandler,
    from_extent: &DynamicWind,
) -> Result<Application, Condition> {
    // env[0] is the continuation
    let cont = env[0].clone();
    // env[1] is the dynamic extend of the continuation
    let to_extent = env[1].clone();
    let to_extent = to_extent.try_into_rust_type::<DynamicWind>()?;

    let thunks = entry_winders(from_extent, &to_extent.read());

    // Clone the continuation
    let cont_ref = cont.unpacked_ref();
    let cont = maybe_clone_continuation(
        &cont_ref,
        &mut StackClone::default(),
        &mut StackClone::default(),
    )
    .unwrap_or_else(|| cont.clone());
    let cont: Procedure = cont.try_into().unwrap();

    let args = args.iter().chain(rest_args).cloned().collect::<Vec<_>>();

    let cont = if thunks.is_null() {
        cont
    } else {
        let (req_args, variadic) = {
            let cont_read = cont.0.read();
            (cont_read.num_required_args, cont_read.variadic)
        };

        Procedure::new(
            runtime.clone(),
            vec![thunks, Value::from(cont)],
            FuncPtr::Continuation(call_thunks),
            req_args,
            variadic,
            None,
        )
    };

    let app = Application::new(
        cont,
        args,
        exception_handler.clone(),
        from_extent.clone(),
        None,
    );

    Ok(app)
}

fn entry_winders(from_extent: &DynamicWind, to_extent: &DynamicWind) -> Value {
    let mut from_winders = from_extent.winders.as_slice();
    let mut to_winders = to_extent.winders.as_slice();

    while let Some((to_first, to_remaining)) = to_winders.split_first() {
        let Some((from_first, from_remaining)) = from_winders.split_first() else {
            break;
        };

        if !Gc::ptr_eq(&from_first.0.0, &to_first.0.0) {
            break;
        }

        from_winders = from_remaining;
        to_winders = to_remaining;
    }

    let mut thunks = Value::null();
    for thunk in from_winders.iter().chain(to_winders).rev() {
        thunks = Value::from((Value::from(thunk.0.clone()), thunks));
    }

    thunks
}

fn prepare_env_variable(
    value: &Value,
    cloned_k: &mut StackClone<ContinuationPtr>,
    cloned_cells: &mut StackClone<*mut GcInner<Value>>,
) -> Value {
    let value_ref = value.unpacked_ref();
    maybe_clone_continuation(&value_ref, cloned_k, cloned_cells)
        .or_else(|| maybe_clone_cell(&value_ref, cloned_cells))
        .unwrap_or_else(|| value.clone())
}

fn maybe_clone_continuation(
    value: &UnpackedValueRef<'_>,
    cloned_k: &mut StackClone<ContinuationPtr>,
    cloned_cells: &mut StackClone<*mut GcInner<Value>>,
) -> Option<Value> {
    let UnpackedValue::Procedure(k) = value.as_ref() else {
        return None;
    };

    let func_ptr = {
        let k_read = k.0.read();
        match k_read.func {
            FuncPtr::Continuation(cont) if !k_read.env.is_empty() => cont,
            _ => return None,
        }
    };

    if let Some(k) = cloned_k.get(&func_ptr) {
        return Some(k.clone());
    }

    let new_k = {
        let k = k.0.read();
        let new_env = k
            .env
            .iter()
            .map(|var| prepare_env_variable(var, cloned_k, cloned_cells))
            .collect::<Vec<_>>();
        Procedure::new(
            k.runtime.clone(),
            new_env,
            k.func,
            k.num_required_args,
            k.variadic,
            k.debug_info.clone(),
        )
    };

    let new_k = Value::from(new_k);
    cloned_k.insert(func_ptr, new_k.clone());

    Some(new_k)
}

fn maybe_clone_cell(
    value: &UnpackedValueRef<'_>,
    cloned: &mut StackClone<*mut GcInner<Value>>,
) -> Option<Value> {
    let UnpackedValue::Cell(cell) = value.as_ref() else {
        return None;
    };

    let cell_ptr = Gc::as_ptr(&cell.0);

    if let Some(cloned) = cloned.get(&cell_ptr) {
        return Some(cloned);
    }

    let new_cell = Value::from(Cell(Gc::new(cell.0.read().clone())));
    cloned.insert(cell_ptr, new_cell.clone());
    Some(new_cell)
}

const MAX_ON_STACK: usize = 10;

enum StackClone<K> {
    Stack {
        len: usize,
        stack: [MaybeUninit<(K, Value)>; MAX_ON_STACK],
    },
    Heap(HashMap<K, Value>),
}

impl<K> Default for StackClone<K> {
    fn default() -> Self {
        StackClone::Stack {
            len: 0,
            stack: [const { MaybeUninit::uninit() }; MAX_ON_STACK],
        }
    }
}

impl<K> StackClone<K>
where
    K: PartialEq + Eq + Hash,
{
    fn get(&self, key: &K) -> Option<Value> {
        match self {
            Self::Stack { len, stack } => {
                for elem in stack.iter().take(*len) {
                    let elem = unsafe { elem.assume_init_ref() };
                    if &elem.0 == key {
                        return Some(elem.1.clone());
                    }
                }
                None
            }
            Self::Heap(heap) => heap.get(key).cloned(),
        }
    }

    fn insert(&mut self, key: K, val: Value) {
        match self {
            Self::Stack { len, stack } if *len == MAX_ON_STACK => {
                let mut heap =
                    std::mem::replace(stack, [const { MaybeUninit::uninit() }; MAX_ON_STACK])
                        .into_iter()
                        .map(|x| unsafe { x.assume_init() })
                        .collect::<HashMap<_, _>>();
                heap.insert(key, val);
                *self = Self::Heap(heap);
            }
            Self::Stack { len, stack } => {
                stack[*len] = MaybeUninit::new((key, val));
                *len += 1;
            }
            Self::Heap(heap) => {
                heap.insert(key, val);
            }
        }
    }
}

pub(crate) unsafe extern "C" fn call_thunks(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] are the thunks:
        let thunks = env.as_ref().unwrap().clone();

        // env[1] is the continuation:
        let k: Procedure = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        // k determines the number of arguments:
        let collected_args = {
            let k_read = k.0.read();
            let mut num_args = k_read.num_required_args;

            if k_read.is_user_func() {
                num_args += 1;
            }

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

        let thunks = Procedure::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![thunks, collected_args, Value::from(k)],
            FuncPtr::Continuation(call_thunks_pass_args),
            0,
            false,
            None,
        );

        let app = Application::new(
            thunks,
            Vec::new(),
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            None,
        );

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn call_thunks_pass_args(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] are the thunks:
        let thunks = env.as_ref().unwrap().clone();

        // env[1] are the collected arguments
        let args = env.add(1).as_ref().unwrap().clone();

        // env[2] is k1, the current continuation
        let k = env.add(2).as_ref().unwrap().clone();

        let app = match &*thunks.unpacked_ref() {
            // UnpackedValue::Pair(head_thunk, tail) => {
            UnpackedValue::Pair(pair) => {
                let lists::Pair(head_thunk, tail) = &*pair.read();
                let head_thunk: Procedure = head_thunk.clone().try_into().unwrap();
                let cont = Procedure::new(
                    Runtime::from_raw_inc_rc(runtime),
                    vec![tail.clone(), args, k],
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
                list_to_vec(&args, &mut collected_args);
                Application::new(
                    k.try_into().unwrap(),
                    collected_args,
                    ExceptionHandler::from_ptr(exception_handler),
                    dynamic_wind.as_ref().unwrap().clone(),
                    None,
                )
            }
            _ => unreachable!(),
        };

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn call_consumer_with_values(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
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
                    ExceptionHandler::from_ptr(exception_handler),
                    dynamic_wind.as_ref().unwrap(),
                );
                return Box::into_raw(Box::new(raised));
            }
        };

        // env[1] is the continuation
        let k = env.add(1).as_ref().unwrap().clone();

        let mut collected_args: Vec<_> = (0..consumer.0.read().num_required_args)
            .map(|i| args.add(i).as_ref().unwrap().clone())
            .collect();

        // I hate this constant going back and forth from variadic to list. I have
        // to figure out a way to make it consistent
        if consumer.0.read().variadic {
            let rest_args = args
                .add(consumer.0.read().num_required_args)
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
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
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
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let [producer, consumer] = args else {
        return Err(Condition::wrong_num_of_args(2, args.len()));
    };

    let producer: Procedure = producer.clone().try_into()?;
    let consumer: Procedure = consumer.clone().try_into()?;

    // Get the details of the consumer:
    let (num_required_args, variadic) = {
        let consumer_read = consumer.0.read();
        (consumer_read.num_required_args, consumer_read.variadic)
    };

    let call_consumer_closure = Procedure::new(
        runtime.clone(),
        vec![Value::from(consumer), cont.clone()],
        FuncPtr::Continuation(call_consumer_with_values),
        num_required_args,
        variadic,
        None,
    );

    Ok(Application::new(
        producer,
        vec![Value::from(call_consumer_closure)],
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    ))
}

#[derive(Clone, Debug, Default, Trace)]
pub struct DynamicWind {
    pub(crate) winders: Vec<(Procedure, Procedure)>,
}

impl SchemeCompatible for DynamicWind {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "dynamic-wind", opaque: true, sealed: true)
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
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
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
            cont.clone(),
        ],
        FuncPtr::Continuation(call_body_thunk),
        0,
        true,
        None,
    );

    Ok(Application::new(
        in_thunk,
        vec![Value::from(call_body_thunk_cont)],
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    ))
}

pub(crate) unsafe extern "C" fn call_body_thunk(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
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

        let mut new_extent = dynamic_wind.as_ref().unwrap().clone();
        new_extent.winders.push((
            in_thunk.clone().try_into().unwrap(),
            out_thunk.clone().try_into().unwrap(),
        ));

        let cont = Procedure::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![out_thunk, k],
            FuncPtr::Continuation(call_out_thunks),
            0,
            true,
            None,
        );

        let app = Application::new(
            body_thunk,
            vec![Value::from(cont)],
            ExceptionHandler::from_ptr(exception_handler),
            new_extent,
            None,
        );

        Box::into_raw(Box::new(app))
    }
}

pub(crate) unsafe extern "C" fn call_out_thunks(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] is the out thunk
        let out_thunk: Procedure = env.as_ref().unwrap().clone().try_into().unwrap();

        // env[1] is k, the remaining continuation
        let k = env.add(1).as_ref().unwrap().clone();

        // args[0] is the result of the body thunk
        let body_thunk_res = args.as_ref().unwrap().clone();

        let mut new_extent = dynamic_wind.as_ref().unwrap().clone();
        new_extent.winders.pop();

        let cont = Procedure(Gc::new(ProcedureInner::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![body_thunk_res, k],
            FuncPtr::Continuation(forward_body_thunk_result),
            0,
            true,
            None,
        )));

        let app = Application::new(
            out_thunk,
            vec![Value::from(cont)],
            ExceptionHandler::from_ptr(exception_handler),
            new_extent,
            None,
        );

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn forward_body_thunk_result(
    _runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] is the result of the body thunk
        let body_thunk_res = env.as_ref().unwrap().clone();
        // env[1] is k, the continuation.
        let k: Procedure = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        let mut args = Vec::new();
        list_to_vec(&body_thunk_res, &mut args);

        let app = Application::new(
            k,
            args,
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            None,
        );

        Box::into_raw(Box::new(app))
    }
}
