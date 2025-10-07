//! Procedures, continuation and user, and applying values to those procedures.
//! Contains the main trampoline.

use crate::{
    env::Local,
    exceptions::{Condition, Exception, ExceptionHandler, ExceptionHandlerInner, Frame, raise},
    gc::{Gc, GcInner, Trace/*, yield_until_gc_cleared*/},
    lists::{self, list_to_vec, slice_to_list},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::BridgeFnDebugInfo,
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    syntax::Span,
    value::{UnpackedValue, Value},
};
use futures::future::BoxFuture;
use scheme_rs_macros::cps_bridge;
use std::{borrow::Cow, fmt, sync::Arc};

/// A function pointer to a generated continuation.
pub(crate) type ContinuationPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const *mut GcInner<Value>,
    globals: *const *mut GcInner<Value>,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application;

/// A function pointer to a generated user function.
pub(crate) type UserPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const *mut GcInner<Value>,
    globals: *const *mut GcInner<Value>,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
    cont: *mut GcInner<Value>,
) -> *mut Application;

/// A function pointer to an async Rust bridge function.
pub type BridgePtr = for<'a> fn(
    runtime: &'a Runtime,
    env: &'a [Gc<Value>],
    args: &'a [Value],
    rest_args: &'a [Value],
    cont: &'a Value,
    exception_handler: &'a ExceptionHandler,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Application>;

#[derive(Copy, Clone, Debug)]
pub(crate) enum FuncPtr {
    Continuation(ContinuationPtr),
    User(UserPtr),
    Bridge(BridgePtr),
    /// Special type to indicate that we should return the argument as an Err.
    HaltError,
}

unsafe impl Trace for FuncPtr {
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(crate::gc::OpaqueGcPtr)) {}

    unsafe fn finalize(&mut self) {}
}

#[derive(Clone, Trace)]
#[repr(align(16))]
pub(crate) struct ClosureInner {
    /// The runtime the Closure is defined in. This is necessary to ensure that
    /// dropping the runtime does not de-allocate the function pointer for this
    /// closure.
    // TODO: Do we make this optional in the case of bridge functions?
    pub(crate) runtime: Runtime,
    /// Environmental variables used by the closure.
    pub(crate) env: Vec<Gc<Value>>,
    /// Global variables used by this closure.
    pub(crate) globals: Vec<Gc<Value>>,
    /// Fuction pointer to the body of the closure.
    pub(crate) func: FuncPtr,
    /// Number of required arguments to this closure.
    pub(crate) num_required_args: usize,
    /// Whether or not this is a variadic function.
    pub(crate) variadic: bool,
    /// Whether or not this function is a variable transformer.
    pub(crate) is_variable_transformer: bool,
    /// Debug information for this function. Only applicable if the function is
    /// a user function, i.e. not a continuation.
    pub(crate) debug_info: Option<Arc<FuncDebugInfo>>,
}

impl ClosureInner {
    pub(crate) fn new(
        runtime: Runtime,
        env: impl Into<Vec<Gc<Value>>>,
        globals: impl Into<Vec<Gc<Value>>>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<FuncDebugInfo>>,
    ) -> Self {
        Self {
            runtime,
            env: env.into(),
            globals: globals.into(),
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

    pub async fn apply(
        &self,
        args: &[Value],
        exception_handler: ExceptionHandler,
        dynamic_wind: &DynamicWind,
    ) -> Result<Application, Value> {
        // Handle arguments:

        // Extract the continuation, if it is required
        let cont = matches!(self.func, FuncPtr::Bridge(_) | FuncPtr::User(_));
        let (cont, args) = if cont {
            let (cont, args) = args.split_last().unwrap();
            (Some(cont.clone()), args)
        } else {
            (None, args)
        };

        // Error if the number of arguments provided is incorrect
        if args.len() < self.num_required_args {
            return Ok(raise(
                self.runtime.clone(),
                Condition::wrong_num_of_args(self.num_required_args, args.len()).into(),
                exception_handler,
                dynamic_wind,
            ));
        }
        if !self.variadic && args.len() > self.num_required_args {
            return Ok(raise(
                self.runtime.clone(),
                Condition::wrong_num_of_args(self.num_required_args, args.len()).into(),
                exception_handler,
                dynamic_wind,
            ));
        }

        // If this function is variadic, create a list to put any extra arguments
        // into
        let bridge = matches!(self.func, FuncPtr::Bridge(_));
        let (args, rest_args) = if self.variadic {
            let (args, rest_args) = args.split_at(self.num_required_args);
            // If this is a bridge function, vector is more natural to work with:
            if bridge {
                (Cow::Borrowed(args), Some(rest_args))
            } else {
                let mut args = args.to_owned();
                args.push(slice_to_list(rest_args));
                (Cow::Owned(args), None)
            }
        } else {
            (Cow::Borrowed(args), None)
        };

        if bridge {
            // If this a bridge functiuon, calling it is relatively simple:
            let FuncPtr::Bridge(async_fn) = self.func else {
                unreachable!()
            };
            Ok((async_fn)(
                &self.runtime,
                &self.env,
                args.as_ref(),
                rest_args.unwrap_or(&[]),
                cont.as_ref().unwrap(),
                &exception_handler,
                dynamic_wind,
            )
            .await)
        } else if let FuncPtr::HaltError = self.func {
            Err(args[0].clone())
        } else {
            // For JIT functions, we need to convert our args into raw pointers
            // and make sure any freshly allocated rest_args are disposed of properly.

            let env = cells_to_vec_of_ptrs(&self.env);
            let globals = cells_to_vec_of_ptrs(&self.globals);
            let cont = cont.map(Gc::new);

            // Finally: call the function pointer
            let app = match self.func {
                FuncPtr::Continuation(sync_fn) => unsafe {
                    let app = (sync_fn)(
                        Gc::as_ptr(&self.runtime.0),
                        env.as_ptr(),
                        globals.as_ptr(),
                        args.as_ptr(),
                        exception_handler.as_ptr(),
                        dynamic_wind as *const DynamicWind,
                    );
                    *Box::from_raw(app)
                },
                FuncPtr::User(sync_fn) => unsafe {
                    let app = (sync_fn)(
                        Gc::as_ptr(&self.runtime.0),
                        env.as_ptr(),
                        globals.as_ptr(),
                        args.as_ptr(),
                        exception_handler.as_ptr(),
                        dynamic_wind as *const DynamicWind,
                        Gc::as_ptr(cont.as_ref().unwrap()),
                    );
                    *Box::from_raw(app)
                },
                _ => unreachable!(),
            };

            drop(cont);

            Ok(app)
        }
    }
}

/// The runtime representation of a Closure, which can be either a user function
/// or a continuation. Contains a reference to all of the globals and
/// environmental variables used in the body, along with a function pointer to
/// the body of the closure.
#[derive(Clone, Trace)]
pub struct Closure(pub(crate) Gc<ClosureInner>);

impl Closure {
    pub(crate) fn new(
        runtime: Runtime,
        env: impl Into<Vec<Gc<Value>>>,
        globals: impl Into<Vec<Gc<Value>>>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<FuncDebugInfo>>,
    ) -> Self {
        Self(Gc::new(ClosureInner {
            runtime,
            env: env.into(),
            globals: globals.into(),
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

    pub async fn call(&self, args: &[Value]) -> Result<Vec<Value>, Exception> {
        unsafe extern "C" fn halt(
            _runtime: *mut GcInner<RuntimeInner>,
            _env: *const *mut GcInner<Value>,
            _globals: *const *mut GcInner<Value>,
            args: *const Value,
            _exception_handler: *mut GcInner<ExceptionHandlerInner>,
            _dynamic_wind: *const DynamicWind,
        ) -> *mut Application {
            unsafe { crate::runtime::halt(Value::into_raw(args.read()) as i64) }
        }

        let mut args = args.to_vec();
        // TODO: We don't need to create a new one of these every time, we should just have
        // one
        args.push(Value::from(Self(Gc::new(ClosureInner::new(
            self.0.read().runtime.clone(),
            Vec::new(),
            Vec::new(),
            FuncPtr::Continuation(halt),
            0,
            true,
            None,
        )))));
        Application::new(
            self.clone(),
            args,
            ExceptionHandler::default(),
            DynamicWind::default(),
            None,
        )
        .eval()
        .await
    }
}

impl fmt::Debug for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.read().fmt(f)
    }
}

impl fmt::Debug for ClosureInner {
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
                write!(f, " . ${})", self.num_required_args)?;
            }
            return Ok(());
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

fn cells_to_vec_of_ptrs(cells: &[Gc<Value>]) -> Vec<*mut GcInner<Value>> {
    cells.iter().map(Gc::as_ptr).collect()
}

/// An application of a function to a given set of values.
pub struct Application {
    /// The operator being applied to. If None, we return the values to the Rust
    /// caller.
    op: Option<Closure>,
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
        op: Closure,
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
    pub async fn eval(mut self) -> Result<Vec<Value>, Exception> {
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
            self = match op.apply(&args, exception_handler, &dynamic_wind).await {
                Err(exception) => {
                    return Err(Exception::new(stack_trace.into_frames(), exception));
                }
                Ok(app) => app,
            };
            // yield_until_gc_cleared().await
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
pub async fn apply(
    _runtime: &Runtime,
    _env: &[Gc<Value>],
    args: &[Value],
    rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    if rest_args.is_empty() {
        return Err(Condition::wrong_num_of_args(2, args.len()));
    }
    let op: Closure = args[0].clone().try_into()?;
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
pub async fn call_with_current_continuation(
    runtime: &Runtime,
    _env: &[Gc<Value>],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let [proc] = args else { unreachable!() };
    let proc: Closure = proc.clone().try_into()?;

    let (req_args, variadic) = {
        let cont: Closure = cont.clone().try_into()?;
        let cont_read = cont.0.read();
        (cont_read.num_required_args, cont_read.variadic)
    };

    let dynamic_wind_value = Value::from(Record::from_rust_type(dynamic_wind.clone()));

    let escape_procedure = Closure::new(
        runtime.clone(),
        vec![Gc::new(cont.clone()), Gc::new(dynamic_wind_value)],
        Vec::new(),
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
///
/// Expects that the continuation and winders will be provided in the form of a
/// pair of the continuation and vector of pairs of in/out winders.
#[cps_bridge]
async fn escape_procedure(
    runtime: &Runtime,
    env: &[Gc<Value>],
    args: &[Value],
    rest_args: &[Value],
    _cont: &Value,
    exception_handler: &ExceptionHandler,
    from_extent: &DynamicWind,
) -> Result<Application, Condition> {
    // env[0] is the continuation
    let cont = env[0].read().clone();
    // env[1] is the dynamic extend of the continuation
    let to_extent = env[1].read().clone();
    let to_extent = to_extent.try_into_rust_type::<DynamicWind>()?;

    let thunks = entry_winders(from_extent, &to_extent.read());

    // Clone the continuation
    let cont = maybe_clone_continuation(&cont, &mut Vec::new()).unwrap();
    let cont: Closure = cont.try_into().unwrap();

    let (req_args, variadic) = {
        let cont_read = cont.0.read();
        (cont_read.num_required_args, cont_read.variadic)
    };

    let args = args.iter().chain(rest_args).cloned().collect::<Vec<_>>();

    let cont = Closure::new(
        runtime.clone(),
        vec![Gc::new(thunks), Gc::new(Value::from(cont))],
        Vec::new(),
        FuncPtr::Continuation(call_thunks),
        req_args,
        variadic,
        None,
    );

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

fn prepare_env_variable(value: &Value, cloned: &mut Vec<(ContinuationPtr, Value)>) -> Value {
    maybe_clone_continuation(value, cloned).unwrap_or_else(|| value.clone())
}

fn maybe_clone_continuation(
    value: &Value,
    cloned: &mut Vec<(ContinuationPtr, Value)>,
) -> Option<Value> {
    let k: Closure = value.clone().try_into().ok()?;

    let func_ptr = {
        match k.0.read().func {
            FuncPtr::Continuation(cont) => cont,
            _ => return None,
        }
    };

    if let Some(cont) = cloned
        .iter()
        .find(|(k, _)| std::ptr::fn_addr_eq(*k, func_ptr))
    {
        return Some(cont.1.clone());
    }

    let new_k = {
        let k = k.0.read();
        let new_env = k
            .env
            .iter()
            .map(|var| Gc::new(prepare_env_variable(&var.read(), cloned)))
            .collect::<Vec<_>>();
        Closure::new(
            k.runtime.clone(),
            new_env,
            k.globals.clone(),
            k.func,
            k.num_required_args,
            k.variadic,
            k.debug_info.clone(),
        )
    };

    let new_k = Value::from(new_k);
    cloned.push((func_ptr, new_k.clone()));

    Some(new_k)
}

pub(crate) unsafe extern "C" fn call_thunks(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] are the thunks:
        let thunks = Gc::from_raw_inc_rc(env.read());

        // env[1] is the continuation:
        let k: Closure = Gc::from_raw_inc_rc(env.add(1).read())
            .read()
            .clone()
            .try_into()
            .unwrap();

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

        let thunks = Closure::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![thunks, Gc::new(collected_args), Gc::new(Value::from(k))],
            Vec::new(),
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
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
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
                let head_thunk: Closure = head_thunk.clone().try_into().unwrap();
                let cont = Closure::new(
                    Runtime::from_raw_inc_rc(runtime),
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

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn call_consumer_with_values(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] is the consumer
        let consumer = Gc::from_raw_inc_rc(env.read());
        let consumer = {
            let consumer_read = consumer.read();
            let consumer: Closure = match consumer_read.clone().try_into() {
                Ok(consumer) => consumer,
                _ => {
                    let raised = raise(
                        Runtime::from_raw_inc_rc(runtime),
                        Condition::invalid_operator(consumer_read.type_name()).into(),
                        ExceptionHandler::from_ptr(exception_handler),
                        dynamic_wind.as_ref().unwrap(),
                    );
                    return Box::into_raw(Box::new(raised));
                }
            };
            consumer.clone()
        };

        let consumer_read = consumer.0.read();

        // env[1] is the continuation
        let cont = Gc::from_raw_inc_rc(env.add(1).read());

        let mut collected_args: Vec<_> = (0..consumer_read.num_required_args)
            .map(|i| args.add(i).as_ref().unwrap().clone())
            .collect();

        // I hate this constant going back and forth from variadic to list. I have
        // to figure out a way to make it consistent
        if consumer_read.variadic {
            let rest_args = args
                .add(consumer_read.num_required_args)
                .as_ref()
                .unwrap()
                .clone();
            let mut vec = Vec::new();
            list_to_vec(&rest_args, &mut vec);
            collected_args.extend(vec);
        }

        collected_args.push(cont.read().clone());

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
pub async fn call_with_values(
    runtime: &Runtime,
    _env: &[Gc<Value>],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let [producer, consumer] = args else {
        return Err(Condition::wrong_num_of_args(2, args.len()));
    };

    let producer: Closure = producer.clone().try_into()?;
    let consumer: Closure = consumer.clone().try_into()?;

    // Get the details of the consumer:
    let (num_required_args, variadic) = {
        let consumer_read = consumer.0.read();
        (consumer_read.num_required_args, consumer_read.variadic)
    };

    let call_consumer_closure = Closure::new(
        runtime.clone(),
        vec![Gc::new(Value::from(consumer)), Gc::new(cont.clone())],
        Vec::new(),
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
    pub(crate) winders: Vec<(Closure, Closure)>,
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
pub async fn dynamic_wind(
    runtime: &Runtime,
    _env: &[Gc<Value>],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let [in_thunk_val, body_thunk_val, out_thunk_val] = args else {
        return Err(Condition::wrong_num_of_args(3, args.len()));
    };

    let in_thunk: Closure = in_thunk_val.clone().try_into()?;
    let _: Closure = body_thunk_val.clone().try_into()?;

    let call_body_thunk_cont = Closure::new(
        runtime.clone(),
        vec![
            Gc::new(in_thunk_val.clone()),
            Gc::new(body_thunk_val.clone()),
            Gc::new(out_thunk_val.clone()),
            Gc::new(cont.clone()),
        ],
        Vec::new(),
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
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] is the in thunk
        let in_thunk = Gc::from_raw_inc_rc(env.read());

        // env[1] is the body thunk
        let body_thunk: Closure = Gc::from_raw_inc_rc(env.add(1).read())
            .read()
            .clone()
            .try_into()
            .unwrap();

        // env[2] is the out thunk
        let out_thunk = Gc::from_raw_inc_rc(env.add(2).read());

        // env[3] is k, the continuation
        let k = Gc::from_raw_inc_rc(env.add(3).read());

        let mut new_extent = dynamic_wind.as_ref().unwrap().clone();
        new_extent.winders.push((
            in_thunk.read().clone().try_into().unwrap(),
            out_thunk.read().clone().try_into().unwrap(),
        ));

        let cont = Closure::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![out_thunk, k],
            Vec::new(),
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
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] is the out thunk
        let out_thunk: Closure = Gc::from_raw_inc_rc(env.read())
            .read()
            .clone()
            .try_into()
            .unwrap();

        // env[1] is k, the remaining continuation
        let k = Gc::from_raw_inc_rc(env.add(1).read());

        // args[0] is the result of the body thunk
        let body_thunk_res = Gc::new(args.as_ref().unwrap().clone());

        let mut new_extent = dynamic_wind.as_ref().unwrap().clone();
        new_extent.winders.pop();

        let cont = Closure(Gc::new(ClosureInner::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![body_thunk_res, k],
            Vec::new(),
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
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] is the result of the body thunk
        let body_thunk_res = Gc::from_raw_inc_rc(env.read()).read().clone();
        // env[1] is k, the continuation.
        let k: Closure = Gc::from_raw_inc_rc(env.add(1).read())
            .read()
            .clone()
            .try_into()
            .unwrap();

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
