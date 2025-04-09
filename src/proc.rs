//! Procedures, continuation and user, and applying values to those procedures.
//! Contains the main trampoline.

use crate::{
    exception::{Condition, Exception, ExceptionHandler, Frame},
    gc::{Gc, GcInner, Trace},
    lists::{list_to_vec, slice_to_list},
    registry::{BridgeFn, BridgeFnDebugInfo},
    runtime::{FunctionDebugInfoId, Runtime, IGNORE_FUNCTION},
    syntax::Span,
    value::Value,
};
use futures::future::BoxFuture;
use std::{borrow::Cow, collections::HashMap, fmt, ptr::null_mut};

pub type Record = Vec<Gc<Value>>;

/// A function pointer to a generated continuation.
pub type ContinuationPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition>;

/// A function pointer to a generated closure function.
pub type ClosurePtr = unsafe extern "C" fn(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
    cont: *mut GcInner<Value>,
) -> *mut Result<Application, Condition>;

/// A function pointer to an async Rust bridge function.
pub type BridgePtr = for<'a> fn(
    args: &'a [Gc<Value>],
    rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Gc<Value>>>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FuncPtr {
    Continuation(ContinuationPtr),
    Closure(ClosurePtr),
    Bridge(BridgePtr),
}

unsafe impl Trace for FuncPtr {
    unsafe fn visit_children(&self, _visitor: unsafe fn(crate::gc::OpaqueGcPtr)) {}

    unsafe fn finalize(&mut self) {}
}

/// The runtime representation of a Closure, which can be either a user function
/// or a continuation. Contains a reference to all of the globals and
/// environmental variables used in the body, along with a function pointer to
/// the body of the closure.
#[derive(Clone, Trace, PartialEq)]
#[repr(align(16))]
pub struct Closure {
    /// The runtime the Closure is defined in. This is necessary to ensure that
    /// dropping the runtime does not de-allocate the function pointer for this
    /// closure.
    pub runtime: Gc<Runtime>,
    /// Environmental variables used by the closure.
    pub env: Record,
    /// Global variables used by this closure.
    pub globals: Record,
    /// Fuction pointer to the body of the closure.
    pub func: FuncPtr,
    /// Number of required arguments to this closure.
    pub num_required_args: usize,
    /// Whether or not this is a variadic function.
    pub variadic: bool,
    /// Debug information for this function. Only applicable if the function is
    /// a user function, i.e. not a continuation.
    pub debug_info: Option<FunctionDebugInfoId>,
}

impl Closure {
    pub fn new(
        runtime: Gc<Runtime>,
        env: impl Into<Record>,
        globals: impl Into<Record>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<FunctionDebugInfoId>,
    ) -> Self {
        Self {
            runtime,
            env: env.into(),
            globals: globals.into(),
            func,
            num_required_args,
            variadic,
            debug_info,
        }
    }

    pub fn is_continuation(&self) -> bool {
        matches!(self.func, FuncPtr::Continuation(_))
    }

    pub fn is_user_func(&self) -> bool {
        !self.is_continuation()
    }

    pub(crate) fn deep_clone(&mut self, cloned: &mut HashMap<Gc<Value>, Gc<Value>>) {
        if self.is_user_func() {
            return;
        }
        for captured in &mut self.env {
            *captured = clone_continuation_env(captured, cloned);
        }
    }

    pub async fn call(&self, args: &[Gc<Value>]) -> Result<Record, Exception> {
        unsafe extern "C" fn halt(
            _runtime: *mut GcInner<Runtime>,
            _env: *const *mut GcInner<Value>,
            _globals: *const *mut GcInner<Value>,
            args: *const *mut GcInner<Value>,
            _exception_handler: *mut GcInner<ExceptionHandler>,
            _dynamic_wind: *const DynamicWind,
        ) -> *mut Result<Application, Condition> {
            crate::runtime::halt(args.read())
        }

        let mut args = args.to_vec();
        // TODO: We don't need to create a new one of these every time, we should just have
        // one
        args.push(Gc::new(Value::Closure(Closure::new(
            self.runtime.clone(),
            Vec::new(),
            Vec::new(),
            FuncPtr::Continuation(halt),
            0,
            true,
            None,
        ))));
        Application::new(self.clone(), args, None, DynamicWind::default(), None)
            .eval()
            .await
    }

    pub async fn apply(
        &self,
        args: &[Gc<Value>],
        exception_handler: Option<Gc<ExceptionHandler>>,
        dynamic_wind: &DynamicWind,
    ) -> Result<Application, Gc<Value>> {
        // Handle arguments

        // Extract the continuation, if it is required
        let cont = !matches!(self.func, FuncPtr::Continuation(_));
        let (cont, args) = if cont {
            let (cont, args) = args.split_last().unwrap();
            (Some(cont), args)
        } else {
            (None, args)
        };

        // Error if the number of arguments provided is incorrect
        if args.len() < self.num_required_args {
            return Err(Condition::wrong_num_of_args(self.num_required_args, args.len()).into());
        }
        if !self.variadic && args.len() > self.num_required_args {
            return Err(Condition::wrong_num_of_args(self.num_required_args, args.len()).into());
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
                args.push(Gc::new(slice_to_list(rest_args)));
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
            (async_fn)(
                args.as_ref(),
                rest_args.unwrap_or(&[]),
                cont.unwrap(),
                &exception_handler,
                dynamic_wind,
            )
            .await
        } else {
            // For LLVM functions, we need to convert our args into raw pointers
            // and make sure any freshly allocated rest_args are disposed of poperly.

            let env = values_to_vec_of_ptrs(&self.env);
            let globals = values_to_vec_of_ptrs(&self.globals);

            // Safety: args must last until the return of app so any freshly allocated var
            // arg isn't dropped before it's upgraded to a proper Gc
            let args = values_to_vec_of_ptrs(args.as_ref());

            // Finally: call the function pointer
            let app = match self.func {
                FuncPtr::Continuation(sync_fn) => unsafe {
                    let app = (sync_fn)(
                        self.runtime.as_ptr(),
                        env.as_ptr(),
                        globals.as_ptr(),
                        args.as_ptr(),
                        exception_handler.as_ref().map_or_else(null_mut, Gc::as_ptr),
                        dynamic_wind as *const DynamicWind,
                    );
                    *Box::from_raw(app)
                },
                FuncPtr::Closure(sync_fn) => unsafe {
                    let app = (sync_fn)(
                        self.runtime.as_ptr(),
                        env.as_ptr(),
                        globals.as_ptr(),
                        args.as_ptr(),
                        exception_handler.as_ref().map_or_else(null_mut, Gc::as_ptr),
                        dynamic_wind as *const DynamicWind,
                        cont.unwrap().as_ptr(),
                    );
                    *Box::from_raw(app)
                },
                _ => unreachable!(),
            };

            // Now we can drop the args
            drop(args);

            Ok(app?)
        }
    }
}

impl fmt::Debug for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some(debug_info_id) = self.debug_info else {
            return write!(f, "continuation");
        };

        let runtime_ref = self.runtime.read();
        let Some(debug_info) = runtime_ref
            .debug_info
            .get_function_debug_info(debug_info_id)
        else {
            return write!(f, "unknown-function");
        };

        if let Some(ref proc_name) = debug_info.name {
            write!(f, "({proc_name}")?;
        } else {
            write!(f, "(<lambda>")?;
        }

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

// This is really sorta emblematic of my excess allocations. Really gotta fix that
// at some point.
fn values_to_vec_of_ptrs(vals: &[Gc<Value>]) -> Vec<*mut GcInner<Value>> {
    vals.iter().map(Gc::as_ptr).collect()
}

/// An application of a function to a given set of values.
pub struct Application {
    /// The operator being applied to. If None, we return the values to the Rust
    /// caller.
    op: Option<Closure>,
    /// The arguments being applied to the operator.
    args: Record,
    /// The current exception handler to be passed to the operator.
    exception_handler: Option<Gc<ExceptionHandler>>,
    /// The dynamic extend of the application.
    dynamic_wind: DynamicWind,
    /// The call site of this application, if it exists.
    call_site: Option<Span>,
}

impl Application {
    pub fn new(
        op: Closure,
        args: Record,
        exception_handler: Option<Gc<ExceptionHandler>>,
        dynamic_wind: DynamicWind,
        call_site: Option<Span>,
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

    pub fn halt(args: Record) -> Self {
        Self {
            op: None,
            args,
            exception_handler: None,
            dynamic_wind: DynamicWind::default(),
            call_site: None,
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    pub async fn eval(mut self) -> Result<Record, Exception> {
        let mut stack_trace = StackTraceCollector::new();

        while let Application {
            op: Some(op),
            args,
            exception_handler,
            dynamic_wind,
            call_site,
        } = self
        {
            stack_trace.collect_application(&op.runtime, op.debug_info, call_site);
            self = match op.apply(&args, exception_handler, &dynamic_wind).await {
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
        let mut frames = Vec::new();
        let mut last_call_site = None;

        for trace in self.stack_trace.into_iter() {
            match trace {
                StackTrace::CallSite(new_call_site) => {
                    last_call_site = last_call_site.or(new_call_site);
                }
                StackTrace::UserFuncCall {
                    runtime,
                    user_func_info_id,
                    call_site,
                } => {
                    if let Some(debug_info) = runtime
                        .read()
                        .debug_info
                        .get_function_debug_info(user_func_info_id)
                    {
                        let proc = debug_info
                            .name
                            .clone()
                            .unwrap_or_else(|| "<lambda>".to_string());
                        last_call_site = call_site.or(last_call_site);
                        frames.push(Frame::new(proc, last_call_site.clone()));
                    }
                }
            }
        }

        frames
    }

    fn collect_application(
        &mut self,
        runtime: &Gc<Runtime>,
        user_func_info: Option<FunctionDebugInfoId>,
        call_site: Option<Span>,
    ) {
        if let Some(user_func_info_id) = user_func_info {
            // This is a user func, and therefore we should push to the
            // current stack trace.
            let trace = if user_func_info_id != IGNORE_FUNCTION {
                StackTrace::UserFuncCall {
                    runtime: runtime.clone(),
                    user_func_info_id,
                    call_site,
                }
            } else {
                StackTrace::CallSite(call_site)
            };
            self.stack_trace.push(trace);
        } else {
            // If this is not user func, we are returning from one via a
            // continuation and should pop the stack frame:
            self.stack_trace.pop();
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
enum StackTrace {
    CallSite(Option<Span>),
    UserFuncCall {
        runtime: Gc<Runtime>,
        user_func_info_id: FunctionDebugInfoId,
        call_site: Option<Span>,
    },
}

#[derive(Clone, Debug, Trace)]
pub struct FunctionDebugInfo {
    /// The name of the function, or None if the function is a lambda
    // TODO(map): Make this an Arc<String> so we aren't constantly cloning strings.
    pub name: Option<String>,
    /// Named arguments for the function
    pub args: Vec<String>,
    /// Location of the function definition
    pub location: Span,
}

impl FunctionDebugInfo {
    pub fn new(name: Option<String>, args: Vec<String>, location: Span) -> Self {
        Self {
            name,
            args,
            location,
        }
    }

    pub fn from_bridge_fn(name: &'static str, debug_info: BridgeFnDebugInfo) -> Self {
        Self {
            name: Some(name.to_string()),
            args: debug_info.args.iter().map(|arg| arg.to_string()).collect(),
            location: Span {
                line: debug_info.line,
                column: debug_info.column as usize,
                offset: debug_info.offset,
                file: std::sync::Arc::new(debug_info.file.to_string()),
            },
        }
    }
}

pub fn apply<'a>(
    args: &'a [Gc<Value>],
    rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Gc<Value>>> {
    Box::pin(async move {
        if rest_args.is_empty() {
            return Err(Condition::wrong_num_of_args(2, args.len()).into());
        }
        let op = args[0].read();
        let op: &Closure = op.as_ref().try_into()?;
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
    })
}

inventory::submit! {
    BridgeFn::new(
        "apply",
        "(base)",
        1,
        true,
        apply,
        BridgeFnDebugInfo::new(
            "proc.rs",
            468,
            7,
            0,
            &[ "arg1", "args" ],
        )
    )
}

pub(crate) fn clone_continuation_env(
    value: &Gc<Value>,
    cloned: &mut HashMap<Gc<Value>, Gc<Value>>,
) -> Gc<Value> {
    if let Some(cloned) = cloned.get(value) {
        return cloned.clone();
    }
    let val_ref = value.read();
    match &*val_ref {
        Value::Closure(clos) => {
            let clos_cloned = Gc::new(Value::Closure(clos.clone()));
            cloned.insert(value.clone(), clos_cloned.clone());
            {
                let mut clos_mut = clos_cloned.write();
                let clos_cloned: &mut Closure = clos_mut.as_mut().try_into().unwrap();
                clos_cloned.deep_clone(cloned);
            }
            clos_cloned
        }
        val => {
            let val_cloned = Gc::new(val.clone());
            cloned.insert(value.clone(), val_cloned.clone());
            val_cloned
        }
    }
}

unsafe extern "C" fn call_consumer_with_values(
    _runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    // env[0] is the consumer
    let consumer = Gc::from_ptr(env.read());
    let consumer = {
        let consumer_ref = consumer.read();
        let consumer: &Closure = if let Ok(consumer) = consumer_ref.as_ref().try_into() {
            consumer
        } else {
            return Box::into_raw(Box::new(Err(Condition::invalid_operator_type(
                consumer_ref.type_name(),
            ))));
        };
        consumer.clone()
    };
    // env[1] is the continuation
    let cont = Gc::from_ptr(env.add(1).read());

    let mut collected_args: Vec<_> = (0..consumer.num_required_args)
        .map(|i| Gc::from_ptr(args.add(i).read()))
        .collect();

    // I hate this constant going back and forth from variadic to list. I have
    // to figure out a way to make it consistent
    if consumer.variadic {
        let rest_args = Gc::from_ptr(args.add(consumer.num_required_args).read());
        let mut vec = Vec::new();
        list_to_vec(&rest_args, &mut vec);
        collected_args.extend(vec);
    }

    collected_args.push(cont);

    Box::into_raw(Box::new(Ok(Application::new(
        consumer,
        collected_args,
        ExceptionHandler::from_ptr(exception_handler),
        dynamic_wind.as_ref().unwrap().clone(),
        None,
    ))))
}

pub fn call_with_values<'a>(
    args: &'a [Gc<Value>],
    _rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Gc<Value>>> {
    Box::pin(async move {
        let [producer, consumer] = args else {
            return Err(Condition::wrong_num_of_args(2, args.len()).into());
        };

        // Fetch the producer
        let producer = {
            let producer_ref = producer.read();
            let producer: &Closure = producer_ref.as_ref().try_into()?;
            producer.clone()
        };

        // Get the details of the consumer:
        let (num_required_args, variadic) = {
            let consumer_ref = consumer.read();
            let consumer: &Closure = consumer_ref.as_ref().try_into()?;
            (consumer.num_required_args, consumer.variadic)
        };

        let call_consumer_closure = Closure::new(
            producer.runtime.clone(),
            vec![consumer.clone(), cont.clone()],
            Vec::new(),
            FuncPtr::Continuation(call_consumer_with_values),
            num_required_args,
            variadic,
            None,
        );

        Ok(Application::new(
            producer,
            vec![Gc::new(Value::Closure(call_consumer_closure))],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

inventory::submit! {
    BridgeFn::new(
        "call-with-values",
        "(base)",
        2,
        false,
        call_with_values,
        BridgeFnDebugInfo::new(
            "proc.rs",
            587,
            7,
            0,
            &["producer", "consumer"]
        )
    )
}

#[derive(Clone, Default, Trace)]
pub struct DynamicWind {
    pub(crate) winders: Vec<(Closure, Closure)>,
}

pub fn dynamic_wind<'a>(
    args: &'a [Gc<Value>],
    _rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Gc<Value>>> {
    Box::pin(async move {
        let [in_thunk_gc, body_thunk, out_thunk_gc] = args else {
            return Err(Condition::wrong_num_of_args(3, args.len()).into());
        };
        let in_thunk = {
            let in_thunk_ref = in_thunk_gc.read();
            let in_thunk: &Closure = in_thunk_ref.as_ref().try_into()?;
            in_thunk.clone()
        };

        {
            let out_thunk_ref = out_thunk_gc.read();
            let _: &Closure = out_thunk_ref.as_ref().try_into()?;
        };

        {
            let body_thunk_ref = body_thunk.read();
            let _: &Closure = body_thunk_ref.as_ref().try_into()?;
        };

        let runtime = in_thunk.runtime.clone();

        let call_body_thunk_cont = Closure::new(
            runtime,
            vec![
                in_thunk_gc.clone(),
                body_thunk.clone(),
                out_thunk_gc.clone(),
                cont.clone(),
            ],
            Vec::new(),
            FuncPtr::Continuation(call_body_thunk),
            0,
            true,
            None,
        );

        Ok(Application::new(
            in_thunk,
            vec![Gc::new(Value::Closure(call_body_thunk_cont))],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

inventory::submit! {
    BridgeFn::new(
        "dynamic-wind",
        "(base)",
        3,
        false,
        dynamic_wind,
        BridgeFnDebugInfo::new(
            "proc.rs",
            0,
            0,
            0,
            &["in", "body", "out"]
        )
    )
}

pub(crate) unsafe extern "C" fn call_body_thunk(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    // env[0] is the in thunk
    let in_thunk = Gc::from_ptr(env.read());
    // env[1] is the body thunk
    let body_thunk: Closure = Gc::from_ptr(env.add(1).read()).try_into().unwrap();
    // env[2] is the out thunk
    let out_thunk = Gc::from_ptr(env.add(2).read());
    // env[3] is k, the continuation
    let k = Gc::from_ptr(env.add(3).read());

    let mut new_extent = dynamic_wind.as_ref().unwrap().clone();
    new_extent.winders.push((
        in_thunk.clone().try_into().unwrap(),
        out_thunk.clone().try_into().unwrap(),
    ));

    let cont = Closure::new(
        Gc::from_ptr(runtime),
        vec![out_thunk, k],
        Vec::new(),
        FuncPtr::Continuation(call_out_thunks),
        0,
        true,
        None,
    );

    let app = Application::new(
        body_thunk,
        vec![Gc::new(Value::Closure(cont))],
        ExceptionHandler::from_ptr(exception_handler),
        new_extent,
        None,
    );

    Box::into_raw(Box::new(Ok(app)))
}

pub(crate) unsafe extern "C" fn call_out_thunks(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    // env[0] is the out thunk
    let out_thunk: Closure = Gc::from_ptr(env.read()).try_into().unwrap();
    // env[1] is k, the remaining continuation
    let k = Gc::from_ptr(env.add(1).read());

    // args[0] is the result of the body thunk
    let body_thunk_res = Gc::from_ptr(args.read());

    let mut new_extent = dynamic_wind.as_ref().unwrap().clone();
    new_extent.winders.pop();

    let cont = Closure::new(
        Gc::from_ptr(runtime),
        vec![body_thunk_res, k],
        Vec::new(),
        FuncPtr::Continuation(forward_body_thunk_result),
        0,
        true,
        None,
    );

    let app = Application::new(
        out_thunk,
        vec![Gc::new(Value::Closure(cont))],
        ExceptionHandler::from_ptr(exception_handler),
        new_extent,
        None,
    );

    Box::into_raw(Box::new(Ok(app)))
}

unsafe extern "C" fn forward_body_thunk_result(
    _runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    // env[0] is the result of the body thunk
    let body_thunk_res = Gc::from_ptr(env.read());
    // env[1] is k, the continuation.
    let k: Closure = Gc::from_ptr(env.add(1).read()).try_into().unwrap();

    let mut args = Vec::new();
    list_to_vec(&body_thunk_res, &mut args);

    let app = Application::new(
        k,
        args,
        ExceptionHandler::from_ptr(exception_handler),
        dynamic_wind.as_ref().unwrap().clone(),
        None,
    );

    Box::into_raw(Box::new(Ok(app)))
}
