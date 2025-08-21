//! Exceptional situations and conditions

use futures::future::BoxFuture;

use crate::{
    ast::ParseAstError,
    gc::{Gc, GcInner, Trace},
    lists,
    proc::{Application, Closure, DynamicWind, FuncPtr},
    registry::{BridgeFn, BridgeFnDebugInfo},
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    syntax::{Identifier, Span, Syntax},
    value::{UnpackedValue, Value},
};
use std::{error::Error as StdError, fmt, ops::Range, sync::Arc};

#[derive(Debug, Clone, Trace)]
pub struct Exception {
    pub backtrace: Vec<Frame>,
    pub obj: Value,
}

impl Exception {
    pub fn new(backtrace: Vec<Frame>, obj: Value) -> Self {
        Self { backtrace, obj }
    }
}

// TODO: This shouldn't be the display impl for Exception, I don' t think.
impl fmt::Display for Exception {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const MAX_BACKTRACE_LEN: usize = 20;
        writeln!(f, "Uncaught exception: {}", self.obj)?;
        if !self.backtrace.is_empty() {
            writeln!(f, "Stack trace:")?;
            for (i, frame) in self.backtrace.iter().rev().enumerate() {
                if i >= MAX_BACKTRACE_LEN {
                    writeln!(f, "(backtrace truncated)")?;
                    break;
                }
                writeln!(f, "{i}: {frame}")?;
            }
        }
        Ok(())
    }
}

impl StdError for Exception {}

#[derive(Debug, Clone, Trace)]
pub enum Condition {
    Condition,
    Message { message: String },
    Warning,
    Serious,
    Error,
    Violation,
    Assertion,
    NonContinuable,
    ImplementationRestriction,
    Lexical,
    Syntax { form: Value, subform: Value },
    Undefined,
    Irritants { irritants: Value },
    Who { who: Value },
    CompoundCondition { simple_conditions: Vec<Value> },
}

impl Condition {
    pub fn error(message: String) -> Self {
        Self::Message { message }
    }

    pub fn syntax_error(form: Syntax, subform: Option<Syntax>) -> Self {
        Self::Syntax {
            form: Value::from(form),
            subform: subform
                .map(Value::from)
                .unwrap_or_else(|| Value::from(false)),
        }
    }

    pub fn assert_eq_failed(expected: &str, actual: &str) -> Self {
        Self::error(format!(
            "Assertion failed, expected: {expected}, actual: {actual}"
        ))
    }

    pub fn undefined_variable(ident: Identifier) -> Self {
        Self::error(format!("Undefined variable {}", ident.sym))
    }

    pub fn invalid_type(expected: &str, provided: &str) -> Self {
        // panic!();
        Self::error(format!(
            "Expected value of type {expected}, provided {provided}"
        ))
    }

    pub fn invalid_operator_type(provided: &str) -> Self {
        Self::error(format!(
            "Invalid operator, expected procedure, provided {provided}"
        ))
    }

    pub fn invalid_index(index: usize, len: usize) -> Self {
        Self::error(format!(
            "Invalid index of {index} into collection of size {len}"
        ))
    }
    pub fn invalid_range(range: Range<usize>, len: usize) -> Self {
        Self::error(format!(
            "Invalid range of {range:?} into collection of size {len}"
        ))
    }

    pub fn wrong_num_of_unicode_chars(expected: usize, provided: usize) -> Self {
        Self::error(format!(
            "Expected to receive {expected} unicode characters from transform, received {provided}"
        ))
    }

    pub fn wrong_num_of_args(expected: usize, provided: usize) -> Self {
        Self::error(format!(
            "Expected {expected} arguments, provided {provided}"
        ))
    }
    pub fn wrong_num_of_variadic_args(expected: Range<usize>, provided: usize) -> Self {
        Self::error(format!(
            "Expected {expected:?} arguments, provided {provided}"
        ))
    }
}

impl From<Exception> for Condition {
    fn from(e: Exception) -> Self {
        // For now just drop the back trace:
        let Ok(v) = Gc::<Gc<dyn std::any::Any>>::try_from(e.obj) else {
            return Condition::Error;
        };
        let Ok(c) = v.read().clone().downcast::<Self>() else {
            return Condition::Error;
        };
        c.read().clone()
    }
}

impl From<ParseAstError> for Condition {
    fn from(value: ParseAstError) -> Self {
        Condition::error(format!("Error parsing: {value:?}"))
    }
}

macro_rules! impl_into_condition_for {
    ($for:ty) => {
        impl From<$for> for Condition {
            fn from(e: $for) -> Self {
                Self::error(e.to_string())
            }
        }
    };
}

impl_into_condition_for!(Box<crate::num::ArithmeticError>);
impl_into_condition_for!(crate::num::NumberToUsizeError);
impl_into_condition_for!(std::num::TryFromIntError);

#[derive(Debug, Clone, Trace)]
pub struct Frame {
    pub proc: Symbol,
    pub call_site_span: Option<Arc<Span>>,
    // pub repeated: usize,
}

impl Frame {
    pub fn new(proc: Symbol, call_site_span: Option<Arc<Span>>) -> Self {
        Self {
            proc,
            call_site_span,
            // repeated: 0,
        }
    }
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref call_site) = self.call_site_span {
            write!(f, "{} at {call_site}", self.proc)
        } else {
            write!(f, "{} at (unknown)", self.proc)
        }
    }
}

/// An exception handler includes the current handler - a function to call with
/// any condition that is raised - and the previous handler.
// TODO: We need to determine include the dynamic extent with the exception handler
// so that we can call the proper winders.
#[derive(Clone, Debug, Trace)]
pub struct ExceptionHandler {
    /// The previously installed handler. If the previously installed handler is
    /// None, we return the condition as an Error.
    prev_handler: Option<Gc<ExceptionHandler>>,
    /// The currently installed handler.
    curr_handler: Closure,
    /// The dynamic extent of the exception handler.
    dynamic_extent: DynamicWind,
}

impl ExceptionHandler {
    /// # Safety
    /// Exception handler must point to a valid Gc'd object.
    pub(crate) unsafe fn from_ptr(ptr: *mut GcInner<Self>) -> Option<Gc<Self>> {
        use std::ops::Not;
        ptr.is_null()
            .not()
            .then(|| unsafe { Gc::from_raw_inc_rc(ptr) })
    }
}

pub fn with_exception_handler<'a>(
    _env: &'a [Gc<Value>],
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let [handler, thunk] = args else {
            return Err(Condition::wrong_num_of_args(2, args.len()).into());
        };

        let handler: Closure = handler.clone().try_into()?;
        let thunk: Closure = thunk.clone().try_into()?;

        let exception_handler = ExceptionHandler {
            prev_handler: exception_handler.clone(),
            curr_handler: handler.clone(),
            dynamic_extent: dynamic_wind.clone(),
        };

        Ok(Application::new(
            thunk.clone(),
            vec![cont.clone()],
            Some(Gc::new(exception_handler)),
            dynamic_wind.clone(),
            None,
        ))
    })
}

inventory::submit! {
    BridgeFn::new(
        "with-exception-handler",
        "(rnrs base builtins (6))",
        2,
        false,
        with_exception_handler,
        BridgeFnDebugInfo::new(
            "exception.rs",
            182,
            7,
            0,
            &[ "handler", "thunk" ],
        )
    )
}

/// Raises a non-continuable exception to the current exception handler.
pub fn raise<'a>(
    _env: &'a [Gc<Value>],
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let [raised] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };

        let (parent_wind, handler, parent_handler) =
            if let Some(exception_handler) = exception_handler {
                let handler = exception_handler.read();
                (
                    handler.dynamic_extent.clone(),
                    Value::from(handler.curr_handler.clone()),
                    handler.prev_handler.clone(),
                )
            } else {
                (DynamicWind::default(), Value::from(false), None)
            };

        let thunks = exit_winders(dynamic_wind, &parent_wind);
        let cont: Closure = cont.clone().try_into().unwrap();

        let calls = Closure::new(
            cont.get_runtime(),
            vec![
                Gc::new(thunks),
                Gc::new(raised.clone()),
                Gc::new(handler),
                Gc::new(Value::from(cont.clone())),
            ],
            Vec::new(),
            FuncPtr::Continuation(call_exits_and_exception_handler_reraise),
            0,
            false,
            None,
        );

        Ok(Application::new(
            calls,
            Vec::new(),
            parent_handler,
            parent_wind,
            None,
        ))
    })
}

inventory::submit! {
    BridgeFn::new(
        "raise",
        "(rnrs base builtins (6))",
        1,
        false,
        raise,
        BridgeFnDebugInfo::new(
            "exception.rs",
            231,
            7,
            0,
            &["condition"],
        )
    )
}

unsafe extern "C" fn call_exits_and_exception_handler_reraise(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    unsafe {
        let runtime = Runtime::from_raw_inc_rc(runtime);

        // env[0] are the thunks:
        let thunks = Gc::from_raw_inc_rc(env.read()).read().clone();

        // env[1] is the raised value:
        let raised = Gc::from_raw_inc_rc(env.add(1).read()).read().clone();

        // env[2] is the next exception handler
        let curr_handler = Gc::from_raw_inc_rc(env.add(2).read()).read().clone();

        // env[3] is the current continuation;
        let k = Gc::from_raw_inc_rc(env.add(3).read());

        let app = match thunks.unpack() {
            UnpackedValue::Pair(pair) => {
                let lists::Pair(head_thunk, tail) = &*pair.read();
                let head_thunk: Closure = head_thunk.clone().try_into().unwrap();
                let cont = Closure::new(
                    head_thunk.get_runtime(),
                    vec![
                        Gc::new(tail.clone()),
                        Gc::new(raised),
                        Gc::new(curr_handler),
                        k,
                    ],
                    Vec::new(),
                    FuncPtr::Continuation(call_exits_and_exception_handler_reraise),
                    0,
                    false,
                    None,
                );
                Application::new(
                    head_thunk,
                    vec![Value::from(cont)],
                    ExceptionHandler::from_ptr(exception_handler),
                    dynamic_wind.as_ref().unwrap().clone(),
                    None,
                )
            }
            UnpackedValue::Null => {
                // If the exception handler is null, we want to return it as an
                // error.
                if !curr_handler.is_true() {
                    let app = Application::new(
                        Closure::new(
                            runtime,
                            Vec::new(),
                            Vec::new(),
                            FuncPtr::Bridge(return_error),
                            1,
                            false,
                            None,
                        ),
                        vec![raised.clone(), k.read().clone()],
                        ExceptionHandler::from_ptr(exception_handler),
                        dynamic_wind.as_ref().unwrap().clone(),
                        None,
                    );
                    return Box::into_raw(Box::new(Ok(app)));
                }

                let curr_handler: Closure = curr_handler.try_into().unwrap();

                Application::new(
                    curr_handler,
                    vec![
                        raised.clone(),
                        Value::from(Closure::new(
                            runtime,
                            vec![Gc::new(raised), k],
                            Vec::new(),
                            FuncPtr::Continuation(reraise_exception),
                            0,
                            true,
                            None,
                        )),
                    ],
                    ExceptionHandler::from_ptr(exception_handler),
                    dynamic_wind.as_ref().unwrap().clone(),
                    None,
                )
            }
            _ => unreachable!(),
        };

        Box::into_raw(Box::new(Ok(app)))
    }
}

fn return_error<'a>(
    _env: &'a [Gc<Value>],
    args: &'a [Value],
    _rest_args: &'a [Value],
    _cont: &'a Value,
    _exception_handler: &'a Option<Gc<ExceptionHandler>>,
    _dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let [raised] = args else { unreachable!() };
        return Err(raised.clone());
    })
}

unsafe extern "C" fn reraise_exception(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    unsafe {
        let runtime = Runtime(Gc::from_raw_inc_rc(runtime));

        // env[0] is the exception
        let exception = Gc::from_raw_inc_rc(env.read());
        let exception = exception.read().clone();

        // env[1] is the continuation
        let cont = Gc::from_raw_inc_rc(env.add(1).read());
        let cont = cont.read().clone();

        Box::into_raw(Box::new(Ok(Application::new(
            Closure::new(
                runtime,
                Vec::new(),
                Vec::new(),
                FuncPtr::Bridge(raise),
                1,
                false,
                None,
            ),
            vec![exception, cont],
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            None,
        ))))
    }
}

/// Raises an exception to the current exception handler and coninues with the
/// value returned by the handler.
pub fn raise_continuable<'a>(
    _env: &'a [Gc<Value>],
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let [condition] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };

        let Some(handler) = exception_handler else {
            return Err(condition.clone());
        };

        let handler = handler.read().clone();

        Ok(Application::new(
            handler.curr_handler,
            vec![condition.clone(), cont.clone()],
            handler.prev_handler,
            dynamic_wind.clone(),
            None,
        ))
    })
}

inventory::submit! {
    BridgeFn::new(
        "raise-continuable",
        "(rnrs base builtins (6))",
        1,
        false,
        raise_continuable,
        BridgeFnDebugInfo::new(
            "exception.rs",
            326,
            7,
            0,
            &["condition"],
        )
    )
}

pub fn exit_winders(from_extent: &DynamicWind, to_extent: &DynamicWind) -> Value {
    let mut from_winders = from_extent.winders.as_slice();
    let mut to_winders = to_extent.winders.as_slice();

    while let Some((to_first, to_remaining)) = to_winders.split_first() {
        let Some((from_first, from_remaining)) = from_winders.split_first() else {
            return Value::null();
        };

        if !Gc::ptr_eq(&from_first.1.0, &to_first.1.0) {
            break;
        }

        from_winders = from_remaining;
        to_winders = to_remaining;
    }

    let mut thunks = Value::null();
    for thunk in from_winders.iter() {
        thunks = Value::from((Value::from(thunk.1.clone()), thunks));
    }

    thunks
}
