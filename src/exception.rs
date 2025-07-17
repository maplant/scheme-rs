//! Exceptional situations and conditions

use futures::future::BoxFuture;

use crate::{
    ast::ParseAstError,
    gc::{Gc, GcInner, Trace},
    proc::{Application, Closure, DynamicWind, FuncPtr},
    registry::{BridgeFn, BridgeFnDebugInfo},
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    syntax::{Identifier, Span},
    value::Value,
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

    pub fn syntax_error() -> Self {
        // TODO: Expand on these
        Self::Syntax {
            form: Value::null(),
            subform: Value::from(false),
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
    fn from(_value: ParseAstError) -> Self {
        // TODO: Make this more descriptive
        Self::syntax_error()
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
    curr_handler: Gc<Closure>,
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
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    _env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let [handler, thunk] = args else {
            return Err(Condition::wrong_num_of_args(2, args.len()).into());
        };

        let handler: Gc<Closure> = handler.clone().try_into()?;

        let thunk: Gc<Closure> = thunk.clone().try_into()?;

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
        "(base)",
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
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    _env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let [condition] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };

        // TODO: Make condition non-continuable when it is re-raised

        let Some(handler) = exception_handler else {
            return Err(condition.clone());
        };

        let handler = handler.read();
        let runtime = {
            let curr_handler = handler.curr_handler.read();
            curr_handler.runtime.clone()
        };

        Ok(Application::new(
            handler.curr_handler.clone(),
            vec![
                condition.clone(),
                Value::from(Closure::new(
                    runtime,
                    vec![Gc::new(condition.clone()), Gc::new(cont.clone())],
                    Vec::new(),
                    FuncPtr::Continuation(reraise_exception),
                    0,
                    true,
                    None,
                )),
            ],
            handler.prev_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

inventory::submit! {
    BridgeFn::new(
        "raise",
        "(base)",
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
            Gc::new(Closure::new(
                runtime,
                Vec::new(),
                Vec::new(),
                FuncPtr::Bridge(raise),
                1,
                false,
                None,
            )),
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
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    _env: &'a [Gc<Value>],
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
        "(base)",
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

/*
pub fn winders(from_extent: &DynamicWind, to_extent: &DynamicWind) -> Gc<Value> {
    let len = from_extent.winders.len().min(to_extent.winders.len());

    let mut split_point = 0;
    for i in 0..len {
        if from_extent.winders[i].0 == to_extent.winders[i].0 {
            split_point = i + 1;
        } else {
            break;
        }
    }

    let (_, to_extent) = to_extent.winders.split_at(split_point);

    let mut thunks = Gc::new(Value::Null);
    for thunk in to_extent
        .iter()
        .map(|to_extent| {
            to_extent.1.clone()
        })
        .rev()
    {
        thunks = Gc::new(Value::Pair(Gc::new(Value::Closure(thunk)), thunks));
    }

    thunks
}
*/
