//! Exceptional situations and conditions

use futures::future::BoxFuture;

use crate::{
    gc::{Gc, GcInner, Trace},
    proc::{Application, Closure, DynamicWind, FuncPtr},
    registry::{BridgeFn, BridgeFnDebugInfo},
    runtime::{Runtime, IGNORE_FUNCTION},
    syntax::{Identifier, Span},
    value::Value,
};
use std::{error::Error as StdError, fmt, ops::Range};

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
        Self::error(format!("Undefined variable {}", ident.name))
    }

    pub fn invalid_type(expected: &str, provided: &str) -> Self {
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

macro_rules! impl_into_condition_for {
    ($for:ty) => {
        impl From<$for> for Condition {
            fn from(e: $for) -> Self {
                Self::error(e.to_string())
            }
        }
    };
}
impl_into_condition_for!(crate::num::ArithmeticError);
impl_into_condition_for!(crate::num::NumberToUsizeError);
impl_into_condition_for!(std::num::TryFromIntError);

#[derive(Debug, Clone, Trace)]
pub struct Frame {
    pub proc: String,
    pub call_site_span: Option<Span>,
    // pub repeated: usize,
}

impl Frame {
    pub fn new(proc: String, call_site_span: Option<Span>) -> Self {
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
#[derive(Clone, Trace)]
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
    pub unsafe fn from_ptr(ptr: *mut GcInner<Self>) -> Option<Gc<Self>> {
        use std::ops::Not;
        ptr.is_null().not().then(|| unsafe { Gc::from_raw(ptr) })
    }
}

pub fn with_exception_handler<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    /*
    Box::pin(async move {
        let [handler, thunk] = args else {
            return Err(Condition::wrong_num_of_args(2, args.len()).into());
        };

        let handler_ref = handler.read();
        let handler: &Closure = handler_ref.as_ref().try_into()?;

        let thunk_ref = thunk.read();
        let thunk: &Closure = thunk_ref.as_ref().try_into()?;

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
     */
    todo!()
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
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    /*
    Box::pin(async move {
        let [condition] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };

        // TODO: Make condition non-continuable when it is re-raised

        let Some(ref handler) = exception_handler else {
            return Err(condition.clone());
        };

        let handler = handler.read().clone();

        Ok(Application::new(
            handler.curr_handler.clone(),
            vec![
                condition.clone(),
                Gc::new(Value::Closure(Closure::new(
                    handler.curr_handler.runtime.clone(),
                    vec![condition.clone(), cont.clone()],
                    Vec::new(),
                    FuncPtr::Continuation(reraise_exception),
                    0,
                    true,
                    Some(IGNORE_FUNCTION),
                ))),
            ],
            handler.prev_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
})
     */
    todo!()
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
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const i64,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    /*
    let runtime = Gc::from_raw(runtime);

    // env[0] is the exception
    let exception = Gc::from_raw(env.read());

    // env[1] is the continuation
    let cont = Gc::from_raw(env.add(1).read());

    Box::into_raw(Box::new(Ok(Application::new(
        Closure::new(
            runtime,
            Vec::new(),
            Vec::new(),
            FuncPtr::Bridge(raise),
            1,
            false,
            Some(IGNORE_FUNCTION),
        ),
        vec![exception, cont],
        ExceptionHandler::from_ptr(exception_handler),
        dynamic_wind.as_ref().unwrap().clone(),
        None,
))))
     */
    todo!()
}

/// Raises an exception to the current exception handler and coninues with the
/// value returned by the handler.
pub fn raise_continuable<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    /*
    Box::pin(async move {
        let [condition] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };

        let Some(ref handler) = exception_handler else {
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
     */
    todo!()
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
