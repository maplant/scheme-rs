//! Exceptional situations and conditions

use futures::future::BoxFuture;

use crate::{
    gc::{Gc, GcInner, Trace},
    proc::{Application, Closure, FuncPtr},
    registry::BridgeFn,
    runtime::Runtime,
    syntax::{Identifier, Span},
    value::Value,
};
use std::{error::Error as StdError, fmt, ops::Range};

#[derive(Debug, Clone, Trace)]
pub struct Exception {
    pub backtrace: Vec<Frame>,
    pub message: String,
    pub condition: Condition,
    pub obj: Gc<Value>,
}
impl fmt::Display for Exception {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl StdError for Exception {}

#[derive(Debug, Clone, Trace)]
pub enum Condition {
    Condition,
    Warning,
    Serious,
    Error,
    Violation,
    Assertion,
    NonContinuable,
    ImplementationRestriction,
    Lexical,
    Syntax,
    Undefined,
    Message,
    Irritants,
    Who,
}

impl Exception {
    pub fn from_value(obj: Gc<Value>) -> Self {
        Self {
            backtrace: Vec::new(),
            message: "Todo".to_string(),
            condition: Condition::Error,
            obj,
        }
    }

    pub fn error(err: String) -> Self {
        Self {
            backtrace: Vec::new(),
            message: err,
            condition: Condition::Error,
            obj: Gc::new(Value::Null),
        }
    }

    pub fn syntax_error() -> Self {
        Self {
            backtrace: Vec::new(),
            message: "Invalid syntax".to_string(),
            condition: Condition::Syntax,
            obj: Gc::new(Value::Null),
        }
    }

    pub fn division_by_zero() -> Self {
        Self::error("division by zero".to_string())
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
macro_rules! impl_into_exception_for {
    ($for:ty) => {
        impl From<$for> for Exception {
            fn from(e: $for) -> Self {
                Self::error(e.to_string())
            }
        }
    };
}
impl_into_exception_for!(std::num::TryFromIntError);

#[derive(Debug, Clone, Trace)]
pub struct Frame {
    pub proc: String,
    pub span: Span,
    pub repeated: usize,
}

impl Frame {
    pub fn new(proc: String, span: Span) -> Self {
        Self {
            proc,
            span,
            repeated: 0,
        }
    }
}

/// An exception handler includes the current handler - a function to call with
/// any condition that is raised - and the previous handler.
#[derive(Clone, Trace)]
pub struct ExceptionHandler {
    /// The previously installed handler. If the previously installed handler is
    /// None, we return the condition as an Error.
    prev_handler: Option<Gc<ExceptionHandler>>,
    /// The currently installed handler.
    curr_handler: Closure,
}

pub fn with_exception_handler<'a>(
    args: &'a [Gc<Value>],
    _rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
) -> BoxFuture<'a, Result<Application, Exception>> {
    Box::pin(async move {
        let [handler, thunk] = args else {
            return Err(Exception::wrong_num_of_args(2, args.len()));
        };

        let handler_ref = handler.read();
        let handler: &Closure = handler_ref.as_ref().try_into()?;

        let thunk_ref = thunk.read();
        let thunk: &Closure = thunk_ref.as_ref().try_into()?;

        let exception_handler = ExceptionHandler {
            prev_handler: exception_handler.clone(),
            curr_handler: handler.clone(),
        };

        Ok(Application::new(
            thunk.clone(),
            vec![cont.clone()],
            Some(Gc::new(exception_handler)),
        ))
    })
}

inventory::submit! {
    BridgeFn::new("with-exception-handler", "(base)", 2, false, with_exception_handler)
}

/// Raises a non-continuable exception to the current exception handler.
pub fn raise<'a>(
    args: &'a [Gc<Value>],
    _rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
) -> BoxFuture<'a, Result<Application, Exception>> {
    Box::pin(async move {
        let [condition] = args else {
            return Err(Exception::wrong_num_of_args(1, args.len()));
        };

        let Some(ref handler) = exception_handler else {
            return Err(Exception::from_value(condition.clone()));
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
                    false,
                ))),
            ],
            handler.prev_handler.clone(),
        ))
    })
}

inventory::submit! {
    BridgeFn::new("raise", "(base)", 1, false, raise)
}

unsafe extern "C" fn reraise_exception(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
) -> *mut Application {
    let runtime = Gc::from_ptr(runtime);

    // env[0] is the exception
    let exception = Gc::from_ptr(env.read());

    // env[1] is the continuation
    let cont = Gc::from_ptr(env.add(1).read());

    let curr_handler = if exception_handler.is_null() {
        None
    } else {
        Some(Gc::from_ptr(exception_handler))
    };

    Box::into_raw(Box::new(Application::new(
        Closure::new(
            runtime,
            Vec::new(),
            Vec::new(),
            FuncPtr::Bridge(raise),
            1,
            false,
            false,
        ),
        vec![exception, cont],
        curr_handler,
    )))
}

/// Raises an exception to the current exception handler and coninues with the
/// value returned by the handler.
pub fn raise_continuable<'a>(
    args: &'a [Gc<Value>],
    _rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
) -> BoxFuture<'a, Result<Application, Exception>> {
    Box::pin(async move {
        let [condition] = args else {
            return Err(Exception::wrong_num_of_args(1, args.len()));
        };

        let Some(ref handler) = exception_handler else {
            return Err(Exception::from_value(condition.clone()));
        };

        let handler = handler.read().clone();

        Ok(Application::new(
            handler.curr_handler,
            vec![condition.clone(), cont.clone()],
            handler.prev_handler,
        ))
    })
}

inventory::submit! {
    BridgeFn::new("raise-continuable", "(base)", 1, false, raise_continuable)
}
