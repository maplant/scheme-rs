//! Exceptional situations and conditions
use crate::{
    gc::Trace,
    syntax::{Identifier, Span},
};
use std::{error::Error as StdError, ops::Range};

#[derive(Debug, Clone, Trace)]
pub struct Exception {
    pub backtrace: Vec<Frame>,
    pub message: String,
    pub condition: Condition,
}

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
    pub fn error(err: String) -> Self {
        Self {
            backtrace: Vec::new(),
            message: err,
            condition: Condition::Error,
        }
    }

    pub fn syntax_error() -> Self {
        Self {
            backtrace: Vec::new(),
            message: "Invalid syntax".to_string(),
            condition: Condition::Syntax,
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
        Self::error(format!("Expected to receive {expected} unicode characters from transform, received {provided}"))
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
impl<E: StdError> From<E> for Exception {
    fn from(e: E) -> Self {
        Self::error(e.to_string())
    }
}

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
