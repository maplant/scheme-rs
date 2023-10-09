use std::sync::Arc;
use derivative::Derivative;
use crate::{
    gc::Gc,
    value::Value,
    compile::CompileError,
    syntax::{Identifier, Span}, continuation::Continuation,
};

// TODO: Rename this to condition to more accurately reflect its purpose
#[derive(Debug)]
pub struct RuntimeError {
    pub backtrace: Vec<Frame>,
    pub kind: RuntimeErrorKind,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub enum RuntimeErrorKind {
    UndefinedVariable(Identifier),
    InvalidType {
        expected: String,
        provided: String,
    },
    InvalidOperatorType {
        provided: String,
    },
    WrongNumberOfArguments {
        expected: usize,
        provided: usize,
    },
    DivisionByZero,
    CompileError(CompileError),
    AbandonCurrentContinuation {
        #[derivative(Debug="ignore")]
        args: Vec<Gc<Value>>,
        #[derivative(Debug="ignore")]
        new_cont: Option<Arc<Continuation>>
    },
    Condition {
        // TODO
    },
}

#[derive(Debug)]
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

impl RuntimeError {
    pub fn push_frame(&mut self, proc: String, span: Span) {
        match self.backtrace.last_mut() {
            Some(last_frame) if last_frame.span == span && last_frame.proc == proc => {
                last_frame.repeated += 1
            }
            _ => self.backtrace.push(Frame::new(proc, span)),
        }
    }

    pub fn abandon_current_continuation(args: Vec<Gc<Value>>, new_cont: Option<Arc<Continuation>>) -> Self {
        Self {
            backtrace: Vec::new(),
            kind: RuntimeErrorKind::AbandonCurrentContinuation { args, new_cont }
        }
    }

    pub fn division_by_zero() -> Self {
        Self {
            backtrace: Vec::new(),
            kind: RuntimeErrorKind::DivisionByZero,
        }
    }

    pub fn undefined_variable(ident: Identifier) -> Self {
        Self {
            backtrace: Vec::new(),
            kind: RuntimeErrorKind::UndefinedVariable(ident),
        }
    }

    pub fn invalid_type(expected: &str, provided: &str) -> Self {
        let expected = expected.to_string();
        let provided = provided.to_string();
        Self {
            backtrace: Vec::new(),
            kind: RuntimeErrorKind::InvalidType { expected, provided },
        }
    }

    pub fn invalid_operator_type(provided: &str) -> Self {
        let provided = provided.to_string();
        Self {
            backtrace: Vec::new(),
            kind: RuntimeErrorKind::InvalidOperatorType { provided },
        }
    }

    pub fn wrong_num_of_args(expected: usize, provided: usize) -> Self {
        Self {
            backtrace: Vec::new(),
            kind: RuntimeErrorKind::WrongNumberOfArguments { expected, provided },
        }
    }
}

impl From<CompileError> for RuntimeError {
    fn from(ce: CompileError) -> Self {
        Self {
            backtrace: Vec::new(),
            kind: RuntimeErrorKind::CompileError(ce),
        }
    }
}

/*
impl fmt::Debug for RuntimeError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}
*/
