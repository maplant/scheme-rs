//! Continuation-Passing Style
//!
//! Our mid-level representation for scheme code that ultimately gets translated
//! into LLVM SSA for JIT compilation. This representation is the ultimate
//! result of our parsing and compilation steps and the final step before JIT
//! compilation.
//!
//! There are two main reasons we choose this IR:
//! - Continuation-Passing Style lets use build our continuations mechanically
//!   once, as opposed to creating them at runtime by hand in a process that is
//!   slow and error prone.
//! - Continuation-Passing Style maps well to SSA, allowing us to compile functions
//!   directly to machine code.

use crate::{
    ast::Literal,
    env::{Global, Local, Var},
    gc::Trace,
};
use std::{
    collections::{HashMap, HashSet},
    fmt,
    str::FromStr,
};

mod analysis;
mod codegen;
mod compile;
mod reduce;

pub use compile::Compile;

#[derive(Clone, PartialEq)]
pub enum Value {
    Var(Var),
    Literal(Literal),
}

impl Value {
    fn to_local(&self) -> Option<Local> {
        if let Self::Var(Var::Local(local)) = self {
            Some(*local)
        } else {
            None
        }
    }

    fn to_global(&self) -> Option<Global> {
        if let Self::Var(Var::Global(global)) = self {
            Some(global.clone())
        } else {
            None
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::Literal(Literal::Boolean(b))
    }
}

impl From<Var> for Value {
    fn from(var: Var) -> Self {
        Self::Var(var)
    }
}

impl From<Local> for Value {
    fn from(local: Local) -> Self {
        Self::Var(Var::Local(local))
    }
}

impl From<Global> for Value {
    fn from(global: Global) -> Self {
        Self::Var(Var::Global(global))
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(var) => var.fmt(f),
            Self::Literal(lit) => lit.fmt(f),
        }
    }
}

#[derive(Copy, Clone, Debug, Trace)]
pub enum PrimOp {
    /// Set cell value:
    Set,

    // Math primitive operators:
    Add,
    Sub,
    Mul,
    Div,

    // Macro expansion primitive operators:
    CaptureEnvironment,
    GetCallTransformerFn,

    // Continuation primitive operators:
    CloneClosure,
    CallWithCurrentContinuation,
}

impl FromStr for PrimOp {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        match s {
            "&call/cc" => Ok(Self::CallWithCurrentContinuation),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClosureArgs {
    args: Vec<Local>,
    variadic: bool,
    continuation: Option<Local>,
}

impl ClosureArgs {
    fn new(args: Vec<Local>, variadic: bool, continuation: Option<Local>) -> Self {
        Self {
            args,
            variadic,
            continuation,
        }
    }

    fn to_vec(&self) -> Vec<Local> {
        self.args
            .clone()
            .into_iter()
            .chain(self.continuation)
            .collect()
    }

    fn num_required(&self) -> usize {
        self.args.len().saturating_sub(self.variadic as usize)
    }
}

#[derive(derive_more::Debug, Clone)]
pub enum Cps {
    /// Generates a cell of type `*const GcInner<Value>`
    AllocCell(Local, Box<Cps>),

    /// Call to a primitive operator.
    PrimOp(PrimOp, Vec<Value>, Local, Box<Cps>),

    /// Function application.
    App(Value, Vec<Value>),

    /// Forward a list of values into an application.
    // TODO: I'm not sure I like this name
    Forward(Value, Value),

    /// Branching.
    If(Value, Box<Cps>, Box<Cps>),

    /// Closure generation. The result of this operation is a *const Value::Closure
    Closure {
        args: ClosureArgs,
        body: Box<Cps>,
        val: Local,
        cexp: Box<Cps>,
    },

    /// Halt execution and return the values
    Halt(Value),
}

impl Cps {
    /// Perform substitutions on local variables.
    fn substitute(&mut self, substitutions: &HashMap<Local, Value>) {
        match self {
            Self::AllocCell(_, cexp) => {
                cexp.substitute(substitutions);
            }
            Self::PrimOp(_, args, _, cexp) => {
                substitute_values(args, substitutions);
                cexp.substitute(substitutions);
            }
            Self::App(value, values) => {
                substitute_value(value, substitutions);
                substitute_values(values, substitutions);
            }
            Self::Forward(op, arg) => {
                substitute_value(op, substitutions);
                substitute_value(arg, substitutions);
            }
            Self::If(cond, success, failure) => {
                substitute_value(cond, substitutions);
                success.substitute(substitutions);
                failure.substitute(substitutions);
            }
            Self::Closure { body, cexp, .. } => {
                body.substitute(substitutions);
                cexp.substitute(substitutions);
            }
            Self::Halt(value) => {
                substitute_value(value, substitutions);
            }
        }
    }
}

fn substitute_value(value: &mut Value, substitutions: &HashMap<Local, Value>) {
    if let Value::Var(Var::Local(local)) = value {
        if let Some(substitution) = substitutions.get(local) {
            *value = substitution.clone();
        }
    }
}

fn substitute_values(values: &mut [Value], substitutions: &HashMap<Local, Value>) {
    values
        .iter_mut()
        .for_each(|value| substitute_value(value, substitutions))
}
