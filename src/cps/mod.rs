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
    env::{Global, Local, Var},
    gc::Trace,
    symbols::Symbol,
    syntax::Span,
    value::Value as RuntimeValue,
};
use std::{
    collections::{HashMap, HashSet},
    fmt,
};

mod analysis;
mod codegen;
mod compile;
mod reduce;

pub use compile::Compile;

#[derive(Clone, PartialEq)]
pub enum Value {
    Var(Var),
    Const(RuntimeValue),
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

impl From<RuntimeValue> for Value {
    fn from(v: RuntimeValue) -> Self {
        Self::Const(v)
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
            Self::Const(val) => val.fmt(f),
        }
    }
}

#[derive(Copy, Clone, Debug, Trace)]
pub enum PrimOp {
    /// Set cell value:
    Set,

    // Cell operations:
    /// Allocate a cell, returning a Gc<Value>:
    AllocCell,

    // List operators:
    Cons,

    // Math primitive operators:
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,

    // Macro expansion primitive operators:
    CaptureEnvironment,
    GetCallTransformerFn,

    // Continuation primitive operators:
    CallWithCurrentContinuation,
    /// Converts a continuation to a callable user function
    PrepareContinuation,
    /// Extract the current winders from the environment into a value
    ExtractWinders,
}

impl PrimOp {
    pub fn from_sym(s: Symbol) -> Option<Self> {
        match s.to_str().as_ref() {
            "&call/cc" => Some(Self::CallWithCurrentContinuation),
            _ => None,
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
    pub fn new(args: Vec<Local>, variadic: bool, continuation: Option<Local>) -> Self {
        Self {
            args,
            variadic,
            continuation,
        }
    }

    fn iter_mut(&mut self) -> impl Iterator<Item = &mut Local> {
        self.args.iter_mut().chain(self.continuation.as_mut())
    }

    fn iter(&self) -> impl Iterator<Item = &Local> {
        self.args.iter().chain(self.continuation.as_ref())
    }

    fn to_vec(&self) -> Vec<Local> {
        self.iter().copied().collect()
    }

    fn num_required(&self) -> usize {
        self.args.len().saturating_sub(self.variadic as usize)
    }
}

#[derive(derive_more::Debug, Clone)]
pub enum Cps {
    /// Call to a primitive operator:
    PrimOp(PrimOp, Vec<Value>, Local, Box<Cps>),

    /// Function application:
    App(Value, Vec<Value>, Option<Span>),

    /// Forward a list of values into an application:
    // TODO: I think we can get rid of this with better primitive operators, maybe.
    Forward(Value, Value),

    /// Branching:
    If(Value, Box<Cps>, Box<Cps>),

    /// Function creation:
    Lambda {
        args: ClosureArgs,
        body: Box<Cps>,
        val: Local,
        cexp: Box<Cps>,
        span: Option<Span>,
    },

    /// Halt execution and return the values:
    Halt(Value),
}

impl Cps {
    /// Perform substitutions on local variables.
    fn substitute(&mut self, substitutions: &HashMap<Local, Value>) {
        match self {
            Self::PrimOp(_, args, _, cexp) => {
                substitute_values(args, substitutions);
                cexp.substitute(substitutions);
            }
            Self::App(value, values, _) => {
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
            Self::Lambda { body, cexp, .. } => {
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
    if let Some(local) = value.to_local()
        && let Some(substitution) = substitutions.get(&local)
    {
        *value = substitution.clone();
    }
}

fn substitute_values(values: &mut [Value], substitutions: &HashMap<Local, Value>) {
    values
        .iter_mut()
        .for_each(|value| substitute_value(value, substitutions))
}
