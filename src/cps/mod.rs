//! Closure-Passing Style
//!
//! Distinct but similar to Continuation-Passing Style, this representation is
//! the ultimate result of our parsing and compilation steps and the final step
//! before interpretation or compilation.
//!
//! There are two main reasons we choose this IR:
//! - Closure-Passing Style lets use build our continuations mechanically once,
//!   as opposed to creating them at runtime by hand in a process that is slow
//!   and error prone.
//! - Closure-Passing Style maps well to SSA, allowing us to compile functions
//!   directly to machine code.
//!

use crate::{
    ast::Literal,
    env::{Global, Local, Var},
    gc::Trace,
};
use std::{collections::HashSet, fmt, str::FromStr};

mod analysis;
mod codegen;
mod compile;

use analysis::AnalysisCache;
pub use compile::{Compile, TopLevelExpr};

#[derive(Clone)]
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
    Set,
    Add,
    Sub,
    Mul,
    Div,
    CloneClosure,
    CaptureEnvironment,
    GetCallTransformerFn,
    CallWithCurrentContinuation,
}

impl FromStr for PrimOp {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        match s {
            // Ignore these right now for testing
            /*
            "+" => Ok(Self::Add),
            "-" => Ok(Self::Sub),
            "/" => Ok(Self::Div),
            "set" => Ok(Self::Set),
             */
            "call/cc" | "call-with-current-continuation" => Ok(Self::CallWithCurrentContinuation),
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
        // + self.continuation.is_some() as usize
    }
}

#[derive(Debug, Clone)]
pub enum Cps {
    /// Generates a cell of type *const GcInner<Value>
    AllocCell(Local, Box<Cps>),
    /// Call to a primitive operator.
    PrimOp(PrimOp, Vec<Value>, Local, Box<Cps>),
    /// Function application.
    App(Value, Vec<Value>),
    /// Forward a list of values into an application.
    Forward(Value, Value),
    /// Branching.
    If(Value, Box<Cps>, Box<Cps>),
    /// Closure generation. The result of this operation is a *const Value::Closure
    Closure {
        args: ClosureArgs,
        body: Box<Cps>,
        val: Local,
        cexp: Box<Cps>,
        analysis: AnalysisCache,
    },
    // Temporary terminating value for debugging purposes (to be removed)
    ReturnValues(Value),
}
