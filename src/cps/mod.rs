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
use std::fmt;

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

mod analysis;
pub(crate) mod codegen;
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

    // List/pair operators:
    Cons,
    List,

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

    // Frame operatiors
    GetFrame,

    // Continuation mark operators:
    SetContinuationMark,

    // Macro expansion primitive operators:
    /// Matches the pattern against the syntax object, returning the bindings if
    /// it does and false otherwise.
    Matches,
    /// Expands a syntax template with the current set of bindings.
    ExpandTemplate,
    /// Raise an error indicating a failure to match the pattern.
    ErrorNoPatternsMatch,
}

#[derive(Debug, Clone)]
pub struct LambdaArgs {
    args: Vec<Local>,
    variadic: bool,
    continuation: Option<Local>,
}

impl LambdaArgs {
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

    fn num_required(&self) -> usize {
        self.args.len().saturating_sub(self.variadic as usize)
    }
}

#[derive(Debug, Clone)]
pub enum Cps {
    /// Call to a primitive operator:
    PrimOp(PrimOp, Vec<Value>, Local, Box<Cps>),

    /// Function application:
    App(Value, Vec<Value>),

    /// Branching:
    If(Value, Box<Cps>, Box<Cps>),

    /// Function creation:
    Lambda {
        args: LambdaArgs,
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
    // TODO: This could probably be improved by being a little smarter about
    // when we clear the uses cache (i.e. return a bool if any substitutions
    // occurred).
    fn substitute(
        &mut self,
        substitutions: &HashMap<Local, Value>,
        uses_cache: &mut HashMap<Local, HashMap<Local, usize>>,
    ) {
        match self {
            Self::PrimOp(_, args, val, cexp) => {
                substitute_values(args, substitutions);
                cexp.substitute(substitutions, uses_cache);
                uses_cache.remove(val);
            }
            Self::App(value, values) => {
                substitute_value(value, substitutions);
                substitute_values(values, substitutions);
            }
            Self::If(cond, success, failure) => {
                substitute_value(cond, substitutions);
                success.substitute(substitutions, uses_cache);
                failure.substitute(substitutions, uses_cache);
            }
            Self::Lambda {
                body, cexp, val, ..
            } => {
                body.substitute(substitutions, uses_cache);
                cexp.substitute(substitutions, uses_cache);
                uses_cache.remove(val);
            }
            Self::Halt(value) => {
                substitute_value(value, substitutions);
            }
        }
    }

    pub(crate) fn pretty_print(&self, indent: usize) {
        match self {
            Cps::PrimOp(PrimOp::Set, vals, _, cexp) => {
                eprintln!("{:>indent$}{:?} <- {:?};", "", vals[0], vals[1]);
                cexp.pretty_print(indent);
            }
            Cps::PrimOp(primop, vals, to, cexp) => {
                eprint!("{:>indent$}let {to:?} = {primop:?}", "");
                pretty_print_values(vals);
                eprintln!(";\n");
                cexp.pretty_print(indent);
            }
            Cps::Lambda {
                args,
                body,
                val,
                cexp,
                ..
            } => {
                eprint!("{:>indent$}def {val}(", "");
                for (i, arg) in args.args.iter().enumerate() {
                    if i > 0 {
                        eprint!(", ");
                    }
                    eprint!("{arg:?}");
                }
                if args.variadic {
                    eprint!("...")
                }
                if let Some(k) = args.continuation {
                    eprint!(", {k:?} k");
                }
                eprintln!("):");
                body.pretty_print(indent + 2);
                eprintln!("{:>indent$}end", "");
                cexp.pretty_print(indent);
            }
            Cps::If(val, succ, fail) => {
                eprintln!("{:>indent$}if {val:?} then", "");
                succ.pretty_print(indent + 2);
                eprintln!("{:>indent$}else", "");
                fail.pretty_print(indent + 2);
                eprintln!("{:>indent$}end", "");
            }
            Cps::App(val, args) => {
                eprint!("{:>indent$}{val:?}", "");
                pretty_print_values(args);
                eprintln!(";");
            }
            Cps::Halt(val) => {
                eprintln!("{:>indent$}Halt({val:?});", "");
            }
        }
    }
}

fn pretty_print_values(vals: &[Value]) {
    eprint!("(");
    for (i, val) in vals.iter().enumerate() {
        if i > 0 {
            eprint!(", ");
        }
        eprint!("{val:?}");
    }
    eprint!(")");
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
