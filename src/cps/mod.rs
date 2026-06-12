//! Continuation-Passing Style
//!
//! Our mid-level representation for scheme code that ultimately gets translated
//! into Cranelift SSA for JIT compilation. This representation is the ultimate
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
    cps::analysis::Uses,
    env::{Global, Local, Var},
    gc::Trace,
    symbols::Symbol,
    syntax::Span,
    value::Value as RuntimeValue,
};
use std::fmt;

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

pub(crate) mod analysis;
pub(crate) mod codegen;
pub(crate) mod compile;
mod contify;
mod reduce;

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
    // Cell operations:
    /// Set cell value:
    Set,
    /// Read a cell value (or return the value):
    Read,
    /// Allocate a cell:
    AllocCell,

    /// Call a known function that returns no values:
    CallKnown0,
    /// Call a known function that returns one value:
    CallKnown1,

    // List/pair operators:
    Car,
    Cdr,
    Cons,
    List,
    IsNull,
    IsPair,

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

    // Boolean operators:
    Not,

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

impl PrimOp {
    pub(crate) fn info(&self) -> PrimOpInfo {
        match self {
            Self::Set => PrimOpInfo::new(2, false, false, false),
            Self::Read => PrimOpInfo::new(1, false, false, false),
            Self::AllocCell => PrimOpInfo::new(0, false, false, true),
            Self::CallKnown0 => PrimOpInfo::new(1, false, true, false),
            Self::CallKnown1 => PrimOpInfo::new(0, false, true, true),
            Self::Car => PrimOpInfo::new(1, false, true, true),
            Self::Cdr => PrimOpInfo::new(1, false, true, true),
            Self::Cons => PrimOpInfo::new(2, false, false, true),
            Self::List => PrimOpInfo::new(0, true, false, true),
            Self::IsNull => PrimOpInfo::new(1, false, false, false),
            Self::IsPair => PrimOpInfo::new(1, false, false, false),
            Self::Add => PrimOpInfo::new(0, true, true, true),
            Self::Mul => PrimOpInfo::new(0, true, true, true),
            Self::Sub => PrimOpInfo::new(1, true, true, true),
            Self::Div => PrimOpInfo::new(1, true, true, true),
            Self::Equal => PrimOpInfo::new(1, true, true, false),
            Self::Greater => PrimOpInfo::new(1, true, true, false),
            Self::GreaterEqual => PrimOpInfo::new(1, true, true, false),
            Self::Lesser => PrimOpInfo::new(1, true, true, false),
            Self::LesserEqual => PrimOpInfo::new(1, true, true, false),
            Self::Not => PrimOpInfo::new(1, false, false, false),
            Self::GetFrame => PrimOpInfo::new(1, false, false, true),
            Self::SetContinuationMark => PrimOpInfo::new(2, false, false, false),
            Self::Matches => PrimOpInfo::new(2, false, false, false),
            Self::ExpandTemplate => PrimOpInfo::new(4, false, true, true),
            Self::ErrorNoPatternsMatch => PrimOpInfo::new(0, false, false, false),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct PrimOpInfo {
    pub required: usize,
    pub variadic: bool,
    pub can_error: bool,
    pub needs_drop: bool,
}

impl PrimOpInfo {
    fn new(required: usize, variadic: bool, can_error: bool, needs_drop: bool) -> Self {
        Self {
            required,
            variadic,
            can_error,
            needs_drop,
        }
    }

    pub(crate) fn matches_args(&self, num: usize) -> bool {
        if self.variadic {
            num >= self.required
        } else {
            num == self.required
        }
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

    /// Mutually-recursive function definitions:
    Fix(Vec<LambdaBinding>, Box<Cps>),

    /// Halt execution and return the values:
    Halt(Value),
}

#[derive(Debug, Clone)]
pub struct LambdaBinding {
    args: LambdaArgs,
    body: Box<Cps>,
    val: Local,
    span: Option<Span>,
}

impl LambdaBinding {
    fn is_continuation(&self) -> bool {
        self.args.continuation.is_none()
    }

    fn is_func(&self) -> bool {
        self.args.continuation.is_some()
    }
}

#[derive(Debug, Clone)]
pub struct LambdaArgs {
    continuation: Option<Local>,
    args: Vec<Local>,
    variadic: bool,
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
        self.continuation
            .as_mut()
            .into_iter()
            .chain(self.args.iter_mut())
    }

    fn iter(&self) -> impl Iterator<Item = &Local> {
        self.continuation
            .as_ref()
            .into_iter()
            .chain(self.args.iter())
    }

    fn num_required(&self) -> usize {
        self.args.len().saturating_sub(self.variadic as usize)
    }

    pub(crate) fn matches_args(&self, num: usize) -> bool {
        // `num` is the length of an application's argument list, which includes
        // the continuation as its first element. Count the continuation here so
        // a well-formed call is not mistaken for a wrong-arity (escaping) one.
        let params = self.args.len() + self.continuation.is_some() as usize;
        if self.variadic {
            num > params
        } else {
            num == params
        }
    }
}

impl Cps {
    /// Take ownership of and modify the term, replacing it
    fn update_term(&mut self, updater: impl FnOnce(Cps) -> Cps) {
        let old_term = std::mem::replace(self, Cps::Halt(Value::from(RuntimeValue::undefined())));
        *self = (updater)(old_term);
    }

    /// Perform substitutions on local variables.
    // TODO: This could probably be improved by being a little smarter about
    // when we clear the uses cache (i.e. return a bool if any substitutions
    // occurred).
    fn substitute(&mut self, substitutions: &HashMap<Local, Value>, uses: &mut Uses) {
        match self {
            Self::PrimOp(_, args, val, cexp) => {
                substitute_values(args, substitutions);
                cexp.substitute(substitutions, uses);
                uses.remove(val);
            }
            Self::App(value, values) => {
                substitute_value(value, substitutions);
                substitute_values(values, substitutions);
            }
            Self::If(cond, success, failure) => {
                substitute_value(cond, substitutions);
                success.substitute(substitutions, uses);
                failure.substitute(substitutions, uses);
            }
            Self::Fix(bindings, cexp) => {
                for binding in bindings {
                    binding.body.substitute(substitutions, uses);
                    uses.remove(&binding.val);
                }
                cexp.substitute(substitutions, uses);
            }
            Self::Halt(value) => {
                substitute_value(value, substitutions);
            }
        }
    }

    pub(crate) fn pretty_print(&self, indent: usize) {
        match self {
            Cps::PrimOp(PrimOp::Set, vals, _, cexp) => {
                eprintln!("{:>indent$}(set! {:?} {:?})", "", vals[0], vals[1]);
                cexp.pretty_print(indent);
            }
            mut next @ Cps::PrimOp(_, _, _, _) => {
                eprint!("{:>indent$}(let* (", "");
                let mut first = true;
                while let Cps::PrimOp(op, vals, to, cexpr) = next {
                    if !first {
                        eprint!("\n{:>new_indent$}", "", new_indent = indent + 7);
                    }
                    eprint!("[{to:?} ({op:?}");
                    pretty_print_values(vals);
                    eprint!(")]");
                    next = cexpr.as_ref();
                    first = false;
                }
                eprintln!(")");
                next.pretty_print(indent + 3);
                eprint!(")");
            }
            Cps::Fix(bindings, cexp) => {
                eprint!("{:>indent$}(letrec (", "");
                for (i, binding) in bindings.iter().enumerate() {
                    if i > 0 {
                        eprint!("\n{:>new_indent$}", "", new_indent = indent + 9);
                    }
                    let binding_name = binding.val.to_string();
                    eprint!("[{binding_name} (λ ",);
                    if binding.args.continuation.is_none()
                        && binding.args.num_required() == 0
                        && binding.args.variadic
                    {
                        eprint!("{:?} ", binding.args.args[0]);
                    } else {
                        eprint!("(");
                        let mut first = true;
                        if let Some(k) = binding.args.continuation {
                            eprint!("{k:?}");
                            first = false;
                        }
                        for (i, arg) in binding.args.args.iter().enumerate() {
                            if !first {
                                eprint!(" ");
                            }
                            if i == binding.args.num_required() && binding.args.variadic {
                                eprint!(". ");
                            }
                            eprint!("{arg:?}");
                            first = false;
                        }
                        eprintln!(")");
                    }
                    binding.body.pretty_print(indent + 13 + binding_name.len());
                    eprint!(")]");
                }
                eprintln!(")");
                cexp.pretty_print(indent + 2);
                eprint!(")");
            }
            Cps::If(val, succ, fail) => {
                eprintln!("{:>indent$}(if {val:?}", "");
                succ.pretty_print(indent + 5);
                eprintln!();
                fail.pretty_print(indent + 5);
                eprint!(")");
            }
            Cps::App(val, args) => {
                eprint!("{:>indent$}({val:?}", "");
                pretty_print_values(args);
                eprint!(")")
            }
            Cps::Halt(val) => {
                eprint!("{:>indent$}(Halt {val:?})", "");
            }
        }
    }
}

fn pretty_print_values(vals: &[Value]) {
    for val in vals {
        eprint!(" {val:?}");
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
