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
//! We go directly from our Scheme AST to CPS because our AST already maps well
//! onto the closure compilation style of linked closures; this is because our
//! AST creation step resolves symbols into pairs of up-links and offsets.

use crate::{ast::Literal, syntax::Identifier};
use std::sync::atomic::{AtomicUsize, Ordering};

mod codegen;
mod compile;

pub enum Value {
    Var(Var),
    Literal(Literal),
}

pub enum PrimOp {
    Set,
    Add,
    Sub,
    Mul,
    Div,
    If,
    And,
    Or,
    CallWithCurrentContinuation,
}

pub enum Cps {
    /// A record, for now, is an array of values. These are used to represent
    /// environments at runtime.
    Record(usize, Var, Box<Cps>),
    /// Operation to get the address of a value in a record.
    Select(usize, Var, Var, Box<Cps>),
    /// Call to a primitive operator.
    PrimOp(PrimOp, Value, Var, Box<Cps>),
    /// Function application.
    App(Value, Vec<Value>),
    /// Anonymous function generation. The result of this operation is a function
    /// pointer.
    Fix(Var, Vec<Var>, Box<Cps>, Box<Cps>),
}

/// To start, a compilation unit will be a single function. This is because
/// calling set! on a function is perfectly legal. Later, we'll prove which
/// functions are set! and which are not, allowing us to make further
/// optimizations.
pub struct CpsCompiledFunction {
    args: Vec<Identifier>,
    remaining: Option<Identifier>,
    cps: Cps,
}

/// Vars in our CPS notation purely represent temporaries; they have no location
/// on the stack and are immutable.
#[derive(Copy, Clone, Debug)]
pub struct Var(usize);

impl Var {
    /// Create a new temporary value.
    fn gensym() -> Self {
        static NEXT_SYM: AtomicUsize = AtomicUsize::new(0);
        Var(NEXT_SYM.fetch_add(1, Ordering::Relaxed))
    }
}

impl ToString for Var {
    fn to_string(&self) -> String {
        format!("v{}", self.0)
    }
}
