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

use indexmap::IndexSet;

use crate::{ast::Literal, syntax::Identifier};
use std::{collections::HashSet, sync::atomic::{AtomicUsize, Ordering}};

mod analysis;
mod codegen;
mod compile;

pub enum Value {
    Var(CpsVar),
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
    Record(usize, CpsVar, Box<Cps>),
    /// Operation to get the address of a value in a record.
    Select(usize, CpsVar, CpsVar, Box<Cps>),
    /// Call to a primitive operator.
    PrimOp(PrimOp, Value, CpsVar, Box<Cps>),
    /// Function application.
    App(Value, Vec<Value>),
    /// Anonymous function generation. The result of this operation is a function
    /// pointer.
    Fix(CpsVar, Vec<CpsVar>, Box<Cps>, Box<Cps>),
}

/// This struct serves two primary purposes:
/// - Allow for conversion between De Bruijn indices and globaly unique names.
/// - Store which functions are determined to be "known" or "escaping".
pub struct CpsCtx<'a> {
    pub up: Option<&'a CpsCtx<'a>>,
    pub vars: IndexSet<CpsVar>,
    pub functions: Option<Functions>,
}

pub struct Functions {
    pub known: HashSet<CpsVar, HashSet<CpsVar>>,
    pub escaping: HashSet<CpsVar, HashSet<CpsVar>>,
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
pub struct CpsVar(usize);

impl CpsVar {
    /// Create a new temporary value.
    fn gensym() -> Self {
        static NEXT_SYM: AtomicUsize = AtomicUsize::new(0);
        CpsVar(NEXT_SYM.fetch_add(1, Ordering::Relaxed))
    }
}

impl ToString for CpsVar {
    fn to_string(&self) -> String {
        format!("v{}", self.0)
    }
}
