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
    env::{Local, Var},
    gc::Trace,
};
use std::{collections::HashSet, str::FromStr};

mod analysis;
mod codegen;
mod compile;

#[derive(Clone)]
pub enum Value {
    Var(Var),
    Literal(Literal),
}

#[derive(Copy, Clone, Debug, Trace)]
pub enum PrimOp {
    Set,
    Add,
    Sub,
    Mul,
    Div,
    CallWithCurrentContinuation,
}

impl FromStr for PrimOp {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        match s.as_ref() {
            "+" => Ok(Self::Add),
            "-" => Ok(Self::Sub),
            "/" => Ok(Self::Div),
            "set" => Ok(Self::Set),
            "call/cc" | "call-with-current-continuation" => Ok(Self::CallWithCurrentContinuation),
            _ => Err(()),
        }
    }
}

pub enum Cps {
    /*
    /// A record, for now, is an array of values. These are used to represent
    /// environments at runtime.
    Record(usize, Local, Box<Cps>),
    /// Operation to get the address of a value in a record.
    Select(usize, Var, Local, Box<Cps>),
     */
    /// Generates a cell of type *const Gc<Value>
    AllocCell(Local, Box<Cps>),
    /// Call to a primitive operator.
    PrimOp(PrimOp, Vec<Value>, Local, Box<Cps>),
    /// Function application.
    App(Value, Vec<Value>),
    /// Branching.
    If(Value, Box<Cps>, Box<Cps>),
    /*
    /// Anonymous function generation. The result of this operation is a function
    /// pointer.
    Fix(Var, Vec<Var>, Box<Cps>, Box<Cps>),
    */
    /// Closure generation. The result of this operation is a *const Value::Closure
    Closure {
        args: Vec<Local>,
        // env: Vec<Local>,
        body: Box<Cps>,
        val: Local,
        cexp: Box<Cps>,
    },
}
