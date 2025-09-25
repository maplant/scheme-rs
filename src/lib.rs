extern crate self as scheme_rs;

pub mod ast;
pub mod character;
pub mod cps;
pub mod env;
pub mod err;
pub mod exceptions;
pub mod expand;
pub mod futures;
pub mod gc;
pub mod lex;
pub mod lists;
pub mod num;
pub mod parse;
pub mod proc;
pub mod records;
pub mod registry;
pub mod runtime;
/// adapted from https://github.com/xnacly/sqleibniz/blob/master/src/highlight/builder.rs
pub mod string_builder;
pub mod strings;
pub mod symbols;
pub mod syntax;
pub mod value;
pub mod vectors;
