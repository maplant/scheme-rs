extern crate self as scheme_rs;

pub mod ast;
pub mod character;
pub mod conditions;
pub mod cps;
pub mod enumerations;
pub mod env;
pub mod eval;
pub mod expand;
pub mod gc;
pub mod hashtables;
pub mod lists;
pub mod num;
pub mod ports;
pub mod proc;
pub mod records;
pub mod registry;
pub mod runtime;
pub mod strings;
pub mod symbols;
pub mod syntax;
pub mod value;
pub mod vectors;

#[cfg(feature = "tokio")]
pub mod futures;

// Require tokio (for now) if the async feature is enabled
#[cfg(all(feature = "async", not(feature = "tokio")))]
compile_error!("async features requires the tokio feature to be enabled");
