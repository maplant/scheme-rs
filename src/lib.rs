#![doc = include_str!("docs.md")]

extern crate self as scheme_rs;

pub(crate) mod ast;
pub(crate) mod character;
pub(crate) mod cps;
pub(crate) mod enumerations;
pub mod env;
pub mod eval;
pub mod exceptions;
pub(crate) mod expand;
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

/// Internal `Either` type
#[derive(Debug, Clone)]
enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Either<L, R> {
    pub fn left_or(self, default: L) -> L {
        if let Self::Left(l) = self { l } else { default }
    }
}

#[cfg(feature = "tokio")]
pub mod futures;

// Require tokio (for now) if the async feature is enabled
#[cfg(all(feature = "async", not(feature = "tokio")))]
compile_error!("async features requires the tokio feature to be enabled");

#[cfg(target_pointer_width = "32")]
compile_error!("32 bit architectures are currently not supported");
