//! scheme-rs is an implementation of the
//! [R6RS](https://www.r6rs.org/final/r6rs.pdf) specification of the [Scheme programming 
//! language](https://en.wikipedia.org/wiki/Scheme_(programming_language)) that is 
//! designed to embedded within sync and async Rust.
//!
//! # Getting started
//!
//! To get started using scheme-rs in your project, create a [Runtime]:
//!
//! ```
//! # use crate::runtime::Runtime;
//! let runtime = Runtime::new();
//! ```
//!
//! # Feature flags:
//! - `async`: Enables support for async functions. Requires the `tokio` feature
//!   flag.
//! - `tokio`: Enables support for the [tokio](https://tokio.rs/) async
//!   executor.


extern crate self as scheme_rs;

pub mod ast;
pub mod character;
pub mod cps;
pub mod env;
pub mod exceptions;
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
