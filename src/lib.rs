//! scheme-rs is an implementation of the
//! [R6RS](https://www.r6rs.org/final/r6rs.pdf) specification of the [Scheme programming
//! language](https://en.wikipedia.org/wiki/Scheme_(programming_language)) that is
//! designed to embedded within sync and async Rust.
//!
//! # Feature flags:
//! - `async`: Enables support for async functions. Requires the `tokio` feature
//!   flag.
//! - `tokio`: Enables support for the [tokio](https://tokio.rs/) async
//!   executor.
//!
//! # Getting started
//!
//! To get started using scheme-rs in your project, create a
//! [`Runtime`](runtime::Runtime):
//!
//! ```
//! # use scheme_rs::runtime::Runtime;
//! let runtime = Runtime::new();
//! ```
//!
//! The `Runtime` struct initializes the garbage collector and handles the
//! memory of JIT compiled functions. The `Runtime` struct is automatically
//! garbage collected so you only need it for as long as you're creating new
//! scheme procedures.
//!
//! # Running Scheme code from Rust
//!
//! The simplest way to run scheme code from Rust is to use the
//! [`Environment::eval`](env::Environment::eval) function which evaluates a
//! string and returns the evaluated scheme values. Before you can call `eval`,
//! you need to create an [`Environment`](env::Environment) which defines the
//! set of imports provided to the scheme code.
//!
//! ```
//! # use scheme_rs::{runtime::Runtime, env::Environment};
//! # let runtime = Runtime::new();
//! let env = Environment::new_repl(&runtime);
//! env.import("(library (rnrs))".parse().unwrap());
//! ```
//!
//! Now that you have an environment, you can call `eval` on it. The first
//! argument to eval determines whether or not the code is allowed to import
//! external packages. If you are running untrusted user code, be sure to pass
//! false and think careful of the functions you provide.
//!
//! ```
//! # use scheme_rs::{runtime::Runtime, env::Environment};
//! # let runtime = Runtime::new();
//! # let env = Environment::new_repl(&runtime);
//! # env.import("(library (rnrs))".parse().unwrap());
//! let [factorial] = env.eval(
//!     false,
//!     r#"
//!     (define (fact n)
//!       (if (= n 1)
//!           1
//!           (* n (fact (- n 1)))))
//!    fact
//!    "#
//! )
//! .unwrap()
//! .try_into()
//! .unwrap();
//! ```
//!
//! ## Procedures
//!
//! Evaluating the previous code example returns a factorial 
//! [`Procedure`](proc::Procedure) which can be called from Rust. To do so, use
//! the [`Procedure::call`](proc::Procedure::call) method. Procedures are 
//! automatically garbage collected and implement `Send` and `Sync` and are 
//! `'static` so you can hold on to them for as long as you want and put them
//! anywhere.
//!
//! ```
//! # use scheme_rs::{runtime::Runtime, env::Environment, value::Value};
//! # let runtime = Runtime::new();
//! # let env = Environment::new_repl(&runtime);
//! # env.import("(library (rnrs))".parse().unwrap());
//! # let [factorial] = env.eval(
//! #     false,
//! #     r#"
//! #     (define (fact n)
//! #       (if (= n 1)
//! #           1
//! #           (* n (fact (- n 1)))))
//! #    fact
//! #    "#
//! # )
//! # .unwrap()
//! # .try_into()
//! # .unwrap();
//! let [result] = factorial.call(&[Value::from(5)]).unwrap().try_into().unwrap();
//! let result: u64 = result.try_into().unwrap();
//! assert_eq!(result, 120);
//! ```
//!
//! # Running Rust code from Scheme
//!
//! ## Embedding Rust structs in Scheme
//!
//! # Garbage Collection
//!
//!
//!
//! See the [gc] module for a more detailed explanation on the garbage collector.
//!
//!
//!

extern crate self as scheme_rs;

pub mod ast;
pub(crate) mod character;
pub(crate) mod cps;
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

#[cfg(feature = "tokio")]
pub mod futures;

// Require tokio (for now) if the async feature is enabled
#[cfg(all(feature = "async", not(feature = "tokio")))]
compile_error!("async features requires the tokio feature to be enabled");
