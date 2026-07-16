#![cfg(all(feature = "async", feature = "tokio"))]

mod common;

common::run_test!(parameters_async);
