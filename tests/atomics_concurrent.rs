#![cfg(feature = "tokio")]

mod common;

common::run_test!(atomics_concurrent);
