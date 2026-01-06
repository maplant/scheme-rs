//! Scaffolding for R*RS specification tests

use scheme_rs::{conditions::Condition, registry::bridge, value::Value};

#[bridge(name = "assert-equal?", lib = "(test)")]
fn test_assert(arg1: &Value, arg2: &Value) -> Result<Vec<Value>, Condition> {
    if !arg1.equal(arg2) {
        Err(Condition::error(format!(
            "assert-equal? failed: {arg1:?} != {arg2:?}"
        )))
    } else {
        Ok(vec![])
    }
}

macro_rules! run_test {
    ($name:ident) => {
        #[scheme_rs_macros::maybe_async]
        #[cfg_attr(feature = "async", ::tokio::test)]
        #[cfg_attr(not(feature = "async"), test)]
        fn $name() {
            use scheme_rs::runtime::Runtime;
            use std::path::Path;

            let test_path = Path::new(concat!("tests/", stringify!($name), ".scm"));
            let rt = Runtime::new();
            scheme_rs_macros::maybe_await!(rt.run_program(test_path))
                .expect(&format!("Test {} failed", stringify!($name)));
        }
    };
}

pub(crate) use run_test;
