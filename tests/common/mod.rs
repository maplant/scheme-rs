//! Scaffolding for R*RS specification tests

use scheme_rs::{exceptions::Condition, registry::bridge, value::Value};

#[bridge(name = "assert-equal?", lib = "(test)")]
async fn test_assert(arg1: &Value, arg2: &Value) -> Result<Vec<Value>, Condition> {
    if arg1 != arg2 {
        Err(Condition::error(format!(
            "assert-equal? failed: {arg1:?} != {arg2:?}"
        )))
    } else {
        Ok(vec![])
    }
}

macro_rules! run_test {
    ($name:ident) => {
        #[::tokio::test]
        async fn $name() {
            use scheme_rs::runtime::Runtime;
            use std::path::Path;

            let test_path = Path::new(concat!("tests/", stringify!($name), ".scm"));
            let rt = Runtime::new();
            rt.run_program(test_path)
                .await
                .expect(&format!("Test {} failed", stringify!($name)));
        }
    };
}

pub(crate) use run_test;
