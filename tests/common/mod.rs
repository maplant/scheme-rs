#![allow(dead_code, unused_macros, unused_imports)]

//! Test to see whether or not passes r*rs specifications

use scheme_rs::{
    ast::DefinitionBody,
    cps::Compile,
    env::{Environment, Top},
    exception::Condition,
    gc::Gc,
    registry::{Registry, bridge},
    runtime::Runtime,
    syntax::{Span, Syntax},
    value::Value,
};
use std::error::Error as StdError;

pub struct TestRuntime {
    runtime: Gc<Runtime>,
    test_top: Environment,
}
impl TestRuntime {
    pub async fn new() -> Self {
        let runtime = Gc::new(Runtime::new());
        let registry = Registry::new(&runtime).await;
        let base = registry.import("(base)").unwrap();
        let mut test_top = Top::program();
        {
            let base = base.read();
            test_top.import(&base);
        }
        let test_top = Environment::from(Gc::new(test_top));

        Self { runtime, test_top }
    }

    pub async fn exec_syn(&self, sexprs: &[Syntax]) -> Result<(), Box<dyn StdError>> {
        let base = DefinitionBody::parse_program_body(
            &self.runtime,
            sexprs,
            &self.test_top,
            &Span::default(),
        )
        .await
        .unwrap();
        let compiled = base.compile_top_level();
        let closure = self
            .runtime
            .compile_expr(compiled)
            .await
            .map_err(Box::new)?;
        Ok(closure.call(&[]).await.map(drop).map_err(Box::new)?)
    }

    pub async fn exec_str<'a>(&self, src: &'a str) -> Result<(), Box<dyn StdError + 'a>> {
        let sexprs = Syntax::from_str(src, None).map_err(Box::new)?;
        self.exec_syn(&sexprs).await
    }
}

#[bridge(name = "assert-eq", lib = "(base)")]
pub async fn test_assert(arg1: &Value, arg2: &Value) -> Result<Vec<Value>, Condition> {
    if arg1 != arg2 {
        let arg1 = format!("{arg1:?}");
        let arg2 = format!("{arg2:?}");
        Err(Condition::assert_eq_failed(&arg2, &arg1))
    } else {
        Ok(vec![])
    }
}

macro_rules! assert_file {
    ($name:ident) => {
        #[::tokio::test]
        async fn $name() {
            let rt = $crate::common::TestRuntime::new().await;
            let sexprs = scheme_rs::syntax::Syntax::from_str(
                include_str!(concat!(stringify!($name), ".scm")),
                Some(concat!(stringify!($name), ".scm")),
            )
            .unwrap();
            rt.exec_syn(&sexprs).await.unwrap();
        }
    };
}

macro_rules! assert_failure {
    ($name:ident, $expr:literal) => {
        #[::tokio::test]
        async fn $name() {
            let rt = $crate::common::TestRuntime::new().await;
            assert!(rt.exec_str($expr).await.is_err())
        }
    };
}

pub(crate) use assert_failure;
pub(crate) use assert_file;
