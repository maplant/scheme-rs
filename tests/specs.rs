//! Test to see whether or not passes r*rs specifications

use scheme_rs::{
    ast::DefinitionBody,
    cps::Compile,
    env::{Environment, Top},
    exception::Exception,
    gc::Gc,
    registry::{bridge, Registry},
    runtime::Runtime,
    syntax::{Span, Syntax},
    value::{eqv, Value},
};

#[bridge(name = "assert-eq", lib = "(base)")]
pub async fn test_assert(arg1: &Gc<Value>, arg2: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    if !eqv(arg1, arg2) {
        let arg1 = format!("{arg1:?}");
        let arg2 = format!("{arg2:?}");
        Err(Exception::assert_eq_failed(&arg2, &arg1))
    } else {
        Ok(vec![])
    }
}

macro_rules! test_specification {
    ($name:ident) => {
        #[tokio::test]
        async fn $name() {
            let runtime = Gc::new(Runtime::new());
            let registry = Registry::new(&runtime).await;
            let base = registry.import("(base)").unwrap();
            let mut test_top = Top::program();
            {
                let base = base.read();
                test_top.import(&base);
            }
            let test_top = Environment::from(Gc::new(test_top));

            let sexprs = Syntax::from_str(
                include_str!(concat!(stringify!($name), ".scm")),
                Some(concat!(stringify!($name), ".scm"))
            ).unwrap();
            let base = DefinitionBody::parse_program_body(&runtime, &sexprs, &test_top, &Span::default())
                .await
                .unwrap();
            let compiled = base.compile_top_level();
            let closure = runtime.compile_expr(compiled).await.unwrap();
            closure.call(&[]).await.unwrap();
        }
    }
}
test_specification!(r6rs);
test_specification!(r7rs);
