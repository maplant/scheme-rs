use std::sync::Arc;

use scheme_rs::{
    ast::AstNode,
    builtin,
    continuation::Continuation,
    env::Env,
    error::RuntimeError,
    gc::Gc,
    lex::Token,
    syntax::ParsedSyntax,
    value::{eqv, Value},
};

#[builtin("assert-eq")]
pub async fn test_assert(
    _cont: &Option<Arc<Continuation>>,
    arg1: &Gc<Value>,
    arg2: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    if !eqv(arg1, arg2) {
        let arg1 = arg1.read().fmt();
        let arg2 = arg2.read().fmt();
        Err(RuntimeError::assert_eq_failed(&arg2, &arg1))
    } else {
        Ok(vec![])
    }
}

#[tokio::test]
async fn r6rs() {
    let top = Env::top().await;

    let _ = top
        .eval("r6rs.scm", include_str!("r6rs.scm"))
        .await
        .unwrap();
}
