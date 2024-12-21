use std::sync::Arc;

use scheme_rs::{
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

    let r6rs_tok = Token::tokenize_file(include_str!("r6rs.scm"), "r6rs.scm").unwrap();
    let r6rs_sexprs = ParsedSyntax::parse(&r6rs_tok).unwrap();
    for sexpr in r6rs_sexprs {
        sexpr
            .compile(&top, &None)
            .await
            .unwrap()
            .eval(&top, &None)
            .await
            .unwrap();
    }
}
