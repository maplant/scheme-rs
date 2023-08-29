use scheme_rs::{
    eval::{Env, Eval, RuntimeError, Value},
    gc::Gc,
    lex,
    sexpr::SExpr,
};

async fn run(code: &str, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
    let tokens = lex::lex(code).unwrap();
    let sexpr = SExpr::parse(&tokens);
    sexpr.compile(env).await.eval(env).await
}

#[tokio::main]
async fn main() {
    let base = Gc::new(Env::base());
    // 1:
    let result = run(
        r#"
        (define (fact x acc)
          (if (= x 0)
              acc
              (fact (- x 1) (* x acc))))
        "#,
        &base,
    )
    .await
    .unwrap();
    println!("result = {}", result.read().await.fmt().await);
    // 2:
    let result = run(
        r#"
        (list 1 2 3 4 5)
        "#,
        &base,
    )
    .await
    .unwrap();
    println!("result = {}", result.read().await.fmt().await);
}
