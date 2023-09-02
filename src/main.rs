use scheme_rs::{
    eval::{Env, Eval, RuntimeError, Value},
    gc::Gc,
    lex::Token,
    sexpr::ParsedSExpr,
};

async fn run(code: &str, env: &Gc<Env>) -> Result<(), RuntimeError> {
    let tokens = Token::tokenize_str(code).unwrap();
    let sexprs = dbg!(ParsedSExpr::parse(&tokens).unwrap());
    for sexpr in sexprs {
        let result = dbg!(sexpr).compile(env).await.eval(env).await?;
        println!("result = {}", result.read().await.fmt().await);
    }
    Ok(())
}

#[tokio::main]
async fn main() {
    let base = Gc::new(Env::base());
    //    let result = run("(5 . 5)", &base).await.unwrap();
    // 1:
    run(
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
    //    println!("result = {}", result.read().await.fmt().await);
    // 2:
    run(
        r#"
        (fact 150 1)
        "#,
        &base,
    )
    .await
    .unwrap();
    //    println!("result = {}", result.read().await.fmt().await);
    // 3:
    run(
        r#"
(define-syntax test
  (syntax-rules ()
     ((test x) (+ x 5))))
        "#,
        &base,
    )
    .await
    .unwrap();
    // 4:
    run("(test 10)", &base).await.unwrap();
    run(
        r#"
(define-syntax test2
  (syntax-rules ()
    ((test2 a b _ x ...) (list a b x ...))))
"#,
        &base,
    )
    .await
    .unwrap();
    run("(test2 1 2 3 4 5)", &base).await.unwrap();
}
