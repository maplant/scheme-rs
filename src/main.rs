use scheme_rs::{
    eval::{Env, RuntimeError},
    gc::Gc,
    lex::Token,
    syntax::ParsedSyntax,
};

async fn run(code: &str, env: &Gc<Env>) -> Result<(), RuntimeError> {
    let tokens = dbg!(Token::tokenize_str(code).unwrap());
    let sexprs = dbg!(ParsedSyntax::parse(&tokens)).unwrap();
    for sexpr in sexprs {
        let result = sexpr.compile(env).await.unwrap().eval(env).await?;
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
    ((test2 a b c d ... e f g) (list e f g a b c d ...))))
"#,
        &base,
    )
    .await
    .unwrap();
    run(
        r#"
(define-syntax kwote
  (syntax-rules ()
    ((kwote exp) (quote exp))))
"#,
        &base,
    )
    .await
    .unwrap();
    run(
        "(kwote (hello my honey (hello my lady (hello my rag-time-gal))))",
        &base,
    )
    .await
    .unwrap();
    run("(test2 1 2 3 4 5 6 7 8 9 10)", &base).await.unwrap();
    run("(quote (a b c d (+ g b)))", &base).await.unwrap();
    // ( a . (b . (c . ((d . (+ . (g . (b . ())))) . ()))))
    run("(quote ((+ g b c 5 10 \"hello!!!\") . a))", &base)
        .await
        .unwrap();
    run("'(a . ())", &base).await.unwrap();
    run("#(5 5 5 ())", &base).await.unwrap();
}
