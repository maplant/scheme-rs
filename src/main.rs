use scheme_rs::{
    eval::{self, Eval},
    gc::Gc,
    lex, parse,
};

#[tokio::main]
async fn main() {
    let tokens = lex::lex(
        r#"
        (define (fact x acc)
          (if (= x 0)
              acc
              (fact (- x 1) (* x acc))))
        "#,
    )
    .unwrap();
    println!("tokens: {tokens:#?}");
    let (_, expression) = parse::expression(&tokens).unwrap();
    println!("ast: {expression:#?}");
    let base = Gc::new(eval::Env::base());
    let result = expression.eval(&base).await.unwrap();
    println!("result = {}", result.read().await.fmt().await);
    // 2:
    let tokens = lex::lex(
        r#"
        (list 1 2 3 4 5)
        "#,
    )
    .unwrap();
    println!("tokens: {tokens:#?}");
    let (_, expression) = parse::expression(&tokens).unwrap();
    println!("ast: {expression:#?}");
    let result = expression.eval(&base).await.unwrap();
    println!("result = {}", result.read().await.fmt().await);
}
