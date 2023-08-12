use scheme_rs::{
    eval::{self, Eval},
    gc::Gc,
    lex, parse,
};

#[tokio::main]
async fn main() {
    let tokens = lex::lex("(+ 3 4)").unwrap();
    println!("tokens: {tokens:#?}");
    let (_, expression) = parse::expression(&tokens).unwrap();
    println!("ast: {expression:#?}");
    let base = Gc::new(eval::Env::base());
    let result = expression.eval(&base).await.unwrap();
    println!("result = {:#?}", *result.read().await);
}
