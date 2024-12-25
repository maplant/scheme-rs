use scheme_rs::{env::Env, lex::Token, syntax::ParsedSyntax};

use criterion::*;

async fn fib() {
    let top = Env::top().await;

    let r6rs_tok = Token::tokenize_file(include_str!("fib.scm"), "fib.scm").unwrap();
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

fn fib_benchmark(c: &mut Criterion) {
    c.bench_function("fib 10000", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(fib)
    });
}

criterion_group!(benches, fib_benchmark);
criterion_main!(benches);
