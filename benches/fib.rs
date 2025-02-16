use scheme_rs::{
    ast::DefinitionBody,
    cps::Compile,
    env::{Environment, Top},
    gc::Gc,
    registry::Registry,
    runtime::Runtime,
    syntax::{Span, Syntax},
};

use criterion::*;

async fn fib() {
    let runtime = Gc::new(Runtime::new());
    let registry = Registry::new(&runtime).await;
    let base = registry.import("(base)").unwrap();
    let mut test_top = Top::program();
    {
        let base = base.read();
        test_top.import(&base);
    }
    let test_top = Environment::from(Gc::new(test_top));

    let sexprs = Syntax::from_str(include_str!("fib.scm"), Some("fib.scm")).unwrap();
    let base = DefinitionBody::parse_program_body(&runtime, &sexprs, &test_top, &Span::default())
        .await
        .unwrap();
    let compiled = base.compile_top_level();
    let closure = runtime.compile_expr(compiled).await.unwrap();
    closure.call(&[]).await.unwrap();
}

fn fib_benchmark(c: &mut Criterion) {
    c.bench_function("fib 10000", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(fib)
    });
}

criterion_group!(benches, fib_benchmark);
criterion_main!(benches);
