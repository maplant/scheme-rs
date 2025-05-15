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

async fn integrate_fn() -> Gc<scheme_rs::proc::Closure> {
    let runtime = Gc::new(Runtime::new());
    let registry = Registry::new(&runtime).await;
    let base = registry.import("(base)").unwrap();
    let mut test_top = Top::program();
    {
        let base = base.read();
        test_top.import(&base);
    }
    let test_top = Environment::from(Gc::new(test_top));

    let sexprs = Syntax::from_str(include_str!("integrate.scm"), Some("integrate.scm")).unwrap();
    let base = DefinitionBody::parse_program_body(&runtime, &sexprs, &test_top, &Span::default())
        .await
        .unwrap();
    let compiled = base.compile_top_level();
    runtime.compile_expr(compiled).await.unwrap()
}

fn integrate_benchmark(c: &mut Criterion) {
    // Set up and compile the closure
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let closure = runtime.block_on(async move { integrate_fn().await });

    c.bench_function("integrate", |b| {
        b.to_async(&runtime).iter(|| {
            let val = closure.clone();
            async move { val.call(&[]).await }
        })
    });
}

criterion_group!(
    name = benches;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(26));
    targets = integrate_benchmark
);

criterion_main!(benches);
