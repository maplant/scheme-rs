use std::path::Path;

use scheme_rs::{
    ast::DefinitionBody,
    cps::Compile,
    env::Environment,
    proc::Closure,
    registry::Library,
    runtime::Runtime,
    syntax::{Span, Syntax},
};

use criterion::*;

async fn integrate_fn() -> Closure {
    let rt = Runtime::new();
    let prog = Library::new_program(&rt, Path::new("integrate.scm"));
    let env = Environment::Top(prog);

    let sexprs = Syntax::from_str(include_str!("integrate.scm"), Some("integrate.scm")).unwrap();
    let base = DefinitionBody::parse_lib_body(&rt, &sexprs, &env, &Span::default())
        .await
        .unwrap();
    let compiled = base.compile_top_level();
    rt.compile_expr(compiled).await
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
