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

async fn fib_fn() -> Closure {
    let rt = Runtime::new();
    let prog = Library::new_program(&rt, Path::new("fib.scm"));
    let env = Environment::Top(prog);

    let sexprs = Syntax::from_str(include_str!("fib.scm"), Some("fib.scm")).unwrap();
    let base = DefinitionBody::parse_lib_body(&rt, &sexprs, &env, &Span::default())
        .await
        .unwrap();
    let compiled = base.compile_top_level();
    rt.compile_expr(compiled).await
}

fn fib_benchmark(c: &mut Criterion) {
    // Set up and compile the closure
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let closure = runtime.block_on(async move { fib_fn().await });

    c.bench_function("fib 10000", |b| {
        b.to_async(&runtime).iter(|| {
            let val = closure.clone();
            async move { val.call(&[]).await }
        })
    });
}

criterion_group!(benches, fib_benchmark);
criterion_main!(benches);
