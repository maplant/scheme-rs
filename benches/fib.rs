use std::path::Path;

use scheme_rs::{
    ast::DefinitionBody,
    cps::Compile,
    env::Environment,
    proc::Procedure,
    registry::Library,
    runtime::Runtime,
    syntax::{Span, Syntax},
};

use criterion::*;
use scheme_rs_macros::{maybe_async, maybe_await};

#[maybe_async]
fn fib_fn() -> Procedure {
    let rt = Runtime::new();
    let prog = Library::new_program(&rt, Path::new("fib.scm"));
    let env = Environment::Top(prog);

    let sexprs = Syntax::from_str(include_str!("fib.scm"), Some("fib.scm")).unwrap();
    let base = maybe_await!(DefinitionBody::parse_lib_body(
        &rt,
        &sexprs,
        &env,
        &Span::default()
    ))
    .unwrap();
    let compiled = base.compile_top_level();
    maybe_await!(rt.compile_expr(compiled))
}

#[cfg(not(feature = "async"))]
fn fib_benchmark(c: &mut Criterion) {
    let proc = fib_fn();

    c.bench_function("fib 10000", |b| b.iter(|| proc.call(&[])));
}

#[cfg(feature = "async")]
fn fib_benchmark(c: &mut Criterion) {
    // Set up and compile the closure
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let proc = runtime.block_on(async move { fib_fn().await });

    c.bench_function("fib 10000", |b| {
        b.to_async(&runtime).iter(|| {
            let val = proc.clone();
            async move { val.call(&[]).await }
        })
    });
}

criterion_group!(benches, fib_benchmark);
criterion_main!(benches);
