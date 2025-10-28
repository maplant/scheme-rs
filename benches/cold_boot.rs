use std::path::Path;

use criterion::*;
use scheme_rs::{
    ast::DefinitionBody,
    cps::Compile,
    env::Environment,
    registry::Library,
    runtime::Runtime,
    syntax::{Span, Syntax},
};
use scheme_rs_macros::{maybe_async, maybe_await};

#[cfg(not(feature = "async"))]
fn cold_boot_benchmark(c: &mut Criterion) {
    c.bench_function("cold boot", |b| b.iter(run_bench));
}

#[cfg(feature = "async")]
fn cold_boot_benchmark(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("cold boot", |b| {
        b.to_async(&runtime)
            .iter(|| async move { run_bench().await })
    });
}

#[maybe_async]
fn run_bench() {
    let rt = Runtime::new();
    let prog = Library::new_program(&rt, Path::new("in-line"));
    let env = Environment::Top(prog);
    let sexprs = Syntax::from_str("(import (rnrs base)) (abs -5)", None).unwrap();
    let base = maybe_await!(DefinitionBody::parse_lib_body(
        &rt,
        &sexprs,
        &env,
        &Span::default()
    ))
    .unwrap();
    let compiled = base.compile_top_level();
    maybe_await!(maybe_await!(rt.compile_expr(compiled)).call(&[])).unwrap();
}

criterion_group!(benches, cold_boot_benchmark);
criterion_main!(benches);
