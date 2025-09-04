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

fn cold_boot_benchmark(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("cold boot", |b| {
        b.to_async(&runtime).iter(|| async move {
            let rt = Runtime::new();
            let prog = Library::new_program(&rt, Path::new("in-line"));
            let env = Environment::Top(prog);
            let sexprs = Syntax::from_str("(import (rnrs base)) (abs -5)", None).unwrap();
            let base = DefinitionBody::parse_lib_body(&rt, &sexprs, &env, &Span::default())
                .await
                .unwrap();
            let compiled = base.compile_top_level();
            rt.compile_expr(compiled).await.call(&[]).await.unwrap();
        })
    });
}

criterion_group!(benches, cold_boot_benchmark);
criterion_main!(benches);
