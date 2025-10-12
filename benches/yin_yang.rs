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

async fn yin_yang_fn() -> Procedure {
    let rt = Runtime::new();
    let prog = Library::new_program(&rt, Path::new("yin-yang.scm"));
    let env = Environment::Top(prog);

    let sexprs = Syntax::from_str(include_str!("yin-yang.scm"), Some("yin-yang.scm")).unwrap();
    let base = DefinitionBody::parse_lib_body(&rt, &sexprs, &env, &Span::default())
        .await
        .unwrap();
    let compiled = base.compile_top_level();
    rt.compile_expr(compiled).await
}

fn yin_yang_benchmark(c: &mut Criterion) {
    // Set up and compile the closure
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let proc = runtime.block_on(async move { yin_yang_fn().await });

    c.bench_function("yin_yang", |b| {
        b.to_async(&runtime).iter(|| {
            let val = proc.clone();
            async move { val.call(&[]).await }
        })
    });
}

criterion_group!(benches, yin_yang_benchmark);
criterion_main!(benches);
