use criterion::*;
use scheme_rs::{env::TopLevelEnvironment, runtime::Runtime};
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
    let repl = TopLevelEnvironment::new_repl(&rt);
    maybe_await!(repl.eval(true, "(import (rnrs base)) (abs -5)")).unwrap();
}

criterion_group!(benches, cold_boot_benchmark);
criterion_main!(benches);
