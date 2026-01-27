use scheme_rs::{env::TopLevelEnvironment, proc::Procedure, runtime::Runtime, value::Expect1};

use criterion::*;
use scheme_rs_macros::{maybe_async, maybe_await};

#[maybe_async]
fn integrate_fn() -> Procedure {
    let rt = Runtime::new();
    let prog = TopLevelEnvironment::new_repl(&rt);
    maybe_await!(prog.eval(true, "(import (benches integrate)) run"))
        .unwrap()
        .expect1()
        .unwrap()
}

#[cfg(not(feature = "async"))]
fn integrate_benchmark(c: &mut Criterion) {
    let proc = integrate_fn();

    c.bench_function("integrate", |b| {
        b.iter(|| proc.call(&[]));
    });
}

#[cfg(feature = "async")]
fn integrate_benchmark(c: &mut Criterion) {
    // Set up and compile the closure
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let proc = runtime.block_on(async move { integrate_fn().await });

    c.bench_function("integrate", |b| {
        b.to_async(&runtime).iter(|| {
            let val = proc.clone();
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
