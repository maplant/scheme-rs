use scheme_rs::{env::TopLevelEnvironment, proc::Procedure, runtime::Runtime, value::Expect1};

use criterion::*;
use scheme_rs_macros::{maybe_async, maybe_await};

#[maybe_async]
fn yin_yang_fn() -> Procedure {
    let rt = Runtime::new();
    let repl = TopLevelEnvironment::new_repl(&rt);
    maybe_await!(repl.eval(
        true,
        "
        (import (benches yin-yang))
        run
        "
    ))
    .unwrap()
    .expect1()
    .unwrap()
}

#[cfg(not(feature = "async"))]
fn yin_yang_benchmark(c: &mut Criterion) {
    // Set up and compile the closure
    let proc = yin_yang_fn();

    c.bench_function("yin_yang", |b| {
        b.iter(|| proc.call(&[]));
    });
}

#[cfg(feature = "async")]
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
