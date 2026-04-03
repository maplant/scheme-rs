use scheme_rs::{
    env::TopLevelEnvironment,
    proc::{ContBarrier, Procedure},
    runtime::Runtime,
    value::Expect1,
};

use criterion::*;
use scheme_rs_macros::{maybe_async, maybe_await};

#[maybe_async]
fn fib_fn() -> Procedure {
    let rt = Runtime::new();
    let repl = TopLevelEnvironment::new_repl(&rt);
    maybe_await!(repl.eval(
        true,
        "
        (import (benches fib))
        run
        "
    ))
    .unwrap()
    .expect1()
    .unwrap()
}

#[cfg(not(feature = "async"))]
fn fib_benchmark(c: &mut Criterion) {
    let proc = fib_fn();

    c.bench_function("fib 10000", |b| {
        b.iter(|| proc.call(&[], &mut ContBarrier::new()))
    });
}

#[cfg(feature = "async")]
fn fib_benchmark(c: &mut Criterion) {
    // Set up and compile the closure
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let proc = runtime.block_on(async move { fib_fn().await });

    c.bench_function("fib 10000", |b| {
        b.to_async(&runtime).iter(|| {
            let val = proc.clone();
            async move { val.call(&[], &mut ContBarrier::new()).await }
        })
    });
}

criterion_group!(benches, fib_benchmark);
criterion_main!(benches);
