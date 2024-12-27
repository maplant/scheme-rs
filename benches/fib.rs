use scheme_rs::env::Env;

use criterion::*;

async fn fib() {
    let top = Env::top().await;

    let _ = top.eval("fib.scm", include_str!("fib.scm")).await.unwrap();
}

fn fib_benchmark(c: &mut Criterion) {
    c.bench_function("fib 10000", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(fib)
    });
}

criterion_group!(benches, fib_benchmark);
criterion_main!(benches);
