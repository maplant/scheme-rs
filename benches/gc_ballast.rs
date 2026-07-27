use criterion::*;
use scheme_rs::gc::{Gc, Trace, collect_garbage, init_gc};

// The large-live-heap scenario: 1M live objects as ballast, then measure
// (a) forced-collection latency and (b) allocation churn throughput.
// The ballast is quiescent — no rc activity — so an O(rc-activity)
// collector should be indifferent to it, while a scanning collector pays
// per-object costs on every epoch.

#[derive(Trace)]
struct Node {
    next: Option<Gc<Node>>,
}

fn live_chain(len: usize) -> Gc<Node> {
    let mut head = Gc::new(Node { next: None });
    for _ in 0..len {
        head = Gc::new(Node { next: Some(head) });
    }
    head
}

#[cfg(not(feature = "async"))]
fn ballast_benchmark(c: &mut Criterion) {
    init_gc();
    let ballast = live_chain(1_000_000);

    let mut group = c.benchmark_group("ballast");
    group.sample_size(10);
    group.bench_function("collect, 1M live quiescent", |b| b.iter(collect_garbage));
    group.bench_function("alloc+drop x10k with 1M live ballast", |b| {
        b.iter(|| {
            for i in 0..10_000u64 {
                std::hint::black_box(Gc::new(i));
            }
        })
    });
    group.finish();

    drop(ballast);
}

#[cfg(feature = "async")]
fn ballast_benchmark(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    init_gc();
    let ballast = live_chain(1_000_000);

    let mut group = c.benchmark_group("ballast");
    group.sample_size(10);
    group.bench_function("collect, 1M live quiescent", |b| {
        b.to_async(&runtime).iter(|| collect_garbage())
    });
    group.bench_function("alloc+drop x10k with 1M live ballast", |b| {
        b.iter(|| {
            for i in 0..10_000u64 {
                std::hint::black_box(Gc::new(i));
            }
        })
    });
    group.finish();

    drop(ballast);
}

criterion_group!(benches, ballast_benchmark);
criterion_main!(benches);
