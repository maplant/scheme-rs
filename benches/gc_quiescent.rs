use criterion::*;
use scheme_rs::gc::{Gc, Trace, collect_garbage, init_gc};

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
fn quiescent_benchmark(c: &mut Criterion) {
    init_gc();
    let chain = live_chain(100_000);

    c.bench_function("collect 100k quiescent", |b| b.iter(collect_garbage));

    drop(chain);
}

#[cfg(feature = "async")]
fn quiescent_benchmark(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    init_gc();
    let chain = live_chain(100_000);

    c.bench_function("collect 100k quiescent", |b| {
        b.to_async(&runtime).iter(|| collect_garbage())
    });

    drop(chain);
}

criterion_group!(benches, quiescent_benchmark);
criterion_main!(benches);
