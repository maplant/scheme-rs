use criterion::*;
use scheme_rs::gc::{Gc, init_gc};

fn alloc_churn(c: &mut Criterion) {
    init_gc();

    c.bench_function("alloc+drop x10k, 1 thread", |b| {
        b.iter(|| {
            for i in 0..10_000u64 {
                std::hint::black_box(Gc::new(i));
            }
        })
    });

    c.bench_function("alloc+drop x10k/thread, 4 threads", |b| {
        b.iter(|| {
            std::thread::scope(|s| {
                for _ in 0..4 {
                    s.spawn(|| {
                        for i in 0..10_000u64 {
                            std::hint::black_box(Gc::new(i));
                        }
                    });
                }
            })
        })
    });
}

criterion_group!(benches, alloc_churn);
criterion_main!(benches);
