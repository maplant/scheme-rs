use criterion::*;
use scheme_rs::gc::{Gc, init_gc};

// Reference-count inc/dec throughput, isolated from allocation: every case
// clones and drops handles to pre-allocated, long-lived objects. No
// allocation or reclamation occurs in the timed loops.
fn incdec(c: &mut Criterion) {
    init_gc();

    let obj = Gc::new(0u64);
    c.bench_function("clone+drop x10k, 1 thread, hot object", |b| {
        b.iter(|| {
            for _ in 0..10_000 {
                std::hint::black_box(obj.clone());
            }
        })
    });

    let pool: Vec<Gc<u64>> = (0..64).map(Gc::new).collect();
    c.bench_function("clone+drop x10k, 1 thread, 64-object set", |b| {
        b.iter(|| {
            for i in 0..10_000usize {
                std::hint::black_box(pool[i & 63].clone());
            }
        })
    });

    c.bench_function("clone+drop x10k/thread, 4 threads, private objects", |b| {
        b.iter(|| {
            std::thread::scope(|s| {
                for t in 0..4u64 {
                    let obj = Gc::new(t);
                    s.spawn(move || {
                        for _ in 0..10_000 {
                            std::hint::black_box(obj.clone());
                        }
                    });
                }
            })
        })
    });

    let shared = Gc::new(0u64);
    c.bench_function("clone+drop x10k/thread, 4 threads, shared object", |b| {
        b.iter(|| {
            std::thread::scope(|s| {
                for _ in 0..4 {
                    let shared = shared.clone();
                    s.spawn(move || {
                        for _ in 0..10_000 {
                            std::hint::black_box(shared.clone());
                        }
                    });
                }
            })
        })
    });
}

criterion_group!(benches, incdec);
criterion_main!(benches);
