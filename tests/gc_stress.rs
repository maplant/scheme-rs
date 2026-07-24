//! Multi-threaded GC stress test (crash/UAF net). `GC_STRESS_ITERS` scales work.
#![cfg(not(feature = "async"))]

use parking_lot::{Mutex, RwLock};
use scheme_rs::gc::{Gc, Trace, collect_garbage, init_gc};
use std::sync::Arc;

#[derive(Default, Trace)]
struct Node {
    edges: Vec<Gc<RwLock<Node>>>,
}

fn xorshift(s: &mut u64) -> u64 {
    *s ^= *s << 13;
    *s ^= *s >> 7;
    *s ^= *s << 17;
    *s
}

#[test]
fn gc_stress() {
    init_gc();

    let iters: usize = std::env::var("GC_STRESS_ITERS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(20_000);

    let exchange: Arc<Mutex<Vec<Gc<RwLock<Node>>>>> = Arc::new(Mutex::new(Vec::new()));

    let threads: Vec<_> = (0..4)
        .map(|t| {
            let exchange = exchange.clone();
            std::thread::spawn(move || {
                let mut rng = 0x9E37_79B9_7F4A_7C15_u64.wrapping_mul(t as u64 + 1);
                let mut pool: Vec<Gc<RwLock<Node>>> = Vec::new();
                for i in 0..iters {
                    match xorshift(&mut rng) % 100 {
                        0..=39 => pool.push(Gc::new(RwLock::new(Node::default()))),
                        40..=69 => {
                            if pool.len() >= 2 {
                                let a = (xorshift(&mut rng) as usize) % pool.len();
                                let b = (xorshift(&mut rng) as usize) % pool.len();
                                let target = pool[b].clone();
                                pool[a].write().edges.push(target);
                            }
                        }
                        70..=84 => {
                            if !pool.is_empty() {
                                let k = (xorshift(&mut rng) as usize) % pool.len();
                                pool.swap_remove(k);
                            }
                        }
                        85..=94 => {
                            let mut ex = exchange.lock();
                            if xorshift(&mut rng) % 2 == 0 {
                                if let Some(n) = pool.last() {
                                    ex.push(n.clone());
                                }
                                if ex.len() > 64 {
                                    ex.remove(0);
                                }
                            } else if let Some(n) = ex.pop() {
                                pool.push(n);
                            }
                        }
                        _ => {
                            if i % 512 == 0 {
                                collect_garbage();
                            }
                        }
                    }
                }
            })
        })
        .collect();

    for t in threads {
        t.join().unwrap();
    }

    exchange.lock().clear();
    for _ in 0..4 {
        collect_garbage();
    }
}
