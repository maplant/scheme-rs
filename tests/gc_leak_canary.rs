//! Leak canary: every allocated object holds a clone of one Arc; after
//! dropping everything and forcing collections, the Arc count must return
//! to 1. A missed attention event shows up here as a stuck count.
#![cfg(not(feature = "async"))]

use parking_lot::{Mutex, RwLock};
use scheme_rs::gc::{Gc, Trace, collect_garbage, init_gc};
use std::sync::Arc;

#[derive(Default, Trace)]
struct Canary {
    token: Option<Arc<()>>,
    next: Option<Gc<RwLock<Canary>>>,
}

#[test]
fn all_garbage_is_reclaimed() {
    init_gc();
    let token = Arc::new(());

    let shared: Arc<Mutex<Vec<Gc<RwLock<Canary>>>>> = Arc::new(Mutex::new(Vec::new()));
    let threads: Vec<_> = (0..4)
        .map(|t| {
            let token = token.clone();
            let shared = shared.clone();
            std::thread::spawn(move || {
                let mut pool: Vec<Gc<RwLock<Canary>>> = Vec::new();
                for i in 0..2_500usize {
                    let node = Gc::new(RwLock::new(Canary {
                        token: Some(token.clone()),
                        next: None,
                    }));
                    // Link every third node to a random-ish earlier one
                    // (creates plenty of cycles).
                    if i % 3 == 0 && !pool.is_empty() {
                        let k = (i * 7 + t) % pool.len();
                        node.write().next = Some(pool[k].clone());
                        pool[k].write().next = Some(node.clone());
                    }
                    pool.push(node);
                    if i % 5 == 0 {
                        let mut s = shared.lock();
                        s.push(pool[pool.len() / 2].clone());
                        if s.len() > 32 {
                            s.remove(0);
                        }
                    }
                }
            })
        })
        .collect();
    for t in threads {
        t.join().unwrap();
    }

    shared.lock().clear();
    for _ in 0..8 {
        collect_garbage();
    }

    assert_eq!(
        Arc::strong_count(&token),
        1,
        "leaked {} objects (missed attention events?)",
        Arc::strong_count(&token) - 1
    );
}
