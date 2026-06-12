//! An object freed by reference counting while parked in the collector's
//! pending cycle list left a dangling entry there (use-after-free). Churning
//! cycles whose lifetimes straddle collection epochs hits that window
//! thousands of times per run; without the purge in free() this crashes
//! intermittently. The Arc canary additionally verifies that cycles touched
//! by the purge are still fully reclaimed (no leak, no double-free).

use parking_lot::RwLock;
use scheme_rs::gc::{Gc, Trace, collect_garbage, init_gc};
use std::collections::VecDeque;
use std::sync::Arc;

#[derive(Default, Trace)]
struct Cyclic {
    next: Option<Gc<RwLock<Cyclic>>>,
    canary: Option<Arc<()>>,
}

fn node(canary: &Arc<()>) -> Gc<RwLock<Cyclic>> {
    Gc::new(RwLock::new(Cyclic {
        next: None,
        canary: Some(canary.clone()),
    }))
}

fn churn(iters: u64, canary: Arc<()>) {
    // Deep enough that pairs outlive an epoch (10k allocs) and get parked.
    const RING: usize = 8192;
    let mut ring: VecDeque<Gc<RwLock<Cyclic>>> = VecDeque::new();
    for _ in 0..iters {
        let a = node(&canary);
        let b = node(&canary);
        a.write().next = Some(b.clone());
        b.write().next = Some(a);
        ring.push_back(b);
        if ring.len() > RING {
            // Sever the cycle so members hit rc 0 while possibly parked.
            ring.pop_front().unwrap().write().next = None;
        }
    }
}

// Triangles: a -> b -> c -> a with only b externally held, so two members
// are internal and can be parked together; severing b -> c then frees c via
// the scan and a via the release() cascade.
fn churn3(iters: u64, canary: Arc<()>) {
    const RING: usize = 8192;
    let mut ring: VecDeque<Gc<RwLock<Cyclic>>> = VecDeque::new();
    for _ in 0..iters {
        let a = node(&canary);
        let b = node(&canary);
        let c = node(&canary);
        a.write().next = Some(b.clone());
        b.write().next = Some(c.clone());
        c.write().next = Some(a);
        ring.push_back(b);
        if ring.len() > RING {
            ring.pop_front().unwrap().write().next = None;
        }
    }
}

#[test]
fn cycle_uaf_churn() {
    init_gc();
    let canary = Arc::new(());
    let t = {
        let canary = canary.clone();
        std::thread::spawn(move || churn(5_000_000, canary))
    };
    let t3 = {
        let canary = canary.clone();
        std::thread::spawn(move || churn3(3_000_000, canary))
    };
    churn(5_000_000, canary.clone());
    t.join().unwrap();
    t3.join().unwrap();

    for _ in 0..4 {
        collect_garbage();
    }
    assert_eq!(Arc::strong_count(&canary), 1, "cyclic garbage leaked");
}
