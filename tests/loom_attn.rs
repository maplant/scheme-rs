#![cfg(loom)]
//! Loom models of the attention-list protocol (design doc §2–§3).
//!
//! KEEP IN SYNC: the RMW sequences below mirror `record_dec_event`,
//! `record_inc_event` (src/gc/collection.rs) and the release CAS in
//! `process_drained`. The word math itself is imported from gc::state,
//! so bit-layout changes cannot drift; sequence changes must be mirrored.

use loom::sync::Arc;
use loom::sync::atomic::{AtomicUsize, Ordering};
use loom::thread;
use scheme_rs::gc::state::{ATTN_CLAIM, Color, GcState, INC_EVENT};

const NOT_IN_LIST: usize = 1;

struct Node {
    state: AtomicUsize,
    attn_next: AtomicUsize,
}

impl Node {
    fn with_state(word: usize) -> Arc<Self> {
        Arc::new(Node {
            state: AtomicUsize::new(word),
            attn_next: AtomicUsize::new(NOT_IN_LIST),
        })
    }
}

/// Mirrors `attn_push`.
fn push(head: &AtomicUsize, node: &Node) {
    let mut h = head.load(Ordering::Relaxed);
    loop {
        node.attn_next.store(h, Ordering::Relaxed);
        match head.compare_exchange_weak(
            h,
            node as *const Node as usize,
            Ordering::Release,
            Ordering::Relaxed,
        ) {
            Ok(_) => return,
            Err(actual) => h = actual,
        }
    }
}

/// Mirrors `record_dec_event` (including the fetch_sub that precedes it).
fn mutator_dec(head: &AtomicUsize, node: &Node) -> bool {
    let old = GcState(node.state.fetch_sub(1, Ordering::Release));
    if old.attn_claimed() {
        return false;
    }
    let w = GcState(node.state.fetch_or(ATTN_CLAIM, Ordering::AcqRel));
    if !w.attn_claimed() {
        push(head, node);
        return true;
    }
    false
}

/// Walk a swapped-out chain, counting how often `node` appears.
fn occurrences(mut chain: usize, node: &Node) -> usize {
    let mut count = 0;
    while chain != 0 {
        if chain == node as *const Node as usize {
            count += 1;
        }
        chain = unsafe { (*(chain as *const Node)).attn_next.load(Ordering::Relaxed) };
    }
    count
}

#[test]
fn claim_is_exactly_once() {
    loom::model(|| {
        // rc = 3, Black, unclaimed: two concurrent final-ish decs.
        let node = Node::with_state(3);
        let head = Arc::new(AtomicUsize::new(0));

        let n1 = node.clone();
        let h1 = head.clone();
        let t1 = thread::spawn(move || mutator_dec(&h1, &n1));
        let n2 = node.clone();
        let h2 = head.clone();
        let t2 = thread::spawn(move || mutator_dec(&h2, &n2));

        let pushed = t1.join().unwrap() as usize + t2.join().unwrap() as usize;
        assert_eq!(pushed, 1, "exactly one thread wins the claim");

        let chain = head.swap(0, Ordering::Acquire);
        assert_eq!(occurrences(chain, &node), 1, "node on the list exactly once");

        let end = GcState(node.state.load(Ordering::Acquire));
        assert!(end.attn_claimed());
        assert_eq!(end.rc(), 1);
    });
}
