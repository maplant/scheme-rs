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

#[test]
fn inc_event_or_counted() {
    loom::model(|| {
        // rc = 1, Black. Mutator clones; collector grays and reads rc for crc.
        let node = Node::with_state(1);

        let n1 = node.clone();
        let mutator = thread::spawn(move || {
            // Mirrors inc_rc + record_inc_event.
            let old = GcState(n1.state.fetch_add(1, Ordering::Relaxed));
            if old.color() != Color::Black {
                n1.state.fetch_or(INC_EVENT | ATTN_CLAIM, Ordering::AcqRel);
            }
        });

        let n2 = node.clone();
        let collector = thread::spawn(move || {
            // Mirrors mark_gray: gray transition, then fresh rc read for crc.
            n2.state
                .fetch_update(Ordering::AcqRel, Ordering::Acquire, |w| {
                    Some(GcState(w).with_color(Color::Gray).0)
                })
                .unwrap();
            GcState(n2.state.load(Ordering::Acquire)).rc()
        });

        mutator.join().unwrap();
        let crc_basis = collector.join().unwrap();

        let end = GcState(node.state.load(Ordering::Acquire));
        assert!(
            crc_basis == 2 || end.inc_event(),
            "increment must be counted in the collector's crc basis or \
             recorded as an event — invisible increments are the Scenario 1 bug"
        );
    });
}

#[test]
fn release_cas_never_loses_a_dec() {
    loom::model(|| {
        // rc = 2, claimed, being processed (as if drained this epoch).
        let node = Node::with_state(2 | ATTN_CLAIM);
        let head = Arc::new(AtomicUsize::new(0));

        let n1 = node.clone();
        let h1 = head.clone();
        let mutator = thread::spawn(move || mutator_dec(&h1, &n1));

        let n2 = node.clone();
        let h2 = head.clone();
        let collector = thread::spawn(move || {
            // Mirrors process_drained's release: re-validate then clear.
            let w = GcState(n2.state.load(Ordering::Acquire));
            n2.attn_next.store(NOT_IN_LIST, Ordering::Relaxed);
            let cleared = w.0 & !(ATTN_CLAIM | INC_EVENT);
            let released = n2
                .state
                .compare_exchange(w.0, cleared, Ordering::AcqRel, Ordering::Acquire)
                .is_ok();
            if !released {
                push(&h2, &n2);
            }
            (w, released)
        });

        mutator.join().unwrap();
        let (w, released) = collector.join().unwrap();

        let end = GcState(node.state.load(Ordering::Acquire));
        // The dec must be observable somewhere: in the word the collector
        // validated against (w.rc()==1), or in renewed membership (the
        // mutator re-claimed after release, or the collector re-pushed).
        let dec_seen_by_release = released && w.rc() == 1;
        let membership_renewed = end.attn_claimed();
        assert!(
            dec_seen_by_release || membership_renewed,
            "a dec landed with no trace: released on stale word without re-enqueue"
        );
        assert_eq!(end.rc(), 1);
    });
}

#[test]
fn fused_release_never_loses_a_dec() {
    loom::model(|| {
        // rc = 2, claimed, Black, being processed as a purple candidate.
        let node = Node::with_state(2 | ATTN_CLAIM);
        let head = Arc::new(AtomicUsize::new(0));

        let n1 = node.clone();
        let h1 = head.clone();
        let mutator = thread::spawn(move || mutator_dec(&h1, &n1));

        let n2 = node.clone();
        let h2 = head.clone();
        let collector = thread::spawn(move || {
            // Mirrors process_drained's fused release CAS: color mutation
            // and membership release happen in the same compare_exchange.
            let w = GcState(n2.state.load(Ordering::Acquire));
            n2.attn_next.store(NOT_IN_LIST, Ordering::Relaxed);
            let target = GcState(w.0 & !(ATTN_CLAIM | INC_EVENT))
                .with_color(Color::Purple)
                .0;
            let released = n2
                .state
                .compare_exchange(w.0, target, Ordering::AcqRel, Ordering::Acquire)
                .is_ok();
            if !released {
                push(&h2, &n2);
            }
            (w, released)
        });

        mutator.join().unwrap();
        let (w, released) = collector.join().unwrap();

        let end = GcState(node.state.load(Ordering::Acquire));
        let dec_seen_by_release = released && w.rc() == 1;
        let membership_renewed = end.attn_claimed();
        assert!(
            dec_seen_by_release || membership_renewed,
            "fused release lost a dec"
        );
        assert_eq!(end.rc(), 1);
        if released && !membership_renewed {
            assert_eq!(end.color(), Color::Purple, "fused color write lost");
        }
    });
}

#[test]
fn drain_race_never_loses_a_node() {
    loom::model(|| {
        // rc = 1, claim freshly won by the mutator; push races the drain swap.
        let node = Node::with_state(1 | ATTN_CLAIM);
        let head = Arc::new(AtomicUsize::new(0));

        let n1 = node.clone();
        let h1 = head.clone();
        let pusher = thread::spawn(move || push(&h1, &n1));

        let h2 = head.clone();
        let drainer = thread::spawn(move || h2.swap(0, Ordering::Acquire));

        pusher.join().unwrap();
        let drained_chain = drainer.join().unwrap();
        let residual_chain = head.swap(0, Ordering::Acquire);

        let total = occurrences(drained_chain, &node) + occurrences(residual_chain, &node);
        assert_eq!(
            total, 1,
            "node must land in exactly one of: this epoch's drain, the next stack"
        );
    });
}
