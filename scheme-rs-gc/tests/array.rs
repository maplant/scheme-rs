#![cfg(not(loom))]

mod common;

use std::sync::atomic::{AtomicUsize, Ordering};

use scheme_rs_gc::{
    BLOCK_SIZE, CycleCollector, CycleHeader, Heap, HeapArray, HeapObject, Trace,
    collect_garbage_in, start_collector,
};

static FINALIZED: AtomicUsize = AtomicUsize::new(0);

struct Counted;

unsafe impl Trace for Counted {
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {}

    unsafe fn finalize(&mut self) {
        FINALIZED.fetch_add(1, Ordering::Relaxed);
    }
}

#[test]
fn zero_length_array_is_allowed() {
    let heap = Box::leak(Box::new(Heap::<CycleHeader>::new(4 * BLOCK_SIZE).unwrap()));
    let mut mutator = heap.mutator();
    let arr = HeapArray::<usize>::from_iter_in(&mut mutator, []).unwrap();
    assert_eq!(arr.len(), 0)
}

#[test]
fn dropped_arrays_are_finalized() {
    let heap = Box::leak(Box::new(Heap::<CycleHeader>::new(4 * BLOCK_SIZE).unwrap()));
    let _gc = start_collector::<CycleCollector>(heap).unwrap();
    let mut mutator = heap.mutator();
    let arr = HeapArray::<Counted>::from_iter_in(&mut mutator, (0..5).map(|_| Counted)).unwrap();

    for _ in 0..3 {
        collect_garbage_in(heap);
    }

    assert_eq!(FINALIZED.load(Ordering::Relaxed), 0, "finalized while held");
    drop(arr);

    for _ in 0..3 {
        collect_garbage_in(heap);
    }

    assert_eq!(
        FINALIZED.load(Ordering::Relaxed),
        5,
        "Something went unfinalized"
    );
}
