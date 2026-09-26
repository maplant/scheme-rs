#![cfg(not(loom))]

mod common;

use core::alloc::Layout;
use std::sync::atomic::Ordering;

use common::*;
use scheme_rs_gc::{BLOCK_SIZE, LOS_MAX_SIZE, Mutator};

#[test]
fn capacity_rounds_up_to_whole_blocks() {
    assert_eq!(TestHeap::new(1).unwrap().capacity(), BLOCK_SIZE);
    assert_eq!(
        TestHeap::new(BLOCK_SIZE + 1).unwrap().capacity(),
        2 * BLOCK_SIZE
    );
}

#[test]
fn allocations_are_aligned_disjoint_and_inside_a_block() {
    let heap = test_heap();
    let mut m = heap.mutator();
    let mut ranges = Vec::new();
    for i in 0..5000 {
        let size = 16 + (i * 37) % 2000;
        let align = [8, 16, 32, 64][i % 4];
        let p = obj(&mut m, size, align).addr().get();
        assert_eq!(p % align, 0);
        assert!(p % BLOCK_SIZE + size <= BLOCK_SIZE);
        ranges.push((p, p + size));
    }
    ranges.sort();
    for pair in ranges.windows(2) {
        assert!(pair[0].1 <= pair[1].0, "overlap: {pair:?}");
    }
}

#[test]
fn small_objects_share_a_block() {
    let heap = test_heap();
    let mut m = heap.mutator();
    let first = block_of(obj(&mut m, 32, 16));
    for _ in 0..100 {
        assert_eq!(block_of(obj(&mut m, 32, 16)), first);
    }
    assert_eq!(heap.stats().free_blocks, total_blocks(&heap) - 1);
}

#[test]
fn the_region_and_large_objects_come_from_the_allocator() {
    let counting = Counting::default();
    let heap = TestHeap::new_in(&counting, HEAP_BYTES).unwrap();
    assert_eq!(counting.allocs(), 1);
    let mut m = heap.mutator();
    obj(&mut m, 32, 16);
    assert_eq!(counting.allocs(), 1);
    obj(&mut m, LOS_MAX_SIZE + 16, 16);
    obj(&mut m, 32, 128);
    assert_eq!(counting.allocs(), 3);
    assert_eq!(heap.stats().large_objects, 2);
}

#[test]
fn allocator_failure_is_reported_and_recoverable() {
    let counting = Counting::default();
    counting.fail.store(true, Ordering::Relaxed);
    assert!(TestHeap::new_in(&counting, HEAP_BYTES).is_err());
    counting.fail.store(false, Ordering::Relaxed);
    let heap = TestHeap::new_in(&counting, HEAP_BYTES).unwrap();
    let mut m = heap.mutator();
    let large = Layout::from_size_align(LOS_MAX_SIZE + 16, 16).unwrap();
    counting.fail.store(true, Ordering::Relaxed);
    assert!(m.alloc(large).is_err());
    counting.fail.store(false, Ordering::Relaxed);
    assert!(m.alloc(large).is_ok());
}

#[test]
fn mutator_is_send() {
    fn assert_send<T: Send>() {}
    assert_send::<Mutator<'_, TestModel>>();
}

#[test]
fn dropping_the_heap_returns_the_region() {
    let counting = Counting::default();
    {
        let heap = TestHeap::new_in(&counting, HEAP_BYTES).unwrap();
        for _ in 0..3 {
            full_block(&heap);
        }
    }
    assert_eq!(counting.allocs(), counting.deallocs());
}
