#![cfg(not(loom))]

mod common;

use core::alloc::Layout;
use std::sync::atomic::Ordering;

use common::*;
use scheme_rs_gc::{BLOCK_SIZE, LINE_SIZE, LOS_MAX_SIZE, Mutator};

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
fn a_full_heap_fails_and_recovers_after_a_free() {
    let heap = TestHeap::new(2 * BLOCK_SIZE).unwrap();
    let first = full_block(&heap);
    full_block(&heap);
    let layout = Layout::from_size_align(32, 16).unwrap();
    assert!(heap.mutator().alloc(layout).is_err());
    let mut c = heap.collector();
    unsafe { c.free(first[0]) };
    c.sweep();
    c.sweep();
    assert_eq!(obj(&mut heap.mutator(), LINE_SIZE, 16), first[0]);
}

#[test]
fn mutator_is_send() {
    fn assert_send<T: Send>() {}
    assert_send::<Mutator<'_, TestModel>>();
}

#[test]
fn freed_lines_are_reused_only_after_the_next_sweep() {
    let heap = test_heap();
    let x = full_block(&heap)[0];
    let mut c = heap.collector();
    unsafe { c.free(x) };
    c.sweep();
    {
        let mut m2 = heap.mutator();
        assert_ne!(obj(&mut m2, LINE_SIZE, 16), x);
    }
    c.sweep();
    let mut m3 = heap.mutator();
    assert_eq!(obj(&mut m3, LINE_SIZE, 16), x);
}

#[test]
fn a_fully_freed_block_is_reused_before_a_fresh_one() {
    let heap = test_heap();
    let total = total_blocks(&heap);
    let objs = full_block(&heap);
    let freed = block_of(objs[0]);
    let mut c = heap.collector();
    for p in objs {
        unsafe { c.free(p) };
    }
    c.sweep();
    assert_eq!(heap.stats().free_blocks, total - 1);
    c.sweep();
    assert_eq!(heap.stats().free_blocks, total);
    assert_eq!(block_of(obj(&mut heap.mutator(), 32, 16)), freed);
}

#[test]
fn freeing_a_large_object_returns_it_to_the_allocator() {
    let counting = Counting::default();
    let heap = TestHeap::new_in(&counting, HEAP_BYTES).unwrap();
    let p = obj(&mut heap.mutator(), LOS_MAX_SIZE + 16, 16);
    unsafe { heap.collector().free(p) };
    assert_eq!(counting.deallocs(), 1);
    assert_eq!(heap.stats().large_objects, 0);
}

#[test]
#[should_panic(expected = "pointer is not in this heap")]
fn freeing_a_pointer_from_another_heap_panics() {
    let a = test_heap();
    let b = test_heap();
    let p = obj(&mut a.mutator(), 32, 16);
    unsafe { b.collector().free(p) };
}

#[test]
fn medium_objects_overflow_without_discarding_holes() {
    let heap = test_heap();
    let objs = full_block(&heap);
    let mut c = heap.collector();
    for &p in objs.iter().step_by(2) {
        unsafe { c.free(p) };
    }
    c.sweep();
    c.sweep();
    let recycled = block_of(objs[0]);
    let mut m2 = heap.mutator();
    assert_eq!(block_of(obj(&mut m2, 32, 16)), recycled);
    assert_ne!(block_of(obj(&mut m2, 4 * LINE_SIZE, 16)), recycled);
    assert_eq!(block_of(obj(&mut m2, 32, 16)), recycled);
    drop(m2);
    assert!(heap.stats().overflow_bytes >= 4 * LINE_SIZE);
}

#[test]
fn a_recycled_block_that_empties_becomes_free() {
    let heap = test_heap();
    let objs = full_block(&heap);
    let (first, rest) = objs.split_at(1);
    let mut c = heap.collector();
    unsafe { c.free(first[0]) };
    c.sweep();
    c.sweep();
    assert_eq!(heap.stats().recycled_blocks, 1);
    for &p in rest {
        unsafe { c.free(p) };
    }
    c.sweep();
    c.sweep();
    let stats = heap.stats();
    assert_eq!(
        (stats.recycled_blocks, stats.free_blocks),
        (0, total_blocks(&heap))
    );
}

#[test]
fn a_free_into_a_quarantined_block_is_found() {
    let heap = test_heap();
    let objs = full_block(&heap);
    let mut c = heap.collector();
    unsafe { c.free(objs[0]) };
    c.sweep();
    // The block now waits in quarantine with line 0 free.
    unsafe { c.free(objs[1]) };
    c.sweep();
    c.sweep();
    let mut m = heap.mutator();
    assert_eq!(obj(&mut m, LINE_SIZE, 16), objs[0]);
    assert_eq!(obj(&mut m, LINE_SIZE, 16), objs[1]);
}

#[test]
fn a_block_both_retired_and_freed_into_is_published_once() {
    let heap = test_heap();
    let objs = full_block(&heap);
    let mut c = heap.collector();
    unsafe { c.free(objs[0]) };
    c.sweep();
    c.sweep();
    assert_eq!(heap.stats().recycled_blocks, 1);
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

#[test]
fn a_temporary_mutator_hands_its_block_back() {
    let heap = test_heap();
    let p = obj(&mut heap.mutator(), 32, 16);
    let mut c = heap.collector();
    unsafe { c.free(p) };
    c.sweep();
    c.sweep();
    assert_eq!(heap.stats().free_blocks, total_blocks(&heap));
}

#[test]
#[should_panic(expected = "collector already taken")]
fn a_second_collector_panics() {
    let heap = test_heap();
    let _c = heap.collector();
    let _d = heap.collector();
}
