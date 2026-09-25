#![cfg(not(loom))]

mod common;

use core::alloc::Layout;
use std::sync::atomic::Ordering;

use common::*;
use scheme_rs_gc::{BLOCK_SIZE, LINE_SIZE, LOS_MAX_SIZE, META_LINES, Mutator};

#[test]
fn allocations_are_aligned_disjoint_and_inside_bump_space() {
    let heap = TestHeap::new();
    let mut m = heap.mutator();
    let mut ranges = Vec::new();
    for i in 0..5000 {
        let size = 16 + (i * 37) % 2000;
        let align = [8, 16, 32, 64][i % 4];
        let p = obj(&mut m, size, align).addr().get();
        assert_eq!(p % align, 0);
        let offset = p % BLOCK_SIZE;
        assert!(offset >= META_LINES * LINE_SIZE && offset + size <= BLOCK_SIZE);
        ranges.push((p, p + size));
    }
    ranges.sort();
    for pair in ranges.windows(2) {
        assert!(pair[0].1 <= pair[1].0, "overlap: {pair:?}");
    }
}

#[test]
fn small_objects_share_a_block() {
    let counting = Counting::default();
    let heap = TestHeap::new_in(&counting);
    let mut m = heap.mutator();
    for _ in 0..100 {
        obj(&mut m, 32, 16);
    }
    assert_eq!(counting.allocs(), 1);
}

#[test]
fn large_and_overaligned_objects_come_from_the_allocator() {
    let counting = Counting::default();
    let heap = TestHeap::new_in(&counting);
    let mut m = heap.mutator();
    obj(&mut m, LOS_MAX_SIZE + 16, 16);
    assert_eq!(counting.allocs(), 1);
    obj(&mut m, 32, 128);
    assert_eq!(counting.allocs(), 2);
    assert_eq!(heap.stats().large_objects, 2);
}

#[test]
fn allocator_failure_is_reported_and_recoverable() {
    let counting = Counting::default();
    let heap = TestHeap::new_in(&counting);
    let mut m = heap.mutator();
    let layout = Layout::from_size_align(32, 16).unwrap();
    counting.fail.store(true, Ordering::Relaxed);
    assert!(m.alloc(layout).is_err());
    counting.fail.store(false, Ordering::Relaxed);
    assert!(m.alloc(layout).is_ok());
}

#[test]
fn mutator_is_send() {
    fn assert_send<T: Send>() {}
    assert_send::<Mutator<'_, TestModel>>();
}

#[test]
fn freed_lines_are_reused_only_after_the_next_sweep() {
    let heap = TestHeap::new();
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
fn a_fully_freed_block_is_reused_without_a_new_allocation() {
    let counting = Counting::default();
    let heap = TestHeap::new_in(&counting);
    let objs = full_block(&heap);
    let mut c = heap.collector();
    for p in objs {
        unsafe { c.free(p) };
    }
    c.sweep();
    assert_eq!(heap.stats().free_blocks, 0);
    c.sweep();
    assert_eq!(heap.stats().free_blocks, 1);
    let blocks = counting.allocs();
    obj(&mut heap.mutator(), 32, 16);
    assert_eq!(counting.allocs(), blocks);
}

#[test]
fn freeing_a_large_object_returns_it_to_the_allocator() {
    let counting = Counting::default();
    let heap = TestHeap::new_in(&counting);
    let p = obj(&mut heap.mutator(), LOS_MAX_SIZE + 16, 16);
    unsafe { heap.collector().free(p) };
    assert_eq!(counting.deallocs(), 1);
    assert_eq!(heap.stats().large_objects, 0);
}

#[test]
fn a_block_both_retired_and_freed_into_is_published_once() {
    let heap = TestHeap::new();
    let objs = full_block(&heap);
    let mut c = heap.collector();
    unsafe { c.free(objs[0]) };
    c.sweep();
    c.sweep();
    assert_eq!(heap.stats().recycled_blocks, 1);
}

#[test]
fn medium_objects_overflow_without_discarding_holes() {
    let heap = TestHeap::new();
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
fn release_to_os_keeps_the_requested_free_blocks() {
    let counting = Counting::default();
    {
        let heap = TestHeap::new_in(&counting);
        let blocks: Vec<_> = (0..3).map(|_| full_block(&heap)).collect();
        let mut c = heap.collector();
        for p in blocks.into_iter().flatten() {
            unsafe { c.free(p) };
        }
        c.sweep();
        c.sweep();
        assert_eq!(heap.stats().free_blocks, 3);
        c.release_to_os(1);
        let stats = heap.stats();
        assert_eq!((stats.free_blocks, stats.blocks_released), (1, 2));
        assert_eq!(counting.deallocs(), 2);
    }
    assert_eq!(counting.allocs(), counting.deallocs());
}

#[test]
fn dropping_the_heap_returns_every_block() {
    let counting = Counting::default();
    {
        let heap = TestHeap::new_in(&counting);
        for _ in 0..3 {
            full_block(&heap);
        }
    }
    assert_eq!(counting.allocs(), counting.deallocs());
}

#[test]
fn heap_alloc_works_without_a_mutator() {
    let heap = TestHeap::new();
    let layout = Layout::from_size_align(32, 16).unwrap();
    let p = stamp(heap.alloc(layout).unwrap(), layout);
    let q = stamp(heap.alloc(layout).unwrap(), layout);
    let (p_addr, q_addr) = (p.addr().get(), q.addr().get());
    assert!(p_addr + 32 <= q_addr || q_addr + 32 <= p_addr);
    let mut c = heap.collector();
    unsafe { c.free(p) };
    c.sweep();
    assert_eq!(heap.stats().bytes_allocated, 64);
}

#[test]
#[should_panic(expected = "collector already taken")]
fn a_second_collector_panics() {
    let heap = TestHeap::new();
    let _c = heap.collector();
    let _d = heap.collector();
}
