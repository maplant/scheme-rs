#![cfg(not(loom))]
mod common;

use common::*;
use std::alloc::Layout;

use scheme_rs_gc::{BLOCK_SIZE, LINE_SIZE, LOS_MAX_SIZE};

#[test]
fn blocks_and_large_objects_share_the_budget() {
    let heap = TestHeap::new(4 * BLOCK_SIZE).unwrap();
    let mut m = heap.mutator();

    // goes in to the large region
    let large = Layout::from_size_align(3 * BLOCK_SIZE, 16).unwrap();
    let _ = stamp(m.alloc(large).unwrap(), large);

    // One block of budget is left: one block's worth of small objects fits
    for _ in 0..BLOCK_SIZE / LINE_SIZE {
        init_obj(&mut m, LINE_SIZE, 16);
    }

    assert!(
        m.alloc(Layout::from_size_align(LINE_SIZE, 16).unwrap())
            .is_err()
    );
}

#[test]
fn a_large_object_does_not_fit_when_blocks_use_the_budget() {
    let heap = TestHeap::new(2 * BLOCK_SIZE).unwrap();
    let mut m = heap.mutator();

    for _ in 0..2 * BLOCK_SIZE / LINE_SIZE {
        init_obj(&mut m, LINE_SIZE, 16);
    }

    let large = Layout::from_size_align(LOS_MAX_SIZE + 16, 16).unwrap();
    assert!(m.alloc(large).is_err());
}

#[test]
fn the_budget_is_increased_when_a_large_object_is_released() {
    let heap = TestHeap::new(4 * BLOCK_SIZE).unwrap();
    let mut m = heap.mutator();
    let big = Layout::from_size_align(4 * BLOCK_SIZE, 16).unwrap();
    let p = stamp(m.alloc(big).unwrap(), big); // fills the whole budget
    assert!(m.alloc(big).is_err()); // a second one does not fit
    let mut r = heap.reclaimer();
    unsafe { r.free(p) }; // free the first one
    r.sweep();
    assert!(m.alloc(big).is_err()); // still in quarantine
    r.sweep();
    assert!(m.alloc(big).is_ok());
}

#[test]
fn large_objects_with_high_alignment_are_aligned() {
    let heap = test_heap();
    let mut m = heap.mutator();
    let p = init_obj(&mut m, 64, 128); // goes to large space because of alignmetn
    assert_eq!(p.addr().get() % 128, 0);
    let q = init_obj(&mut m, 2 * LOS_MAX_SIZE, 8192);
    assert_eq!(q.addr().get() % 8192, 0);
}

#[test]
fn large_objects_with_over_32k_alignment_are_rejected() {
    let heap = test_heap();
    let mut m = heap.mutator();
    let layout = Layout::from_size_align(16, BLOCK_SIZE * 2).expect("Test failed");
    let err = m.alloc(layout);
    assert!(err.is_err());
    assert_eq!(heap.stats().budget_used, 0);
}
