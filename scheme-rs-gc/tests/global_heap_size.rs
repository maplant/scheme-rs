#![cfg(not(loom))]

use core::alloc::Layout;

use scheme_rs_gc::{BLOCK_SIZE, alloc, heap_capacity, set_heap_size};

#[test]
fn set_heap_size_applies_before_first_use_and_fails_after() {
    set_heap_size(4 * BLOCK_SIZE).unwrap();
    alloc(Layout::new::<[u64; 4]>());
    assert_eq!(heap_capacity(), 4 * BLOCK_SIZE);
    assert!(set_heap_size(8 * BLOCK_SIZE).is_err());
}
