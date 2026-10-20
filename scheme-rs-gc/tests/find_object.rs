#![cfg(not(loom))]

mod common;

use core::ptr::NonNull;

use common::*;
use scheme_rs_gc::{GRANULE, LOS_MAX_SIZE};

fn at(obj: NonNull<u8>, offset: usize) -> usize {
    obj.addr().get() + offset
}

#[test]
fn find_object_of_starts_and_interiors_get_their_object() {
    let heap = test_heap();
    let mut m = heap.mutator();
    let a = init_obj(&mut m, 32, 16);
    let b = init_obj(&mut m, 1000, 16);
    let big = init_obj(&mut m, 2 * LOS_MAX_SIZE, 16);
    unsafe {
        assert_eq!(heap.find_object_of_addr(at(a, 0)), Some(a));
        assert_eq!(heap.find_object_of_addr(at(a, 31)), Some(a));
        assert_eq!(heap.find_object_of_addr(at(b, 0)), Some(b));
        assert_eq!(heap.find_object_of_addr(at(b, 999)), Some(b));
        assert_eq!(heap.find_object_of_addr(at(big, 0)), Some(big));
        assert_eq!(
            heap.find_object_of_addr(at(big, 2 * LOS_MAX_SIZE - 1)),
            Some(big)
        );
    }
}

#[test]
fn find_object_of_does_reject_addresses_outside_objects() {
    let heap = test_heap();
    let mut m = heap.mutator();
    let small = init_obj(&mut m, GRANULE, 16);
    // 16KB plus 16 bytes takes 5 pages; the rest of the last one is no object.
    let big = init_obj(&mut m, 2 * LOS_MAX_SIZE + 16, 16);
    let local = 0u64;
    unsafe {
        assert_eq!(heap.find_object_of_addr(at(small, GRANULE)), None);
        assert_eq!(
            heap.find_object_of_addr(at(big, 2 * LOS_MAX_SIZE + 16)),
            None
        );
        assert_eq!(heap.find_object_of_addr(1), None);
        assert_eq!(heap.find_object_of_addr(usize::MAX), None);
        assert_eq!(
            heap.find_object_of_addr(&local as *const u64 as usize),
            None
        );
    }
}

#[test]
fn find_object_of_ignores_freed_objects() {
    let heap = test_heap();
    let mut m = heap.mutator();
    let a = init_obj(&mut m, 64, 16);
    let first = init_obj(&mut m, 2 * LOS_MAX_SIZE, 16);
    let second = init_obj(&mut m, 2 * LOS_MAX_SIZE, 16);
    drop(m);
    let mut r = heap.reclaimer();
    unsafe {
        r.free(a);
        r.free(second);
        assert_eq!(heap.find_object_of_addr(at(a, 0)), None);
        assert_eq!(heap.find_object_of_addr(at(a, 8)), None);
        assert_eq!(heap.find_object_of_addr(at(second, 0)), None);
        assert_eq!(heap.find_object_of_addr(at(second, LOS_MAX_SIZE)), None);
        assert_eq!(heap.find_object_of_addr(at(first, 0)), Some(first));
    }
}
