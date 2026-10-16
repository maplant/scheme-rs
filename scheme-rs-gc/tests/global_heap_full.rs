#![cfg(not(loom))]

use core::sync::atomic::Ordering;
use std::any::TypeId;

use scheme_rs_gc::{
    BLOCK_SIZE, HeapObject, VTable, budget, init_gc, set_heap_size, shutdown_gc, unroot,
};

struct Obj(#[allow(dead_code)] [u8; 1024]);

fn vtable() -> VTable {
    VTable {
        visit_children: |_, _| {},
        finalize: |_| {},
    }
}

#[test]
fn a_full_heap_collects_and_retries() {
    set_heap_size(2 * BLOCK_SIZE).unwrap();
    init_gc();
    assert_eq!(budget(), 2 * BLOCK_SIZE);
    for _ in 0..(16 * 2 * BLOCK_SIZE / size_of::<Obj>()) {
        unsafe {
            let obj = HeapObject::from_data(HeapObject::alloc(Obj([0; 1024])));
            unroot(obj, TypeId::of::<Obj>(), vtable);
            obj.rc().fetch_sub(1, Ordering::Release);
        }
    }
    shutdown_gc();
}
