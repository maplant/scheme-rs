#![cfg(not(loom))]

use core::{alloc::Layout, cell::UnsafeCell, sync::atomic::Ordering};
use std::any::TypeId;

use scheme_rs_gc::{BLOCK_SIZE, GcHeader, VTable, alloc, budget, init_gc, set_heap_size, unroot};

#[repr(C)]
struct Obj {
    header: UnsafeCell<GcHeader>,
    data: UnsafeCell<[u8; 1024]>,
}

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
    let layout = Layout::new::<Obj>();
    for _ in 0..(16 * 2 * BLOCK_SIZE / layout.size()) {
        let obj = alloc(layout).cast::<Obj>();
        unsafe {
            obj.write(Obj {
                header: UnsafeCell::new(GcHeader::new(layout)),
                data: UnsafeCell::new([0; 1024]),
            });
            unroot(obj.cast(), TypeId::of::<Obj>(), vtable, layout);
            (*obj.as_ref().header.get())
                .shared_rc()
                .fetch_sub(1, Ordering::Release);
        }
    }
}
