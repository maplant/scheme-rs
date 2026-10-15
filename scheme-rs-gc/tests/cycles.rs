#![cfg(not(loom))]

use core::sync::atomic::{AtomicUsize, Ordering};
use std::any::TypeId;

use scheme_rs_gc::{HeapObject, VTable, collect_garbage, init_gc, shutdown_gc, unroot};

struct Node {
    next: Option<HeapObject>,
}

static FINALIZED: AtomicUsize = AtomicUsize::new(0);

fn node_vtable() -> VTable {
    VTable {
        visit_children: |data, visitor| unsafe {
            if let Some(next) = (*(data as *const Node)).next {
                visitor(next);
            }
        },
        finalize: |_| {
            FINALIZED.fetch_add(1, Ordering::Relaxed);
        },
    }
}

fn new_node() -> HeapObject {
    let data = HeapObject::alloc(Node { next: None });
    unsafe {
        let obj = HeapObject::from_data(data);
        unroot(obj, TypeId::of::<Node>(), node_vtable);
        obj
    }
}

unsafe fn data(obj: HeapObject) -> *mut Node {
    unsafe { obj.data_mut() as *mut Node }
}

#[test]
fn an_unreachable_cycle_is_finalized() {
    init_gc();
    let a = new_node();
    let b = new_node();
    unsafe {
        (*data(a)).next = Some(b);
        b.rc().fetch_add(1, Ordering::Relaxed);
        (*data(b)).next = Some(a);
        a.rc().fetch_add(1, Ordering::Relaxed);
        a.rc().fetch_sub(1, Ordering::Release);
        b.rc().fetch_sub(1, Ordering::Release);
    }
    for _ in 0..3 {
        collect_garbage();
    }
    assert_eq!(FINALIZED.load(Ordering::Relaxed), 2);
    shutdown_gc();
}
