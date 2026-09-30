#![cfg(not(loom))]

use core::{
    alloc::Layout,
    cell::UnsafeCell,
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
};
use std::{alloc::alloc, any::TypeId};

use scheme_rs_gc::{GcHeader, OpaqueGcPtr, VTable, collect_garbage, init_gc, unroot};

struct Node {
    next: Option<OpaqueGcPtr>,
}

#[repr(C)]
struct Obj {
    header: UnsafeCell<GcHeader>,
    data: UnsafeCell<Node>,
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

fn new_node() -> OpaqueGcPtr {
    let layout = Layout::new::<Obj>();
    unsafe {
        let obj = alloc(layout) as *mut Obj;
        obj.write(Obj {
            header: UnsafeCell::new(GcHeader::new(layout)),
            data: UnsafeCell::new(Node { next: None }),
        });
        let header = NonNull::from(&(*obj).header);
        let data = NonNull::new_unchecked((*obj).data.get() as *mut UnsafeCell<()>);
        unroot(header.cast(), TypeId::of::<Node>(), node_vtable, layout);
        OpaqueGcPtr::new(header, data)
    }
}

unsafe fn data(obj: OpaqueGcPtr) -> *mut Node {
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
}
