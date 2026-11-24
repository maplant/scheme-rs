use core::sync::atomic::{AtomicUsize, Ordering};
use std::{alloc::Layout, any::TypeId};

use scheme_rs_gc::{
    BLOCK_SIZE, CycleCollector, CycleHeader, GcWatching, Heap, HeapObject, Mutator, VTable,
    collect_garbage_in, start_collector, unroot_in, with_gc_watching,
};

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

fn new_node(
    heap: &Heap<CycleHeader>,
    mutator: &mut Mutator<'_, CycleHeader>,
    watching: &mut GcWatching,
) -> HeapObject {
    unsafe {
        let data = HeapObject::alloc_layout_in(mutator, watching, Layout::new::<Node>())
            .unwrap()
            .cast::<Node>();
        data.write(Node { next: None });
        let obj = HeapObject::from_data(data);
        unroot_in(heap, obj, TypeId::of::<Node>(), node_vtable);
        obj
    }
}

unsafe fn data(obj: HeapObject) -> *mut Node {
    unsafe { obj.data_mut() as *mut Node }
}

#[test]
fn an_unreachable_cycle_is_finalized() {
    let heap = Box::leak(Box::new(Heap::<CycleHeader>::new(4 * BLOCK_SIZE).unwrap()));
    let _gc = start_collector::<CycleCollector>(heap).unwrap();
    let mut mutator = heap.mutator();
    let (a, b) = with_gc_watching(|w| {
        (
            new_node(heap, &mut mutator, w),
            new_node(heap, &mut mutator, w),
        )
    });
    unsafe {
        (*data(a)).next = Some(b);
        b.rc().fetch_add(1, Ordering::Relaxed);
        (*data(b)).next = Some(a);
        a.rc().fetch_add(1, Ordering::Relaxed);
        a.rc().fetch_sub(1, Ordering::Release);
        b.rc().fetch_sub(1, Ordering::Release);
    }
    for _ in 0..3 {
        collect_garbage_in(heap);
    }
    assert_eq!(FINALIZED.load(Ordering::Relaxed), 2);
}
