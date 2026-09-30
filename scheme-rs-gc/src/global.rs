use core::{alloc::Layout, cell::RefCell, ptr::NonNull};
use std::{
    alloc::handle_alloc_error,
    env,
    sync::{
        OnceLock,
        atomic::{AtomicUsize, Ordering},
    },
};

use crate::{GcHeader, Heap, Mutator, ObjectModel, collect_garbage};

const DEFAULT_HEAP_SIZE: usize = 512 * 1024 * 1024;

/// Reads an object's layout from the cycle collector's header.
pub(crate) struct HeaderModel;

unsafe impl ObjectModel for HeaderModel {
    type Header = GcHeader;

    fn layout(header: &GcHeader) -> Layout {
        header.layout()
    }
}

static HEAP: OnceLock<Heap<HeaderModel>> = OnceLock::new();
static HEAP_SIZE: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub struct HeapAlreadyCreated;

/// Sets the heap size in bytes. Must be called before the first `Gc`
/// allocation.
pub fn set_heap_size(bytes: usize) -> Result<(), HeapAlreadyCreated> {
    if HEAP.get().is_some() {
        return Err(HeapAlreadyCreated);
    }
    HEAP_SIZE.store(bytes, Ordering::Relaxed);
    Ok(())
}

pub(crate) fn heap() -> &'static Heap<HeaderModel> {
    HEAP.get_or_init(|| {
        let bytes = match HEAP_SIZE.load(Ordering::Relaxed) {
            0 => env::var("SCHEME_RS_HEAP_SIZE")
                .ok()
                .map(|v| {
                    v.parse()
                        .expect("SCHEME_RS_HEAP_SIZE must be a number of bytes")
                })
                .unwrap_or(DEFAULT_HEAP_SIZE),
            bytes => bytes,
        };
        Heap::new(bytes).expect("could not reserve the GC heap")
    })
}

/// Bytes of block space in the global heap.
pub fn heap_capacity() -> usize {
    heap().capacity()
}

thread_local! {
    static MUTATOR: RefCell<Mutator<'static, HeaderModel>> = RefCell::new(heap().mutator());
}

fn try_alloc(layout: Layout) -> Option<NonNull<u8>> {
    MUTATOR
        .try_with(|m| m.borrow_mut().alloc(layout).ok())
        .expect("Gc allocated after this thread's GC mutator was destroyed")
}

/// Allocates a `Gc` object. When the heap is full, forces two collections
/// (freed lines are reusable only after two sweeps) and retries once.
pub fn alloc(layout: Layout) -> NonNull<u8> {
    if let Some(obj) = try_alloc(layout) {
        return obj;
    }
    collect_garbage();
    collect_garbage();
    try_alloc(layout).unwrap_or_else(|| handle_alloc_error(layout))
}
