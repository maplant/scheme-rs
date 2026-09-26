#![allow(dead_code)]

use core::{alloc::Layout, ptr::NonNull};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use scheme_rs_gc::{
    AllocError, Allocator, BLOCK_SIZE, Global, Heap, LINE_SIZE, Mutator, ObjectModel,
};

/// Test objects keep their layout in their first 16 bytes.
pub struct TestModel;

unsafe impl ObjectModel for TestModel {
    type Header = Layout;

    fn layout(header: &Layout) -> Layout {
        *header
    }
}

pub type TestHeap<A = Global> = Heap<TestModel, A>;

pub const LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;

pub const HEAP_BYTES: usize = 16 << 20;

pub fn test_heap() -> TestHeap {
    TestHeap::new(HEAP_BYTES).unwrap()
}

pub fn total_blocks<A: Allocator>(heap: &TestHeap<A>) -> usize {
    heap.capacity() / BLOCK_SIZE
}

pub fn stamp(obj: NonNull<u8>, layout: Layout) -> NonNull<u8> {
    unsafe { obj.cast::<Layout>().write(layout) };
    obj
}

pub fn obj<A: Allocator>(
    m: &mut Mutator<'_, TestModel, A>,
    size: usize,
    align: usize,
) -> NonNull<u8> {
    assert!(size >= size_of::<Layout>() && align >= align_of::<Layout>());
    let layout = Layout::from_size_align(size, align).unwrap();
    stamp(m.alloc(layout).unwrap(), layout)
}

pub fn block_of(obj: NonNull<u8>) -> usize {
    obj.addr().get() & !(BLOCK_SIZE - 1)
}

/// Fills one fresh block with one-line objects and retires it.
pub fn full_block<A: Allocator>(heap: &TestHeap<A>) -> Vec<NonNull<u8>> {
    let mut m = heap.mutator();
    let objs: Vec<_> = (0..LINES_PER_BLOCK)
        .map(|_| obj(&mut m, LINE_SIZE, 16))
        .collect();
    assert!(objs.iter().all(|&o| block_of(o) == block_of(objs[0])));
    objs
}

/// Forwards to `Global`, counting calls. Can be told to fail.
#[derive(Default)]
pub struct Counting {
    allocs: AtomicUsize,
    deallocs: AtomicUsize,
    pub fail: AtomicBool,
}

impl Counting {
    pub fn allocs(&self) -> usize {
        self.allocs.load(Ordering::Relaxed)
    }

    pub fn deallocs(&self) -> usize {
        self.deallocs.load(Ordering::Relaxed)
    }
}

unsafe impl Allocator for &Counting {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        if self.fail.load(Ordering::Relaxed) {
            return Err(AllocError);
        }
        self.allocs.fetch_add(1, Ordering::Relaxed);
        Global.allocate(layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        self.deallocs.fetch_add(1, Ordering::Relaxed);
        unsafe { Global.deallocate(ptr, layout) }
    }
}
