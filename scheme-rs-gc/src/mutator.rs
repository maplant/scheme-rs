use core::{alloc::Layout, ptr::NonNull};

use allocator_api2::alloc::{AllocError, Allocator, Global};

use crate::{
    Heap, ObjectModel,
    meta::{BUMP_LINES, Block, LINE_SIZE, MIN_SIZE, is_large, take_hole},
    sync::Ordering,
};

/// Bump window over the holes of one block.
#[derive(Default)]
struct Window {
    block: Option<Block>,
    /// Offsets into the block: the open hole is `cursor..limit`.
    cursor: usize,
    limit: usize,
    /// Holes not opened yet. A snapshot taken at acquisition.
    holes: u128,
}

impl Window {
    fn install(&mut self, block: Block, holes: u128) {
        *self = Window {
            block: Some(block),
            cursor: 0,
            limit: 0,
            holes,
        };
        let opened = self.open_next_hole();
        debug_assert!(opened, "installed a block with no holes");
    }

    fn take(&mut self) -> Option<Block> {
        core::mem::take(self).block
    }

    fn open_next_hole(&mut self) -> bool {
        match take_hole(&mut self.holes) {
            Some((start, end)) => {
                self.cursor = start * LINE_SIZE;
                self.limit = end * LINE_SIZE;
                true
            }
            None => false,
        }
    }

    fn bump(&mut self, layout: Layout) -> Option<NonNull<u8>> {
        let block = self.block?;
        let start = self.cursor.next_multiple_of(layout.align());
        let end = start + layout.size();
        if end > self.limit {
            return None;
        }
        self.cursor = end;
        block.on_alloc(start, layout.size());
        Some(unsafe { block.base().byte_add(start) })
    }
}

/// Allocation state of one mutator.
#[derive(Default)]
pub(crate) struct Bump {
    primary: Window,
    overflow: Window,
    bytes: usize,
    overflow_bytes: usize,
}

impl Bump {
    pub(crate) fn alloc<M: ObjectModel, A: Allocator>(
        &mut self,
        heap: &Heap<M, A>,
        layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        if is_large(layout) {
            return heap.alloc_large(layout);
        }
        let layout = Layout::from_size_align(layout.size().max(MIN_SIZE), layout.align())
            .expect("rounding up a valid layout");
        let obj = self.alloc_small(heap, layout)?;
        self.bytes += layout.size();
        Ok(obj)
    }

    fn alloc_small<M: ObjectModel, A: Allocator>(
        &mut self,
        heap: &Heap<M, A>,
        layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        loop {
            if let Some(obj) = self.primary.bump(layout) {
                return Ok(obj);
            }
            if layout.size() > LINE_SIZE && self.primary.holes != 0 {
                return self.alloc_overflow(heap, layout);
            }
            if !self.primary.open_next_hole() {
                if let Some(block) = self.primary.take() {
                    self.retire(heap, block);
                }
                let (block, holes) = heap.acquire()?;
                self.primary.install(block, holes);
            }
        }
    }

    /// Medium objects that do not fit the open hole go to a clean block, so
    /// the primary block's remaining holes are kept for small objects.
    fn alloc_overflow<M: ObjectModel, A: Allocator>(
        &mut self,
        heap: &Heap<M, A>,
        layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        let obj = match self.overflow.bump(layout) {
            Some(obj) => obj,
            None => {
                if let Some(block) = self.overflow.take() {
                    self.retire(heap, block);
                }
                self.overflow.install(heap.acquire_clean()?, BUMP_LINES);
                self.overflow
                    .bump(layout)
                    .expect("a clean block fits any medium object")
            }
        };
        self.overflow_bytes += layout.size();
        Ok(obj)
    }

    fn retire<M: ObjectModel, A: Allocator>(&mut self, heap: &Heap<M, A>, block: Block) {
        heap.retire(block);
        let counters = &heap.counters;
        counters
            .bytes_allocated
            .fetch_add(core::mem::take(&mut self.bytes), Ordering::Relaxed);
        counters
            .overflow_bytes
            .fetch_add(core::mem::take(&mut self.overflow_bytes), Ordering::Relaxed);
    }

    pub(crate) fn retire_all<M: ObjectModel, A: Allocator>(&mut self, heap: &Heap<M, A>) {
        for block in [self.primary.take(), self.overflow.take()]
            .into_iter()
            .flatten()
        {
            self.retire(heap, block);
        }
    }
}

/// A handle for allocating in a `Heap`. Any number may exist; each owns the
/// blocks it bumps into. It is `Send`, so it can follow a task between
/// threads.
pub struct Mutator<'h, M: ObjectModel, A: Allocator = Global> {
    heap: &'h Heap<M, A>,
    bump: Bump,
}

impl<'h, M: ObjectModel, A: Allocator> Mutator<'h, M, A> {
    pub(crate) fn new(heap: &'h Heap<M, A>) -> Self {
        Self {
            heap,
            bump: Bump::default(),
        }
    }

    /// Returns uninitialized memory for `layout`.
    pub fn alloc(&mut self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        self.bump.alloc(self.heap, layout)
    }
}

impl<M: ObjectModel, A: Allocator> Drop for Mutator<'_, M, A> {
    fn drop(&mut self) {
        self.bump.retire_all(self.heap);
    }
}
