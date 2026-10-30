use core::{alloc::Layout, mem::take, ptr::NonNull};

use allocator_api2::alloc::{AllocError, Allocator, Global};

use crate::{
    Heap,
    heap::ObjectHeader,
    region::{ALL_LINES, GRANULE, LINE_SIZE, OwnedBlock, Region, is_large, take_hole},
    sync::Ordering,
};

/// Bump window over the holes of one block.
#[derive(Default)]
struct Window {
    block: Option<OwnedBlock>,
    /// Offsets into the block: the open hole is `cursor..limit`.
    cursor: usize,
    limit: usize,
    /// Holes not opened yet. A snapshot taken at acquisition.
    holes: u128,
}

impl Window {
    fn install(&mut self, block: OwnedBlock, holes: u128) {
        *self = Window {
            block: Some(block),
            cursor: 0,
            limit: 0,
            holes,
        };
        let opened = self.open_next_hole();
        debug_assert!(opened, "installed a block with no holes");
    }

    fn retire(&mut self) -> Option<OwnedBlock> {
        take(self).block
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

    #[inline]
    fn bump<A: Allocator>(&mut self, region: &Region<A>, layout: Layout) -> Option<NonNull<u8>> {
        let block = self.block.as_ref()?;
        let start = self.cursor.next_multiple_of(layout.align());
        let end = start + layout.size();
        if end > self.limit {
            return None;
        }
        self.cursor = end;
        region.on_alloc(block, start, layout.size());
        Some(unsafe { region.base(block).byte_add(start) })
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
    pub(crate) fn alloc<H: ObjectHeader, A: Allocator>(
        &mut self,
        heap: &Heap<H, A>,
        layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        if is_large(layout) {
            return heap.alloc_large(layout);
        }
        let layout =
            Layout::from_size_align(layout.size().max(GRANULE), layout.align().max(GRANULE))
                .expect("rounding up a valid layout");
        let obj = self.alloc_small(heap, layout)?;
        self.bytes += layout.size();
        Ok(obj)
    }

    fn alloc_small<H: ObjectHeader, A: Allocator>(
        &mut self,
        heap: &Heap<H, A>,
        layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        loop {
            if let Some(obj) = self.primary.bump(&heap.region, layout) {
                return Ok(obj);
            }
            if layout.size() > LINE_SIZE && self.primary.holes != 0 {
                return self.alloc_overflow(heap, layout);
            }
            if !self.primary.open_next_hole() {
                if let Some(block) = self.primary.retire() {
                    self.retire(heap, block);
                }
                let (block, holes) = heap.acquire()?;
                self.primary.install(block, holes);
            }
        }
    }

    /// Medium objects that do not fit the open hole go to a clean block, so
    /// the primary block's remaining holes are kept for small objects.
    fn alloc_overflow<H: ObjectHeader, A: Allocator>(
        &mut self,
        heap: &Heap<H, A>,
        layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        let obj = match self.overflow.bump(&heap.region, layout) {
            Some(obj) => obj,
            None => {
                if let Some(block) = self.overflow.retire() {
                    self.retire(heap, block);
                }
                self.overflow.install(heap.acquire_clean()?, ALL_LINES);
                self.overflow
                    .bump(&heap.region, layout)
                    .expect("a clean block fits any medium object")
            }
        };
        self.overflow_bytes += layout.size();
        Ok(obj)
    }

    fn retire<H: ObjectHeader, A: Allocator>(&mut self, heap: &Heap<H, A>, block: OwnedBlock) {
        heap.retire(block);
        heap.bytes_allocated
            .fetch_add(take(&mut self.bytes), Ordering::Relaxed);
        heap.overflow_bytes
            .fetch_add(take(&mut self.overflow_bytes), Ordering::Relaxed);
    }

    pub(crate) fn retire_all<H: ObjectHeader, A: Allocator>(&mut self, heap: &Heap<H, A>) {
        for block in [self.primary.retire(), self.overflow.retire()]
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
pub struct Mutator<'h, H: ObjectHeader, A: Allocator = Global> {
    heap: &'h Heap<H, A>,
    bump: Bump,
}

impl<'h, H: ObjectHeader, A: Allocator> Mutator<'h, H, A> {
    pub(crate) fn new(heap: &'h Heap<H, A>) -> Self {
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

impl<H: ObjectHeader, A: Allocator> Drop for Mutator<'_, H, A> {
    fn drop(&mut self) {
        self.bump.retire_all(self.heap);
    }
}
