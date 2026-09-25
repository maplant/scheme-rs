use core::{alloc::Layout, marker::PhantomData, ptr::NonNull};

use allocator_api2::alloc::{AllocError, Allocator, Global};

use crate::{
    Collector, Mutator, ObjectModel,
    collector::CollectorState,
    meta::{BLOCK_LAYOUT, BUMP_LINES, Block, State},
    mutator::Bump,
    sync::{AtomicUsize, Mutex, Ordering, lock},
};

/// An Immix heap of 32KB blocks divided into 256B lines.
///
/// Objects up to `LOS_MAX_SIZE` bytes (and `MAX_ALIGN` alignment) are bump
/// allocated into blocks. Larger ones come from `A` directly. `A` also
/// supplies the blocks.
///
/// Dropping the heap returns every block to `A` without finalizing the
/// objects in them. Large objects still alive are leaked.
pub struct Heap<M: ObjectModel, A: Allocator = Global> {
    pub(crate) alloc: A,
    pub(crate) free: Mutex<Vec<Block>>,
    pub(crate) recycled: Mutex<Vec<(Block, u128)>>,
    pub(crate) retired: Mutex<Vec<Block>>,
    /// Every block taken from `alloc`.
    blocks: Mutex<Vec<Block>>,
    pub(crate) collector: Mutex<CollectorState>,
    pub(crate) shared: Mutex<Bump>,
    pub(crate) counters: Counters,
    _model: PhantomData<fn() -> M>,
}

#[derive(Default)]
pub(crate) struct Counters {
    pub(crate) blocks_allocated: AtomicUsize,
    pub(crate) blocks_released: AtomicUsize,
    pub(crate) bytes_allocated: AtomicUsize,
    pub(crate) overflow_bytes: AtomicUsize,
    pub(crate) large_objects: AtomicUsize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HeapStats {
    pub blocks_allocated: usize,
    pub blocks_released: usize,
    pub free_blocks: usize,
    pub recycled_blocks: usize,
    /// Bytes allocated into blocks. Mutators publish when they retire a block.
    pub bytes_allocated: usize,
    /// The part of `bytes_allocated` that went to overflow blocks.
    pub overflow_bytes: usize,
    pub large_objects: usize,
}

impl<M: ObjectModel> Heap<M, Global> {
    pub fn new() -> Self {
        Self::new_in(Global)
    }
}

impl<M: ObjectModel> Default for Heap<M, Global> {
    fn default() -> Self {
        Self::new()
    }
}

impl<M: ObjectModel, A: Allocator> Heap<M, A> {
    pub fn new_in(alloc: A) -> Self {
        Self {
            alloc,
            free: Mutex::new(Vec::new()),
            recycled: Mutex::new(Vec::new()),
            retired: Mutex::new(Vec::new()),
            blocks: Mutex::new(Vec::new()),
            collector: Mutex::new(CollectorState::default()),
            shared: Mutex::new(Bump::default()),
            counters: Counters::default(),
            _model: PhantomData,
        }
    }

    pub fn mutator(&self) -> Mutator<'_, M, A> {
        Mutator::new(self)
    }

    /// # Panics
    ///
    /// If another `Collector` for this heap is alive.
    pub fn collector(&self) -> Collector<'_, M, A> {
        Collector::new(self)
    }

    /// Allocates through a shared, locked bump window. For callers that have
    /// no `Mutator`, e.g. during thread-local teardown.
    pub fn alloc(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        lock(&self.shared).alloc(self, layout)
    }

    pub fn stats(&self) -> HeapStats {
        let c = &self.counters;
        HeapStats {
            blocks_allocated: c.blocks_allocated.load(Ordering::Relaxed),
            blocks_released: c.blocks_released.load(Ordering::Relaxed),
            free_blocks: lock(&self.free).len(),
            recycled_blocks: lock(&self.recycled).len(),
            bytes_allocated: c.bytes_allocated.load(Ordering::Relaxed),
            overflow_bytes: c.overflow_bytes.load(Ordering::Relaxed),
            large_objects: c.large_objects.load(Ordering::Relaxed),
        }
    }

    /// A block to bump into and its free lines. Recycled blocks first.
    pub(crate) fn acquire(&self) -> Result<(Block, u128), AllocError> {
        let mut recycled = lock(&self.recycled);
        if let Some((block, holes)) = recycled.pop() {
            block.set_state(State::Owned);
            return Ok((block, holes));
        }
        drop(recycled);
        Ok((self.acquire_clean()?, BUMP_LINES))
    }

    /// A block with every bump line free.
    pub(crate) fn acquire_clean(&self) -> Result<Block, AllocError> {
        let mut free = lock(&self.free);
        if let Some(block) = free.pop() {
            block.set_state(State::Owned);
            return Ok(block);
        }
        drop(free);
        let base = self.alloc.allocate(BLOCK_LAYOUT)?.cast::<u8>();
        let block = unsafe { Block::init(base) };
        lock(&self.blocks).push(block);
        self.counters
            .blocks_allocated
            .fetch_add(1, Ordering::Relaxed);
        Ok(block)
    }

    /// Hands an owned block to the collector.
    pub(crate) fn retire(&self, block: Block) {
        block.set_state(State::Full);
        lock(&self.retired).push(block);
    }

    pub(crate) fn alloc_large(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        let obj = self.alloc.allocate(layout)?.cast::<u8>();
        self.counters.large_objects.fetch_add(1, Ordering::Relaxed);
        Ok(obj)
    }

    /// # Safety
    ///
    /// `obj` came from `alloc_large` with `layout` and is dead.
    pub(crate) unsafe fn free_large(&self, obj: NonNull<u8>, layout: Layout) {
        unsafe { self.alloc.deallocate(obj, layout) };
        self.counters.large_objects.fetch_sub(1, Ordering::Relaxed);
    }

    /// Returns a block to `alloc`.
    ///
    /// # Safety
    ///
    /// The block is Free: it holds no live objects and is in no pool or window.
    pub(crate) unsafe fn release(&self, block: Block) {
        debug_assert_eq!(block.state(), State::Free);
        // ponytail: O(blocks) removal; index the registry if release_to_os gets hot.
        lock(&self.blocks).retain(|&b| b != block);
        unsafe { self.alloc.deallocate(block.deinit(), BLOCK_LAYOUT) };
        self.counters
            .blocks_released
            .fetch_add(1, Ordering::Relaxed);
    }
}

impl<M: ObjectModel, A: Allocator> Drop for Heap<M, A> {
    /// Returns every block to `A`. Objects still in the heap are not
    /// finalized, and large objects still alive are leaked.
    fn drop(&mut self) {
        for block in lock(&self.blocks).drain(..) {
            unsafe { self.alloc.deallocate(block.deinit(), BLOCK_LAYOUT) };
        }
    }
}
