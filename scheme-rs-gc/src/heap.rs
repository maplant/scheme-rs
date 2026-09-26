use core::{alloc::Layout, marker::PhantomData, ptr::NonNull};

use allocator_api2::alloc::{AllocError, Allocator, Global};

use crate::{
    Mutator, ObjectModel,
    region::{ALL_LINES, BLOCK_SIZE, OwnedBlock, Region, State},
    sync::{AtomicUsize, Mutex, Ordering, lock},
};

/// An Immix heap of 32KB blocks divided into 256B lines, in one region
/// allocated up front.
///
/// Objects up to `LOS_MAX_SIZE` bytes (and `MAX_ALIGN` alignment) are bump
/// allocated into blocks. Larger ones come from `A` directly. When every
/// block is in use, allocation fails with `AllocError`.
///
/// Dropping the heap returns the region to `A` without finalizing the
/// objects in it. Large objects still alive are leaked.
pub struct Heap<M: ObjectModel, A: Allocator = Global> {
    pub(crate) region: Region<A>,
    pub(crate) free: Mutex<Vec<OwnedBlock>>,
    pub(crate) recycled: Mutex<Vec<(OwnedBlock, u128)>>,
    pub(crate) retired: Mutex<Vec<OwnedBlock>>,
    pub(crate) counters: Counters,
    _model: PhantomData<fn() -> M>,
}

#[derive(Default)]
pub(crate) struct Counters {
    pub(crate) bytes_allocated: AtomicUsize,
    pub(crate) overflow_bytes: AtomicUsize,
    pub(crate) large_objects: AtomicUsize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HeapStats {
    pub free_blocks: usize,
    pub recycled_blocks: usize,
    /// Bytes allocated into blocks. Mutators publish when they retire a block.
    pub bytes_allocated: usize,
    /// The part of `bytes_allocated` that went to overflow blocks.
    pub overflow_bytes: usize,
    pub large_objects: usize,
}

impl<M: ObjectModel> Heap<M, Global> {
    /// A heap with room for at least `bytes` of blocks.
    pub fn new(bytes: usize) -> Result<Self, AllocError> {
        Self::new_in(Global, bytes)
    }
}

impl<M: ObjectModel, A: Allocator> Heap<M, A> {
    /// A heap with room for at least `bytes` of blocks, allocated from `alloc`
    /// in one region.
    pub fn new_in(alloc: A, bytes: usize) -> Result<Self, AllocError> {
        let (region, mut blocks) = Region::new_in(alloc, bytes)?;
        // Popped from the end, so blocks are handed out in address order.
        blocks.reverse();
        Ok(Self {
            region,
            free: Mutex::new(blocks),
            recycled: Mutex::new(Vec::new()),
            retired: Mutex::new(Vec::new()),
            counters: Counters::default(),
            _model: PhantomData,
        })
    }

    /// Bytes of block space, a multiple of `BLOCK_SIZE`.
    pub fn capacity(&self) -> usize {
        self.region.capacity() * BLOCK_SIZE
    }

    pub fn mutator(&self) -> Mutator<'_, M, A> {
        Mutator::new(self)
    }

    pub fn stats(&self) -> HeapStats {
        let c = &self.counters;
        HeapStats {
            free_blocks: lock(&self.free).len(),
            recycled_blocks: lock(&self.recycled).len(),
            bytes_allocated: c.bytes_allocated.load(Ordering::Relaxed),
            overflow_bytes: c.overflow_bytes.load(Ordering::Relaxed),
            large_objects: c.large_objects.load(Ordering::Relaxed),
        }
    }

    /// A block to bump into and its free lines. Recycled blocks first.
    pub(crate) fn acquire(&self) -> Result<(OwnedBlock, u128), AllocError> {
        if let Some((block, holes)) = lock(&self.recycled).pop() {
            self.region
                .moved(&block, &[State::Recycled], State::MutatorOwned);
            return Ok((block, holes));
        }
        Ok((self.acquire_clean()?, ALL_LINES))
    }

    /// A block with every line free.
    pub(crate) fn acquire_clean(&self) -> Result<OwnedBlock, AllocError> {
        let block = lock(&self.free).pop().ok_or(AllocError)?;
        self.region
            .moved(&block, &[State::Free], State::MutatorOwned);
        Ok(block)
    }

    /// Hands an owned block to the collector.
    pub(crate) fn retire(&self, block: OwnedBlock) {
        self.region
            .moved(&block, &[State::MutatorOwned], State::Full);
        lock(&self.retired).push(block);
    }

    pub(crate) fn alloc_large(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        let obj = self.region.alloc.allocate(layout)?.cast::<u8>();
        self.counters.large_objects.fetch_add(1, Ordering::Relaxed);
        Ok(obj)
    }
}
