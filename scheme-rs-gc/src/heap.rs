use core::{alloc::Layout, marker::PhantomData, ptr::NonNull};

use allocator_api2::alloc::{AllocError, Allocator, Global};

use crate::{
    Mutator, ObjectModel, Reclaimer,
    large::PAGE,
    large::Run,
    reclaimer::ReclaimerState,
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
    pub(crate) reclaimer: Mutex<ReclaimerState>,
    pub(crate) bytes_allocated: AtomicUsize,
    pub(crate) overflow_bytes: AtomicUsize,
    pub(crate) large_objects: AtomicUsize,
    pub(crate) large_bytes: AtomicUsize,
    pub(crate) budget: usize,
    pub(crate) used: AtomicUsize,
    _model: PhantomData<fn() -> M>,
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
    pub large_bytes: usize,
    pub budget_used: usize,
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
        let bytes = region.n_blocks() * BLOCK_SIZE;
        // Popped from the end, so blocks are handed out in address order.
        blocks.reverse();
        let token = unsafe { region.reclaimer_token() };
        let reclaimer = ReclaimerState::new(token, region.n_blocks());
        Ok(Self {
            region,
            free: Mutex::new(blocks),
            recycled: Mutex::new(Vec::new()),
            retired: Mutex::new(Vec::new()),
            reclaimer: Mutex::new(reclaimer),
            bytes_allocated: AtomicUsize::new(0),
            overflow_bytes: AtomicUsize::new(0),
            large_objects: AtomicUsize::new(0),
            large_bytes: AtomicUsize::new(0),
            budget: bytes,
            used: AtomicUsize::new(0),
            _model: PhantomData,
        })
    }

    pub fn budget(&self) -> usize {
        self.budget
    }

    pub fn mutator(&self) -> Mutator<'_, M, A> {
        Mutator::new(self)
    }

    /// # Panics
    ///
    /// If another `Reclaimer` for this heap is alive.
    pub fn reclaimer(&self) -> Reclaimer<'_, M, A> {
        Reclaimer::new(self)
    }

    pub fn stats(&self) -> HeapStats {
        HeapStats {
            free_blocks: lock(&self.free).len(),
            recycled_blocks: lock(&self.recycled).len(),
            bytes_allocated: self.bytes_allocated.load(Ordering::Relaxed),
            overflow_bytes: self.overflow_bytes.load(Ordering::Relaxed),
            large_objects: self.large_objects.load(Ordering::Relaxed),
            large_bytes: self.large_bytes.load(Ordering::Relaxed),
            budget_used: self.used.load(Ordering::Relaxed),
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
        let mut lock = lock(&self.free);

        if self.claim_space(BLOCK_SIZE).is_ok() {
            let block = lock.pop().ok_or(AllocError)?;
            self.region
                .moved(&block, &[State::Free], State::MutatorOwned);
            Ok(block)
        } else {
            Result::Err(AllocError)
        }
    }

    /// Hands an owned block to the reclaimer.
    pub(crate) fn retire(&self, block: OwnedBlock) {
        self.region
            .moved(&block, &[State::MutatorOwned], State::Full);
        lock(&self.retired).push(block);
    }

    pub(crate) fn claim_space(&self, bytes: usize) -> Result<(), AllocError> {
        self.used
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |used| {
                (used + bytes <= self.budget).then_some(used + bytes)
            })
            .map(drop)
            .map_err(|_| AllocError)
    }

    pub(crate) fn return_space(&self, bytes: usize) {
        self.used.fetch_sub(bytes, Ordering::Relaxed);
    }

    pub(crate) fn alloc_large(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        let n_pages = layout.size().div_ceil(PAGE);
        let alignment = layout.align();
        let bytes = n_pages * PAGE;

        if alignment > 8192 {
            return Err(AllocError);
        }

        self.claim_space(bytes)?;

        let Some(Run {
            start_index: page,
            n_pages: _,
        }) = self.region.large.claim_run(n_pages, alignment)
        else {
            self.return_space(bytes);
            return Err(AllocError);
        };

        self.region.large.mark_occupied(page);
        self.large_objects.fetch_add(1, Ordering::Relaxed);
        self.large_bytes.fetch_add(bytes, Ordering::Relaxed);

        Ok(self.region.large.object_at(page))
    }
}
