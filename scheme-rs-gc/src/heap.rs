use core::{alloc::Layout, marker::PhantomData, ptr::NonNull};

use allocator_api2::alloc::{AllocError, Allocator, Global};

use crate::{
    GRANULE, Mutator, Reclaimer,
    large::{PAGE, Run},
    reclaimer::ReclaimerState,
    region::{ALL_LINES, BLOCK_SIZE, OwnedBlock, Region, State},
    sync::{AtomicUsize, Condvar, Mutex, Ordering, lock, wait_while},
};

/// Tells the heap how to read an object it allocated.
///
/// # Safety
///
/// Every object allocated in a `Heap<Self, _>` starts with an initialized
/// `Self` until it is freed, and `layout` returns the exact layout the
/// object was allocated with. `layout` may be called from any thread while
/// other threads use the object.
pub unsafe trait ObjectHeader {
    /// Per-heap state for the collector of this header. The heap stores it
    /// but never reads it.
    type HeapState: Default + Send + Sync;
    fn layout(&self) -> Layout;
}

/// Lives here as the heap has to be able to mutate it for requesting, see below.
/// Unintuitive, but keeps the collector out.
/// The alternative is that Heap gets another type parameter.
pub(crate) struct CollectionStatus {
    pub(crate) running: bool,
    pub(crate) shutdown: bool,
    pub(crate) alive: bool,
    pub(crate) requested: bool,
    pub(crate) epoch: usize,
}

/// The heaps way of telling any listening collector that it requests collection
/// Needed here, as the heap 1. can't depend on collectors and
/// 2. it knows when to collect.
pub(crate) struct CollectionSignal {
    pub(crate) status: Mutex<CollectionStatus>,
    pub(crate) start: Condvar,
    pub(crate) done: Condvar,
}

/// An Immix heap of 32KB blocks divided into 256B lines, in one region
/// allocated up front.
///
/// Objects up to `LOS_MAX_SIZE` bytes (and `MAX_ALIGN` alignment) are bump
/// allocated into blocks. Larger ones come from `A` directly. When every
/// block is in use, allocation fails with `AllocError`.
///
/// Dropping the heap returns the region to `A` without finalizing the
/// objects in it. Large objects still alive are leaked.
pub struct Heap<H: ObjectHeader, A: Allocator = Global> {
    pub(crate) region: Region<A>,
    pub(crate) free: Mutex<Vec<OwnedBlock>>,
    pub(crate) recycled: Mutex<Vec<(OwnedBlock, u128)>>,
    pub(crate) retired: Mutex<Vec<OwnedBlock>>,
    pub(crate) reclaimer: Mutex<ReclaimerState>,
    pub(crate) bytes_allocated: AtomicUsize,
    pub(crate) overflow_bytes: AtomicUsize,
    pub(crate) large_objects: AtomicUsize,
    pub(crate) large_bytes: AtomicUsize,
    pub(crate) next_trigger: AtomicUsize,
    pub(crate) signal: CollectionSignal,
    pub(crate) budget: usize,
    pub(crate) used: AtomicUsize,
    pub(crate) state: H::HeapState,
    _model: PhantomData<fn() -> H>,
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

impl<H: ObjectHeader> Heap<H, Global> {
    /// A heap with room for at least `bytes` of blocks.
    pub fn new(bytes: usize) -> Result<Self, AllocError> {
        Self::new_in(Global, bytes)
    }
}

impl<H: ObjectHeader, A: Allocator> Heap<H, A> {
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
            next_trigger: AtomicUsize::new(usize::MAX),
            overflow_bytes: AtomicUsize::new(0),
            large_objects: AtomicUsize::new(0),
            large_bytes: AtomicUsize::new(0),
            budget: bytes,
            used: AtomicUsize::new(0),
            signal: CollectionSignal {
                status: Mutex::new(CollectionStatus {
                    running: false,
                    shutdown: false,
                    alive: false,
                    requested: false,
                    epoch: 0,
                }),
                start: Condvar::new(),
                done: Condvar::new(),
            },
            state: H::HeapState::default(),
            _model: PhantomData,
        })
    }

    pub fn budget(&self) -> usize {
        self.budget
    }

    pub fn mutator(&self) -> Mutator<'_, H, A> {
        Mutator::new(self)
    }

    /// # Panics
    ///
    /// If another `Reclaimer` for this heap is alive.
    pub fn reclaimer(&self) -> Reclaimer<'_, H, A> {
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
            let block = lock.pop().ok_or_else(|| {
                self.return_space(BLOCK_SIZE);
                AllocError
            })?;

            self.region
                .moved(&block, &[State::Free], State::MutatorOwned);
            Ok(block)
        } else {
            // This means that no block is available.
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
        #[allow(deprecated)]
        self.used
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |used| {
                (used + bytes <= self.budget).then_some(used + bytes)
            })
            .map(|prev| {
                let trigger = self.next_trigger.load(Ordering::Relaxed);
                if prev < trigger && prev + bytes >= trigger {
                    self.request_collection();
                }
            })
            .map_err(|_| AllocError)
    }

    /// The object containing `addr`, if any: an exact or interior pointer into
    /// a live small or large object.
    ///
    /// # Safety
    ///
    /// No object is freed concurrently: call it from the thread that frees,
    /// or while that thread is stopped.
    pub unsafe fn find_object_of_addr(&self, addr: usize) -> Option<NonNull<u8>> {
        let obj = self
            .region
            .object_at_or_before(addr)
            .or(self.region.large.object_at_or_before(addr))?;

        let layout = unsafe { obj.cast::<H>().as_ref().layout() };

        let size = layout.size().max(GRANULE);

        (addr - obj.addr().get() < size).then_some(obj)
    }

    /// Wakes the collector of this heap, if one runs.
    pub fn request_collection(&self) {
        lock(&self.signal.status).requested = true;
        self.signal.start.notify_one();
    }

    /// Requests collections and waits until `epochs` epochs that started
    /// after this call have finished, or until the collector exits.
    pub fn wait_for_epochs(&self, epochs: usize) {
        for _ in 0..epochs {
            let mut status = lock(&self.signal.status);
            let target = if status.running {
                status.epoch + 2
            } else {
                status.epoch + 1
            };
            status.requested = true;
            self.signal.start.notify_one();
            drop(wait_while(&self.signal.done, status, |s| {
                s.epoch < target && s.alive
            }));
        }
    }

    // In theory something could allocate between reading used
    // and storing to next_trigger, making the calculation wrong.
    // In practice this will not have any effect, so I'm leaving it.
    pub(crate) fn set_trigger(&self, distance: usize) {
        self.next_trigger.store(
            self.used.load(Ordering::Relaxed) + distance,
            Ordering::Relaxed,
        );
    }

    pub(crate) fn return_space(&self, bytes: usize) {
        self.used.fetch_sub(bytes, Ordering::Relaxed);
    }

    pub(crate) fn alloc_large(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        let n_pages = layout.size().div_ceil(PAGE);
        let alignment = layout.align();
        let bytes = n_pages * PAGE;

        if alignment > BLOCK_SIZE {
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
