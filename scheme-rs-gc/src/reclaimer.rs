use core::{mem::take, ptr::NonNull};
use std::alloc::Layout;
use std::sync::TryLockError;

use allocator_api2::alloc::{Allocator, Global};

use crate::{
    Heap, ObjectModel,
    large::PAGE,
    large::Run,
    region::{
        ALL_LINES, BLOCK_SIZE, BlockId, MIN_SIZE, OwnedBlock, ReclaimerToken, Region, State,
        is_large,
    },
    sync::{MutexGuard, Ordering, lock},
};

/// State only a `Reclaimer` can reach, behind the heap's reclaimer mutex.
pub(crate) struct ReclaimerState {
    pub(crate) token: ReclaimerToken,
    /// Blocks freed into since the last sweep; the flags dedupe the list.
    dirty: Vec<BlockId>,
    dirty_flags: Vec<bool>,
    /// Full blocks waiting for a free.
    full: Vec<Option<OwnedBlock>>,
    /// Found by the last sweep, published by the next.
    quarantine: Vec<(OwnedBlock, u128)>,
    large_freed: Vec<Run>,
    large_quarantined: Vec<Run>,
}

impl ReclaimerState {
    pub(crate) fn new(token: ReclaimerToken, capacity: usize) -> Self {
        Self {
            token,
            dirty: Vec::new(),
            dirty_flags: vec![false; capacity],
            full: (0..capacity).map(|_| None).collect(),
            quarantine: Vec::new(),
            large_freed: Vec::new(),
            large_quarantined: Vec::new(),
        }
    }

    fn mark_dirty(&mut self, id: BlockId) {
        let flag = &mut self.dirty_flags[id.index()];
        if !*flag {
            *flag = true;
            self.dirty.push(id);
        }
    }
}

/// The heap's one handle for freeing objects and sweeping. `!Send`: it holds
/// the reclaimer lock.
pub struct Reclaimer<'h, M: ObjectModel, A: Allocator = Global> {
    heap: &'h Heap<M, A>,
    state: MutexGuard<'h, ReclaimerState>,
}

impl<'h, M: ObjectModel, A: Allocator> Reclaimer<'h, M, A> {
    pub(crate) fn new(heap: &'h Heap<M, A>) -> Self {
        let state = match heap.reclaimer.try_lock() {
            Ok(state) => state,
            Err(TryLockError::Poisoned(poisoned)) => poisoned.into_inner(),
            Err(TryLockError::WouldBlock) => panic!("reclaimer already taken"),
        };
        Self { heap, state }
    }

    /// Frees a dead object. Its lines are reusable after two sweeps.
    ///
    /// # Safety
    ///
    /// `obj` was allocated by this heap, is dead, is freed at most once, and
    /// its `M::Header` is still intact.
    ///
    /// # Panics
    ///
    /// If an object's pointer is not in this heap.
    pub unsafe fn free(&mut self, obj: NonNull<u8>) {
        let layout = M::layout(unsafe { obj.cast::<M::Header>().as_ref() });
        if is_large(layout) {
            self.free_large(layout, obj);
            return;
        }
        let region = &self.heap.region;
        let id = region.block_of(obj);
        let offset = region.offset_of(id, obj);
        region.on_free(&self.state.token, id, offset, layout.size().max(MIN_SIZE));
        self.state.mark_dirty(id);
    }

    fn free_large(&mut self, layout: Layout, obj: NonNull<u8>) {
        let page = self
            .heap
            .region
            .large
            .page_of(obj)
            .expect("Large pointer not in this heap");
        debug_assert!(
            self.heap.region.large.is_occupied(page),
            "Unmarked page being freed; double free, or incorrect bookkeeping."
        );
        self.heap.region.large.mark_unoccupied(page);
        self.state.large_freed.push(Run {
            start_index: page,
            n_pages: layout.size().div_ceil(PAGE),
        });
        self.heap.large_objects.fetch_sub(1, Ordering::Relaxed);
    }

    /// Publishes the last sweep's finds, then looks for free lines in blocks
    /// retired or freed into since.
    pub fn sweep(&mut self) {
        let heap = self.heap;
        let region = &heap.region;
        let state = &mut *self.state;

        // Block quarantine
        for (block, holes) in state.quarantine.drain(..) {
            if holes == ALL_LINES {
                region.moved(&block, &[State::AwaitingClearance], State::Free);
                lock(&heap.free).push(block);
                heap.return_space(BLOCK_SIZE);
            } else {
                region.moved(&block, &[State::AwaitingClearance], State::Recycled);
                lock(&heap.recycled).push((block, holes));
            }
        }

        // Large quarantine
        for run in state.large_quarantined.drain(..) {
            let bytes = run.n_pages * PAGE;
            region.large.release_run(run);
            heap.return_space(bytes);
            heap.large_bytes.fetch_sub(bytes, Ordering::Relaxed);
        }
        state.large_quarantined = take(&mut state.large_freed);

        // Recycled blocks freed into have stale holes; take them back.
        let reclaimed: Vec<_> = lock(&heap.recycled)
            .extract_if(.., |entry| state.dirty_flags[entry.0.id().index()])
            .map(|(block, _)| block)
            .collect();
        let retired = take(&mut *lock(&heap.retired));
        let mut candidates = take(&mut state.dirty);
        for &id in &candidates {
            state.dirty_flags[id.index()] = false;
        }
        for block in retired {
            let id = block.id();
            candidates.push(id);
            state.full[id.index()] = Some(block);
        }
        // Candidates not in `full` are seen at retirement or the next reclaim.
        for id in candidates {
            if let Some(block) = state.full[id.index()].take() {
                quarantine_or_hold(region, state, block, State::Full);
            }
        }
        for block in reclaimed {
            quarantine_or_hold(region, state, block, State::Recycled);
        }
    }
}

/// Quarantines a block with free lines; otherwise keeps it in `full`.
fn quarantine_or_hold<A: Allocator>(
    region: &Region<A>,
    state: &mut ReclaimerState,
    block: OwnedBlock,
    from: State,
) {
    let holes = region.free_lines(&block);
    if holes == 0 {
        // A reclaimed block had holes, and frees only add more.
        debug_assert_eq!(from, State::Full, "reclaimed block without holes");
        let index = block.id().index();
        state.full[index] = Some(block);
        return;
    }
    region.moved(&block, &[from], State::AwaitingClearance);
    state.quarantine.push((block, holes));
}
