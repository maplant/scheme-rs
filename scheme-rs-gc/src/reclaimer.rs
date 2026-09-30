use core::{mem::take, ptr::NonNull};
use std::sync::TryLockError;

use allocator_api2::alloc::{Allocator, Global};

use crate::{
    Heap, ObjectModel,
    region::{ALL_LINES, BlockId, MIN_SIZE, OwnedBlock, ReclaimerToken, State, is_large},
    sync::{MutexGuard, lock},
};

/// State only a `Reclaimer` can reach, behind the heap's reclaimer mutex.
pub(crate) struct ReclaimerState {
    pub(crate) token: ReclaimerToken,
    /// Blocks freed into since the last sweep; the flags dedupe the list.
    dirty: Vec<BlockId>,
    dirty_flags: Vec<bool>,
    /// Full blocks waiting for a free.
    held: Vec<Option<OwnedBlock>>,
    /// Found by the last sweep, published by the next.
    quarantine: Vec<(OwnedBlock, u128)>,
}

impl ReclaimerState {
    pub(crate) fn new(token: ReclaimerToken, capacity: usize) -> Self {
        Self {
            token,
            dirty: Vec::new(),
            dirty_flags: vec![false; capacity],
            held: (0..capacity).map(|_| None).collect(),
            quarantine: Vec::new(),
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
    /// If a small object's pointer is not in this heap. A large object from
    /// another heap is not detected.
    pub unsafe fn free(&mut self, obj: NonNull<u8>) {
        let layout = M::layout(unsafe { obj.cast::<M::Header>().as_ref() });
        if is_large(layout) {
            unsafe { self.heap.free_large(obj, layout) };
            return;
        }
        let region = &self.heap.region;
        let id = region.block_of(obj);
        let offset = region.offset_of(id, obj);
        region.on_free(&self.state.token, id, offset, layout.size().max(MIN_SIZE));
        self.state.mark_dirty(id);
    }

    /// Publishes the last sweep's finds, then looks for free lines in blocks
    /// retired or freed into since.
    pub fn sweep(&mut self) {
        let heap = self.heap;
        let region = &heap.region;
        let state = &mut *self.state;
        for (block, holes) in state.quarantine.drain(..) {
            if holes == ALL_LINES {
                region.moved(&block, &[State::AwaitingClearance], State::Free);
                lock(&heap.free).push(block);
            } else {
                region.moved(&block, &[State::AwaitingClearance], State::Recycled);
                lock(&heap.recycled).push((block, holes));
            }
        }
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
            state.held[id.index()] = Some(block);
        }
        // Unheld candidates are seen at retirement or the next reclaim.
        for id in candidates {
            if let Some(block) = state.held[id.index()].take() {
                queue(region, state, block, State::Full);
            }
        }
        for block in reclaimed {
            queue(region, state, block, State::Recycled);
        }
    }
}

/// Queues a block the reclaimer holds if it has free lines; keeps it otherwise.
fn queue<A: Allocator>(
    region: &crate::region::Region<A>,
    state: &mut ReclaimerState,
    block: OwnedBlock,
    from: State,
) {
    let holes = region.free_lines(&block);
    if holes == 0 {
        // A reclaimed block had holes, and frees only add more.
        debug_assert_eq!(from, State::Full, "reclaimed block without holes");
        let index = block.id().index();
        state.held[index] = Some(block);
        return;
    }
    region.moved(&block, &[from], State::AwaitingClearance);
    state.quarantine.push((block, holes));
}
