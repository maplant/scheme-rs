use core::{mem::take, ptr::NonNull};
use std::sync::TryLockError;

use allocator_api2::alloc::{Allocator, Global};

use crate::{
    Heap, ObjectModel,
    region::{ALL_LINES, BlockId, CollectorToken, MIN_SIZE, OwnedBlock, State, is_large},
    sync::{MutexGuard, lock},
};

/// Everything only the collector uses. Lives behind the heap's collector
/// mutex, so only a `Collector` can reach it.
pub(crate) struct CollectorState {
    pub(crate) token: CollectorToken,
    /// Blocks that took a free since the last sweep, and one flag per block
    /// saying which are in that list.
    dirty: Vec<BlockId>,
    dirty_flags: Vec<bool>,
    /// Blocks the collector owns that are in no pool: full ones, waiting for
    /// a free. Each sweep also passes retired blocks through here.
    held: Vec<Option<OwnedBlock>>,
    /// Blocks and free lines found by the last sweep, published by the next.
    quarantine: Vec<(OwnedBlock, u128)>,
}

impl CollectorState {
    pub(crate) fn new(token: CollectorToken, capacity: usize) -> Self {
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

/// The one handle that frees objects and reclaims lines. At most one exists
/// per heap at a time.
///
/// It holds the heap's collector lock, so it is `!Send`: take it on the
/// thread that will collect.
pub struct Collector<'h, M: ObjectModel, A: Allocator = Global> {
    heap: &'h Heap<M, A>,
    state: MutexGuard<'h, CollectorState>,
}

impl<'h, M: ObjectModel, A: Allocator> Collector<'h, M, A> {
    pub(crate) fn new(heap: &'h Heap<M, A>) -> Self {
        let state = match heap.collector.try_lock() {
            Ok(state) => state,
            Err(TryLockError::Poisoned(poisoned)) => poisoned.into_inner(),
            Err(TryLockError::WouldBlock) => panic!("collector already taken"),
        };
        Self { heap, state }
    }

    /// Frees a dead object. Its lines become allocatable after the second
    /// `sweep` from now, or, in a block a mutator owns, after the block's
    /// retirement and the two sweeps that follow.
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

    /// Epoch boundary. Publishes the lines found by the previous sweep, then
    /// looks for free lines in blocks that were retired or freed into since.
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
        // Recycled blocks freed into since the last sweep have more free lines
        // than their pool entry says. Take them back; under the pool lock no
        // mutator can be taking them at the same time. This includes blocks
        // just published from quarantine that took a free meanwhile: their
        // older holes wait one more sweep, as the new ones must.
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
        // A candidate the collector does not hold belongs to a mutator (seen
        // at its retirement) or to a pool (seen at the next reclaim).
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

/// Queues a block the collector holds if it has free lines; keeps it otherwise.
fn queue<A: Allocator>(
    region: &crate::region::Region<A>,
    state: &mut CollectorState,
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
