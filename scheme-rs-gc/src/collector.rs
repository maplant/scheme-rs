use core::ptr::NonNull;
use std::sync::TryLockError;

use allocator_api2::alloc::{Allocator, Global};

use crate::{
    Heap, ObjectModel,
    meta::{BUMP_LINES, Block, MIN_SIZE, State, is_large},
    sync::{MutexGuard, lock},
};

#[derive(Default)]
pub(crate) struct CollectorState {
    /// Blocks that took a free since the last sweep.
    dirty: Vec<Block>,
    /// Blocks and free lines found by the last sweep, published by the next.
    quarantine: Vec<(Block, u128)>,
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

    /// Frees a dead object. If its block is Full, its lines become
    /// allocatable after the second `sweep` from now; in an Owned, Queued or
    /// Recycled block they wait for the block's next retirement.
    ///
    /// # Safety
    ///
    /// `obj` was allocated by this heap (not another `Heap`), is dead, is
    /// freed at most once, and `M::layout` still returns the layout it was
    /// allocated with.
    pub unsafe fn free(&mut self, obj: NonNull<u8>) {
        let layout = unsafe { M::layout(obj) };
        if is_large(layout) {
            unsafe { self.heap.free_large(obj, layout) };
            return;
        }
        let block = unsafe { Block::of(obj) };
        block.on_free(block.offset_of(obj), layout.size().max(MIN_SIZE));
        if block.mark_dirty() {
            self.state.dirty.push(block);
        }
    }

    /// Epoch boundary. Publishes the lines found by the previous sweep, then
    /// looks for free lines in blocks that were retired or freed into since.
    pub fn sweep(&mut self) {
        let heap = self.heap;
        let state = &mut *self.state;
        for (block, holes) in state.quarantine.drain(..) {
            if holes == BUMP_LINES {
                block.set_state(State::Free);
                lock(&heap.free).push(block);
            } else {
                block.set_state(State::Recycled);
                lock(&heap.recycled).push((block, holes));
            }
        }
        // The shared window has no owner to retire it; retire it each epoch.
        lock(&heap.shared).retire_all(heap);
        let retired = core::mem::take(&mut *lock(&heap.retired));
        let dirty = core::mem::take(&mut state.dirty);
        for &block in &dirty {
            block.clear_dirty();
        }
        // ponytail: frees into Owned, Queued or Recycled blocks are picked up
        // only after the block's next retirement, so a Recycled block that
        // empties stays in the recycled pool with stale holes and never
        // reaches release_to_os. Upgrade: compute holes from the line counts
        // at acquire (as LXR does) once the reuse rule can be relaxed.
        for block in dirty.into_iter().chain(retired) {
            if block.state() != State::Full {
                continue;
            }
            let holes = block.free_lines();
            if holes != 0 {
                block.set_state(State::Queued);
                state.quarantine.push((block, holes));
            }
        }
    }

    /// Returns free blocks to the allocator until at most `keep` remain.
    pub fn release_to_os(&mut self, keep: usize) {
        let excess = {
            let mut free = lock(&self.heap.free);
            let at = keep.min(free.len());
            free.split_off(at)
        };
        for block in excess {
            unsafe { self.heap.release(block) };
        }
    }
}
