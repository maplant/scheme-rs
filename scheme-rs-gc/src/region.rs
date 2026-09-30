use core::{
    alloc::Layout,
    ops::RangeInclusive,
    ptr::{NonNull, slice_from_raw_parts_mut},
};

use allocator_api2::alloc::{AllocError, Allocator};

// A static, so it cannot be a loom atomic; it only numbers heaps.
#[cfg(debug_assertions)]
use std::sync::atomic::AtomicU32;

use crate::sync::{AtomicU8, Ordering};

pub const BLOCK_SIZE: usize = 32 * 1024;
pub const LINE_SIZE: usize = 256;
pub const LOS_MAX_SIZE: usize = 8 * 1024;
pub const MAX_ALIGN: usize = 64;

/// Allocations are rounded up to this, which bounds a line's live count
/// at 17 and matches LXR's RC granule.
pub const MIN_SIZE: usize = 16;

pub(crate) const LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;
/// Every line of a block is a bump line.
pub(crate) const ALL_LINES: u128 = !0;

const _: () = assert!(LINES_PER_BLOCK == u128::BITS as usize);
const _: () = assert!(LINE_SIZE.is_multiple_of(MAX_ALIGN));

pub(crate) fn is_large(layout: Layout) -> bool {
    layout.size() > LOS_MAX_SIZE || layout.align() > MAX_ALIGN
}

/// Lines covered by `size` bytes at `offset`.
pub(crate) fn lines(offset: usize, size: usize) -> RangeInclusive<usize> {
    debug_assert!(size > 0, "zero-size allocation");
    offset / LINE_SIZE..=(offset + size - 1) / LINE_SIZE
}

/// Removes the lowest run of set bits from `holes`, returning its lines as
/// `(start, end)` with `end` exclusive.
pub(crate) fn take_hole(holes: &mut u128) -> Option<(usize, usize)> {
    if *holes == 0 {
        return None;
    }
    let start = holes.trailing_zeros() as usize;
    let end = start + (!(*holes >> start)).trailing_zeros() as usize;
    *holes = if end == LINES_PER_BLOCK {
        0
    } else {
        *holes & (!0 << end)
    };
    Some((start, end))
}

/// Where a block's owner token is. Recorded only in debug builds, to check the
/// moves; ownership of `OwnedBlock` is what enforces them.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum State {
    MutatorOwned,
    Full,
    #[expect(dead_code, reason = "the collector uses it")]
    AwaitingClearance,
    Recycled,
    Free,
}

/// A block's index. Names a block; grants nothing.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct BlockId(u32);

impl BlockId {
    pub(crate) fn index(self) -> usize {
        self.0 as usize
    }
}

/// The one owner of a block. Only `Region::new_in` makes them, one per block,
/// so a block is always in exactly one place: a pool, a mutator's window, the
/// retired list, or the collector.
#[derive(Debug)]
pub(crate) struct OwnedBlock {
    id: BlockId,
    #[cfg(debug_assertions)]
    heap: u32,
}

impl OwnedBlock {
    #[expect(dead_code, reason = "the collector uses it")]
    pub(crate) fn id(&self) -> BlockId {
        self.id
    }
}

/// Proof of being the heap's one collector.
#[derive(Debug)]
pub(crate) struct CollectorToken {
    #[cfg(debug_assertions)]
    heap: u32,
}

#[cfg(debug_assertions)]
static NEXT_HEAP: AtomicU32 = AtomicU32::new(0);

/// The heap's memory, allocated once: the line-count table first, then the
/// blocks.
///
/// ```text
/// [ line_live | pad to BLOCK_SIZE | block 0 | block 1 | ... ]
/// ```
///
/// It owns the allocator the memory came from and returns the memory to it
/// on drop.
pub(crate) struct Region<A: Allocator> {
    pub(crate) alloc: A,
    base: NonNull<u8>,
    layout: Layout,
    /// `LINES_PER_BLOCK` live-object counts per block. The owner increments,
    /// the collector decrements.
    line_live: NonNull<AtomicU8>,
    blocks: NonNull<u8>,
    capacity: usize,
    #[cfg(debug_assertions)]
    id: u32,
    #[cfg(debug_assertions)]
    states: Box<[AtomicU8]>,
}

// The table holds only atomics, and the memory is owned by `Region`.
unsafe impl<A: Allocator + Send> Send for Region<A> {}
unsafe impl<A: Allocator + Sync> Sync for Region<A> {}

impl<A: Allocator> Region<A> {
    /// Allocates at least `bytes` of blocks, plus their line counts, from
    /// `alloc`. Returns the owner of every block.
    pub(crate) fn new_in(alloc: A, bytes: usize) -> Result<(Self, Vec<OwnedBlock>), AllocError> {
        let capacity = bytes.div_ceil(BLOCK_SIZE).max(1);
        if capacity > u32::MAX as usize {
            return Err(AllocError);
        }
        // The largest product; once it fits, `capacity * LINES_PER_BLOCK` does.
        let blocks_size = capacity.checked_mul(BLOCK_SIZE).ok_or(AllocError)?;
        let table =
            Layout::array::<AtomicU8>(capacity * LINES_PER_BLOCK).map_err(|_| AllocError)?;
        let blocks_at = table.size().next_multiple_of(BLOCK_SIZE);
        let layout = blocks_size
            .checked_add(blocks_at)
            .and_then(|size| Layout::from_size_align(size, BLOCK_SIZE).ok())
            .ok_or(AllocError)?;
        let base = alloc.allocate(layout)?.cast::<u8>();
        let region = Region {
            alloc,
            base,
            layout,
            line_live: base.cast(),
            blocks: unsafe { base.byte_add(blocks_at) },
            capacity,
            #[cfg(debug_assertions)]
            id: NEXT_HEAP.fetch_add(1, Ordering::Relaxed),
            #[cfg(debug_assertions)]
            states: (0..capacity)
                .map(|_| AtomicU8::new(State::Free as u8))
                .collect(),
        };
        for i in 0..capacity * LINES_PER_BLOCK {
            unsafe { region.line_live.add(i).write(AtomicU8::new(0)) };
        }
        let owners = (0..capacity as u32)
            .map(|index| OwnedBlock {
                id: BlockId(index),
                #[cfg(debug_assertions)]
                heap: region.id,
            })
            .collect();
        Ok((region, owners))
    }

    /// # Safety
    ///
    /// At most one per region, held by the heap's collector mutex.
    #[expect(dead_code, reason = "the collector uses it")]
    pub(crate) unsafe fn collector_token(&self) -> CollectorToken {
        CollectorToken {
            #[cfg(debug_assertions)]
            heap: self.id,
        }
    }

    /// Number of blocks.
    pub(crate) fn capacity(&self) -> usize {
        self.capacity
    }

    /// The block holding `obj`.
    ///
    /// # Panics
    ///
    /// If `obj` is not in this heap's blocks.
    #[expect(dead_code, reason = "the collector uses it")]
    pub(crate) fn block_of(&self, obj: NonNull<u8>) -> BlockId {
        let offset = obj.addr().get().wrapping_sub(self.blocks.addr().get());
        assert!(
            offset < self.capacity * BLOCK_SIZE,
            "pointer is not in this heap"
        );
        BlockId((offset / BLOCK_SIZE) as u32)
    }

    #[inline]
    fn start(&self, id: BlockId) -> NonNull<u8> {
        unsafe { self.blocks.byte_add(id.index() * BLOCK_SIZE) }
    }

    /// The block's first byte. Only its owner gets one to bump into.
    #[inline]
    pub(crate) fn base(&self, block: &OwnedBlock) -> NonNull<u8> {
        self.check_owner(block);
        self.start(block.id)
    }

    #[expect(dead_code, reason = "the collector uses it")]
    pub(crate) fn offset_of(&self, id: BlockId, obj: NonNull<u8>) -> usize {
        obj.addr().get() - self.start(id).addr().get()
    }

    #[inline]
    fn line(&self, id: BlockId, line: usize) -> &AtomicU8 {
        unsafe {
            self.line_live
                .add(id.index() * LINES_PER_BLOCK + line)
                .as_ref()
        }
    }

    #[inline]
    fn check_owner(&self, _block: &OwnedBlock) {
        #[cfg(debug_assertions)]
        debug_assert_eq!(_block.heap, self.id, "block from another heap");
    }

    fn check_token(&self, _token: &CollectorToken) {
        #[cfg(debug_assertions)]
        debug_assert_eq!(_token.heap, self.id, "collector of another heap");
    }

    #[inline]
    pub(crate) fn on_alloc(&self, block: &OwnedBlock, offset: usize, size: usize) {
        self.check_owner(block);
        for line in lines(offset, size) {
            let prev = self.line(block.id, line).fetch_add(1, Ordering::Relaxed);
            debug_assert!(prev < u8::MAX, "line {line} count overflow");
        }
    }

    /// The collector frees into blocks it may not own.
    #[expect(dead_code, reason = "the collector uses it")]
    pub(crate) fn on_free(&self, token: &CollectorToken, id: BlockId, offset: usize, size: usize) {
        self.check_token(token);
        for line in lines(offset, size) {
            let prev = self.line(id, line).fetch_sub(1, Ordering::Relaxed);
            debug_assert!(prev > 0, "line {line} count underflow");
        }
    }

    #[cfg(all(test, loom))]
    #[expect(dead_code, reason = "the loom models use it")]
    pub(crate) fn line_count(&self, token: &CollectorToken, id: BlockId, line: usize) -> u8 {
        self.check_token(token);
        self.line(id, line).load(Ordering::Relaxed)
    }

    /// Lines with no live objects, for the block's holder.
    #[expect(dead_code, reason = "the collector uses it")]
    pub(crate) fn free_lines(&self, block: &OwnedBlock) -> u128 {
        self.check_owner(block);
        (0..LINES_PER_BLOCK)
            .filter(|&line| self.line(block.id, line).load(Ordering::Relaxed) == 0)
            .fold(0, |holes, line| holes | 1 << line)
    }

    /// Records a move of `block` from one of `from` to `to` (debug builds only).
    #[inline]
    pub(crate) fn moved(&self, _block: &OwnedBlock, _from: &[State], _to: State) {
        #[cfg(debug_assertions)]
        {
            let cell = &self.states[_block.id.index()];
            let now = cell.load(Ordering::Relaxed);
            debug_assert!(
                _from.iter().any(|&s| s as u8 == now),
                "block {:?} moved to {_to:?} from state {now}",
                _block.id
            );
            cell.store(_to as u8, Ordering::Relaxed);
        }
    }
}

impl<A: Allocator> Drop for Region<A> {
    fn drop(&mut self) {
        unsafe {
            slice_from_raw_parts_mut(self.line_live.as_ptr(), self.capacity * LINES_PER_BLOCK)
                .drop_in_place();
            self.alloc.deallocate(self.base, self.layout);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn oversized_regions_are_refused() {
        use allocator_api2::alloc::Global;
        assert!(Region::new_in(Global, usize::MAX).is_err());
        assert!(Region::new_in(Global, (u32::MAX as usize + 1) * BLOCK_SIZE).is_err());
    }

    #[test]
    fn lines_covers_every_touched_line() {
        assert_eq!(lines(0, 1), 0..=0);
        assert_eq!(lines(0, LINE_SIZE), 0..=0);
        assert_eq!(lines(LINE_SIZE - 8, 16), 0..=1);
        assert_eq!(lines(3 * LINE_SIZE, 2 * LINE_SIZE + 1), 3..=5);
    }

    #[test]
    fn take_hole_yields_runs_in_address_order() {
        let mut holes: u128 = 0b1110_0110 << 8;
        assert_eq!(take_hole(&mut holes), Some((9, 11)));
        assert_eq!(take_hole(&mut holes), Some((13, 16)));
        assert_eq!(take_hole(&mut holes), None);
    }

    #[test]
    fn take_hole_handles_a_run_to_the_last_line() {
        let mut holes = ALL_LINES;
        assert_eq!(take_hole(&mut holes), Some((0, LINES_PER_BLOCK)));
        assert_eq!(holes, 0);
    }

    #[test]
    fn take_hole_handles_single_lines_and_empty_bitmaps() {
        let mut holes: u128 = 1 | 1 << 127;
        assert_eq!(take_hole(&mut holes), Some((0, 1)));
        assert_eq!(take_hole(&mut holes), Some((127, 128)));
        assert_eq!(take_hole(&mut holes), None);
        let mut empty = 0;
        assert_eq!(take_hole(&mut empty), None);
    }
}
