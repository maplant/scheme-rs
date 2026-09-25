use core::{alloc::Layout, num::NonZero, ops::RangeInclusive, ptr::NonNull};

use crate::sync::{AtomicBool, AtomicU8, Ordering};

pub const BLOCK_SIZE: usize = 32 * 1024;
pub const LINE_SIZE: usize = 256;
pub const META_LINES: usize = 6;
pub const LOS_MAX_SIZE: usize = 8 * 1024;
pub const MAX_ALIGN: usize = 64;

/// Allocations are rounded up to this, which bounds a line's live count
/// at 17 and matches LXR's RC granule.
pub const MIN_SIZE: usize = 16;

pub(crate) const LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;
/// Bit `i` set: line `i` is a bump line.
pub(crate) const BUMP_LINES: u128 = !0 << META_LINES;
pub(crate) const BLOCK_LAYOUT: Layout = match Layout::from_size_align(BLOCK_SIZE, BLOCK_SIZE) {
    Ok(layout) => layout,
    Err(_) => panic!("invalid block layout"),
};

const _: () = assert!(LINES_PER_BLOCK == u128::BITS as usize);
const _: () = assert!(LINE_SIZE.is_multiple_of(MAX_ALIGN));

const _: () = assert!(size_of::<BlockHeader>() <= META_LINES * LINE_SIZE);

#[cfg(debug_assertions)]
const MAGIC: u32 = 0x1337_B10C;

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
    debug_assert_eq!(*holes & !BUMP_LINES, 0, "metadata lines in a hole bitmap");
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

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum State {
    /// A mutator bumps into it from a hole snapshot; the sweep never touches it.
    Owned,
    Full,
    Queued,
    Recycled,
    Free,
}

/// Lives in the metadata lines at the start of every block.
#[repr(C)]
pub(crate) struct BlockHeader {
    line_live: [AtomicU8; LINES_PER_BLOCK],
    state: AtomicU8,
    /// Collector only: the block is in the collector's dirty list.
    dirty: AtomicBool,
    #[cfg(debug_assertions)]
    magic: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct Block(NonNull<BlockHeader>);

unsafe impl Send for Block {}

impl Block {
    /// # Safety
    ///
    /// `base` is a fresh allocation of `BLOCK_LAYOUT`.
    pub(crate) unsafe fn init(base: NonNull<u8>) -> Self {
        let header = base.cast::<BlockHeader>();
        unsafe {
            header.write(BlockHeader {
                line_live: core::array::from_fn(|_| AtomicU8::new(0)),
                state: AtomicU8::new(State::Owned as u8),
                dirty: AtomicBool::new(false),
                #[cfg(debug_assertions)]
                magic: MAGIC,
            })
        };
        Block(header)
    }

    /// # Safety
    ///
    /// The block is not used again.
    pub(crate) unsafe fn deinit(self) -> NonNull<u8> {
        #[cfg(debug_assertions)]
        unsafe {
            (*self.0.as_ptr()).magic = 0;
        }
        unsafe { self.0.drop_in_place() };
        self.0.cast()
    }

    /// # Safety
    ///
    /// `obj` points into a live block.
    pub(crate) unsafe fn of(obj: NonNull<u8>) -> Self {
        let base = obj.map_addr(|addr| NonZero::new(addr.get() & !(BLOCK_SIZE - 1)).unwrap());
        let block = Block(base.cast());
        #[cfg(debug_assertions)]
        assert_eq!(
            block.header().magic,
            MAGIC,
            "pointer is not in a heap block"
        );
        block
    }

    fn header(&self) -> &BlockHeader {
        unsafe { self.0.as_ref() }
    }

    pub(crate) fn base(self) -> NonNull<u8> {
        self.0.cast()
    }

    pub(crate) fn offset_of(self, obj: NonNull<u8>) -> usize {
        let offset = obj.addr().get() - self.0.addr().get();
        debug_assert!(offset < BLOCK_SIZE, "pointer is not in this block");
        offset
    }

    pub(crate) fn state(self) -> State {
        match self.header().state.load(Ordering::Acquire) {
            0 => State::Owned,
            1 => State::Full,
            2 => State::Queued,
            3 => State::Recycled,
            4 => State::Free,
            s => unreachable!("corrupt block state {s}"),
        }
    }

    /// Each state has one writer: the owner moves Owned to Full, the
    /// collector moves Full, Queued and pooled states on, and the thread
    /// that pops a block from a pool (under its lock) moves it to Owned.
    pub(crate) fn set_state(self, state: State) {
        self.header().state.store(state as u8, Ordering::Release);
    }

    /// Collector, or tests. Exact only after an Acquire load of Full.
    pub(crate) fn line_count(self, line: usize) -> u8 {
        self.header().line_live[line].load(Ordering::Relaxed)
    }

    /// Owner only.
    pub(crate) fn on_alloc(self, offset: usize, size: usize) {
        for line in lines(offset, size) {
            let prev = self.header().line_live[line].fetch_add(1, Ordering::Relaxed);
            debug_assert!(prev < u8::MAX, "line {line} count overflow");
        }
    }

    /// Collector only.
    pub(crate) fn on_free(self, offset: usize, size: usize) {
        for line in lines(offset, size) {
            let prev = self.header().line_live[line].fetch_sub(1, Ordering::Relaxed);
            debug_assert!(prev > 0, "line {line} count underflow");
        }
    }

    /// Bump lines with no live objects. Collector only, on a Full block.
    pub(crate) fn free_lines(self) -> u128 {
        (META_LINES..LINES_PER_BLOCK)
            .filter(|&line| self.line_count(line) == 0)
            .fold(0, |holes, line| holes | 1 << line)
    }

    /// Collector only. True if the block was not dirty.
    pub(crate) fn mark_dirty(self) -> bool {
        !self.header().dirty.swap(true, Ordering::Relaxed)
    }

    /// Collector only.
    pub(crate) fn clear_dirty(self) {
        self.header().dirty.store(false, Ordering::Relaxed);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let mut holes = BUMP_LINES;
        assert_eq!(take_hole(&mut holes), Some((META_LINES, LINES_PER_BLOCK)));
        assert_eq!(holes, 0);
    }

    #[test]
    fn take_hole_handles_single_lines_and_empty_bitmaps() {
        let mut holes: u128 = 1 << META_LINES | 1 << 127;
        assert_eq!(take_hole(&mut holes), Some((META_LINES, META_LINES + 1)));
        assert_eq!(take_hole(&mut holes), Some((127, 128)));
        assert_eq!(take_hole(&mut holes), None);
        let mut empty = 0;
        assert_eq!(take_hole(&mut empty), None);
    }
}
