use core::{alloc::Layout, ops::RangeInclusive};

pub const BLOCK_SIZE: usize = 32 * 1024;
pub const LINE_SIZE: usize = 256;
pub const META_LINES: usize = 6;
pub const LOS_MAX_SIZE: usize = 8 * 1024;
pub const MAX_ALIGN: usize = 64;

pub(crate) const LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;
/// Bit `i` set: line `i` is a bump line.
pub(crate) const BUMP_LINES: u128 = !0 << META_LINES;
pub(crate) const BLOCK_LAYOUT: Layout = match Layout::from_size_align(BLOCK_SIZE, BLOCK_SIZE) {
    Ok(layout) => layout,
    Err(_) => panic!("invalid block layout"),
};

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
