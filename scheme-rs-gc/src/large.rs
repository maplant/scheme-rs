#![allow(dead_code)]

use core::ptr::NonNull;
use std::collections::BTreeMap;

use crate::region::BLOCK_SIZE;
use crate::sync::{AtomicU64, Mutex, Ordering, lock};

pub(crate) const PAGE: usize = 4096; // bytes

#[derive(Debug, PartialEq)]
pub(crate) struct Run {
    pub(crate) start_index: usize,
    pub(crate) n_pages: usize,
}

pub(crate) struct LargeSpace {
    bitmap: NonNull<AtomicU64>,
    first_page: NonNull<u8>,
    n_pages: usize,
    // A map of free consecutive pages, K being starts and V being ends. Starts with one: (0, N)
    runs: Mutex<BTreeMap<usize, usize>>,
}

impl LargeSpace {
    // bytes needed to fit a bitmap of size n_pages
    pub(crate) fn bitmap_bytes_needed_for(n_pages: usize) -> usize {
        (n_pages.div_ceil(64) * size_of::<AtomicU64>()).next_multiple_of(BLOCK_SIZE)
    }

    // bytes needed to fit a large space of size n_pages
    pub(crate) fn bytes_needed_for(n_pages: usize) -> usize {
        LargeSpace::bitmap_bytes_needed_for(n_pages) + (n_pages * PAGE)
    }

    pub(crate) fn initialize_bitmap(&self) {
        for i in 0..self.n_pages.div_ceil(64) {
            unsafe { self.bitmap.add(i).write(AtomicU64::new(0)) };
        }
    }

    /// Safety: start has to be 8-byte aligned.
    pub(crate) unsafe fn init(start: NonNull<u8>, n_pages: usize) -> Self {
        let runs = if n_pages == 0 {
            BTreeMap::new()
        } else {
            BTreeMap::from([(0, n_pages)])
        };
        let offset = LargeSpace::bitmap_bytes_needed_for(n_pages);
        let space = LargeSpace {
            bitmap: start.cast(),
            first_page: unsafe { start.byte_add(offset) },
            n_pages,
            runs: Mutex::new(runs),
        };
        space.initialize_bitmap();
        space
    }

    pub(crate) fn page_addr_for_index(&self, page: usize) -> NonNull<u8> {
        unsafe { self.first_page.add(page * PAGE) }
    }

    pub(crate) fn page_of(&self, obj_addr: NonNull<u8>) -> Option<usize> {
        let offset = obj_addr
            .addr()
            .get()
            .wrapping_sub(self.first_page.addr().get());
        (offset < self.n_pages * PAGE).then_some(offset / PAGE)
    }

    pub(crate) fn claim_run(&self, n_pages: usize, align_bytes: usize) -> Option<Run> {
        let align_pages = align_bytes.div_ceil(PAGE).max(1);
        let mut runs = lock(&self.runs);

        let (start, length, at) = runs.iter().find_map(|(&start, &length)| {
            let at = start.next_multiple_of(align_pages);
            (at + n_pages <= start + length).then_some((start, length, at))
        })?;

        runs.remove(&start);
        if at > start {
            runs.insert(start, at - start);
        }
        if at + n_pages < start + length {
            runs.insert(at + n_pages, start + length - at - n_pages);
        }

        Some(Run {
            start_index: at,
            n_pages,
        })
    }

    pub(crate) fn release_run(&self, run: Run) {
        let mut runs = lock(&self.runs);
        let mut start = run.start_index;
        let mut length = run.n_pages;

        // hole before?
        if let Some((&prev, &prev_len)) = runs.range(..start).next_back()
            && prev + prev_len == start
        {
            runs.remove(&prev);
            start = prev;
            length += prev_len;
        }

        // hole after?
        if let Some(next_len) = runs.remove(&(start + length)) {
            length += next_len;
        }

        runs.insert(start, length);
    }

    fn bitmap_word_for(&self, page_index: usize) -> &AtomicU64 {
        unsafe { self.bitmap.add(page_index / 64).as_ref() }
    }

    pub(crate) fn mark_occupied(&self, page: usize) {
        self.bitmap_word_for(page)
            .fetch_or(1 << (page % 64), Ordering::Relaxed);
    }

    pub(crate) fn mark_unoccupied(&self, page: usize) {
        self.bitmap_word_for(page)
            .fetch_and(!(1 << (page % 64)), Ordering::Relaxed);
    }

    pub(crate) fn object_at_or_prior_to(&self, addr: NonNull<u8>) -> Option<NonNull<u8>> {
        let page = self.page_of(addr)?;
        let mut mask = u64::MAX >> (63 - page % 64);
        for word in (0..=page / 64).rev() {
            let bits = self.bitmap_word_for(word * 64).load(Ordering::Relaxed) & mask;
            if bits != 0 {
                return Some(self.object_at(word * 64 + 63 - bits.leading_zeros() as usize));
            }
            mask = u64::MAX;
        }
        None
    }

    pub(crate) fn object_at(&self, page: usize) -> NonNull<u8> {
        // large objects are always at page starts.
        unsafe { self.first_page.byte_add(page * PAGE) }
    }
}

#[cfg(test)]
#[cfg(not(loom))]
mod tests {
    use crate::large::BLOCK_SIZE;
    use crate::large::LargeSpace;
    use crate::large::PAGE;
    use crate::large::Run;
    use core::ptr::NonNull;
    use std::alloc::Layout;
    use std::alloc::alloc;

    fn test_space(pages: usize) -> LargeSpace {
        let layout = Layout::from_size_align(LargeSpace::bytes_needed_for(pages), BLOCK_SIZE)
            .expect("Failed to align to page");
        let addr = NonNull::new(unsafe { alloc(layout) }).expect("Test allocation failed");
        unsafe { LargeSpace::init(addr, pages) }
    }

    #[test]
    fn holes_are_first_fit_and_aligned() {
        let space = test_space(16);
        assert_eq!(
            space.claim_run(3, PAGE),
            Some(Run {
                start_index: 0,
                n_pages: 3
            })
        );
        assert_eq!(
            space.claim_run(2, 4 * PAGE),
            Some(Run {
                start_index: 4,
                n_pages: 2
            })
        );
        assert_eq!(
            space.claim_run(1, PAGE),
            Some(Run {
                start_index: 3,
                n_pages: 1
            })
        );
        assert_eq!(space.claim_run(16, PAGE), None);
    }

    #[test]
    fn freed_runs_coalesce() {
        let space = test_space(8);
        let a = space.claim_run(2, 1).unwrap();
        let b = space.claim_run(2, 1).unwrap();
        let _c = space.claim_run(4, 1).unwrap();
        space.release_run(a);
        space.release_run(b);
        assert_eq!(
            space.claim_run(4, 1),
            Some(Run {
                start_index: 0,
                n_pages: 4
            })
        );
    }

    #[test]
    fn starts_resolve_interior_pages() {
        let space = test_space(8);
        space.mark_occupied(2);
        space.mark_occupied(5);
        assert_eq!(
            space.object_at_or_prior_to(space.page_addr_for_index(2)),
            Some(space.page_addr_for_index(2))
        );
        assert_eq!(
            space.object_at_or_prior_to(unsafe { space.page_addr_for_index(4).byte_add(100) }),
            Some(space.page_addr_for_index(2))
        );
        assert_eq!(
            space.object_at_or_prior_to(space.page_addr_for_index(5)),
            Some(space.page_addr_for_index(5))
        );
        assert_eq!(
            space.object_at_or_prior_to(space.page_addr_for_index(1)),
            None
        );
        space.mark_unoccupied(2);
        assert_eq!(
            space.object_at_or_prior_to(space.page_addr_for_index(2)),
            None
        );
        assert_eq!(
            space.object_at_or_prior_to(space.page_addr_for_index(8)),
            None
        );
    }
}
