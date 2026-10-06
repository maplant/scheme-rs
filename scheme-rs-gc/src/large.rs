use core::ptr::NonNull;
use std:collections::BTreeMap;

use crate::sync::{AtomicU64, Mutex, Ordering, lock};

type PageIndex = usize;
type Address = NonNull<u8>;

const PAGE: usize = 4096; // bytes

struct LargeSpace {
    bitmap: NonNull<AtomicU64>,
    first_page: NonNull<u8>,
    n_pages: usize,
    // A map of free consecutive pages, K being starts and V being ends. Starts with one: (0, N)
    free_holes: Mutex<BTreeMap<usize, usize>>
}

impl LargeSpace {
    // bytes needed to fit a bitmap of size n_pages
    pub(crate) fn bitmap_bytes_needed_for(n_pages: usize) -> usize {
        (n_pages.div_ceil(64) * size_of::<AtomicU64>()).next_multiple_of(PAGE)
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

    pub(crate) unsafe fn init(start: NonNull<u8>, n_pages: usize) -> Self {
        let free_holes = if n_pages == 0 { BTreeMap::new() } else { BTreeMap::from([(0, n_pages)]) };
        let offset = LargeSpace::bitmap_bytes_needed_for(n_pages);
        let space = LargeSpace {
            bitmap: start,
            first_page: unsafe { start.byte_add(offset) },
            n_pages: n_pages,
            free_holes: Mutex::new(free_holes)
        };
        space.initialize_bitmap();
        space
    }

    pub(crate) fn page_addr_for_index(&self, page: usize) -> usize {
        self.first_page.addr().get() + page * PAGE
    }

    pub(crate) fn page_of(&self, obj_addr: usize) -> Option<usize> {
        let offset = obj_addr.wrapping_sub(self.first_page.addr().get());
        (offset < self.n_pages * PAGE).then_some(offset / PAGE)
    }

    pub(crate) fn claim_hole(&self, n_pages: usize, align_bytes: usize) -> Option<usize> {
        let align_pages = align_bytes.div_ceil(PAGE);
        let mut holes = lock(&self.free_holes);

        let (start, length, at) = holes.iter().find_map(|(&start, &length)| {
            let at = start.next_multiple_of(align_pages);
            (at + n_pages <= start + length).then_some((start, length, at))
        })?;

        holes.remove(&start);
        if at > start {
            holes.insert(start, at - start);
        }
        if at + n_pages < start + length {
            holes.insert (at + n_pages, start + length - at - n_pages);
        }

        Some(at)
    }

    pub(crate) fn release_hole(&self, mut start: usize, mut length: usize) {
        let mut holes = lock(&self.free_holes);

        // hole before?
        if let Some((&prev, &prev_len)) = holes.range(..start).next_back()
            && prev + prev_len == start
        {
            holes.remove(&prev);
            start = prev;
            length += prev_len;
        }

        // hole after?
        if let Some(next_len) = holes.remove(&(start + length)) {
            length += next_len;
        }

        holes.insert(start, length);
    }

    fn bitmap_word_for(&self, page_index: usize) -> &AtomicU64 {
        unsafe { self.bitmap.add(page_index / 64).as_ref() }
    }

    pub(crate) fn set_start(&self, page: usize) {
        self.bitmap_word_for(page).fetch_or(1 << (page % 64), Ordering::Relaxed);
    }

    pub(crate) fn clear_start(&self, page: usize) {
        self.bitmap_word_for(page).fetch_and(!(1 << (page % 64)), Ordering::Relaxed);
    }

    pub(crate) fn object_at_or_prior_to(&self, addr: usize) -> Option<NonNull<u8>> {
        let page = self.page_of(addr)?;
        let mut mask = u64::MAX >> (63 - page % 64);
        for word in (0..=page / 64).rev() {
            let bits = self.bitmap_word_for(word * 64).load(Ordering::Relaxed) & mask;
            if bits != 0 {
                return Some(self.object_at(page * 64 + 63 - bits.leading_zeros() as usize));
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
