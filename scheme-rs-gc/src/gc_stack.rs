#![allow(dead_code)]

use std::hint::black_box;

use parking_lot::Mutex;

const WORD: usize = size_of::<usize>();

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
compile_error!("stack direction and the capture shim are verified only on x86_64 and aarch64");

unsafe extern "C" {
    static gc_registers_size: usize;
    static gc_registers_align: usize;
    fn gc_capture_ctx(regs: *mut u8, sp: *mut usize);
}

pub(crate) struct Segment {
    base: usize,
    sp: usize,
    registers: Vec<u128>,
}

impl Segment {
    pub(crate) fn words(&self) -> Words<'_> {
        Words {
            registers: &self.registers,
            word_index: 0,
            top: self.sp,
            base: self.base,
        }
    }
}

pub(crate) struct GcStack {
    segments: Mutex<Vec<Segment>>,
    regs_words: usize,
}

impl GcStack {
    pub(crate) fn new() -> Self {
        assert!(
            unsafe { gc_registers_align } <= align_of::<u128>(),
            "jmp_buf needs more than 16-byte alignment"
        );
        GcStack {
            segments: Mutex::new(Vec::new()),
            regs_words: unsafe { gc_registers_size }.div_ceil(size_of::<u128>()),
        }
    }

    #[inline(always)]
    pub(crate) fn start_segment_here(&self) {
        let anchor = 0usize;
        let base = black_box(&anchor) as *const usize as usize / WORD * WORD;
        let registers = vec![0; self.regs_words];
        self.segments.lock().push(Segment {
            base,
            sp: base,
            registers,
        });
    }

    pub(crate) fn close_segment(&self) {
        self.segments.lock().pop();
    }

    #[inline(always)]
    pub(crate) fn capture(&self) {
        let mut segments = self.segments.lock();
        let seg = segments
            .last_mut()
            .expect("context and stack capture outside gc awareness");
        let mut sp = 0usize;
        unsafe { gc_capture_ctx(seg.registers.as_mut_ptr().cast(), &mut sp) };
        debug_assert!(sp <= seg.base);
        seg.sp = sp / WORD * WORD;
    }

    pub(crate) unsafe fn for_each_word(&self, f: &mut impl FnMut(usize)) {
        for segment in self.segments.lock().iter() {
            for word in segment.words() {
                f(word);
            }
        }
    }
}

pub(crate) struct Words<'a> {
    registers: &'a [u128],
    word_index: usize,
    top: usize,
    base: usize,
}

impl Iterator for Words<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        // First registers
        // The /2 is because the registers are stored 16 byte wide due to alignment
        if let Some(&pair) = self.registers.get(self.word_index / 2) {
            let word = if self.word_index.is_multiple_of(2) {
                pair
            } else {
                pair >> 64
            };
            self.word_index += 1;
            return Some(word as usize);
        }

        // Then stack
        if self.top >= self.base {
            // The empty range
            return None;
        }
        let word = unsafe { (self.top as *const usize).read_volatile() };
        self.top += WORD;
        Some(word)
    }
}
