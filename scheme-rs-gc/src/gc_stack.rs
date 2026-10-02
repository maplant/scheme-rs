use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::hint::black_box;

use parking_lot::{Mutex, MutexGuard};

const WORD: usize = size_of::<usize>();

unsafe extern "C" {
    static gc_registers_size: usize;
    fn gc_capture_ctx(regs: *mut u8, sp: *mut usize);
}

pub(crate) struct GcStack {
    base: AtomicUsize,
    sp: AtomicUsize,
    registers: Mutex<Box<[u128]>>,
}

#[inline(always)]
fn current_sp() -> usize {
    let anchor = 0usize;
    return ((black_box(&anchor) as *const usize) as usize) / WORD * WORD;
}

impl GcStack {
    pub(crate) fn new() -> Self {
        let words = unsafe { gc_registers_size }.div_ceil(size_of::<u128>());
        GcStack {
            base: AtomicUsize::new(0),
            sp: AtomicUsize::new(0),
            registers: Mutex::new(vec![0; words].into_boxed_slice()),
        }
    }

    #[inline(always)] // As to not capture a new stack frame
    pub(crate) fn set_base(&self) {
        self.base.store(current_sp(), Ordering::Relaxed);
    }

    pub(crate) fn clear(&self) {
        self.sp.store(self.base.load(Ordering::Relaxed), Ordering::Relaxed);
    }

    #[inline(always)]
    pub(crate) fn capture(&self) {
        let mut registers = self.registers.lock();
        let mut sp = 0usize;
        unsafe { gc_capture_ctx(registers.as_mut_ptr().cast(), &mut sp) };
        self.sp.store(sp * WORD / WORD, Ordering::Relaxed);
    }

    pub(crate) unsafe fn words(&self) -> Words<'_> {
        Words {
            registers: self.registers.lock(),
            word_index: 0,
            top: self.sp.load(Ordering::Relaxed),
            base: self.base.load(Ordering::Relaxed)
        }
    }
}

pub(crate) struct Words<'a> {
    registers: MutexGuard<'a, Box<[u128]>>,
    word_index: usize,
    top: usize,
    base: usize
}

impl Iterator for Words<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        // First registers
        if let Some(&pair) = self.registers.get(self.word_index / 2) {
            let word = if self.word_index % 2 == 0 { pair } else { pair >> 64 };
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
