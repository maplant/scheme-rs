//! The control stack for scheme-rs.
//!
//! Implementation of segmented stacks from "Representing Control in the
//! Presence of First-Class Continuations" by Robert Hieb, R. Kent Dybvig, and
//! Carl Bruggeman.

use scheme_rs_macros::rtd;

use crate::{
    gc::{Gc, Trace},
    records::{Embeddable, RecordTypeDescriptor},
    value::Value,
};
use std::{
    cell::UnsafeCell,
    mem::ManuallyDrop,
    ptr::NonNull,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

const BLOCK_SIZE: usize = 2048;

/// 16 Kb block of memory.
struct StackBlock {
    slots: NonNull<[UnsafeCell<Value>; BLOCK_SIZE]>,
    sealed: AtomicUsize,
}

unsafe impl Send for StackBlock {}
unsafe impl Sync for StackBlock {}

impl StackBlock {
    fn new() -> Gc<Self> {
        let slots =
            unsafe { NonNull::new_unchecked(Box::into_raw(Box::new_zeroed().assume_init())) };
        Gc::new(Self {
            slots,
            sealed: AtomicUsize::new(0),
        })
    }
}

unsafe impl Trace for StackBlock {
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(crate::gc::OpaqueGcPtr)) {
        let sealed = self.sealed.load(Ordering::Acquire);
        let slots = unsafe { self.slots.as_ref() };
        for i in 0..sealed {
            unsafe {
                (*slots[i].get()).visit_children(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        let sealed = self.sealed.load(Ordering::Acquire);
        let slots = unsafe { self.slots.as_mut() };
        for slot in &mut slots[..sealed] {
            unsafe {
                slot.get_mut().finalize();
            }
        }
        unsafe {
            let _ = Box::from_raw(
                self.slots.as_ptr() as *mut ManuallyDrop<[UnsafeCell<Value>; BLOCK_SIZE]>
            );
        }
    }
}

#[derive(Trace)]
struct SealedStackRecord {
    next: Option<Gc<SealedStackRecord>>,
    #[trace(skip)]
    segment: NonNull<Value>,
    cap: usize,
    block: Gc<StackBlock>,
}

impl SealedStackRecord {
    fn split(this: &Gc<Self>) -> Option<SealedStackRecord> {
        const MAX_CAP: usize = 1024;

        (this.cap > MAX_CAP).then(|| {
            // Split the next stack record into two
            SealedStackRecord {
                segment: unsafe { this.segment.add(this.cap - MAX_CAP) },
                cap: MAX_CAP,
                block: this.block.clone(),
                // Second part of the split:
                next: Some(Gc::new(SealedStackRecord {
                    next: this.next.clone(),
                    segment: this.segment,
                    cap: this.cap - MAX_CAP,
                    block: this.block.clone(),
                })),
            }
        })
    }
}

unsafe impl Send for SealedStackRecord {}
unsafe impl Sync for SealedStackRecord {}

pub struct StackRecord {
    next: Option<Gc<SealedStackRecord>>,
    segment: NonNull<Value>,
    len: usize,
    cap: usize,
    block: Gc<StackBlock>,
}

unsafe impl Send for StackRecord {}
unsafe impl Sync for StackRecord {}

impl StackRecord {
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn next(&self) -> &Gc<SealedStackRecord> {
        self.next.as_ref().unwrap()
    }

    fn alloc(next: Option<Gc<SealedStackRecord>>) -> Self {
        let block = StackBlock::new();
        Self {
            next,
            segment: NonNull::new(block.slots.as_ptr() as *mut Value).unwrap(),
            len: 0,
            cap: BLOCK_SIZE,
            block,
        }
    }

    pub fn seal(&mut self) -> Gc<SealedStackRecord> {
        if self.is_empty() {
            return self.next().clone();
        }

        let sealed = Gc::new(SealedStackRecord {
            next: self.next.clone(),
            cap: self.len,
            segment: self.segment,
            block: self.block.clone(),
        });

        self.block.sealed.fetch_add(self.len, Ordering::Release);

        self.next = Some(sealed.clone());
        self.segment = unsafe { self.segment.add(self.len) };
        self.cap -= self.len;
        self.len = 0;

        sealed
    }

    #[inline]
    pub fn reinstate(&mut self, new_stack: Gc<SealedStackRecord>) {
        let new_stack = if let Some(new_next) = SealedStackRecord::split(&new_stack) {
            Gc::new(new_next)
        } else {
            new_stack
        };

        // Drop any leftover values on the stack:
        for i in 0..self.len {
            unsafe {
                let _ = self.segment.add(i).replace(Value::undefined());
            }
        }

        // Copy the next segment into the current stack; allocate a new
        // segment if there's no room (or just enough room).
        if new_stack.cap >= self.cap {
            *self = Self::alloc(self.next.clone());
        }
        self.len = new_stack.cap;

        // Clone over all of the values:
        for i in 0..new_stack.cap {
            unsafe {
                self.segment
                    .add(i)
                    .write(Value::from_raw_inc_rc(Value::as_raw(
                        new_stack.segment.add(i).as_ref(),
                    )));
            }
        }

        // Point to the next region:
        self.next = new_stack.next.clone();
    }

    pub fn pop(&mut self) -> Value {
        // Check for underflow
        if self.len == 0 {
            let next = self.next().clone();
            self.reinstate(next);
        }

        // Pop the value
        self.len -= 1;

        unsafe { self.segment.add(self.len).replace(Value::undefined()) }
    }

    pub fn push(&mut self, value: Value) {
        unsafe {
            self.segment.add(self.len).write(value);
        }

        self.len += 1;

        if self.cap == self.len {
            // Overflow: seal the current segment and allocate a new one
            self.seal();
            let next = self.next.take();
            *self = Self::alloc(next);
        }
    }
}

impl Drop for StackRecord {
    fn drop(&mut self) {
        for i in 0..self.len {
            unsafe {
                let _ = self.segment.add(i).replace(Value::undefined());
            }
        }
    }
}

#[derive(Copy, Clone, Trace)]
struct PromptBarrier(usize);

unsafe impl Embeddable for PromptBarrier {
    fn rtd() -> Arc<RecordTypeDescriptor>
    where
        Self: Sized,
    {
        rtd! {
            name: "%prompt-barrier",
            ty: PromptBarrier,
            sealed: true,
            opaque: true,
        }
    }
}
