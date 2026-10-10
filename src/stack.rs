//! The control stack for scheme-rs.
//!
//! Implementation of segmented stacks from "Representing Control in the
//! Presence of First-Class Continuations" by Robert Hieb, R. Kent Dybvig, and
//! Carl Bruggeman.

use crate::{
    gc::{Gc, Trace},
    value::Value,
};
use std::{
    cell::UnsafeCell,
    mem::ManuallyDrop,
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
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

#[derive(Trace, Clone)]
#[repr(transparent)]
pub struct SealedStackRecord(Gc<SealedStackRecordInner>);

impl SealedStackRecord {
    fn split(&self) -> Option<SealedStackRecord> {
        const MAX_CAP: usize = 1024;

        (self.0.cap > MAX_CAP).then(|| {
            // Split the next stack record into two
            Self(Gc::new(SealedStackRecordInner {
                segment: unsafe { self.0.segment.add(self.0.cap - MAX_CAP) },
                cap: MAX_CAP,
                block: self.0.block.clone(),
                // Second part of the split:
                next: Some(SealedStackRecord(Gc::new(SealedStackRecordInner {
                    next: self.0.next.clone(),
                    segment: self.0.segment,
                    cap: self.0.cap - MAX_CAP,
                    block: self.0.block.clone(),
                }))),
            }))
        })
    }
}

impl PartialEq for SealedStackRecord {
    fn eq(&self, other: &Self) -> bool {
        Gc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Trace)]
struct SealedStackRecordInner {
    next: Option<SealedStackRecord>,
    #[trace(skip)]
    segment: NonNull<Value>,
    cap: usize,
    block: Gc<StackBlock>,
}

unsafe impl Send for SealedStackRecordInner {}
unsafe impl Sync for SealedStackRecordInner {}

pub struct StackRecord {
    next: Option<SealedStackRecord>,
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

    fn next(&self) -> &SealedStackRecord {
        self.next.as_ref().unwrap()
    }

    pub fn alloc(next: Option<SealedStackRecord>) -> Self {
        let block = StackBlock::new();
        Self {
            next,
            segment: NonNull::new(block.slots.as_ptr() as *mut Value).unwrap(),
            len: 0,
            cap: BLOCK_SIZE,
            block,
        }
    }

    pub fn seal(&mut self) -> SealedStackRecord {
        if self.is_empty() {
            return self.next().clone();
        }

        let sealed = SealedStackRecord(Gc::new(SealedStackRecordInner {
            next: self.next.clone(),
            cap: self.len,
            segment: self.segment,
            block: self.block.clone(),
        }));

        self.block.sealed.fetch_add(self.len, Ordering::Release);

        self.next = Some(sealed.clone());
        self.segment = unsafe { self.segment.add(self.len) };
        self.cap -= self.len;
        self.len = 0;

        sealed
    }

    #[inline]
    pub fn reinstate(&mut self, new_stack: SealedStackRecord) {
        let new_stack = if let Some(new_next) = new_stack.split() {
            new_next
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
        if new_stack.0.cap >= self.cap {
            *self = Self::alloc(self.next.clone());
        }
        self.len = new_stack.0.cap;

        // Clone over all of the values:
        for i in 0..new_stack.0.cap {
            unsafe {
                self.segment
                    .add(i)
                    .write(Value::from_raw_inc_rc(Value::as_raw(
                        new_stack.0.segment.add(i).as_ref(),
                    )));
            }
        }

        // Point to the next region:
        self.next = new_stack.0.next.clone();
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

    /// Returns the value on the top of the stack, panicking if none exists
    pub fn top(&self) -> &Value {
        if self.len == 0 {
            let next_record = self.next.as_ref().unwrap();
            unsafe { next_record.0.segment.add(next_record.0.cap - 1).as_ref() }
        } else {
            unsafe { self.segment.add(self.len - 1).as_ref() }
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
