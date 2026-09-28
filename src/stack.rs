//! The control stack for scheme-rs.
//!
//! Implementation of segmented stacks from "Representing Control in the
//! Presence of First-Class Continuations" by Robert Hieb, R. Kent Dybvig, and
//! Carl Bruggeman.

use crate::{gc::{Trace, Gc}, value::Value};
use std::{ptr::NonNull, sync::LazyLock};

/// 16 Kb block of memory.
type StackSegment = NonNull<Value>;

pub struct StackRecord {
    next: Option<Gc<StackRecord>>,
    segment: StackSegment,
    len: usize,
    cap: usize,
}

unsafe impl Trace for StackRecord {
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(crate::gc::OpaqueGcPtr)) {
        todo!()
    }

    unsafe fn finalize(&mut self) {
        todo!()
    }
}  

impl StackRecord {
    
}
