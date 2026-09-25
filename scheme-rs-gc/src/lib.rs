//! Immix line/block heap: the first step toward an LXR collector for scheme-rs.

mod collector;
mod heap;
mod meta;
mod mutator;
mod sync;

#[cfg(all(test, loom))]
mod loom_tests;

use core::{alloc::Layout, ptr::NonNull};

pub use allocator_api2::alloc::{AllocError, Allocator, Global};
pub use collector::Collector;
pub use heap::{Heap, HeapStats};
pub use meta::{BLOCK_SIZE, LINE_SIZE, LOS_MAX_SIZE, MAX_ALIGN, META_LINES, MIN_SIZE};
pub use mutator::Mutator;

/// Tells the heap how to read an object it allocated.
///
/// # Safety
///
/// `layout` must return the exact layout `obj` was allocated with.
pub unsafe trait ObjectModel {
    /// # Safety
    ///
    /// `obj` was returned by a `Heap<Self, _>` and has not been freed.
    unsafe fn layout(obj: NonNull<u8>) -> Layout;
}
