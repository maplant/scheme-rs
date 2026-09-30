//! Immix line/block heap: the first step toward an LXR collector for scheme-rs.

#[cfg(not(loom))]
mod collection;
mod heap;
mod mutator;
mod reclaimer;
mod region;
mod sync;

#[cfg(all(test, loom))]
mod loom_tests;

use core::alloc::Layout;

pub use allocator_api2::alloc::{AllocError, Allocator, Global};
#[cfg(not(loom))]
pub use collection::{GcHeader, OpaqueGcPtr, VTable, collect_garbage, init_gc, unroot};
pub use heap::{Heap, HeapStats};
pub use mutator::Mutator;
pub use reclaimer::Reclaimer;
pub use region::{BLOCK_SIZE, LINE_SIZE, LOS_MAX_SIZE, MAX_ALIGN, MIN_SIZE};

/// Tells the heap how to read an object it allocated.
///
/// # Safety
///
/// Every object allocated in a `Heap<Self, _>` starts with an initialized
/// `Header` until it is freed, and `layout` returns the exact layout the
/// object was allocated with.
pub unsafe trait ObjectModel {
    /// The type every object starts with.
    type Header;

    fn layout(header: &Self::Header) -> Layout;
}
