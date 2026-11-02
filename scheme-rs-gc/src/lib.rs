//! Immix line/block heap: the first step toward an LXR collector for scheme-rs.

mod bitmap;
#[cfg(not(loom))]
mod collection;
mod gc_stack;
#[cfg(not(loom))]
mod global;
mod heap;
mod large;
mod mutator;
mod reclaimer;
mod region;
mod safepoint;
mod sync;

#[cfg(all(test, loom))]
mod loom_tests;

use core::alloc::Layout;

pub use allocator_api2::alloc::{AllocError, Allocator, Global};
#[cfg(not(loom))]
pub use collection::{
    HEADER_SIZE, HeapObject, VTable, collect_garbage, init_gc, shutdown_gc, unroot,
};
#[cfg(not(loom))]
pub use global::{HeapAlreadyCreated, budget, set_heap_size};
pub use heap::{Heap, HeapStats};
pub use mutator::Mutator;
pub use reclaimer::Reclaimer;
pub use region::{BLOCK_SIZE, GRANULE, LINE_SIZE, LOS_MAX_SIZE, MAX_ALIGN};
pub use safepoint::{
    Stopped, for_each_threads_words, park_self, should_park, stop_the_world, with_gc_watching,
    without_gc_watching,
};

#[cfg(not(loom))]
pub use safepoint::safepoint_flag;

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
