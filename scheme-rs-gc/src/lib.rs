//! Immix line/block heap: the first step toward an LXR collector for scheme-rs.

mod bitmap;
mod collector;
mod cycle_collector;
mod gc_stack;
mod heap;
#[cfg(not(loom))]
mod heap_object;
mod large;
#[cfg(feature = "lxr")]
mod lxr_collector;
mod mutator;
mod reclaimer;
mod region;
mod safepoint;
mod sync;

#[cfg(all(test, loom))]
mod loom_tests;

pub use allocator_api2::alloc::{AllocError, Allocator, Global};
#[cfg(not(loom))]
pub use cycle_collector::{CycleCollector, CycleHeader, CycleList, collect_garbage_in, unroot_in};
#[cfg(not(loom))]
pub use heap_object::{HEADER_SIZE, Header, HeapObject, VTable};

pub use heap::{Heap, HeapStats, ObjectHeader};
pub use mutator::Mutator;
pub use reclaimer::Reclaimer;
pub use region::{BLOCK_SIZE, GRANULE, LINE_SIZE, LOS_MAX_SIZE, MAX_ALIGN};
pub use safepoint::{
    Stopped, for_each_threads_words, park_self, should_park, stop_the_world, with_gc_watching,
    without_gc_watching,
};

#[cfg(not(loom))]
pub use collector::{
    Collector, CollectorAlreadyRunning, CollectorHandle, DefaultCollector, on_collector_thread,
    start_collector,
};

#[cfg(feature = "lxr")]
pub(crate) use lxr_collector::LxrCollector;

#[cfg(not(loom))]
pub use safepoint::safepoint_flag;
