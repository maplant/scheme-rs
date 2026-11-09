#![cfg(not(loom))]

mod common;

use std::{
    alloc::Layout,
    sync::atomic::{AtomicUsize, Ordering},
    time::{Duration, Instant},
};

use common::TestHeader;
use scheme_rs_gc::{BLOCK_SIZE, Collector, Heap, start_collector};

static EPOCHS: AtomicUsize = AtomicUsize::new(0);

struct CountingCollector;

impl Collector for CountingCollector {
    type Header = TestHeader;
    fn new(_heap: &'static Heap<TestHeader>) -> Self {
        CountingCollector
    }
    fn collect(&mut self) {
        EPOCHS.fetch_add(1, Ordering::Relaxed);
    }
    fn shutdown(&mut self) {}
}

#[test]
fn crossing_the_trigger_notifies_the_collector() {
    let heap = Box::leak(Box::new(Heap::<TestHeader>::new(4 * BLOCK_SIZE).unwrap()));
    let _gc = start_collector::<CountingCollector>(heap).unwrap();
    let mut mutator = heap.mutator();
    let layout = Layout::from_size_align(1024, 8).unwrap();

    while mutator.alloc(layout).is_ok() {}

    let deadline = Instant::now() + Duration::from_millis(200);
    while EPOCHS.load(Ordering::Relaxed) == 0 {
        assert!(Instant::now() < deadline, "collector was not notified");
        std::thread::yield_now();
    }
}
