#![cfg(not(loom))]

mod common;

use common::TestHeader;
use scheme_rs_gc::{BLOCK_SIZE, Collector, Heap, start_collector};

struct IdleCollector;

impl Collector for IdleCollector {
    type Header = TestHeader;
    fn new(_heap: &'static Heap<TestHeader>) -> Self {
        IdleCollector
    }
    fn collect(&mut self) {}
    fn shutdown(&mut self) {}
}

#[test]
fn one_collector_per_heap() {
    let heap = Box::leak(Box::new(Heap::<TestHeader>::new(BLOCK_SIZE).unwrap()));

    let gc = start_collector::<IdleCollector>(heap).unwrap();
    assert!(start_collector::<IdleCollector>(heap).is_err());

    drop(gc);

    let _gc = start_collector::<IdleCollector>(heap).unwrap();
    assert!(start_collector::<IdleCollector>(heap).is_err());
}
