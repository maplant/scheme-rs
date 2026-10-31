#![cfg(not(loom))]

use std::{
    cell::Cell,
    thread::{JoinHandle, spawn},
};

use crate::{
    ObjectHeader,
    sync::{lock, wait_while},
};

#[cfg(not(feature = "lxr"))]
use crate::CycleCollector;
#[cfg(feature = "lxr")]
use crate::LxrCollector;
use crate::{HeapObject, VTable, heap::Heap};

pub(crate) static INVALID_VTABLE: VTable = VTable {
    visit_children: |_, _| unreachable!(),
    finalize: |_| unreachable!(),
};

#[derive(Debug)]
pub(crate) enum DropAction {
    Decrement(HeapObject),
    Release(HeapObject),
    Free(HeapObject),
}

thread_local! {
    pub(crate) static ON_COLLECTOR_THREAD: Cell<bool> = const { Cell::new(false) };
}

/// A collector algorithm. `start_collector` constructs it on the collector
/// thread and calls it from there only.
pub trait Collector: Sized {
    type Header: ObjectHeader + 'static;
    /// Called once, on the collector thread, before the first epoch.
    fn new(heap: &'static Heap<Self::Header>) -> Self;
    /// Runs one epoch. Must not wait on the heap's collection signal. To run
    /// another epoch right after this one, set `requested`.
    fn collect(&mut self);
    /// Called once, on the collector thread, after the last epoch. Cleans up
    /// collector state; the thread is joined when the `CollectorHandle` drops.
    fn shutdown(&mut self);
}

#[cfg(feature = "lxr")]
pub type DefaultCollector = LxrCollector;

#[cfg(not(feature = "lxr"))]
pub type DefaultCollector = CycleCollector;

#[derive(Debug)]
pub struct CollectorAlreadyRunning;

/// Owns a heap's collector thread. Dropping it stops the collector and waits
/// for it to exit. Pending requests run first, so threads that keep
/// requesting collections delay the drop.
pub struct CollectorHandle<H: ObjectHeader + 'static> {
    heap: &'static Heap<H>,
    thread: Option<JoinHandle<()>>,
}

impl<H: ObjectHeader + 'static> Drop for CollectorHandle<H> {
    fn drop(&mut self) {
        lock(&self.heap.signal.status).shutdown = true;
        self.heap.signal.start.notify_all();
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
        lock(&self.heap.signal.status).shutdown = false;
    }
}

/// Starts a collector thread for `heap`.
///
/// There is one collector per heap: fails if a collector is already running.
pub fn start_collector<C: Collector>(
    heap: &'static Heap<C::Header>,
) -> Result<CollectorHandle<C::Header>, CollectorAlreadyRunning> {
    let mut status = lock(&heap.signal.status);
    if status.alive {
        return Err(CollectorAlreadyRunning);
    }
    status.alive = true;
    // Before the spawn: a mutator that allocates right after this call must
    // already see the trigger.
    heap.set_trigger();
    drop(status);
    let thread = spawn(move || run::<C>(heap));
    Ok(CollectorHandle {
        heap,
        thread: Some(thread),
    })
}

// So that panics shut the collector off.
struct ClearAlive<H: ObjectHeader + 'static>(&'static Heap<H>);

impl<H: ObjectHeader + 'static> Drop for ClearAlive<H> {
    fn drop(&mut self) {
        lock(&self.0.signal.status).alive = false;
        self.0.signal.done.notify_all();
    }
}

fn run<C: Collector>(heap: &'static Heap<C::Header>) {
    ON_COLLECTOR_THREAD.set(true);
    let _alive = ClearAlive(heap);
    let mut collector = C::new(heap);

    loop {
        let mut status = lock(&heap.signal.status);

        status = wait_while(&heap.signal.start, status, |s| !s.requested && !s.shutdown);

        if status.shutdown && !status.requested {
            break;
        }

        status.running = true;
        status.requested = false;
        drop(status);

        collector.collect();

        let mut status = lock(&heap.signal.status);
        status.epoch += 1;
        heap.set_trigger();
        status.running = false;
        heap.signal.done.notify_all();
    }

    collector.shutdown();
}

pub fn on_collector_thread() -> bool {
    ON_COLLECTOR_THREAD.get()
}
