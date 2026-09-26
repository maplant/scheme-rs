//! Loom models of the block handoffs. Run exhaustively, in debug so the
//! heap's debug assertions are checked:
//!
//! `RUSTFLAGS="--cfg loom" CARGO_TARGET_DIR=target/loom cargo test -p scheme-rs-gc --lib loom_tests`

use core::{alloc::Layout, ptr::NonNull};

use loom::{
    sync::{Arc, Mutex},
    thread,
};

use crate::{
    Heap, Mutator, ObjectModel,
    region::{BLOCK_SIZE, LINE_SIZE},
    sync::lock,
};

struct Model;

unsafe impl ObjectModel for Model {
    type Header = Layout;

    fn layout(header: &Layout) -> Layout {
        *header
    }
}

type TestHeap = Heap<Model>;

fn test_heap() -> TestHeap {
    TestHeap::new(4 * BLOCK_SIZE).unwrap()
}

struct Sent(NonNull<u8>);

unsafe impl Send for Sent {}

fn obj(m: &mut Mutator<'_, Model>, size: usize) -> NonNull<u8> {
    let layout = Layout::from_size_align(size, 16).unwrap();
    let obj = m.alloc(layout).unwrap();
    unsafe { obj.cast::<Layout>().write(layout) };
    obj
}

#[test]
fn owner_increment_races_collector_decrement() {
    loom::model(|| {
        let heap = Arc::new(test_heap());
        let mut m = heap.mutator();
        let x = Sent(obj(&mut m, 32));
        let h = heap.clone();
        let t = thread::spawn(move || {
            let x = x;
            unsafe { h.collector().free(x.0) };
        });
        let y = obj(&mut m, 32);
        t.join().unwrap();
        let state = lock(&heap.collector);
        let count = heap
            .region
            .line_count(&state.token, heap.region.block_of(y), 0);
        assert_eq!(count, 1);
    });
}

#[test]
fn sweep_sees_complete_counts_after_retire() {
    loom::model(|| {
        let heap = Arc::new(test_heap());
        let h = heap.clone();
        let t = thread::spawn(move || {
            let mut m = h.mutator();
            obj(&mut m, LINE_SIZE);
            obj(&mut m, LINE_SIZE);
        });
        let mut c = heap.collector();
        // May run before or after the retire; both orders must publish once.
        c.sweep();
        t.join().unwrap();
        c.sweep();
        c.sweep();
        let recycled = lock(&heap.recycled);
        let holes = recycled[0].1;
        assert_eq!(holes & 0b11, 0);
        assert_ne!(holes & 0b100, 0);
    });
}

/// The collector frees into a block while its owner still allocates, then
/// sweeps it after retirement. The retired-list mutex is what makes the
/// owner's counts visible to the sweep.
#[test]
fn dirty_sweep_sees_complete_counts_after_retire() {
    loom::model(|| {
        let heap = Arc::new(test_heap());
        let slot = Arc::new(Mutex::new(None));
        let (h, s) = (heap.clone(), slot.clone());
        let t = thread::spawn(move || {
            let mut m = h.mutator();
            *lock(&s) = Some(Sent(obj(&mut m, LINE_SIZE)));
            obj(&mut m, LINE_SIZE);
        });
        let mut c = heap.collector();
        let x = loop {
            if let Some(x) = lock(&slot).take() {
                break x;
            }
            thread::yield_now();
        };
        unsafe { c.free(x.0) };
        c.sweep();
        t.join().unwrap();
        c.sweep();
        c.sweep();
        let stats = heap.stats();
        let recycled = lock(&heap.recycled);
        assert_eq!(recycled.len(), 1, "{stats:?}");
        let holes = recycled[0].1;
        assert_eq!(holes & 0b10, 0, "live line published as free");
    });
}

#[test]
fn two_mutators_never_share_a_recycled_block() {
    loom::model(|| {
        let heap = Arc::new(test_heap());
        let x = {
            let mut m = heap.mutator();
            let x = obj(&mut m, LINE_SIZE);
            obj(&mut m, LINE_SIZE);
            x
        };
        let block = x.addr().get() & !(BLOCK_SIZE - 1);
        {
            let mut c = heap.collector();
            unsafe { c.free(x) };
            c.sweep();
            c.sweep();
        }
        assert_eq!(lock(&heap.recycled).len(), 1);
        let spawn = |heap: Arc<TestHeap>| thread::spawn(move || Sent(obj(&mut heap.mutator(), 32)));
        let a = spawn(heap.clone());
        let b = spawn(heap.clone());
        let (a, b) = (a.join().unwrap().0, b.join().unwrap().0);
        assert!(lock(&heap.recycled).is_empty());
        let in_block = [a, b]
            .iter()
            .filter(|p| p.addr().get() & !(BLOCK_SIZE - 1) == block)
            .count();
        assert_eq!(
            in_block, 1,
            "the recycled block went to exactly one mutator"
        );
    });
}

/// The sweep takes a Recycled block that was freed into back out of the pool
/// while a mutator may be taking it from the same pool.
#[test]
fn sweep_reclaim_races_a_mutator_taking_the_block() {
    loom::model(|| {
        let heap = Arc::new(test_heap());
        let (x, y) = {
            let mut m = heap.mutator();
            (obj(&mut m, LINE_SIZE), obj(&mut m, LINE_SIZE))
        };
        let mut c = heap.collector();
        unsafe { c.free(x) };
        c.sweep();
        c.sweep();
        assert_eq!(lock(&heap.recycled).len(), 1);
        unsafe { c.free(y) };
        let h = heap.clone();
        let t = thread::spawn(move || Sent(obj(&mut h.mutator(), 32)));
        c.sweep();
        let z = t.join().unwrap().0;
        c.sweep();
        // Whichever of the reclaim and the mutator won, the pool must never
        // offer the line that holds the live object z.
        let region = &heap.region;
        let z_line = region.offset_of(region.block_of(z), z) / LINE_SIZE;
        let z_block = region.block_of(z);
        assert!(
            !lock(&heap.free).iter().any(|block| block.id() == z_block),
            "live block published as free"
        );
        for (block, holes) in lock(&heap.recycled).iter() {
            if block.id() == z_block {
                assert_eq!(holes & (1 << z_line), 0, "live line published as free");
            }
        }
    });
}
