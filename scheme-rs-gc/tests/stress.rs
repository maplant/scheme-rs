#![cfg(not(loom))]

mod common;

use core::ptr::NonNull;
use std::{sync::mpsc, thread};

use common::*;
use scheme_rs_gc::Collector;

struct Sent(NonNull<u8>, usize, u8);

unsafe impl Send for Sent {}

fn check_and_free(c: &mut Collector<'_, TestModel>, Sent(p, size, fill): Sent) {
    let body = unsafe { core::slice::from_raw_parts(p.add(16).as_ptr(), size - 16) };
    assert!(
        body.iter().all(|&b| b == fill),
        "object {p:?} was overwritten"
    );
    unsafe { c.free(p) };
}

#[test]
fn mutators_and_a_collector_under_churn() {
    const THREADS: usize = 4;
    const PER_THREAD: usize = 50_000;
    let heap = TestHeap::new();
    let (tx, rx) = mpsc::channel();
    thread::scope(|s| {
        for t in 0..THREADS {
            let tx = tx.clone();
            let heap = &heap;
            s.spawn(move || {
                let mut m = heap.mutator();
                for i in 0..PER_THREAD {
                    let size = [24, 48, 200, 256, 700, 3000][i % 6];
                    let p = obj(&mut m, size, 16);
                    let fill = (t * 31 + i) as u8;
                    unsafe { p.add(16).write_bytes(fill, size - 16) };
                    tx.send(Sent(p, size, fill)).unwrap();
                }
            });
        }
        drop(tx);
        let mut c = heap.collector();
        // Every 5th object lives longer and dies out of order, so blocks
        // come back with holes and medium objects take the overflow path.
        let mut held = Vec::new();
        for (n, sent) in rx.iter().enumerate() {
            if n % 5 == 0 {
                held.push(sent);
                if held.len() > 2000 {
                    let i = n.wrapping_mul(2_654_435_761) % held.len();
                    check_and_free(&mut c, held.swap_remove(i));
                }
            } else {
                check_and_free(&mut c, sent);
            }
            if n % 1000 == 0 {
                c.sweep();
            }
        }
        for sent in held {
            check_and_free(&mut c, sent);
        }
    });
    assert!(
        heap.stats().overflow_bytes > 0,
        "overflow path not exercised"
    );
}
