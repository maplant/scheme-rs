#![cfg(not(loom))]

use std::{
    hint::black_box,
    panic::{AssertUnwindSafe, catch_unwind},
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU64, Ordering},
        mpsc,
    },
    thread::{sleep, spawn},
    time::{Duration, Instant},
};

use scheme_rs_gc::{
    for_each_threads_words, park_self, should_park, stop_the_world, with_gc_watching,
    without_gc_watching,
};

// tests run serially, as to not interfere with each other.
static SERIAL: Mutex<()> = Mutex::new(());

fn spinner(count: Arc<AtomicU64>, done: Arc<AtomicBool>) {
    with_gc_watching(|| {
        while !done.load(Ordering::Relaxed) {
            if should_park() {
                park_self();
            }
            count.fetch_add(1, Ordering::Relaxed);
        }
    })
}

#[test]
fn a_stop_halts_running_threads_until_released() {
    let _serial = SERIAL.lock().unwrap_or_else(|p| p.into_inner());
    let done = Arc::new(AtomicBool::new(false));
    let counts: Vec<_> = (0..4).map(|_| Arc::new(AtomicU64::new(0))).collect();
    let threads: Vec<_> = counts
        .iter()
        .map(|c| {
            let (c, d) = (c.clone(), done.clone());
            spawn(move || spinner(c, d))
        })
        .collect();

    sleep(Duration::from_millis(50));

    let stop = stop_the_world();
    let before: Vec<_> = counts.iter().map(|c| c.load(Ordering::Relaxed)).collect();

    sleep(Duration::from_millis(50));

    let after: Vec<_> = counts.iter().map(|c| c.load(Ordering::Relaxed)).collect();

    assert_eq!(before, after);
    drop(stop);
    sleep(Duration::from_millis(50));

    assert!(
        counts
            .iter()
            .zip(&after)
            .all(|(c, a)| c.load(Ordering::Relaxed) > *a)
    );

    done.store(true, Ordering::Relaxed);
    threads.into_iter().for_each(|t| t.join().unwrap());
}

#[test]
fn a_thread_without_gc_watching_does_not_delay_a_stop() {
    let _serial = SERIAL.lock().unwrap_or_else(|p| p.into_inner());
    let t =
        spawn(|| with_gc_watching(|| without_gc_watching(|| sleep(Duration::from_millis(500)))));
    sleep(Duration::from_millis(50));
    let start = Instant::now();
    let stop = stop_the_world();
    assert!(start.elapsed() < Duration::from_millis(200));
    drop(stop);
    t.join().unwrap();
}

#[test]
fn leaving_without_gc_watching_waits_for_the_stop() {
    let _serial = SERIAL.lock().unwrap_or_else(|p| p.into_inner());
    let back = Arc::new(AtomicBool::new(false));
    let b = back.clone();
    let t = spawn(move || {
        with_gc_watching(|| {
            without_gc_watching(|| sleep(Duration::from_millis(50)));
            // it should be parked here.
            b.store(true, Ordering::Relaxed);
        })
    });

    sleep(Duration::from_millis(10));
    let stop = stop_the_world();
    sleep(Duration::from_millis(200));

    assert!(!back.load(Ordering::Relaxed));

    drop(stop);
    t.join().unwrap();

    assert!(back.load(Ordering::Relaxed));
}

#[test]
fn the_scan_sees_values_live_across_park() {
    let _serial = SERIAL.lock().unwrap_or_else(|p| p.into_inner());
    const MARKER: usize = 0x5ca1_ab1e_0000_1230;
    let done = Arc::new(AtomicBool::new(false));
    let d = done.clone();
    let t = spawn(move || {
        with_gc_watching(|| {
            let marker = black_box(MARKER);
            while !d.load(Ordering::Relaxed) {
                if should_park() {
                    park_self();
                }
            }
            black_box(marker);
        })
    });

    sleep(Duration::from_millis(50));

    let stop = stop_the_world();
    let mut seen = false;

    for_each_threads_words(&stop, |w| seen |= w == MARKER);
    assert!(seen);

    done.store(true, Ordering::Relaxed);
    drop(stop);
    t.join().unwrap();
}

#[test]
fn the_scan_sees_values_live_across_re_entry() {
    let _serial = SERIAL.lock().unwrap_or_else(|p| p.into_inner());
    const MARKER: usize = 0x5ca1_ab1e_0000_1230;
    const MARKER_B: usize = 0x5ca1_ab1e_0000_1231;
    let done = Arc::new(AtomicBool::new(false));
    let count = Arc::new(AtomicU64::new(0));
    let c = count.clone();
    let d = done.clone();
    let t = spawn(move || {
        with_gc_watching(|| {
            let marker = black_box(MARKER);
            without_gc_watching(|| {
                with_gc_watching(|| {
                    let marker_b = black_box(MARKER_B);
                    while !d.load(Ordering::Relaxed) {
                        if should_park() {
                            park_self();
                        }
                        c.fetch_add(1, Ordering::Relaxed);
                    }
                    black_box(marker_b);
                });
            });
            black_box(marker);
        })
    });

    sleep(Duration::from_millis(50));

    let stop = stop_the_world();

    let before = count.load(Ordering::Relaxed);
    sleep(Duration::from_millis(50));
    let after = count.load(Ordering::Relaxed);

    assert_eq!(before, after);

    let mut seen = false;
    let mut seen_b = false;

    for_each_threads_words(&stop, |w| {
        seen |= w == MARKER;
        seen_b |= w == MARKER_B
    });
    assert!(seen);
    assert!(seen_b);

    done.store(true, Ordering::Relaxed);
    drop(stop);
    t.join().unwrap();
}

#[test]
fn panic_in_with_gc_watching_does_not_break_stops() {
    let _serial = SERIAL.lock().unwrap_or_else(|p| p.into_inner());
    let (unwind_tx, unwind) = mpsc::channel();
    let (done_tx, done) = mpsc::channel::<()>();
    let t = spawn(move || {
        let r = catch_unwind(AssertUnwindSafe(|| with_gc_watching(|| panic!("panic!"))));
        assert!(r.is_err());
        unwind_tx.send(()).unwrap();
        done.recv().unwrap();
    });
    unwind.recv().unwrap();

    let start = Instant::now();
    let stop = stop_the_world();
    assert!(start.elapsed() < Duration::from_millis(200));

    drop(stop);

    done_tx.send(()).unwrap();
    t.join().unwrap();
}

#[test]
fn panic_in_without_gc_watching_does_not_break_stops() {
    let _serial = SERIAL.lock().unwrap_or_else(|p| p.into_inner());
    let (unwind_tx, unwind) = mpsc::channel();
    let (done_tx, done) = mpsc::channel::<()>();
    let t = spawn(move || {
        let r = catch_unwind(AssertUnwindSafe(|| {
            with_gc_watching(|| without_gc_watching(|| panic!("panic!")))
        }));
        assert!(r.is_err());
        unwind_tx.send(()).unwrap();
        done.recv().unwrap();
    });
    unwind.recv().unwrap();

    let start = Instant::now();
    let stop = stop_the_world();
    assert!(start.elapsed() < Duration::from_millis(200));

    drop(stop);

    done_tx.send(()).unwrap();
    t.join().unwrap();
}

#[test]
fn park_outside_gc_does_nothing() {
    let _serial = SERIAL.lock().unwrap_or_else(|p| p.into_inner());
    let stop = stop_the_world();

    let (returned_tx, returned) = mpsc::channel();
    let t = spawn(move || {
        park_self(); // not inside with_gc_watching
        returned_tx.send(()).unwrap();
    });
    assert!(
        returned.recv_timeout(Duration::from_millis(200)).is_ok(),
        "park_self parked a thread outside the GC"
    );

    drop(stop);
    t.join().unwrap();
}
