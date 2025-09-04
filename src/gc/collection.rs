//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use std::{
    ptr::NonNull,
    sync::{Mutex, OnceLock},
    time::{Duration, Instant},
};
use tokio::{
    sync::mpsc::{UnboundedReceiver, UnboundedSender, unbounded_channel},
    task::JoinHandle,
};

use super::{Color, GcInner, OpaqueGcPtr};

#[derive(Copy, Clone)]
pub struct Mutation {
    kind: MutationKind,
    gc: OpaqueGcPtr,
}

impl Mutation {
    fn new(kind: MutationKind, gc: OpaqueGcPtr) -> Self {
        Self { kind, gc }
    }
}

unsafe impl Send for Mutation {}
unsafe impl Sync for Mutation {}

#[derive(Copy, Clone, Debug)]
pub enum MutationKind {
    Inc,
    Dec,
}

/// Instead of mutations being atomic (via an atomic variable), they're buffered into
/// "epochs", and handled by precisely one thread.
struct MutationBuffer {
    mutation_buffer_tx: UnboundedSender<Mutation>,
    mutation_buffer_rx: Mutex<Option<UnboundedReceiver<Mutation>>>,
}

unsafe impl Sync for MutationBuffer {}

impl Default for MutationBuffer {
    fn default() -> Self {
        let (mutation_buffer_tx, mutation_buffer_rx) = unbounded_channel();
        Self {
            mutation_buffer_tx,
            mutation_buffer_rx: Mutex::new(Some(mutation_buffer_rx)),
        }
    }
}

static MUTATION_BUFFER: OnceLock<MutationBuffer> = OnceLock::new();

pub(super) fn inc_rc<T: ?Sized>(gc: NonNull<GcInner<T>>) {
    // Disregard any send errors. If the receiver was dropped then the process
    // is exiting and we don't care if we leak.
    let _ = MUTATION_BUFFER
        .get_or_init(MutationBuffer::default)
        .mutation_buffer_tx
        .send(Mutation::new(MutationKind::Inc, OpaqueGcPtr::from(gc)));
}

pub(super) fn dec_rc<T: ?Sized>(gc: NonNull<GcInner<T>>) {
    // Disregard any send errors. If the receiver was dropped then the process
    // is exiting and we don't care if we leak.
    let _ = MUTATION_BUFFER
        .get_or_init(MutationBuffer::default)
        .mutation_buffer_tx
        .send(Mutation::new(MutationKind::Dec, OpaqueGcPtr::from(gc)));
}

static COLLECTOR_TASK: OnceLock<JoinHandle<()>> = OnceLock::new();

pub fn init_gc() {
    // SAFETY: We DO NOT mutate MUTATION_BUFFER, we mutate the _interior once lock_.
    let _ = MUTATION_BUFFER.get_or_init(MutationBuffer::default);
    let _ = COLLECTOR_TASK
        .get_or_init(|| tokio::task::spawn(async { unsafe { run_garbage_collector().await } }));
}

const MIN_MUTATIONS_PER_EPOCH: usize = 10;
const MAX_MUTATIONS_PER_EPOCH: usize = 10_000; // No idea what a good value is here.

async unsafe fn run_garbage_collector() {
    unsafe {
        let mut last_epoch = Instant::now();
        let mut mutation_buffer: Vec<_> = Vec::with_capacity(MAX_MUTATIONS_PER_EPOCH);
        let mut mutation_buffer_rx = MUTATION_BUFFER
            .get_or_init(MutationBuffer::default)
            .mutation_buffer_rx
            .lock()
            .unwrap()
            .take()
            .unwrap();
        while epoch(
            &mut last_epoch,
            &mut mutation_buffer_rx,
            &mut mutation_buffer,
        )
        .await
        {}
    }
}

// Run a collection epoch. Returns false if we've been cancelled and should exit.
async unsafe fn epoch(
    last_epoch: &mut Instant,
    mutation_buffer_rx: &mut UnboundedReceiver<Mutation>,
    mutation_buffer: &mut Vec<Mutation>,
) -> bool {
    unsafe {
        process_mutation_buffer(mutation_buffer_rx, mutation_buffer).await;
    }
    let duration_since_last_epoch = Instant::now() - *last_epoch;
    if duration_since_last_epoch > Duration::from_millis(100) {
        if tokio::task::spawn_blocking(|| unsafe { process_cycles() })
            .await
            .is_err()
        {
            return false;
        }

        *last_epoch = Instant::now();
    }
    true
}

/// SAFETY: this function is _not reentrant_, may only be called by once per epoch,
/// and must _complete_ before the next epoch.
async unsafe fn process_mutation_buffer(
    mutation_buffer_rx: &mut UnboundedReceiver<Mutation>,
    mutation_buffer: &mut Vec<Mutation>,
) {
    // It is very important that we do not delay any mutations that
    // have occurred at this point by an extra epoch.
    let to_recv = mutation_buffer_rx.len().max(MIN_MUTATIONS_PER_EPOCH);

    mutation_buffer_rx.recv_many(mutation_buffer, to_recv).await;
    for mutation in mutation_buffer.drain(..) {
        unsafe {
            match mutation.kind {
                MutationKind::Inc => increment(mutation.gc),
                MutationKind::Dec => decrement(mutation.gc),
            }
        }
    }
}

// SAFETY: These values can only be accessed by one thread at once.
static mut ROOTS: Vec<OpaqueGcPtr> = Vec::new();
static mut CYCLE_BUFFER: Vec<Vec<OpaqueGcPtr>> = Vec::new();
static mut CURRENT_CYCLE: Vec<OpaqueGcPtr> = Vec::new();

unsafe fn increment(s: OpaqueGcPtr) {
    unsafe {
        s.set_rc(s.rc() + 1);
        scan_black(s);
    }
}

unsafe fn decrement(s: OpaqueGcPtr) {
    unsafe {
        s.set_rc(s.rc() - 1);
        if s.rc() == 0 {
            release(s);
        } else {
            possible_root(s);
        }
    }
}

unsafe fn release(s: OpaqueGcPtr) {
    unsafe {
        for_each_child(s, &mut |c| decrement(c));
        s.set_color(Color::Black);
        if !s.buffered() {
            free(s);
        }
    }
}

unsafe fn possible_root(s: OpaqueGcPtr) {
    unsafe {
        scan_black(s);
        s.set_color(Color::Purple);
        if !s.buffered() {
            s.set_buffered(true);
            (&raw mut ROOTS).as_mut().unwrap().push(s);
        }
    }
}

unsafe fn process_cycles() {
    unsafe {
        free_cycles();
        collect_cycles();
        sigma_preparation();
    }
}

unsafe fn collect_cycles() {
    unsafe {
        mark_roots();
        scan_roots();
        collect_roots();
    }
}

// SAFETY: No function called by mark_roots may access ROOTS
unsafe fn mark_roots() {
    unsafe {
        let mut new_roots = Vec::new();
        for s in (&raw const ROOTS).as_ref().unwrap().iter() {
            if s.color() == Color::Purple && s.rc() > 0 {
                mark_gray(*s);
                new_roots.push(*s);
            } else {
                s.set_buffered(false);
                if s.rc() == 0 {
                    free(*s);
                }
            }
        }
        ROOTS = new_roots;
    }
}

unsafe fn scan_roots() {
    unsafe {
        for s in (&raw const ROOTS).as_ref().unwrap().iter() {
            scan(*s)
        }
    }
}

unsafe fn collect_roots() {
    unsafe {
        for s in std::mem::take((&raw mut ROOTS).as_mut().unwrap()) {
            if s.color() == Color::White {
                collect_white(s);
                let current_cycle = std::mem::take((&raw mut CURRENT_CYCLE).as_mut().unwrap());
                (&raw mut CYCLE_BUFFER)
                    .as_mut()
                    .unwrap()
                    .push(current_cycle);
            } else {
                s.set_buffered(false);
            }
        }
    }
}

enum MarkGrayPhase {
    MarkGray(OpaqueGcPtr),
    SetCrc(OpaqueGcPtr),
}

unsafe fn mark_gray(s: OpaqueGcPtr) {
    unsafe {
        let mut stack = Vec::new();
        if s.color() != Color::Gray {
            s.set_color(Color::Gray);
            s.set_crc(s.rc() as isize);
            for_each_child(s, &mut |t| stack.push(MarkGrayPhase::MarkGray(t)))
        }
        while let Some(s) = stack.pop() {
            match s {
                MarkGrayPhase::MarkGray(s) => {
                    if s.color() != Color::Gray {
                        s.set_color(Color::Gray);
                        s.set_crc(s.rc() as isize);
                        for_each_child(s, &mut |t| stack.push(MarkGrayPhase::MarkGray(t)))
                    }
                    stack.push(MarkGrayPhase::SetCrc(s))
                }
                MarkGrayPhase::SetCrc(s) => {
                    let s_crc = s.crc();
                    if s_crc > 0 {
                        s.set_crc(s_crc - 1);
                    }
                }
            }
        }
    }
}

unsafe fn scan(s: OpaqueGcPtr) {
    unsafe {
        let mut stack = vec![s];
        while let Some(s) = stack.pop() {
            if s.color() == Color::Gray {
                if s.crc() == 0 {
                    s.set_color(Color::White);
                    for_each_child(s, &mut |c| stack.push(c));
                } else {
                    scan_black(s);
                }
            }
        }
    }
}

unsafe fn scan_black(s: OpaqueGcPtr) {
    unsafe {
        let mut stack = vec![s];
        while let Some(s) = stack.pop() {
            if s.color() != Color::Black {
                s.set_color(Color::Black);
                for_each_child(s, &mut |c| stack.push(c));
            }
        }
    }
}

unsafe fn collect_white(s: OpaqueGcPtr) {
    unsafe {
        let mut stack = vec![s];
        while let Some(s) = stack.pop() {
            if s.color() == Color::White {
                s.set_color(Color::Orange);
                s.set_buffered(true);
                (&raw mut CURRENT_CYCLE).as_mut().unwrap().push(s);
                for_each_child(s, &mut |c| stack.push(c));
            }
        }
    }
}

unsafe fn sigma_preparation() {
    unsafe {
        for c in (&raw const CYCLE_BUFFER).as_ref().unwrap() {
            for n in c {
                n.set_color(Color::Red);
                n.set_crc(n.rc() as isize);
            }
            for n in c {
                for_each_child(*n, &mut |m| {
                    if m.color() == Color::Red && m.crc() > 0 {
                        m.set_crc(m.crc() - 1);
                    }
                });
            }
            for n in c {
                n.set_color(Color::Orange);
            }
        }
    }
}

unsafe fn free_cycles() {
    unsafe {
        for c in std::mem::take((&raw mut CYCLE_BUFFER).as_mut().unwrap())
            .into_iter()
            .rev()
        {
            if delta_test(&c) && sigma_test(&c) {
                free_cycle(&c);
            } else {
                refurbish(&c);
            }
        }
    }
}

unsafe fn delta_test(c: &[OpaqueGcPtr]) -> bool {
    unsafe {
        for n in c {
            if n.color() != Color::Orange {
                return false;
            }
        }
        true
    }
}

unsafe fn sigma_test(c: &[OpaqueGcPtr]) -> bool {
    unsafe {
        let mut sum = 0;
        for n in c {
            sum += n.crc();
        }
        sum == 0
        // TODO: I think this is still correct. Make CRC a usize and uncomment
        // out this code.
        /*
        // NOTE: This is the only function so far that I have not implemented
        // _exactly_ as the text reads. I do not understand why I would have to
        // continue iterating if I see a CRC > 0, as CRCs cannot be negative.
        for n in c {
            if *crc(*n) > 0 {
                return false;
            }
        }
        true
        */
    }
}

unsafe fn refurbish(c: &[OpaqueGcPtr]) {
    unsafe {
        for (i, n) in c.iter().enumerate() {
            match (i, n.color()) {
                (0, Color::Orange) | (_, Color::Purple) => {
                    n.set_color(Color::Purple);
                    (&raw mut ROOTS).as_mut().unwrap().push(*n);
                }
                _ => {
                    n.set_color(Color::Black);
                    n.set_buffered(false);
                }
            }
        }
    }
}

unsafe fn free_cycle(c: &[OpaqueGcPtr]) {
    unsafe {
        for n in c {
            n.set_color(Color::Red);
        }
        for n in c {
            for_each_child(*n, &mut |c| cyclic_decrement(c));
        }
        for n in c {
            free(*n);
        }
    }
}

unsafe fn cyclic_decrement(m: OpaqueGcPtr) {
    unsafe {
        if m.color() != Color::Red {
            if m.color() == Color::Orange {
                m.set_rc(m.rc() - 1);
                m.set_crc(m.crc() - 1);
            } else {
                decrement(m);
            }
        }
    }
}

unsafe fn for_each_child(s: OpaqueGcPtr, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
    unsafe {
        let lock = s.lock().read().unwrap();
        (s.visit_children())(s.data(), visitor);
        drop(lock);
    }
}

unsafe fn free(s: OpaqueGcPtr) {
    unsafe {
        // Safety: No need to acquire a permit, s is guaranteed to be garbage.

        // Finalize the object:
        (s.finalize())(s.data_mut());

        // Deallocate the object:
        std::alloc::dealloc(s.header.as_ptr() as *mut u8, s.layout());
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::gc::*;
    use std::sync::Arc;

    #[tokio::test]
    async fn cycles() {
        #[derive(Default, Trace)]
        struct Cyclic {
            next: Option<Gc<Cyclic>>,
            out: Option<Arc<()>>,
        }

        let out_ptr = Arc::new(());

        let a = Gc::new(Cyclic::default());
        let b = Gc::new(Cyclic::default());
        let c = Gc::new(Cyclic::default());

        // a -> b -> c -
        // ^----------/
        a.write().next = Some(b.clone());
        b.write().next = Some(c.clone());
        b.write().out = Some(out_ptr.clone());
        c.write().next = Some(a.clone());

        assert_eq!(Arc::strong_count(&out_ptr), 2);

        drop(a);
        drop(b);
        drop(c);
        let mut mutation_buffer_rx = MUTATION_BUFFER
            .get_or_init(MutationBuffer::default)
            .mutation_buffer_rx
            .lock()
            .unwrap()
            .take()
            .unwrap();
        let mut mutation_buffer = Vec::new();
        unsafe {
            process_mutation_buffer(&mut mutation_buffer_rx, &mut mutation_buffer).await;
            process_cycles();
            process_cycles();
        }

        assert_eq!(Arc::strong_count(&out_ptr), 1);
    }
}
