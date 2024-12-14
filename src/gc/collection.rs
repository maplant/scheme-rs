//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use std::{
    alloc::Layout,
    ptr::NonNull,
    sync::{Mutex, OnceLock},
    time::{Duration, Instant},
};
use tokio::{
    sync::{
        mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender},
        Semaphore, SemaphorePermit,
    },
    task::JoinHandle,
};

use super::{Color, GcInner, OpaqueGc, OpaqueGcPtr, Trace};

#[derive(Copy, Clone, Debug)]
pub struct Mutation {
    kind: MutationKind,
    gc: NonNull<OpaqueGc>,
}

impl Mutation {
    fn new(kind: MutationKind, gc: NonNull<OpaqueGc>) -> Self {
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

pub(super) fn inc_rc<T: Trace>(gc: NonNull<GcInner<T>>) {
    // SAFETY: send takes an immutable reference and is atomic
    MUTATION_BUFFER
        .get_or_init(MutationBuffer::default)
        .mutation_buffer_tx
        .send(Mutation::new(MutationKind::Inc, gc as NonNull<OpaqueGc>))
        .unwrap();
}

pub(super) fn dec_rc<T: Trace>(gc: NonNull<GcInner<T>>) {
    // SAFETY: send takes an immutable reference and is atomic
    MUTATION_BUFFER
        .get_or_init(MutationBuffer::default)
        .mutation_buffer_tx
        .send(Mutation::new(MutationKind::Dec, gc as NonNull<OpaqueGc>))
        .unwrap();
}

static COLLECTOR_TASK: OnceLock<JoinHandle<()>> = OnceLock::new();

pub fn init_gc() {
    // SAFETY: We DO NOT mutate MUTATION_BUFFER, we mutate the _interior once lock_.
    let _ = MUTATION_BUFFER.get_or_init(MutationBuffer::default);
    let _ = COLLECTOR_TASK
        .get_or_init(|| tokio::task::spawn(async { unsafe { run_garbage_collector().await } }));
}

const MAX_MUTATIONS_PER_EPOCH: usize = 10_000; // No idea what a good value is here.

async unsafe fn run_garbage_collector() {
    let mut last_epoch = Instant::now();
    let mut mutation_buffer: Vec<_> = Vec::with_capacity(MAX_MUTATIONS_PER_EPOCH);
    let mut mutation_buffer_rx = MUTATION_BUFFER
        .get_or_init(MutationBuffer::default)
        .mutation_buffer_rx
        .lock()
        .unwrap()
        .take()
        .unwrap();
    loop {
        if !epoch(
            &mut last_epoch,
            &mut mutation_buffer_rx,
            &mut mutation_buffer,
        )
        .await
        {
            return;
        }
        mutation_buffer.clear();
    }
}

// Run a collection epoch. Returns false if we've been cancelled and should exit.
async unsafe fn epoch(
    last_epoch: &mut Instant,
    mutation_buffer_rx: &mut UnboundedReceiver<Mutation>,
    mutation_buffer: &mut Vec<Mutation>,
) -> bool {
    process_mutation_buffer(mutation_buffer_rx, mutation_buffer).await;
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
    let to_recv = mutation_buffer_rx.len();
    mutation_buffer_rx.recv_many(mutation_buffer, to_recv).await;

    for mutation in mutation_buffer {
        match mutation.kind {
            MutationKind::Inc => increment(mutation.gc),
            MutationKind::Dec => decrement(mutation.gc),
        }
    }
}

// SAFETY: These values can only be accessed by one thread at once.
static mut ROOTS: Vec<OpaqueGcPtr> = Vec::new();
static mut CYCLE_BUFFER: Vec<Vec<OpaqueGcPtr>> = Vec::new();
static mut CURRENT_CYCLE: Vec<OpaqueGcPtr> = Vec::new();

unsafe fn increment(s: OpaqueGcPtr) {
    *rc(s) += 1;
    scan_black(s);
}

unsafe fn decrement(s: OpaqueGcPtr) {
    *rc(s) -= 1;
    if *rc(s) == 0 {
        release(s);
    } else {
        possible_root(s);
    }
}

unsafe fn release(s: OpaqueGcPtr) {
    for_each_child(s, decrement);
    *color(s) = Color::Black;
    if !*buffered(s) {
        free(s);
    }
}

unsafe fn possible_root(s: OpaqueGcPtr) {
    scan_black(s);
    *color(s) = Color::Purple;
    if !*buffered(s) {
        *buffered(s) = true;
        (&raw mut ROOTS).as_mut().unwrap().push(s);
    }
}

unsafe fn process_cycles() {
    free_cycles();
    collect_cycles();
    sigma_preparation();
}

unsafe fn collect_cycles() {
    mark_roots();
    scan_roots();
    collect_roots();
}

// SAFETY: No function called by mark_roots may access ROOTS
unsafe fn mark_roots() {
    let mut new_roots = Vec::new();
    for s in (&raw const ROOTS).as_ref().unwrap().iter() {
        if *color(*s) == Color::Purple && *rc(*s) > 0 {
            mark_gray(*s);
            new_roots.push(*s);
        } else {
            *buffered(*s) = false;
            if *rc(*s) == 0 {
                free(*s);
            }
        }
    }
    ROOTS = new_roots;
}

unsafe fn scan_roots() {
    for s in (&raw const ROOTS).as_ref().unwrap().iter() {
        scan(*s)
    }
}

unsafe fn collect_roots() {
    for s in std::mem::take((&raw mut ROOTS).as_mut().unwrap()) {
        if *color(s) == Color::White {
            collect_white(s);
            let current_cycle = std::mem::take((&raw mut CURRENT_CYCLE).as_mut().unwrap());
            (&raw mut CYCLE_BUFFER)
                .as_mut()
                .unwrap()
                .push(current_cycle);
        } else {
            *buffered(s) = false;
        }
    }
}

unsafe fn mark_gray(s: OpaqueGcPtr) {
    if *color(s) != Color::Gray {
        *color(s) = Color::Gray;
        *crc(s) = *rc(s) as isize;
        for_each_child(s, |t| {
            mark_gray(t);
            if *crc(t) > 0 {
                *crc(t) -= 1;
            }
        });
    }
}

unsafe fn scan(s: OpaqueGcPtr) {
    if *color(s) == Color::Gray {
        if *crc(s) == 0 {
            *color(s) = Color::White;
            for_each_child(s, scan);
        } else {
            scan_black(s);
        }
    }
}

unsafe fn scan_black(s: OpaqueGcPtr) {
    if *color(s) != Color::Black {
        *color(s) = Color::Black;
        for_each_child(s, scan_black);
    }
}

unsafe fn collect_white(s: OpaqueGcPtr) {
    if *color(s) == Color::White {
        *color(s) = Color::Orange;
        *buffered(s) = true;
        (&raw mut CURRENT_CYCLE).as_mut().unwrap().push(s);
        for_each_child(s, collect_white);
    }
}

unsafe fn sigma_preparation() {
    for c in (&raw const CYCLE_BUFFER).as_ref().unwrap() {
        for n in c {
            *color(*n) = Color::Red;
            *crc(*n) = *rc(*n) as isize;
        }
        for n in c {
            for_each_child(*n, |m| {
                if *color(m) == Color::Red && *crc(m) > 0 {
                    *crc(m) -= 1;
                }
            });
        }
        for n in c {
            *color(*n) = Color::Orange;
        }
    }
}

unsafe fn free_cycles() {
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

unsafe fn delta_test(c: &[OpaqueGcPtr]) -> bool {
    for n in c {
        if *color(*n) != Color::Orange {
            return false;
        }
    }
    true
}

unsafe fn sigma_test(c: &[OpaqueGcPtr]) -> bool {
    let mut sum = 0;
    for n in c {
        sum += *crc(*n);
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

unsafe fn refurbish(c: &[OpaqueGcPtr]) {
    for (i, n) in c.iter().enumerate() {
        match (i, *color(*n)) {
            (0, Color::Orange) | (_, Color::Purple) => {
                *color(*n) = Color::Purple;
                unsafe {
                    (&raw mut ROOTS).as_mut().unwrap().push(*n);
                }
            }
            _ => {
                *color(*n) = Color::Black;
                *buffered(*n) = false;
            }
        }
    }
}

unsafe fn free_cycle(c: &[OpaqueGcPtr]) {
    for n in c {
        *color(*n) = Color::Red;
    }
    for n in c {
        for_each_child(*n, cyclic_decrement);
    }
    for n in c {
        free(*n);
    }
}

unsafe fn cyclic_decrement(m: OpaqueGcPtr) {
    if *color(m) != Color::Red {
        if *color(m) == Color::Orange {
            *rc(m) -= 1;
            *crc(m) -= 1;
        } else {
            decrement(m);
        }
    }
}

unsafe fn color<'a>(s: OpaqueGcPtr) -> &'a mut Color {
    &mut (*s.as_ref().header.get()).color
}

unsafe fn rc<'a>(s: OpaqueGcPtr) -> &'a mut usize {
    &mut (*s.as_ref().header.get()).rc
}

unsafe fn crc<'a>(s: OpaqueGcPtr) -> &'a mut isize {
    &mut (*s.as_ref().header.get()).crc
}

unsafe fn buffered<'a>(s: OpaqueGcPtr) -> &'a mut bool {
    &mut (*s.as_ref().header.get()).buffered
}

unsafe fn semaphore<'a>(s: OpaqueGcPtr) -> &'a Semaphore {
    &(*s.as_ref().header.get()).semaphore
}

fn acquire_permit(semaphore: &'_ Semaphore) -> SemaphorePermit<'_> {
    loop {
        if let Ok(permit) = semaphore.try_acquire() {
            return permit;
        }
    }
}

unsafe fn trace<'a>(s: OpaqueGcPtr) -> &'a mut dyn Trace {
    &mut *s.as_ref().data.get()
}

unsafe fn for_each_child(s: OpaqueGcPtr, visitor: unsafe fn(OpaqueGcPtr)) {
    let permit = acquire_permit(semaphore(s));
    (*s.as_ref().data.get()).visit_children(visitor);
    drop(permit);
}

unsafe fn free(s: OpaqueGcPtr) {
    // Safety: No need to acquire a permit, s is guaranteed to be garbage.
    let trace = trace(s);
    let layout = Layout::for_value(trace);
    trace.finalize();
    std::alloc::dealloc(s.as_ptr() as *mut u8, layout);
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
        a.write().await.next = Some(b.clone());
        b.write().await.next = Some(c.clone());
        b.write().await.out = Some(out_ptr.clone());
        c.write().await.next = Some(a.clone());

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
