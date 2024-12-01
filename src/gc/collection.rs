//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use std::{
    alloc::Layout,
    ptr::{addr_of_mut, NonNull},
    sync::OnceLock,
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
    mutation_buffer_rx: UnboundedReceiver<Mutation>,
}

impl Default for MutationBuffer {
    fn default() -> Self {
        let (mutation_buffer_tx, mutation_buffer_rx) = unbounded_channel();
        Self {
            mutation_buffer_tx,
            mutation_buffer_rx,
        }
    }
}

static mut MUTATION_BUFFER: OnceLock<MutationBuffer> = OnceLock::new();

pub(super) fn inc_rc<T: Trace>(gc: NonNull<GcInner<T>>) {
    // SAFETY: send takes an immutable reference and is atomic
    unsafe {
        MUTATION_BUFFER
            .get()
            .unwrap()
            .mutation_buffer_tx
            .send(Mutation::new(MutationKind::Inc, gc as NonNull<OpaqueGc>))
            .unwrap();
    }
}

pub(super) fn dec_rc<T: Trace>(gc: NonNull<GcInner<T>>) {
    // SAFETY: send takes an immutable reference and is atomic
    unsafe {
        MUTATION_BUFFER
            .get()
            .unwrap()
            .mutation_buffer_tx
            .send(Mutation::new(MutationKind::Dec, gc as NonNull<OpaqueGc>))
            .unwrap();
    }
}

static COLLECTOR_TASK: OnceLock<JoinHandle<()>> = OnceLock::new();

pub fn init_gc() {
    // SAFETY: We DO NOT mutate MUTATION_BUFFER, we mutate the _interior once lock_.
    let _ = unsafe { MUTATION_BUFFER.get_or_init(MutationBuffer::default) };
    let _ = COLLECTOR_TASK.get_or_init(|| {
        tokio::task::spawn(async {
            loop {
                epoch().await
            }
        })
    });
}

async fn epoch() {
    process_mutation_buffer().await;
    tokio::task::spawn_blocking(process_cycles).await.unwrap();
}

/// SAFETY: this function is _not reentrant_, may only be called by once per epoch,
/// and must _complete_ before the next epoch.
async fn process_mutation_buffer() {
    const MUTATIONS_PER_EPOCH: usize = 10_000; // No idea what a good value is here.

    let mut mutation_buffer: Vec<_> = Vec::with_capacity(MUTATIONS_PER_EPOCH);
    // SAFETY: This function has _exclusive access_ to the receive buffer.
    unsafe {
        let buffer = &mut MUTATION_BUFFER.get_mut().unwrap().mutation_buffer_rx;
        if buffer.len() < MUTATIONS_PER_EPOCH {
            tokio::time::sleep(tokio::time::Duration::from_secs(10)).await;
            return;
        }
        buffer
            .recv_many(&mut mutation_buffer, MUTATIONS_PER_EPOCH)
            .await;
    }

    // SAFETY: This function has _exclusive access_ to mutate the header of
    // _every_ garbage collected object. It does so _only now_.
    for mutation in mutation_buffer.into_iter() {
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

fn increment(s: OpaqueGcPtr) {
    *rc(s) += 1;
    scan_black(s);
}

fn decrement(s: OpaqueGcPtr) {
    // TODO: Possible optimization: check if the semaphore's value is
    // greater than 0.
    *rc(s) -= 1;
    if *rc(s) == 0 {
        release(s);
    } else {
        possible_root(s);
    }
}

fn release(s: OpaqueGcPtr) {
    for_each_child(s, decrement);
    *color(s) = Color::Black;
    if !*buffered(s) {
        free(s);
    }
}

fn possible_root(s: OpaqueGcPtr) {
    scan_black(s);
    *color(s) = Color::Purple;
    if !*buffered(s) {
        *buffered(s) = true;
        unsafe { ROOTS.push(s) };
    }
}

fn process_cycles() {
    free_cycles();
    collect_cycles();
    sigma_preparation();
}

fn collect_cycles() {
    mark_roots();
    scan_roots();
    collect_roots();
}

// SAFETY: No function called by mark_roots may access ROOTS
fn mark_roots() {
    let mut new_roots = Vec::new();
    for s in unsafe { ROOTS.iter() } {
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
    unsafe { ROOTS = new_roots }
}

fn scan_roots() {
    for s in unsafe { ROOTS.iter() } {
        scan(*s)
    }
}

fn collect_roots() {
    for s in unsafe { std::mem::take(&mut *addr_of_mut!(ROOTS)) } {
        if *color(s) == Color::White {
            collect_white(s);
            unsafe {
                CYCLE_BUFFER.push(std::mem::take(&mut *addr_of_mut!(CURRENT_CYCLE)));
            }
        } else {
            *buffered(s) = false;
        }
    }
}

fn mark_gray(s: OpaqueGcPtr) {
    if *color(s) != Color::Gray {
        *color(s) = Color::Gray;
        *crc(s) = *rc(s) as isize;
        for_each_child(s, mark_gray);
    } else if *crc(s) > 0 {
        *crc(s) -= 1;
    }
}

fn scan(s: OpaqueGcPtr) {
    if *color(s) == Color::Gray && *crc(s) == 0 {
        *color(s) = Color::White;
        for_each_child(s, scan);
    } else {
        scan_black(s);
    }
}

fn scan_black(s: OpaqueGcPtr) {
    if *color(s) != Color::Black {
        *color(s) = Color::Black;
        for_each_child(s, scan_black);
    }
}

fn collect_white(s: OpaqueGcPtr) {
    if *color(s) == Color::White {
        *color(s) = Color::Orange;
        *buffered(s) = true;
        unsafe {
            CURRENT_CYCLE.push(s);
        }
        for_each_child(s, collect_white);
    }
}

fn sigma_preparation() {
    for c in unsafe { CYCLE_BUFFER.iter() } {
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

fn free_cycles() {
    for c in unsafe {
        std::mem::take(&mut *addr_of_mut!(CYCLE_BUFFER))
            .into_iter()
            .rev()
    } {
        if delta_test(&c) && sigma_test(&c) {
            free_cycle(&c);
        } else {
            refurbish(&c);
        }
    }
}

fn delta_test(c: &[OpaqueGcPtr]) -> bool {
    for n in c {
        if *color(*n) != Color::Orange {
            return false;
        }
    }
    true
}

fn sigma_test(c: &[OpaqueGcPtr]) -> bool {
    let mut sum = 0;
    for n in c {
        sum += *crc(*n);
    }
    sum == 0
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

fn refurbish(c: &[OpaqueGcPtr]) {
    for (i, n) in c.iter().enumerate() {
        match (i, *color(*n)) {
            (0, Color::Orange) | (_, Color::Purple) => {
                *color(*n) = Color::Purple;
                unsafe {
                    ROOTS.push(*n);
                }
            }
            _ => {
                *color(*n) = Color::Black;
                *buffered(*n) = false;
            }
        }
    }
}

fn free_cycle(c: &[OpaqueGcPtr]) {
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

fn cyclic_decrement(m: OpaqueGcPtr) {
    if *color(m) != Color::Red {
        if *color(m) == Color::Orange {
            *rc(m) -= 1;
            *crc(m) -= 1;
        } else {
            decrement(m);
        }
    }
}

fn color<'a>(s: OpaqueGcPtr) -> &'a mut Color {
    unsafe { &mut (*s.as_ref().header.get()).color }
}

fn rc<'a>(s: OpaqueGcPtr) -> &'a mut usize {
    unsafe { &mut (*s.as_ref().header.get()).rc }
}

fn crc<'a>(s: OpaqueGcPtr) -> &'a mut isize {
    unsafe { &mut (*s.as_ref().header.get()).crc }
}

fn buffered<'a>(s: OpaqueGcPtr) -> &'a mut bool {
    unsafe { &mut (*s.as_ref().header.get()).buffered }
}

fn semaphore<'a>(s: OpaqueGcPtr) -> &'a Semaphore {
    unsafe { &(*s.as_ref().header.get()).semaphore }
}

fn acquire_permit(semaphore: &'_ Semaphore) -> SemaphorePermit<'_> {
    loop {
        if let Ok(permit) = semaphore.try_acquire() {
            return permit;
        }
    }
}

fn trace<'a>(s: OpaqueGcPtr) -> &'a mut dyn Trace {
    unsafe { &mut *s.as_ref().data.get() }
}

fn for_each_child(s: OpaqueGcPtr, visitor: fn(OpaqueGcPtr)) {
    let permit = acquire_permit(semaphore(s));
    unsafe { (*s.as_ref().data.get()).visit_children(visitor) }
    drop(permit);
}

fn free(s: OpaqueGcPtr) {
    // Safety: No need to acquire a permit, s is guaranteed to be garbage.
    let trace = trace(s);
    unsafe {
        trace.finalize();
        let layout = Layout::for_value(trace);
        std::alloc::dealloc(trace.as_ptr(), layout);
    }
}
