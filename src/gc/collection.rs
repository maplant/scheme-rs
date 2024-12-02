//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use std::{
    alloc::Layout,
    ptr::NonNull,
    sync::OnceLock,
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
        (&raw const MUTATION_BUFFER)
            .as_ref()
            .unwrap()
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
        (&raw const MUTATION_BUFFER)
            .as_ref()
            .unwrap()
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
    let _ = unsafe {
        (&raw const MUTATION_BUFFER)
            .as_ref()
            .unwrap()
            .get_or_init(MutationBuffer::default)
    };
    let _ = COLLECTOR_TASK.get_or_init(|| {
        tokio::task::spawn(async {
            let mut last_epoch = Instant::now();
            loop {
                epoch(&mut last_epoch).await
            }
        })
    });
}

#[cfg(test)]
pub fn init_gc_test() {
    let _ = unsafe {
        (&raw const MUTATION_BUFFER)
            .as_ref()
            .unwrap()
            .get_or_init(MutationBuffer::default)
    };
}

async fn epoch(last_epoch: &mut Instant) {
    process_mutation_buffer().await;
    let duration_since_last_epoch = Instant::now() - *last_epoch;
    if duration_since_last_epoch > Duration::from_millis(100) {
        tokio::task::spawn_blocking(process_cycles).await.unwrap();
        *last_epoch = Instant::now();
    }
}

/// SAFETY: this function is _not reentrant_, may only be called by once per epoch,
/// and must _complete_ before the next epoch.
pub async fn process_mutation_buffer() {
    const MAX_MUTATIONS_PER_EPOCH: usize = 10_000; // No idea what a good value is here.

    let mut mutation_buffer: Vec<_> = Vec::with_capacity(MAX_MUTATIONS_PER_EPOCH);
    // SAFETY: This function has _exclusive access_ to the receive buffer.
    unsafe {
        (&raw mut MUTATION_BUFFER)
            .as_mut()
            .unwrap()
            .get_mut()
            .unwrap()
            .mutation_buffer_rx
            .recv_many(&mut mutation_buffer, MAX_MUTATIONS_PER_EPOCH)
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
        unsafe { (&raw mut ROOTS).as_mut().unwrap().push(s) };
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
    for s in unsafe { (&raw const ROOTS).as_ref().unwrap().iter() } {
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
    for s in unsafe { (&raw const ROOTS).as_ref().unwrap().iter() } {
        scan(*s)
    }
}

fn collect_roots() {
    for s in unsafe { std::mem::take((&raw mut ROOTS).as_mut().unwrap()) } {
        if *color(s) == Color::White {
            collect_white(s);
            unsafe {
                let current_cycle = std::mem::take((&raw mut CURRENT_CYCLE).as_mut().unwrap());
                (&raw mut CYCLE_BUFFER)
                    .as_mut()
                    .unwrap()
                    .push(current_cycle);
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
        for_each_child(s, |t| {
            mark_gray(t);
            if *crc(t) > 0 {
                *crc(t) -= 1;
            }
        });
    }
}

fn scan(s: OpaqueGcPtr) {
    if *color(s) == Color::Gray {
        if *crc(s) == 0 {
            *color(s) = Color::White;
            for_each_child(s, scan);
        } else {
            scan_black(s);
        }
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
            (&raw mut CURRENT_CYCLE).as_mut().unwrap().push(s);
        }
        for_each_child(s, collect_white);
    }
}

fn sigma_preparation() {
    for c in unsafe { (&raw const CYCLE_BUFFER).as_ref().unwrap() } {
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
        std::mem::take((&raw mut CYCLE_BUFFER).as_mut().unwrap())
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
        let layout = Layout::for_value(trace);
        trace.finalize();
        std::alloc::dealloc(s.as_ptr() as *mut u8, layout);
    }
}

#[cfg(test)]
mod test {
    use collection::{init_gc_test, process_cycles};

    use crate::gc::*;

    #[tokio::test]
    async fn cycles() {
        #[derive(Default, Trace)]
        struct Cyclic {
            next: Option<Gc<Cyclic>>,
            out: Option<Arc<()>>,
        }

        let out_ptr = Arc::new(());

        init_gc_test();

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
        process_mutation_buffer().await;
        process_cycles();
        process_cycles();
        assert_eq!(Arc::strong_count(&out_ptr), 1);
    }
}
