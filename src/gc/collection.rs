//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

// use kanal::{AsyncReceiver, AsyncSender, unbounded_async};
use std::{
    alloc::Layout, cell::UnsafeCell, collections::HashSet, marker::PhantomData, ptr::NonNull, sync::{
        atomic::{AtomicU64, AtomicUsize}, LazyLock, Mutex, OnceLock, RwLock
    }, time::{Duration, Instant}
};
use tokio::{sync::mpsc::{UnboundedReceiver, UnboundedSender, unbounded_channel}, task::JoinHandle};

use crate::gc::{
    GcInner,
    pool::{Pool, PoolEntry},
};

//use super::{GcInner, OpaqueGcPtr};

#[derive(Debug)]
pub(crate) struct GcHeader {
    /// Reference count shared with the Gc types
    pub(crate) shared_rc: AtomicUsize,
    /// Lock for the data
    pub(crate) lock: RwLock<()>,
    /// Reference count as of the current epoch
    epoch_rc: usize,
    /// Circular reference count
    crc: isize,
    /// Color of the object
    color: Color,
    /// Whether or not the object has been buffered for deletion
    buffered: bool,
    /*
    /// Pool index
    pool_index: Option<usize>,
    */
    /// Type-erased visitor function
    visit_children: unsafe fn(this: *const (), visitor: &mut dyn FnMut(HeapObject<()>)),
    /// Type-erased finalizer function
    finalize: unsafe fn(this: *mut ()),
    /// Layout for the underlying data
    layout: Layout,
}

impl GcHeader {
    pub(crate) fn new<T: super::GcOrTrace>() -> Self {
        Self {
            shared_rc: AtomicUsize::new(1),
            lock: RwLock::new(()),
            epoch_rc: 1,
            crc: 1,
            color: Color::Black,
            buffered: true,
            // pool_index: None,
            visit_children: |this, visitor| unsafe {
                let this = this as *const T;
                T::visit_or_recurse(this.as_ref().unwrap(), visitor);
            },
            finalize: |this| unsafe {
                let this = this as *mut T;
                T::finalize_or_skip(this.as_mut().unwrap());
            },
            layout: Layout::new::<super::GcInner<T>>(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Color {
    /// In use or free
    Black,
    /// Possible member of a cycle
    Gray,
    /// Member of a garbage cycle
    White,
    /// Possible root of cycle
    Purple,
    /// Candidate cycle undergoing Σ-computation
    Red,
    /// Candidate cycle awaiting epoch boundary
    Orange,
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct HeapObject<T> {
    /// Object header
    pub(super) header: NonNull<UnsafeCell<GcHeader>>,
    /// Allocated data
    pub(super) data: NonNull<UnsafeCell<T>>,
}

impl std::fmt::Debug for OpaqueGcPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { (*self.header.as_ref().get()).fmt(f) }
    }
}

pub type OpaqueGcPtr = HeapObject<()>;

impl HeapObject<()> {
    unsafe fn shared_rc(&self) -> usize {
        unsafe {
            (*self.header.as_ref().get())
                .shared_rc
                .load(std::sync::atomic::Ordering::Acquire)
        }
    }

    unsafe fn dec_shared_rc(&self) -> usize {
        unsafe {
            (*self.header.as_ref().get())
                .shared_rc
                .fetch_sub(1, std::sync::atomic::Ordering::Release)
        }
    }

    unsafe fn dec_epoch_rc(&self) {
        unsafe { (*self.header.as_ref().get()).epoch_rc -= 1 }
    }

    unsafe fn epoch_rc(&self) -> usize {
        unsafe { (*self.header.as_ref().get()).epoch_rc }
    }

    unsafe fn set_epoch_rc(&self, rc: usize) {
        unsafe { (*self.header.as_ref().get()).epoch_rc = rc }
    }

    unsafe fn crc(&self) -> isize {
        unsafe { (*self.header.as_ref().get()).crc }
    }

    unsafe fn set_crc(&self, crc: isize) {
        unsafe {
            (*self.header.as_ref().get()).crc = crc;
        }
    }

    unsafe fn color(&self) -> Color {
        unsafe { (*self.header.as_ref().get()).color }
    }

    unsafe fn set_color(&self, color: Color) {
        unsafe {
            (*self.header.as_ref().get()).color = color;
        }
    }

    unsafe fn buffered(&self) -> bool {
        unsafe { (*self.header.as_ref().get()).buffered }
    }

    unsafe fn set_buffered(&self, buffered: bool) {
        unsafe {
            (*self.header.as_ref().get()).buffered = buffered;
        }
    }

    unsafe fn pool_index(&self) -> Option<usize> {
        unsafe { (*self.header.as_ref().get()).pool_index }
    }

    unsafe fn set_pool_index(&self, index: usize) {
        unsafe { (*self.header.as_ref().get()).pool_index = Some(index); }
    }

    unsafe fn lock(&self) -> &RwLock<()> {
        unsafe { &(*self.header.as_ref().get()).lock }
    }

    unsafe fn visit_children(
        &self,
    ) -> unsafe fn(this: *const (), visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        unsafe { (*self.header.as_ref().get()).visit_children }
    }

    unsafe fn finalize(&self) -> unsafe fn(this: *mut ()) {
        unsafe { (*self.header.as_ref().get()).finalize }
    }

    unsafe fn layout(&self) -> Layout {
        unsafe { (*self.header.as_ref().get()).layout }
    }

    unsafe fn data(&self) -> *const () {
        unsafe { self.data.as_ref().get() as *const () }
    }

    unsafe fn data_mut(&self) -> *mut () {
        unsafe { self.data.as_ref().get() }
    }
}

unsafe impl Send for HeapObject<()> {}
unsafe impl Sync for HeapObject<()> {}

/// All of the heap objects that have been allocated as of the given epoch.
/// SAFETY: This variable can only be accessed by the collector thread.
static mut HEAP: HashSet<HeapObject<()>> = HashSet::new();

/*
/// New heap objects that have not been added to HEAP yet.
static NEW_HEAP_OBJECTS: LazyLock<Mutex<Vec<HeapObject<()>>>> =
LazyLock::new(|| Mutex::new(Vec::new()));
*/

pub(super) fn alloc_gc_object<T: super::GcOrTrace>(data: T) -> super::Gc<T> {
    let new_gc = super::Gc {
        ptr: NonNull::from(Box::leak(Box::new(GcInner {
            header: UnsafeCell::new(GcHeader::new::<T>()),
            data: UnsafeCell::new(data),
        }))),
        marker: PhantomData,
    };

    let _ = NEW_ALLOCS_BUFFER
        .get_or_init(NewAllocsBuffer::default)
        .new_allocs_buffer_tx
        .send(unsafe { new_gc.as_opaque() });
    //        .unwrap_or(true)
    // {}
    /*
    unsafe {
        
        NEW_HEAP_OBJECTS.lock().unwrap().push(new_gc.as_opaque());
    }
    */
    new_gc
}

struct NewAllocsBuffer {
    new_allocs_buffer_tx: UnboundedSender<HeapObject<()>>,
    new_allocs_buffer_rx: Mutex<Option<UnboundedReceiver<HeapObject<()>>>>,
}

static PENDING_MUTATIONS: AtomicUsize = AtomicUsize::new(0);

unsafe impl Sync for NewAllocsBuffer {}

impl Default for NewAllocsBuffer {
    fn default() -> Self {
        let (new_allocs_buffer_tx, new_allocs_buffer_rx) = unbounded_channel();
        Self {
            new_allocs_buffer_tx,
            new_allocs_buffer_rx: Mutex::new(Some(new_allocs_buffer_rx)),
        }
    }
}

static NEW_ALLOCS_BUFFER: OnceLock<NewAllocsBuffer> = OnceLock::new();


/*
/// Instead of mutations being atomic (via an atomic variable), they're buffered into
/// "epochs", and handled by precisely one thread.
struct MutationBuffer {
    mutation_buffer_tx: AsyncSender<Mutation>,
    mutation_buffer_rx: Mutex<Option<AsyncReceiver<Mutation>>>,
}

static PENDING_MUTATIONS: AtomicUsize = AtomicUsize::new(0);

unsafe impl Sync for MutationBuffer {}

impl Default for MutationBuffer {
    fn default() -> Self {
        let (mutation_buffer_tx, mutation_buffer_rx) = unbounded_async();
        Self {
            mutation_buffer_tx,
            mutation_buffer_rx: Mutex::new(Some(mutation_buffer_rx)),
        }
    }
}

static MUTATION_BUFFER: OnceLock<MutationBuffer> = OnceLock::new();

static MAX_PENDING_MUTATIONS_ALLOWED: usize = 100_000;

pub(crate) async fn yield_until_gc_cleared() {
    while PENDING_MUTATIONS.load(std::sync::atomic::Ordering::Relaxed)
        > MAX_PENDING_MUTATIONS_ALLOWED
    {
        tokio::task::yield_now().await
    }
}

pub(super) fn inc_rc<T: ?Sized>(gc: NonNull<GcInner<T>>) {
    /*
    // Disregard any send errors. If the receiver was dropped then the process
    // is exiting and we don't care if we leak.
    while !MUTATION_BUFFER
        .get_or_init(MutationBuffer::default)
        .mutation_buffer_tx
        .try_send(Mutation::new(MutationKind::Inc, OpaqueGcPtr::from(gc)))
        .unwrap_or(true)
    {}
    */
}

pub(super) fn dec_rc<T: ?Sized>(gc: NonNull<GcInner<T>>) {
    /*
    // Disregard any send errors. If the receiver was dropped then the process
    // is exiting and we don't care if we leak.

    while !MUTATION_BUFFER
        .get_or_init(MutationBuffer::default)
        .mutation_buffer_tx
        .try_send(Mutation::new(MutationKind::Dec, OpaqueGcPtr::from(gc)))
        .unwrap_or(true)
    {}
    */
}
*/

static COLLECTOR_TASK: OnceLock<JoinHandle<()>> = OnceLock::new();

pub fn init_gc() {
    // SAFETY: We DO NOT mutate NEW_ALLOCS_BUFFER, we mutate the _interior once lock_.
    let _ = NEW_ALLOCS_BUFFER.get_or_init(NewAllocsBuffer::default);
    let _ = COLLECTOR_TASK
        .get_or_init(|| tokio::task::spawn(async { unsafe { run_garbage_collector().await } }));
}

async unsafe fn run_garbage_collector() {
    let mut new_allocs_buffer_rx = NEW_ALLOCS_BUFFER
        .get_or_init(NewAllocsBuffer::default)
        .new_allocs_buffer_rx
        .lock()
        .unwrap()
        .take()
        .unwrap();
    loop {
        tokio::task::yield_now().await;

        // Proces new heap objects
        let to_recv = new_allocs_buffer_rx.len();

        if to_recv == 0 {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }

        let mut recvd = Vec::new();
        new_allocs_buffer_rx.recv_many(&mut recvd, to_recv).await;
        for new_alloc in recvd.into_iter() {
            let heap = unsafe { (&raw mut HEAP).as_mut().unwrap() };
            let pool_idx = heap.next_index();
            unsafe {
                new_alloc.set_pool_index(pool_idx);
                new_alloc.set_buffered(false);
            }
            heap.push(new_alloc);
        }

         tokio::task::block_in_place(|| unsafe { epoch() }); // {
        //    break;
        //}
    }
    /*
    unsafe {
        let mut last_epoch = Instant::now();
        let mut mutation_buffer_rx = MUTATION_BUFFER
            .get_or_init(MutationBuffer::default)
            .mutation_buffer_rx
            .lock()
            .unwrap()
            .take()
            .unwrap();
        while epoch(&mut last_epoch, &mut mutation_buffer_rx).await {}
    }
    */
}

// SAFETY: These values can only be accessed by one thread at once.
static mut ROOTS: HashSet<OpaqueGcPtr> = Vec::new();
static mut CYCLE_BUFFER: Vec<Vec<OpaqueGcPtr>> = Vec::new();
static mut CURRENT_CYCLE: Vec<OpaqueGcPtr> = Vec::new();

/// SAFETY: this function is _not reentrant_, may only be called by once per epoch,
/// and must _complete_ before the next epoch.
unsafe fn epoch() {

    let heap = unsafe { (&raw mut HEAP).as_mut().unwrap() };

    /*
    // Collect the new heap objects:
    let new_heap_objects = { std::mem::take(&mut *NEW_HEAP_OBJECTS.lock().unwrap()) };

    for new_heap_object in new_heap_objects {
        let pool_idx = heap.next_index();
        unsafe { new_heap_object.set_pool_index(pool_idx); }
        heap.push(new_heap_object);
    }
     */
    // println!("heap size: {}", heap.len());

    for heap_object in heap.values() {
        unsafe {

            let shared_rc = heap_object.shared_rc();
            let epoch_rc = heap_object.epoch_rc();

            if shared_rc == 0 {
                // If shared_rc is zero, then we can release this object
                release(*heap_object);
                /*
            } else if shared_rc > epoch_rc {
                // If the epoch_rc is less than the shared_rc, we've seen an
                // increment and can mark the object black.
                heap_object.set_epoch_rc(shared_rc);
                scan_black(*heap_object);
            } else {
                heap_object.set_epoch_rc(shared_rc);
                if heap_object.color() != Color::Black {
                    possible_root(*heap_object);
                    continue;
                }
                // Otherwise, we must assume that object is a possible root
                // heap_object.set_epoch_rc(shared_rc);
                */
            }
        }
    }

    unsafe { process_cycles() };
}

/*
// Run a collection epoch. Returns false if we've been cancelled and should exit.
async unsafe fn epoch(
    last_epoch: &mut Instant,
    mutation_buffer_rx: &mut AsyncReceiver<Mutation>,
) -> bool {
    unsafe {
        process_mutation_buffer(mutation_buffer_rx).await;
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
async unsafe fn process_mutation_buffer(mutation_buffer_rx: &mut AsyncReceiver<Mutation>) {
    // let to_recv = mutation_buffer_rx.len();
    tokio::time::sleep(Duration::from_millis(10)).await;

    // It is very important that we do not delay any mutations that
    // have occurred at this point by an extra epoch.
    PENDING_MUTATIONS.store(
        mutation_buffer_rx.len(),
        std::sync::atomic::Ordering::Relaxed,
    );

    let to_recv = mutation_buffer_rx.len();

    for _ in 0..to_recv {
        let mutation = mutation_buffer_rx.recv().await.unwrap();
        unsafe {
            match mutation.kind {
                MutationKind::Inc => increment(mutation.gc),
                MutationKind::Dec => decrement(mutation.gc),
            }
        }
    }
}


unsafe fn increment(s: OpaqueGcPtr) {
    unsafe {
        s.set_rc(s.rc() + 1);
        scan_black(s);
    }
}
*/

unsafe fn decrement(s: OpaqueGcPtr) {
    unsafe {
        if s.dec_shared_rc() == 1 {
            if !s.buffered() {
                release(s);
            }
        //} else {
        //    possible_root(s);
        }
    }
}

unsafe fn release(s: OpaqueGcPtr) {
    unsafe {
        for_each_child(s, &mut |c| decrement(c));
        s.set_color(Color::Black);
        // if !s.buffered() {
            free(s);
        // }
    }
}

unsafe fn possible_root(s: OpaqueGcPtr) {
    unsafe {
        // println!("possible root!");
        scan_black(s);
        s.set_color(Color::Purple);
        // if !s.buffered() {
        //     s.set_buffered(true);
            (&raw mut ROOTS).as_mut().unwrap().push(s);
        // }
    }
}

unsafe fn process_cycles() {
    /*
    unsafe {
        free_cycles();
        collect_cycles();
        sigma_preparation();
    }
     */
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
            if s.color() == Color::Purple && s.epoch_rc() > 0 {
                mark_gray(*s);
                new_roots.push(*s);
            } else {
                // s.set_buffered(false);
                if s.epoch_rc() == 0 {
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
//            } else {
//                s.set_buffered(false);
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
            s.set_crc(s.epoch_rc() as isize);
            for_each_child(s, &mut |t| stack.push(MarkGrayPhase::MarkGray(t)))
        }
        while let Some(s) = stack.pop() {
            match s {
                MarkGrayPhase::MarkGray(s) => {
                    if s.color() != Color::Gray {
                        s.set_color(Color::Gray);
                        s.set_crc(s.epoch_rc() as isize);
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

unsafe fn scan_black(s: HeapObject<()>) {
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
                // s.set_buffered(true);
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
                n.set_crc(n.epoch_rc() as isize);
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
                    // n.set_buffered(false);
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
                m.dec_shared_rc();
                m.dec_epoch_rc();
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

        // Remove the object from the pool
        (&raw mut HEAP).as_mut().unwrap().remove(s.pool_index().unwrap());

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

    #[test]
    #[ignore]
    fn cycles() {
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
        unsafe {
            epoch();
            epoch();
        }

        assert_eq!(Arc::strong_count(&out_ptr), 1);
    }
}
