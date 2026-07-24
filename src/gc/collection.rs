//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use std::{
    alloc::Layout,
    cell::UnsafeCell,
    ptr::NonNull,
    sync::{OnceLock, atomic::AtomicUsize},
    thread::JoinHandle,
};

use parking_lot::{Condvar, Mutex};
use rustc_hash::FxHashSet as HashSet;
use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    exceptions::Exception,
    gc::state::{ATTN_CLAIM, ATTN_DEAD, Color, GcState, INC_EVENT},
    registry::bridge,
    value::Value,
};

#[derive(Debug)]
#[repr(C, align(8))]
pub(crate) struct GcHeader {
    /// Packed state word: rc | color | flags. See [`crate::gc::state`].
    /// Mutators touch ONLY this field, and only via atomic RMWs.
    pub(crate) state: AtomicUsize,
    /// Circular reference count (collector-private)
    crc: isize,
    /// VTable for the type
    vtable: &'static VTable,
    /// Layout of the type and header
    layout: Layout,
    /// Intrusive attention-list link. NOT_IN_LIST when unclaimed; the claim
    /// winner (mutator or re-enqueueing collector) is its sole writer until
    /// the drain resets it.
    attn_next: AtomicUsize,
}

#[bridge(name = "gc-header-size", lib = "(runtime (1))")]
pub fn gc_header_size() -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(std::mem::size_of::<GcHeader>())])
}

impl GcHeader {
    pub(crate) fn new<T: super::GcOrTrace>() -> Self {
        Self {
            state: AtomicUsize::new(GcState::new_initial().0),
            crc: 1,
            vtable: T::VTABLE,
            layout: Layout::new::<super::GcInner<T>>(),
            attn_next: AtomicUsize::new(NOT_IN_LIST),
        }
    }
}

#[derive(Debug, Clone)]
struct VTable {
    /// Type-erased visitor function
    visit_children: unsafe fn(this: *const (), visitor: &mut dyn FnMut(HeapObject<()>)),
    /// Type-erased finalizer function
    finalize: unsafe fn(this: *mut ()),
}

impl VTable {
    const fn new<T: super::GcOrTrace>() -> Self {
        Self {
            visit_children: |this, visitor| unsafe {
                let this = this as *const UnsafeCell<T> as *const T;
                T::visit_or_recurse(this.as_ref().unwrap(), visitor);
            },
            finalize: |this| unsafe {
                let this = this as *mut T;
                T::finalize_or_skip(this.as_mut().unwrap());
            },
        }
    }
}

trait TypeVTable {
    const VTABLE: &'static VTable;
}

impl<T: super::GcOrTrace> TypeVTable for T {
    const VTABLE: &'static VTable = &VTable::new::<T>();
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
        write!(f, "{:p}", self.header.as_ptr())
    }
}

#[doc(hidden)]
pub type OpaqueGcPtr = HeapObject<()>;

impl HeapObject<()> {
    unsafe fn from_ptr(ptr: *mut GcHeader) -> Option<Self> {
        if ptr.is_null() {
            return None;
        }

        let header = NonNull::new(ptr as *mut UnsafeCell<GcHeader>).unwrap();

        let (_, header_offset) = Layout::new::<GcHeader>()
            .extend(unsafe { (*header.as_ref().get()).layout })
            .unwrap();

        let data = unsafe { (ptr as *mut ()).byte_add(header_offset) };
        Some(Self {
            header,
            data: NonNull::new(data as *mut UnsafeCell<()>).unwrap(),
        })
    }

    unsafe fn as_ptr(&self) -> *mut GcHeader {
        self.header.as_ptr() as *mut GcHeader
    }

    unsafe fn state(&self) -> &AtomicUsize {
        unsafe { &(*self.header.as_ref().get()).state }
    }

    unsafe fn shared_rc(&self) -> usize {
        unsafe { GcState(self.state().load(std::sync::atomic::Ordering::Acquire)).rc() }
    }

    unsafe fn dec_shared_rc(&self) -> usize {
        unsafe { GcState(self.state().fetch_sub(1, std::sync::atomic::Ordering::Release)).rc() }
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
        unsafe { GcState(self.state().load(std::sync::atomic::Ordering::Acquire)).color() }
    }

    unsafe fn set_color(&self, color: Color) {
        unsafe {
            self.state()
                .fetch_update(
                    std::sync::atomic::Ordering::AcqRel,
                    std::sync::atomic::Ordering::Acquire,
                    |w| Some(GcState(w).with_color(color).0),
                )
                .unwrap();
        }
    }

    unsafe fn visit_children(
        &self,
    ) -> unsafe fn(this: *const (), visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        unsafe { (*self.header.as_ref().get()).vtable.visit_children }
    }

    unsafe fn finalize(&self) -> unsafe fn(this: *mut ()) {
        unsafe { (*self.header.as_ref().get()).vtable.finalize }
    }

    unsafe fn layout(&self) -> Layout {
        unsafe { (*self.header.as_ref().get()).layout }
    }

    unsafe fn data(&self) -> *const () {
        self.data.as_ptr() as *const UnsafeCell<()> as *const ()
    }

    unsafe fn data_mut(&self) -> *mut () {
        self.data.as_ptr() as *mut ()
    }
}

unsafe impl Send for HeapObject<()> {}
unsafe impl Sync for HeapObject<()> {}

#[allow(private_bounds)]
pub(crate) unsafe fn unroot<T: super::GcOrTrace>(gc: &super::Gc<T>, layout: Layout) {
    let header = gc.ptr.as_ptr() as *mut GcHeader;

    unsafe {
        (*header).layout = layout;
    }

    alloc_tick();
}

thread_local! {
    static ALLOC_TICK: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Total objects allocated (statistics; relaxed, collector never reads it
/// for decisions).
pub(crate) static TOTAL_ALLOCS: AtomicUsize = AtomicUsize::new(0);
/// Total objects freed (statistics; relaxed, incremented at the two dealloc
/// sites in `process_drained` and `free`).
pub(crate) static TOTAL_FREES: AtomicUsize = AtomicUsize::new(0);

/// Measure-first counters (design doc §3, plan Task 4): decide whether
/// `freed_objs`/`cycles.retain` simplification and candidate aging are worth
/// building, rather than speculating. All relaxed; the collector never reads
/// them for decisions.
///
/// Incremented once per cycle member purged by `epoch`'s retain pass (an
/// object recorded in a pending cycle that was freed before the cycle's
/// trial ran).
pub(crate) static RETAIN_PURGES: AtomicUsize = AtomicUsize::new(0);
/// Incremented once per cycle where `free_cycles`' in-loop freed-member guard
/// fires (a recorded cycle sharing a member with one already freed earlier
/// in the same batch).
pub(crate) static INLOOP_GUARD_HITS: AtomicUsize = AtomicUsize::new(0);
/// Incremented once per candidate `mark_roots` processes (every entry in
/// `self.roots` at the start of a trial, purple or not).
pub(crate) static TRIALS_RUN: AtomicUsize = AtomicUsize::new(0);

const LOCAL_ALLOCS_PER_SIGNAL: usize = 1024;

/// Batches per-thread allocation counts into `COLLECTOR_STATE.pending_allocs`
/// every `LOCAL_ALLOCS_PER_SIGNAL` allocations, keeping the allocation path
/// lock-free the rest of the time.
fn alloc_tick() {
    ALLOC_TICK.with(|t| {
        let n = t.get() + 1;
        if n >= LOCAL_ALLOCS_PER_SIGNAL {
            t.set(0);
            flush_events();
            TOTAL_ALLOCS.fetch_add(n, std::sync::atomic::Ordering::Relaxed);
            let mut state = COLLECTOR_STATE.lock();
            state.pending_allocs += n;
            if state.pending_allocs >= MIN_ALLOCS_TO_COLLECT {
                COLLECTION_START_SIGNAL.notify_one();
            }
        } else {
            t.set(n);
        }
    });
}

struct CollectorState {
    epoch: usize,
    force_collection: bool,
    pending_allocs: usize,
}

impl CollectorState {
    const fn new() -> Self {
        Self {
            epoch: 0,
            force_collection: false,
            pending_allocs: 0,
        }
    }

    fn should_not_collect(&mut self) -> bool {
        self.pending_allocs < MIN_ALLOCS_TO_COLLECT && !self.force_collection
    }
}

static COLLECTOR_STATE: Mutex<CollectorState> = Mutex::new(CollectorState::new());
static COLLECTION_START_SIGNAL: Condvar = Condvar::new();
static COLLECTION_DONE_SIGNAL: Condvar = Condvar::new();
static COLLECTOR_TASK: OnceLock<JoinHandle<()>> = OnceLock::new();
const MIN_ALLOCS_TO_COLLECT: usize = 10_000;

/// Sentinel for "not on the attention list". 0 terminates a chain.
pub(crate) const NOT_IN_LIST: usize = 1;

/// Global attention list: push-only Treiber stack, swap-drained whole by the
/// collector each epoch. No pops → no ABA.
static ATTN_HEAD: AtomicUsize = AtomicUsize::new(0);

unsafe fn attn_push(header: *mut GcHeader) {
    unsafe {
        let mut head = ATTN_HEAD.load(std::sync::atomic::Ordering::Relaxed);
        loop {
            (*header)
                .attn_next
                .store(head, std::sync::atomic::Ordering::Relaxed);
            match ATTN_HEAD.compare_exchange_weak(
                head,
                header as usize,
                std::sync::atomic::Ordering::Release,
                std::sync::atomic::Ordering::Relaxed,
            ) {
                Ok(_) => return,
                Err(actual) => head = actual,
            }
        }
    }
}

const EVENT_BATCH: usize = 64;

thread_local! {
    static EVENT_BUFFER: EventBuffer = const { EventBuffer::new() };
}

struct EventBuffer(std::cell::RefCell<Vec<*mut GcHeader>>);

impl EventBuffer {
    const fn new() -> Self {
        Self(std::cell::RefCell::new(Vec::new()))
    }
}

impl Drop for EventBuffer {
    // Thread exit (including unwind): nothing may be left behind.
    fn drop(&mut self) {
        flush_chain(&mut self.0.borrow_mut());
    }
}

/// Mutator-side event enqueue: buffer locally, splice whole batches with a
/// single ATTN_HEAD CAS. Claim bit semantics widen to "in some buffer".
pub(crate) fn buffer_event(header: *mut GcHeader) {
    let ok = EVENT_BUFFER.try_with(|buf| {
        let mut buf = buf.0.borrow_mut();
        buf.push(header);
        if buf.len() >= EVENT_BATCH {
            flush_chain(&mut buf);
        }
    });
    if ok.is_err() {
        // TLS already destroyed (thread teardown): push directly.
        unsafe { attn_push(header) };
    }
}

/// Flush the calling thread's buffered events to the global list.
pub(crate) fn flush_events() {
    let _ = EVENT_BUFFER.try_with(|buf| flush_chain(&mut buf.0.borrow_mut()));
}

fn flush_chain(buf: &mut Vec<*mut GcHeader>) {
    let Some(&first) = buf.first() else { return };
    unsafe {
        // Sole-owner links: every header here is claim-won by this thread.
        for w in buf.windows(2) {
            (*w[0])
                .attn_next
                .store(w[1] as usize, std::sync::atomic::Ordering::Relaxed);
        }
        let tail = *buf.last().unwrap();
        let mut head = ATTN_HEAD.load(std::sync::atomic::Ordering::Relaxed);
        loop {
            (*tail)
                .attn_next
                .store(head, std::sync::atomic::Ordering::Relaxed);
            match ATTN_HEAD.compare_exchange_weak(
                head,
                first as usize,
                std::sync::atomic::Ordering::Release,
                std::sync::atomic::Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(actual) => head = actual,
            }
        }
    }
    buf.clear();
}

/// Called by every mutator decrement with the pre-decrement word.
/// One claim per object per epoch: repeat decs see ATTN_CLAIM and skip.
#[inline]
pub(crate) unsafe fn record_dec_event(header: *mut GcHeader, old: GcState) {
    if old.attn_claimed() {
        return;
    }
    unsafe {
        let w = GcState(
            (*header)
                .state
                .fetch_or(ATTN_CLAIM, std::sync::atomic::Ordering::AcqRel),
        );
        if !w.attn_claimed() {
            buffer_event(header);
        }
    }
}

/// Called by every mutator increment with the pre-increment word. Only
/// increments on non-black objects (active trial windows) are events
/// (design doc "Scenario 1"); the fast path is a branch on a value the
/// fetch_add already returned.
#[inline]
pub(crate) unsafe fn record_inc_event(header: *mut GcHeader, old: GcState) {
    if old.color() == Color::Black {
        return;
    }
    unsafe {
        let w = GcState(
            (*header)
                .state
                .fetch_or(INC_EVENT | ATTN_CLAIM, std::sync::atomic::Ordering::AcqRel),
        );
        if !w.attn_claimed() {
            buffer_event(header);
        }
    }
}

/// Initializes the garbage collector thread. Calling this function is typically
/// not required as creating a [`Runtime`](crate::runtime::Runtime)
/// automatically calls it.
///
/// Calling this function multiple times does nothing, there is only one
/// collector thread allowed at a time.
pub fn init_gc() {
    let _ = COLLECTOR_TASK.get_or_init(|| Collector::new().run());
}

fn collect_garbage_sync() {
    // An explicit collection must see the caller's own pending events.
    flush_events();
    let mut state = COLLECTOR_STATE.lock();
    let target_epoch = state.epoch + 1;
    state.force_collection = true;
    COLLECTION_START_SIGNAL.notify_one();
    COLLECTION_DONE_SIGNAL.wait_while(&mut state, |state| state.epoch < target_epoch);
}

/// Force a garbage collection pause.
#[cfg(not(feature = "async"))]
pub fn collect_garbage() {
    collect_garbage_sync();
}

#[cfg(feature = "tokio")]
pub async fn collect_garbage() {
    tokio::task::spawn_blocking(|| {
        collect_garbage_sync();
    })
    .await
    .unwrap();
}

#[maybe_async]
#[bridge(name = "collect-garbage", lib = "(runtime (1))")]
pub fn collect_garbage_bridge() -> Result<Vec<Value>, Exception> {
    maybe_await!(collect_garbage());
    Ok(Vec::new())
}

#[derive(Debug)]
pub struct Collector {
    roots: HashSet<OpaqueGcPtr>,
    /// Cycles recorded last epoch by `collect_roots`, freed or refurbished
    /// by `free_cycles` at the top of this epoch (design §4: free at N+1,
    /// full Δ/σ/fresh-Σ/flag revalidation at free time — the N+2 grace
    /// WITHDRAWN, commit aa6fe74: a pending cycle must never coexist with a
    /// running trial).
    cycles: Vec<Vec<OpaqueGcPtr>>,
    freed_objs: HashSet<OpaqueGcPtr>,
    /// Dealloc quarantine: finalization happens on schedule, but the
    /// underlying dealloc is deferred one epoch so the mutator never
    /// reallocates memory the collector freed microseconds earlier
    /// (free/realloc cacheline ping-pong under churn). Purely collector-
    /// local; entries are already finalized and unreachable.
    dealloc_quarantine: Vec<(*mut u8, Layout)>,
}

unsafe impl Send for Collector {}

impl Collector {
    fn new() -> Self {
        Self {
            roots: HashSet::default(),
            cycles: Vec::new(),
            freed_objs: HashSet::default(),
            dealloc_quarantine: Vec::new(),
        }
    }

    /// Dealloc everything quarantined during the previous epoch.
    fn flush_quarantine(&mut self) {
        for (ptr, layout) in self.dealloc_quarantine.drain(..) {
            unsafe { std::alloc::dealloc(ptr, layout) };
        }
    }

    fn run(mut self) -> JoinHandle<()> {
        std::thread::spawn(move || {
            let this = &mut self;
            loop {
                if let Err(panic) = std::panic::catch_unwind(
                    std::panic::AssertUnwindSafe(|| this.epoch()),
                ) {
                    // A dead collector turns every later collect_garbage()
                    // into a silent hang; loud crash beats silent hang.
                    eprintln!(
                        "fatal: GC collector thread panicked: {panic:?}\n{}",
                        std::backtrace::Backtrace::force_capture()
                    );
                    std::process::abort();
                }
            }
        })
    }

    fn await_epoch(&mut self) {
        let mut state = COLLECTOR_STATE.lock();

        COLLECTION_START_SIGNAL.wait_while(&mut state, CollectorState::should_not_collect);

        state.pending_allocs = 0;
        state.force_collection = false;
    }

    fn epoch(&mut self) {
        self.await_epoch();

        // Release last epoch's quarantined memory before doing anything
        // else: entries have aged one full epoch, so the mutator is no
        // longer hot on those cachelines.
        self.flush_quarantine();

        // Drain the attention list before free_cycles: any corpse freed this
        // epoch either predates this drain (entry consumed now) or was
        // pushed after it (ATTN_DEAD defers its dealloc to the next drain).
        // Order is load-bearing — see the phase 2 design doc §2.
        self.drain_attention_list();

        // Remove freed objects from cycles recorded last epoch. Every free
        // since the last retain is in freed_objs (free() records
        // unconditionally, covering release() cascades), and cycles are only
        // dereferenced below, after this retain. Clearing per epoch keeps
        // recycled addresses from purging fresh parkings later.
        self.cycles.retain_mut(|cycle| {
            cycle.retain(|obj| {
                let freed = self.freed_objs.contains(obj);
                if freed {
                    RETAIN_PURGES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                }
                !freed
            });
            !cycle.is_empty()
        });

        // Free cycles recorded last epoch: the N -> free-at-N+1 hand-over
        // (design §0/§4). free_cycles takes ALL of self.cycles, so
        // process_cycles below records this epoch's new candidates into an
        // empty vec — a recorded cycle is always consumed before the next
        // trial runs.
        unsafe {
            self.free_cycles();
        }

        // Process cycles: trial deletion, then sigma_preparation for the
        // cycles this trial just recorded.
        unsafe {
            self.process_cycles();
        }

        // Frees recorded during free_cycles target objects that cannot be in
        // any pending cycle; drop them now so recycled addresses never purge
        // a fresh parking in a later epoch.
        self.freed_objs.clear();

        let mut state = COLLECTOR_STATE.lock();
        state.epoch += 1;
        COLLECTION_DONE_SIGNAL.notify_all();
    }

    unsafe fn decrement(&mut self, s: OpaqueGcPtr) {
        unsafe {
            let old_rc = s.dec_shared_rc();
            if old_rc == 1 {
                let w = GcState(
                    (*s.header.as_ref().get())
                        .state
                        .load(std::sync::atomic::Ordering::Acquire),
                );
                if w.attn_claimed() {
                    // A pending entry exists; the drain that consumes it
                    // finds rc==0 and releases (or ATTN_DEAD-deallocs) then.
                } else if w.color() == Color::Orange {
                    // Orange-skip (design §0): a collector-internal cascade
                    // never releases a currently-Orange object — free_cycles
                    // owns it this epoch. If its cycle refurbishes, the
                    // refurbish-zero reroute frees it via the drain path.
                } else {
                    self.release(s);
                }
            } else {
                s.set_color(Color::Purple);
                self.roots.insert(s);
            }
        }
    }

    unsafe fn release(&mut self, s: OpaqueGcPtr) {
        unsafe {
            for_each_child(s, &mut |c| self.decrement(c));
            s.set_color(Color::Black);
            self.free(s)
        }
    }

    fn drain_attention_list(&mut self) {
        let mut node =
            ATTN_HEAD.swap(0, std::sync::atomic::Ordering::Acquire) as *mut GcHeader;
        while !node.is_null() {
            let next = unsafe {
                (*node)
                    .attn_next
                    .load(std::sync::atomic::Ordering::Relaxed)
            };
            unsafe {
                self.process_drained(node);
            }
            node = next as *mut GcHeader;
        }
    }

    /// Process one drained entry (now authoritative — design doc §3).
    /// Membership ends only via the release CAS proving the word didn't
    /// change during processing; otherwise keep the claim and re-enqueue.
    unsafe fn process_drained(&mut self, header: *mut GcHeader) {
        unsafe {
            let word = GcState((*header).state.load(std::sync::atomic::Ordering::Acquire));

            if word.attn_dead() {
                // Finalized while claimed; we own the last reference to the
                // header memory. No live handles exist, so no concurrent RMW
                // can race this dealloc.
                let layout = (*header).layout;
                self.dealloc_quarantine.push((header as *mut u8, layout));
                TOTAL_FREES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                return;
            }

            let obj = OpaqueGcPtr::from_ptr(header).unwrap();

            if word.rc() == 0 {
                if word.color() == Color::Orange {
                    // Orange-skip (design §3): a recorded cycle member —
                    // free_cycles owns it this epoch. Consume the entry only.
                } else {
                    // Genuinely dead: release now, single stage. The
                    // counted-homes invariant (a raw pointer's home keeps it
                    // counted until the increment that reads it back) is
                    // fixed at its single violation site (CPS liveness, see
                    // cps::analysis) and asserted in `inc_rc`, so a zero
                    // sighting here is no longer a transient resurrection
                    // window.
                    (*header)
                        .attn_next
                        .store(NOT_IN_LIST, std::sync::atomic::Ordering::Relaxed);
                    if (*header)
                        .state
                        .compare_exchange(
                            word.0,
                            word.0 & !(ATTN_CLAIM | INC_EVENT),
                            std::sync::atomic::Ordering::AcqRel,
                            std::sync::atomic::Ordering::Acquire,
                        )
                        .is_err()
                    {
                        // Foreign RMW mid-processing: keep the claim,
                        // reclassify from the fresh word next epoch.
                        attn_push(header);
                        return;
                    }
                    self.release(obj);
                    return;
                }
            } else {
                // Non-zero rc: FUSE the color mutation into the release CAS
                // (design §3 amendment). A separate set_color would change
                // the live word and self-defeat a stale-snapshot CAS —
                // guaranteed failure, eternal re-enqueue, Orange members
                // repainted Purple, Δ-test permanently broken.
                let (new_color, is_inc) = if word.inc_event() {
                    (Color::Black, true)
                } else {
                    (Color::Purple, false)
                };
                let target = GcState(word.0 & !(ATTN_CLAIM | INC_EVENT))
                    .with_color(new_color)
                    .0;
                (*header)
                    .attn_next
                    .store(NOT_IN_LIST, std::sync::atomic::Ordering::Relaxed);
                match (*header).state.compare_exchange(
                    word.0,
                    target,
                    std::sync::atomic::Ordering::AcqRel,
                    std::sync::atomic::Ordering::Acquire,
                ) {
                    Ok(_) => {
                        if is_inc {
                            // Node is Black via the fused CAS; scan_black
                            // skips already-black roots, so recurse from
                            // the children.
                            for_each_child(obj, &mut |c| scan_black(c));
                        } else {
                            self.roots.insert(obj);
                        }
                    }
                    // Foreign mutator RMW during processing: keep the claim,
                    // reclassify from the fresh word next epoch.
                    Err(_) => attn_push(header),
                }
                return;
            }

            // Orange-skip falls through to here: consume the entry only
            // (no color mutation → original-snapshot CAS is correct).
            (*header)
                .attn_next
                .store(NOT_IN_LIST, std::sync::atomic::Ordering::Relaxed);
            let cleared = word.0 & !(ATTN_CLAIM | INC_EVENT);
            if (*header)
                .state
                .compare_exchange(
                    word.0,
                    cleared,
                    std::sync::atomic::Ordering::AcqRel,
                    std::sync::atomic::Ordering::Acquire,
                )
                .is_err()
            {
                attn_push(header);
            }
        }
    }

    unsafe fn process_cycles(&mut self) {
        unsafe {
            self.collect_cycles();
            self.sigma_preparation();
        }
    }

    unsafe fn collect_cycles(&mut self) {
        unsafe {
            self.mark_roots();
            self.scan_roots();
            self.collect_roots()
        }
    }

    unsafe fn mark_roots(&mut self) {
        unsafe {
            self.roots.retain(|s| {
                TRIALS_RUN.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                if s.color() == Color::Purple {
                    mark_gray(*s);
                    true
                } else {
                    false
                }
            })
        }
    }

    unsafe fn scan_roots(&mut self) {
        for s in self.roots.iter() {
            unsafe {
                scan(*s);
            }
        }
    }

    unsafe fn collect_roots(&mut self) {
        for s in self.roots.drain() {
            unsafe {
                if s.color() == Color::White {
                    let mut curr_cycle = Vec::new();
                    collect_white(s, &mut curr_cycle);
                    self.cycles.push(curr_cycle);
                }
            }
        }
    }

    /// Runs over `self.cycles`: `free_cycles` (earlier this epoch) consumed
    /// everything recorded before this epoch via `mem::take`, so at this
    /// point `self.cycles` holds only the candidates `collect_roots` just
    /// recorded.
    unsafe fn sigma_preparation(&self) {
        unsafe {
            for c in &self.cycles {
                for n in c {
                    n.set_color(Color::Red);
                    n.set_crc(n.shared_rc() as isize);
                }
                for n in c {
                    for_each_child(*n, &mut |m| {
                        if m.color() == Color::Red && m.crc() > 0 {
                            m.set_crc(m.crc() - 1);
                        }
                    })
                }
                for n in c {
                    n.set_color(Color::Orange);
                }
            }
        }
    }

    unsafe fn free_cycles(&mut self) {
        unsafe {
            for c in std::mem::take(&mut self.cycles).into_iter().rev() {
                // In-loop freed guard (design §0): drain-time repainting can
                // let two recorded cycles share a member, so an earlier
                // cycle in THIS SAME batch may have already freed one of
                // this cycle's members. Pointer-membership check only — no
                // deref — before the Δ/σ tests below dereference every
                // member.
                if c.iter().any(|n| self.freed_objs.contains(n)) {
                    INLOOP_GUARD_HITS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    for n in c.iter().filter(|n| !self.freed_objs.contains(n)) {
                        if n.shared_rc() == 0 {
                            refurbish_zero(*n);
                        } else {
                            n.set_color(Color::Purple);
                            self.roots.insert(*n);
                        }
                    }
                    continue;
                }
                if delta_test(&c) && sigma_test(&c) && sigma_recheck(&c) && member_flags_clear(&c) {
                    self.free_cycle(&c);
                } else {
                    self.refurbish(&c);
                }
            }
        }
    }

    unsafe fn free_cycle(&mut self, c: &[OpaqueGcPtr]) {
        unsafe {
            for n in c {
                n.set_color(Color::Red);
            }
            for n in c {
                for_each_child(*n, &mut |c| self.cyclic_decrement(c));
            }
            for n in c {
                self.free(*n);
            }
        }
    }

    unsafe fn refurbish(&mut self, c: &[OpaqueGcPtr]) {
        unsafe {
            for (i, n) in c.iter().enumerate() {
                if n.shared_rc() == 0 {
                    // Zero event may already be consumed (Orange-skip or a
                    // fresh decrement racing validation) — the refurbish-zero
                    // reroute re-enters the drain path rather than freeing
                    // inline (design §0/§4).
                    refurbish_zero(*n);
                    continue;
                }
                match (i, n.color()) {
                    (0, Color::Orange) | (_, Color::Purple) => {
                        n.set_color(Color::Purple);
                        self.roots.insert(*n);
                    }
                    _ => n.set_color(Color::Black),
                }
            }
        }
    }

    unsafe fn cyclic_decrement(&mut self, m: OpaqueGcPtr) {
        unsafe {
            if m.color() != Color::Red {
                if m.color() == Color::Orange {
                    m.dec_shared_rc();
                    m.set_crc(m.crc() - 1);
                } else {
                    self.decrement(m);
                }
            }
        }
    }

    unsafe fn free(&mut self, s: OpaqueGcPtr) {
        unsafe {
            // Safety: No need to acquire a permit, s is guaranteed to be
            // garbage.

            // Ensure it is no longer a possible root:
            self.roots.remove(&s);

            // Record the free so the next epoch purges any entry for this
            // object from the pending cycle list before dereferencing it.
            self.freed_objs.insert(s);

            // Finalize the object:
            (s.finalize())(s.data_mut());

            // Deallocate — unless a mutator claim is pending on the
            // attention list, in which case the header memory must outlive
            // the list entry (never dealloc while claimed). Finalization
            // above already ran on schedule; the drain that consumes the
            // entry performs the dealloc. No new claim can arrive after
            // this check: free() only runs on objects with no live handles,
            // and claims require a handle.
            let word = GcState(
                (*s.header.as_ref().get())
                    .state
                    .load(std::sync::atomic::Ordering::Acquire),
            );
            if word.attn_claimed() {
                (*s.header.as_ref().get())
                    .state
                    .fetch_or(ATTN_DEAD, std::sync::atomic::Ordering::AcqRel);
            } else {
                self.dealloc_quarantine
                    .push((s.header.as_ptr() as *mut u8, s.layout()));
                TOTAL_FREES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }
        }
    }
}

/// The refurbish-zero reroute (design §0/§4): a rc==0 member whose zero
/// event may already be consumed (Orange-skip, or a fresh decrement racing
/// validation) is NOT freed inline — recolor Black (so the next drain's
/// Orange-skip cannot misroute it), then claim so `process_drained`'s
/// single-stage release cascades it properly at the next drain. Every zero
/// free flows through the same drain path.
unsafe fn refurbish_zero(n: OpaqueGcPtr) {
    unsafe {
        n.set_color(Color::Black);
        let old = GcState(
            (*n.header.as_ref().get())
                .state
                .fetch_or(ATTN_CLAIM, std::sync::atomic::Ordering::AcqRel),
        );
        if !old.attn_claimed() {
            attn_push(n.as_ptr());
        }
    }
}

unsafe fn for_each_child(s: OpaqueGcPtr, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
    unsafe {
        (s.visit_children())(s.data(), visitor);
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

enum MarkGrayPhase {
    MarkGray(OpaqueGcPtr),
    SetCrc(OpaqueGcPtr),
}

unsafe fn mark_gray(s: OpaqueGcPtr) {
    unsafe {
        let mut stack = Vec::new();
        if s.color() != Color::Gray {
            s.set_color(Color::Gray);
            s.set_crc(s.shared_rc() as isize);
            for_each_child(s, &mut |t| stack.push(MarkGrayPhase::MarkGray(t)))
        }
        while let Some(s) = stack.pop() {
            match s {
                MarkGrayPhase::MarkGray(s) => {
                    if s.color() != Color::Gray {
                        s.set_color(Color::Gray);
                        s.set_crc(s.shared_rc() as isize);
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

unsafe fn collect_white(s: OpaqueGcPtr, current_cycle: &mut Vec<OpaqueGcPtr>) {
    unsafe {
        let mut stack = vec![s];
        while let Some(s) = stack.pop() {
            if s.color() == Color::White {
                s.set_color(Color::Orange);
                current_cycle.push(s);
                for_each_child(s, &mut |c| stack.push(c));
            }
        }
    }
}

unsafe fn sigma_test(c: &[OpaqueGcPtr]) -> bool {
    unsafe {
        let mut sum = 0;
        for n in c {
            sum += n.crc();
        }
        sum == 0
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

/// Fresh-Σ recheck (phase 2 design §4): recompute the external-reference sum
/// from CURRENT state-word rcs and CURRENT edges, restricted to the member
/// set. Rc effects are visible instantly even when an event push is delayed,
/// so this closes the delayed-notification window ("Scenario 3").
unsafe fn sigma_recheck(c: &[OpaqueGcPtr]) -> bool {
    unsafe {
        let members: HashSet<OpaqueGcPtr> = c.iter().copied().collect();
        let mut external: isize = 0;
        for n in c {
            external += n.shared_rc() as isize;
        }
        for n in c {
            for_each_child(*n, &mut |child| {
                if members.contains(&child) {
                    external -= 1;
                }
            });
        }
        external == 0
    }
}

/// No member may carry an unprocessed event (phase 2 design §4, check 3):
/// a claim is a pending notification — defer the free rather than reason
/// about it.
unsafe fn member_flags_clear(c: &[OpaqueGcPtr]) -> bool {
    unsafe {
        c.iter().all(|n| {
            let w = GcState(
                (*n.header.as_ref().get())
                    .state
                    .load(std::sync::atomic::Ordering::Acquire),
            );
            !w.attn_claimed() && !w.inc_event()
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::gc::*;
    use parking_lot::RwLock;
    use std::sync::Arc;

    // `cycles` and `zero_event_reclaimed_by_first_collection` both force
    // epochs via `collect_garbage_sync` and inspect epoch-count-sensitive
    // state; run them serially against each other so one test's forced epoch
    // can't land inside another's collection window (see the plan's caveat on
    // this test).
    static GC_TEST_SERIAL: Mutex<()> = Mutex::new(());

    #[test]
    fn cycles() {
        let _guard = GC_TEST_SERIAL.lock();
        init_gc();

        #[derive(Default, Trace)]
        struct Cyclic {
            next: Option<Gc<RwLock<Cyclic>>>,
            out: Option<Arc<()>>,
        }

        let out_ptr = Arc::new(());

        let a = Gc::new(RwLock::new(Cyclic::default()));
        let b = Gc::new(RwLock::new(Cyclic::default()));
        let c = Gc::new(RwLock::new(Cyclic::default()));

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

        collect_garbage_sync();
        collect_garbage_sync();
        collect_garbage_sync();

        assert_eq!(Arc::strong_count(&out_ptr), 1);
    }

    #[test]
    fn zero_event_reclaimed_by_first_collection() {
        let _guard = GC_TEST_SERIAL.lock();
        init_gc();

        let out_ptr = Arc::new(());
        let obj = Gc::new(Some(out_ptr.clone()));
        drop(obj);

        // No aging (design §0 superseded, phase 3): the counted-homes
        // invariant is fixed at its single violation site (CPS liveness) and
        // asserted in `inc_rc`, so a zero-rc sighting is reclaimed on the
        // very first drain that observes it.
        collect_garbage_sync();
        assert_eq!(
            Arc::strong_count(&out_ptr),
            1,
            "zero event not consumed by the first drain"
        );
    }

    #[test]
    fn packed_word_rc_and_color_do_not_interfere() {
        let _guard = GC_TEST_SERIAL.lock();
        init_gc();

        let obj = Gc::new(0u64);
        let opaque = unsafe { obj.as_opaque() };

        let mutators: Vec<_> = (0..4)
            .map(|_| {
                let obj = obj.clone();
                std::thread::spawn(move || {
                    for _ in 0..100_000 {
                        let c = obj.clone();
                        drop(c);
                    }
                })
            })
            .collect();

        for _ in 0..10_000 {
            for color in [Color::Gray, Color::Purple, Color::Orange, Color::Black] {
                unsafe {
                    opaque.set_color(color);
                    let observed = opaque.color();
                    assert!(
                        matches!(
                            observed,
                            Color::Gray | Color::Purple | Color::Orange | Color::Black
                        ),
                        "torn color read: {observed:?}"
                    );
                }
            }
        }

        for m in mutators {
            m.join().unwrap();
        }

        unsafe {
            opaque.set_color(Color::Black);
            assert_eq!(opaque.shared_rc(), 1, "rc not conserved under color churn");
        }
        drop(obj);
    }

    #[derive(Default, Trace)]
    struct Linked {
        next: Option<Gc<RwLock<Linked>>>,
    }

    unsafe fn force_rc(obj: &OpaqueGcPtr, rc: usize) {
        unsafe {
            let state = &(*obj.header.as_ref().get()).state;
            let mut w = GcState(state.load(std::sync::atomic::Ordering::Acquire));
            w = GcState((w.0 & !crate::gc::state::RC_MASK) | rc);
            state.store(w.0, std::sync::atomic::Ordering::Release);
        }
    }

    #[test]
    fn sigma_recheck_detects_external_refs() {
        let a = Gc::rooted(RwLock::new(Linked::default()));
        let b = Gc::rooted(RwLock::new(Linked::default()));
        a.write().next = Some(b.clone());
        b.write().next = Some(a.clone());
        let (oa, ob) = unsafe { (a.as_opaque(), b.as_opaque()) };
        // Handles held: rc(a) = rc(b) = 2 (handle + internal edge) →
        // two external refs.
        assert!(!unsafe { sigma_recheck(&[oa, ob]) });

        // Simulate the handles dying without emitting events:
        unsafe {
            force_rc(&oa, 1);
            force_rc(&ob, 1);
        }
        assert!(unsafe { sigma_recheck(&[oa, ob]) });

        // Leak a and b deliberately (rooted, rc now lies) — do not drop.
        std::mem::forget(a);
        std::mem::forget(b);
    }

    #[test]
    fn member_flag_check_blocks_pending_events() {
        let a = Gc::rooted(RwLock::new(Linked::default()));
        let oa = unsafe { a.as_opaque() };
        assert!(unsafe { member_flags_clear(&[oa]) });
        unsafe {
            (*oa.header.as_ref().get())
                .state
                .fetch_or(crate::gc::state::ATTN_CLAIM, std::sync::atomic::Ordering::AcqRel);
        }
        assert!(!unsafe { member_flags_clear(&[oa]) });
        std::mem::forget(a);
    }
}
