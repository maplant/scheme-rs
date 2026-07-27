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
    /// Packed state word (rc | color | flags). Mutators' sole touchpoint, via atomic RMWs only.
    pub(crate) state: AtomicUsize,
    /// Circular reference count (collector-private)
    crc: isize,
    /// VTable for the type
    vtable: &'static VTable,
    layout: Layout,
    /// Intrusive attention-list link; sole-writer is the claim winner until drain resets it.
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

pub(crate) static TOTAL_ALLOCS: AtomicUsize = AtomicUsize::new(0);
pub(crate) static TOTAL_FREES: AtomicUsize = AtomicUsize::new(0);
#[cfg(debug_assertions)]
pub(crate) static RETAIN_PURGES: AtomicUsize = AtomicUsize::new(0);
#[cfg(debug_assertions)]
pub(crate) static INLOOP_GUARD_HITS: AtomicUsize = AtomicUsize::new(0);
#[cfg(debug_assertions)]
pub(crate) static TRIALS_RUN: AtomicUsize = AtomicUsize::new(0);

const LOCAL_ALLOCS_PER_SIGNAL: usize = 1024;

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
    /// True while an epoch is running; an epoch already past its drain phase
    /// cannot satisfy a sync caller.
    collecting: bool,
}

impl CollectorState {
    const fn new() -> Self {
        Self {
            epoch: 0,
            force_collection: false,
            pending_allocs: 0,
            collecting: false,
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

/// 0 terminates the attention-list chain; 1 means not enqueued.
pub(crate) const NOT_IN_LIST: usize = 1;

/// Push-only Treiber stack; no pops, so no ABA.
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

/// Only non-black increments are events (active trial window).
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
    // An in-flight epoch may already be past its drain phase, so its
    // completion proves nothing about our events; require one more.
    let target_epoch = state.epoch + if state.collecting { 2 } else { 1 };
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
    cycles: Vec<Vec<OpaqueGcPtr>>,
    freed_objs: HashSet<OpaqueGcPtr>,
    /// Defers deallocs one epoch to avoid cacheline ping-pong with mutators.
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
                    // Loud abort beats silent hang if the collector dies.
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
        state.collecting = true;
    }

    fn epoch(&mut self) {
        self.await_epoch();

        self.flush_quarantine();

        // Drain before free_cycles: ordering is load-bearing for ATTN_DEAD deferral.
        self.drain_attention_list();

        // Purge freed members before dereferencing; clear per-epoch to avoid stale-address collisions.
        self.cycles.retain_mut(|cycle| {
            cycle.retain(|obj| {
                let freed = self.freed_objs.contains(obj);
                if freed {
                    #[cfg(debug_assertions)]
                    RETAIN_PURGES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                }
                !freed
            });
            !cycle.is_empty()
        });

        unsafe {
            self.free_cycles();
        }

        unsafe {
            self.process_cycles();
        }

        // Frees recorded during free_cycles target objects that cannot be in
        // any pending cycle; drop them now so recycled addresses never purge
        // a fresh parking in a later epoch.
        self.freed_objs.clear();

        let mut state = COLLECTOR_STATE.lock();
        state.epoch += 1;
        state.collecting = false;
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
                    // Pending drain entry will handle the release.
                } else if w.color() == Color::Orange {
                    // Orange: free_cycles owns it this epoch.
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

    unsafe fn process_drained(&mut self, header: *mut GcHeader) {
        unsafe {
            let word = GcState((*header).state.load(std::sync::atomic::Ordering::Acquire));

            if word.attn_dead() {
                // Finalized while claimed; no live handles, safe to dealloc.
                let layout = (*header).layout;
                self.dealloc_quarantine.push((header as *mut u8, layout));
                TOTAL_FREES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                return;
            }

            let obj = OpaqueGcPtr::from_ptr(header).unwrap();

            if word.rc() == 0 {
                if word.color() == Color::Orange {
                    // Orange: free_cycles owns it this epoch.
                } else {
                    // Zero rc is authoritative (counted-homes invariant holds); release now.
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
                        // Foreign RMW raced; re-enqueue for next epoch.
                        attn_push(header);
                        return;
                    }
                    self.release(obj);
                    return;
                }
            } else {
                // Fuse color into the CAS: a separate set_color would invalidate the snapshot.
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
                            // Already black via fused CAS; recurse children directly.
                            for_each_child(obj, &mut |c| scan_black(c));
                        } else {
                            self.roots.insert(obj);
                        }
                    }
                    // Foreign RMW raced; re-enqueue for next epoch.
                    Err(_) => attn_push(header),
                }
                return;
            }

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
                #[cfg(debug_assertions)]
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
                // An earlier cycle in this batch may have freed a shared member; check without deref.
                if c.iter().any(|n| self.freed_objs.contains(n)) {
                    #[cfg(debug_assertions)]
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

            self.roots.remove(&s);

            // Record the free so the next epoch purges any entry for this
            // object from the pending cycle list before dereferencing it.
            self.freed_objs.insert(s);

            // Finalize the object:
            (s.finalize())(s.data_mut());

            // Defer dealloc if claimed: header must outlive pending attention-list entry.
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

/// Recolor Black and re-claim so the next drain releases via the standard path.
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

/// Recomputes external-ref sum from current rcs/edges to close the delayed-notification window.
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

    // Tests that force epochs must not interleave.
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
        assert!(!unsafe { sigma_recheck(&[oa, ob]) });

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
