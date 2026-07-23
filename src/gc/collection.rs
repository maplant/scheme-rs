//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use std::{
    alloc::Layout,
    cell::UnsafeCell,
    ptr::{NonNull, null_mut},
    sync::{OnceLock, atomic::AtomicUsize},
    thread::JoinHandle,
};

use parking_lot::{Condvar, Mutex};
use rustc_hash::FxHashSet as HashSet;
use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    exceptions::Exception,
    gc::state::{ATTN_CLAIM, ATTN_DEAD, BUFFERED, Color, GcState, INC_EVENT},
    registry::bridge,
    value::Value,
};

#[derive(Debug)]
#[repr(C, align(8))]
pub(crate) struct GcHeader {
    /// Packed state word: rc | color | flags. See [`crate::gc::state`].
    /// Mutators touch ONLY this field, and only via atomic RMWs.
    pub(crate) state: AtomicUsize,
    /// Reference count as of the current epoch (collector-private)
    epoch_rc: usize,
    /// Circular reference count (collector-private)
    crc: isize,
    /// VTable for the type
    vtable: &'static VTable,
    /// Layout of the type and header
    layout: Layout,
    /// Next item in the heap, or null (collector/heap-lock only)
    next: *mut GcHeader,
    /// Previous item in the heap, or null (collector/heap-lock only)
    prev: *mut GcHeader,
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
            epoch_rc: 1,
            crc: 1,
            vtable: T::VTABLE,
            layout: Layout::new::<super::GcInner<T>>(),
            next: null_mut(),
            prev: null_mut(),
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

    unsafe fn buffered(&self) -> bool {
        unsafe { GcState(self.state().load(std::sync::atomic::Ordering::Acquire)).buffered() }
    }

    unsafe fn set_buffered(&self, buffered: bool) {
        unsafe {
            if buffered {
                self.state()
                    .fetch_or(BUFFERED, std::sync::atomic::Ordering::AcqRel);
            } else {
                self.state()
                    .fetch_and(!BUFFERED, std::sync::atomic::Ordering::AcqRel);
            }
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

    unsafe fn next(&self) -> *mut GcHeader {
        unsafe { (*self.header.as_ref().get()).next }
    }

    unsafe fn set_next(&self, next: *mut GcHeader) {
        unsafe {
            (*self.header.as_ref().get()).next = next;
        }
    }

    unsafe fn prev(&self) -> *mut GcHeader {
        unsafe { (*self.header.as_ref().get()).prev }
    }

    unsafe fn set_prev(&self, prev: *mut GcHeader) {
        unsafe {
            (*self.header.as_ref().get()).prev = prev;
        }
    }
}

unsafe impl Send for HeapObject<()> {}
unsafe impl Sync for HeapObject<()> {}

#[allow(private_bounds)]
pub(crate) unsafe fn unroot<T: super::GcOrTrace>(gc: &super::Gc<T>, layout: Layout) {
    let new_gc_ptr = gc.ptr.as_ptr() as *mut GcHeader;

    let mut heap = HEAP.lock();

    unsafe {
        (*new_gc_ptr).layout = layout;

        if heap.nursery_head.is_null() {
            heap.nursery_tail = new_gc_ptr;
        } else {
            (*heap.nursery_head).prev = new_gc_ptr;
        }

        (*new_gc_ptr).next = heap.nursery_head;
    }

    heap.nursery_head = new_gc_ptr;
    heap.new_allocs += 1;

    if heap.should_collect() {
        COLLECTION_START_SIGNAL.notify_one();
    }
}

struct Heap {
    head: *mut GcHeader,
    tail: *mut GcHeader,
    nursery_head: *mut GcHeader,
    nursery_tail: *mut GcHeader,
    new_allocs: usize,
    epoch: usize,
    force_collection: bool,
}

impl Heap {
    const fn new() -> Self {
        Self {
            head: std::ptr::null_mut(),
            tail: std::ptr::null_mut(),
            nursery_head: std::ptr::null_mut(),
            nursery_tail: std::ptr::null_mut(),
            new_allocs: 0,
            epoch: 0,
            force_collection: false,
        }
    }

    fn should_collect(&mut self) -> bool {
        !self.should_not_collect()
    }

    fn should_not_collect(&mut self) -> bool {
        self.new_allocs < MIN_ALLOCS_TO_COLLECT && !self.force_collection
    }
}

unsafe impl Send for Heap {}
unsafe impl Sync for Heap {}

static HEAP: Mutex<Heap> = Mutex::new(Heap::new());
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
            attn_push(header);
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
            attn_push(header);
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
    let mut heap = HEAP.lock();
    let target_epoch = heap.epoch + 1;
    heap.force_collection = true;
    COLLECTION_START_SIGNAL.notify_one();
    COLLECTION_DONE_SIGNAL.wait_while(&mut heap, |heap| heap.epoch < target_epoch);
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
    head: *mut GcHeader,
    tail: *mut GcHeader,
    next: *mut GcHeader,
}

unsafe impl Send for Collector {}

impl Collector {
    fn new() -> Self {
        Self {
            roots: HashSet::default(),
            cycles: Vec::new(),
            freed_objs: HashSet::default(),
            head: null_mut(),
            tail: null_mut(),
            next: null_mut(),
        }
    }

    fn run(mut self) -> JoinHandle<()> {
        std::thread::spawn(move || {
            loop {
                self.epoch();
            }
        })
    }

    fn await_epoch(&mut self) {
        let mut heap = HEAP.lock();

        COLLECTION_START_SIGNAL.wait_while(&mut heap, Heap::should_not_collect);

        let nursery_head = heap.nursery_head;
        let nursery_tail = heap.nursery_tail;
        self.head = std::mem::replace(&mut heap.head, nursery_head);
        self.tail = std::mem::replace(&mut heap.tail, nursery_tail);
        heap.nursery_head = null_mut();
        heap.nursery_tail = null_mut();
        heap.new_allocs = 0;
        heap.force_collection = false;
    }

    fn epoch(&mut self) {
        self.await_epoch();

        // Drain the attention list before the scan: any corpse the scan
        // frees this epoch either predates this drain (entry consumed now)
        // or was pushed after it (ATTN_DEAD defers its dealloc to the next
        // drain). Order is load-bearing — see the phase 1b plan.
        self.drain_attention_list();

        self.next = self.head;

        // Collect obvious garbage; i.e. heap objects that have a ref count of zero,
        // and potential candidates for cycles.
        while let Some(curr_heap_object) = unsafe { OpaqueGcPtr::from_ptr(self.next) } {
            unsafe {
                curr_heap_object.set_buffered(false);

                let shared_rc = curr_heap_object.shared_rc();
                let epoch_rc = curr_heap_object.epoch_rc();

                self.next = curr_heap_object.next();

                if shared_rc == 0 {
                    // If shared_rc is zero, then we can release this object
                    self.release(curr_heap_object);
                } else if shared_rc > epoch_rc {
                    // If the epoch_rc is less than the shared_rc, we've seen an
                    // increment and can mark the object black.
                    curr_heap_object.set_epoch_rc(shared_rc);
                    scan_black(curr_heap_object);
                } else {
                    curr_heap_object.set_epoch_rc(shared_rc);
                    // Otherwise, we must assume that object is a possible root
                    if curr_heap_object.color() == Color::Black {
                        scan_black(curr_heap_object);
                        curr_heap_object.set_color(Color::Purple);
                        self.roots.insert(curr_heap_object);
                    }
                }
            }
        }

        // Remove freed objects from cycles recorded on a previous epoch.
        // Every free since the last retain is in freed_objs (free() records
        // unconditionally, covering release() cascades), and cycles are only
        // dereferenced below, after this retain. Clearing per epoch keeps
        // recycled addresses from purging fresh parkings later.
        self.cycles.retain_mut(|cycle| {
            cycle.retain(|obj| !self.freed_objs.contains(obj));
            !cycle.is_empty()
        });

        // Free any cycles from the previous epoch
        unsafe {
            self.free_cycles();
        }

        // Process cycles
        unsafe {
            self.process_cycles();
        }

        // Frees recorded during free_cycles target objects that cannot be in
        // any pending cycle; drop them now so recycled addresses never purge
        // a fresh parking in a later epoch.
        self.freed_objs.clear();

        let mut heap = HEAP.lock();
        if !self.head.is_null() {
            unsafe {
                if heap.head.is_null() {
                    heap.head = self.head;
                    heap.tail = self.tail;
                } else {
                    (*self.tail).next = heap.head;
                    (*heap.head).prev = self.tail;
                    heap.head = self.head;
                }
            }
        }

        heap.epoch += 1;
        COLLECTION_DONE_SIGNAL.notify_all();
    }

    unsafe fn decrement(&mut self, s: OpaqueGcPtr) {
        unsafe {
            if s.dec_shared_rc() == 1 && !s.buffered() {
                self.release(s);
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

    /// Process one drained entry. Shadow mode: classify and record, act on
    /// nothing. Membership ends only via the release CAS proving the word
    /// didn't change during processing; otherwise keep the claim and
    /// re-enqueue (design doc §3 "Release").
    unsafe fn process_drained(&mut self, header: *mut GcHeader) {
        unsafe {
            let word = GcState((*header).state.load(std::sync::atomic::Ordering::Acquire));

            if word.attn_dead() {
                // Finalized by the scan while claimed; we own the last
                // reference to the header memory. No live handles exist, so
                // no concurrent RMW can race this dealloc.
                let layout = (*header).layout;
                self.note_dealloc(header);
                std::alloc::dealloc(header as *mut u8, layout);
                return;
            }

            #[cfg(feature = "gc-shadow-validate")]
            self.shadow_classify(header, word);

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
                // A racing inc/dec landed mid-processing: the event must not
                // be lost. Keep the claim, re-enqueue for next epoch.
                attn_push(header);
            }
        }
    }

    #[cfg(not(feature = "gc-shadow-validate"))]
    fn note_dealloc(&mut self, _header: *mut GcHeader) {}

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
                    n.set_crc(n.epoch_rc() as isize);
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
                if delta_test(&c) && sigma_test(&c) {
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

            // Remove the object from the heap and ensure it is no longer a
            // possible root:
            let prev = s.prev();
            let next = s.next();

            if self.head == s.as_ptr() {
                self.head = next;
            }

            if self.tail == s.as_ptr() {
                self.tail = prev;
            }

            if self.next == s.as_ptr() {
                self.next = next;
            }

            if let Some(prev) = OpaqueGcPtr::from_ptr(prev) {
                prev.set_next(next);
            }

            if let Some(next) = OpaqueGcPtr::from_ptr(next) {
                next.set_prev(prev);
            }

            // self.heap.remove(&s);
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
                self.note_dealloc(s.header.as_ref().get());
                std::alloc::dealloc(s.header.as_ptr() as *mut u8, s.layout());
            }
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::gc::*;
    use parking_lot::RwLock;
    use std::sync::Arc;

    // `cycles` and `nursery_delays_reclamation_one_epoch` both force epochs via
    // `collect_garbage_sync` and inspect epoch-count-sensitive state; run them
    // serially against each other so one test's forced epoch can't land inside
    // another's collection window (see the plan's caveat on this test).
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
    fn nursery_delays_reclamation_one_epoch() {
        let _guard = GC_TEST_SERIAL.lock();
        init_gc();

        let out_ptr = Arc::new(());
        let obj = Gc::new(Some(out_ptr.clone()));
        drop(obj);

        collect_garbage_sync();
        assert_eq!(
            Arc::strong_count(&out_ptr),
            2,
            "nursery object was scanned in its first epoch"
        );

        collect_garbage_sync();
        assert_eq!(
            Arc::strong_count(&out_ptr),
            1,
            "object not reaped after nursery promotion"
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
}
