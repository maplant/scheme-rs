//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use std::{
    alloc::Layout,
    any::TypeId,
    cell::UnsafeCell,
    ptr::{NonNull, null_mut},
    sync::{OnceLock, atomic::AtomicUsize},
    thread::JoinHandle,
};

use parking_lot::{Condvar, Mutex};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{exceptions::Exception, registry::bridge, value::Value};

#[derive(Debug)]
#[repr(C, align(8))]
pub(crate) struct GcHeader {
    /// Reference count shared with the Gc types
    pub(crate) shared_rc: AtomicUsize,
    /// Reference count as of the current epoch
    epoch_rc: usize,
    /// Circular reference count
    crc: isize,
    /// VTable for the type
    vtable: &'static VTable,
    /// Layout of the type and header
    layout: Layout,
    /// Next item in the heap, or null. Lower 3 bits are the color
    next: *mut GcHeader,
    /// Previous item in the heap, or null. Lower 1 bit is the buffered flag
    prev: *mut GcHeader,
}

#[bridge(name = "gc-header-size", lib = "(runtime (1))")]
pub fn gc_header_size() -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(std::mem::size_of::<GcHeader>())])
}

impl GcHeader {
    pub(crate) fn new<T: super::GcOrTrace>() -> Self {
        Self {
            shared_rc: AtomicUsize::new(1),
            epoch_rc: 1,
            crc: 1,
            vtable: &INVALID_VTABLE,
            layout: Layout::new::<super::GcInner<T>>(),
            next: null_mut(),
            prev: null_mut::<GcHeader>().map_addr(|addr| addr | 1),
        }
    }

    fn get_color(&self) -> Color {
        Color::from((self.next as usize & 0b111) as u8)
    }

    fn set_color(&mut self, color: Color) {
        self.next = self.get_next().map_addr(|addr| addr | color as usize);
    }

    fn get_next(&self) -> *mut GcHeader {
        self.next.map_addr(|addr| addr & !0b111)
    }

    fn set_next(&mut self, new: *mut GcHeader) {
        self.next = new.map_addr(|addr| addr | self.get_color() as usize);
    }

    fn get_buffered(&self) -> bool {
        (self.prev as usize & 0b1) == 1
    }

    fn set_buffered(&mut self, buffered: bool) {
        self.prev = self.get_prev().map_addr(|addr| addr | buffered as usize);
    }

    fn get_prev(&self) -> *mut GcHeader {
        self.prev.map_addr(|addr| addr & !0b1)
    }

    fn set_prev(&mut self, new: *mut GcHeader) {
        self.prev = new.map_addr(|addr| addr | self.get_buffered() as usize);
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

static INVALID_VTABLE: VTable = VTable {
    visit_children: |_, _| unreachable!(),
    finalize: |_| unreachable!(),
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
enum Color {
    /// In use or free
    Black = 0,
    /// Possible member of a cycle
    Gray = 1,
    /// Member of a garbage cycle
    White = 2,
    /// Possible root of cycle
    Purple = 3,
    /// Candidate cycle undergoing Σ-computation
    Red = 4,
    /// Candidate cycle awaiting epoch boundary
    Orange = 5,
}

impl From<u8> for Color {
    fn from(value: u8) -> Self {
        match value {
            0 => Self::Black,
            1 => Self::Gray,
            2 => Self::White,
            3 => Self::Purple,
            4 => Self::Red,
            5 => Self::Orange,
            _ => unreachable!(),
        }
    }
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
        unsafe { (*self.header.as_ref().get()).get_color() }
    }

    unsafe fn set_color(&self, color: Color) {
        unsafe {
            (*self.header.as_ref().get()).set_color(color);
        }
    }

    unsafe fn buffered(&self) -> bool {
        unsafe { (*self.header.as_ref().get()).get_buffered() }
    }

    unsafe fn set_buffered(&self, buffered: bool) {
        unsafe {
            (*self.header.as_ref().get()).set_buffered(buffered);
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
        unsafe { (*self.header.as_ref().get()).get_next() }
    }

    unsafe fn set_next(&self, next: *mut GcHeader) {
        unsafe {
            (*self.header.as_ref().get()).set_next(next);
        }
    }

    unsafe fn prev(&self) -> *mut GcHeader {
        unsafe { (*self.header.as_ref().get()).get_prev() }
    }

    unsafe fn set_prev(&self, prev: *mut GcHeader) {
        unsafe {
            (*self.header.as_ref().get()).set_prev(prev);
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
        let vtable = heap
            .vtables
            .get_or_insert_with(HashMap::default)
            .entry(TypeId::of::<T>())
            // This technically doesn't have to be a leak, we could just use
            // unsafe, but this plays nicely with the rust typesystem
            .or_insert_with(|| Box::leak(Box::new(VTable::new::<T>())));

        (*new_gc_ptr).vtable = vtable;
        (*new_gc_ptr).layout = layout;

        if heap.head.is_null() {
            heap.tail = new_gc_ptr;
        } else {
            (*heap.head).set_prev(new_gc_ptr);
        }

        (*new_gc_ptr).set_next(heap.head);
    }

    heap.head = new_gc_ptr;
    heap.new_allocs += 1;

    if heap.should_collect() {
        COLLECTION_START_SIGNAL.notify_one();
    }
}

struct Heap {
    head: *mut GcHeader,
    tail: *mut GcHeader,
    new_allocs: usize,
    epoch: usize,
    force_collection: bool,
    vtables: Option<HashMap<TypeId, &'static VTable>>,
}

impl Heap {
    const fn new() -> Self {
        Self {
            head: std::ptr::null_mut(),
            tail: std::ptr::null_mut(),
            new_allocs: 0,
            epoch: 0,
            force_collection: false,
            vtables: None,
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
    /// Empty between drives; kept allocated to avoid a Vec per cascade.
    release_stack: Vec<ReleaseFrame>,
}

#[derive(Debug)]
enum ReleaseFrame {
    Decrement(OpaqueGcPtr),
    Release(OpaqueGcPtr),
    Free(OpaqueGcPtr),
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
            release_stack: Vec::new(),
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

        self.head = std::mem::take(&mut heap.head);
        self.tail = std::mem::take(&mut heap.tail);
        heap.new_allocs = 0;
        heap.force_collection = false;
    }

    fn epoch(&mut self) {
        self.await_epoch();

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
                    (*self.tail).set_next(heap.head);
                    (*heap.head).set_prev(self.tail);
                    heap.head = self.head;
                }
            }
        }

        heap.epoch += 1;
        COLLECTION_DONE_SIGNAL.notify_all();
    }

    unsafe fn decrement(&mut self, s: OpaqueGcPtr) {
        unsafe { self.drive_release(ReleaseFrame::Decrement(s)) }
    }

    unsafe fn release(&mut self, s: OpaqueGcPtr) {
        unsafe { self.drive_release(ReleaseFrame::Release(s)) }
    }

    /// Iterative decrement/release cascade; the recursive form overflowed the
    /// collector's 2 MiB stack on deep chains.
    unsafe fn drive_release(&mut self, first: ReleaseFrame) {
        let mut stack = std::mem::take(&mut self.release_stack);
        debug_assert!(stack.is_empty());
        stack.push(first);
        while let Some(frame) = stack.pop() {
            unsafe {
                match frame {
                    ReleaseFrame::Decrement(s) => {
                        if s.dec_shared_rc() == 1 && !s.buffered() {
                            push_release_frames(&mut stack, s);
                        }
                    }
                    ReleaseFrame::Release(s) => push_release_frames(&mut stack, s),
                    ReleaseFrame::Free(s) => {
                        s.set_color(Color::Black);
                        self.free(s);
                    }
                }
            }
        }
        self.release_stack = stack;
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

            // Deallocate the object:
            std::alloc::dealloc(s.header.as_ptr() as *mut u8, s.layout());
        }
    }
}

/// Frame order reproduces the recursion exactly, and both halves matter: `Free`
/// goes below the children so a node finalizes only after its subtree, and the
/// child slice is reversed so siblings run in visit order.
unsafe fn push_release_frames(stack: &mut Vec<ReleaseFrame>, s: OpaqueGcPtr) {
    unsafe {
        stack.push(ReleaseFrame::Free(s));
        let children = stack.len();
        for_each_child(s, &mut |c| stack.push(ReleaseFrame::Decrement(c)));
        stack[children..].reverse();
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

    #[test]
    fn cycles() {
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

    static FINALIZE_ORDER: Mutex<Vec<usize>> = Mutex::new(Vec::new());

    struct Tag(usize);

    unsafe impl Trace for Tag {
        unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {}

        unsafe fn finalize(&mut self) {
            FINALIZE_ORDER.lock().push(self.0);
        }
    }

    #[derive(Trace)]
    struct TreeNode {
        tag: Tag,
        kids: Vec<Gc<RwLock<TreeNode>>>,
    }

    fn tree_node(tag: usize, kids: Vec<Gc<RwLock<TreeNode>>>) -> Gc<RwLock<TreeNode>> {
        Gc::new(RwLock::new(TreeNode {
            tag: Tag(tag),
            kids,
        }))
    }

    /// The expected order is not arbitrary: it is what the recursive cascade
    /// produced, so this pins pre-existing behaviour rather than the rewrite.
    #[test]
    fn release_cascade_finalizes_depth_first_in_visit_order() {
        init_gc();

        // 0 -> (1 -> (3, 4), 2 -> 5)
        let root = tree_node(
            0,
            vec![
                tree_node(1, vec![tree_node(3, Vec::new()), tree_node(4, Vec::new())]),
                tree_node(2, vec![tree_node(5, Vec::new())]),
            ],
        );

        collect_garbage_sync();
        collect_garbage_sync();
        FINALIZE_ORDER.lock().clear();

        drop(root);
        collect_garbage_sync();
        collect_garbage_sync();

        assert_eq!(*FINALIZE_ORDER.lock(), vec![3, 4, 1, 5, 2, 0]);
    }

    /// Deep enough to exhaust the collector's 2 MiB stack in any build profile.
    const DEEP: usize = 200_000;

    #[derive(Default, Trace)]
    struct DeepNode {
        next: Option<Gc<RwLock<DeepNode>>>,
        chain: Option<Gc<RwLock<DeepNode>>>,
        out: Option<Arc<()>>,
    }

    fn deep_chain(len: usize, out: &Arc<()>) -> Gc<RwLock<DeepNode>> {
        let mut head = Gc::new(RwLock::new(DeepNode {
            out: Some(out.clone()),
            ..Default::default()
        }));
        for _ in 1..len {
            head = Gc::new(RwLock::new(DeepNode {
                next: Some(head),
                ..Default::default()
            }));
        }
        head
    }

    #[test]
    fn deep_chain_drop_releases_fully() {
        init_gc();

        let out = Arc::new(());
        let head = deep_chain(DEEP, &out);
        assert_eq!(Arc::strong_count(&out), 2);

        // Load-bearing, not defensive: until every node has been through an
        // epoch its buffered flag stops the cascade after one node, and the
        // drop below would not recurse deeply enough to fail pre-fix.
        collect_garbage_sync();
        collect_garbage_sync();

        drop(head);
        collect_garbage_sync();
        collect_garbage_sync();
        assert_eq!(Arc::strong_count(&out), 1, "deep chain not fully released");
    }

    /// Covers the other entry point: `free_cycle` reaching the cascade through
    /// `cyclic_decrement`, rather than the rc-zero sweep.
    #[test]
    fn deep_chain_behind_freed_cycle_releases_fully() {
        init_gc();

        let out = Arc::new(());
        let chain = deep_chain(DEEP, &out);
        // Load-bearing, as above.
        collect_garbage_sync();
        collect_garbage_sync();

        let a = Gc::new(RwLock::new(DeepNode::default()));
        let b = Gc::new(RwLock::new(DeepNode::default()));
        a.write().next = Some(b.clone());
        b.write().next = Some(a.clone());
        b.write().chain = Some(chain.clone());

        drop(a);
        drop(b);
        // Trial-deletes the a/b cycle.
        collect_garbage_sync();

        // Frees a/b, whose chain edge then decrements the head to zero.
        drop(chain);
        collect_garbage_sync();
        collect_garbage_sync();
        assert_eq!(
            Arc::strong_count(&out),
            1,
            "chain behind freed cycle not released"
        );
    }
}
