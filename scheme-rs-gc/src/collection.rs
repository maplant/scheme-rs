//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use std::{
    alloc::{Layout, dealloc},
    any::TypeId,
    cell::UnsafeCell,
    fmt::{self, Debug, Formatter},
    mem::take,
    ptr::{NonNull, null_mut},
    sync::{
        OnceLock,
        atomic::{AtomicUsize, Ordering},
    },
    thread::{JoinHandle, spawn},
};

use parking_lot::{Condvar, Mutex};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

#[derive(Debug)]
#[repr(C, align(8))]
pub struct GcHeader {
    /// Reference count shared with the Gc types
    shared_rc: AtomicUsize,
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

impl GcHeader {
    pub fn new(layout: Layout) -> Self {
        Self {
            shared_rc: AtomicUsize::new(1),
            epoch_rc: 1,
            crc: 1,
            vtable: &INVALID_VTABLE,
            layout,
            next: null_mut(),
            prev: null_mut::<GcHeader>().map_addr(|addr| addr | 1),
        }
    }

    #[inline]
    pub fn shared_rc(&self) -> &AtomicUsize {
        &self.shared_rc
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

#[derive(Debug)]
pub struct VTable {
    /// Type-erased visitor function
    pub visit_children: unsafe fn(this: *const (), visitor: &mut dyn FnMut(HeapObject<()>)),
    /// Type-erased finalizer function
    pub finalize: unsafe fn(this: *mut ()),
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
    header: NonNull<UnsafeCell<GcHeader>>,
    /// Allocated data
    data: NonNull<UnsafeCell<T>>,
}

impl Debug for OpaqueGcPtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.header.as_ptr())
    }
}

#[doc(hidden)]
pub type OpaqueGcPtr = HeapObject<()>;

impl HeapObject<()> {
    /// # Safety
    ///
    /// `header` and `data` belong to one object whose header was made by
    /// `GcHeader::new`.
    pub unsafe fn new(
        header: NonNull<UnsafeCell<GcHeader>>,
        data: NonNull<UnsafeCell<()>>,
    ) -> Self {
        Self { header, data }
    }

    /// # Safety
    ///
    /// The object is live.
    pub unsafe fn rc(&self) -> &AtomicUsize {
        unsafe { &(*self.header.as_ref().get()).shared_rc }
    }

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
                .load(Ordering::Acquire)
        }
    }

    unsafe fn dec_shared_rc(&self) -> usize {
        unsafe {
            (*self.header.as_ref().get())
                .shared_rc
                .fetch_sub(1, Ordering::Release)
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

    /// # Safety
    ///
    /// The object is live.
    pub unsafe fn data(&self) -> *const () {
        self.data.as_ptr() as *const UnsafeCell<()> as *const ()
    }

    /// # Safety
    ///
    /// The object is live.
    pub unsafe fn data_mut(&self) -> *mut () {
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

/// Links a new object into the heap list.
///
/// # Safety
///
/// `header` starts an allocation made by `std::alloc` with exactly `layout`,
/// holds a live `GcHeader` not yet linked, and is followed by the object's
/// data, which stays valid until the collector frees it. The collector may
/// visit and free the object from its own thread. `vtable` builds the vtable
/// for `type_id`; the first one registered for a `type_id` is used for every
/// later object of it.
#[inline]
pub unsafe fn unroot(
    header: NonNull<GcHeader>,
    type_id: TypeId,
    vtable: fn() -> VTable,
    layout: Layout,
) {
    let new_gc_ptr = header.as_ptr();

    let mut heap = HEAP.lock();

    unsafe {
        let vtable = heap
            .vtables
            .get_or_insert_with(HashMap::default)
            .entry(type_id)
            // This technically doesn't have to be a leak, we could just use
            // unsafe, but this plays nicely with the rust typesystem
            .or_insert_with(|| Box::leak(Box::new(vtable())));

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
            head: null_mut(),
            tail: null_mut(),
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
/// not required as creating a scheme-rs `Runtime` automatically calls it.
///
/// Calling this function multiple times does nothing, there is only one
/// collector thread allowed at a time.
pub fn init_gc() {
    let _ = COLLECTOR_TASK.get_or_init(|| Collector::new().run());
}

/// Force a garbage collection pause.
pub fn collect_garbage() {
    let mut heap = HEAP.lock();
    let target_epoch = heap.epoch + 1;
    heap.force_collection = true;
    COLLECTION_START_SIGNAL.notify_one();
    COLLECTION_DONE_SIGNAL.wait_while(&mut heap, |heap| heap.epoch < target_epoch);
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
    release_stack: Vec<DropAction>,
}

#[derive(Debug)]
enum DropAction {
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
        spawn(move || {
            loop {
                self.epoch();
            }
        })
    }

    fn await_epoch(&mut self) {
        let mut heap = HEAP.lock();

        COLLECTION_START_SIGNAL.wait_while(&mut heap, Heap::should_not_collect);

        self.head = take(&mut heap.head);
        self.tail = take(&mut heap.tail);
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
        unsafe { self.maybe_release(DropAction::Decrement(s)) }
    }

    unsafe fn release(&mut self, s: OpaqueGcPtr) {
        unsafe { self.maybe_release(DropAction::Release(s)) }
    }

    unsafe fn maybe_release(&mut self, first: DropAction) {
        self.release_stack.push(first);
        while let Some(action) = self.release_stack.pop() {
            unsafe {
                match action {
                    DropAction::Decrement(s) => {
                        if s.dec_shared_rc() == 1 && !s.buffered() {
                            self.drop_children(s);
                        }
                    }
                    DropAction::Release(s) => self.drop_children(s),
                    DropAction::Free(s) => {
                        s.set_color(Color::Black);
                        self.free(s);
                    }
                }
            }
        }
    }

    /// `Free` goes below the children so a node finalizes only after its
    /// subtree.
    unsafe fn drop_children(&mut self, s: OpaqueGcPtr) {
        unsafe {
            self.release_stack.push(DropAction::Free(s));
            let stack = &mut self.release_stack;
            for_each_child(s, &mut |c| stack.push(DropAction::Decrement(c)));
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
            for c in take(&mut self.cycles).into_iter().rev() {
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
            dealloc(s.header.as_ptr() as *mut u8, s.layout());
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
