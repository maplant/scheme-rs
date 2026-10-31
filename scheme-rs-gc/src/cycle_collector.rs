#![cfg(not(loom))]

//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use std::{alloc::Layout, any::TypeId, mem::take, ptr::null_mut};

use parking_lot::Mutex;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::{
    HeapObject, MAX_ALIGN, Reclaimer, VTable,
    collector::{Collector, DropAction, INVALID_VTABLE, on_collector_thread},
    heap::{Heap, ObjectHeader},
    sync::{AtomicUsize, Ordering},
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

#[derive(Debug)]
#[repr(C, align(8))]
pub struct CycleHeader {
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
    next: *mut CycleHeader,
    /// Previous item in the heap, or null. Lower 1 bit is the buffered flag
    prev: *mut CycleHeader,
}

const _: () = assert!(size_of::<CycleHeader>().is_multiple_of(MAX_ALIGN));

unsafe impl ObjectHeader for CycleHeader {
    type HeapState = Mutex<CycleList>;
    fn layout(&self) -> Layout {
        self.layout
    }
}

impl CycleHeader {
    pub(crate) fn new(layout: Layout) -> Self {
        Self {
            shared_rc: AtomicUsize::new(1),
            epoch_rc: 1,
            crc: 1,
            vtable: &INVALID_VTABLE,
            layout,
            next: null_mut(),
            prev: null_mut::<CycleHeader>().map_addr(|addr| addr | 1),
        }
    }

    pub(crate) fn rc(&self) -> &AtomicUsize {
        &self.shared_rc
    }

    fn get_color(&self) -> Color {
        Color::from((self.next as usize & 0b111) as u8)
    }

    fn set_color(&mut self, color: Color) {
        self.next = self.get_next().map_addr(|addr| addr | color as usize);
    }

    fn get_next(&self) -> *mut CycleHeader {
        self.next.map_addr(|addr| addr & !0b111)
    }

    fn set_next(&mut self, new: *mut CycleHeader) {
        self.next = new.map_addr(|addr| addr | self.get_color() as usize);
    }

    fn get_buffered(&self) -> bool {
        (self.prev as usize & 0b1) == 1
    }

    fn set_buffered(&mut self, buffered: bool) {
        self.prev = self.get_prev().map_addr(|addr| addr | buffered as usize);
    }

    fn get_prev(&self) -> *mut CycleHeader {
        self.prev.map_addr(|addr| addr & !0b1)
    }

    fn set_prev(&mut self, new: *mut CycleHeader) {
        self.prev = new.map_addr(|addr| addr | self.get_buffered() as usize);
    }
}

/// Objects unrooted since the collector last took the list.
#[derive(Default)]
pub struct CycleList {
    head: Option<HeapObject>,
    tail: Option<HeapObject>,
}

static VTABLES: Mutex<Option<HashMap<TypeId, &'static VTable>>> = Mutex::new(None);

pub struct CycleCollector {
    heap: &'static Heap<CycleHeader>,
    reclaimer: Reclaimer<'static, CycleHeader>,
    roots: HashSet<HeapObject>,
    cycles: Vec<Vec<HeapObject>>,
    freed_objs: HashSet<HeapObject>,
    head: Option<HeapObject>,
    tail: Option<HeapObject>,
    next: Option<HeapObject>,
    /// Empty between drives; kept allocated to avoid a Vec per cascade.
    release_stack: Vec<DropAction>,
}

impl Collector for CycleCollector {
    type Header = CycleHeader;

    fn collect(&mut self) {
        self.epoch();
    }

    fn new(heap: &'static Heap<CycleHeader>) -> Self {
        Self {
            heap,
            reclaimer: heap.reclaimer(),
            roots: HashSet::default(),
            cycles: Vec::new(),
            freed_objs: HashSet::default(),
            head: None,
            tail: None,
            next: None,
            release_stack: Vec::new(),
        }
    }

    fn shutdown(&mut self) {}
}

impl CycleCollector {
    fn epoch(&mut self) {
        let mut list = self.heap.state.lock();
        self.head = take(&mut list.head);
        self.tail = take(&mut list.tail);
        drop(list);

        self.next = self.head;

        // Collect obvious garbage; i.e. heap objects that have a ref count of zero,
        // and potential candidates for cycles.
        while let Some(curr_heap_object) = self.next {
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

        self.reclaimer.sweep();

        let mut list = self.heap.state.lock();
        if let Some(tail) = self.tail {
            match list.head {
                None => {
                    list.head = self.head;
                    list.tail = self.tail;
                }
                Some(head) => unsafe {
                    tail.set_next(Some(head));
                    head.set_prev(Some(tail));
                    list.head = self.head;
                },
            }
        }
    }

    unsafe fn decrement(&mut self, s: HeapObject) {
        unsafe { self.maybe_release(DropAction::Decrement(s)) }
    }

    unsafe fn release(&mut self, s: HeapObject) {
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
    unsafe fn drop_children(&mut self, s: HeapObject) {
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

    unsafe fn free_cycle(&mut self, c: &[HeapObject]) {
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

    unsafe fn refurbish(&mut self, c: &[HeapObject]) {
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

    unsafe fn cyclic_decrement(&mut self, m: HeapObject) {
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

    unsafe fn free(&mut self, s: HeapObject) {
        unsafe {
            // Safety: No need to acquire a permit, s is guaranteed to be
            // garbage.

            // Remove the object from the heap and ensure it is no longer a
            // possible root:
            let prev = s.prev();
            let next = s.next();

            if self.head == Some(s) {
                self.head = next;
            }

            if self.tail == Some(s) {
                self.tail = prev;
            }

            if self.next == Some(s) {
                self.next = next;
            }

            if let Some(prev) = prev {
                prev.set_next(next);
            }

            if let Some(next) = next {
                next.set_prev(prev);
            }

            // self.heap.remove(&s);
            self.roots.remove(&s);

            // Record the free so the next epoch purges any entry for this
            // object from the pending cycle list before dereferencing it.
            self.freed_objs.insert(s);

            // Finalize the object:
            (s.finalize())(s.data_mut());

            self.reclaimer.free(s.header.cast());
        }
    }
}

impl HeapObject {
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
    ) -> unsafe fn(this: *const (), visitor: &mut dyn FnMut(HeapObject)) {
        unsafe { (*self.header.as_ref().get()).vtable.visit_children }
    }

    unsafe fn finalize(&self) -> unsafe fn(this: *mut ()) {
        unsafe { (*self.header.as_ref().get()).vtable.finalize }
    }

    unsafe fn next(&self) -> Option<HeapObject> {
        unsafe { HeapObject::from_header((*self.header.as_ref().get()).get_next()) }
    }

    unsafe fn set_next(&self, next: Option<HeapObject>) {
        let ptr = unsafe {
            match next {
                Some(obj) => obj.as_ptr(),
                None => null_mut(),
            }
        };
        unsafe { (*self.header.as_ref().get()).set_next(ptr) } // keeps the color bits
    }

    unsafe fn prev(&self) -> Option<HeapObject> {
        unsafe { HeapObject::from_header((*self.header.as_ref().get()).get_prev()) }
    }

    unsafe fn set_prev(&self, prev: Option<HeapObject>) {
        let ptr = unsafe {
            match prev {
                Some(obj) => obj.as_ptr(),
                None => null_mut(),
            }
        };
        unsafe { (*self.header.as_ref().get()).set_prev(ptr) };
    }
}

unsafe impl Send for CycleList {}
unsafe impl Sync for CycleList {}

/// Links a new object into the heap list.
///
/// # Safety
///
/// `obj` is allocated by the allocator in this crate.
/// It contains a Header not yet linked, and is followed by the
/// object's data, which stays valid until the collector frees it. The
/// collector may visit and free the object from its own thread. `obj.header.vtable`
/// builds the vtable for `type_id`; the first one registered for a `type_id`
/// is used for every later object of it. `obj` was allocated in `heap`.
pub unsafe fn unroot_in(
    heap: &Heap<CycleHeader>,
    obj: HeapObject,
    type_id: TypeId,
    vtable: fn() -> VTable,
) {
    let header = unsafe { obj.as_ptr() };

    let vtable = *VTABLES
        .lock()
        .get_or_insert_with(HashMap::default)
        .entry(type_id)
        // This technically doesn't have to be a leak, we could just use
        // unsafe, but this plays nicely with the rust typesystem
        .or_insert_with(|| Box::leak(Box::new(vtable())));

    let mut list = heap.state.lock();

    unsafe {
        (*header).vtable = vtable;

        match list.head {
            None => list.tail = Some(obj),
            Some(h) => h.set_prev(Some(obj)),
        }

        obj.set_next(list.head);
    }

    list.head = Some(obj);
}

/// Force a garbage collection pause.
pub fn collect_garbage_in(heap: &Heap<CycleHeader>) {
    assert!(!on_collector_thread(), "Collect garbage in gc thread.");
    heap.wait_for_epochs(1);
}

unsafe fn for_each_child(s: HeapObject, visitor: &mut dyn FnMut(HeapObject)) {
    unsafe {
        (s.visit_children())(s.data(), visitor);
    }
}

unsafe fn scan_black(s: HeapObject) {
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

unsafe fn scan(s: HeapObject) {
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
    MarkGray(HeapObject),
    SetCrc(HeapObject),
}

unsafe fn mark_gray(s: HeapObject) {
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

unsafe fn collect_white(s: HeapObject, current_cycle: &mut Vec<HeapObject>) {
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

unsafe fn sigma_test(c: &[HeapObject]) -> bool {
    unsafe {
        let mut sum = 0;
        for n in c {
            sum += n.crc();
        }
        sum == 0
    }
}

unsafe fn delta_test(c: &[HeapObject]) -> bool {
    unsafe {
        for n in c {
            if n.color() != Color::Orange {
                return false;
            }
        }
        true
    }
}
