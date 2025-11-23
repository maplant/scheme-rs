//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use parking_lot::RwLock;
use std::{
    alloc::Layout,
    cell::UnsafeCell,
    marker::PhantomData,
    mem::offset_of,
    ptr::{NonNull, null_mut},
    sync::{
        OnceLock,
        atomic::{AtomicPtr, AtomicUsize, Ordering},
    },
    thread::JoinHandle,
};

use rustc_hash::FxHashSet as HashSet;

use crate::gc::GcInner;

#[derive(Debug)]
#[repr(C)]
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
    /// Type-erased visitor function
    visit_children: unsafe fn(this: *const (), visitor: &mut dyn FnMut(HeapObject<()>)),
    /// Type-erased finalizer function
    finalize: unsafe fn(this: *mut ()),
    /// Layout for the underlying data
    layout: Layout,
    /// Offset into the underlying data
    offset: usize,
    /// Next item in the heap, or null
    next: *mut GcHeader,
    /// Previous item in the heap, or null
    prev: *mut GcHeader,
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
            visit_children: |this, visitor| unsafe {
                let this = this as *const UnsafeCell<T> as *const T;
                T::visit_or_recurse(this.as_ref().unwrap(), visitor);
            },
            finalize: |this| unsafe {
                let this = this as *mut T;
                T::finalize_or_skip(this.as_mut().unwrap());
            },
            layout: Layout::new::<super::GcInner<T>>(),
            offset: offset_of!(super::GcInner<T>, data),
            next: null_mut(),
            prev: null_mut(),
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
        write!(f, "{:p}", self.header.as_ptr())
    }
}

pub type OpaqueGcPtr = HeapObject<()>;

impl HeapObject<()> {
    unsafe fn from_ptr(ptr: *mut GcHeader) -> Option<Self> {
        if ptr.is_null() {
            return None;
        }

        let header = NonNull::new(ptr as *mut UnsafeCell<GcHeader>).unwrap();
        let header_offset = unsafe { (*header.as_ref().get()).offset };
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

pub(super) fn alloc_gc_object<T: super::GcOrTrace>(data: T) -> super::Gc<T> {
    let new_gc = super::Gc {
        ptr: NonNull::from(Box::leak(Box::new(GcInner {
            header: UnsafeCell::new(GcHeader::new::<T>()),
            data: UnsafeCell::new(data),
        }))),
        marker: PhantomData,
    };

    HEAP_START
        .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |next| unsafe {
            let new_gc_ptr = new_gc.ptr.as_ptr() as *mut GcHeader;
            (*new_gc_ptr).next = next;
            Some(new_gc_ptr)
        })
        .unwrap();

    new_gc
}

static HEAP_START: AtomicPtr<GcHeader> = AtomicPtr::new(std::ptr::null_mut());
static COLLECTOR_TASK: OnceLock<JoinHandle<()>> = OnceLock::new();

pub fn init_gc() {
    let _ = COLLECTOR_TASK.get_or_init(|| Collector::new().run());
}

#[derive(Debug)]
pub struct Collector {
    roots: HashSet<OpaqueGcPtr>,
    cycles: Vec<Vec<OpaqueGcPtr>>,
    start: *mut GcHeader,
    next: *mut GcHeader,
}

unsafe impl Send for Collector {}

impl Collector {
    fn new() -> Self {
        Self {
            roots: HashSet::default(),
            cycles: Vec::new(),
            start: null_mut(),
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

    fn epoch(&mut self) {
        self.start = HEAP_START
            .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |_| Some(null_mut()))
            .unwrap();

        // Go through and set the prev ptr for all of the new items:
        let mut prev = null_mut();
        let mut next = self.start;
        let mut new_live_objects = 0;
        while let Some(curr_heap_object) = unsafe { OpaqueGcPtr::from_ptr(next) } {
            unsafe {
                // curr_heap_object.set_prev(prev);
                if !curr_heap_object.prev().is_null() {
                    break;
                }

                new_live_objects += 1;

                curr_heap_object.set_prev(prev);
                prev = next;
                next = curr_heap_object.next();
            }
        }

        if new_live_objects == 1 {
            std::thread::yield_now();
        }

        self.next = self.start;

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

        // Free any cycles from the previous epoch
        unsafe {
            self.free_cycles();
        }

        // Process cycles
        unsafe {
            self.process_cycles();
        }

        // Add the heap back into HEAP_START, unless we freed everything:
        if self.start.is_null() {
            return;
        }

        if let Err(mut curr_ptr) =
            HEAP_START.fetch_update(Ordering::SeqCst, Ordering::SeqCst, |new_start| {
                new_start.is_null().then_some(self.start)
            })
        {
            // We have a new start, append start to the end of the linked list
            loop {
                let curr = unsafe { OpaqueGcPtr::from_ptr(curr_ptr) }.unwrap();
                let next = unsafe { curr.next() };
                if next.is_null() {
                    if let Some(start) = unsafe { OpaqueGcPtr::from_ptr(self.start) } {
                        unsafe { start.set_prev(curr_ptr) };
                    }
                    unsafe { curr.set_next(self.start) };
                    return;
                }
                curr_ptr = next;
            }
        }
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

            if self.start == s.as_ptr() {
                self.start = next;
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

            // Finalize the object:
            (s.finalize())(s.data_mut());

            // Deallocate the object:
            std::alloc::dealloc(s.header.as_ptr() as *mut u8, s.layout());
        }
    }
}

unsafe fn for_each_child(s: OpaqueGcPtr, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
    unsafe {
        let lock = s.lock().read();
        (s.visit_children())(s.data(), visitor);
        drop(lock);
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
    use std::sync::Arc;

    #[test]
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

        let mut collector = Collector::new();

        collector.epoch();

        drop(a);
        drop(b);
        drop(c);

        collector.epoch();
        collector.epoch();

        assert_eq!(Arc::strong_count(&out_ptr), 1);
    }
}
