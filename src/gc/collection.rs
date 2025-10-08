//! An implementation of the algorithm described in the paper Concurrent
//! Cycle Collection in Reference Counted Systems by David F. Bacon and
//! V.T. Rajan.

use std::{
    alloc::Layout,
    cell::UnsafeCell,
    collections::HashSet,
    marker::PhantomData,
    ptr::NonNull,
    sync::{LazyLock, Mutex, OnceLock, RwLock, atomic::AtomicUsize},
    time::Duration,
};
use tokio::{
    sync::mpsc::{UnboundedReceiver, UnboundedSender, unbounded_channel},
    task::JoinHandle,
};

use crate::gc::GcInner;

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
        unsafe { self.data.as_ref().get() as *const () }
    }

    unsafe fn data_mut(&self) -> *mut () {
        unsafe { self.data.as_ref().get() }
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

    let _ = NEW_ALLOCS_BUFFER
        .new_allocs_buffer_tx
        .send(unsafe { new_gc.as_opaque() });

    new_gc
}

struct NewAllocsBuffer {
    new_allocs_buffer_tx: UnboundedSender<HeapObject<()>>,
    new_allocs_buffer_rx: Mutex<Option<UnboundedReceiver<HeapObject<()>>>>,
}

impl NewAllocsBuffer {
    fn take_new_allocs_buffer(&self) -> UnboundedReceiver<HeapObject<()>> {
        self.new_allocs_buffer_rx.lock().unwrap().take().unwrap()
    }
}

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

static NEW_ALLOCS_BUFFER: LazyLock<NewAllocsBuffer> = LazyLock::new(NewAllocsBuffer::default);
static COLLECTOR_TASK: OnceLock<JoinHandle<()>> = OnceLock::new();

pub fn init_gc() {
    let _ = COLLECTOR_TASK.get_or_init(|| Collector::new().run());
}

#[derive(Debug)]
pub struct Collector {
    new_allocs_buffer: Option<UnboundedReceiver<OpaqueGcPtr>>,
    heap: HashSet<OpaqueGcPtr>,
    roots: HashSet<OpaqueGcPtr>,
    cycles: Vec<Vec<OpaqueGcPtr>>,
}

impl Collector {
    fn new() -> Self {
        let new_allocs_buffer = NEW_ALLOCS_BUFFER.take_new_allocs_buffer();
        Self {
            new_allocs_buffer: Some(new_allocs_buffer),
            heap: HashSet::new(),
            roots: HashSet::new(),
            cycles: Vec::new(),
        }
    }

    fn run(mut self) -> JoinHandle<()> {
        tokio::task::spawn(async move {
            while self.recv_new_allocs().await {
                tokio::task::block_in_place(|| self.epoch());
            }
        })
    }

    async fn recv_new_allocs(&mut self) -> bool {
        let mut new_allocs_buffer = self.new_allocs_buffer.take().unwrap();

        // This allows us to cleanly detect the shutdown of the Runtime without
        // panicking.
        let recv_result = tokio::task::spawn(async move {
            let to_recv = new_allocs_buffer.len();

            if to_recv == 0 {
                tokio::time::sleep(Duration::from_millis(10)).await;
            }

            // Maybe only allocate this once
            let mut recvd = Vec::new();
            new_allocs_buffer.recv_many(&mut recvd, to_recv).await;
            (new_allocs_buffer, recvd)
        })
        .await;

        match recv_result {
            Ok((new_allocs_buffer, recvd)) => {
                self.new_allocs_buffer = Some(new_allocs_buffer);

                for new_alloc in recvd.into_iter() {
                    unsafe {
                        new_alloc.set_buffered(false);
                    }
                    self.heap.insert(new_alloc);
                }

                true
            }
            Err(_) => false,
        }
    }

    fn epoch(&mut self) {
        // Collect obvious garbage; i.e. heap objects that have a ref count of zero,
        // and potential candidates for cycles.
        let mut garbage = Vec::new();

        for heap_object in self.heap.iter() {
            unsafe {
                let shared_rc = heap_object.shared_rc();
                let epoch_rc = heap_object.epoch_rc();

                if shared_rc == 0 {
                    // If shared_rc is zero, then we can release this object
                    garbage.push(*heap_object);
                } else if shared_rc > epoch_rc {
                    // If the epoch_rc is less than the shared_rc, we've seen an
                    // increment and can mark the object black.
                    heap_object.set_epoch_rc(shared_rc);
                    scan_black(*heap_object);
                } else {
                    heap_object.set_epoch_rc(shared_rc);
                    // Otherwise, we must assume that object is a possible root
                    if heap_object.color() == Color::Black {
                        scan_black(*heap_object);
                        heap_object.set_color(Color::Purple);
                        self.roots.insert(*heap_object);
                    }
                }
            }
        }

        // Free any garbage
        unsafe {
            self.free_cycles();
        }

        for garbage in garbage.into_iter() {
            unsafe {
                self.release(garbage);
            }
        }

        // Process cycles
        unsafe {
            self.process_cycles();
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
            self.heap.remove(&s);
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
        let lock = s.lock().read().unwrap();
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

    #[tokio::test]
    async fn cycles() {
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

        collector.recv_new_allocs().await;

        collector.epoch();

        drop(a);
        drop(b);
        drop(c);

        collector.epoch();
        collector.epoch();
        collector.epoch();

        assert_eq!(Arc::strong_count(&out_ptr), 1);
    }
}
