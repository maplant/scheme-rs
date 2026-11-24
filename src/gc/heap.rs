//! The process-wide heap and collector that `Gc` allocates in.

use std::{
    alloc::{Layout, handle_alloc_error},
    any::TypeId,
    cell::RefCell,
    env,
    ptr::NonNull,
    sync::{
        OnceLock,
        atomic::{AtomicUsize, Ordering},
    },
};

use parking_lot::Mutex;
use scheme_rs_gc::{
    CollectorAlreadyRunning, CollectorHandle, DefaultCollector, GcWatching, Header, Heap,
    HeapArray, HeapObject, Mutator, Trace, VTable, collect_garbage_in, on_collector_thread,
    start_collector, unroot_in, with_gc_watching,
};

use crate::{
    env::TOP_LEVEL_BINDINGS,
    gc::HANDLES,
    keywords::INTERNED,
    ports::{EOF_OBJECT, FILE_OPTIONS, LATIN_1_CODEC, UTF_8_CODEC, UTF_16_CODEC},
    records::NONGENERATIVE,
    registry::PRIMITIVES_LIB,
};

const DEFAULT_HEAP_SIZE: usize = 512 * 1024 * 1024;

static HEAP: OnceLock<Heap<Header>> = OnceLock::new();
static HEAP_SIZE: AtomicUsize = AtomicUsize::new(0);
static COLLECTOR: Mutex<Option<CollectorHandle<Header>>> = Mutex::new(None);

#[derive(Debug)]
pub struct HeapAlreadyCreated;

/// Sets the heap size in bytes. Must be called before the first `Gc`
/// allocation.
pub fn set_heap_size(bytes: usize) -> Result<(), HeapAlreadyCreated> {
    if HEAP.get().is_some() {
        return Err(HeapAlreadyCreated);
    }
    HEAP_SIZE.store(bytes, Ordering::Relaxed);
    Ok(())
}

pub(crate) fn heap() -> &'static Heap<Header> {
    HEAP.get_or_init(|| {
        let bytes = match HEAP_SIZE.load(Ordering::Relaxed) {
            0 => env::var("SCHEME_RS_HEAP_SIZE")
                .ok()
                .map(|v| {
                    v.parse()
                        .expect("SCHEME_RS_HEAP_SIZE must be a number of bytes")
                })
                .unwrap_or(DEFAULT_HEAP_SIZE),
            bytes => bytes,
        };
        let heap = Heap::new(bytes).expect("could not reserve the GC heap");
        heap.register_roots(&INTERNED);
        heap.register_roots(&TOP_LEVEL_BINDINGS);
        heap.register_roots(&*HANDLES);
        heap.register_roots(&LATIN_1_CODEC);
        heap.register_roots(&UTF_8_CODEC);
        heap.register_roots(&UTF_16_CODEC);
        heap.register_roots(&EOF_OBJECT);
        heap.register_roots(&FILE_OPTIONS);
        heap.register_roots(&NONGENERATIVE);
        heap.register_roots(&PRIMITIVES_LIB);
        heap
    })
}

/// Bytes of space in the heap
pub fn budget() -> usize {
    heap().budget()
}

/// Starts the collector thread for the global heap. Calling this function is
/// typically not required as creating a scheme-rs `Runtime` automatically
/// calls it.
///
/// There is one collector per heap: fails if a collector is already running.
pub fn init_gc() -> Result<(), CollectorAlreadyRunning> {
    let mut collector = COLLECTOR.lock();
    if collector.is_some() {
        return Err(CollectorAlreadyRunning);
    }
    *collector = Some(start_collector::<DefaultCollector>(heap())?);
    Ok(())
}

/// Stops the global heap's collector thread and waits for it to exit. Only
/// for tests: Miri requires every thread to be joined before `main` returns.
/// Objects still alive are not collected again until `init_gc` starts a new
/// collector.
///
/// Pending requests run first, so threads that keep requesting collections
/// delay the return.
pub fn shutdown_gc() {
    COLLECTOR.lock().take();
}

thread_local! {
    static MUTATOR: RefCell<Mutator<'static, Header>> = RefCell::new(heap().mutator());
}

/// # Safety
///
/// The memory returned is uninitialized.
///
/// # Panics
///
/// On the collector thread: finalizers must not allocate.
/// On exiting threads: Drop and thread-local allocators must not allocate via `alloc`
pub(crate) unsafe fn alloc_layout(layout: Layout) -> NonNull<u8> {
    assert!(!on_collector_thread(), "Gc allocated during finalization");
    with_gc_watching(|watching| {
        let try_alloc = |watching: &mut GcWatching| {
            MUTATOR
                .try_with(|m| unsafe {
                    HeapObject::alloc_layout_in(&mut m.borrow_mut(), watching, layout)
                })
                .expect("Gc allocated after this thread's GC mutator was destroyed")
        };
        if let Ok(obj) = try_alloc(watching) {
            return obj;
        } else {
            handle_alloc_error(layout)
        }
    })
}

pub(crate) fn heap_array<T: Trace + 'static, I>(items: I) -> HeapArray<T>
where
    I: IntoIterator<Item = T>,
    I::IntoIter: ExactSizeIterator,
{
    let items = items.into_iter();
    let data = unsafe { alloc_layout(HeapArray::<T>::layout(items.len())) };
    unsafe { HeapArray::init_in(heap(), data, items) }
}

pub(crate) fn heap_array_with_len<T: Trace + 'static, I>(len: usize, items: I) -> HeapArray<T>
where
    I: Iterator<Item = T>,
{
    let data = unsafe { alloc_layout(HeapArray::<T>::layout(len)) };
    unsafe { HeapArray::init_in_with_len(heap(), data, items, len) }
}

pub(crate) fn alloc<T>(data: T) -> NonNull<T> {
    let ptr = unsafe { alloc_layout(Layout::new::<T>()) }.cast::<T>();
    unsafe { ptr.write(data) };
    ptr
}

/// # Safety
///
/// The return value is valid only if all zeros is a valid bit pattern for T,
/// otherwise it is invalid to deref.
pub(crate) unsafe fn alloc_zeroed<T>() -> NonNull<T> {
    let ptr = unsafe { alloc_layout(Layout::new::<T>()) }.cast::<T>();
    unsafe { ptr.write_bytes(0, 1) };
    ptr
}

/// # Safety
///
/// See `scheme_rs_gc::unroot_in`; `obj` was allocated with `alloc`.
pub(crate) unsafe fn unroot_obj(obj: HeapObject, type_id: TypeId, vtable: fn() -> VTable) {
    unsafe { unroot_in(heap(), obj, type_id, vtable) }
}

pub(crate) fn collect_garbage_sync() {
    assert!(
        COLLECTOR.lock().is_some(),
        "Collect garbage called without running gc."
    );
    collect_garbage_in(heap());
}
