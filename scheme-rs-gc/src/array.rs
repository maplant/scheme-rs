use std::{
    alloc::Layout, any::TypeId, marker::PhantomData, ops::Deref, ptr::NonNull, slice,
    sync::atomic::Ordering,
};

use allocator_api2::alloc::AllocError;

use crate::{GcWatching, Header, Heap, HeapObject, Mutator, SlotValue, Trace, VTable, unroot_in};

#[repr(C)]
struct HeapArrayInner<T> {
    len: usize,
    data: [T; 0],
}

impl<T> HeapArrayInner<T> {
    fn layout(len: usize) -> Layout {
        Layout::new::<Self>()
            .extend(Layout::array::<T>(len).unwrap())
            .unwrap()
            .0
            .pad_to_align()
    }

    unsafe fn as_slice<'a>(this: *const Self) -> &'a [T] {
        unsafe {
            let len = (*this).len;
            let data = (&raw const (*this).data).cast::<T>();
            slice::from_raw_parts(data, len)
        }
    }

    unsafe fn as_mut_slice<'a>(this: *mut Self) -> &'a mut [T] {
        unsafe {
            let len = (*this).len;
            let data = (&raw mut (*this).data).cast::<T>();
            slice::from_raw_parts_mut(data, len)
        }
    }
}

pub struct HeapArray<T>(HeapObject, PhantomData<T>);

unsafe impl<T> SlotValue for HeapArray<T> {
    fn object(&self) -> Option<HeapObject> {
        Some(self.0)
    }
}

// These currently assume a header layout, calling rc on the cycle header, but HeapObject should expose RC functions instead.
impl<T> Clone for HeapArray<T> {
    fn clone(&self) -> Self {
        unsafe { self.0.rc().fetch_add(1, Ordering::Relaxed) };
        Self(self.0, PhantomData)
    }
}

impl<T> Drop for HeapArray<T> {
    fn drop(&mut self) {
        unsafe { self.0.rc().fetch_sub(1, Ordering::Relaxed) };
    }
}

impl<T: Trace> HeapArray<T> {
    pub fn layout(len: usize) -> Layout {
        HeapArrayInner::<T>::layout(len)
    }

    /// # Safety
    /// `data` came from `alloc_layout_in` with `Self::layout(len)`, in `heap`.
    pub unsafe fn init_in<I>(heap: &Heap<Header>, data: NonNull<u8>, items: I) -> Self
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        let it = items.into_iter();
        let len = it.len();
        unsafe { HeapArray::init_in_with_len(heap, data, it, len) }
    }

    /// # Safety
    /// `data` came from `alloc_layout_in` with `Self::layout(len)`, in `heap`.
    pub unsafe fn init_in_with_len<I>(
        heap: &Heap<Header>,
        data: NonNull<u8>,
        items: I,
        len: usize,
    ) -> Self
    where
        I: Iterator<Item = T>,
    {
        unsafe {
            let inner = data.cast::<HeapArrayInner<T>>();
            (&raw mut (*inner.as_ptr()).len).write(len);
            let data_ptr = (&raw mut (*inner.as_ptr()).data).cast::<T>();

            let mut written = 0;
            for item in items.take(len) {
                data_ptr.add(written).write(item);
                written += 1;
            }

            assert_eq!(
                written, len,
                "Iterator gave different amount of items than specified by len"
            );

            let obj = HeapObject::from_data(inner);
            unroot_in(
                // TODO: Move unrooting to a generic API.
                // This is specific to the cycle collector, but registering/unrooting is a generic collector concern.
                heap,
                obj,
                TypeId::of::<HeapArrayInner<T>>(),
                Self::vtable,
            );

            Self(obj, PhantomData)
        }
    }

    pub fn from_iter_in<I>(
        m: &mut Mutator<'_, Header>,
        watching: &mut GcWatching,
        items: I,
    ) -> Result<Self, AllocError>
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        let items = items.into_iter();
        let data = unsafe { HeapObject::alloc_layout_in(m, watching, Self::layout(items.len()))? };
        Ok(unsafe { Self::init_in(m.heap, data, items) })
    }

    fn as_slice(&self) -> &[T] {
        unsafe { HeapArrayInner::as_slice(self.0.data().cast::<HeapArrayInner<T>>()) }
    }

    fn vtable() -> VTable {
        VTable {
            visit_children: |this, visitor| unsafe {
                for e in HeapArrayInner::<T>::as_slice(this.cast()) {
                    e.visit_children(visitor);
                }
            },
            finalize: |this| unsafe {
                for e in HeapArrayInner::<T>::as_mut_slice(this.cast()) {
                    e.finalize();
                }
            },
        }
    }
}

unsafe impl<T: Trace> Trace for HeapArray<T> {
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        visitor(self.0)
    }

    unsafe fn finalize(&mut self) {}
}

impl<T: Trace> Deref for HeapArray<T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        self.as_slice()
    }
}

impl<'a, T: Trace> IntoIterator for &'a HeapArray<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.as_slice().iter()
    }
}

#[cfg(test)]
mod array_tests {
    use std::sync::atomic::{AtomicUsize, Ordering};

    use crate::{BLOCK_SIZE, CycleHeader, Heap, HeapArray, HeapObject, Trace, with_gc_watching};

    #[test]
    pub fn tracing_arrays_visits_every_slot() {
        let heap = Box::leak(Box::new(Heap::<CycleHeader>::new(4 * BLOCK_SIZE).unwrap()));
        let mut m = heap.mutator();
        let (expected, parent) = with_gc_watching(|w| {
            let children: Vec<_> = (0..5)
                .map(|_| HeapArray::<usize>::from_iter_in(&mut m, w, []).unwrap())
                .collect();
            let expected: Vec<HeapObject> = children.iter().map(|c| c.0).collect();
            (expected, HeapArray::from_iter_in(&mut m, w, children).unwrap())
        });

        let mut seen = Vec::new();
        unsafe {
            (HeapArray::<HeapArray<usize>>::vtable().visit_children)(
                parent.0.data().cast(),
                &mut |o| seen.push(o),
            );
        }
        assert_eq!(seen, expected);
    }

    static FINALIZED: AtomicUsize = AtomicUsize::new(0);
    struct Counted;
    unsafe impl Trace for Counted {
        unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {}
        unsafe fn finalize(&mut self) {
            FINALIZED.fetch_add(1, Ordering::Relaxed);
        }
    }

    #[test]
    pub fn finalizing_array_finalizes_each_child() {
        let heap = Box::leak(Box::new(Heap::<CycleHeader>::new(4 * BLOCK_SIZE).unwrap()));
        let mut m = heap.mutator();
        let array = with_gc_watching(|w| {
            HeapArray::from_iter_in(&mut m, w, (0..5).map(|_| Counted)).unwrap()
        });

        unsafe {
            (HeapArray::<Counted>::vtable().finalize)(array.0.data_mut().cast());
        }
        assert_eq!(FINALIZED.load(Ordering::Relaxed), 5);
    }
}
