//! The `Trace` trait and its impls for std and third-party types.

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    mem::ManuallyDrop,
    ops::Deref,
    path::PathBuf,
    ptr::drop_in_place,
};

use crate::HeapObject;

/// A type that can be traced for garbage collection. Types that implement this
/// trait can be converted into a `Gc` for automatic garbage collection.
///
/// # Safety
///
/// This trait should _not_ be manually implemented! Instead, use the
/// `Trace` derive macro from scheme-rs.
pub unsafe trait Trace: 'static {
    /// # Safety
    ///
    /// This function may _ONLY_ be called by the garbage collector! Calling this
    /// function **ANYWHERE ELSE** is a **RACE CONDITION**!
    ///
    /// **DO NOT CALL THIS FUNCTION!!**
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject));

    /// # Safety
    ///
    /// This function may _ONLY_ be called by the garbage collector! Calling this
    /// function **ANYWHERE ELSE** is a **RACE CONDITION**!
    ///
    /// **DO NOT CALL THIS FUNCTION!!**
    unsafe fn finalize(&mut self);
}

macro_rules! impl_empty_trace {
    ( $( $x:ty ),* ) => {
        $(
            unsafe impl Trace for $x {
                unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {}

                unsafe fn finalize(&mut self) {
                    unsafe {
                        drop_in_place(self as *mut Self);
                    }
                }
            }
        )*
    }
}

impl_empty_trace! {
    (),
    bool,
    char,
    f32,
    f64,
    // fn
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    // pointer
    // reference
    // slice,
    // tuple
    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
    &'static str,
    String,
    PathBuf
}

// Function pointer impls:
unsafe impl<A, B> Trace for fn(A) -> B
where
    A: ?Sized + 'static,
    B: ?Sized + 'static,
{
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {}

    unsafe fn finalize(&mut self) {
        unsafe {
            drop_in_place(self as *mut Self);
        }
    }
}

/// # Safety
///
/// This function is _not_ safe to implement!
pub unsafe trait GcOrTrace: 'static {
    /// # Safety
    ///
    /// This function may _ONLY_ be called by the garbage collector!
    unsafe fn visit_or_recurse(&self, visitor: &mut dyn FnMut(HeapObject));

    /// # Safety
    ///
    /// This function may _ONLY_ be called by the garbage collector!
    unsafe fn finalize_or_skip(&mut self);
}

unsafe impl<T: Trace + ?Sized> GcOrTrace for T {
    unsafe fn visit_or_recurse(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            self.visit_children(visitor);
        }
    }

    unsafe fn finalize_or_skip(&mut self) {
        unsafe {
            self.finalize();
        }
    }
}

unsafe impl<A, B> Trace for (A, B)
where
    A: GcOrTrace,
    B: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            self.0.visit_or_recurse(visitor);
            self.1.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.0.finalize_or_skip();
            self.1.finalize_or_skip();
        }
    }
}

unsafe impl<A, B, C> Trace for (A, B, C)
where
    A: GcOrTrace,
    B: GcOrTrace,
    C: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            self.0.visit_or_recurse(visitor);
            self.1.visit_or_recurse(visitor);
            self.2.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.0.finalize_or_skip();
            self.1.finalize_or_skip();
            self.2.finalize_or_skip();
        }
    }
}

unsafe impl<T, const N: usize> Trace for [T; N]
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            for item in self {
                item.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for item in self {
                item.finalize_or_skip();
            }
        }
    }
}

unsafe impl<T> Trace for Vec<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            for child in self {
                child.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for mut child in std::mem::take(self).into_iter().map(ManuallyDrop::new) {
                child.finalize_or_skip();
            }
        }
    }
}

unsafe impl<K, S> Trace for HashSet<K, S>
where
    K: GcOrTrace,
    S: Default + 'static,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            for k in self {
                k.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for mut k in std::mem::take(self).into_iter().map(ManuallyDrop::new) {
                k.finalize_or_skip();
            }
        }
    }
}

unsafe impl<K, V, S> Trace for HashMap<K, V, S>
where
    K: GcOrTrace,
    V: GcOrTrace,
    S: Default + 'static,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            for (k, v) in self {
                k.visit_or_recurse(visitor);
                v.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for (k, v) in std::mem::take(self) {
                let mut k = ManuallyDrop::new(k);
                let mut v = ManuallyDrop::new(v);
                k.finalize_or_skip();
                v.finalize_or_skip();
            }
        }
    }
}

unsafe impl<K> Trace for hashbrown::HashTable<K>
where
    K: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            for k in self {
                k.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for k in std::mem::take(self) {
                let mut k = ManuallyDrop::new(k);
                k.finalize_or_skip();
            }
        }
    }
}

unsafe impl<K> Trace for indexmap::IndexSet<K>
where
    K: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            for k in self {
                k.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for mut k in std::mem::take(self).into_iter().map(ManuallyDrop::new) {
                k.finalize_or_skip();
            }
        }
    }
}

unsafe impl<K, V> Trace for indexmap::IndexMap<K, V>
where
    K: GcOrTrace,
    V: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            for (k, v) in self {
                k.visit_or_recurse(visitor);
                v.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for (k, v) in std::mem::take(self) {
                let mut k = ManuallyDrop::new(k);
                let mut v = ManuallyDrop::new(v);
                k.finalize_or_skip();
                v.finalize_or_skip();
            }
        }
    }
}

unsafe impl<K> Trace for BTreeSet<K>
where
    K: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            for k in self {
                k.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for mut k in std::mem::take(self).into_iter().map(ManuallyDrop::new) {
                k.finalize_or_skip();
            }
        }
    }
}

unsafe impl<K, V> Trace for BTreeMap<K, V>
where
    K: GcOrTrace,
    V: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            for (k, v) in self {
                k.visit_or_recurse(visitor);
                v.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for (k, v) in std::mem::take(self).into_iter() {
                let mut k = ManuallyDrop::new(k);
                let mut v = ManuallyDrop::new(v);
                k.finalize_or_skip();
                v.finalize_or_skip();
            }
        }
    }
}

unsafe impl<T> Trace for Option<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            if let Some(inner) = self {
                inner.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            if let Some(inner) = self {
                inner.finalize_or_skip();
            }
        }
    }
}

unsafe impl<V, E> Trace for Result<V, E>
where
    V: GcOrTrace,
    E: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            match self {
                Ok(inner) => inner.visit_or_recurse(visitor),
                Err(inner) => inner.visit_or_recurse(visitor),
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            match self {
                Ok(inner) => inner.finalize_or_skip(),
                Err(inner) => inner.finalize_or_skip(),
            }
        }
    }
}

unsafe impl<T> Trace for Box<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            self.as_ref().visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.as_mut().finalize_or_skip();
            drop(Box::from_raw(
                self.as_mut() as *mut T as *mut ManuallyDrop<T>
            ));
        }
    }
}

unsafe impl<T> Trace for Box<[T]>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            for child in self.as_ref() {
                child.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for child in self.as_mut() {
                child.finalize_or_skip();
            }
            drop(Box::from_raw(
                self.as_mut() as *mut [T] as *mut ManuallyDrop<[T]>
            ));
        }
    }
}

unsafe impl<T> Trace for by_address::ByAddress<T>
where
    T: ?Sized + GcOrTrace + Deref,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe { self.0.visit_or_recurse(visitor) }
    }

    unsafe fn finalize(&mut self) {
        unsafe { self.0.finalize_or_skip() }
    }
}

unsafe impl<T> Trace for std::sync::Arc<T>
where
    T: ?Sized + 'static,
{
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {
        // We cannot visit the children for an Arc, as it may lead to situations
        // were we incorrectly decrement a child twice.
        // An Arc wrapping a Gc effectively creates an additional ref count for
        // that Gc that we cannot access.
    }

    unsafe fn finalize(&mut self) {
        unsafe { drop_in_place(self as *mut Self) }
    }
}

unsafe impl<T> Trace for std::sync::Weak<T>
where
    T: ?Sized + 'static,
{
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {
        // Same reasoning as Arc. If we're not visiting Arcs, we shouldn't visit Weak.
        // Let it handle its own ref count.
    }

    unsafe fn finalize(&mut self) {
        unsafe { drop_in_place(self as *mut Self) }
    }
}

#[cfg(feature = "async")]
unsafe impl<T> Trace for futures::future::Shared<T>
where
    T: std::future::Future + 'static,
{
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {}

    unsafe fn finalize(&mut self) {
        unsafe { drop_in_place(self as *mut Self) }
    }
}

unsafe impl<T> Trace for std::sync::mpsc::Sender<T>
where
    T: 'static,
{
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {}

    unsafe fn finalize(&mut self) {
        unsafe { drop_in_place(self as *mut Self) }
    }
}

unsafe impl<T> Trace for std::sync::mpsc::SyncSender<T>
where
    T: 'static,
{
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {}

    unsafe fn finalize(&mut self) {
        unsafe { drop_in_place(self as *mut Self) }
    }
}

// We can safely skip recursing in the cases when a mutex is being held, because
// a held mutex must be a live one.

#[cfg(feature = "tokio")]
unsafe impl<T> Trace for tokio::sync::Mutex<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            if let Ok(lock) = self.try_lock() {
                lock.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.get_mut().finalize_or_skip();
        }
    }
}

unsafe impl<T> Trace for parking_lot::RwLock<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            if let Some(read_lock) = self.try_read() {
                read_lock.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.get_mut().finalize_or_skip();
        }
    }
}

#[cfg(feature = "async")]
unsafe impl<T> Trace for async_lock::RwLock<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            if let Some(read_lock) = async_lock::RwLock::try_read(self) {
                read_lock.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.get_mut().finalize_or_skip();
        }
    }
}

#[cfg(feature = "tokio")]
unsafe impl<T> Trace for tokio::sync::RwLock<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            if let Ok(read_lock) = tokio::sync::RwLock::try_read(self) {
                read_lock.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.get_mut().finalize_or_skip();
        }
    }
}

#[cfg(feature = "tokio")]
unsafe impl<T> Trace for tokio::sync::mpsc::Sender<T>
where
    T: 'static,
{
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {}

    unsafe fn finalize(&mut self) {
        unsafe {
            drop_in_place(self as *mut Self);
        }
    }
}

unsafe impl<T> Trace for std::sync::Mutex<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            if let Ok(lock) = self.try_lock() {
                lock.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.get_mut().unwrap().finalize_or_skip();
        }
    }
}

unsafe impl<T> Trace for parking_lot::Mutex<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            if let Some(lock) = self.try_lock() {
                lock.visit_or_recurse(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.get_mut().finalize_or_skip();
        }
    }
}
