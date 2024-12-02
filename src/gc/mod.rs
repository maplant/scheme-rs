//! Garbage-Collected smart pointers with interior mutability.
//!
//! Gc<T> is conceptually similar to Arc<tokio::sync::RwLock<T>>, but garbage
//! collection occurs concurrently at a fixed cadence or whenever a threshold
//! of memory has been allocated as opposed to when the type is Dropped.
//!
//! Strictly speaking, Gc<T> is not garbage collection per-se but instead uses
//! "cycle collection".
//!
//! Cycle collection was chosen because it has similar characteristics to Gc,
//! providing all of the semantics Scheme expects and also plays nicely as a
//! Rust type (no need to root/unroot).

mod collection;

use collection::{dec_rc, inc_rc};
pub use collection::{init_gc, process_mutation_buffer};
use futures::future::Shared;
pub use proc_macros::Trace;

use std::{
    cell::UnsafeCell,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    future::Future,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::{drop_in_place, NonNull},
    sync::Arc,
};
use tokio::sync::{RwLock, Semaphore, SemaphorePermit};

/// A Garbage-Collected smart pointer with interior mutability.
pub struct Gc<T: Trace> {
    ptr: NonNull<GcInner<T>>,
    marker: PhantomData<Arc<RwLock<T>>>,
}

impl<T: Trace> Gc<T> {
    pub fn new(data: T) -> Gc<T> {
        Self {
            ptr: NonNull::from(Box::leak(Box::new(GcInner {
                header: UnsafeCell::new(GcHeader::default()),
                data: UnsafeCell::new(data),
            }))),
            marker: PhantomData,
        }
    }
}

impl<T: Trace> Gc<T> {
    /// # Safety
    ///
    /// This function is not safe and basically useless for anything outside of
    /// the Trace proc macro's generated code.
    pub unsafe fn as_opaque(&self) -> OpaqueGcPtr {
        self.ptr as OpaqueGcPtr
    }

    /// Acquire a read lock for the object
    pub async fn read(&self) -> GcReadGuard<'_, T> {
        unsafe {
            let _permit = (*self.ptr.as_ref().header.get())
                .semaphore
                .acquire()
                .await
                .unwrap();
            let data = &*self.ptr.as_ref().data.get() as *const T;
            GcReadGuard {
                _permit,
                data,
                marker: PhantomData,
            }
        }
    }

    /// Attempt to acquire a read lock for the object
    pub fn try_read(&self) -> Option<GcReadGuard<'_, T>> {
        unsafe {
            let _permit = (*self.ptr.as_ref().header.get())
                .semaphore
                .try_acquire()
                .ok()?;
            let data = &*self.ptr.as_ref().data.get() as *const T;
            Some(GcReadGuard {
                _permit,
                data,
                marker: PhantomData,
            })
        }
    }

    /// Acquire a write lock for the object
    pub async fn write(&self) -> GcWriteGuard<'_, T> {
        unsafe {
            let _permit = (*self.ptr.as_ref().header.get())
                .semaphore
                .acquire_many(MAX_READS)
                .await
                .unwrap();
            let data = &mut *self.ptr.as_ref().data.get() as *mut T;
            GcWriteGuard {
                _permit,
                data,
                marker: PhantomData,
            }
        }
    }
}

impl<T: Trace> std::fmt::Debug for Gc<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        loop {
            if let Some(x) = self.try_read() {
                return x.fmt(fmt);
            }
        }
    }
}

impl<T: Trace> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        inc_rc(self.ptr);
        Self {
            ptr: self.ptr,
            marker: PhantomData,
        }
    }
}

impl<T: Trace> Drop for Gc<T> {
    fn drop(&mut self) {
        dec_rc(self.ptr);
    }
}

unsafe impl<T: Trace + Send + Sync> Send for Gc<T> {}
unsafe impl<T: Trace + Send + Sync> Sync for Gc<T> {}

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

const MAX_READS: u32 = u32::MAX >> 3;

#[derive(derive_more::Debug)]
pub struct GcHeader {
    rc: usize,
    crc: isize,
    color: Color,
    buffered: bool,
    #[debug(skip)]
    semaphore: Semaphore,
}

impl Default for GcHeader {
    fn default() -> Self {
        Self {
            rc: 1,
            crc: 1,
            color: Color::Black,
            buffered: false,
            semaphore: Semaphore::new(MAX_READS as usize),
        }
    }
}

unsafe impl Send for GcHeader {}
unsafe impl Sync for GcHeader {}

pub struct GcInner<T: ?Sized> {
    header: UnsafeCell<GcHeader>,
    data: UnsafeCell<T>,
}

unsafe impl<T: ?Sized + Send + Sync> Send for GcInner<T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for GcInner<T> {}

type OpaqueGc = GcInner<dyn Trace>;
pub type OpaqueGcPtr = NonNull<OpaqueGc>;

pub struct GcReadGuard<'a, T: ?Sized> {
    _permit: SemaphorePermit<'a>,
    data: *const T,
    marker: PhantomData<&'a T>,
}

impl<T: ?Sized> Deref for GcReadGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.data }
    }
}

impl<T: ?Sized> AsRef<T> for GcReadGuard<'_, T> {
    fn as_ref(&self) -> &T {
        self
    }
}

unsafe impl<T: ?Sized + Send + Sync> Send for GcReadGuard<'_, T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for GcReadGuard<'_, T> {}

pub struct GcWriteGuard<'a, T: ?Sized> {
    _permit: SemaphorePermit<'a>,
    data: *mut T,
    marker: PhantomData<&'a mut T>,
}

impl<T: ?Sized> Deref for GcWriteGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.data }
    }
}

impl<T: ?Sized> DerefMut for GcWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.data }
    }
}

/// # Safety
///
/// This trait should _not_ be manually implemented!
pub unsafe trait Trace: 'static {
    /// # Safety
    ///
    /// This function may _ONLY_ be called by the garbage collector! Calling this
    /// function **ANYWHERE ELSE** is a **RACE CONDITION**!
    ///
    /// **DO NOT CALL THIS FUNCTION!!**
    // TODO(map): Make this function async
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr));

    /// # Safety
    ///
    /// This function may _ONLY_ be called by the garbage collector! Calling this
    /// function **ANYWHERE ELSE** is a **RACE CONDITION**!
    ///
    /// **DO NOT CALL THIS FUNCTION!!**
    unsafe fn finalize(&mut self) {
        drop_in_place(self as *mut Self);
    }
}

macro_rules! impl_empty_trace {
    ( $( $x:ty ),* ) => {
        $(
            unsafe impl Trace for $x {
                unsafe fn visit_children(&self, _visitor: fn(OpaqueGcPtr)) {}
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
    String
}

/// # Safety
///
/// This function is _not_ safe to implement!
unsafe trait GcOrTrace: 'static {
    unsafe fn visit_or_recurse(&self, visitor: fn(OpaqueGcPtr));

    unsafe fn finalize_or_skip(&mut self);
}

unsafe impl<T: Trace> GcOrTrace for Gc<T> {
    unsafe fn visit_or_recurse(&self, visitor: fn(OpaqueGcPtr)) {
        visitor(self.as_opaque())
    }

    unsafe fn finalize_or_skip(&mut self) {}
}

unsafe impl<T: Trace + ?Sized> GcOrTrace for T {
    unsafe fn visit_or_recurse(&self, visitor: fn(OpaqueGcPtr)) {
        self.visit_children(visitor);
    }

    unsafe fn finalize_or_skip(&mut self) {
        self.finalize();
    }
}

unsafe impl<A, B> Trace for (A, B)
where
    A: GcOrTrace,
    B: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        self.0.visit_or_recurse(visitor);
        self.1.visit_or_recurse(visitor);
    }

    unsafe fn finalize(&mut self) {
        self.0.finalize_or_skip();
        self.1.finalize_or_skip();
    }
}

unsafe impl<T> Trace for Vec<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        for child in self {
            child.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for mut child in std::mem::take(self).into_iter() {
            child.finalize_or_skip();
            std::mem::forget(child);
        }
    }
}

unsafe impl<K> Trace for HashSet<K>
where
    K: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        for k in self {
            k.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for mut k in std::mem::take(self).into_iter() {
            k.finalize_or_skip();
            std::mem::forget(k);
        }
    }
}

unsafe impl<K, V> Trace for HashMap<K, V>
where
    K: GcOrTrace,
    V: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        for (k, v) in self {
            k.visit_or_recurse(visitor);
            v.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for (mut k, mut v) in std::mem::take(self) {
            k.finalize_or_skip();
            v.finalize_or_skip();
            std::mem::forget((k, v));
        }
    }
}

unsafe impl<K> Trace for BTreeSet<K>
where
    K: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        for k in self {
            k.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for mut k in std::mem::take(self) {
            k.finalize_or_skip();
            std::mem::forget(k);
        }
    }
}

unsafe impl<K, V> Trace for BTreeMap<K, V>
where
    K: GcOrTrace,
    V: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        for (k, v) in self {
            k.visit_or_recurse(visitor);
            v.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for (mut k, mut v) in std::mem::take(self).into_iter() {
            k.finalize_or_skip();
            v.finalize_or_skip();
            std::mem::forget((k, v));
        }
    }
}

unsafe impl<T> Trace for Option<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        if let Some(inner) = self {
            inner.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        if let Some(inner) = self {
            inner.finalize_or_skip();
        }
    }
}

unsafe impl<T> Trace for Box<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        self.as_ref().visit_or_recurse(visitor);
    }

    unsafe fn finalize(&mut self) {
        self.as_mut().finalize_or_skip();
        todo!("need to dealloc data without dropping box");
    }
}

unsafe impl<T> Trace for [T]
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        for item in self {
            item.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for item in self {
            item.finalize_or_skip();
        }
    }
}

unsafe impl<T> Trace for std::sync::Arc<T>
where
    T: ?Sized + 'static,
{
    unsafe fn visit_children(&self, _visitor: fn(OpaqueGcPtr)) {
        // We cannot visit the children for an Arc, as it may lead to situations
        // were we incorrectly decrement a child twice.
        // An Arc wrapping a Gc effectively creates an additional ref count for
        // that Gc that we cannot access.
    }
}

unsafe impl<T> Trace for std::sync::Weak<T>
where
    T: ?Sized + 'static,
{
    unsafe fn visit_children(&self, _visitor: fn(OpaqueGcPtr)) {
        // Same reasoning as Arc. If we're not visiting Arcs, we shouldn't visit Weak.
        // Let it handle its own ref count.
    }
}

unsafe impl<T> Trace for Shared<T>
where
    T: Future + 'static,
{
    unsafe fn visit_children(&self, _visitor: fn(OpaqueGcPtr)) {}
}

unsafe impl<A: 'static, O: 'static> Trace for fn(A) -> O {
    unsafe fn visit_children(&self, _visitor: fn(OpaqueGcPtr)) {}
}

unsafe impl<A: 'static, B: 'static, O: 'static> Trace for fn(A, B) -> O {
    unsafe fn visit_children(&self, _visitor: fn(OpaqueGcPtr)) {}
}

unsafe impl<T> Trace for tokio::sync::Mutex<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        // TODO: Think really hard as to if this is correct
        // This _should_ be fine, while not optimally efficient.
        let lock = self.blocking_lock();
        lock.visit_or_recurse(visitor);
    }
}

unsafe impl<T> Trace for tokio::sync::RwLock<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        // TODO: Think really hard as to if this is correct
        loop {
            if let Ok(read_lock) = self.try_read() {
                read_lock.visit_or_recurse(visitor);
                return;
            }
        }
    }
}

unsafe impl<T> Trace for std::sync::Mutex<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: fn(OpaqueGcPtr)) {
        // TODO: Think really hard as to if this is correct
        let lock = self.lock().unwrap();
        lock.visit_or_recurse(visitor);
    }
}
