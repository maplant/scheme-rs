//! Garbage collected smart pointers.
//!
//! `Gc<T>` is conceptually similar to `Arc<T>`, but garbage collection occurs
//! concurrently at a fixed cadence or whenever a threshold of memory has been
//! allocated as opposed to when the type is Dropped.
//!
//! `Gc<T>` does not use tracing garbage collection but instead uses a technique
//! where garbage cycles are detected known as "cycle collection". This is done
//! in a separate thread and is concurrent to the running program.
//!
//! Cycle collection was chosen because it has similar characteristics to `Gc`,
//! providing all of the semantics Scheme expects and also plays nicely as a
//! Rust type (no need to root/unroot).

mod heap;

pub use heap::{HeapAlreadyCreated, budget, init_gc, set_heap_size, shutdown_gc};
pub use scheme_rs_gc::HeapObject;
pub use scheme_rs_macros::Trace;

use std::{
    any::{Any, TypeId},
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    hash::Hash,
    mem::ManuallyDrop,
    ops::Deref,
    path::PathBuf,
    ptr::{NonNull, drop_in_place},
    sync::atomic::Ordering,
};

pub(crate) use heap::alloc_layout;
use heap::{alloc, alloc_zeroed, collect_garbage_sync, unroot_obj};
use scheme_rs_gc::{HEADER_SIZE, VTable};
use scheme_rs_macros::{maybe_async, maybe_await};
#[cfg(feature = "tokio")]
use tokio::task::spawn_blocking;

use crate::{Either, registry::bridge};

/// A heap allocated garbage collected smart pointer. Gc requires that `T`
/// implements the [`Trace`] trait to properly track references.
#[repr(transparent)]
pub struct Gc<T: ?Sized> {
    pub(crate) ptr: NonNull<T>,
}

#[allow(private_bounds)]
impl<T: Send + GcOrTrace + 'static> Gc<T> {
    /// Allocate a new object on the heap and track it.
    pub fn new(data: T) -> Gc<T> {
        let new_gc = Self::rooted(data);
        unsafe {
            unroot(&new_gc);
        }
        new_gc
    }

    /// Allocate a new object and do not track it. The object will only become
    /// tracked when it is called with `unroot`.
    pub(crate) fn rooted(data: T) -> Gc<T> {
        Gc { ptr: alloc(data) }
    }

    /// Allocate a new, zeroed `T` in place.
    ///
    /// # Safety
    ///
    /// `T` must be valid with a zeroed bit pattern
    pub(crate) unsafe fn new_zeroed() -> Gc<T> {
        let gc = Gc {
            ptr: unsafe { alloc_zeroed() },
        };
        unsafe { unroot(&gc) };
        gc
    }
}

#[allow(private_bounds)]
impl<T: GcOrTrace + Send + Sync + 'static> Gc<T> {
    /// Convert a `Gc<T>` into a `Gc<dyn Any>`. This is a separate function
    /// since [`CoerceUnsized`](std::ops::CoerceUnsized) is unstable.
    pub fn into_any(this: Self) -> Gc<dyn Any + Send + Sync> {
        let this = ManuallyDrop::new(this);
        let any: NonNull<dyn Any + Send + Sync> = this.ptr;
        Gc { ptr: any }
    }
}

#[allow(private_bounds)]
impl<T: GcOrTrace> Gc<T> {
    /// Unroot a rooted Gc object.
    ///
    /// # Safety
    ///
    /// Calling this function more than once on a rooted Gc object or at all on
    /// an unrooted Gc object is undefined behavior.
    pub(crate) unsafe fn unroot(this: &Self) {
        unsafe { unroot(this) };
    }
}

impl<T: ?Sized> Gc<T> {
    /// # Safety
    ///
    /// This function is not safe and basically useless for anything outside of
    /// the Trace proc macro's generated code.
    #[doc(hidden)]
    pub unsafe fn as_opaque(&self) -> HeapObject {
        unsafe { HeapObject::from_data(self.ptr) }
    }

    /// Determine if two Gc types share the same pointer (i.e. are equivalent).
    pub fn ptr_eq(lhs: &Self, rhs: &Self) -> bool {
        std::ptr::addr_eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr())
    }

    pub(crate) fn as_ptr(this: &Self) -> *mut T {
        this.ptr.as_ptr()
    }

    pub(crate) fn into_raw(gc: Self) -> *mut T {
        ManuallyDrop::new(gc).ptr.as_ptr()
    }

    /// Safety: ptr is allocated by alloc and is directly preceded by a GcHeader
    pub(crate) unsafe fn increment_reference_count(ptr: *mut T) {
        debug_assert!(!ptr.is_null());
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        inc_rc(ptr);
    }

    pub(crate) unsafe fn from_raw(ptr: *mut T) -> Self {
        debug_assert!(!ptr.is_null());
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        Self { ptr }
    }
}

impl Gc<dyn Any + Send + Sync> {
    /// Attempt to downcase a `Gc<dyn Any>` into `T`, returning Self on
    /// failure.
    pub fn downcast<T: Any + Send + Sync>(self) -> Result<Gc<T>, Self> {
        if self.as_ref().is::<T>() {
            let this = ManuallyDrop::new(self);
            let ptr = this.ptr.cast::<T>();
            Ok(Gc { ptr })
        } else {
            Err(self)
        }
    }
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: ?Sized> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T: Send + Trace> From<T> for Gc<T> {
    fn from(t: T) -> Self {
        Gc::new(t)
    }
}

impl<T: Send + Trace + Default> Default for Gc<T> {
    fn default() -> Self {
        Gc::new(T::default())
    }
}

impl<T: ?Sized> std::fmt::Display for Gc<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // self.fmt(fmt)
        self.as_ref().fmt(f)
    }
}

impl<T: ?Sized> std::fmt::Debug for Gc<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        inc_rc(self.ptr);
        Self { ptr: self.ptr }
    }
}

impl<T: ?Sized> Drop for Gc<T> {
    fn drop(&mut self) {
        dec_rc(self.ptr);
    }
}

impl<T: ?Sized + Hash> Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}

impl<T: ?Sized + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: ?Sized + Eq> Eq for Gc<T> {}

unsafe impl<T: ?Sized + Send> Send for Gc<T> {}
unsafe impl<T: ?Sized + Sync> Sync for Gc<T> {}

unsafe impl<T> arc_swap::RefCnt for Gc<T> {
    type Base = T;

    fn into_ptr(me: Self) -> *mut T {
        Gc::into_raw(me)
    }

    fn as_ptr(me: &Self) -> *mut T {
        Gc::as_ptr(me)
    }

    unsafe fn from_ptr(ptr: *const Self::Base) -> Self {
        unsafe { Gc::from_raw(ptr as *mut T) }
    }
}

fn inc_rc<T: ?Sized>(ptr: NonNull<T>) {
    unsafe {
        HeapObject::from_data(ptr)
            .rc()
            .fetch_add(1, Ordering::Relaxed);
    }
}

fn dec_rc<T: ?Sized>(ptr: NonNull<T>) {
    unsafe {
        HeapObject::from_data(ptr)
            .rc()
            .fetch_sub(1, Ordering::Relaxed);
    }
}

fn vtable<T: GcOrTrace>() -> VTable {
    VTable {
        visit_children: |this, visitor| unsafe {
            T::visit_or_recurse(&*(this as *const T), visitor);
        },
        finalize: |this| unsafe {
            T::finalize_or_skip(&mut *(this as *mut T));
        },
    }
}

#[allow(private_bounds)]
pub(crate) unsafe fn unroot<T: GcOrTrace>(gc: &Gc<T>) {
    unsafe {
        unroot_obj(
            HeapObject::from_data(gc.ptr),
            TypeId::of::<T>(),
            vtable::<T>,
        )
    }
}

/// A type that can be traced for garbage collection. Types that implement this
/// trait can be converted into a [`Gc`] for automatic garbage collection.
///
/// # Safety
///
/// This trait should _not_ be manually implemented! Instead, use the
/// [`Trace`](scheme_rs_macros::Trace) derive macro.
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
unsafe trait GcOrTrace: 'static {
    unsafe fn visit_or_recurse(&self, visitor: &mut dyn FnMut(HeapObject));

    unsafe fn finalize_or_skip(&mut self);
}

unsafe impl<T: ?Sized + 'static> GcOrTrace for Gc<T> {
    unsafe fn visit_or_recurse(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe { visitor(self.as_opaque()) }
    }

    unsafe fn finalize_or_skip(&mut self) {}
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

unsafe impl<L, R> Trace for Either<L, R>
where
    L: GcOrTrace,
    R: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            match self {
                Either::Left(inner) => inner.visit_or_recurse(visitor),
                Either::Right(inner) => inner.visit_or_recurse(visitor),
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            match self {
                Either::Left(inner) => inner.finalize_or_skip(),
                Either::Right(inner) => inner.finalize_or_skip(),
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

#[bridge(name = "gc-header-size", lib = "(runtime (1))")]
pub fn gc_header_size() -> usize {
    HEADER_SIZE
}

/// Force a garbage collection pause.
#[cfg(not(feature = "async"))]
pub fn collect_garbage() {
    collect_garbage_sync();
}

#[cfg(feature = "tokio")]
pub async fn collect_garbage() {
    spawn_blocking(collect_garbage_sync).await.unwrap();
}

#[maybe_async]
#[bridge(name = "collect-garbage", lib = "(runtime (1))")]
pub fn collect_garbage_bridge() {
    maybe_await!(collect_garbage());
}

#[cfg(test)]
mod test {
    use super::collect_garbage_sync;
    use crate::gc::*;
    use parking_lot::{Mutex, RwLock};
    use std::sync::Arc;

    #[test]
    fn cycles() {
        _ = init_gc();

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
        unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(HeapObject)) {}

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

    #[test]
    fn release_cascade_finalizes_children_before_parents() {
        _ = init_gc();

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

        let order = FINALIZE_ORDER.lock().clone();
        assert_eq!(order.len(), 6, "not every node finalized: {order:?}");
        let at = |tag| order.iter().position(|&t| t == tag).unwrap();
        for (child, parent) in [(3, 1), (4, 1), (5, 2), (1, 0), (2, 0)] {
            assert!(at(child) < at(parent), "{child} finalized after {parent}");
        }
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
        _ = init_gc();

        let out = Arc::new(());
        let head = deep_chain(DEEP, &out);
        assert_eq!(Arc::strong_count(&out), 2);

        // Do not remove these. Until every node has been through an epoch its
        // buffered flag stops the cascade after a single node, so without them
        // the drop below never recurses deeply and this test passes even
        // against the unfixed recursive cascade.
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
        _ = init_gc();

        let out = Arc::new(());
        let chain = deep_chain(DEEP, &out);
        // Do not remove these; same reason as the previous test.
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
