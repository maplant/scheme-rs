//! Garbage-Collected smart pointers with interior mutability.
//!
//! `Gc<T>` is conceptually similar to `Arc<tokio::sync::RwLock<T>>`, but garbage
//! collection occurs concurrently at a fixed cadence or whenever a threshold
//! of memory has been allocated as opposed to when the type is Dropped.
//!
//! Strictly speaking, `Gc<T>` is not garbage collection per-se but instead uses
//! "cycle collection".
//!
//! Cycle collection was chosen because it has similar characteristics to `Gc`,
//! providing all of the semantics Scheme expects and also plays nicely as a
//! Rust type (no need to root/unroot).

mod collection;

pub use collection::init_gc;
use collection::{dec_rc, inc_rc};
use either::Either;
use futures::future::Shared;
pub use scheme_rs_macros::Trace;

use std::{
    alloc::Layout,
    any::Any,
    cell::UnsafeCell,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    future::Future,
    hash::Hash,
    marker::PhantomData,
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
    path::PathBuf,
    ptr::{NonNull, drop_in_place},
    sync::{RwLock, RwLockReadGuard, RwLockWriteGuard},
};

/// A Garbage-Collected smart pointer with interior mutability.
pub struct Gc<T: ?Sized> {
    pub(crate) ptr: NonNull<GcInner<T>>,
    pub(crate) marker: PhantomData<GcInner<T>>,
}

#[allow(private_bounds)]
impl<T: GcOrTrace + 'static> Gc<T> {
    pub fn new(data: T) -> Gc<T> {
        Self {
            ptr: NonNull::from(Box::leak(Box::new(GcInner {
                header: UnsafeCell::new(GcHeader::new::<T>()),
                data: UnsafeCell::new(data),
            }))),
            marker: PhantomData,
        }
    }

    /// Convert a `Gc<T>` into a `Gc<dyn Any>`. This is a separate function
    /// since [CoerceUnsized] is unstable.
    pub fn into_any(this: Self) -> Gc<dyn Any> {
        let this = ManuallyDrop::new(this);
        let any: NonNull<GcInner<dyn Any>> = this.ptr;
        Gc {
            ptr: any,
            marker: PhantomData,
        }
    }
}

impl<T: ?Sized> Gc<T> {
    /// # Safety
    ///
    /// This function is not safe and basically useless for anything outside of
    /// the Trace proc macro's generated code.
    pub unsafe fn as_opaque(&self) -> OpaqueGcPtr {
        OpaqueGcPtr::from(self.ptr)
    }

    /// Acquire a read lock for the object
    pub fn read(&self) -> GcReadGuard<'_, T> {
        unsafe {
            let _permit = (*self.ptr.as_ref().header.get()).lock.read().unwrap();
            let data = &*self.ptr.as_ref().data.get() as *const T;
            GcReadGuard {
                _permit,
                data,
                marker: PhantomData,
            }
        }
    }

    /// Acquire a write lock for the object
    pub fn write(&self) -> GcWriteGuard<'_, T> {
        unsafe {
            let _permit = (*self.ptr.as_ref().header.get()).lock.write().unwrap();
            let data = &mut *self.ptr.as_ref().data.get() as *mut T;
            GcWriteGuard {
                _permit,
                data,
                marker: PhantomData,
            }
        }
    }

    pub fn ptr_eq(lhs: &Self, rhs: &Self) -> bool {
        std::ptr::addr_eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr())
    }

    pub(crate) fn as_ptr(this: &Self) -> *mut GcInner<T> {
        this.ptr.as_ptr()
    }

    pub(crate) fn into_raw(gc: Self) -> *mut GcInner<T> {
        ManuallyDrop::new(gc).ptr.as_ptr()
    }

    pub(crate) unsafe fn increment_reference_count(ptr: *mut GcInner<T>) {
        let ptr = NonNull::new(ptr).unwrap();
        inc_rc(ptr);
    }

    pub(crate) unsafe fn decrement_reference_count(ptr: *mut GcInner<T>) {
        let ptr = NonNull::new(ptr).unwrap();
        dec_rc(ptr);
    }

    /// Create a new Gc from the raw pointer. Does not increment the reference
    /// count.
    pub(crate) unsafe fn from_raw(ptr: *mut GcInner<T>) -> Self {
        let ptr = NonNull::new(ptr).unwrap();
        Self {
            ptr,
            marker: PhantomData,
        }
    }

    /// The same as from_raw, but increments the reference count.
    pub(crate) unsafe fn from_raw_inc_rc(ptr: *mut GcInner<T>) -> Self {
        let ptr = NonNull::new(ptr).unwrap();
        inc_rc(ptr);
        Self {
            ptr,
            marker: PhantomData,
        }
    }
}

impl Gc<dyn Any> {
    pub fn downcast<T: Any>(self) -> Result<Gc<T>, Self> {
        if self.read().as_ref().is::<T>() {
            let this = ManuallyDrop::new(self);
            let ptr = this.ptr.as_ptr() as *mut GcInner<T>;
            let ptr = unsafe { NonNull::new_unchecked(ptr) };
            Ok(Gc {
                ptr,
                marker: PhantomData,
            })
        } else {
            Err(self)
        }
    }
}

impl<T: Trace> From<T> for Gc<T> {
    fn from(t: T) -> Self {
        Gc::new(t)
    }
}

impl<T: ?Sized> std::fmt::Display for Gc<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.read().fmt(fmt)
    }
}

impl<T: ?Sized> std::fmt::Debug for Gc<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.read().fmt(fmt)
    }
}

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        inc_rc(self.ptr);
        Self {
            ptr: self.ptr,
            marker: PhantomData,
        }
    }
}

impl<T: ?Sized> Drop for Gc<T> {
    fn drop(&mut self) {
        dec_rc(self.ptr);
    }
}

impl<T: ?Sized + Hash> Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.read().hash(state);
    }
}

impl<T: ?Sized + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        let self_read = self.read();
        let other_read = other.read();
        self_read.as_ref() == other_read.as_ref()
    }
}

impl<T: ?Sized + Eq> Eq for Gc<T> {}

unsafe impl<T: ?Sized + Send> Send for Gc<T> {}
unsafe impl<T: ?Sized + Sync> Sync for Gc<T> {}

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

pub(crate) struct GcHeader {
    rc: usize,
    crc: isize,
    color: Color,
    buffered: bool,
    lock: RwLock<()>,
    // Vtable for Trace:
    visit_children: unsafe fn(this: *const (), visitor: &mut dyn FnMut(OpaqueGcPtr)),
    finalize: unsafe fn(this: *mut ()),
    layout: Layout,
}

impl GcHeader {
    fn new<T: GcOrTrace>() -> Self {
        Self {
            rc: 1,
            crc: 1,
            color: Color::Black,
            buffered: false,
            lock: RwLock::new(()),
            visit_children: |this, visitor| unsafe {
                let this = this as *const T;
                T::visit_or_recurse(this.as_ref().unwrap(), visitor);
            },
            finalize: |this| unsafe {
                let this = this as *mut T;
                T::finalize_or_skip(this.as_mut().unwrap());
            },
            layout: Layout::new::<GcInner<T>>(),
        }
    }
}

unsafe impl Send for GcHeader {}
unsafe impl Sync for GcHeader {}

pub(crate) struct GcInner<T: ?Sized> {
    header: UnsafeCell<GcHeader>,
    data: UnsafeCell<T>,
}

unsafe impl<T: ?Sized + Send> Send for GcInner<T> {}
unsafe impl<T: ?Sized + Sync> Sync for GcInner<T> {}

/// Fat pointer to the header and data of the Gc
#[derive(Clone, Copy)]
pub struct OpaqueGcPtr {
    header: NonNull<UnsafeCell<GcHeader>>,
    data: NonNull<UnsafeCell<()>>,
}

impl<T: ?Sized> From<NonNull<GcInner<T>>> for OpaqueGcPtr {
    fn from(mut value: NonNull<GcInner<T>>) -> Self {
        unsafe {
            let value_mut = value.as_mut();
            let header = NonNull::new(value_mut.header.get() as *mut _).unwrap();
            let data = NonNull::new(UnsafeCell::from_mut(
                &mut *(value_mut.data.get() as *mut ()),
            ))
            .unwrap();
            Self { header, data }
        }
    }
}

impl OpaqueGcPtr {
    unsafe fn rc(&self) -> usize {
        unsafe { (*self.header.as_ref().get()).rc }
    }

    unsafe fn set_rc(&self, rc: usize) {
        unsafe {
            (*self.header.as_ref().get()).rc = rc;
        }
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

pub struct GcReadGuard<'a, T: ?Sized> {
    _permit: RwLockReadGuard<'a, ()>,
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

pub struct GcWriteGuard<'a, T: ?Sized> {
    _permit: RwLockWriteGuard<'a, ()>,
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

impl<T: ?Sized> AsRef<T> for GcWriteGuard<'_, T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T: ?Sized> AsMut<T> for GcWriteGuard<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        self
    }
}

/// A copy of a Gc that is ready only.
pub struct GcReadOnly<T>(Gc<T>);

impl<T> GcReadOnly<T> {
    pub fn new(gc: Gc<T>) -> Self {
        Self(gc)
    }

    pub fn read(&self) -> GcReadGuard<'_, T> {
        self.0.read()
    }
}

/// A type that can be traced for garbage collection.
///
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
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr));

    /// # Safety
    ///
    /// This function may _ONLY_ be called by the garbage collector! Calling this
    /// function **ANYWHERE ELSE** is a **RACE CONDITION**!
    ///
    /// **DO NOT CALL THIS FUNCTION!!**
    unsafe fn finalize(&mut self) {
        unsafe {
            drop_in_place(self as *mut Self);
        }
    }
}

macro_rules! impl_empty_trace {
    ( $( $x:ty ),* ) => {
        $(
            unsafe impl Trace for $x {
                unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {}
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
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {}
}

/// # Safety
///
/// This function is _not_ safe to implement!
unsafe trait GcOrTrace: 'static {
    unsafe fn visit_or_recurse(&self, visitor: &mut dyn FnMut(OpaqueGcPtr));

    unsafe fn finalize_or_skip(&mut self);
}

unsafe impl<T: ?Sized + 'static> GcOrTrace for Gc<T> {
    unsafe fn visit_or_recurse(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        unsafe { visitor(self.as_opaque()) }
    }

    unsafe fn finalize_or_skip(&mut self) {}
}

unsafe impl<T: Trace + ?Sized> GcOrTrace for T {
    unsafe fn visit_or_recurse(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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

unsafe impl<T> Trace for Vec<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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

unsafe impl<K> Trace for indexmap::IndexSet<K>
where
    K: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    T: GcOrTrace + ?Sized,
{
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        // self.as_ref().visit_or_recurse(visitor);
    }

    /*
    unsafe fn finalize(&mut self) {
        println!("finalizing box!");
        self.as_mut().finalize_or_skip();
        std::alloc::dealloc(self.as_mut() as *mut T as *mut u8, Layout::new::<T>());
        // todo!("need to dealloc data without dropping box");
    }
    */
}

/*
unsafe impl<T> Trace for [T]
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
 */

unsafe impl<T> Trace for by_address::ByAddress<T>
where
    T: ?Sized + GcOrTrace + Deref,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {
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
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        // Same reasoning as Arc. If we're not visiting Arcs, we shouldn't visit Weak.
        // Let it handle its own ref count.
    }
}

unsafe impl<T> Trace for Shared<T>
where
    T: Future + 'static,
{
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {}
}

unsafe impl<T> Trace for tokio::sync::Mutex<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        unsafe {
            // TODO: Think really hard as to if this is correct
            // This _should_ be fine, while not optimally efficient.
            let lock = self.blocking_lock();
            lock.visit_or_recurse(visitor);
        }
    }
}

unsafe impl<T> Trace for tokio::sync::RwLock<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        unsafe {
            // TODO: Think really hard as to if this is correct
            loop {
                if let Ok(read_lock) = self.try_read() {
                    read_lock.visit_or_recurse(visitor);
                    return;
                }
            }
        }
    }
}

unsafe impl<T> Trace for tokio::sync::mpsc::Sender<T>
where
    T: 'static,
{
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {}
}

unsafe impl<T> Trace for std::sync::Mutex<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        unsafe {
            // TODO: Think really hard as to if this is correct
            let lock = self.lock().unwrap();
            lock.visit_or_recurse(visitor);
        }
    }
}
