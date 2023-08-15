use std::{
    cell::UnsafeCell,
    collections::HashMap,
    future::Future,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::{
        atomic::{self, Ordering},
        Arc,
    },
};
use tokio::sync::{RwLock, Semaphore, SemaphorePermit};

const MAX_READS: u32 = std::u32::MAX >> 3;

struct GcInner<T: ?Sized> {
    roots: atomic::AtomicUsize,
    semaphore: Semaphore,
    data: UnsafeCell<T>,
}

impl<T: ?Sized> GcInner<T> {
    fn inc_roots(&self) {
        self.roots.fetch_add(1, Ordering::Relaxed);
    }

    fn dec_roots(&self) {
        self.roots.fetch_sub(1, Ordering::Release);
    }

    async fn read(&self) -> GcReadGuard<'_, T> {
        let permit = self.semaphore.acquire().await.unwrap();
        let data = self.data.get() as *const T;
        GcReadGuard {
            permit,
            data,
            marker: PhantomData,
        }
    }
}

impl<T: ?Sized + Trace> GcInner<T> {
    async fn write(&self) -> GcWriteGuard<'_, T> {
        unsafe { (&*self.data.get()).root() }
        let permit = self.semaphore.acquire_many(MAX_READS).await.unwrap();
        let data = self.data.get();
        GcWriteGuard {
            permit,
            data,
            marker: PhantomData,
        }
    }
}

unsafe impl<T: ?Sized + Send + Sync> Send for GcInner<T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for GcInner<T> {}

pub struct GcReadGuard<'a, T: ?Sized> {
    permit: SemaphorePermit<'a>,
    data: *const T,
    marker: PhantomData<&'a T>,
}

impl<'a, T: ?Sized> Deref for GcReadGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.data }
    }
}

unsafe impl<T: ?Sized + Send + Sync> Send for GcReadGuard<'_, T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for GcReadGuard<'_, T> {}

pub struct GcWriteGuard<'a, T: ?Sized + Trace> {
    permit: SemaphorePermit<'a>,
    data: *mut T,
    marker: PhantomData<&'a mut T>,
}

impl<'a, T: ?Sized + Trace> Deref for GcWriteGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.data }
    }
}

impl<'a, T: ?Sized + Trace> DerefMut for GcWriteGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.data }
    }
}

impl<'a, T: ?Sized + Trace> Drop for GcWriteGuard<'a, T> {
    fn drop(&mut self) {
        unsafe { (&*self.data).unroot() }
    }
}

pub struct Gc<T: ?Sized> {
    ptr: NonNull<GcInner<T>>,
    marker: PhantomData<Arc<RwLock<T>>>,
}

impl<T: Trace> Gc<T> {
    pub fn new(t: T) -> Gc<T> {
        t.unroot();
        Self {
            ptr: NonNull::from(Box::leak(Box::new(GcInner {
                roots: atomic::AtomicUsize::new(1),
                semaphore: Semaphore::new(MAX_READS as usize),
                data: UnsafeCell::new(t),
            }))),
            marker: PhantomData,
        }
    }
}

impl<T: ?Sized + Trace> Gc<T> {
    unsafe fn to_inner(&self) -> &GcInner<T> {
        self.ptr.as_ref()
    }

    pub async fn read(&self) -> GcReadGuard<'_, T> {
        unsafe { self.to_inner().read().await }
    }

    pub async fn write(&self) -> GcWriteGuard<'_, T> {
        unsafe { self.to_inner().write().await }
    }
}

impl<T: ?Sized + Trace> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        unsafe { self.ptr.as_ref().inc_roots() }
        Self {
            ptr: self.ptr,
            marker: PhantomData,
        }
    }
}

impl<T: ?Sized + Trace> Trace for Gc<T> {
    fn unroot(&self) {
        unsafe { self.ptr.as_ref().dec_roots() }
    }
}

unsafe impl<T: ?Sized + Send + Sync> Send for Gc<T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for Gc<T> {}

#[async_trait::async_trait]
pub trait Trace {
    fn root(&self) {}

    fn unroot(&self) {}

    async fn visit_children(&self, visitor: fn(Gc<&dyn Trace>) -> Box<dyn Future<Output = ()>>) {}
}

impl<K, T: Trace> Trace for HashMap<K, T> {
    fn root(&self) {
        for item in self.values() {
            item.root();
        }
    }

    fn unroot(&self) {
        for item in self.values() {
            item.unroot();
        }
    }
}

impl<T: Trace> Trace for Vec<T> {
    fn root(&self) {
        for item in self {
            item.root();
        }
    }

    fn unroot(&self) {
        for item in self {
            item.unroot();
        }
    }
}
