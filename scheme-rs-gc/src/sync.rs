#[cfg(not(loom))]
pub(crate) use std::sync::{
    Condvar, Mutex, MutexGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
    atomic::{AtomicBool, AtomicU8, AtomicU64, AtomicUsize, Ordering},
};

#[cfg(loom)]
pub(crate) use loom::sync::{
    Condvar, Mutex, MutexGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
    atomic::{AtomicBool, AtomicU8, AtomicU64, AtomicUsize, Ordering},
};

pub(crate) fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

pub(crate) fn wait_while<'a, T>(
    cv: &Condvar,
    mut guard: MutexGuard<'a, T>,
    mut condition: impl FnMut(&mut T) -> bool,
) -> MutexGuard<'a, T> {
    while condition(&mut guard) {
        guard = cv.wait(guard).unwrap_or_else(|p| p.into_inner())
    }
    guard
}

pub(crate) fn read<T>(l: &RwLock<T>) -> RwLockReadGuard<'_, T> {
    l.read().unwrap_or_else(|p| p.into_inner())
}

pub(crate) fn write<T>(l: &RwLock<T>) -> RwLockWriteGuard<'_, T> {
    l.write().unwrap_or_else(|p| p.into_inner())
}
