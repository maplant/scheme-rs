#[cfg(not(loom))]
pub(crate) use std::sync::{
    Condvar, Mutex, MutexGuard,
    atomic::{AtomicBool, AtomicU8, AtomicU64, AtomicUsize, Ordering},
};

#[cfg(loom)]
pub(crate) use loom::sync::{
    Condvar, Mutex, MutexGuard,
    atomic::{AtomicBool, AtomicU8, AtomicU64, AtomicUsize, Ordering},
};

pub(crate) fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}
