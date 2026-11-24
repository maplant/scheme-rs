use std::sync::{LazyLock, OnceLock};

use parking_lot::{Mutex, MutexGuard};

use crate::{HeapObject, Trace};

/// # Safety
/// visit_roots is called during a pause.
/// Be careful that mutators are not holding locks that are then acquired inside visit_roots.
pub unsafe trait Roots: Send + Sync {
    fn visit_roots(&self, visitor: &mut dyn FnMut(HeapObject));
}

pub struct RootedStatic<T: Trace>(LazyLock<Mutex<T>>);

unsafe impl<T: Trace + Send> Roots for RootedStatic<T> {
    fn visit_roots(&self, visitor: &mut dyn FnMut(HeapObject)) {
        if let Some(m) = LazyLock::get(&self.0) {
            unsafe { m.lock().visit_children(visitor) }
        }
    }
}

impl<T: Trace + Send> RootedStatic<T> {
    pub const fn new(init: fn() -> Mutex<T>) -> Self {
        RootedStatic(LazyLock::new(init))
    }

    pub fn lock(&self) -> MutexGuard<'_, T> {
        self.0.lock()
    }
}

unsafe impl<T: Trace + Send + Sync> Roots for LazyLock<T> {
    fn visit_roots(&self, visitor: &mut dyn FnMut(HeapObject)) {
        if let Some(t) = LazyLock::get(self) {
            unsafe { t.visit_children(visitor) }
        }
    }
}

unsafe impl<T: Trace + Send + Sync> Roots for OnceLock<T> {
    fn visit_roots(&self, visitor: &mut dyn FnMut(HeapObject)) {
        if let Some(t) = self.get() {
            unsafe { t.visit_children(visitor) }
        }
    }
}
