#[cfg(feature = "barrier_count")]
use std::cell::Cell;
use std::{
    fmt::{self, Debug, Display, Formatter},
    mem::replace,
    ops::Deref,
    sync::RwLock,
};

use crate::{
    HeapObject, Trace,
    sync::{read, write},
};

/// A value that can be stored in a `HeapSlot`.
///
/// # Safety
///
/// `object` returns the heap object the value points to, and `None` exactly
/// for values that point to no heap object. The write barrier relies on it.
pub unsafe trait SlotValue: Clone {
    fn object(&self) -> Option<HeapObject>;
}

/// A value stored inside a heap object. Every write goes through the write
/// barrier.
///
/// A slot inside a published object is never simply dropped: a container
/// that removes one calls `retire`, and only the collector drops slots, when it
/// finalizes their object.
pub struct HeapSlot<V: SlotValue> {
    value: RwLock<V>, // later, this becomes one atomic word.
}

/// Formats as the current value, so a slot prints the same as the value it holds.
impl<V: SlotValue + Debug> Debug for HeapSlot<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        read(&self.value).fmt(f)
    }
}

/// Displays as the current value.
impl<V: SlotValue + Display> Display for HeapSlot<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        read(&self.value).fmt(f)
    }
}

/// A new, independent slot holding a clone of the current value. Like `new`,
/// for initializing an object that is not yet published.
impl<V: SlotValue> Clone for HeapSlot<V> {
    fn clone(&self) -> Self {
        Self::new(self.get())
    }
}

impl<V: SlotValue> HeapSlot<V> {
    pub fn new(v: V) -> Self {
        HeapSlot {
            value: RwLock::new(v),
        }
    }

    /// A new slot in an object the collector already knows, such as a
    /// hashtable entry. Logs an increment for `v`.
    pub fn new_published(v: V) -> Self {
        log_increment(&v);
        Self::new(v)
    }

    /// Takes the slot out of a published object. Logs a decrement for its
    /// value.
    pub fn retire(self) -> V {
        let v = self.value.into_inner().unwrap_or_else(|p| p.into_inner());
        log_decrement(&v);
        v
    }

    pub fn get(&self) -> V {
        read(&self.value).clone()
    }

    pub fn read(&self) -> impl Deref<Target = V> + '_ {
        read(&self.value)
    }

    pub fn set(&self, v: V) {
        write_barrier(self);
        *write(&self.value) = v;
    }

    pub fn replace(&self, v: V) -> V {
        write_barrier(self);
        replace(&mut *write(&self.value), v)
    }
}

unsafe impl<V: SlotValue + Trace> Trace for HeapSlot<V> {
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(HeapObject)) {
        unsafe {
            read(&self.value).visit_children(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        let v = self.value.get_mut().unwrap_or_else(|p| p.into_inner());
        unsafe { v.finalize() }
    }
}

#[cfg(feature = "barrier_count")]
thread_local! { static BARRIER_COUNT: Cell<usize> = const { Cell::new(0) }; }

#[cfg(feature = "barrier_count")]
pub fn barrier_count() -> usize {
    BARRIER_COUNT.get()
}

#[inline]
fn write_barrier<V: SlotValue>(_slot: &HeapSlot<V>) {
    count_barrier();
}

#[inline]
fn log_increment<V: SlotValue>(_v: &V) {
    count_barrier();
}

#[inline]
fn log_decrement<V: SlotValue>(_v: &V) {
    count_barrier();
}

#[inline]
fn count_barrier() {
    #[cfg(feature = "barrier_count")]
    BARRIER_COUNT.set(BARRIER_COUNT.get() + 1);
}

#[cfg(test)]
mod test_slot_value {
    #[cfg(feature = "barrier_count")]
    use super::barrier_count;
    use crate::{HeapObject, HeapSlot, SlotValue};

    #[derive(Clone, Debug, PartialEq)]
    struct TestValue(u32);

    unsafe impl SlotValue for TestValue {
        fn object(&self) -> Option<HeapObject> {
            None
        }
    }

    #[test]
    pub fn set_and_replace_mutates() {
        let value = HeapSlot::new(TestValue(5));
        let old = value.replace(TestValue(6));
        assert_eq!(*value.read(), TestValue(6));
        assert_eq!(old, TestValue(5));

        value.set(TestValue(7));
        assert_eq!(*value.read(), TestValue(7));
    }

    #[cfg(feature = "barrier_count")]
    #[test]
    pub fn only_writes_reaches_barrier() {
        let before = barrier_count();
        let slot = HeapSlot::new(TestValue(1));
        let _ = slot.get();
        let _ = slot.read();
        assert_eq!(barrier_count(), before);
        slot.set(TestValue(2));
        slot.replace(TestValue(3));
        assert_eq!(barrier_count(), before + 2);
    }
}
