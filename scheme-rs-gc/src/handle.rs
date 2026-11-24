use std::sync::atomic::{AtomicU64, Ordering};

use parking_lot::Mutex;

use hashbrown::HashMap;

use crate::{HeapObject, Roots, SlotValue};

pub struct Handles<V: SlotValue> {
    next: AtomicU64,
    map: Mutex<HashMap<u64, V>>,
}

impl<V: SlotValue> Handles<V> {
    pub fn new() -> Self {
        Handles {
            next: AtomicU64::new(0),
            map: Mutex::new(HashMap::new()),
        }
    }

    pub(crate) fn create(&self, value: V) -> Handle<'_, V> {
        let key = self.next.fetch_add(1, Ordering::Relaxed);
        self.map.lock().insert(key, value);
        Handle { table: self, key }
    }

    pub(crate) fn read(&self, handle: &Handle<'_, V>) -> Option<V> {
        self.map.lock().get(&handle.key).cloned()
    }

    pub(crate) fn scan_values(&self, mut f: impl FnMut(&HeapObject)) {
        self.map
            .lock()
            .values()
            .filter_map(V::object)
            .for_each(|o| f(&o));
    }
}

unsafe impl<V: SlotValue + Send> Roots for Handles<V> {
    fn visit_roots(&self, visitor: &mut dyn FnMut(HeapObject)) {
        self.scan_values(|v| visitor(*v));
    }
}

pub struct Handle<'h, V: SlotValue> {
    table: &'h Handles<V>,
    key: u64,
}

impl<V: SlotValue> Drop for Handle<'_, V> {
    fn drop(&mut self) {
        // Because this takes a lock, handles can't be dropped on the collector thread
        // during a pause as it would be a deadlock.
        self.table.map.lock().remove(&self.key);
    }
}
