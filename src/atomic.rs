use std::sync::Arc;

use arc_swap::ArcSwapAny;
use scheme_rs_macros::bridge;

use crate::{
    gc::{Gc, OpaqueGcPtr, Trace},
    records::{Embeddable, Embedded, RecordTypeDescriptor, rtd},
    value::Value,
};

#[derive(Debug)]
pub struct AtomicBox {
    inner: ArcSwapAny<Gc<Value>>,
}

unsafe impl Trace for AtomicBox {
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        // Same reasoning as Arc (gc/mod.rs:722): we cannot visit children
        // for a Gc managed by ArcSwap, as it may lead to situations where
        // we incorrectly decrement a child twice.
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            std::ptr::drop_in_place(self as *mut Self);
        }
    }
}

unsafe impl Embeddable for AtomicBox {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            ty: AtomicBox,
            name: "atomic-box",
            opaque: true,
            sealed: true,
        )
    }
}

#[bridge(name = "atomic-box?", lib = "(srfi :230)")]
pub fn atomic_box_p(obj: &Value) -> bool {
    obj.cast::<Embedded<AtomicBox>>().is_some()
}

#[bridge(name = "make-atomic-box", lib = "(srfi :230)")]
pub fn make_atomic_box(val: &Value) -> AtomicBox {
    AtomicBox {
        inner: ArcSwapAny::new(Gc::new(val.clone())),
    }
}

#[bridge(name = "atomic-box-ref", lib = "(srfi :230)")]
pub fn atomic_box_ref(ab: Embedded<AtomicBox>) -> Value {
    let guard = ab.inner.load();
    Value::clone(&guard)
}

#[bridge(name = "atomic-box-set!", lib = "(srfi :230)")]
pub fn atomic_box_set(ab: Embedded<AtomicBox>, new_val: &Value) {
    ab.inner.store(Gc::new(new_val.clone()));
}

#[bridge(name = "atomic-box-swap!", lib = "(srfi :230)")]
pub fn atomic_box_swap(ab: Embedded<AtomicBox>, new_val: &Value) -> Value {
    let old = ab.inner.swap(Gc::new(new_val.clone()));
    Value::clone(&old)
}

#[bridge(name = "atomic-box-compare-and-swap!", lib = "(srfi :230)")]
pub fn atomic_box_compare_and_swap(
    ab: Embedded<AtomicBox>,
    expected: &Value,
    desired: &Value,
) -> Value {
    let expected_bits = Value::as_raw(expected);
    let desired_gc = Gc::new(desired.clone());
    let prev = ab.inner.rcu(|current| {
        if Value::as_raw(current) == expected_bits {
            desired_gc.clone()
        } else {
            current.clone()
        }
    });
    Value::clone(&prev)
}
