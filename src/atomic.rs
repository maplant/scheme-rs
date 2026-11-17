use scheme_rs_macros::bridge;

use crate::{
    gc::{HeapValue, Trace},
    records::{Embeddable, Embedded},
    value::Value,
};

#[derive(Debug, Embeddable, Trace)]
#[scheme(name = "%atomic-box", opaque = true, sealed = true)]
pub struct AtomicBox {
    inner: HeapValue,
}

#[bridge(name = "atomic-box?", lib = "(srfi :230)")]
pub fn atomic_box_p(obj: Value) -> bool {
    obj.cast::<Embedded<AtomicBox>>().is_some()
}

#[bridge(name = "make-atomic-box", lib = "(srfi :230)")]
pub fn make_atomic_box(val: Value) -> AtomicBox {
    AtomicBox {
        inner: HeapValue::new(val.clone()),
    }
}

#[bridge(name = "atomic-box-ref", lib = "(srfi :230)")]
pub fn atomic_box_ref(ab: Embedded<AtomicBox>) -> Value {
    let guard = ab.inner.get();
    Value::clone(&guard)
}

#[bridge(name = "atomic-box-set!", lib = "(srfi :230)")]
pub fn atomic_box_set(ab: Embedded<AtomicBox>, new_val: Value) {
    ab.inner.set(new_val);
}

#[bridge(name = "atomic-box-swap!", lib = "(srfi :230)")]
pub fn atomic_box_swap(ab: Embedded<AtomicBox>, new_val: Value) -> Value {
    let old = ab.inner.swap(new_val);
    Value::clone(&old)
}

#[bridge(name = "atomic-box-compare-and-swap!", lib = "(srfi :230)")]
pub fn atomic_box_compare_and_swap(
    ab: Embedded<AtomicBox>,
    expected: Value,
    desired: Value,
) -> Value {
    let expected_bits = Value::as_raw(&expected);
    ab.inner
        .compare_exchange(desired, |cur| Value::as_raw(cur) == expected_bits)
}
