use std::sync::Arc;

use arc_swap::ArcSwapAny;
use scheme_rs_macros::bridge;

use crate::{
    exceptions::Exception,
    gc::{Gc, OpaqueGcPtr, Trace},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
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

impl SchemeCompatible for AtomicBox {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            name: "atomic-box",
            opaque: true,
            sealed: true,
        )
    }
}

#[bridge(name = "atomic-box?", lib = "(srfi :230)")]
pub fn atomic_box_p(obj: &Value) -> Result<Vec<Value>, Exception> {
    let is_atomic_box = obj.cast_to_rust_type::<AtomicBox>().is_some();
    Ok(vec![Value::from(is_atomic_box)])
}

#[bridge(name = "make-atomic-box", lib = "(srfi :230)")]
pub fn make_atomic_box(val: &Value) -> Result<Vec<Value>, Exception> {
    let ab = AtomicBox {
        inner: ArcSwapAny::new(Gc::new(val.clone())),
    };
    Ok(vec![Value::from(Record::from_rust_type(ab))])
}

#[bridge(name = "atomic-box-ref", lib = "(srfi :230)")]
pub fn atomic_box_ref(box_val: &Value) -> Result<Vec<Value>, Exception> {
    let ab = box_val.try_to_rust_type::<AtomicBox>()?;
    let guard = ab.inner.load();
    Ok(vec![Value::clone(&guard)])
}

#[bridge(name = "atomic-box-set!", lib = "(srfi :230)")]
pub fn atomic_box_set(box_val: &Value, new_val: &Value) -> Result<Vec<Value>, Exception> {
    let ab = box_val.try_to_rust_type::<AtomicBox>()?;
    ab.inner.store(Gc::new(new_val.clone()));
    Ok(vec![Value::undefined()])
}

#[bridge(name = "atomic-box-swap!", lib = "(srfi :230)")]
pub fn atomic_box_swap(box_val: &Value, new_val: &Value) -> Result<Vec<Value>, Exception> {
    let ab = box_val.try_to_rust_type::<AtomicBox>()?;
    let old = ab.inner.swap(Gc::new(new_val.clone()));
    Ok(vec![Value::clone(&old)])
}

#[bridge(name = "atomic-box-compare-and-swap!", lib = "(srfi :230)")]
pub fn atomic_box_compare_and_swap(
    box_val: &Value,
    expected: &Value,
    desired: &Value,
) -> Result<Vec<Value>, Exception> {
    let ab = box_val.try_to_rust_type::<AtomicBox>()?;
    let expected_bits = Value::as_raw(expected);
    let desired_gc = Gc::new(desired.clone());
    let prev = ab.inner.rcu(|current| {
        if Value::as_raw(current) == expected_bits {
            desired_gc.clone()
        } else {
            current.clone()
        }
    });
    Ok(vec![Value::clone(&prev)])
}
