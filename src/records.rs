//! Rudimentary structure support. CPS will probably make a lot of this redundant.

use std::{cell::LazyCell, sync::Arc};

use crate::{
    exception::Condition,
    gc::Trace,
    registry::bridge,
    syntax::{Identifier, Syntax},
    value::Value,
};

/// Type declaration for a record.
#[derive(Debug, Trace, Clone)]
#[repr(align(16))]
pub struct RecordType {
    name: String, // Make Arc<AlignedString>?
    sealed: bool,
    opaque: bool,
    /// Parent is most recently inserted record type, if one exists.
    // TODO: We need to change this from an Arc to some other type that hashes
    // pointers rather than the inner type.
    inherits: indexmap::IndexSet<Arc<RecordType>>,
    fields: Vec<Field>,
}

#[derive(Debug, Trace, Clone)]
pub enum Field {
    Immutable(Identifier),
    Mutable(Identifier),
}

/// The record type for the "record type" type.
const RECORD_TYPE_RT: LazyCell<Arc<RecordType>> = LazyCell::new(|| {
    Arc::new(RecordType {
        name: "rt".to_string(),
        sealed: true,
        opaque: true,
        inherits: indexmap::IndexSet::new(),
        fields: vec![],
    })
});

/*
impl RecordType {
    pub fn new(name: &str, parent: Option<&RecordType>, sealed: bool, opaque: bool) -> Self {
        Self {
            name: name.to_string(),
            inherits: indexmap::IndexSet::new(),
            fields: Vec::new(),
        }
    }
}

*/

#[bridge(name = "make-record-type-descriptor", lib = "(base)")]
pub async fn make_record_type_descriptor(
    name: &Value,
    parent: &Value,
    _uid: &Value,
    sealed: &Value,
    opaque: &Value,
    fields: &Value,
) -> Result<Vec<Value>, Condition> {
    let name = name.clone().try_into_sym()?;
    let parent: Option<Arc<RecordType>> = parent
        .is_true()
        .then(|| parent.clone().try_into())
        .transpose()?;
    let inherits = if let Some(parent) = parent {
        let mut inherits = parent.inherits.clone();
        // inherits.insert(parent);
        inherits
    } else {
        indexmap::IndexSet::new()
    };
    let sealed = sealed.is_true();
    let opaque = opaque.is_true();
    let _fields: Arc<Syntax> = fields.clone().try_into()?;
    Ok(vec![Value::from(Arc::new(RecordType {
        name: name.to_string(),
        sealed,
        opaque,
        inherits,
        fields: todo!(),
    }))])
}

/*
fn is_subtype_of(lhs: &Gc<RecordType>, rhs: &Gc<RecordType>) -> bool {
    lhs == rhs || {
        let lhs = lhs.read();
        lhs.inherits.contains(rhs)
    }
}
*/

#[derive(Debug, Trace, Clone)]
#[repr(align(16))]
pub struct Record {
    record_type: Arc<RecordType>,
    fields: Vec<Value>,
}
