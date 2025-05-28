//! Rudimentary structure support. CPS will probably make a lot of this redundant.

use std::sync::Arc;

use crate::{
    gc::{Gc, Trace},
    syntax::Identifier, 
    value::Value,
};

/// Type declaration for a record.
#[derive(Debug, Trace, Clone)]
#[repr(align(16))]
pub struct RecordType {
    name: String,
    /// Parent is most recently inserted record type, if one exists.
    inherits: indexmap::IndexSet<Arc<RecordType>>,
    fields: Vec<Identifier>,
}

impl RecordType {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            inherits: indexmap::IndexSet::new(),
            fields: Vec::new(),
        }
    }
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
    fields: Vec<Gc<Value>>,
}
