use std::collections::{HashMap, HashSet};

use crate::{gc::{Gc, Trace}, syntax::Identifier, value::Value};

/// Type declaration for a record.
#[derive(Trace)]
pub struct RecordType {
    inherits: Option<Gc<RecordType>>,
    /// While _we_ don't care about this being ordered, guile says they should
    /// be, so we order them.
    fields: Vec<Identifier>,
}

pub struct Record {
    type_decl: Gc<RecordType>,
    fields: HashMap<String, Gc<Value>>,
}

struct FieldRef {
    identifier: Identifier,
}

struct FieldSet {
    identifier: Identifier,
}
