//! Scheme enumerations and enumeration sets.

use std::{collections::HashSet, sync::Arc};

use indexmap::IndexSet;

use crate::{
    conditions::Condition,
    gc::{Gc, Trace},
    records::{RecordTypeDescriptor, SchemeCompatible, rtd},
    symbols::Symbol,
};

#[derive(Trace, Debug)]
pub struct EnumerationType {
    symbols: IndexSet<Symbol>,
}

impl EnumerationType {
    pub fn new(symbols: impl IntoIterator<Item = Symbol>) -> Self {
        Self {
            symbols: symbols.into_iter().collect(),
        }
    }
}

#[derive(Trace, Debug)]
pub struct EnumerationSet {
    enum_type: Gc<EnumerationType>,
    set: HashSet<Symbol>,
}

impl EnumerationSet {
    pub fn new(enum_type: &Gc<EnumerationType>, set: impl IntoIterator<Item = Symbol>) -> Self {
        Self {
            enum_type: enum_type.clone(),
            set: set.into_iter().collect(),
        }
    }

    pub fn type_check(&self, ty: &Gc<EnumerationType>) -> Result<(), Condition> {
        if !Gc::ptr_eq(&self.enum_type, ty) {
            Err(Condition::error("wrong enumeration type"))
        } else {
            Ok(())
        }
    }

    /// Checks for membership in the set
    pub fn contains(&self, sym: &str) -> bool {
        self.set.contains(&Symbol::intern(sym))
    }
}

impl SchemeCompatible for EnumerationSet {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "enum-set", sealed: true, opaque: true)
    }
}
