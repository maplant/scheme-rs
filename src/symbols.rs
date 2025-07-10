//! Interned symbols

use std::{
    fmt,
    sync::{Arc, LazyLock, RwLock},
};

use indexmap::IndexSet;
use scheme_rs_macros::Trace;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Trace)]
pub struct Symbol(pub(crate) u32);

static SYMTAB: LazyLock<RwLock<IndexSet<Arc<str>>>> =
    LazyLock::new(|| RwLock::new(IndexSet::new()));

impl Symbol {
    pub fn intern(s: &str) -> Self {
        let mut symtab = SYMTAB.write().unwrap();
        let s = Arc::from(s.to_string());
        let id = if let Some(id) = symtab.get_index_of(&s) {
            id
        } else {
            let (id, _) = symtab.insert_full(s);
            id
        };
        Self(id.try_into().unwrap())
    }

    pub fn to_str(self) -> Arc<str> {
        let symtab = SYMTAB.read().unwrap();
        symtab[self.0 as usize].clone()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl PartialEq<&'_ str> for Symbol {
    fn eq(&self, rhs: &&str) -> bool {
        self.to_str().as_ref() == *rhs
    }
}
