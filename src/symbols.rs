//! Interned symbols

use std::{
    fmt,
    sync::{Arc, LazyLock, RwLock},
};

use indexmap::IndexSet;
use scheme_rs_macros::{Trace, bridge};

use crate::{exception::Condition, strings, value::Value};

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

#[bridge(name = "string->symbol", lib = "(base)")]
pub async fn string_to_symbol(s: &Value) -> Result<Vec<Value>, Condition> {
    let s: Arc<strings::AlignedString> = s.clone().try_into()?;
    Ok(vec![Value::from(Symbol::intern(&*s))])
}
