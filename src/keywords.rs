use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use crate::{
    gc::Trace,
    records::{Embeddable, Embedded},
    registry::define,
    symbols::Symbol,
    value::Value,
};

#[derive(Debug, Clone, Trace, Embeddable)]
#[scheme(name = "keyword", sealed = true, opaque = true)]
pub struct Keyword(Symbol);

static INTERNED: LazyLock<Mutex<HashMap<Symbol, Value>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

impl Keyword {
    pub fn intern(name: &str) -> Value {
        let sym = Symbol::intern(name);
        let mut cache = INTERNED.lock().unwrap();
        cache
            .entry(sym)
            .or_insert_with(|| Value::from(Keyword(sym)))
            .clone()
    }
}

#[define(name = "keyword?", lib = "(srfi :88)")]
pub fn keyword_pred(obj: Value) -> bool {
    obj.cast::<Embedded<Keyword>>().is_some()
}

#[define(name = "keyword->string", lib = "(srfi :88)")]
pub fn keyword_to_string(kw: Embedded<Keyword>) -> String {
    kw.0.to_str().to_string()
}

#[define(name = "string->keyword", lib = "(srfi :88)")]
pub fn string_to_keyword(s: Embedded<String>) -> Value {
    Keyword::intern(&s)
}
