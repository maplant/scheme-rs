use std::collections::HashMap;
use std::sync::{Arc, LazyLock, Mutex};

use scheme_rs_macros::{Trace, bridge};

use crate::{
    exceptions::Exception,
    records::{RecordTypeDescriptor, SchemeCompatible, rtd},
    strings::WideString,
    symbols::Symbol,
    value::Value,
};

#[derive(Debug, Clone, Trace)]
pub struct Keyword(Symbol);

static INTERNED: LazyLock<Mutex<HashMap<Symbol, Value>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

impl Keyword {
    pub fn intern(name: &str) -> Value {
        let sym = Symbol::intern(name);
        let mut cache = INTERNED.lock().unwrap();
        cache
            .entry(sym)
            .or_insert_with(|| Value::from_rust_type(Keyword(sym)))
            .clone()
    }
}

impl SchemeCompatible for Keyword {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "keyword", sealed: true, opaque: true)
    }
}

#[bridge(name = "keyword?", lib = "(srfi :88)")]
pub fn keyword_pred(obj: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        obj.cast_to_rust_type::<Keyword>().is_some(),
    )])
}

#[bridge(name = "keyword->string", lib = "(srfi :88)")]
pub fn keyword_to_string(obj: &Value) -> Result<Vec<Value>, Exception> {
    let kw = obj.try_to_rust_type::<Keyword>()?;
    Ok(vec![Value::from(kw.0.to_str().to_string())])
}

#[bridge(name = "string->keyword", lib = "(srfi :88)")]
pub fn string_to_keyword(s: &Value) -> Result<Vec<Value>, Exception> {
    let s: WideString = s.clone().try_into()?;
    Ok(vec![Keyword::intern(&s.to_string())])
}
