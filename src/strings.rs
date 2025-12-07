//! String builtins and data types

use std::{fmt, hash::Hash, sync::Arc};

use parking_lot::RwLock;

use crate::{
    exceptions::Condition,
    gc::Trace,
    registry::bridge,
    value::{Value, ValueType},
};

#[repr(align(16))]
pub(crate) struct WideStringInner {
    chars: RwLock<Vec<char>>,
    mutable: bool,
}

impl Hash for WideStringInner {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.chars.read().hash(state);
    }
}

/// A string that is a vector of characters, rather than a vector bytes encoding
/// a utf-8 string. This is because R6RS mandates O(1) lookups of character
/// indices.
#[derive(Clone, Trace, Hash)]
pub struct WideString(pub(crate) Arc<WideStringInner>);

impl WideString {
    pub fn new(s: impl fmt::Display) -> Self {
        Self::from(s.to_string())
    }
}

impl From<String> for WideString {
    fn from(value: String) -> Self {
        Self(Arc::new(WideStringInner {
            chars: RwLock::new(value.chars().collect()),
            mutable: false,
        }))
    }
}

impl From<WideString> for String {
    fn from(value: WideString) -> Self {
        value.0.chars.read().iter().copied().collect()
    }
}

impl PartialEq for WideString {
    fn eq(&self, rhs: &Self) -> bool {
        &*self.0.chars.read() == &*rhs.0.chars.read()
    }
}

impl PartialEq<str> for WideString {
    fn eq(&self, rhs: &str) -> bool {
        self.0.chars.read().iter().copied().eq(rhs.chars())
    }
}

impl fmt::Display for WideString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for char in &*self.0.chars.read() {
            write!(f, "{char}")?;
        }
        Ok(())
    }
}

impl fmt::Debug for WideString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"")?;
        for char in self
            .0
            .chars
            .read()
            .iter()
            .map(|chr| chr.escape_debug())
            .flatten()
        {
            write!(f, "{char}")?;
        }
        write!(f, "\"")
    }
}

#[bridge(name = "string?", lib = "(rnrs base builtins (6))")]
pub fn string_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::String)])
}

#[bridge(name = "make-string", lib = "(rnrs base builtins (6))")]
pub fn make_string(k: &Value, chr: &[Value]) -> Result<Vec<Value>, Condition> {
    let chr: char = match chr {
        [] => '\0',
        [chr] => chr.clone().try_into()?,
        x => return Err(Condition::wrong_num_of_args(2, 1 + x.len())),
    };
    let k: usize = k.clone().try_into()?;
    let ret = Value::from(WideString(Arc::new(WideStringInner {
        chars: RwLock::new(std::iter::repeat_n(chr, k).collect()),
        mutable: true,
    })));
    Ok(vec![ret])
}

#[bridge(name = "string", lib = "(rnrs base builtins (6))")]
pub fn string(char: &Value, chars: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(WideString(Arc::new(WideStringInner {
        chars: RwLock::new(
            Some(char)
                .into_iter()
                .chain(chars.iter())
                .cloned()
                .map(Value::try_into)
                .collect::<Result<Vec<char>, _>>()?,
        ),
        mutable: true,
    })))])
}

#[bridge(name = "string-ref", lib = "(rnrs base builtins (6))")]
pub fn string_ref(string: &Value, k: &Value) -> Result<Vec<Value>, Condition> {
    let string: WideString = string.clone().try_into()?;
    let k: usize = k.clone().try_into()?;
    let chars = string.0.chars.read();
    if k >= chars.len() {
        return Err(Condition::invalid_index(k, chars.len()));
    }
    Ok(vec![Value::from(chars[k])])
}

#[bridge(name = "string=?", lib = "(rnrs base builtins (6))")]
pub fn string_eq_pred(
    string_1: &Value,
    string_2: &Value,
    string_n: &[Value],
) -> Result<Vec<Value>, Condition> {
    let string_1: WideString = string_1.clone().try_into()?;
    let string_1_chars = string_1.0.chars.read();
    for string_n in Some(string_2).into_iter().chain(string_n.iter()).cloned() {
        let string_n: WideString = string_n.try_into()?;
        if &*string_1_chars != &*string_n.0.chars.read() {
            return Ok(vec![Value::from(false)]);
        }
    }
    Ok(vec![Value::from(true)])
}

#[bridge(name = "string-append", lib = "(rnrs base builtins (6))")]
pub fn list(args: &[Value]) -> Result<Vec<Value>, Condition> {
    let mut output = String::new();
    for arg in args.iter().cloned() {
        let arg: String = arg.try_into()?;
        output += arg.as_str();
    }
    Ok(vec![Value::from(output)])
}

#[bridge(name = "string->vector", lib = "(rnrs base builtins (6))")]
pub fn string_to_vector(from: &Value, range: &[Value]) -> Result<Vec<Value>, Condition> {
    let string: WideString = from.clone().try_into()?;

    let len = string.0.chars.read().len();
    let start: usize = range
        .first()
        .cloned()
        .map(Value::try_into)
        .transpose()?
        .unwrap_or(0);
    let end: usize = range
        .get(1)
        .cloned()
        .map(Value::try_into)
        .transpose()?
        .unwrap_or(len);

    if end < start {
        return Err(Condition::error(format!(
            "Range end {end} cannot be less than start {start}",
        )));
    } else if end > len {
        return Err(Condition::invalid_range(start..end, len));
    }

    Ok(vec![Value::from(
        string.0.chars.read()[start..end]
            .iter()
            .copied()
            .map(Value::from)
            .collect::<Vec<_>>(),
    )])
}

#[bridge(name = "string-set!", lib = "(rnrs mutable-strings (6))")]
pub fn string_set_bang(string: &Value, k: &Value, chr: &Value) -> Result<Vec<Value>, Condition> {
    let string: WideString = string.clone().try_into()?;
    let k: usize = k.clone().try_into()?;
    let chr: char = chr.clone().try_into()?;
    if !string.0.mutable {
        return Err(Condition::error("string is immutable"));
    }
    let mut chars = string.0.chars.write();
    if k >= chars.len() {
        return Err(Condition::invalid_index(k, chars.len()));
    }
    chars[k] = chr;
    Ok(vec![])
}
