//! String builtins and data types.
//!
//! The R6RS standard mandates that scheme strings must be essentially vectors
//! of codepoints. This is an obvious incompatability with Rust, which mandates
//! strings to be UTF-8 encoded byte arrays.
//!
//! We reject the R6RS mandate and all features that depend on it, such as
//! custom textual ports. In scheme-rs, strings are Rust strings.

use std::{fmt, iter, sync::Arc};

use indexmap::IndexSet;
use scheme_rs_macros::rtd;

use crate::{
    Either, Result,
    character::{char_switch_case, to_foldcase},
    exceptions::Exception,
    fmt::{Formatter, Mode},
    lists::iter_list,
    records::{Embeddable, Embedded, Record, RecordTypeDescriptor},
    registry::bridge,
    value::Value,
};

unsafe impl Embeddable for String {
    fn rtd() -> Arc<RecordTypeDescriptor>
    where
        Self: Sized,
    {
        rtd! {
            ty: String,
            name: "string",
            sealed: true,
            opaque: true,
        }
    }

    fn fmt(&self, _record: &Record, fmt: &mut Formatter<'_>) -> fmt::Result {
        match fmt.mode() {
            Mode::Display => write!(fmt, "{self}"),
            Mode::Write => write!(fmt, "{self:?}"),
        }
    }

    fn equal(&self, rhs: &Record) -> bool
    where
        Self: Sized,
    {
        let Some(rhs) = rhs.cast::<String>() else {
            return false;
        };
        self == rhs.as_ref()
    }

    fn equal_hash(&self, _recursive: &mut IndexSet<Value>, hasher: &mut dyn std::hash::Hasher)
    where
        Self: Sized,
    {
        hasher.write_usize(self.len());
        hasher.write(self.as_bytes());
    }
}

#[bridge(name = "string?", lib = "(rnrs base builtins (6))")]
pub fn string_pred(arg: Value) -> bool {
    arg.is_a::<Embedded<String>>()
}

#[bridge(name = "make-string", lib = "(rnrs base builtins (6))")]
pub fn make_string(k: usize, chr: Option<char>) -> Result<String> {
    let chr = chr.unwrap_or('\0');
    Ok(iter::repeat_n(chr, k).collect::<String>())
}

#[bridge(name = "string", lib = "(rnrs base builtins (6))")]
pub fn string(chr: Value, #[rest_args] chars: Value) -> Result<String> {
    [chr.clone()]
        .into_iter()
        .chain(iter_list(&chars))
        .map(|x| -> Result<char> { x.try_into() })
        .collect()
}

#[bridge(name = "string-length", lib = "(rnrs base builtins (6))")]
pub fn string_length(s: Embedded<String>) -> usize {
    s.chars().count()
}

#[bridge(name = "string-ref", lib = "(rnrs base builtins (6))")]
pub fn string_ref(string: Embedded<String>, k: usize) -> Result<char> {
    string
        .chars()
        .nth(k)
        .ok_or_else(|| Exception::error("out of range"))
}

#[bridge(name = "string=?", lib = "(rnrs base builtins (6))")]
pub fn is_string_eq(
    string_1: Embedded<String>,
    string_2: Value,
    #[rest_args] string_n: Value,
) -> Result<bool> {
    for string_n in [string_2.clone()].into_iter().chain(iter_list(&string_n)) {
        let string_n: Embedded<String> = string_n.try_into()?;
        if *string_1 != *string_n {
            return Ok(false);
        }
    }
    Ok(true)
}

#[bridge(name = "string<?", lib = "(rnrs base builtins (6))")]
pub fn is_string_less(
    string_1: Embedded<String>,
    string_2: Value,
    #[rest_args] string_n: Value,
) -> Result<bool> {
    let mut prev_string = string_1;
    for string_n in [string_2.clone()].into_iter().chain(iter_list(&string_n)) {
        let string_n: Embedded<String> = string_n.try_into()?;
        if *prev_string >= *string_n {
            return Ok(false);
        }
        prev_string = string_n;
    }
    Ok(true)
}

#[bridge(name = "string>?", lib = "(rnrs base builtins (6))")]
pub fn is_string_greater(
    string_1: Embedded<String>,
    string_2: Value,
    #[rest_args] string_n: Value,
) -> Result<bool> {
    let mut prev_string = string_1;
    for string_n in [string_2.clone()].into_iter().chain(iter_list(&string_n)) {
        let string_n: Embedded<String> = string_n.try_into()?;
        if *prev_string <= *string_n {
            return Ok(false);
        }
        prev_string = string_n;
    }
    Ok(true)
}

#[bridge(name = "string<=?", lib = "(rnrs base builtins (6))")]
pub fn is_string_less_equal(
    string_1: Embedded<String>,
    string_2: Value,
    #[rest_args] string_n: Value,
) -> Result<bool> {
    let mut prev_string = string_1;
    for string_n in [string_2.clone()].into_iter().chain(iter_list(&string_n)) {
        let string_n: Embedded<String> = string_n.try_into()?;
        if *prev_string > *string_n {
            return Ok(false);
        }
        prev_string = string_n;
    }
    Ok(true)
}

#[bridge(name = "string>=?", lib = "(rnrs base builtins (6))")]
pub fn is_string_greater_equal(
    string_1: Embedded<String>,
    string_2: Value,
    #[rest_args] string_n: Value,
) -> Result<bool> {
    let mut prev_string = string_1;
    for string_n in [string_2.clone()].into_iter().chain(iter_list(&string_n)) {
        let string_n: Embedded<String> = string_n.try_into()?;
        if *prev_string < *string_n {
            return Ok(false);
        }
        prev_string = string_n;
    }
    Ok(true)
}

#[bridge(name = "substring", lib = "(rnrs base builtins (6))")]
pub fn substring(string: Embedded<String>, start: usize, end: usize) -> Result<String> {
    if start > end {
        return Err(Exception::error(format!(
            "start {start} is greater than end {end}"
        )));
    }
    Ok(string.chars().skip(start).take(end).collect())
}

#[bridge(name = "string-append", lib = "(rnrs base builtins (6))")]
pub fn list(#[rest_args] args: Value) -> Result<String> {
    let mut output = String::new();
    for arg in iter_list(&args) {
        let arg: Embedded<String> = arg.try_into()?;
        output += arg.as_str();
    }
    Ok(output)
}

#[bridge(name = "string->list", lib = "(rnrs base builtins (6))")]
pub fn string_to_list(string: Embedded<String>) -> Value {
    let mut list = Value::null();
    for chr in string.chars().rev() {
        list = Value::cons(chr, list);
    }
    list
}

#[bridge(name = "string-copy", lib = "(rnrs base builtins (6))")]
pub fn string_copy(string: Embedded<String>) -> String {
    string.as_ref().clone()
}

#[bridge(name = "string->vector", lib = "(rnrs base builtins (6))")]
pub fn string_to_vector(
    from: Embedded<String>,
    start: Option<usize>,
    end: Option<usize>,
) -> Result<Vec<Value>> {
    let chars = from.chars().collect::<Vec<_>>();
    let len = chars.len();
    let start = start.unwrap_or(0);
    let end = end.unwrap_or(len);

    if end < start {
        return Err(Exception::error(format!(
            "range end {end} is less than start {start}",
        )));
    } else if end > len {
        return Err(Exception::invalid_range(start..end, len));
    }

    Ok(chars
        .into_iter()
        .skip(start)
        .take(end)
        .map(Value::from)
        .collect())
}

#[bridge(name = "string-set!", lib = "(rnrs mutable-strings (6))")]
pub fn string_set_bang(_string: Embedded<String>, _k: usize, _chr: char) -> Result<()> {
    Err(Exception::implementation_restriction(
        "string-set! is not available",
    ))
}

#[bridge(name = "string-foldcase", lib = "(rnrs base builtins (6))")]
pub fn string_foldcase(string: Embedded<String>) -> String {
    string
        .chars()
        .flat_map(|ch| match char_switch_case(ch, to_foldcase) {
            Either::Left(ch) => vec![ch],
            Either::Right(s) => s,
        })
        .collect::<String>()
}
