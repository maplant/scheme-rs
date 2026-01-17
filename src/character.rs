use crate::{exceptions::Exception, registry::bridge, value::Value};
use either::Either;
use unicode_categories::UnicodeCategories;

mod unicode;
pub use unicode::*;

pub(crate) fn char_switch_case<I: Iterator<Item = char> + ExactSizeIterator>(
    ch: char,
    operation: fn(char) -> I,
) -> Either<char, Vec<char>> {
    let mut ch = operation(ch);
    let len = ch.len();
    if len == 1 {
        Either::Left(ch.next().unwrap())
    } else {
        Either::Right(ch.collect())
    }
}

#[bridge(name = "char->integer", lib = "(rnrs base builtins (6))")]
pub fn char_to_integer(ch: &Value) -> Result<Vec<Value>, Exception> {
    let ch: char = ch.clone().try_into()?;

    Ok(vec![Value::from(<char as Into<u32>>::into(ch))])
}

#[bridge(name = "integer->char", lib = "(rnrs base builtins (6))")]
pub fn integer_to_char(int: &Value) -> Result<Vec<Value>, Exception> {
    let int: usize = int.try_to_scheme_type()?;
    if let Ok(int) = <usize as TryInto<u32>>::try_into(int)
        && let Some(ch) = char::from_u32(int)
    {
        return Ok(vec![Value::from(ch)]);
    }

    // char->integer returns a number larger than 0x10FFFF if integer is not an unicode scalar
    Ok(vec![Value::from(0x10FFFF + 1)])
}

macro_rules! impl_char_operator {
    (
        $(($bridge_name:literal,
        $function_name:ident,
        $cmp_function:ident)),* $(,)?
    ) => {
        $(#[bridge(name = $bridge_name, lib = "(rnrs base builtins (6))")]
        pub fn $function_name(req_lhs: &Value, req_rhs: &Value, opt_chars: &[Value]) -> Result<Vec<Value>, Exception> {
            for window in [req_lhs, req_rhs]
                .into_iter()
                .chain(opt_chars)
                .map(|ch| {
                    ch.clone().try_into()
                })
                .collect::<Result<Vec<char>, Exception>>()?
                .windows(2) {

                if !window.first()
                    .and_then(|lhs| Some((lhs, window.get(1)?)))
                    .map(|(lhs, rhs)| lhs.$cmp_function(rhs))
                    .unwrap_or(true) {
                    return Ok(vec![Value::from(false)]);
                }
            }

            Ok(vec![Value::from(true)])
        })*
    }
}

impl_char_operator![
    ("char=?", char_eq, eq),
    ("char<?", char_lt, lt),
    ("char>?", char_gt, gt),
    ("char>=?", char_ge, ge),
    ("char<=?", char_le, le),
];

macro_rules! impl_char_ci_operator {
    (
        $(($bridge_name:literal,
        $function_name:ident,
        $cmp_function:ident)),* $(,)?
    ) => {
        $(#[bridge(name = $bridge_name, lib = "(rnrs base builtins (6))")]
        pub fn $function_name(req_lhs: &Value, req_rhs: &Value, opt_chars: &[Value]) -> Result<Vec<Value>, Exception> {
            for window in [req_lhs, req_rhs]
                .into_iter()
                .chain(opt_chars)
                .map(|ch| {
                    let ch: char = ch.clone().try_into()?;
                    Ok(char_switch_case(ch, to_foldcase).left_or(ch))
                })
                .collect::<Result<Vec<char>, Exception>>()?
                .windows(2) {

                if !window.first()
                    .and_then(|lhs| Some((lhs, window.get(1)?)))
                    .map(|(lhs, rhs)| lhs.$cmp_function(rhs))
                    .unwrap_or(true) {
                    return Ok(vec![Value::from(false)]);
                }
            }

            Ok(vec![Value::from(true)])
        })*
    }
}

impl_char_ci_operator![
    ("char-ci-=?", char_ci_eq, eq),
    ("char-ci-<?", char_ci_lt, lt),
    ("char-ci->?", char_ci_gt, gt),
    ("char-ci->=?", char_ci_ge, ge),
    ("char-ci-<=?", char_ci_le, le),
];

macro_rules! impl_char_predicate {
    ($(($bridge_name:literal, $function_name:ident, $predicate:ident)),* $(,)?) => {
        $(#[bridge(name = $bridge_name, lib = "(base)")]
        pub fn $function_name(ch: &Value) -> Result<Vec<Value>, Exception> {
            let ch: char = ch.clone().try_into()?;
            Ok(vec![Value::from(ch.$predicate())])
        })*
    }
}

impl_char_predicate![
    ("char-alphabetic?", char_is_alphabetic, is_ascii_alphabetic),
    ("char-numeric?", char_is_numeric, is_number_decimal_digit),
    ("char-whitespace?", char_is_whitespace, is_whitespace),
    ("char-upper?", char_is_uppercase, is_uppercase),
    ("char-lower?", char_is_lowercase, is_lowercase),
];

#[bridge(name = "digit-value", lib = "(rnrs base builtins (6))")]
pub fn digit_value(ch: &Value) -> Result<Vec<Value>, Exception> {
    let ch: char = ch.clone().try_into()?;

    Ok(vec![
        digit_to_num(ch).map_or_else(|| Value::from(false), Value::from),
    ])
}

macro_rules! impl_char_case_converter {
    ($(($bridge_name:literal, $function_name:ident, $converter:expr_2021)),* $(,)?) => {
        $(#[bridge(name = $bridge_name, lib = "(rnrs base builtins (6))")]
        pub fn $function_name(ch: &Value) -> Result<Vec<Value>, Exception> {
            let ch: char = ch.clone().try_into()?;
            Ok(vec![Value::from(char_switch_case(ch, $converter).left_or(ch))])
        })*
    }
}

impl_char_case_converter![
    ("char-upcase", char_upcase, char::to_uppercase),
    ("char-downcase", char_downcase, char::to_lowercase),
];

#[bridge(name = "char-foldcase", lib = "(rnrs base builtins (6))")]
pub fn char_foldcase(ch: &Value) -> Result<Vec<Value>, Exception> {
    let ch: char = ch.clone().try_into()?;
    Ok(vec![Value::from(
        char_switch_case(ch, to_foldcase).left_or(ch),
    )])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_digit_to_num() {
        (char::MIN..char::MAX)
            .filter(|c| c.is_number_decimal_digit())
            .map(digit_to_num)
            .for_each(|d| assert!(d.is_some()));
    }
}
