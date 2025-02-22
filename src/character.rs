use crate::{exception::Exception, gc::Gc, num::Number, registry::bridge, value::Value};
use unicode_categories::UnicodeCategories;

mod unicode;
use unicode::*;

fn char_switch_case<I: Iterator<Item = char> + ExactSizeIterator>(
    ch: char,
    operation: fn(char) -> I,
) -> Result<char, Exception> {
    let mut ch = operation(ch);
    let len = ch.len();
    if len == 1 {
        Ok(ch.next().unwrap())
    } else {
        Err(Exception::wrong_num_of_unicode_chars(1, len))
    }
}
fn char_switch_case_ref<'a, I: Iterator<Item = char> + ExactSizeIterator + 'a>(
    ch: &'a char,
    operation: fn(&'a char) -> I,
) -> Result<char, Exception> {
    let mut ch = operation(ch);
    let len = ch.len();
    if len == 1 {
        Ok(ch.next().unwrap())
    } else {
        Err(Exception::wrong_num_of_unicode_chars(1, len))
    }
}

#[bridge(name = "char->integer", lib = "(base)")]
pub async fn char_to_integer(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let ch = ch.read();
    let ch: char = ch.as_ref().try_into()?;

    Ok(vec![Gc::new(Value::Number(Number::FixedInteger(
        <char as Into<u32>>::into(ch).into(),
    )))])
}

#[bridge(name = "integer->char", lib = "(base)")]
pub async fn integer_to_char(int: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let int = int.read();
    let int: &Number = int.as_ref().try_into()?;
    let int: u64 = int.to_u64();
    if let Ok(int) = <u64 as TryInto<u32>>::try_into(int) {
        if let Some(ch) = char::from_u32(int) {
            return Ok(vec![Gc::new(Value::Character(ch))]);
        }
    }

    // char->integer returns a number larger than 0x10FFFF if integer is not an unicode scalar
    Ok(vec![Gc::new(Value::Number(Number::FixedInteger(
        0x10FFFF + 1,
    )))])
}

macro_rules! impl_char_operator {
    (
        $(($bridge_name:literal,
        $function_name:ident,
        $cmp_function:ident)),* $(,)?
    ) => {
        $(#[bridge(name = $bridge_name, lib = "(base)")]
        pub async fn $function_name(req_lhs: &Gc<Value>, req_rhs: &Gc<Value>, opt_chars: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Exception> {
            for window in [req_lhs, req_rhs]
                .into_iter()
                .chain(opt_chars)
                .map(|ch| {
                    let ch = ch.read();
                    ch.as_ref().try_into()
                })
                .collect::<Result<Vec<char>, Exception>>()?
                .windows(2) {

                if !window.first()
                    .and_then(|lhs| Some((lhs, window.get(1)?)))
                    .map(|(lhs, rhs)| lhs.$cmp_function(rhs))
                    .unwrap_or(true) {
                    return Ok(vec![Gc::new(Value::Boolean(false))]);
                }
            }

            Ok(vec![Gc::new(Value::Boolean(true))])
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
        $(#[bridge(name = $bridge_name, lib = "(base)")]
        pub async fn $function_name(req_lhs: &Gc<Value>, req_rhs: &Gc<Value>, opt_chars: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Exception> {
            for window in [req_lhs, req_rhs]
                .into_iter()
                .chain(opt_chars)
                .map(|ch| {
                    let ch = ch.read();
                    <&Value as TryInto<char>>::try_into(ch.as_ref())
                        .and_then(|c| char_switch_case_ref(&c, to_foldcase))
                })
                .collect::<Result<Vec<char>, Exception>>()?
                .windows(2) {

                if !window.first()
                    .and_then(|lhs| Some((lhs, window.get(1)?)))
                    .map(|(lhs, rhs)| lhs.$cmp_function(rhs))
                    .unwrap_or(true) {
                    return Ok(vec![Gc::new(Value::Boolean(false))]);
                }
            }

            Ok(vec![Gc::new(Value::Boolean(true))])
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
        pub async fn $function_name(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
            let ch = ch.read();
            let ch: char = ch.as_ref().try_into()?;
            Ok(vec![Gc::new(Value::Boolean(ch.$predicate()))])
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

#[bridge(name = "digit-value", lib = "(base)")]
pub async fn digit_value(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let ch = ch.read();
    let ch: char = ch.as_ref().try_into()?;

    Ok(vec![Gc::new(
        digit_to_num(ch)
            .map(Number::FixedInteger)
            .map(Value::Number)
            .unwrap_or(Value::Boolean(false)),
    )])
}

macro_rules! impl_char_case_converter {
    ($(($bridge_name:literal, $function_name:ident, $converter:expr)),* $(,)?) => {
        $(#[bridge(name = $bridge_name, lib = "(base)")]
        pub async fn $function_name(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
            let ch = ch.read();
            let ch: char = ch.as_ref().try_into()?;
            Ok(vec![Gc::new(Value::Character(char_switch_case(ch, $converter)?))])
        })*
    }
}
impl_char_case_converter![
    ("char-upcase", char_upcase, char::to_uppercase),
    ("char-downcase", char_downcase, char::to_lowercase),
];

#[bridge(name = "char-foldcase", lib = "(base)")]
pub async fn char_foldcase(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let ch = ch.read();
    let ch: &char = ch.as_ref().try_into()?;
    Ok(vec![Gc::new(Value::Character(char_switch_case_ref(
        ch,
        to_foldcase,
    )?))])
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
