use crate::{
    exception::Exception, gc::Gc, lists::slice_to_list, num::Number, registry::bridge, value::Value,
};
use rug::Integer;

#[bridge(name = "char->integer", lib = "(base)")]
pub async fn char_to_integer(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let ch = ch.read();
    let ch: char = ch.as_ref().try_into()?;

    Ok(vec![Gc::new(Value::Number(Number::FixedInteger(<char as Into<u32>>::into(ch).into())))])
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
    Ok(vec![Gc::new(Value::Number(Number::FixedInteger(0x10FFFF + 1)))])
}

macro_rules! impl_char_operator {
    (
        $bridge_name:literal,
        $function_name:ident,
        $cmp_function:ident $(,)?
    ) => {
        #[bridge(name = $bridge_name, lib = "(base)")]
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
        }
    }
}
macro_rules! impl_char_ci_operator {
    (
        $bridge_name:literal,
        $function_name:ident,
        $cmp_function:ident $(,)?
    ) => {
        #[bridge(name = $bridge_name, lib = "(base)")]
        pub async fn $function_name(req_lhs: &Gc<Value>, req_rhs: &Gc<Value>, opt_chars: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Exception> {
            for window in [req_lhs, req_rhs]
                .into_iter()
                .chain(opt_chars)
                .map(|ch| {
                    let ch = ch.read();
                    <&Value as TryInto<char>>::try_into(ch.as_ref())
                        .and_then(|ch| {
                            let mut ch = ch.to_lowercase();
                            let len = ch.len();
                            if len == 1 {
                                Err(Exception::wrong_num_of_unicode_chars(1, len))
                            } else {
                                Ok(ch.next().unwrap())
                            }
                        })
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
        }
    }
}

impl_char_operator!("char=?",  char_eq, eq);
impl_char_operator!("char<?",  char_lt, lt);
impl_char_operator!("char>?",  char_gt, gt);
impl_char_operator!("char>=?", char_ge, ge);
impl_char_operator!("char<=?", char_le, le);

impl_char_ci_operator!("char-ci-=?",  char_ci_eq, eq);
impl_char_ci_operator!("char-ci-<?",  char_ci_lt, lt);
impl_char_ci_operator!("char-ci->?",  char_ci_gt, gt);
impl_char_ci_operator!("char-ci->=?", char_ci_ge, ge);
impl_char_ci_operator!("char-ci-<=?", char_ci_le, le);
