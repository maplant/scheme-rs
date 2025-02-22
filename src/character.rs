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
