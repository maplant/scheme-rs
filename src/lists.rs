use crate::{
    exception::Condition,
    gc::{Gc, Trace},
    num::Number,
    registry::bridge,
    value::{UnpackedValue, Value},
};
use std::fmt;

/// A pair of scheme values. Has a head and tail.
#[derive(Trace)]
pub struct Pair(pub Value, pub Value);

impl Pair {
    pub fn new(car: Value, cdr: Value) -> Self {
        Self(car, cdr)
    }
}

impl PartialEq for Pair {
    fn eq(&self, rhs: &Self) -> bool {
        // TODO: Avoid circular lists causing an infinite loop
        self.0 == rhs.0 && self.1 == rhs.1
    }
}

pub fn display_list(car: &Value, cdr: &Value, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // TODO(map): If the list is circular, DO NOT print infinitely!
    match &*cdr.unpacked_ref() {
        UnpackedValue::Pair(_) | UnpackedValue::Null => (),
        cdr => {
            // This is not a proper list
            return write!(f, "({car} . {cdr})");
        }
    }

    write!(f, "({car}")?;

    let mut stack = vec![cdr.clone()];

    while let Some(head) = stack.pop() {
        match &*head.unpacked_ref() {
            UnpackedValue::Null => {
                if !stack.is_empty() {
                    write!(f, " ()")?;
                }
            }
            UnpackedValue::Pair(pair) => {
                let pair_read = pair.read();
                let Pair(car, cdr) = pair_read.as_ref();
                write!(f, " {car}")?;
                stack.push(cdr.clone());
            }
            x => {
                write!(f, " {x}")?;
            }
        }
    }

    write!(f, ")")
}

pub fn debug_list(car: &Value, cdr: &Value, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // TODO(map): If the list is circular, DO NOT print infinitely!
    match &*cdr.unpacked_ref() {
        UnpackedValue::Pair(_) | UnpackedValue::Null => (),
        cdr => {
            // This is not a proper list
            return write!(f, "({car:?} . {cdr:?})");
        }
    }

    write!(f, "({car:?}")?;

    let mut stack = vec![cdr.clone()];

    while let Some(head) = stack.pop() {
        match &*head.unpacked_ref() {
            UnpackedValue::Null => {
                if !stack.is_empty() {
                    write!(f, " ()")?;
                }
            }
            UnpackedValue::Pair(pair) => {
                let pair_read = pair.read();
                let Pair(car, cdr) = pair_read.as_ref();
                write!(f, " {car:?}")?;
                stack.push(cdr.clone());
            }
            x => {
                write!(f, " {x:?}")?;
            }
        }
    }

    write!(f, ")")
}

pub fn slice_to_list(items: &[Value]) -> Value {
    match items {
        [] => Value::null(),
        [head, tail @ ..] => Value::from(Gc::new(Pair(head.clone(), slice_to_list(tail)))),
    }
}

pub fn list_to_vec(curr: &Value, out: &mut Vec<Value>) {
    match &*curr.unpacked_ref() {
        UnpackedValue::Pair(pair) => {
            let pair_read = pair.read();
            let Pair(car, cdr) = pair_read.as_ref();
            out.push(car.clone());
            list_to_vec(cdr, out);
        }
        UnpackedValue::Null => (),
        _ => out.push(curr.clone()),
    }
}

pub fn list_to_vec_with_null(curr: &Value, out: &mut Vec<Value>) {
    match &*curr.unpacked_ref() {
        UnpackedValue::Pair(pair) => {
            let pair_read = pair.read();
            let Pair(car, cdr) = pair_read.as_ref();
            out.push(car.clone());
            list_to_vec_with_null(cdr, out);
        }
        _ => out.push(curr.clone()),
    }
}

#[bridge(name = "list", lib = "(base)")]
pub async fn list(args: &[Value]) -> Result<Vec<Value>, Condition> {
    // Construct the list in reverse
    let mut cdr = Value::null();
    for arg in args.iter().rev() {
        cdr = Value::from(Gc::new(Pair(arg.clone(), cdr)));
    }
    Ok(vec![cdr])
}

#[bridge(name = "cons", lib = "(base)")]
pub async fn cons(car: &Value, cdr: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(Gc::new(Pair(car.clone(), cdr.clone())))])
}

#[bridge(name = "car", lib = "(base)")]
pub async fn car(val: &Value) -> Result<Vec<Value>, Condition> {
    let pair: Gc<Pair> = val.clone().try_into()?;
    let pair_read = pair.read();
    let Pair(car, _) = pair_read.as_ref();
    Ok(vec![car.clone()])
}

#[bridge(name = "cdr", lib = "(base)")]
pub async fn cdr(val: &Value) -> Result<Vec<Value>, Condition> {
    let pair: Gc<Pair> = val.clone().try_into()?;
    let pair_read = pair.read();
    let Pair(_, cdr) = pair_read.as_ref();
    Ok(vec![cdr.clone()])
}

#[bridge(name = "set-car!", lib = "(base)")]
pub async fn set_car(var: &Value, val: &Value) -> Result<Vec<Value>, Condition> {
    let pair: Gc<Pair> = var.clone().try_into()?;
    let mut pair_write = pair.write();
    let Pair(ref mut car, _) = pair_write.as_mut();
    *car = val.clone();
    Ok(Vec::new())
}

#[bridge(name = "set-cdr!", lib = "(base)")]
pub async fn set_cdr(var: &Value, val: &Value) -> Result<Vec<Value>, Condition> {
    let pair: Gc<Pair> = var.clone().try_into()?;
    let mut pair_write = pair.write();
    let Pair(_, ref mut cdr) = pair_write.as_mut();
    *cdr = val.clone();
    Ok(Vec::new())
}

#[bridge(name = "length", lib = "(base)")]
pub async fn length(arg: &Value) -> Result<Vec<Value>, Condition> {
    let mut length = 0;
    let mut arg = arg.clone();
    loop {
        arg = {
            match &*arg.unpacked_ref() {
                UnpackedValue::Pair(pair) => {
                    let pair_read = pair.read();
                    let Pair(_, cdr) = pair_read.as_ref();
                    cdr.clone()
                }
                _ => break,
            }
        };
        length += 1;
    }
    Ok(vec![Value::from(Number::from(length))])
}

/*
#[bridge(name = "list->vector", lib = "(base)")]
pub async fn list_to_vector(list: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let mut vec = Vec::new();
    list_to_vec(list, &mut vec);

    Ok(vec![Gc::new(Value::Vector(
        vec.into_iter().map(|i| i.read().as_ref().clone()).collect(),
    ))])
}
*/
