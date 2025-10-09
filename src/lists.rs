use indexmap::IndexMap;

use crate::{
    exceptions::Condition,
    gc::{Gc, Trace},
    num::Number,
    registry::bridge,
    syntax::Syntax,
    value::{write_value, EqvValue, UnpackedValue, Value, ValueType},
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

pub(crate) fn write_list(
    car: &Value,
    cdr: &Value,
    fmt: fn(&Value, &mut IndexMap<EqvValue, bool>, &mut fmt::Formatter<'_>) -> fmt::Result,
    circular_values: &mut IndexMap<EqvValue, bool>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    match cdr.type_of() {
        ValueType::Pair | ValueType::Null => (),
        _ => {
            // This is not a proper list
            write!(f, "(")?;
            write_value(car, fmt, circular_values, f)?;
            write!(f, " . ")?;
            write_value(cdr, fmt, circular_values, f)?;
            write!(f, ")")?;
            return Ok(());
        }
    }

    write!(f, "(")?;
    write_value(car, fmt, circular_values, f)?;
    let mut stack = vec![cdr.clone()];

    while let Some(head) = stack.pop() {
        if let Some((idx, _, seen)) = circular_values.get_full_mut(&EqvValue(head.clone())) {
            if *seen {
                write!(f, " . #{idx}#")?;
                continue;
            } else {
                write!(f, " #{idx}=")?;
                *seen = true;
            }
        }
        match &*head.unpacked_ref() {
            UnpackedValue::Null => {
                if !stack.is_empty() {
                    write!(f, " ()")?;
                }
            }
            UnpackedValue::Pair(pair) => {
                let pair_read = pair.read();
                let Pair(car, cdr) = pair_read.as_ref();
                write!(f, " ")?;
                write_value(car, fmt, circular_values, f)?;
                // write!(f, " {car}")?;
                stack.push(cdr.clone());
            }
            x => {
                let val = x.clone().into_value();
                write!(f, " ")?;
                write_value(&val, fmt, circular_values, f)?;
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

#[bridge(name = "list", lib = "(rnrs base builtins (6))")]
pub async fn list(args: &[Value]) -> Result<Vec<Value>, Condition> {
    // Construct the list in reverse
    let mut cdr = Value::null();
    for arg in args.iter().rev() {
        cdr = Value::from(Gc::new(Pair(arg.clone(), cdr)));
    }
    Ok(vec![cdr])
}

#[bridge(name = "cons", lib = "(rnrs base builtins (6))")]
pub async fn cons(car: &Value, cdr: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(Gc::new(Pair(car.clone(), cdr.clone())))])
}

#[bridge(name = "car", lib = "(rnrs base builtins (6))")]
pub async fn car(val: &Value) -> Result<Vec<Value>, Condition> {
    match val.clone().unpack() {
        UnpackedValue::Pair(pair) => {
            let pair_read = pair.read();
            let Pair(car, _) = pair_read.as_ref();
            Ok(vec![car.clone()])
        }
        UnpackedValue::Syntax(syn) if syn.is_list() => {
            let Some([car, ..]) = syn.as_list() else {
                unreachable!()
            };
            Ok(vec![Value::from(car.clone())])
        }
        _ => Err(Condition::type_error("list", val.type_name())),
    }
}

#[bridge(name = "cdr", lib = "(rnrs base builtins (6))")]
pub async fn cdr(val: &Value) -> Result<Vec<Value>, Condition> {
    match val.clone().unpack() {
        UnpackedValue::Pair(pair) => {
            let pair_read = pair.read();
            let Pair(_, cdr) = pair_read.as_ref();
            Ok(vec![cdr.clone()])
        }
        UnpackedValue::Syntax(syn) if syn.is_list() => match syn.as_list() {
            Some([_, null @ Syntax::Null { .. }]) => Ok(vec![Value::from(null.clone())]),
            Some([_, cdr @ ..]) => Ok(vec![Value::from(Syntax::List {
                list: cdr.to_vec(),
                span: syn.span().clone(),
            })]),
            _ => unreachable!(),
        },
        _ => Err(Condition::type_error("list", val.type_name())),
    }
}

#[bridge(name = "set-car!", lib = "(rnrs base builtins (6))")]
pub async fn set_car(var: &Value, val: &Value) -> Result<Vec<Value>, Condition> {
    let pair: Gc<Pair> = var.clone().try_into()?;
    let mut pair_write = pair.write();
    let Pair(car, _) = pair_write.as_mut();
    *car = val.clone();
    Ok(Vec::new())
}

#[bridge(name = "set-cdr!", lib = "(rnrs base builtins (6))")]
pub async fn set_cdr(var: &Value, val: &Value) -> Result<Vec<Value>, Condition> {
    let pair: Gc<Pair> = var.clone().try_into()?;
    let mut pair_write = pair.write();
    let Pair(_, cdr) = pair_write.as_mut();
    *cdr = val.clone();
    Ok(Vec::new())
}

#[bridge(name = "length", lib = "(rnrs base builtins (6))")]
pub async fn length_builtin(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(Number::from(length(arg)?))])
}

pub fn length(arg: &Value) -> Result<usize, Condition> {
    let mut length = 0usize;
    let mut arg = arg.clone();
    loop {
        arg = {
            match &*arg.unpacked_ref() {
                UnpackedValue::Pair(pair) => {
                    let pair_read = pair.read();
                    let Pair(_, cdr) = pair_read.as_ref();
                    cdr.clone()
                }
                UnpackedValue::Null => break,
                _ => return Err(Condition::error("list must be proper".to_string())),
            }
        };
        length += 1;
    }
    Ok(length)
}

#[bridge(name = "list->vector", lib = "(rnrs base builtins (6))")]
pub async fn list_to_vector(list: &Value) -> Result<Vec<Value>, Condition> {
    let mut vec = Vec::new();
    list_to_vec(list, &mut vec);

    Ok(vec![Value::from(vec)])
}
