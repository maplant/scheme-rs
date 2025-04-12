use crate::{exception::Condition, gc::{Trace, Gc}, num::Number, registry::bridge, value::Value};
use std::fmt;

/// A pair of scheme values. Has a head and tail.
#[derive(Trace)]
pub struct Pair(Value, Value);

pub fn display_list(car: &Value, cdr: &Value, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    /*
    // TODO(map): If the list is circular, DO NOT print infinitely!
    match &*cdr.read() {
        Value::Pair(_, _) | Value::Null => (),
        cdr => {
            // This is not a proper list
            return write!(f, "({car} . {cdr})");
        }
    }

    write!(f, "({car}")?;

    let mut stack = vec![cdr.clone()];

    while let Some(head) = stack.pop() {
        match &*head.read() {
            Value::Null => {
                if !stack.is_empty() {
                    write!(f, " ()")?;
                }
            }
            Value::Pair(car, cdr) => {
                write!(f, " {car}")?;
                stack.push(cdr.clone());
            }
            x => {
                write!(f, " {x}")?;
            }
        }
    }

    write!(f, ")")
     */
    todo!()
}

pub fn debug_list(car: &Value, cdr: &Value, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    /*
    // TODO(map): If the list is circular, DO NOT print infinitely!
    match &*cdr.read() {
        Value::Pair(_, _) | Value::Null => (),
        cdr => {
            // This is not a proper list
            return write!(f, "({car:?} . {cdr:?})");
        }
    }

    write!(f, "({car:?}")?;

    let mut stack = vec![cdr.clone()];

    while let Some(head) = stack.pop() {
        match &*head.read() {
            Value::Null => {
                if !stack.is_empty() {
                    write!(f, " ()")?;
                }
            }
            Value::Pair(car, cdr) => {
                write!(f, " {car:?}")?;
                stack.push(cdr.clone());
            }
            x => {
                write!(f, " {x:?}")?;
            }
        }
    }

    write!(f, ")")
     */
    todo!()
}

pub fn slice_to_list(items: &[Value]) -> Value {
    /*
    match items {
        [] => Value::null(),
        [head, tail @ ..] => Value::new((head.clone(), slice_to_list(tail))),
    }
     */
    todo!()
}

pub fn list_to_vec(curr: &Value, out: &mut Vec<Value>) {
    /*
    let val = curr.read();
    match &*val {
        Value::Pair(a, b) => {
            out.push(a.clone());
            list_to_vec(b, out);
        }
        Value::Null => (),
        _ => out.push(curr.clone()),
    }
     */
}

pub fn list_to_vec_with_null(curr: &Value, out: &mut Vec<Value>) {
    /*
    let val = curr.read();
    match &*val {
        Value::Pair(a, b) => {
            out.push(a.clone());
            list_to_vec_with_null(b, out);
        }
        _ => out.push(curr.clone()),
    }
     */
    todo!()
}

/*
#[bridge(name = "list", lib = "(base)")]
pub async fn list(args: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Condition> {
    // Construct the list in reverse
    let mut cdr = Gc::new(Value::Null);
    for arg in args.iter().rev() {
        cdr = Gc::new(Value::Pair(arg.clone(), cdr.clone()));
    }
    Ok(vec![cdr])
}

#[bridge(name = "cons", lib = "(base)")]
pub async fn cons(car: &Gc<Value>, cdr: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let car = Gc::new(car.read().clone());
    let cdr = Gc::new(cdr.read().clone());
    Ok(vec![Gc::new(Value::Pair(car, cdr))])
}

#[bridge(name = "car", lib = "(base)")]
pub async fn car(val: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let val = val.read();
    match &*val {
        Value::Pair(car, _cdr) => Ok(vec![car.clone()]),
        _ => Err(Condition::invalid_type("pair", val.type_name())),
    }
}

#[bridge(name = "cdr", lib = "(base)")]
pub async fn cdr(val: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let val = val.read();
    match &*val {
        Value::Pair(_car, cdr) => Ok(vec![cdr.clone()]),
        _ => Err(Condition::invalid_type("pair", val.type_name())),
    }
}

#[bridge(name = "set-car!", lib = "(base)")]
pub async fn set_car(var: &Gc<Value>, val: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let mut var = var.write();
    match &mut *var {
        Value::Pair(ref mut car, _cdr) => *car = val.clone(),
        _ => todo!(),
    }
    Ok(vec![Gc::new(Value::Null)])
}

#[bridge(name = "set-cdr!", lib = "(base)")]
pub async fn set_cdr(var: &Gc<Value>, val: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let mut var = var.write();
    match &mut *var {
        Value::Pair(_car, ref mut cdr) => *cdr = val.clone(),
        _ => todo!(),
    }
    Ok(vec![Gc::new(Value::Null)])
}

#[bridge(name = "length", lib = "(base)")]
pub async fn length(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let mut length = 0;
    let mut arg = arg.clone();
    loop {
        arg = {
            let val = arg.read();
            match &*val {
                Value::Pair(_, cdr) => cdr.clone(),
                _ => break,
            }
        };
        length += 1;
    }
    Ok(vec![Gc::new(Value::Number(Number::from(length)))])
}

#[bridge(name = "list->vector", lib = "(base)")]
pub async fn list_to_vector(list: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let mut vec = Vec::new();
    list_to_vec(list, &mut vec);

    Ok(vec![Gc::new(Value::Vector(
        vec.into_iter().map(|i| i.read().as_ref().clone()).collect(),
    ))])
}
*/
