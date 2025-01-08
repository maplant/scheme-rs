use crate::{error::RuntimeError, gc::Gc, num::Number, value::Value};
use proc_macros::builtin;
use std::sync::Arc;

pub fn fmt_list(car: &Gc<Value>, cdr: &Gc<Value>) -> String {
    // TODO(map): If the list is circular, DO NOT print infinitely!
    match &*cdr.read() {
        Value::Pair(_, _) | Value::Null => (),
        cdr => {
            // This is not a proper list
            let car = car.read().fmt();
            let cdr = cdr.fmt();
            return format!("({car} . {cdr})");
        }
    }

    let mut output = String::from("(");
    output.push_str(&car.read().fmt());

    let mut stack = vec![cdr.clone()];

    while let Some(head) = stack.pop() {
        match &*head.read() {
            Value::Null => {
                if !stack.is_empty() {
                    output.push_str(" ()");
                }
            }
            Value::Pair(car, cdr) => {
                output.push(' ');
                output.push_str(&car.read().fmt());
                stack.push(cdr.clone());
            }
            x => {
                output.push(' ');
                output.push_str(&x.fmt())
            }
        }
    }

    output.push(')');
    output
}

pub fn list_to_vec(curr: &Gc<Value>, out: &mut Vec<Gc<Value>>) {
    let val = curr.read();
    match &*val {
        Value::Pair(a, b) => {
            out.push(a.clone());
            list_to_vec(b, out);
        }
        Value::Null => (),
        _ => out.push(curr.clone()),
    }
}

pub fn list_to_vec_with_null(curr: &Gc<Value>, out: &mut Vec<Gc<Value>>) {
    let val = curr.read();
    match &*val {
        Value::Pair(a, b) => {
            out.push(a.clone());
            list_to_vec_with_null(b, out);
        }
        _ => out.push(curr.clone()),
    }
}

/*
#[builtin("list")]
pub async fn list(
    _cont: &Option<Arc<Continuation>>,
    args: Vec<Gc<Value>>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    // Construct the list in reverse
    let mut cdr = Gc::new(Value::Null);
    for arg in args.into_iter().rev() {
        cdr = Gc::new(Value::Pair(arg, cdr.clone()));
    }
    Ok(vec![cdr])
}

#[builtin("cons")]
pub async fn cons(
    _cont: &Option<Arc<Continuation>>,
    car: &Gc<Value>,
    cdr: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let car = Gc::new(car.read().clone());
    let cdr = Gc::new(cdr.read().clone());
    Ok(vec![Gc::new(Value::Pair(car, cdr))])
}

#[builtin("car")]
pub async fn car(
    _cont: &Option<Arc<Continuation>>,
    val: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let val = val.read();
    match &*val {
        Value::Pair(car, _cdr) => Ok(vec![car.clone()]),
        _ => Err(RuntimeError::invalid_type("pair", val.type_name())),
    }
}

#[builtin("cdr")]
pub async fn cdr(
    _cont: &Option<Arc<Continuation>>,
    val: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let val = val.read();
    match &*val {
        Value::Pair(_car, cdr) => Ok(vec![cdr.clone()]),
        _ => Err(RuntimeError::invalid_type("pair", val.type_name())),
    }
}

#[builtin("set-car!")]
pub async fn set_car(
    _cont: &Option<Arc<Continuation>>,
    var: &Gc<Value>,
    val: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let mut var = var.write();
    match &mut *var {
        Value::Pair(ref mut car, _cdr) => *car = val.clone(),
        _ => todo!(),
    }
    Ok(vec![Gc::new(Value::Null)])
}

#[builtin("set-cdr!")]
pub async fn set_cdr(
    _cont: &Option<Arc<Continuation>>,
    var: &Gc<Value>,
    val: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let mut var = var.write();
    match &mut *var {
        Value::Pair(_car, ref mut cdr) => *cdr = val.clone(),
        _ => todo!(),
    }
    Ok(vec![Gc::new(Value::Null)])
}

#[builtin("length")]
pub async fn length(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
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
*/
