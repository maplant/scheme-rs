use crate::{continuation::Continuation, error::RuntimeError, gc::Gc, num::Number, value::Value};
use futures::future::BoxFuture;
use proc_macros::builtin;
use std::sync::Arc;

pub fn fmt_list<'a>(car: &'a Gc<Value>, cdr: &'a Gc<Value>) -> BoxFuture<'a, String> {
    Box::pin(async move {
        match &*cdr.read().await {
            Value::Pair(_, _) | Value::Null => (),
            cdr => {
                // This is not a proper list
                let car = car.read().await.fmt().await;
                let cdr = cdr.fmt().await;
                return format!("({car} . {cdr})");
            }
        }

        let mut output = String::from("(");
        output.push_str(&car.read().await.fmt().await);

        let mut stack = vec![cdr.clone()];

        while let Some(head) = stack.pop() {
            match &*head.read().await {
                Value::Null => {
                    if !stack.is_empty() {
                        output.push_str(" ()");
                    }
                }
                Value::Pair(car, cdr) => {
                    output.push(' ');
                    output.push_str(&car.read().await.fmt().await);
                    stack.push(cdr.clone());
                }
                x => {
                    output.push(' ');
                    output.push_str(&x.fmt().await)
                }
            }
        }

        output.push(')');
        output
    })
}

pub fn list_to_vec<'a>(curr: &'a Gc<Value>, out: &'a mut Vec<Gc<Value>>) -> BoxFuture<'a, ()> {
    Box::pin(async move {
        let val = curr.read().await;
        match &*val {
            Value::Pair(a, b) => {
                out.push(a.clone());
                list_to_vec(b, out).await;
            }
            Value::Null => (),
            _ => out.push(curr.clone()),
        }
    })
}

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
    Ok(vec![Gc::new(Value::Pair(car.clone(), cdr.clone()))])
}

#[builtin("car")]
pub async fn car(
    _cont: &Option<Arc<Continuation>>,
    val: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let val = val.read().await;
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
    let val = val.read().await;
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
    let mut var = var.write().await;
    match &mut *var {
        Value::Pair(ref mut car, _cdr) => *car = val.clone(),
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
            let val = arg.read().await;
            match &*val {
                Value::Pair(_, cdr) => cdr.clone(),
                _ => break,
            }
        };
        length += 1;
    }
    Ok(vec![Gc::new(Value::Number(Number::from(length)))])
}
