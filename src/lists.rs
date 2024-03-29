use crate::{continuation::Continuation, error::RuntimeError, gc::Gc, value::Value};
use futures::future::BoxFuture;
use proc_macros::builtin;
use std::sync::Arc;

pub fn fmt_list<'a>(car: &'a Gc<Value>, cdr: &'a Gc<Value>) -> BoxFuture<'a, String> {
    Box::pin(async move {
        match &*cdr.read().await {
            Value::Pair(_, _) | Value::Nil => (),
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
                Value::Nil => {
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

#[builtin("list")]
pub async fn list(
    _cont: &Option<Arc<Continuation>>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    // Construct the list in reverse
    let mut cdr = Gc::new(Value::Nil);
    for arg in args.into_iter().rev() {
        cdr = Gc::new(Value::Pair(arg, cdr.clone()));
    }
    Ok(cdr)
}

#[builtin("cons")]
pub async fn cons(
    _cont: &Option<Arc<Continuation>>,
    car: &Gc<Value>,
    cdr: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    Ok(Gc::new(Value::Pair(car.clone(), cdr.clone())))
}

#[builtin("car")]
pub async fn car(
    _cont: &Option<Arc<Continuation>>,
    val: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let val = val.read().await;
    match &*val {
        Value::Pair(car, _cdr) => Ok(car.clone()),
        _ => todo!(),
    }
}

#[builtin("cdr")]
pub async fn cdr(
    _cont: &Option<Arc<Continuation>>,
    val: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let val = val.read().await;
    match &*val {
        Value::Pair(_car, cdr) => Ok(cdr.clone()),
        _ => todo!(),
    }
}

#[builtin("set-car!")]
pub async fn set_car(
    _cont: &Option<Arc<Continuation>>,
    var: &Gc<Value>,
    val: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let mut var = var.write().await;
    match &mut *var {
        Value::Pair(ref mut car, _cdr) => *car = val.clone(),
        _ => todo!(),
    }
    Ok(Gc::new(Value::Nil))
}
