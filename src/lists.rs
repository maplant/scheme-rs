use crate::{
    eval::{Env, RuntimeError, Value},
    gc::Gc,
};
use proc_macros::builtin;

pub async fn fmt_list(car: &Gc<Value>, cdr: &Gc<Value>) -> String {
    todo!()
    /*
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
                stack.push(cdr.clone());
                stack.push(car.clone());
            }
            x => {
                output.push(' ');
                output.push_str(&x.fmt().await)
            }
        }
    }

    output.push(')');
    output
     */
}

#[builtin(list)]
pub async fn list(_env: &Gc<Env>, args: Vec<Gc<Value>>) -> Result<Gc<Value>, RuntimeError> {
    // Construct the list in reverse
    let mut cdr = Gc::new(Value::Nil);
    for arg in args.into_iter().rev() {
        cdr = Gc::new(Value::Pair(arg, cdr.clone()));
    }
    Ok(cdr)
}

#[builtin(cons)]
pub async fn cons(
    _env: &Gc<Env>,
    car: &Gc<Value>,
    cdr: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    Ok(Gc::new(Value::Pair(car.clone(), cdr.clone())))
}
