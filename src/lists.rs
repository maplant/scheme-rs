use crate::{
    env::Env,
    eval::{RuntimeError, Value},
    gc::Gc,
};
use futures::future::BoxFuture;
use proc_macros::builtin;

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

#[builtin(list)]
pub async fn list(_env: Env, args: Vec<Gc<Value>>) -> Result<Gc<Value>, RuntimeError> {
    // Construct the list in reverse
    let mut cdr = Gc::new(Value::Nil);
    for arg in args.into_iter().rev() {
        cdr = Gc::new(Value::Pair(arg, cdr.clone()));
    }
    Ok(cdr)
}

#[builtin(cons)]
pub async fn cons(_env: Env, car: &Gc<Value>, cdr: &Gc<Value>) -> Result<Gc<Value>, RuntimeError> {
    Ok(Gc::new(Value::Pair(car.clone(), cdr.clone())))
}
