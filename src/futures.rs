use crate::{continuation::Continuation, error::RuntimeError, gc::Gc, num::Number, value::Value};
use futures::{future::try_join_all, FutureExt};
use proc_macros::builtin;
use std::{sync::Arc, time::Duration};

#[builtin("spawn")]
pub async fn spawn(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let value = arg.read().await;
    let callable = value
        .as_callable()
        .ok_or_else(|| RuntimeError::invalid_type("callable", value.type_name()))?;
    /*
    let Some(0) = callable.max_args() else {
        todo!();
    };
     */
    let task = tokio::task::spawn(async move {
        let val = callable.call(Vec::new(), &None).await?;
        val.eval(&None).await
    });
    let future = async move { task.await.unwrap() }.boxed().shared();
    Ok(Gc::new(Value::Future(future)))
}

#[builtin("sleep")]
pub async fn sleep(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let value = arg.read().await;
    let time: &Number = value.as_ref().try_into()?;
    let millis = time.to_u64();
    let future = async move {
        tokio::time::sleep(Duration::from_millis(millis)).await;
        Ok(Gc::new(Value::Nil))
    }
    .boxed()
    .shared();
    Ok(Gc::new(Value::Future(future)))
}

#[builtin("await")]
pub async fn await_value(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let value = arg.read().await;
    match &*value {
        Value::Future(fut) => fut.clone().await,
        _ => Ok(arg.clone()),
    }
}

#[builtin("join")]
pub async fn join(
    _cont: &Option<Arc<Continuation>>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    let mut futs = Vec::new();
    for arg in args.into_iter() {
        let value = arg.read().await;
        let fut = match &*value {
            Value::Future(fut) => fut.clone(),
            _ => {
                // I can't figure out a way to get rid of this clone
                // at the current moment without writing annoying code
                let arg = arg.clone();
                async move { Ok(arg) }.boxed().shared()
            }
        };
        futs.push(fut);
    }
    let future = async move {
        let results = try_join_all(futs).await?;
        Ok(Gc::new(Value::from(results)))
    }
    .boxed()
    .shared();
    Ok(Gc::new(Value::Future(future)))
}
