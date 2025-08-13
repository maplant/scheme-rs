use std::{any::Any, sync::Arc};

use futures::{
    FutureExt,
    future::{BoxFuture, Shared},
};
use scheme_rs_macros::bridge;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::{TcpListener, TcpStream},
    sync::Mutex,
};

use crate::{
    exception::Condition, gc::Gc, num::Number, proc::Closure, strings::AlignedString, value::Value,
    vectors::AlignedVector,
};

type Future = Shared<BoxFuture<'static, Result<Vec<Value>, Condition>>>;

#[bridge(name = "spawn", lib = "(base)")]
pub async fn spawn(task: &Value) -> Result<Vec<Value>, Condition> {
    let task: Gc<Closure> = task.clone().try_into()?;
    let task = tokio::task::spawn(async move { Ok(task.call(&[]).await?) });
    let future: Future = async move { task.await.unwrap() }.boxed().shared();
    let future = Value::from(Gc::new(Gc::into_any(Gc::new(future))));
    Ok(vec![future])
}

#[bridge(name = "await", lib = "(base)")]
pub async fn await_future(future: &Value) -> Result<Vec<Value>, Condition> {
    let future = {
        let any: Gc<Gc<dyn Any>> = future.clone().try_into()?;
        let future: Gc<Future> = any
            .read()
            .clone()
            .downcast()
            .map_err(|_| Condition::Error)?;
        future.read().clone()
    };
    future.await
}

#[bridge(name = "bind-tcp", lib = "(base)")]
pub async fn bind_tcp(addr: &Value) -> Result<Vec<Value>, Condition> {
    let addr: Arc<AlignedString> = addr.clone().try_into()?;
    let listener = TcpListener::bind(addr.as_str())
        .await
        .map_err(|e| Condition::error(format!("failed to bind to address: {e:?}")))?;
    let listener = Value::from(Gc::new(Gc::into_any(Gc::new(Arc::new(listener)))));
    Ok(vec![listener])
}

#[bridge(name = "accept", lib = "(base)")]
pub async fn accept(listener: &Value) -> Result<Vec<Value>, Condition> {
    let listener = {
        let any: Gc<Gc<dyn Any>> = listener.clone().try_into()?;
        let listener: Gc<Arc<TcpListener>> = any
            .read()
            .clone()
            .downcast()
            .map_err(|_| Condition::Error)?;
        listener.read().clone()
    };
    let (socket, addr) = listener
        .accept()
        .await
        .map_err(|e| Condition::error(format!("Could not accept client: {e:?}")))?;
    let socket = Value::from(Gc::new(Gc::into_any(Gc::new(Arc::new(Mutex::new(socket))))));
    let addr = Value::from(addr.to_string());
    Ok(vec![socket, addr])
}

#[bridge(name = "read", lib = "(base)")]
pub async fn read(socket: &Value, buff_size: &Value) -> Result<Vec<Value>, Condition> {
    let buff_size: Arc<Number> = buff_size.clone().try_into()?;
    let buff_size: usize = buff_size.as_ref().try_into()?;
    let mut buffer = vec![0u8; buff_size];
    let socket = {
        let any: Gc<Gc<dyn Any>> = socket.clone().try_into()?;
        let socket: Gc<Arc<Mutex<TcpStream>>> = any
            .read()
            .clone()
            .downcast()
            .map_err(|_| Condition::Error)?;
        socket.read().clone()
    };

    let len = socket
        .lock()
        .await
        .read(&mut buffer)
        .await
        .map_err(|e| Condition::error(format!("failed to read from socket: {e:?}")))?;

    buffer.resize(len, 0);

    Ok(vec![Value::from(buffer)])
}

#[bridge(name = "write", lib = "(base)")]
pub async fn write(socket: &Value, buffer: &Value) -> Result<Vec<Value>, Condition> {
    let buffer: Arc<AlignedVector<u8>> = buffer.clone().try_into()?;
    let socket = {
        let any: Gc<Gc<dyn Any>> = socket.clone().try_into()?;
        let socket: Gc<Arc<Mutex<TcpStream>>> = any
            .read()
            .clone()
            .downcast()
            .map_err(|_| Condition::Error)?;
        socket.read().clone()
    };

    socket
        .lock()
        .await
        .write(&buffer)
        .await
        .map_err(|e| Condition::error(format!("failed to read from socket: {e:?}")))?;

    Ok(vec![Value::from(buffer)])
}
