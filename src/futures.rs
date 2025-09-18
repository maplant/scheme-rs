use std::sync::Arc;

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
    exceptions::Condition,
    num::Number,
    proc::Closure,
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    strings::AlignedString,
    value::Value,
    vectors::AlignedVector,
};

type Future = Shared<BoxFuture<'static, Result<Vec<Value>, Condition>>>;

impl SchemeCompatible for Future {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            name: "future",
            opaque: true,
            sealed: true,
        )
    }
}

#[bridge(name = "spawn", lib = "(tokio)")]
pub async fn spawn(task: &Value) -> Result<Vec<Value>, Condition> {
    let task: Closure = task.clone().try_into()?;
    let task = tokio::task::spawn(async move { Ok(task.call(&[]).await?) });
    let future: Future = async move { task.await.unwrap() }.boxed().shared();
    let future = Value::from(Record::from_rust_type(future));
    Ok(vec![future])
}

#[bridge(name = "await", lib = "(tokio)")]
pub async fn await_future(future: &Value) -> Result<Vec<Value>, Condition> {
    let future = {
        future
            .clone()
            .try_into_rust_type::<Future>()?
            .read()
            .clone()
    };
    future.await
}

impl SchemeCompatible for Arc<TcpListener> {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            name: "tcp-listener",
            opaque: true,
            sealed: true,
        )
    }
}

#[bridge(name = "bind-tcp", lib = "(tokio)")]
pub async fn bind_tcp(addr: &Value) -> Result<Vec<Value>, Condition> {
    let addr: Arc<AlignedString> = addr.clone().try_into()?;
    let listener = TcpListener::bind(addr.as_str())
        .await
        .map_err(|e| Condition::error(format!("failed to bind to address: {e:?}")))?;
    let listener = Value::from(Record::from_rust_type(Arc::new(listener)));
    Ok(vec![listener])
}

impl SchemeCompatible for Arc<Mutex<TcpStream>> {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            name: "socket",
            opaque: true,
            sealed: true,
        )
    }
}

#[bridge(name = "accept", lib = "(tokio)")]
pub async fn accept(listener: &Value) -> Result<Vec<Value>, Condition> {
    let listener = {
        listener
            .clone()
            .try_into_rust_type::<Arc<TcpListener>>()?
            .read()
            .clone()
    };
    let (socket, addr) = listener
        .accept()
        .await
        .map_err(|e| Condition::error(format!("Could not accept client: {e:?}")))?;
    let socket = Value::from(Record::from_rust_type(Arc::new(Mutex::new(socket))));
    let addr = Value::from(addr.to_string());
    Ok(vec![socket, addr])
}

#[bridge(name = "read", lib = "(tokio)")]
pub async fn read(socket: &Value, buff_size: &Value) -> Result<Vec<Value>, Condition> {
    let buff_size: Arc<Number> = buff_size.clone().try_into()?;
    let buff_size: usize = buff_size.as_ref().try_into()?;
    let mut buffer = vec![0u8; buff_size];
    let socket = {
        socket
            .clone()
            .try_into_rust_type::<Arc<Mutex<TcpStream>>>()?
            .read()
            .clone()
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

#[bridge(name = "write", lib = "(tokio)")]
pub async fn write(socket: &Value, buffer: &Value) -> Result<Vec<Value>, Condition> {
    let buffer: Arc<AlignedVector<u8>> = buffer.clone().try_into()?;
    let socket = {
        socket
            .clone()
            .try_into_rust_type::<Arc<Mutex<TcpStream>>>()?
            .read()
            .clone()
    };

    socket
        .lock()
        .await
        .write(&buffer)
        .await
        .map_err(|e| Condition::error(format!("failed to read from socket: {e:?}")))?;

    Ok(vec![Value::from(buffer)])
}
