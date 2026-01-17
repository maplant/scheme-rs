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
    exceptions::Exception,
    proc::Procedure,
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    strings::WideString,
    value::Value,
    vectors::ByteVector,
};

type Future = Shared<BoxFuture<'static, Result<Vec<Value>, Exception>>>;

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
pub async fn spawn(task: &Value) -> Result<Vec<Value>, Exception> {
    let task: Procedure = task.clone().try_into()?;
    let task = tokio::task::spawn(async move { task.call(&[]).await });
    let future: Future = async move { task.await.unwrap() }.boxed().shared();
    let future = Value::from(Record::from_rust_type(future));
    Ok(vec![future])
}

#[bridge(name = "await", lib = "(tokio)")]
pub async fn await_future(future: &Value) -> Result<Vec<Value>, Exception> {
    future.try_to_rust_type::<Future>()?.as_ref().clone().await
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
pub async fn bind_tcp(addr: &Value) -> Result<Vec<Value>, Exception> {
    let addr: WideString = addr.clone().try_into()?;
    let listener = TcpListener::bind(&addr.to_string())
        .await
        .map_err(|e| Exception::error(format!("failed to bind to address: {e:?}")))?;
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
pub async fn accept(listener: &Value) -> Result<Vec<Value>, Exception> {
    let listener = { listener.try_to_rust_type::<Arc<TcpListener>>()?.clone() };
    let (socket, addr) = listener
        .accept()
        .await
        .map_err(|e| Exception::error(format!("Could not accept client: {e:?}")))?;
    let socket = Value::from(Record::from_rust_type(Arc::new(Mutex::new(socket))));
    let addr = Value::from(addr.to_string());
    Ok(vec![socket, addr])
}

#[bridge(name = "read", lib = "(tokio)")]
pub async fn read(socket: &Value, buff_size: &Value) -> Result<Vec<Value>, Exception> {
    let buff_size: usize = buff_size.try_to_scheme_type()?;
    let mut buffer = vec![0u8; buff_size];
    let socket = socket.try_to_rust_type::<Arc<Mutex<TcpStream>>>()?;

    let len = socket
        .lock()
        .await
        .read(&mut buffer)
        .await
        .map_err(|e| Exception::error(format!("failed to read from socket: {e:?}")))?;

    buffer.resize(len, 0);

    Ok(vec![Value::from(buffer)])
}

#[bridge(name = "write", lib = "(tokio)")]
pub async fn write(socket: &Value, buffer: &Value) -> Result<Vec<Value>, Exception> {
    let buffer: ByteVector = buffer.clone().try_into()?;
    let socket = { socket.try_to_rust_type::<Arc<Mutex<TcpStream>>>()?.clone() };

    let buff = buffer.0.vec.read().clone();

    socket
        .lock()
        .await
        .write(&buff)
        .await
        .map_err(|e| Exception::error(format!("failed to read from socket: {e:?}")))?;

    Ok(vec![Value::from(buffer)])
}
