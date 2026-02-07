use std::sync::Arc;

use futures::{
    FutureExt,
    future::{BoxFuture, Shared},
};
use scheme_rs_macros::bridge;

use tokio::{
    net::{TcpListener, TcpStream},
    sync::Mutex,
    time::{Duration, sleep},
};

use crate::{
    exceptions::Exception,
    ports::{BufferMode, Port},
    proc::Procedure,
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    strings::WideString,
    value::Value,
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

#[bridge(name = "future", lib = "(async)")]
pub async fn make_future(proc: Procedure) -> Result<Vec<Value>, Exception> {
    let future: Future = async move { proc.call(&[]).await }.boxed().shared();
    let future = Value::from_rust_type(future);
    Ok(vec![future])
}

#[bridge(name = "spawn", lib = "(async)")]
pub async fn spawn(task: &Value) -> Result<Vec<Value>, Exception> {
    let task: Procedure = task.clone().try_into()?;
    let task = tokio::task::spawn(async move { task.call(&[]).await });
    let future: Future = async move { task.await.unwrap() }.boxed().shared();
    let future = Value::from(Record::from_rust_type(future));
    Ok(vec![future])
}

#[bridge(name = "await", lib = "(async)")]
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

#[bridge(name = "bind-tcp", lib = "(async)")]
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

#[bridge(name = "accept", lib = "(async)")]
pub async fn accept(listener: &Value) -> Result<Vec<Value>, Exception> {
    let listener = { listener.try_to_rust_type::<Arc<TcpListener>>()?.clone() };
    let (socket, addr) = listener
        .accept()
        .await
        .map_err(|e| Exception::error(format!("Could not accept client: {e:?}")))?;
    let socket = Value::from(Port::new(addr.to_string(), socket, BufferMode::Block, None));
    let addr = Value::from(addr.to_string());
    Ok(vec![socket, addr])
}

#[bridge(name = "sleep", lib = "(async)")]
pub async fn sleep_ms(ms: u64) -> Result<Vec<Value>, Exception> {
    sleep(Duration::from_millis(ms)).await;
    Ok(Vec::new())
}
