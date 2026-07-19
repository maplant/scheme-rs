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
    proc::{Application, ContBarrier, Procedure},
    records::{Embeddable, Embedded, RecordTypeDescriptor, rtd},
    registry::cps_bridge,
    runtime::Runtime,
    strings::WideString,
    value::Value,
};

type Future = Shared<BoxFuture<'static, Result<Vec<Value>, Exception>>>;

unsafe impl Embeddable for Future {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            ty: Future,
            name: "future",
            opaque: true,
            sealed: true,
        )
    }
}

// `future` and `spawn` both start a new dynamic extent (a future body may
// run interleaved with, or after, whatever called `future`/`spawn`), so
// they need a spawn snapshot rather than the caller's barrier itself:
// promoted from `#[bridge]` to `#[cps_bridge]` to reach it.

#[cps_bridge(def = "future proc", lib = "(async)")]
pub async fn make_future(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier<'_>,
) -> Result<Application, Exception> {
    let proc: Procedure = args[0].clone().try_into()?;
    let mut snapshot = barrier.spawn_snapshot();
    let future: Future = async move { proc.call(&[], &mut snapshot).await }
        .boxed()
        .shared();
    let future = Value::from(future);
    Ok(Application::new(k, None, vec![future]))
}

#[cps_bridge(def = "spawn task", lib = "(async)")]
pub async fn spawn(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier<'_>,
) -> Result<Application, Exception> {
    let task: Procedure = args[0].clone().try_into()?;
    let mut snapshot = barrier.spawn_snapshot();
    let task = tokio::task::spawn(async move { task.call(&[], &mut snapshot).await });
    let future: Future = async move { task.await.unwrap() }.boxed().shared();
    let future = Value::from(future);
    Ok(Application::new(k, None, vec![future]))
}

#[bridge(name = "await", lib = "(async)")]
pub async fn await_future(future: &Value) -> Result<Vec<Value>, Exception> {
    future.try_to::<Embedded<Future>>()?.as_ref().clone().await
}

unsafe impl Embeddable for Arc<TcpListener> {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            ty: Arc<TcpListener>,
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
        .map_err(|e| Exception::error(format!("failed to bind to address: {e}")))?;
    let listener = Value::from(Arc::new(listener));
    Ok(vec![listener])
}

unsafe impl Embeddable for Arc<Mutex<TcpStream>> {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            ty: Arc<Mutex<TcpStream>>,
            name: "socket",
            opaque: true,
            sealed: true,
        )
    }
}

#[bridge(name = "accept", lib = "(async)")]
pub async fn accept(listener: &Value) -> Result<Vec<Value>, Exception> {
    let listener = {
        listener
            .try_to::<Embedded<Arc<TcpListener>>>()?
            .as_ref()
            .clone()
    };
    let (socket, addr) = listener
        .accept()
        .await
        .map_err(|e| Exception::error(format!("could not accept client: {e}")))?;
    let socket = Value::from(Port::new(addr.to_string(), socket, BufferMode::Block, None));
    let addr = Value::from(addr.to_string());
    Ok(vec![socket, addr])
}

#[bridge(name = "sleep", lib = "(async)")]
pub async fn sleep_ms(ms: u64) -> Result<Vec<Value>, Exception> {
    sleep(Duration::from_millis(ms)).await;
    Ok(Vec::new())
}
