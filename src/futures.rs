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

#[bridge(name = "future", lib = "(async)")]
pub async fn make_future(proc: Procedure) -> Future {
    async move { proc.call(&[], &mut ContBarrier::new()).await }
        .boxed()
        .shared()
}

#[bridge(name = "spawn", lib = "(async)")]
pub async fn spawn(task: Procedure) -> Future {
    let task = tokio::task::spawn(async move { task.call(&[], &mut ContBarrier::new()).await });
    async move { task.await.unwrap() }.boxed().shared()
}

#[bridge(name = "await", lib = "(async)")]
pub async fn await_future(
    future: Embedded<Future>,
    barrier: &mut ContBarrier<'_>,
) -> Result<Application, Exception> {
    Ok(barrier.call_cont(future.as_ref().clone().await?))
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
pub async fn bind_tcp(addr: WideString) -> Result<Arc<TcpListener>, Exception> {
    let listener = TcpListener::bind(&addr.to_string())
        .await
        .map_err(|e| Exception::error(format!("failed to bind to address: {e}")))?;
    Ok(Arc::new(listener))
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
pub async fn accept(listener: Embedded<Arc<TcpListener>>) -> Result<(Port, String), Exception> {
    let (socket, addr) = listener
        .accept()
        .await
        .map_err(|e| Exception::error(format!("could not accept client: {e}")))?;
    let socket = Port::new(addr.to_string(), socket, BufferMode::Block, None);
    let addr = addr.to_string();
    Ok((socket, addr))
}

#[bridge(name = "sleep", lib = "(async)")]
pub async fn sleep_ms(ms: u64) -> Result<(), Exception> {
    sleep(Duration::from_millis(ms)).await;
    Ok(())
}
