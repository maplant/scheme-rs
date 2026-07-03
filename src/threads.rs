//! Threading primitives

use std::{
    fmt,
    sync::Arc,
    thread::{self, ThreadId},
    time::Duration,
};

use parking_lot::Mutex;
use scheme_rs_macros::bridge;

use crate::{
    exceptions::Exception,
    gc::{Gc, Trace},
    proc::{Application, ContBarrier, Procedure},
    records::{RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::cps_bridge,
    runtime::Runtime,
    value::Value,
};

#[derive(Trace)]
pub struct JoinHandle {
    #[trace(skip)]
    id: ThreadId,
    #[trace(skip)]
    thread: Mutex<Option<thread::JoinHandle<()>>>,
    result: Gc<Mutex<Result<Vec<Value>, Exception>>>,
}

impl fmt::Debug for JoinHandle {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl SchemeCompatible for JoinHandle {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "join-handle", sealed: true, opaque: true)
    }
}

#[cps_bridge(def = "spawn thunk", lib = "(threads (1))")]
pub fn spawn(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let thunk: Procedure = args[0].clone().try_into()?;
    let cell = Gc::new(Mutex::new(Ok(Vec::new())));
    let cell_cloned = cell.clone();
    // Built into a ContBarrier inside the closure; ContBarrier itself is
    // not Send in non-async builds.
    let child = barrier.child_state();
    // Capture the runtime handle so the child thread can enter the reactor
    // context (timers/IO in async bridges work). Remaining ceiling:
    // reactor-backed bridges reached from hashtable hash/eq callbacks park a
    // runtime worker inside call_sync — deadlock on current_thread runtimes,
    // pool starvation on multi_thread. Goes away once the hashtable path is
    // asyncified (follow-up); call_sync then only parks non-worker threads.
    #[cfg(feature = "async")]
    let handle = tokio::runtime::Handle::try_current().ok();
    let join_handle = thread::spawn(move || {
        let mut cell_write = cell_cloned.lock();
        let mut barrier = ContBarrier::from(child);

        #[cfg(not(feature = "async"))]
        {
            *cell_write = thunk.call(&[], &mut barrier);
        }

        #[cfg(feature = "async")]
        {
            *cell_write = match handle {
                Some(handle) => handle.block_on(thunk.call(&[], &mut barrier)),
                None => thunk.call_sync(&[], &mut barrier),
            };
        }
    });
    let id = join_handle.thread().id();
    let handle = Value::from_rust_type(JoinHandle {
        id,
        thread: Mutex::new(Some(join_handle)),
        result: cell,
    });
    Ok(Application::new(k, None, vec![handle]))
}

#[bridge(name = "join", lib = "(threads (1))")]
pub fn join(handle: &Value) -> Result<Vec<Value>, Exception> {
    let handle = handle.try_to_rust_type::<JoinHandle>()?;
    join_inner(&handle)
}

fn join_inner(handle: &JoinHandle) -> Result<Vec<Value>, Exception> {
    let curr_id = thread::current().id();
    if curr_id == handle.id {
        return Err(Exception::error(format!(
            "thread {curr_id:?} attempted to join itself"
        )));
    }
    // The scrutinee's MutexGuard is held across join() (if-let temporary
    // scope), which is what blocks concurrent joiners until the thread has
    // finished; don't hoist the lock() into its own binding.
    if let Some(thread) = handle.thread.lock().take()
        && let Err(payload) = thread.join()
    {
        let msg = payload
            .downcast_ref::<&str>()
            .map(ToString::to_string)
            .or_else(|| payload.downcast_ref::<String>().cloned())
            .unwrap_or_else(|| "unknown panic".to_string());
        *handle.result.lock() = Err(Exception::error(format!("thread panicked: {msg}")));
    }
    handle.result.lock().clone()
}

#[bridge(name = "sleep", lib = "(threads (1))")]
pub fn sleep(ms: u64) -> Result<Vec<Value>, Exception> {
    thread::sleep(Duration::from_millis(ms));
    Ok(Vec::new())
}

#[bridge(name = "join-handle?", lib = "(threads (1))")]
pub fn join_handle_pred(obj: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        obj.cast_to_rust_type::<JoinHandle>().is_some(),
    )])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn join_surfaces_panics() {
        let thread = thread::spawn(|| panic!("boom"));
        let handle = JoinHandle {
            id: thread.thread().id(),
            thread: Mutex::new(Some(thread)),
            result: Gc::new(Mutex::new(Ok(Vec::new()))),
        };
        let err = join_inner(&handle).unwrap_err();
        assert!(format!("{err:?}").contains("boom"));
        // Subsequent joins see the persisted error, not the default cell.
        let err = join_inner(&handle).unwrap_err();
        assert!(format!("{err:?}").contains("boom"));
    }
}
