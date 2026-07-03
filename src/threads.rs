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
    proc::{Procedure, dyn_state_snapshot, with_dyn_state_sync},
    records::{Embeddable, Embedded, RecordTypeDescriptor, rtd},
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

unsafe impl Embeddable for JoinHandle {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(ty: JoinHandle, name: "join-handle", sealed: true, opaque: true)
    }
}

#[bridge(name = "spawn", lib = "(threads (1))")]
pub fn spawn(thunk: Procedure) -> Result<Vec<Value>, Exception> {
    let cell = Gc::new(Mutex::new(Ok(Vec::new())));
    let cell_cloned = cell.clone();
    // Snapshot the current dynamic state now, on the spawning thread: the
    // spawned thread starts a new dynamic extent and must not run the
    // parent's winders or see its exception handlers.
    let snapshot = dyn_state_snapshot();
    let join_handle = thread::spawn(move || {
        let mut cell_write = cell_cloned.lock();

        with_dyn_state_sync(snapshot, || {
            #[cfg(not(feature = "async"))]
            {
                *cell_write = thunk.call(&[]);
            }

            #[cfg(feature = "async")]
            {
                *cell_write = thunk.call_sync(&[]);
            }
        });
    });
    let id = join_handle.thread().id();
    Ok(vec![Value::from(JoinHandle {
        id,
        thread: Mutex::new(Some(join_handle)),
        result: cell,
    })])
}

#[bridge(name = "join", lib = "(threads (1))")]
pub fn join(handle: Embedded<JoinHandle>) -> Result<Vec<Value>, Exception> {
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
    Ok(vec![Value::from(obj.is_a::<Embedded<JoinHandle>>())])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::exceptions::{CompoundCondition, Message};

    fn exception_message(err: &Exception) -> String {
        let cond: Embedded<CompoundCondition> = Option::from(&err.0).expect("compound condition");
        cond.0
            .iter()
            .find_map(|c| Option::<Embedded<Message>>::from(c).map(|m| m.message.clone()))
            .expect("message condition")
    }

    #[test]
    fn join_surfaces_panics() {
        let thread = thread::spawn(|| panic!("boom"));
        let handle = JoinHandle {
            id: thread.thread().id(),
            thread: Mutex::new(Some(thread)),
            result: Gc::new(Mutex::new(Ok(Vec::new()))),
        };
        let err = join_inner(&handle).unwrap_err();
        assert!(exception_message(&err).contains("boom"));
        // Subsequent joins see the persisted error, not the default cell.
        let err = join_inner(&handle).unwrap_err();
        assert!(exception_message(&err).contains("boom"));
    }
}
