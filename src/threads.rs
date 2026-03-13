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
    proc::Procedure,
    records::{RecordTypeDescriptor, SchemeCompatible, rtd},
    value::Value,
};

#[derive(Trace)]
pub struct JoinHandle {
    #[trace(skip)]
    id: ThreadId,
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

#[bridge(name = "spawn", lib = "(threads (1))")]
pub fn spawn(thunk: Procedure) -> Result<Vec<Value>, Exception> {
    let cell = Gc::new(Mutex::new(Ok(Vec::new())));
    let cell_cloned = cell.clone();
    let join_handle = thread::spawn(move || {
        let mut cell_write = cell_cloned.lock();

        #[cfg(not(feature = "async"))]
        {
            *cell_write = thunk.call(&[]);
        }

        #[cfg(feature = "async")]
        {
            *cell_write = thunk.call_sync(&[]);
        }
    });
    let id = join_handle.thread().id();
    Ok(vec![Value::from_rust_type(JoinHandle { id, result: cell })])
}

#[bridge(name = "join", lib = "(threads (1))")]
pub fn join(handle: &Value) -> Result<Vec<Value>, Exception> {
    let handle = handle.try_to_rust_type::<JoinHandle>()?;
    let curr_id = thread::current().id();
    if curr_id == handle.id {
        Err(Exception::error(format!(
            "thread {curr_id:?} attempted to join itself"
        )))
    } else {
        handle.result.lock().clone()
    }
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
