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
    proc::{ContBarrier, Procedure},
    records::{Embeddable, Embedded, RecordTypeDescriptor, rtd},
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

unsafe impl Embeddable for JoinHandle {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(ty: JoinHandle, name: "join-handle", sealed: true, opaque: true)
    }
}

#[bridge(name = "spawn", lib = "(threads (1))")]
pub fn spawn(thunk: Procedure) -> JoinHandle {
    let cell = Gc::new(Mutex::new(Ok(Vec::new())));
    let cell_cloned = cell.clone();
    let join_handle = thread::spawn(move || {
        let mut cell_write = cell_cloned.lock();

        #[cfg(not(feature = "async"))]
        {
            *cell_write = thunk.call(&[], &mut ContBarrier::new());
        }

        #[cfg(feature = "async")]
        {
            *cell_write = thunk.call_sync(&[], &mut ContBarrier::new());
        }
    });
    let id = join_handle.thread().id();
    JoinHandle { id, result: cell }
}

#[bridge(name = "join", lib = "(threads (1))")]
pub fn join(handle: Embedded<JoinHandle>) -> Result<Value, Exception> {
    let curr_id = thread::current().id();
    if curr_id == handle.id {
        Err(Exception::error(format!(
            "thread {curr_id:?} attempted to join itself"
        )))
    } else {
        Ok(Value::from(handle.result.lock().clone()?))
    }
}

#[bridge(name = "sleep", lib = "(threads (1))")]
pub fn sleep(ms: u64) -> Result<(), Exception> {
    thread::sleep(Duration::from_millis(ms));
    Ok(())
}

#[bridge(name = "join-handle?", lib = "(threads (1))")]
pub fn join_handle_pred(obj: &Value) -> bool {
    obj.is_a::<Embedded<JoinHandle>>()
}
