//! Threading primitives

use std::{
    sync::{
        OnceLock,
        atomic::{AtomicBool, Ordering},
    },
    thread::{self, ThreadId},
    time::Duration,
};

use parking_lot::Mutex;
use scheme_rs_gc::{HeapSlot, without_gc_watching};
use scheme_rs_macros::bridge;

use crate::{
    exceptions::Exception,
    gc::Trace,
    lists::{List, list_to_vec},
    proc::{Application, Args, ContBarrier, Procedure},
    records::{Embeddable, Embedded},
    value::Value,
};

#[derive(Trace, Embeddable)]
#[scheme(name = "%join-handle", sealed = true, opaque = true)]
pub struct JoinHandle {
    #[trace(skip)]
    id: OnceLock<ThreadId>,
    #[trace(skip)]
    thread: Mutex<Option<thread::JoinHandle<()>>>,
    result: HeapSlot<Value>,
    #[trace(skip)]
    failed: AtomicBool,
}

#[bridge(name = "spawn", lib = "(threads (1))")]
pub fn spawn(thunk: Procedure) -> Embedded<JoinHandle> {
    let handle = Embedded::new(JoinHandle {
        id: OnceLock::new(),
        thread: Mutex::new(None),
        result: HeapSlot::new(Value::undefined()),
        failed: AtomicBool::new(false),
    });
    let h = handle.clone();
    let t = thread::spawn(move || {
        let _ = h.id.set(thread::current().id());
        #[cfg(not(feature = "async"))]
        let v = thunk.call(&[], &mut ContBarrier::new());

        #[cfg(feature = "async")]
        let v = thunk.call_sync(&[], &mut ContBarrier::new());

        match v {
            Ok(vals) => h.result.set(Value::from(List::from_iter(vals))),
            Err(e) => {
                h.result.set(e.0);
                h.failed.store(true, Ordering::Release);
            }
        }
    });

    *handle.thread.lock() = Some(t);
    handle
}

#[bridge(name = "join", lib = "(threads (1))")]
pub fn join(
    handle: Embedded<JoinHandle>,
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let thread = handle.thread.lock().take();
    if let Some(t) = thread {
        without_gc_watching(|| t.join()).expect("spawned thread panicked");
    }
    let v = handle.result.get();
    if handle.failed.load(Ordering::Acquire) {
        return Err(Exception(v));
    }
    let mut vals = Vec::new();
    list_to_vec(&v, &mut vals);
    Ok(barrier.call_cont(Args::pack(vals)))
}

#[bridge(name = "sleep", lib = "(threads (1))")]
pub fn sleep(ms: u64) -> Result<(), Exception> {
    without_gc_watching(|| {
        thread::sleep(Duration::from_millis(ms));
        Ok(())
    })
}

#[bridge(name = "join-handle?", lib = "(threads (1))")]
pub fn join_handle_pred(obj: Value) -> bool {
    obj.is_a::<Embedded<JoinHandle>>()
}
