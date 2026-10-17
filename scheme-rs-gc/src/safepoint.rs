use std::sync::Arc;

use crate::gc_stack::GcStack;
use crate::sync::{AtomicU8, Ordering, lock};
use rt::{CHANGED, STOP, THREADS, ThreadIndex, thread, thread_local};

#[derive(PartialEq)]
#[repr(u8)]
enum RunState {
    OutsideGc = 0, // In Rust code, or blocked
    InsideGc = 1,
    Parked = 2,
}

impl RunState {
    const fn from_u8(n: u8) -> Self {
        match n {
            0 => RunState::OutsideGc,
            1 => RunState::InsideGc,
            2 => RunState::Parked,
            _ => panic!("Invalid runstate"),
        }
    }
}

struct AtomicRunState(AtomicU8);

impl AtomicRunState {
    fn new(state: RunState) -> Self {
        Self(AtomicU8::new(state as u8))
    }

    fn load(&self) -> RunState {
        RunState::from_u8(self.0.load(Ordering::Relaxed))
    }

    fn store(&self, state: RunState) {
        self.0.store(state as u8, Ordering::Relaxed);
    }
}

// Represents a single thread and it's state at pause.
struct ThreadState {
    state: AtomicRunState,
    stack: GcStack,
}

impl ThreadState {
    fn new() -> ThreadState {
        ThreadState {
            state: AtomicRunState::new(RunState::OutsideGc),
            stack: GcStack::new(),
        }
    }

    fn set_runstate(&self, state: RunState) {
        let _lock = lock(&THREADS);
        self.state.store(state);

        if STOP.load(Ordering::Relaxed) {
            CHANGED.notify_all();
        }
    }

    fn enter_gc_awareness(&self) {
        let mut threads = lock(&THREADS);
        while STOP.load(Ordering::Acquire) {
            threads = CHANGED.wait(threads).unwrap_or_else(|p| p.into_inner())
        }
        self.state.store(RunState::InsideGc); // No need to notify here
    }

    fn is_in_gc(&self) -> bool {
        self.state.load() == RunState::InsideGc
    }
}

#[cfg(not(loom))]
mod rt {
    use std::{
        collections::HashMap,
        sync::{Arc, LazyLock},
    };

    use super::ThreadState;
    use crate::sync::{AtomicBool, Condvar, Mutex};

    pub(super) use std::{thread, thread::ThreadId as ThreadIndex, thread_local};

    pub(super) static THREADS: LazyLock<Mutex<HashMap<ThreadIndex, Arc<ThreadState>>>> =
        LazyLock::new(|| Mutex::new(HashMap::new()));
    pub(super) static STOP: AtomicBool = AtomicBool::new(false);
    pub(super) static CHANGED: Condvar = Condvar::new();

    // For JIT
    pub fn safepoint_flag() -> *const AtomicBool {
        &STOP
    }
}

#[cfg(loom)]
mod rt {
    use std::{collections::HashMap, sync::Arc};

    use super::ThreadState;
    use crate::sync::{AtomicBool, Condvar, Mutex};

    pub(super) use loom::{thread, thread::ThreadId as ThreadIndex, thread_local};

    loom::lazy_static! {
        pub(super) static ref THREADS: Mutex<HashMap<ThreadIndex, Arc<ThreadState>>> =
            Mutex::new(HashMap::new());
        pub(super) static ref STOP: AtomicBool = AtomicBool::new(false);
        pub(super) static ref CHANGED: Condvar = Condvar::new();
    }
}

struct RegisteredThread {
    id: ThreadIndex,
    state: Arc<ThreadState>,
}

impl Drop for RegisteredThread {
    fn drop(&mut self) {
        let mut threads = lock(&THREADS);
        let id = self.id;
        threads.remove(&id);
        CHANGED.notify_all();
    }
}

impl RegisteredThread {
    pub(crate) fn is_in_gc(&self) -> bool {
        self.state.is_in_gc()
    }
}

thread_local! {
    static ME: RegisteredThread = {
        let state = Arc::new(ThreadState::new());
        let id = thread::current().id();
        lock(&THREADS).insert(id, state.clone());
        RegisteredThread { id, state }
    }
}

#[inline(always)]
pub fn should_park() -> bool {
    STOP.load(Ordering::Relaxed)
}

#[cfg(not(loom))]
pub use rt::safepoint_flag;

// Values must be created inside this block, otherwise they are invisible to
// memory management.
#[inline(always)] // stack is captured, so this needs to be inlined.
pub fn with_gc_watching<R>(f: impl FnOnce() -> R) -> R {
    ME.with(|me| {
        if me.is_in_gc() {
            f()
        } else {
            me.state.stack.start_segment_here();
            me.state.enter_gc_awareness();
            let _in_gc_scope = InGcScope(me);
            f()
        }
    })
}

// Makes it safe for if f() panics
struct InGcScope<'a>(&'a RegisteredThread);

impl Drop for InGcScope<'_> {
    fn drop(&mut self) {
        self.0.state.stack.close_segment();
        self.0.state.set_runstate(RunState::OutsideGc);
    }
}

#[inline(always)]
pub fn without_gc_watching<R>(f: impl FnOnce() -> R) -> R {
    ME.with(|me| {
        if !me.is_in_gc() {
            f()
        } else {
            me.state.stack.capture();
            me.state.set_runstate(RunState::OutsideGc);
            let _outside_gc = OutsideGcScope(me);
            f()
        }
    })
}

struct OutsideGcScope<'a>(&'a RegisteredThread);

impl Drop for OutsideGcScope<'_> {
    fn drop(&mut self) {
        self.0.state.enter_gc_awareness();
    }
}

#[inline(never)]
pub fn park_self() {
    ME.with(|me| {
        if !me.is_in_gc() {
            return;
        }
        me.state.stack.capture();
        me.state.set_runstate(RunState::Parked);
        me.state.enter_gc_awareness();
    })
}

// Marker indicating that mutators are Parked or OutsideGc
pub struct Stopped(());
impl Drop for Stopped {
    fn drop(&mut self) {
        let _threads = lock(&THREADS);
        STOP.store(false, Ordering::Relaxed);
        CHANGED.notify_all();
    }
}

pub fn stop_the_world() -> Stopped {
    STOP.store(true, Ordering::Relaxed);
    let mut threads = lock(&THREADS);

    while threads.values().any(|t| t.is_in_gc()) {
        threads = CHANGED.wait(threads).unwrap_or_else(|p| p.into_inner());
    }

    Stopped(())
}

pub fn for_each_threads_words(_stopped: &Stopped, mut f: impl FnMut(usize)) {
    let threads = lock(&THREADS);
    for t in threads.values() {
        unsafe {
            t.stack.for_each_word(&mut f);
        };
    }
}
