use std::thread::ThreadId;
use std::collections::HashMap;
use std::sync::LazyLock;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::thread_local;
use std::cell::Cell;

use parking_lot::{Condvar, Mutex};

use crate::gc_stack::GcStack;

#[derive(Default)]
enum RunState {
    Running,
    Paused,
    #[default]
    InNative // In Rust code, or blocked
}

// Represents a single thread and it's state at pause.
struct ThreadState {
    state: Mutex<RunState>,
    stack: GcStack
}

impl ThreadState {
    fn new() -> ThreadState {
        ThreadState {
            state: Mutex::new(RunState::InNative),
            stack: GcStack::new()
        }
    }

    fn set_state(&self, state: RunState) {
        let mut _lock = THREADS.lock();
        let mut runstate = self.state.lock();
        *runstate = state;

        if STOP.load(Ordering::Relaxed) {
            CHANGED.notify_all();
        }
    }

    fn resume(&self) {
        let mut lock = THREADS.lock();
        while STOP.load(Ordering::Acquire) {
            CHANGED.wait(&mut lock);
        }
        *self.state.lock() = RunState::Running; // No need to notify here
    }
}

static STOP: AtomicBool = AtomicBool::new(false);
static CHANGED: Condvar = Condvar::new();
static THREADS: LazyLock<Mutex<HashMap<ThreadId, Arc<ThreadState>>>> = LazyLock::new(|| { Mutex::new(HashMap::new()) });

struct RegisteredThread {
    state: Arc<ThreadState>,
    depth: Cell<usize>
}

impl Drop for RegisteredThread {
    fn drop(&mut self) {
        let mut threads = THREADS.lock();
        let id = std::thread::current().id();
        threads.remove(&id);
        CHANGED.notify_all();
        // A subtle bug here: the same owning thread must drop it.
        // So do not move the reference between threads.
    }
}

struct SchemeScope<'a> {
    me: &'a RegisteredThread,
    depth: usize
}

impl Drop for SchemeScope<'_> {
    // if f (further down) aborts we still want to reset depth
    fn drop(&mut self) {
        self.me.depth.set(self.depth);
        if self.depth == 0 {
            self.me.state.stack.clear();
            self.me.state.set_state(RunState::InNative);
        }
    }
}

thread_local! {
    static ME: RegisteredThread = {
        let state = Arc::new(ThreadState::new());
        let id = std::thread::current().id();
        THREADS.lock().insert(id, state.clone());
        RegisteredThread { state, depth: Cell::new(0) }
    }
}

#[inline(always)]
pub fn safepoint() -> bool {
    STOP.load(Ordering::Relaxed)
}

// For JIT
pub fn safepoint_flag() -> *const AtomicBool {
    &STOP
}

// Values must be created inside this block, otherwise they are invisible to
// memory management.
pub fn with_scheme<R>(f: impl FnOnce() -> R) -> R {
    ME.with(|me| {
        let depth = me.depth.get();
        if depth == 0 {
            me.state.stack.set_base();
            me.state.resume();
        }
        me.depth.set(depth + 1);
        let r = f();
        me.depth.set(depth);
        if depth == 0 {
            me.state.stack.clear();
            me.state.set_state(RunState::InNative);
        }
        r
    })
}
