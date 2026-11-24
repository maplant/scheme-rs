use std::marker::PhantomData;
use std::sync::Arc;

use crate::gc_stack::GcStack;
use std::sync::atomic::{AtomicU8, Ordering};
use rt::{CHANGED, STOP, THREADS, ThreadIndex, thread, thread_local};
use std::mem::DropGuard;

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
        let _lock = THREADS.lock();
        self.state.store(state);

        if STOP.load(Ordering::Relaxed) {
            CHANGED.notify_all();
        }
    }

    fn enter_gc_awareness(&self) {
        let mut threads = THREADS.lock();
        while STOP.load(Ordering::Acquire) {
            CHANGED.wait(&mut threads);
        }
        self.state.store(RunState::InsideGc); // No need to notify here
    }

    fn is_in_gc(&self) -> bool {
        self.state.load() == RunState::InsideGc
    }
}

mod rt {
    use std::{
        collections::HashMap,
        sync::{Arc, LazyLock},
    };

    use super::ThreadState;
    use std::sync::atomic::AtomicBool;

    use parking_lot::{Condvar, Mutex};

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

// !send + !sync
pub struct GcWatching(PhantomData<*mut ()>);

impl GcWatching {
    #[inline(always)]
    pub fn without_gc_watching<R>(&mut self, f: impl FnOnce() -> R) -> R {
        ME.with(|me| {
            if !me.is_in_gc() {
                f()
            } else {
                let _scope = DropGuard::new(me, |me| {
                    me.state.enter_gc_awareness();
                });
                me.state.stack.capture();
                me.state.set_runstate(RunState::OutsideGc);
                f()
            }
        })
    }
}

struct RegisteredThread {
    id: ThreadIndex,
    state: Arc<ThreadState>,
}

impl Drop for RegisteredThread {
    fn drop(&mut self) {
        let mut threads = THREADS.lock();
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
        THREADS.lock().insert(id, state.clone());
        RegisteredThread { id, state }
    }
}

#[inline(always)]
pub fn should_park() -> bool {
    STOP.load(Ordering::Relaxed)
}

pub use rt::safepoint_flag;

// Values must be created inside this block, otherwise they are invisible to
// memory management.
#[inline(always)] // stack is captured, so this needs to be inlined.
pub fn with_gc_watching<R>(f: impl FnOnce(&mut GcWatching) -> R) -> R {
    let mut watching = GcWatching(PhantomData);
    ME.with(|me| {
        if me.is_in_gc() {
            f(&mut watching)
        } else {
            let _scope = DropGuard::new(me, |me| {
                me.state.stack.close_segment();
                me.state.set_runstate(RunState::OutsideGc);
            });

            me.state.stack.start_segment_here();
            me.state.enter_gc_awareness();
            f(&mut watching)
        }
    })
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
        let _threads = THREADS.lock();
        STOP.store(false, Ordering::Relaxed);
        CHANGED.notify_all();
    }
}

pub fn stop_the_world() -> Stopped {
    STOP.store(true, Ordering::Relaxed);
    let mut threads = THREADS.lock();

    while threads.values().any(|t| t.is_in_gc()) {
        CHANGED.wait(&mut threads);
    }

    Stopped(())
}

pub fn for_each_threads_words(_stopped: &Stopped, mut f: impl FnMut(usize)) {
    let threads = THREADS.lock();
    for t in threads.values() {
        unsafe {
            t.stack.for_each_word(&mut f);
        };
    }
}
