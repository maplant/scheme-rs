use crate::{
    gc::{Gc, GcInner, Trace}, lists::slice_to_list, runtime::Runtime, value::Value
};
use futures::future::BoxFuture;
use std::borrow::Cow;

/*
pub struct ProcCallDebugInfo {
    proc_name: String,
    location: Span,
}

impl ProcCallDebugInfo {
    pub fn new(proc_name: &str, location: &Span) -> Self {
        Self {
            proc_name: proc_name.to_string(),
            location: location.clone(),
        }
    }
}

#[builtin("apply")]
pub async fn apply(
    _cont: &Option<Arc<Continuation>>,
    mut args: Vec<Gc<Value>>,
) -> Result<PreparedCall, RuntimeError> {
    if args.len() < 2 {
        return Err(RuntimeError::wrong_num_of_args(2, args.len()));
    }
    let last = args.pop().unwrap();
    list_to_vec(&last, &mut args);
    Ok(PreparedCall::prepare(args, None))
}
*/

pub type Record = Box<[Gc<Value>]>;

pub type SyncFuncPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
) -> *mut Application;

pub type SyncFuncWithContinuationPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    cont: *mut GcInner<Value>,
) -> *mut Application;

pub type AsyncFuncPtr = fn(args: &[Gc<Value>]) -> BoxFuture<'static, Application>;

#[derive(Debug)]
pub enum FuncPtr {
    SyncFunc(SyncFuncPtr),
    SyncFuncWithContinuation(SyncFuncWithContinuationPtr),
    AsyncFunc(AsyncFuncPtr),
}

#[derive(Debug)]
pub struct Closure {
    runtime: Gc<Runtime>,
    env: Record,
    globals: Record,
    func: FuncPtr,
    num_required_args: usize,
    variadic: bool,
}

unsafe impl Trace for Closure {
    unsafe fn visit_children(&self, visitor: unsafe fn(crate::gc::OpaqueGcPtr)) {
        visitor(self.runtime.as_opaque());
        self.env.visit_children(visitor);
        self.globals.visit_children(visitor);
    }

    unsafe fn finalize(&mut self) {
        self.env.finalize();
        self.globals.finalize();
    }
}

impl Closure {
    pub fn new(
        runtime: Gc<Runtime>,
        env: impl Into<Record>,
        globals: impl Into<Record>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
    ) -> Self {
        Self {
            runtime,
            env: env.into(),
            globals: globals.into(),
            func,
            num_required_args,
            variadic,
        }
    }

    pub async fn apply(&self, args: &[Gc<Value>]) -> Application {
        match self.func {
            FuncPtr::SyncFunc(sync_fn) => {
                if args.len() < self.num_required_args {
                    panic!("Too few arguments");
                }
                let args = match (self.variadic, args.split_at_checked(self.num_required_args)) {
                    (true, Some((args, rest_args))) => {
                        let mut args = args.to_owned();
                        args.push(Gc::new(slice_to_list(rest_args)));
                        Cow::Owned(args)
                    }
                    (true, None) => {
                        let mut args = args.to_owned();
                        args.push(Gc::new(Value::Null));
                        Cow::Owned(args)
                    }
                    (false, _) => Cow::Borrowed(args),
                };

                let env = values_to_vec_of_ptrs(&self.env);
                let globals = values_to_vec_of_ptrs(&self.globals);

                // Safety: args must last until the return of app so any freshly allocated var
                // arg isn't dropped before it's upgraded to a proper Gc
                let args = values_to_vec_of_ptrs(args.as_ref());

                let app = unsafe { (sync_fn)(self.runtime.as_ptr(), env.as_ptr(), globals.as_ptr(), args.as_ptr()) };
                let app = unsafe { Box::from_raw(app) };

                drop(args);

                *app
            }
            FuncPtr::SyncFuncWithContinuation(sync_fn) => {
                if args.len() < self.num_required_args {
                    panic!("Too few arguments");
                }
                // I think this code could definitely be simplified, but I am a little scatter-brained right now.
                let (args, cont) =
                    match (self.variadic, args.split_at_checked(self.num_required_args)) {
                        (true, Some((args, [rest_args @ .., cont]))) => {
                            let mut args = args.to_owned();
                            args.push(Gc::new(slice_to_list(rest_args)));
                            (Cow::Owned(args), cont)
                        }
                        (true, Some(([ cont ], []))) => {
                            (Cow::Owned(vec![Gc::new(Value::Null)]), cont)
                        }
                        (false, _) => {
                            let (cont, args) = args.split_last().unwrap();
                            (Cow::Borrowed(args), cont)
                        }
                        _ => unreachable!("self: {:#?}, args: {:#?}", self, args),
                    };

                let env = values_to_vec_of_ptrs(&self.env);
                let globals = values_to_vec_of_ptrs(&self.globals);

                // Safety: args must last until the return of app so any freshly allocated var
                // arg isn't dropped before it's upgraded to a proper Gc
                let args = values_to_vec_of_ptrs(args.as_ref());

                let app = unsafe {
                    (sync_fn)(self.runtime.as_ptr(), env.as_ptr(), globals.as_ptr(), args.as_ptr(), cont.as_ptr())
                };
                let app = unsafe { Box::from_raw(app) };

                drop(args);

                *app
            }
            FuncPtr::AsyncFunc(async_fn) => async_fn(args).await,
        }
    }
}

// This is really sorta emblematic of my excess allocations. Really gotta fix that
// at some point.
fn values_to_vec_of_ptrs(vals: &[Gc<Value>]) -> Vec<*mut GcInner<Value>> {
    vals.iter().map(Gc::as_ptr).collect()
}

pub struct Application {
    op: Option<Gc<Closure>>,
    // Consider making this a Cow
    args: Box<[Gc<Value>]>,
}

impl Application {
    pub fn new(op: Gc<Closure>, args: impl Into<Record>) -> Self {
        Self {
            op: Some(op),
            args: args.into(),
        }
    }

    pub fn new_empty(args: impl Into<Record>) -> Self {
        Self {
            op: None,
            args: args.into(),
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    pub async fn eval(mut self) -> Box<[Gc<Value>]> {
        while let Application { op: Some(op), args } = self {
            self = op.read().apply(&args).await;
        }
        // If we have no operator left, return the arguments as the final values:
        self.args
    }
}
