use crate::{
    gc::{Gc, GcInner, Trace},
    lists::slice_to_list,
    runtime::Runtime,
    value::Value,
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

pub type AsyncFuncPtr = for<'a> fn(
    args: &'a [Gc<Value>],
    rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
) -> BoxFuture<'a, Application>;

#[derive(Debug)]
pub enum FuncPtr {
    SyncFunc(SyncFuncPtr),
    SyncFuncWithContinuation(SyncFuncWithContinuationPtr),
    AsyncFunc(AsyncFuncPtr),
}

#[derive(derive_more::Debug)]
pub struct Closure {
    #[debug(skip)]
    runtime: Gc<Runtime>,
    #[debug(skip)]
    env: Record,
    #[debug(skip)]
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
        // Handle arguments

        // Error if the number of arguments provided is incorrect
        if args.len() < self.num_required_args {
            panic!("Too few arguments");
        }
        if !self.variadic && args.len() > self.num_required_args {
            panic!("Too many arguments");
        }

        // Extract the continuation, if it is required
        let cont = !matches!(self.func, FuncPtr::SyncFunc(_));
        let (cont, args) = if cont {
            let (cont, args) = args.split_last().unwrap();
            (Some(cont), args)
        } else {
            (None, args)
        };

        // If this function is variadic, create a list to put any extra arguments
        // into
        let bridge = !matches!(self.func, FuncPtr::AsyncFunc(_));
        let (args, rest_args) = if self.variadic {
            let (args, rest_args) = args.split_at(self.num_required_args);
            // If this is a bridge function, vector is more natural to work with:
            if bridge {
                (Cow::Borrowed(args), Some(rest_args))
            } else {
                let mut args = args.to_owned();
                args.push(Gc::new(slice_to_list(rest_args)));
                (Cow::Owned(args), None)
            }
        } else {
            (Cow::Borrowed(args), None)
        };

        if bridge {
            // If this a bridge functiuon, calling it is relatively simple:
            let FuncPtr::AsyncFunc(async_fn) = self.func else {
                unreachable!()
            };
            (async_fn)(args.as_ref(), rest_args.unwrap(), cont.unwrap()).await
        } else {
            // For LLVM functions, we need to convert our args into raw pointers
            // and make sure any freshly allocated rest_args are disposed of poperly.

            let env = values_to_vec_of_ptrs(&self.env);
            let globals = values_to_vec_of_ptrs(&self.globals);

            // Safety: args must last until the return of app so any freshly allocated var
            // arg isn't dropped before it's upgraded to a proper Gc
            let args = values_to_vec_of_ptrs(args.as_ref());

            // Finally: call the function pointer
            let app = match self.func {
                FuncPtr::SyncFunc(sync_fn) => unsafe {
                    let app = (sync_fn)(
                        self.runtime.as_ptr(),
                        env.as_ptr(),
                        globals.as_ptr(),
                        args.as_ptr(),
                    );
                    *Box::from_raw(app)
                },
                FuncPtr::SyncFuncWithContinuation(sync_fn) => unsafe {
                    let app = (sync_fn)(
                        self.runtime.as_ptr(),
                        env.as_ptr(),
                        globals.as_ptr(),
                        args.as_ptr(),
                        cont.unwrap().as_ptr(),
                    );
                    *Box::from_raw(app)
                },
                _ => unreachable!(),
            };

            // Now we can drop the args
            drop(args);

            app
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
