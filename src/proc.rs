use crate::{
    exception::Exception,
    gc::{Gc, GcInner, Trace},
    lists::{list_to_vec, slice_to_list},
    registry::BridgeFn,
    runtime::Runtime,
    value::Value,
};
use futures::future::BoxFuture;
use std::{borrow::Cow, collections::HashMap};

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
) -> BoxFuture<'a, Result<Application, Exception>>;

#[derive(Debug, Copy, Clone)]
pub enum FuncPtr {
    SyncFunc(SyncFuncPtr),
    SyncFuncWithContinuation(SyncFuncWithContinuationPtr),
    AsyncFunc(AsyncFuncPtr),
}

#[derive(Clone, derive_more::Debug)]
// TODO: Add an optional name to the closure for debugging purposes
pub struct Closure {
    #[debug(skip)]
    pub runtime: Gc<Runtime>,
    #[debug(skip)]
    pub(crate) env: Record,
    #[debug(skip)]
    pub(crate) globals: Record,
    pub(crate) func: FuncPtr,
    pub(crate) num_required_args: usize,
    pub(crate) variadic: bool,
    pub(crate) continuation: bool,
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
        continuation: bool,
    ) -> Self {
        Self {
            runtime,
            env: env.into(),
            globals: globals.into(),
            func,
            num_required_args,
            variadic,
            continuation,
        }
    }

    pub(crate) fn deep_clone(&mut self, cloned: &mut HashMap<Gc<Value>, Gc<Value>>) {
        if !self.continuation {
            return;
        }
        for captured in &mut self.env {
            *captured = deep_clone_value(captured, cloned);
        }
    }

    pub async fn call(&self, args: &[Gc<Value>]) -> Result<Box<[Gc<Value>]>, Exception> {
        unsafe extern "C" fn just_return(
            _runtime: *mut GcInner<Runtime>,
            _env: *const *mut GcInner<Value>,
            _globals: *const *mut GcInner<Value>,
            args: *const *mut GcInner<Value>,
        ) -> *mut Application {
            crate::runtime::make_return_values(args.read())
        }

        let mut args = args.to_vec();
        // TODO: We don't need to create a new one of these every time, we should just have
        // one
        args.push(Gc::new(Value::Closure(Closure::new(
            self.runtime.clone(),
            Vec::new(),
            Vec::new(),
            FuncPtr::SyncFunc(just_return),
            0,
            true,
            true,
        ))));
        self.apply(&args).await?.eval().await
    }

    pub async fn apply(&self, args: &[Gc<Value>]) -> Result<Application, Exception> {
        // Handle arguments

        // Extract the continuation, if it is required
        let cont = !matches!(self.func, FuncPtr::SyncFunc(_));
        let (cont, args) = if cont {
            let (cont, args) = args.split_last().unwrap();
            (Some(cont), args)
        } else {
            (None, args)
        };

        // Error if the number of arguments provided is incorrect
        if args.len() < self.num_required_args {
            return Err(Exception::wrong_num_of_args(
                self.num_required_args,
                args.len(),
            ));
        }
        if !self.variadic && args.len() > self.num_required_args {
            return Err(Exception::wrong_num_of_args(
                self.num_required_args,
                args.len(),
            ));
        }

        // If this function is variadic, create a list to put any extra arguments
        // into
        let bridge = matches!(self.func, FuncPtr::AsyncFunc(_));
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
            (async_fn)(args.as_ref(), rest_args.unwrap_or(&[]), cont.unwrap()).await
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

            Ok(app)
        }
    }
}

// This is really sorta emblematic of my excess allocations. Really gotta fix that
// at some point.
fn values_to_vec_of_ptrs(vals: &[Gc<Value>]) -> Vec<*mut GcInner<Value>> {
    vals.iter().map(Gc::as_ptr).collect()
}

pub struct Application {
    op: Option<Closure>,
    // Consider making this a Cow
    args: Box<[Gc<Value>]>,
}

impl Application {
    pub fn new(op: Closure, args: impl Into<Record>) -> Self {
        Self {
            // We really gotta figure out how to deal with this better
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
    pub async fn eval(mut self) -> Result<Box<[Gc<Value>]>, Exception> {
        while let Application { op: Some(op), args } = self {
            self = op.apply(&args).await?;
        }
        // If we have no operator left, return the arguments as the final values:
        Ok(self.args)
    }
}

pub fn apply<'a>(
    args: &'a [Gc<Value>],
    rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
) -> BoxFuture<'a, Result<Application, Exception>> {
    Box::pin(async move {
        if rest_args.is_empty() {
            return Err(Exception::wrong_num_of_args(2, args.len()));
        }
        let op = args[0].read();
        let op: &Closure = op.as_ref().try_into()?;
        let (last, args) = rest_args.split_last().unwrap();
        let mut args = args.to_vec();
        list_to_vec(last, &mut args);
        args.push(cont.clone());
        Ok(Application::new(op.clone(), args))
    })
}

inventory::submit! {
    BridgeFn::new("apply", "(base)", 1, true, apply)
}

pub(crate) fn deep_clone_value(
    value: &Gc<Value>,
    cloned: &mut HashMap<Gc<Value>, Gc<Value>>,
) -> Gc<Value> {
    if let Some(cloned) = cloned.get(value) {
        return cloned.clone();
    }
    let val_ref = value.read();
    match &*val_ref {
        Value::Closure(clos) => {
            let clos_cloned = Gc::new(Value::Closure(clos.clone()));
            cloned.insert(value.clone(), clos_cloned.clone());
            {
                let mut clos_mut = clos_cloned.write();
                let clos_cloned: &mut Closure = clos_mut.as_mut().try_into().unwrap();
                clos_cloned.deep_clone(cloned);
            }
            clos_cloned
        }
        val => {
            let val_cloned = Gc::new(val.clone());
            cloned.insert(value.clone(), val_cloned.clone());
            val_cloned
        }
    }
}
