use crate::{
    exception::{Exception, ExceptionHandler},
    gc::{Gc, GcInner, Trace},
    lists::{list_to_vec, slice_to_list},
    registry::BridgeFn,
    runtime::Runtime,
    value::Value,
};
use futures::future::BoxFuture;
use std::{borrow::Cow, collections::HashMap, ptr::null_mut};

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

pub type Record = Vec<Gc<Value>>;

/// A function pointer to a generated continuation.
pub type ContinuationPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
) -> *mut Application;

/// A function pointer to a generated closure function.
pub type ClosurePtr = unsafe extern "C" fn(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
    cont: *mut GcInner<Value>,
) -> *mut Application;

/// A function pointer to an async Rust bridge function.
pub type BridgePtr = for<'a> fn(
    args: &'a [Gc<Value>],
    rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
) -> BoxFuture<'a, Result<Application, Exception>>;

#[derive(Debug, Copy, Clone)]
pub enum FuncPtr {
    Continuation(ContinuationPtr),
    Closure(ClosurePtr),
    Bridge(BridgePtr),
}

unsafe impl Trace for FuncPtr {
    unsafe fn visit_children(&self, _visitor: unsafe fn(crate::gc::OpaqueGcPtr)) {}

    unsafe fn finalize(&mut self) {}
}

#[derive(Clone, derive_more::Debug, Trace)]
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

    pub async fn call(&self, args: &[Gc<Value>]) -> Result<Record, Exception> {
        unsafe extern "C" fn just_return(
            _runtime: *mut GcInner<Runtime>,
            _env: *const *mut GcInner<Value>,
            _globals: *const *mut GcInner<Value>,
            args: *const *mut GcInner<Value>,
            _exception_handler: *mut GcInner<ExceptionHandler>,
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
            FuncPtr::Continuation(just_return),
            0,
            true,
            true,
        ))));
        self.apply(&args, None).await?.eval().await
    }

    pub async fn apply(
        &self,
        args: &[Gc<Value>],
        exception_handler: Option<Gc<ExceptionHandler>>,
    ) -> Result<Application, Exception> {
        // Handle arguments

        // Extract the continuation, if it is required
        let cont = !matches!(self.func, FuncPtr::Continuation(_));
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
        let bridge = matches!(self.func, FuncPtr::Bridge(_));
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
            let FuncPtr::Bridge(async_fn) = self.func else {
                unreachable!()
            };
            (async_fn)(
                args.as_ref(),
                rest_args.unwrap_or(&[]),
                cont.unwrap(),
                &exception_handler,
            )
            .await
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
                FuncPtr::Continuation(sync_fn) => unsafe {
                    let app = (sync_fn)(
                        self.runtime.as_ptr(),
                        env.as_ptr(),
                        globals.as_ptr(),
                        args.as_ptr(),
                        exception_handler.as_ref().map_or_else(null_mut, Gc::as_ptr),
                    );
                    *Box::from_raw(app)
                },
                FuncPtr::Closure(sync_fn) => unsafe {
                    let app = (sync_fn)(
                        self.runtime.as_ptr(),
                        env.as_ptr(),
                        globals.as_ptr(),
                        args.as_ptr(),
                        exception_handler.as_ref().map_or_else(null_mut, Gc::as_ptr),
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

/// An application of a function to a given set of values.
pub struct Application {
    /// The operator being applied to. If None, we return the values to the Rust
    /// caller.
    op: Option<Closure>,
    /// The arguments being applied to the operator.
    args: Record,
    /// The current exception handler to be passed to the operator.
    exception_handler: Option<Gc<ExceptionHandler>>,
}

impl Application {
    pub fn new(op: Closure, args: Record, exception_handler: Option<Gc<ExceptionHandler>>) -> Self {
        Self {
            // We really gotta figure out how to deal with this better
            op: Some(op),
            args,
            exception_handler,
        }
    }

    pub fn values(args: Record) -> Self {
        Self {
            op: None,
            args,
            exception_handler: None,
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    pub async fn eval(mut self) -> Result<Record, Exception> {
        while let Application {
            op: Some(op),
            args,
            exception_handler,
        } = self
        {
            self = op.apply(&args, exception_handler).await?;
        }
        // If we have no operator left, return the arguments as the final values:
        Ok(self.args)
    }
}

pub fn apply<'a>(
    args: &'a [Gc<Value>],
    rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
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
        Ok(Application::new(
            op.clone(),
            args,
            exception_handler.clone(),
        ))
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

unsafe extern "C" fn call_consumer_with_values(
    _runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
) -> *mut Application {
    // env[0] is the consumer
    let consumer = Gc::from_ptr(env.read());
    let consumer = {
        let consumer_ref = consumer.read();
        let consumer: &Closure = consumer_ref.as_ref().try_into().unwrap();
        consumer.clone()
    };
    // env[1] is the continuation
    let cont = Gc::from_ptr(env.add(1).read());

    let mut collected_args: Vec<_> = (0..consumer.num_required_args)
        .map(|i| Gc::from_ptr(args.add(i).read()))
        .collect();

    // I hate this constant going back and forth from variadic to list. I have
    // to figure out a way to make it consistent
    if consumer.variadic {
        let rest_args = Gc::from_ptr(args.add(consumer.num_required_args).read());
        let mut vec = Vec::new();
        list_to_vec(&rest_args, &mut vec);
        collected_args.extend(vec);
    }

    collected_args.push(cont);

    let exception_handler = if exception_handler.is_null() {
        None
    } else {
        Some(Gc::from_ptr(exception_handler))
    };

    Box::into_raw(Box::new(Application::new(
        consumer,
        collected_args,
        exception_handler,
    )))
}

pub fn call_with_values<'a>(
    args: &'a [Gc<Value>],
    _rest_args: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
) -> BoxFuture<'a, Result<Application, Exception>> {
    Box::pin(async move {
        let [producer, consumer] = args else {
            return Err(Exception::wrong_num_of_args(2, args.len()));
        };

        // Fetch the producer
        let producer = {
            let producer_ref = producer.read();
            let producer: &Closure = producer_ref.as_ref().try_into()?;
            producer.clone()
        };

        // Get the details of the consumer:
        let (num_required_args, variadic) = {
            let consumer_ref = consumer.read();
            let consumer: &Closure = consumer_ref.as_ref().try_into()?;
            (consumer.num_required_args, consumer.variadic)
        };

        let call_consumer_closure = Closure::new(
            producer.runtime.clone(),
            vec![consumer.clone(), cont.clone()],
            Vec::new(),
            FuncPtr::Continuation(call_consumer_with_values),
            num_required_args,
            variadic,
            false,
        );

        Ok(Application::new(
            producer,
            vec![Gc::new(Value::Closure(call_consumer_closure))],
            exception_handler.clone(),
        ))
    })
}

inventory::submit! {
    BridgeFn::new("call-with-values", "(base)", 2, false, call_with_values)
}
