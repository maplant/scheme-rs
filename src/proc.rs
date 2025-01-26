use crate::{
    ast::Body,
    builtin,
    // continuation::Continuation,
    // env::Env,
    error::{Frame, RuntimeError},
    gc::{Gc, GcInner, Trace},
    lists::list_to_vec,
    syntax::{Identifier, Span},
    value::Value,
};
use async_trait::async_trait;
use either::Either;
use futures::future::BoxFuture;
use std::sync::Arc;

/*

pub enum ValuesOrPreparedCall {
    Values(Vec<Gc<Value>>),
    PreparedCall(PreparedCall),
}

impl From<Vec<Gc<Value>>> for ValuesOrPreparedCall {
    fn from(values: Vec<Gc<Value>>) -> Self {
        Self::Values(values)
    }
}

impl From<PreparedCall> for ValuesOrPreparedCall {
    fn from(prepared_call: PreparedCall) -> ValuesOrPreparedCall {
        Self::PreparedCall(prepared_call)
    }
}

impl ValuesOrPreparedCall {
    pub async fn eval(
        self,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        match self {
            Self::Values(val) => Ok(val),
            Self::PreparedCall(prepared_call) => prepared_call.eval(cont).await,
        }
    }
}

#[async_trait]
pub trait Callable: Send + Sync + 'static + std::fmt::Debug {
    fn min_args(&self) -> usize;

    fn max_args(&self) -> Option<usize>;

    async fn call(
        &self,
        mut args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError>;
}

#[derive(Clone, derive_more::Debug, Trace)]
pub struct Procedure {
    #[debug(skip)]
    pub up: Gc<Env>,
    pub args: Vec<Identifier>,
    pub remaining: Option<Identifier>,
    pub body: Body,
    pub is_variable_transformer: bool,
}

#[async_trait]
impl Callable for Procedure {
    fn min_args(&self) -> usize {
        self.args.len()
    }

    fn max_args(&self) -> Option<usize> {
        self.remaining.is_none().then_some(self.args.len())
    }

    async fn call(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let env = Gc::new(self.up.new_lexical_contour());
        let provided = args.len();
        let mut args_iter = args.iter().peekable();

        for required in &self.args {
            // We shouldn't ever need to check this, but probably safer to put
            // this call here as well.
            let Some(value) = args_iter.next().cloned() else {
                return Err(RuntimeError::wrong_num_of_args(self.args.len(), provided));
            };
            env.write().def_local_var(required, value);
        }

        if let Some(ref remaining) = self.remaining {
            env.write().def_local_var(
                remaining,
                Gc::new(Value::from(args_iter.cloned().collect::<Vec<_>>())),
            );
        } else if args_iter.peek().is_some() {
            return Err(RuntimeError::wrong_num_of_args(self.args.len(), provided));
        }

        self.body.tail_eval(&env, cont).await
    }
}

pub type ExprFuture = BoxFuture<'static, Result<ValuesOrPreparedCall, RuntimeError>>;

#[derive(Debug, Copy, Clone, Trace)]
pub struct ExternalFn {
    pub name: &'static str,
    pub num_args: usize,
    pub variadic: bool,
    pub func: fn(Option<Arc<Continuation>>, Vec<Gc<Value>>) -> ExprFuture,
}

#[async_trait]
impl Callable for ExternalFn {
    fn min_args(&self) -> usize {
        self.num_args
    }

    fn max_args(&self) -> Option<usize> {
        (!self.variadic).then_some(self.num_args)
    }

    async fn call(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        // TODO: check the arguments
        (self.func)(cont.clone(), args).await
    }
}

pub struct PreparedCall {
    proc_debug_info: Option<ProcCallDebugInfo>,
    operator: Gc<Value>,
    args: Vec<Gc<Value>>,
}

impl PreparedCall {
    pub async fn eval(
        self,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let mut curr_proc = Some(self);
        let mut bt = Vec::new();
        loop {
            let proc = curr_proc.take().unwrap();
            if let Some(ProcCallDebugInfo {
                proc_name,
                location,
            }) = proc.proc_debug_info
            {
                bt.push(Frame::new(proc_name.clone(), location.clone()));
            }
            let callable = {
                let proc = proc.operator.read();
                let Some(callable) = proc.as_callable() else {
                    return Err(RuntimeError::invalid_operator_type(proc.type_name()));
                };
                drop(proc);
                callable
            };
            if proc.args.len() < callable.min_args() {
                return Err(RuntimeError::wrong_num_of_args(
                    callable.min_args(),
                    proc.args.len(),
                ));
            }
            if let Some(max_args) = callable.max_args() {
                if proc.args.len() > max_args {
                    return Err(RuntimeError::wrong_num_of_args(max_args, proc.args.len()));
                }
            }
            let ret = callable.call(proc.args, cont).await.map_err(|mut err| {
                err.backtrace.extend(std::mem::take(&mut bt));
                err
            })?;
            match ret {
                ValuesOrPreparedCall::Values(value) => return Ok(value),
                ValuesOrPreparedCall::PreparedCall(prepared) => {
                    curr_proc = Some(prepared);
                    // Continue
                }
            }
        }
    }

    /// Such a strange interface. Whatever. FIXME
    pub fn prepare(args: Vec<Gc<Value>>, proc_debug_info: Option<ProcCallDebugInfo>) -> Self {
        let operator = args[0].clone();
        let args = args[1..].to_owned();
        Self {
            proc_debug_info,
            operator,
            args,
        }
    }
}

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
    env: *const *mut GcInner<Value>,
    globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
) -> *mut Application;

pub type AsyncFuncPtr = fn(args: &[Gc<Value>]) -> BoxFuture<'static, Application>;

#[derive(Debug)]
pub struct Closure {
    env: Record,
    globals: Record,
    func: Either<SyncFuncPtr, AsyncFuncPtr>,
    // is_variable_transformer: bool,
    // is_variadic: bool,
}

unsafe impl Trace for Closure {
    unsafe fn visit_children(&self, visitor: unsafe fn(crate::gc::OpaqueGcPtr)) {
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
        env: impl Into<Record>,
        globals: impl Into<Record>,
        func: Either<SyncFuncPtr, AsyncFuncPtr>,
    ) -> Self {
        Self {
            env: env.into(),
            globals: globals.into(),
            func,
        }
    }

    pub async fn apply(&self, args: &[Gc<Value>]) -> Application {
        match self.func {
            Either::Left(sync_fn) => {
                let env = values_to_vec_of_ptrs(&self.env);
                let globals = values_to_vec_of_ptrs(&self.globals);
                let args = values_to_vec_of_ptrs(args);
                let app = unsafe { (sync_fn)(env.as_ptr(), globals.as_ptr(), args.as_ptr()) };
                let app = unsafe { Box::from_raw(app) };
                *app
            }
            Either::Right(async_fn) => async_fn(args).await,
        }
    }
}

// This is really sorta emblematic of my excess allocations. Really gotta fix that
// at some point.
fn values_to_vec_of_ptrs(vals: &[Gc<Value>]) -> Vec<*mut GcInner<Value>> {
    vals.iter().map(|val| val.clone().into_raw()).collect()
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
