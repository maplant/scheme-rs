use crate::{
    ast::Body,
    builtin,
    continuation::Continuation,
    env::Env,
    error::{Frame, RuntimeError},
    eval::{Eval, ValuesOrPreparedCall},
    gc::Gc,
    lists::list_to_vec,
    syntax::{Identifier, Span},
    value::Value,
};
use async_trait::async_trait;
use futures::future::BoxFuture;
use proc_macros::Trace;
use std::sync::Arc;

#[async_trait]
pub trait Callable: Send + Sync + 'static {
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
    pub up: Env,
    pub args: Vec<Identifier>,
    pub remaining: Option<Identifier>,
    #[debug(skip)]
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
            env.write().await.def_var(required, value);
        }

        if let Some(ref remaining) = self.remaining {
            env.write().await.def_var(
                remaining,
                Gc::new(Value::from(args_iter.cloned().collect::<Vec<_>>())),
            );
        } else if args_iter.peek().is_some() {
            return Err(RuntimeError::wrong_num_of_args(self.args.len(), provided));
        }

        self.body.tail_eval(&Env::from(env.clone()), cont).await
    }
}

pub type ExprFuture = BoxFuture<'static, Result<ValuesOrPreparedCall, RuntimeError>>;

#[derive(Debug, Clone, Trace)]
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
    proc_debug_info: Option<ProcDebugInfo>,
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
            if let Some(ProcDebugInfo {
                proc_name,
                location,
            }) = proc.proc_debug_info
            {
                bt.push(Frame::new(proc_name.clone(), location.clone()));
            }
            let callable = {
                let proc = proc.operator.read().await;
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
    pub fn prepare(args: Vec<Gc<Value>>, proc_debug_info: Option<ProcDebugInfo>) -> Self {
        let operator = args[0].clone();
        let args = args[1..].to_owned();
        Self {
            proc_debug_info,
            operator,
            args,
        }
    }
}

pub struct ProcDebugInfo {
    proc_name: String,
    location: Span,
}

impl ProcDebugInfo {
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
        return Err(RuntimeError::wrong_num_of_args(2, 3));
    }
    let last = args.pop().unwrap();
    list_to_vec(&last, &mut args).await;
    Ok(PreparedCall::prepare(args, None))
}
