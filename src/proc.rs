use crate::{
    ast::Body,
    continuation::Continuation,
    env::Env,
    error::{Frame, RuntimeError},
    eval::{Eval, ValuesOrPreparedCall},
    gc::Gc,
    syntax::{Identifier, Mark, Span},
    value::Value,
};
use async_trait::async_trait;
use futures::future::BoxFuture;
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

#[derive(Clone)]
pub struct Procedure {
    pub up: Env,
    pub args: Vec<Identifier>,
    pub remaining: Option<Identifier>,
    pub body: Body,
    pub mark: Mark,
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
        println!("this one?");
        let env = Gc::new(self.up.new_lexical_contour(self.mark));
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

pub type ExprFuture = BoxFuture<'static, Result<Vec<Gc<Value>>, RuntimeError>>;

#[derive(Debug, Clone)]
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
        println!("maybe this one?");
        // TODO: check the arguments
        Ok(ValuesOrPreparedCall::Values(
            (self.func)(cont.clone(), args).await?,
        ))
    }
}

pub struct PreparedCall {
    proc_name: String,
    location: Span,
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
            bt.push(Frame::new(proc.proc_name.clone(), proc.location.clone()));
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

    pub fn prepare(proc_name: &str, location: &Span, args: Vec<Gc<Value>>) -> Self {
        let operator = args[0].clone();
        let args = args[1..].to_owned();
        Self {
            proc_name: proc_name.to_string(),
            location: location.clone(),
            operator,
            args,
        }
    }
}
