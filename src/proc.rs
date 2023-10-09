use crate::{
    ast::Body,
    continuation::Continuation,
    env::Env,
    error::RuntimeError,
    eval::{Eval, ValueOrPreparedCall},
    gc::Gc,
    syntax::{Identifier, Mark},
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
    ) -> Result<ValueOrPreparedCall, RuntimeError>;
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
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
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

        if let Some(ref _remaining) = self.remaining {
            todo!()
        } else if args_iter.peek().is_some() {
            return Err(RuntimeError::wrong_num_of_args(self.args.len(), provided));
        }

        self.body.tail_eval(&Env::from(env.clone()), cont).await
    }
}

pub type ExprFuture = BoxFuture<'static, Result<Gc<Value>, RuntimeError>>;

#[derive(Debug, Clone)]
pub struct ExternalFn {
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
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        // TODO: check the arguments
        Ok(ValueOrPreparedCall::Value(
            (self.func)(cont.clone(), args).await?,
        ))
    }
}

pub struct PreparedCall {
    operator: Gc<Value>,
    args: Vec<Gc<Value>>,
}

impl PreparedCall {
    pub async fn eval(self, cont: &Option<Arc<Continuation>>) -> Result<Gc<Value>, RuntimeError> {
        let mut curr_proc = Some(self);
        loop {
            let proc = curr_proc.take().unwrap();
            let op = proc.operator.read().await;
            let Some(callable) = op.as_callable() else {
                return Err(RuntimeError::invalid_operator_type(op.type_name()));
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
            let ret = callable.call(proc.args, cont).await?;
            match ret {
                ValueOrPreparedCall::Value(value) => return Ok(value),
                ValueOrPreparedCall::PreparedCall(prepared) => {
                    curr_proc = Some(prepared);
                    // Continue
                }
            }
        }
    }

    pub fn prepare(args: Vec<Gc<Value>>) -> Self {
        let operator = args[0].clone();
        let args = args[1..].to_owned();
        Self { operator, args }
    }
}
