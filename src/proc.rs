use crate::{
    ast::{self, Body},
    env::Env,
    error::RuntimeError,
    eval::{Eval, ValueOrPreparedCall},
    gc::Gc,
    syntax::{Identifier, Mark},
    value::Value, continuation::Continuation,
};
use futures::future::BoxFuture;
use std::{sync::Arc, borrow::Cow};

#[derive(Clone)]
pub struct Procedure {
    pub up: Env,
    pub args: Vec<Identifier>,
    pub remaining: Option<Identifier>,
    pub body: Body,
    pub mark: Mark,
    pub is_variable_transformer: bool,
}

impl Procedure {
    fn min_args(&self) -> usize {
        self.args.len()
    }

    fn max_args(&self) -> Option<usize> {
        self.remaining.is_none().then_some(self.args.len())
    }

    pub async fn call(&self, mut args: Vec<Gc<Value>>, cont: Option<Arc<Continuation>>) -> Result<Gc<Value>, RuntimeError> {
        let mut proc = Cow::Borrowed(self);
        loop {
            let env = Gc::new(proc.up.new_lexical_contour(proc.mark));
            let provided = args.len();
            let mut args_iter = args.iter().peekable();
            {
                for required in &proc.args {
                    // We shouldn't ever need to check this, but probably safer to put
                    // this call here as well.
                    let Some(value) = args_iter.next().cloned() else {
                        return Err(RuntimeError::wrong_num_of_args(proc.args.len(), provided));
                    };
                    env.write().await.def_var(required, value);
                }
            }

            if let Some(ref _remaining) = self.remaining {
                todo!()
            } else if args_iter.peek().is_some() {
                return Err(RuntimeError::wrong_num_of_args(proc.args.len(), provided));
            }

            let ret = proc.body.tail_eval(&Env::from(env.clone()), cont.clone()).await?;
            match ret {
                ValueOrPreparedCall::Value(value) => return Ok(value),
                ValueOrPreparedCall::PreparedCall(prepared) => {
                    proc = Cow::Owned(prepared.operator.read().await.as_proc().unwrap().clone());
                    args = prepared.args;
                    // Continue
                }
            }
        }
    }
}

pub type ExprFuture = BoxFuture<'static, Result<Gc<Value>, RuntimeError>>;

#[derive(Debug, Clone)]
pub struct ExternalFn {
    pub num_args: usize,
    pub variadic: bool,
    pub func: fn(Env, Vec<Gc<Value>>) -> ExprFuture,
}

impl ExternalFn {
    fn min_args(&self) -> usize {
        self.num_args
    }

    fn max_args(&self) -> Option<usize> {
        (!self.variadic).then_some(self.num_args)
    }

    async fn call(&self, env: &Env, args: Vec<Gc<Value>>) -> Result<Gc<Value>, RuntimeError> {
        // TODO: check arguments
        (self.func)(env.clone(), args).await
    }
}

pub struct PreparedCall {
    pub is_external: bool,
    operator: Gc<Value>,
    args: Vec<Gc<Value>>,
}

impl PreparedCall {
    pub async fn eval(self, env: &Env, cont: Option<Arc<Continuation>>) -> Result<Gc<Value>, RuntimeError> {
        let read_op = self.operator.read().await;
        // Call the operator with the arguments
        match &*read_op {
            Value::ExternalFn(extern_fn) => extern_fn.call(env, self.args).await,
            Value::Procedure(proc) => proc.call(self.args, cont).await,
            _ => unreachable!(),
        }
    }

    pub async fn prepare(call: &ast::Call, env: &Env) -> Result<Self, RuntimeError> {
        // Collect the operator
        let operator = call.operator.eval(env, todo!()).await?;
        let (is_external, args) = {
            let read_op = operator.read().await;
            if !read_op.is_callable() {
                return Err(RuntimeError::invalid_operator_type(read_op.type_name()));
            }
            // Check the number of arguments provided
            let (is_external, min_args, max_args) = match &*read_op {
                Value::ExternalFn(extern_fn) => (true, extern_fn.min_args(), extern_fn.max_args()),
                Value::Procedure(proc) => (false, proc.min_args(), proc.max_args()),
                _ => unreachable!(),
            };
            if call.args.len() < min_args {
                return Err(RuntimeError::wrong_num_of_args(min_args, call.args.len()));
            }
            if let Some(max_args) = max_args {
                if call.args.len() > max_args {
                    return Err(RuntimeError::wrong_num_of_args(max_args, call.args.len()));
                }
            }
            // Collect the arguments
            let mut args = Vec::new();
            for arg in &call.args {
                args.push(arg.eval(env, todo!()).await?);
            }
            (is_external, args)
        };

        Ok(Self {
            is_external,
            operator,
            args,
        })
    }
}
