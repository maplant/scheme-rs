use crate::{
    ast::{self, Body},
    builtin::Builtin,
    gc::{Gc, Trace},
    num::Number,
};
use async_trait::async_trait;
use futures::future::BoxFuture;
use std::{collections::HashMap, future::Future, sync::Arc};

#[derive(Debug)]
pub enum Value {
    Boolean(bool),
    Number(Number),
    Character(char),
    String(String),
    Symbol(usize),
    Nil,
    Pair(Gc<Value>, Gc<Value>),
    Vector(Vec<Value>),
    ByteVector(Vec<u8>),
    Procedure(Procedure),
    ExternalFn(ExternalFn),
}

impl Value {
    pub fn is_callable(&self) -> bool {
        matches!(self, Self::Procedure(_) | Self::ExternalFn(_))
    }
}

impl From<ExternalFn> for Value {
    fn from(extern_fn: ExternalFn) -> Self {
        Value::ExternalFn(extern_fn)
    }
}

impl Trace for Value {}

#[derive(Debug)]
pub struct Env {
    up: Option<Gc<Env>>,
    // TODO: This can, and should, be optimized into a symtab of offsets and a
    // Vector.
    defs: HashMap<String, Gc<Value>>,
}

impl Trace for Env {
    fn unroot(&self) {
        if let Some(ref up) = self.up {
            up.unroot();
        }
        self.defs.unroot();
    }
}

impl Env {
    pub fn base() -> Self {
        let mut base = Self {
            up: None,
            defs: HashMap::default(),
        };

        for builtin in inventory::iter::<Builtin> {
            println!("installing builtin: {}", builtin.name);
            builtin.install(&mut base);
        }

        base
    }

    pub async fn fetch(&self, var: &str) -> Option<Gc<Value>> {
        if let Some(val) = self.defs.get(var) {
            return Some(val.clone());
        }
        let mut up = self.up.clone();
        while let Some(curr_up) = up {
            if let Some(val) = curr_up.read().await.defs.get(var) {
                return Some(val.clone());
            }
            up = curr_up.read().await.up.clone();
        }
        None
    }

    pub fn define(&mut self, var: &str, val: Gc<Value>) {
        self.defs.insert(var.to_string(), val);
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    UndefinedVariable(String),
    InvalidOperator(Gc<Value>),
}

#[async_trait]
pub trait Eval {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError>;
}

#[derive(Debug)]
pub struct Procedure {
    up: Gc<Env>,
    args: Vec<String>,
    body: Arc<Body>,
}

impl Procedure {
    async fn call(&self, args: &[Gc<Value>]) -> Result<Gc<Value>, RuntimeError> {
        // TODO: Construct a new environment with the new arguments
        // TODO: Evaluate the function body
        todo!()
    }
}

#[derive(Debug)]
pub struct ExternalFn {
    pub num_args: usize,
    pub variadic: bool,
    pub func: fn(Gc<Env>, Vec<Gc<Value>>) -> BoxFuture<'static, Result<Gc<Value>, RuntimeError>>,
}

impl ExternalFn {
    async fn call(&self, env: &Gc<Env>, args: Vec<Gc<Value>>) -> Result<Gc<Value>, RuntimeError> {
        // TODO: check arguments
        (self.func)(env.clone(), args).await
    }
}

#[async_trait]
impl Eval for ast::Expression {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        match self {
            Self::Literal(ast::Literal::Number(n)) => Ok(Gc::new(Value::Number(n.clone()))),
            Self::VariableRef(ast::Ident(var)) => Ok(env
                .read()
                .await
                .fetch(&var)
                .await
                .ok_or_else(|| RuntimeError::UndefinedVariable(var.clone()))?),
            Self::Call(call) => call.eval(env).await,
            _ => todo!(),
        }
    }
}

#[async_trait]
impl Eval for ast::Call {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        // Collect the operator
        let op = self.operator.eval(env).await?;
        let read_op = op.read().await;
        if !read_op.is_callable() {
            return Err(RuntimeError::InvalidOperator(op.clone()));
        }
        // Collect the arguments
        let mut args = Vec::new();
        for arg in &self.args {
            args.push(arg.eval(env).await?);
        }
        // Call the operator with the arguments
        match &*read_op {
            Value::ExternalFn(extern_fn) => extern_fn.call(env, args).await,
            Value::Procedure(proc) => proc.call(args.as_slice()).await,
            _ => unreachable!(),
        }
    }
}
