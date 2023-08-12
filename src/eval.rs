use std::{sync::Arc, collections::HashMap, future::Future};
use async_trait::async_trait;
use futures::future::BoxFuture;
use crate::{ast::{self, Body}, gc::{Gc, Trace}, num::Number};

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

pub enum RuntimeError {
    InvalidOperator(Gc<Value>),
}

#[async_trait]
pub trait Eval {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError>;
}

struct Procedure {
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

pub struct ExternalFn {
    pub num_args: usize,
    pub variadic: bool,
    pub func: fn(&Gc<Env>, &[Gc<Value>]) -> BoxFuture<'static, Result<Gc<Value>, RuntimeError>>
}

impl ExternalFn {
    async fn call(&self, env: &Gc<Env>, args: &[Gc<Value>]) -> Result<Gc<Value>, RuntimeError> {
        // TODO: check arguments
        (self.func)(env, args).await
    }
}

#[async_trait]
impl Eval for ast::Expression {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        todo!()
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
            Value::ExternalFn(extern_fn) => extern_fn.call(env, args.as_slice()).await,
            Value::Procedure(proc) => proc.call(args.as_slice()).await,
            _ => unreachable!(),
        }
    }
}
