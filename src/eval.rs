use crate::{
    ast::{self, Body},
    builtin::Builtin,
    gc::{Gc, Trace},
    num::Number,
};
use async_trait::async_trait;
use futures::future::BoxFuture;
use std::{borrow::Cow, collections::HashMap, fmt, future::Future, sync::Arc};

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

    /// #f is false, everything else is true
    pub fn is_true(&self) -> bool {
        match self {
            Self::Boolean(x) if !x => false,
            _ => true,
        }
    }

    pub fn as_proc(&self) -> Option<&Procedure> {
        match self {
            Self::Procedure(ref proc) => Some(proc),
            _ => None,
        }
    }

    pub fn fmt(&self) -> BoxFuture<'_, String> {
        Box::pin(async move {
            match self {
                Self::Boolean(true) => "#t".to_string(),
                Self::Boolean(false) => "#f".to_string(),
                Self::Number(number) => number.to_string(),
                Self::String(string) => string.clone(),
                Self::Pair(car, cdr) => crate::lists::fmt_list(car, cdr).await,
                Self::Nil => "()".to_string(),
                _ => todo!(),
            }
        })
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
    pub fn new(up: &Gc<Env>) -> Self {
        Self {
            up: Some(up.clone()),
            defs: HashMap::new(),
        }
    }

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

pub enum RuntimeError {
    UndefinedVariable(String),
    InvalidOperator(Gc<Value>),
    TooFewArguments,
    TooManyArguments,
}

impl fmt::Debug for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UndefinedVariable(var) => write!(f, "UndefinedVariable({var})"),
            Self::InvalidOperator(_) => write!(f, "InvalidOperation(<Gc>)"),
            Self::TooFewArguments => write!(f, "TooFewArguments"),
            Self::TooManyArguments => write!(f, "TooManyArguments"),
        }
    }
}

pub enum ValueOrPreparedCall {
    Value(Gc<Value>),
    PreparedCall(PreparedCall),
}

impl ValueOrPreparedCall {
    async fn eval(self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        match self {
            Self::Value(val) => Ok(val),
            Self::PreparedCall(prepared_call) => prepared_call.eval(env).await,
        }
    }
}

/// Core evaulation trait for expressions
///
/// Any struct implementing this trait must either implement `eval`, `tail_eval`, or
/// both, even though both methods are provided.
#[async_trait]
pub trait Eval {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        self.tail_eval(env).await?.eval(env).await
    }

    /// Evaluate the expression in a tail environment
    async fn tail_eval(&self, env: &Gc<Env>) -> Result<ValueOrPreparedCall, RuntimeError> {
        Ok(ValueOrPreparedCall::Value(self.eval(env).await?))
    }
}

#[derive(Clone)]
pub struct Procedure {
    up: Gc<Env>,
    args: Vec<ast::Ident>,
    remaining: Option<ast::Ident>,
    body: Body,
}

impl Procedure {
    fn min_args(&self) -> usize {
        self.args.len()
    }

    fn max_args(&self) -> Option<usize> {
        self.remaining.is_none().then(|| self.args.len())
    }

    async fn call(&self, mut args: Vec<Gc<Value>>) -> Result<Gc<Value>, RuntimeError> {
        let env = Gc::new(Env::new(&self.up));
        let mut proc = Cow::Borrowed(self);
        loop {
            let mut args_iter = args.iter().peekable();
            {
                let mut env = env.write().await;
                for ast::Ident(ref required) in &proc.args {
                    // We shouldn't ever need to check this, but probably safer to put
                    // this call here as well.
                    let Some(value) = args_iter.next().cloned() else {
                return Err(RuntimeError::TooFewArguments);
            };
                    env.define(required, value);
                }
            }
            if let Some(ref remaining) = self.remaining {
                todo!()
            } else if args_iter.peek().is_some() {
                return Err(RuntimeError::TooManyArguments);
            }

            let ret = self.body.tail_eval(&env).await?;
            match ret {
                ValueOrPreparedCall::Value(value) => return Ok(value),
                ValueOrPreparedCall::PreparedCall(prepared) => {
                    proc = Cow::Owned(prepared.operator.read().await.as_proc().unwrap().clone());
                    args = prepared.args;
                    println!("We are tail calling!");
                    // Continue
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct ExternalFn {
    pub num_args: usize,
    pub variadic: bool,
    pub func: fn(Gc<Env>, Vec<Gc<Value>>) -> BoxFuture<'static, Result<Gc<Value>, RuntimeError>>,
}

impl ExternalFn {
    fn min_args(&self) -> usize {
        self.num_args
    }

    fn max_args(&self) -> Option<usize> {
        (!self.variadic).then(|| self.num_args)
    }

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
            Self::DefFunc(def_func) => def_func.eval(env).await,
            Self::DefVar(def_var) => def_var.eval(env).await,
            Self::And(and) => and.eval(env).await,
            Self::Or(or) => or.eval(env).await,
            Self::Call(call) => call.eval(env).await,
            Self::Lambda(lambda) => lambda.eval(env).await,
            Self::Let(let_expr) => let_expr.eval(env).await,
            Self::If(if_expr) => if_expr.eval(env).await,
            _ => todo!(),
        }
    }

    async fn tail_eval(&self, env: &Gc<Env>) -> Result<ValueOrPreparedCall, RuntimeError> {
        match self {
            Self::If(if_expr) => if_expr.tail_eval(env).await,
            Self::Body(body) => body.tail_eval(env).await,
            Self::Call(call) => call.tail_eval(env).await,
            Self::And(and) => and.tail_eval(env).await,
            Self::Or(or) => or.tail_eval(env).await,
            Self::Let(let_expr) => let_expr.tail_eval(env).await,
            _ => Ok(ValueOrPreparedCall::Value(self.eval(env).await?)),
        }
    }
}

#[async_trait]
impl Eval for ast::Body {
    async fn tail_eval(&self, env: &Gc<Env>) -> Result<ValueOrPreparedCall, RuntimeError> {
        let Some((last, body)) = self.exprs.split_last() else {
            return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Nil)));
        };
        for expr in body {
            // Discard values that aren't returned
            expr.eval(env).await?;
        }
        // Return the last value
        last.tail_eval(env).await
    }
}

pub struct PreparedCall {
    operator: Gc<Value>,
    args: Vec<Gc<Value>>,
}

impl PreparedCall {
    async fn eval(self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        let read_op = self.operator.read().await;
        // Call the operator with the arguments
        match &*read_op {
            Value::ExternalFn(extern_fn) => extern_fn.call(env, self.args).await,
            Value::Procedure(proc) => proc.call(self.args).await,
            _ => unreachable!(),
        }
    }

    async fn prepare(call: &ast::Call, env: &Gc<Env>) -> Result<Self, RuntimeError> {
        // Collect the operator
        let operator = call.operator.eval(env).await?;
        let args = {
            let read_op = operator.read().await;
            if !read_op.is_callable() {
                return Err(RuntimeError::InvalidOperator(operator.clone()));
            }
            // Check the number of arguments provided
            let (min_args, max_args) = match &*read_op {
                Value::ExternalFn(extern_fn) => (extern_fn.min_args(), extern_fn.max_args()),
                Value::Procedure(proc) => (proc.min_args(), proc.max_args()),
                _ => unreachable!(),
            };
            if call.args.len() < min_args {
                return Err(RuntimeError::TooFewArguments);
            }
            if let Some(max_args) = max_args {
                if call.args.len() > max_args {
                    return Err(RuntimeError::TooManyArguments);
                }
            }
            // Collect the arguments
            let mut args = Vec::new();
            for arg in &call.args {
                args.push(arg.eval(env).await?);
            }
            args
        };

        Ok(Self { operator, args })
    }
}

#[async_trait]
impl Eval for ast::Call {
    async fn tail_eval(&self, env: &Gc<Env>) -> Result<ValueOrPreparedCall, RuntimeError> {
        // TODO: if external fn, call it and return the value
        Ok(ValueOrPreparedCall::PreparedCall(
            PreparedCall::prepare(self, env).await?,
        ))
    }
}

#[async_trait]
impl Eval for ast::Lambda {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        // TODO: Optimize the AST with smart pointers to prevent constantly
        // cloning.
        let (args, remaining) = self.args.to_args_and_remaining();
        Ok(Gc::new(Value::Procedure(Procedure {
            up: env.clone(),
            args,
            remaining,
            body: self.body.clone(),
        })))
    }
}

#[async_trait]
impl Eval for ast::Let {
    async fn tail_eval(&self, env: &Gc<Env>) -> Result<ValueOrPreparedCall, RuntimeError> {
        let mut new_scope = Env::new(env);
        for (ast::Ident(ref var), expr) in &self.bindings {
            new_scope.define(var, expr.eval(env).await?);
        }
        self.body.tail_eval(&Gc::new(new_scope)).await
    }
}

#[async_trait]
impl Eval for ast::If {
    async fn tail_eval(&self, env: &Gc<Env>) -> Result<ValueOrPreparedCall, RuntimeError> {
        if self.cond.eval(env).await?.read().await.is_true() {
            self.success.tail_eval(env).await
        } else {
            self.failure.tail_eval(env).await
        }
    }
}

#[async_trait]
impl Eval for ast::DefineFunc {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        let (args, remaining) = self.args.to_args_and_remaining();
        let func = Gc::new(Value::Procedure(Procedure {
            up: env.clone(),
            args,
            remaining,
            body: self.body.clone(),
        }));
        env.write().await.define(&self.name.0, func);
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::DefineVar {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        let val = self.val.eval(env).await?;
        env.write().await.define(&self.name.0, val);
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::And {
    async fn tail_eval(&self, env: &Gc<Env>) -> Result<ValueOrPreparedCall, RuntimeError> {
        let Some((last, args)) = self.args.split_last() else {
            return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(true))));
        };
        for arg in args {
            // If one of the arguments does not evaluate to true, then the result
            // is false
            if !arg.eval(env).await?.read().await.is_true() {
                return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(false))));
            }
        }
        // If all of the other arguments are true, then the result is the last expression
        last.tail_eval(env).await
    }
}

#[async_trait]
impl Eval for ast::Or {
    async fn tail_eval(&self, env: &Gc<Env>) -> Result<ValueOrPreparedCall, RuntimeError> {
        let Some((last, args)) = self.args.split_last() else {
            return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(false))));
        };
        for arg in args {
            // If one of the arguments evaluates to true, then the result is true
            if arg.eval(env).await?.read().await.is_true() {
                return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(true))));
            }
        }
        // If all of the other arguments are false, then the result is the last expression
        last.tail_eval(env).await
    }
}
