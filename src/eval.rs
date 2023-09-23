use crate::{
    ast::{self, Body},
    builtin::Builtin,
    compile::CompileError,
    env::Env,
    gc::{Gc, Trace},
    num::Number,
    syntax::{Identifier, Syntax},
};
use async_trait::async_trait;
use futures::future::{BoxFuture, Shared};
use std::{borrow::Cow, collections::HashMap, fmt, sync::Arc};

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(Number),
    Character(char),
    String(String),
    Symbol(String),
    Pair(Gc<Value>, Gc<Value>),
    Vector(Vec<Gc<Value>>),
    ByteVector(Vec<u8>),
    Syntax { syntax: Syntax, env: Env },
    Procedure(Procedure),
    ExternalFn(ExternalFn),
    Future(Shared<BoxFuture<'static, Value>>),
    Transformer(crate::expand::Transformer),
}

impl Value {
    pub fn is_callable(&self) -> bool {
        matches!(self, Self::Procedure(_) | Self::ExternalFn(_))
    }

    /// #f is false, everything else is true
    pub fn is_true(&self) -> bool {
        !matches!(self, Self::Boolean(x) if !x)
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
                Self::String(string) => format!("\"{string}\""),
                Self::Symbol(symbol) => symbol.clone(),
                Self::Pair(car, cdr) => crate::lists::fmt_list(car, cdr).await,
                Self::Vector(vec) => {
                    let mut iter = vec.iter().peekable();
                    let mut output = String::from("#(");
                    while let Some(item) = iter.next() {
                        output.push_str(&item.read().await.fmt().await);
                        if iter.peek().is_some() {
                            output.push(' ');
                        }
                    }
                    output.push(')');
                    output
                }
                Self::Nil => "()".to_string(),
                Self::Character(c) => format!("\\x{c}"),
                Self::ByteVector(_) => "<byte_vector>".to_string(),
                Self::Syntax { .. } => "<syntax>".to_string(),
                Self::Procedure(_) => "<lambda>".to_string(),
                Self::ExternalFn(_) => "<external_fn>".to_string(),
                Self::Future(_) => "<future>".to_string(),
                Self::Transformer(_) => "<transformer>".to_string(),
            }
        })
    }

    pub fn from_literal(literal: &ast::Literal) -> Self {
        match literal {
            ast::Literal::Number(n) => Value::Number(n.clone()),
            ast::Literal::Boolean(b) => Value::Boolean(*b),
            ast::Literal::String(s) => Value::String(s.clone()),
            _ => todo!("Literal evaluation not implemented"),
        }
    }

    pub fn from_syntax(syntax: &Syntax) -> Self {
        match syntax {
            Syntax::Nil { .. } => Self::Nil,
            Syntax::List { list, .. } => {
                let mut curr = Self::from_syntax(list.last().unwrap());
                for item in list[..list.len() - 1].iter().rev() {
                    curr = Self::Pair(Gc::new(Self::from_syntax(item)), Gc::new(curr));
                }
                curr
            }
            Syntax::Vector { vector, .. } => {
                Self::Vector(vector.iter().map(Self::from_syntax).map(Gc::new).collect())
            }
            Syntax::Literal { literal, .. } => Self::from_literal(literal),
            Syntax::Identifier { ident, .. } => Self::Symbol(ident.name.clone()),
        }
    }
}

impl Trace for Value {}

impl From<ExternalFn> for Value {
    fn from(extern_fn: ExternalFn) -> Self {
        Value::ExternalFn(extern_fn)
    }
}

pub enum RuntimeError {
    UndefinedVariable(Identifier),
    InvalidOperator(Gc<Value>),
    TooFewArguments,
    TooManyArguments,
    CompileError(CompileError),
}

impl From<CompileError> for RuntimeError {
    fn from(ce: CompileError) -> Self {
        Self::CompileError(ce)
    }
}

impl fmt::Debug for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UndefinedVariable(var) => write!(f, "UndefinedVariable({var:?})"),
            Self::InvalidOperator(_) => write!(f, "InvalidOperation(<Gc>)"),
            Self::TooFewArguments => write!(f, "TooFewArguments"),
            Self::TooManyArguments => write!(f, "TooManyArguments"),
            _ => todo!(),
        }
    }
}

pub enum ValueOrPreparedCall {
    Value(Gc<Value>),
    PreparedCall(PreparedCall),
}

impl ValueOrPreparedCall {
    async fn eval(self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        match self {
            Self::Value(val) => Ok(val),
            Self::PreparedCall(prepared_call) => prepared_call.eval(env).await,
        }
    }
}

/// Core evaulation trait for expressions.
///
/// Any struct implementing this trait must either implement `eval`, `tail_eval`, or
/// both, even though both methods are provided.
#[async_trait]
pub trait Eval: dyn_clone::DynClone + Send + Sync {
    async fn eval(&self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        self.tail_eval(env).await?.eval(env).await
    }

    /// Evaluate the expression in a tail environment
    async fn tail_eval(&self, env: &Env) -> Result<ValueOrPreparedCall, RuntimeError> {
        Ok(ValueOrPreparedCall::Value(self.eval(env).await?))
    }
}

dyn_clone::clone_trait_object!(Eval);

#[async_trait]
impl Eval for Gc<Value> {
    async fn eval(&self, _env: &Env) -> Result<Gc<Value>, RuntimeError> {
        Ok(self.clone())
    }
}

#[async_trait]
impl Eval for ast::Literal {
    async fn eval(&self, _env: &Env) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(Value::from_literal(self)))
    }
}

#[derive(Clone)]
pub struct Procedure {
    up: Env,
    args: Vec<Identifier>,
    remaining: Option<Identifier>,
    body: Body,
}

impl Procedure {
    fn min_args(&self) -> usize {
        self.args.len()
    }

    fn max_args(&self) -> Option<usize> {
        self.remaining.is_none().then_some(self.args.len())
    }

    async fn call(&self, mut args: Vec<Gc<Value>>) -> Result<Gc<Value>, RuntimeError> {
        let env = Gc::new(self.up.new_lexical_contour());
        let mut proc = Cow::Borrowed(self);
        loop {
            let mut args_iter = args.iter().peekable();
            {
                for required in &proc.args {
                    // We shouldn't ever need to check this, but probably safer to put
                    // this call here as well.
                    let Some(value) = args_iter.next().cloned() else {
                        return Err(RuntimeError::TooFewArguments);
                    };
                    env.write().await.def_var(required, value);
                }
            }
            if let Some(ref _remaining) = self.remaining {
                todo!()
            } else if args_iter.peek().is_some() {
                return Err(RuntimeError::TooManyArguments);
            }

            let ret = self.body.tail_eval(&Env::from(env.clone())).await?;
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
    is_external: bool,
    operator: Gc<Value>,
    args: Vec<Gc<Value>>,
}

impl PreparedCall {
    async fn eval(self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        let read_op = self.operator.read().await;
        // Call the operator with the arguments
        match &*read_op {
            Value::ExternalFn(extern_fn) => extern_fn.call(env, self.args).await,
            Value::Procedure(proc) => proc.call(self.args).await,
            _ => unreachable!(),
        }
    }

    async fn prepare(call: &ast::Call, env: &Env) -> Result<Self, RuntimeError> {
        // Collect the operator
        let operator = call.operator.eval(env).await?;
        let (is_external, args) = {
            let read_op = operator.read().await;
            if !read_op.is_callable() {
                return Err(RuntimeError::InvalidOperator(operator.clone()));
            }
            // Check the number of arguments provided
            let (is_external, min_args, max_args) = match &*read_op {
                Value::ExternalFn(extern_fn) => (true, extern_fn.min_args(), extern_fn.max_args()),
                Value::Procedure(proc) => (false, proc.min_args(), proc.max_args()),
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
            (is_external, args)
        };

        Ok(Self {
            is_external,
            operator,
            args,
        })
    }
}

#[async_trait]
impl Eval for ast::Body {
    async fn tail_eval(&self, env: &Env) -> Result<ValueOrPreparedCall, RuntimeError> {
        let Some((last, body)) = self.exprs.split_last() else {
            return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Nil)));
        };
        for expr in body {
            // Discard values that aren't returned
            expr.compile(env).await?.eval(env).await?;
        }
        // Return the last value
        last.compile(env).await?.tail_eval(env).await
    }
}

#[async_trait]
impl Eval for ast::Let {
    async fn tail_eval(&self, _env: &Env) -> Result<ValueOrPreparedCall, RuntimeError> {
        let up = self.scope.read().await.up.clone();
        for (ident, expr) in &self.bindings {
            let val = expr.eval(&up).await?;
            self.scope.write().await.def_var(ident, val);
        }
        self.body.tail_eval(&Env::from(self.scope.clone())).await
    }
}

#[async_trait]
impl Eval for ast::Call {
    async fn tail_eval(&self, env: &Env) -> Result<ValueOrPreparedCall, RuntimeError> {
        let prepared_call = PreparedCall::prepare(self, env).await?;
        if prepared_call.is_external {
            Ok(ValueOrPreparedCall::Value(prepared_call.eval(env).await?))
        } else {
            Ok(ValueOrPreparedCall::PreparedCall(prepared_call))
        }
    }
}

/*
// TODO: Get rid of this implementation and use one that has a location
#[async_trait]
impl Eval for ast::Ident {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        let result = env.read().await.fetch(self).await;
        // This should very rarely occur, but it is possible in certain circumstances
        // where macros refer to items in the global scope that have yet to be defined
        if result.is_none() && self.hygiene.is_some() {
            self.hygiene
                .as_ref()
                .unwrap()
                .read()
                .await
                .fetch(&Ident::new(&self.sym))
                .await
        } else {
            result
        }
        .ok_or_else(|| RuntimeError::UndefinedVariable(self.clone()))
    }
}

#[async_trait]
impl Eval for ast::Ref {
    async fn eval(&self, _env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        Ok(self.val.clone())
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
        for (ident, expr) in &self.bindings {
            new_scope.define(ident, expr.eval(env).await?);
        }
        self.body.tail_eval(&Gc::new(new_scope)).await
    }
}

#[async_trait]
impl Eval for ast::Set {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        // TODO: Add try_unwrap to GC to avoid the clone of the inner value
        *env.read()
            .await
            .fetch(&self.var)
            .await
            .ok_or_else(|| RuntimeError::UndefinedVariable(self.var.clone()))?
            .write()
            .await = self.val.eval(env).await?.read().await.clone();
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::If {
    async fn tail_eval(&self, env: &Gc<Env>) -> Result<ValueOrPreparedCall, RuntimeError> {
        if self.cond.eval(env).await?.read().await.is_true() {
            self.success.tail_eval(env).await
        } else if let Some(ref failure) = self.failure {
            failure.tail_eval(env).await
        } else {
            Ok(ValueOrPreparedCall::Value(Gc::new(Value::Nil)))
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
        env.write().await.define(&self.name, func);
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::DefineVar {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        let val = self.val.eval(env).await?;
        env.write().await.define(&self.name, val);
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::Define {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        match self {
            ast::Define::DefineFunc(define_func) => define_func.eval(env).await,
            ast::Define::DefineVar(define_var) => define_var.eval(env).await,
        }
    }
}

#[async_trait]
impl Eval for ast::DefineSyntax {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        let macro_env = Gc::new(Env::new(env));
        env.write().await.define(
            &self.name,
            Gc::new(Value::Transformer(crate::expand::Transformer {
                env: macro_env,
                rules: self.rules.clone(),
            })),
        );
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

#[async_trait]
impl Eval for ast::Literal {
    async fn eval(&self, _env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(Value::from_literal(self)))
    }
}

#[async_trait]
impl Eval for ast::Quote {
    async fn eval(&self, _env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(self.val.clone()))
    }
}

#[async_trait]
impl Eval for ast::Syntax {
    async fn eval(&self, _env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(Value::Syntax {
            syntax: self.syn.clone(),
            binds: self.binds.clone(),
        }))
    }
}

#[async_trait]
impl Eval for ast::Vector {
    async fn eval(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        let mut output = Vec::new();
        for item in &self.vals {
            output.push(item.eval(env).await?);
        }
        Ok(Gc::new(Value::Vector(output)))
    }
}

#[async_trait]
impl Eval for ast::Nil {
    async fn eval(&self, _env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(Value::Nil))
    }
}
*/
