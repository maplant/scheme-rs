use crate::{
    ast,
    env::Env,
    error::RuntimeError,
    gc::Gc,
    proc::{PreparedCall, Procedure},
    value::Value,
};
use async_trait::async_trait;

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

#[async_trait]
impl Eval for ast::Quote {
    async fn eval(&self, _env: &Env) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(self.val.clone()))
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

#[async_trait]
impl Eval for ast::If {
    async fn tail_eval(&self, env: &Env) -> Result<ValueOrPreparedCall, RuntimeError> {
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
    async fn eval(&self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        let (args, remaining) = self.args.to_args_and_remaining();
        let func = Gc::new(Value::Procedure(Procedure {
            up: env.clone(),
            args,
            remaining,
            mark: self.mark,
            body: self.body.clone(),
            is_variable_transformer: false,
        }));
        env.def_var(&self.name, func).await;
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::DefineVar {
    async fn eval(&self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        let val = self.val.eval(env).await?;
        env.def_var(&self.name, val).await;
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::Define {
    async fn eval(&self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        match self {
            ast::Define::DefineFunc(define_func) => define_func.eval(env).await,
            ast::Define::DefineVar(define_var) => define_var.eval(env).await,
        }
    }
}

#[async_trait]
impl Eval for ast::DefineSyntax {
    async fn eval(&self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        let val = self.transformer.eval(env).await?;
        env.def_macro(&self.name, val).await;
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::And {
    async fn tail_eval(&self, env: &Env) -> Result<ValueOrPreparedCall, RuntimeError> {
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
    async fn tail_eval(&self, env: &Env) -> Result<ValueOrPreparedCall, RuntimeError> {
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
impl Eval for ast::Vector {
    async fn eval(&self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        let mut output = Vec::new();
        for item in &self.vals {
            output.push(item.eval(env).await?);
        }
        Ok(Gc::new(Value::Vector(output)))
    }
}

#[async_trait]
impl Eval for ast::Nil {
    async fn eval(&self, _env: &Env) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::Set {
    async fn eval(&self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        // TODO: Add try_unwrap to GC to avoid the clone of the inner value
        *env.fetch_var(&self.var)
            .await
            .ok_or_else(|| RuntimeError::undefined_variable(self.var.clone()))?
            .write()
            .await = self.val.eval(env).await?.read().await.clone();
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::Lambda {
    async fn eval(&self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        // TODO: Optimize the AST with smart pointers to prevent constantly
        // cloning.
        let (args, remaining) = self.args.to_args_and_remaining();
        Ok(Gc::new(Value::Procedure(Procedure {
            up: env.clone(),
            args,
            remaining,
            mark: self.mark,
            body: self.body.clone(),
            is_variable_transformer: false,
        })))
    }
}

#[async_trait]
impl Eval for ast::SyntaxQuote {
    async fn eval(&self, _env: &Env) -> Result<Gc<Value>, RuntimeError> {
        let mut syntax = self.syn.clone();
        syntax.strip_unused_marks(&self.env).await;
        Ok(Gc::new(Value::Syntax(syntax)))
    }
}

#[async_trait]
impl Eval for ast::SyntaxCase {
    async fn eval(&self, env: &Env) -> Result<Gc<Value>, RuntimeError> {
        let val = self.arg.eval(env).await?;
        let val = val.read().await;
        match &*val {
            Value::Syntax(syntax) => {
                let result = self.transformer.expand(syntax).unwrap();
                result.compile(env).await?.eval(env).await
            }
            _ => todo!(),
        }
    }
}

#[async_trait]
impl Eval for ast::SyntaxRules {
    async fn eval(&self, _env: &Env) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(Value::Transformer(self.transformer.clone())))
    }
}
