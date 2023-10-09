use crate::{
    ast,
    continuation::{
        Continuation, ResumableAnd, ResumableBody, ResumableCall, ResumableDefineSyntax,
        ResumableDefineVar, ResumableIf, ResumableLet, ResumableOr, ResumableSet,
        ResumableSyntaxCase,
    },
    env::Env,
    error::RuntimeError,
    gc::Gc,
    proc::{PreparedCall, Procedure},
    util,
    value::Value,
};
use async_trait::async_trait;
use std::sync::Arc;

pub enum ValueOrPreparedCall {
    Value(Gc<Value>),
    PreparedCall(PreparedCall),
}

impl ValueOrPreparedCall {
    pub async fn eval(self, cont: &Option<Arc<Continuation>>) -> Result<Gc<Value>, RuntimeError> {
        match self {
            Self::Value(val) => Ok(val),
            Self::PreparedCall(prepared_call) => prepared_call.eval(cont).await,
        }
    }
}

/// Core evaulation trait for expressions.
///
/// Any struct implementing this trait must either implement `eval`, `tail_eval`, or
/// both, even though both methods are provided.
#[async_trait]
pub trait Eval: Send + Sync {
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        self.tail_eval(env, cont).await?.eval(cont).await
    }

    /// Evaluate the expression in a tail environment
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        Ok(ValueOrPreparedCall::Value(self.eval(env, cont).await?))
    }
}

#[async_trait]
impl Eval for Gc<Value> {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        Ok(self.clone())
    }
}

#[async_trait]
impl Eval for ast::Literal {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(Value::from_literal(self)))
    }
}

#[async_trait]
impl Eval for ast::Quote {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(self.val.clone()))
    }
}

#[async_trait]
impl Eval for ast::Body {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        let Some(last) = self.exprs.last() else {
            return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Nil)));
        };
        for (expr, tail) in self.exprs.skip_last() {
            let cont = Some(Arc::new(Continuation::new(
                Arc::new(ResumableBody::new(env, &tail)),
                cont,
            )));
            // println!("new_cont = {:#?}", cont);
            // Discard values that aren't returned
            expr.compile(env, &cont).await?.eval(env, &cont).await?;
        }
        last.compile(env, cont).await?.tail_eval(env, cont).await
    }
}

#[async_trait]
impl Eval for ast::Let {
    async fn tail_eval(
        &self,
        _env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        let up = self.scope.read().await.up.clone();
        for ((ident, expr), remaining) in util::iter_arc(&self.bindings) {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableLet::new(&self.scope, ident, remaining, &self.body)),
                cont,
            ));
            let val = expr.eval(&up, &Some(cont)).await?;
            self.scope.write().await.def_var(ident, val);
        }
        self.body
            .tail_eval(&Env::from(self.scope.clone()), cont)
            .await
    }
}

#[async_trait]
impl Eval for ast::Call {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        let mut collected = Vec::new();
        for (arg, remaining) in self.args.iter() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableCall::new(env, &collected, remaining)),
                cont,
            ));
            let arg = arg.eval(env, &Some(cont)).await?;
            collected.push(arg);
        }
        Ok(ValueOrPreparedCall::PreparedCall(PreparedCall::prepare(
            collected,
        )))
    }
}

#[async_trait]
impl Eval for ast::If {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        let cond_cont = Arc::new(Continuation::new(
            Arc::new(ResumableIf::new(env, &self.success, &self.failure)),
            cont,
        ));
        let condition = self
            .cond
            .eval(env, &Some(cond_cont))
            .await?
            .read()
            .await
            .is_true();
        if condition {
            self.success.tail_eval(env, cont).await
        } else if let Some(ref failure) = self.failure {
            failure.tail_eval(env, cont).await
        } else {
            Ok(ValueOrPreparedCall::Value(Gc::new(Value::Nil)))
        }
    }
}

#[async_trait]
impl Eval for ast::DefineFunc {
    async fn eval(
        &self,
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
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
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        let cont = Arc::new(Continuation::new(
            Arc::new(ResumableDefineVar::new(env, &self.name)),
            cont,
        ));
        let val = self.val.eval(env, &Some(cont)).await?;
        env.def_var(&self.name, val).await;
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::Define {
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        match self {
            ast::Define::DefineFunc(define_func) => define_func.eval(env, cont).await,
            ast::Define::DefineVar(define_var) => define_var.eval(env, cont).await,
        }
    }
}

#[async_trait]
impl Eval for ast::DefineSyntax {
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        let cont = Arc::new(Continuation::new(
            Arc::new(ResumableDefineSyntax::new(env, &self.name)),
            cont,
        ));
        let val = self.transformer.eval(env, &Some(cont)).await?;
        env.def_macro(&self.name, val).await;
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::And {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        let Some(last) = self.args.last() else {
            return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(true))));
        };
        for (arg, tail) in self.args.skip_last() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableAnd::new(env, &tail)),
                cont,
            ));
            // If one of the arguments does not evaluate to true, then the result
            // is false
            if !arg.eval(env, &Some(cont)).await?.read().await.is_true() {
                return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(false))));
            }
        }
        // If all of the other arguments are true, then the result is the last expression
        last.tail_eval(env, cont).await
    }
}

#[async_trait]
impl Eval for ast::Or {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        let Some(last) = self.args.last() else {
            return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(false))));
        };
        for (arg, tail) in self.args.skip_last() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableOr::new(env, &tail)),
                cont,
            ));
            // If one of the arguments evaluates to true, then the result is true
            if arg.eval(env, &Some(cont)).await?.read().await.is_true() {
                return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(true))));
            }
        }
        // If all of the other arguments are false, then the result is the last expression
        last.tail_eval(env, cont).await
    }
}

#[async_trait]
impl Eval for ast::Vector {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        /*
        let mut output = Vec::new();
        for item in &self.vals {
            output.push(item.eval(env, cont.clone()).await?);
        }
        Ok(Gc::new(Value::Vector(output)))
         */
        todo!("FIXME: Vectors don't evaluate their arguments, take the literals")
    }
}

#[async_trait]
impl Eval for ast::Nil {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::Set {
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        let new_cont = Arc::new(Continuation::new(
            Arc::new(ResumableSet::new(env, &self.var)),
            cont,
        ));
        // TODO: Add try_unwrap to GC to avoid the clone of the inner value
        let val = self
            .val
            .eval(env, &Some(new_cont))
            .await?
            .read()
            .await
            .clone();
        *env.fetch_var(&self.var)
            .await
            .ok_or_else(|| RuntimeError::undefined_variable(self.var.clone()))?
            .write()
            .await = val;
        Ok(Gc::new(Value::Nil))
    }
}

#[async_trait]
impl Eval for ast::Lambda {
    async fn eval(
        &self,
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
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
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        let mut syntax = self.syn.clone();
        syntax.strip_unused_marks(&self.env).await;
        Ok(Gc::new(Value::Syntax(syntax)))
    }
}

#[async_trait]
impl Eval for ast::SyntaxCase {
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        let new_cont = Arc::new(Continuation::new(
            Arc::new(ResumableSyntaxCase::new(env, &self.transformer)),
            cont,
        ));
        let val = self.arg.eval(env, &Some(new_cont)).await?;
        let val = val.read().await;
        match &*val {
            Value::Syntax(syntax) => {
                let result = self.transformer.expand(syntax).unwrap();
                result.compile(env, cont).await?.eval(env, cont).await
            }
            _ => todo!(),
        }
    }
}

#[async_trait]
impl Eval for ast::SyntaxRules {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        Ok(Gc::new(Value::Transformer(self.transformer.clone())))
    }
}
