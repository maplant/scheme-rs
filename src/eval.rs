use crate::{
    ast,
    continuation::{
        Continuation, ResumableAnd, ResumableApply, ResumableBody, ResumableCall,
        ResumableDefineVar, ResumableIf, ResumableLet, ResumableOr, ResumableSet,
        ResumableSyntaxCase,
    },
    env::Env,
    error::RuntimeError,
    gc::Gc,
    lists::list_to_vec,
    proc::{PreparedCall, Procedure},
    util::{self, ArcSlice, RequireOne},
    value::Value,
};
use async_trait::async_trait;
use std::sync::Arc;

pub enum ValuesOrPreparedCall {
    Values(Vec<Gc<Value>>),
    PreparedCall(PreparedCall),
}

impl ValuesOrPreparedCall {
    pub async fn eval(
        self,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        match self {
            Self::Values(val) => Ok(val),
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
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        self.tail_eval(env, cont).await?.eval(cont).await
    }

    /// Evaluate the expression in a tail environment
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        Ok(ValuesOrPreparedCall::Values(self.eval(env, cont).await?))
    }
}

#[async_trait]
impl Eval for Gc<Value> {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        Ok(vec![self.clone()])
    }
}

#[async_trait]
impl Eval for Vec<Gc<Value>> {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        Ok(self.clone())
    }
}

#[async_trait]
impl Eval for ast::Literal {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        Ok(vec![Gc::new(Value::from_literal(self))])
    }
}

#[async_trait]
impl Eval for ast::Quote {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        Ok(vec![Gc::new(self.val.clone())])
    }
}

#[async_trait]
impl Eval for ast::Body {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let Some(last) = self.exprs.last() else {
            return Ok(ValuesOrPreparedCall::Values(vec![Gc::new(Value::Null)]));
        };
        for (expr, tail) in self.exprs.skip_last() {
            let cont = Some(Arc::new(Continuation::new(
                Arc::new(ResumableBody::new(env, &tail)),
                cont,
            )));
            // Discard values that aren't returned
            expr.eval(env, &cont).await?;
        }
        last.tail_eval(env, cont).await
    }
}

#[async_trait]
impl Eval for ast::Let {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let scope = Gc::new(env.new_lexical_contour());
        for ((ident, expr), remaining) in util::iter_arc(&self.bindings) {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableLet::new(&scope, ident, remaining, &self.body)),
                cont,
            ));
            let val = expr
                .eval(&Env::from(scope.clone()), &Some(cont))
                .await?
                .require_one()?;
            scope.write().await.def_var(ident, val);
        }
        self.body.tail_eval(&Env::from(scope), cont).await
    }
}

#[async_trait]
impl Eval for ast::Call {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let mut collected = Vec::new();
        for (arg, remaining) in self.args.iter() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableCall::new(
                    &self.proc_name,
                    &self.location,
                    env,
                    &collected,
                    remaining,
                )),
                cont,
            ));
            let arg = arg.eval(env, &Some(cont)).await?.require_one()?;
            collected.push(arg);
        }
        Ok(ValuesOrPreparedCall::PreparedCall(PreparedCall::prepare(
            &self.proc_name,
            &self.location,
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
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let cond_cont = Arc::new(Continuation::new(
            Arc::new(ResumableIf::new(env, &self.success, &self.failure)),
            cont,
        ));
        let condition = self
            .cond
            .eval(env, &Some(cond_cont))
            .await?
            .require_one()?
            .read()
            .await
            .is_true();
        if condition {
            self.success.tail_eval(env, cont).await
        } else if let Some(ref failure) = self.failure {
            failure.tail_eval(env, cont).await
        } else {
            Ok(ValuesOrPreparedCall::Values(vec![Gc::new(Value::Null)]))
        }
    }
}

#[async_trait]
impl Eval for ast::DefineFunc {
    async fn eval(
        &self,
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let (args, remaining) = self.args.to_args_and_remaining();
        let func = Gc::new(Value::Procedure(Procedure {
            up: env.clone(),
            args,
            remaining,
            // mark: self.mark,
            body: self.body.clone(),
            is_variable_transformer: false,
        }));
        env.def_var(&self.name, func).await;
        Ok(vec![Gc::new(Value::Null)])
    }
}

#[async_trait]
impl Eval for ast::DefineVar {
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let cont = Arc::new(Continuation::new(
            Arc::new(ResumableDefineVar::new(env, &self.name)),
            cont,
        ));
        let val = self.val.eval(env, &Some(cont)).await?.require_one()?;
        env.def_var(&self.name, val).await;
        Ok(vec![Gc::new(Value::Null)])
    }
}

#[async_trait]
impl Eval for ast::Define {
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
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
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        Ok(vec![Gc::new(Value::Null)])
    }
}

#[async_trait]
impl Eval for ast::And {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let Some(last) = self.args.last() else {
            return Ok(ValuesOrPreparedCall::Values(vec![Gc::new(Value::Boolean(
                true,
            ))]));
        };
        for (arg, tail) in self.args.skip_last() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableAnd::new(env, &tail)),
                cont,
            ));
            // If one of the arguments does not evaluate to true, then the result
            // is false
            if !arg
                .eval(env, &Some(cont))
                .await?
                .require_one()?
                .read()
                .await
                .is_true()
            {
                return Ok(ValuesOrPreparedCall::Values(vec![Gc::new(Value::Boolean(
                    false,
                ))]));
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
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let Some(last) = self.args.last() else {
            return Ok(ValuesOrPreparedCall::Values(vec![Gc::new(Value::Boolean(
                false,
            ))]));
        };
        for (arg, tail) in self.args.skip_last() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableOr::new(env, &tail)),
                cont,
            ));
            // If one of the arguments evaluates to true, then the result is true
            if arg
                .eval(env, &Some(cont))
                .await?
                .require_one()?
                .read()
                .await
                .is_true()
            {
                return Ok(ValuesOrPreparedCall::Values(vec![Gc::new(Value::Boolean(
                    true,
                ))]));
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
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
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
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        Ok(vec![Gc::new(Value::Null)])
    }
}

#[async_trait]
impl Eval for ast::Set {
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let new_cont = Arc::new(Continuation::new(
            Arc::new(ResumableSet::new(env, &self.var)),
            cont,
        ));
        // TODO: Add try_unwrap to GC to avoid the clone of the inner value
        let val = self
            .val
            .eval(env, &Some(new_cont))
            .await?
            .require_one()?
            .read()
            .await
            .clone();
        *env.fetch_var(&self.var)
            .await
            .ok_or_else(|| RuntimeError::undefined_variable(self.var.clone()))?
            .write()
            .await = val;
        Ok(vec![Gc::new(Value::Null)])
    }
}

#[async_trait]
impl Eval for ast::Lambda {
    async fn eval(
        &self,
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        // TODO: Optimize the AST with smart pointers to prevent constantly
        // cloning.
        let (args, remaining) = self.args.to_args_and_remaining();
        Ok(vec![Gc::new(Value::Procedure(Procedure {
            up: env.clone(),
            args,
            remaining,
            // mark: self.mark,
            body: self.body.clone(),
            is_variable_transformer: false,
        }))])
    }
}

#[async_trait]
impl Eval for ast::SyntaxQuote {
    async fn eval(
        &self,
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        Ok(vec![Gc::new(Value::Syntax(self.syn.clone()))])
    }
}

#[async_trait]
impl Eval for ast::SyntaxCase {
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let new_cont = Arc::new(Continuation::new(
            Arc::new(ResumableSyntaxCase::new(env, &self.transformer)),
            cont,
        ));
        let val = self.arg.eval(env, &Some(new_cont)).await?.require_one()?;
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
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        Ok(vec![Gc::new(Value::Transformer(self.transformer.clone()))])
    }
}

#[async_trait]
impl Eval for ast::Apply {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let mut collected = Vec::new();

        for (arg, remaining) in self.args.iter() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableApply::new(
                    &self.proc_name,
                    &self.location,
                    env,
                    &collected,
                    remaining,
                    Some(self.rest_args.clone()),
                )),
                cont,
            ));
            let arg = arg.eval(env, &Some(cont)).await?.require_one()?;
            collected.push(arg);
        }

        let cont = Arc::new(Continuation::new(
            Arc::new(ResumableApply::new(
                &self.proc_name,
                &self.location,
                env,
                &collected,
                ArcSlice::empty(),
                None,
            )),
            cont,
        ));
        let rest_args = self.rest_args.eval(env, &Some(cont)).await?.require_one()?;
        // TODO: Throw an error if rest_args is not a list
        list_to_vec(&rest_args, &mut collected).await;

        Ok(ValuesOrPreparedCall::PreparedCall(PreparedCall::prepare(
            &self.proc_name,
            &self.location,
            collected,
        )))
    }
}

#[async_trait]
impl Eval for ast::FetchVar {
    async fn eval(
        &self,
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        Ok(vec![env.fetch_var(&self.ident).await.ok_or_else(|| {
            RuntimeError::undefined_variable(self.ident.clone())
        })?])
    }
}

#[async_trait]
impl Eval for ast::MacroExpansionPoint {
    async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let env = Gc::new(env.new_expansion_context(self.mark, self.macro_env.clone()));
        self.expr.tail_eval(&Env::from(env), cont).await
    }
}
