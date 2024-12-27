//! todo

use std::collections::BTreeSet;

use futures::future::BoxFuture;

use super::*;
use crate::{
    continuation::*,
    proc::{PreparedCall, ProcCallDebugInfo, Procedure},
    util::{self, RequireOne},
};

impl Definition {
    pub async fn eval(
        &self,
        env: &Gc<Env>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<(), RuntimeError> {
        match self {
            Self::DefineVar(var) => var.eval(env, cont).await,
            Self::DefineFunc(func_def) => func_def.eval(env).await,
        }
    }
}

impl DefineVar {
    pub(super) async fn eval(
        &self,
        env: &Gc<Env>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<(), RuntimeError> {
        let cont = Arc::new(Continuation::new(
            Arc::new(ResumableDefineVar::new(env, &self.name)),
            cont,
        ));
        let val = self.val.eval(env, &Some(cont)).await?.require_one()?;
        env.write().def_local_var(&self.name, val);
        Ok(())
    }
}

impl DefineFunc {
    pub(super) async fn eval(&self, env: &Gc<Env>) -> Result<(), RuntimeError> {
        let (args, remaining) = self.args.to_args_and_remaining();
        let func = Gc::new(Value::Procedure(Procedure {
            up: env.clone(),
            args,
            remaining,
            body: self.body.clone(),
            is_variable_transformer: false,
        }));
        env.write().def_local_var(&self.name, func);
        Ok(())
    }
}

impl Body {
    pub(crate) async fn eval(
        &self,
        env: &Gc<Env>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        self.tail_eval(env, cont).await?.eval(cont).await
    }

    pub(crate) async fn tail_eval(
        &self,
        env: &Gc<Env>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let Some(last) = self.forms.last() else {
            return Ok(ValuesOrPreparedCall::Values(Vec::new()));
        };
        for (form, tail) in self.forms.skip_last() {
            let cont = Some(Arc::new(Continuation::new(
                Arc::new(ResumableBody::new(env, &tail)),
                cont,
            )));
            // Discard values that aren't returned
            form.eval(env, &cont).await?;
        }
        last.tail_eval(env, cont).await
    }
}

impl Expression {
    pub fn tail_eval<'a>(
        &'a self,
        env: &'a Gc<Env>,
        cont: &'a Option<Arc<Continuation>>,
    ) -> BoxFuture<'a, Result<ValuesOrPreparedCall, RuntimeError>> {
        //println!("expr: {self:#?}");
        Box::pin(async move {
            match self {
                Self::Undefined => val(Value::Undefined),
                Self::Literal(literal) => val(Value::from_literal(literal)),
                Self::Quote(quote) => val(quote.val.clone()),
                Self::SyntaxQuote(syn_quote) => val(Value::Syntax(syn_quote.syn.clone())),
                Self::SyntaxCase(syn_case) => vals(syn_case.eval(env, cont).await?),
                Self::Call(call) => call.tail_eval(env, cont).await,
                Self::Let(bind) => bind.tail_eval(env, cont).await,
                Self::If(cond) => cond.tail_eval(env, cont).await,
                Self::And(and) => and.tail_eval(env, cont).await,
                Self::Or(or) => or.tail_eval(env, cont).await,
                Self::Lambda(lambda) => val(lambda.eval(env)),
                Self::Set(set) => {
                    set.eval(env, cont).await?;
                    no_vals()
                }
                Self::Vector(vec) => val(vec.eval()),
                Self::Begin(body) => body.tail_eval(env, cont).await,
                Self::Var(var) => Ok(ValuesOrPreparedCall::Values(vec![var
                    .fetch(env)
                    .map_err(RuntimeError::undefined_variable)?])),
            }
        })
    }

    pub async fn eval(
        &self,
        env: &Gc<Env>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        self.tail_eval(env, cont).await?.eval(cont).await
    }
}

fn no_vals() -> Result<ValuesOrPreparedCall, RuntimeError> {
    Ok(ValuesOrPreparedCall::Values(Vec::new()))
}

fn val(value: Value) -> Result<ValuesOrPreparedCall, RuntimeError> {
    Ok(ValuesOrPreparedCall::Values(vec![Gc::new(value)]))
}

fn vals(values: Vec<Gc<Value>>) -> Result<ValuesOrPreparedCall, RuntimeError> {
    Ok(ValuesOrPreparedCall::Values(values))
}

impl Call {
    async fn tail_eval(
        &self,
        env: &Gc<Env>,
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
        // println!("prepared: {:#?}", env);
        Ok(ValuesOrPreparedCall::PreparedCall(PreparedCall::prepare(
            collected,
            Some(ProcCallDebugInfo::new(&self.proc_name, &self.location)),
        )))
    }
}

impl Lambda {
    fn eval(&self, env: &Gc<Env>) -> Value {
        let (args, remaining) = self.args.to_args_and_remaining();
        Value::Procedure(Procedure {
            up: env.clone(),
            args,
            remaining,
            body: self.body.clone(),
            is_variable_transformer: false,
        })
    }
}

impl Let {
    async fn tail_eval(
        &self,
        env: &Gc<Env>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let scope = Gc::new(env.new_lexical_contour());
        for ((ident, expr), remaining) in util::iter_arc(&self.bindings) {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableLet::new(&scope, ident, remaining, &self.body)),
                cont,
            ));
            let val = expr.eval(env, &Some(cont)).await?.require_one()?;
            scope.write().def_local_var(ident, val);
        }
        self.body.tail_eval(&scope, cont).await
    }
}

impl If {
    async fn tail_eval(
        &self,
        env: &Gc<Env>,
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
            .is_true();
        if condition {
            self.success.tail_eval(env, cont).await
        } else if let Some(ref failure) = self.failure {
            failure.tail_eval(env, cont).await
        } else {
            Ok(ValuesOrPreparedCall::Values(Vec::new()))
        }
    }
}

impl And {
    async fn tail_eval(
        &self,
        env: &Gc<Env>,
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

impl Or {
    async fn tail_eval(
        &self,
        env: &Gc<Env>,
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

impl Set {
    async fn eval(
        &self,
        env: &Gc<Env>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<(), RuntimeError> {
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
            .clone();
        self.var.set(env, &Gc::new(val)).await;
        Ok(())
    }
}

impl SyntaxCase {
    async fn eval(
        &self,
        env: &Gc<Env>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let new_cont = Arc::new(Continuation::new(
            Arc::new(ResumableSyntaxCase::new(env, &self.transformer)),
            cont,
        ));
        let val = self.arg.eval(env, &Some(new_cont)).await?.require_one()?;
        // This clones _all_ syntax objects; we should fix this to be more optimal.
        let syntax = Syntax::from_datum(&BTreeSet::default(), &val);
        let transformed = self.transformer.expand(&syntax).unwrap();
        let expansion_env = ExpansionEnv::from_env(env);
        Expression::parse(transformed, &expansion_env, cont)
            .await
            .expect("fixme")
            .eval(env, cont)
            .await
    }
}

impl Vector {
    fn eval(&self) -> Value {
        Value::Vector(self.vals.clone())
    }
}
