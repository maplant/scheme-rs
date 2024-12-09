use async_trait::async_trait;
use futures::future::BoxFuture;
use proc_macros::builtin;

use crate::{
    ast,
    env::{Env, LexicalContour},
    error::{RuntimeError, RuntimeErrorKind},
    eval::{Eval, ValuesOrPreparedCall},
    expand::Transformer,
    gc::{Gc, Trace},
    proc::{Callable, PreparedCall, ProcDebugInfo},
    syntax::{Identifier, Span},
    util::{ArcSlice, RequireOne},
    value::Value,
};
use std::sync::Arc;

#[async_trait]
pub trait Resumable: Trace + Send + Sync {
    fn min_args(&self) -> usize {
        1
    }

    fn max_args(&self) -> Option<usize> {
        Some(1)
    }

    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError>;

    async fn clone_stack(&self) -> Arc<dyn Resumable>;
}

#[derive(Clone, Trace)]
pub struct Continuation {
    resume_point: Arc<dyn Resumable>,
    remaining: Option<Arc<Continuation>>,
}

impl Continuation {
    pub fn new(resume_point: Arc<dyn Resumable>, remaining: &Option<Arc<Continuation>>) -> Self {
        Self {
            resume_point,
            remaining: remaining.clone(),
        }
    }

    fn clone_stack(&self) -> BoxFuture<'_, Arc<Self>> {
        Box::pin(async move {
            Arc::new(Self {
                resume_point: self.resume_point.clone_stack().await,
                remaining: if let Some(ref cont) = self.remaining {
                    Some(cont.clone_stack().await)
                } else {
                    None
                },
            })
        })
    }
}

#[async_trait]
impl Resumable for Continuation {
    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        if let Some(ref remaining) = self.remaining {
            let new_cont = Some(Arc::new(Continuation::new(remaining.clone(), cont)));
            let resume_result = self.resume_point.resume(args, &new_cont).await?;
            remaining.resume(resume_result, cont).await
        } else {
            self.resume_point.resume(args, cont).await
        }
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        self.clone_stack().await
    }
}

#[async_trait]
impl Callable for Option<Arc<Continuation>> {
    fn min_args(&self) -> usize {
        self.as_ref()
            .map(|x| x.resume_point.min_args())
            .unwrap_or(1)
    }

    fn max_args(&self) -> Option<usize> {
        self.as_ref()
            .map(|x| x.resume_point.max_args())
            .unwrap_or(None)
    }

    async fn call(
        &self,
        args: Vec<Gc<Value>>,
        _calling_cont: &Self,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        Err(RuntimeError::abandon_current_continuation(
            args,
            self.clone(),
        ))
    }
}

#[derive(Trace)]
pub struct CatchContinuationCall {
    inner: Arc<dyn Eval>,
}

impl CatchContinuationCall {
    pub fn new(inner: Arc<dyn Eval>) -> Self {
        Self { inner }
    }
}

#[async_trait]
impl Eval for CatchContinuationCall {
    async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let mut inner = self.inner.eval(env, cont).await;
        while let Err(RuntimeError {
            kind: RuntimeErrorKind::AbandonCurrentContinuation { args, new_cont },
            ..
        }) = inner
        {
            // Abandon the current continuation and evaluate the newly returned one
            // TODO: Retain the backtrace for errors
            // let arg = args.pop().unwrap();
            if let Some(new_cont) = new_cont {
                inner = new_cont.resume(args, cont).await;
            } else {
                return Ok(args);
            }
        }
        inner
    }
}

#[derive(Trace, Clone)]
pub struct ResumableBody {
    env: Env,
    remaining: ArcSlice<Arc<dyn Eval>>,
}

impl ResumableBody {
    pub fn new(env: &Env, remaining: &ArcSlice<Arc<dyn Eval>>) -> Self {
        Self {
            env: env.clone(),
            remaining: remaining.clone(),
        }
    }
}

#[async_trait]
impl Resumable for ResumableBody {
    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let Some(last) = self.remaining.last() else {
            return Ok(args);
        };
        for (expr, tail) in self.remaining.skip_last() {
            let cont = Some(Arc::new(Continuation::new(
                Arc::new(ResumableBody::new(&self.env, &tail)),
                cont,
            )));
            expr.eval(&self.env, &cont).await?;
        }
        last.eval(&self.env, cont).await
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone().await,
            remaining: self.remaining.clone(),
        })
    }
}

#[derive(Trace)]
pub struct ResumableSyntaxCase {
    env: Env,
    transformer: Transformer,
}

impl ResumableSyntaxCase {
    pub fn new(env: &Env, transformer: &Transformer) -> Self {
        Self {
            env: env.clone(),
            transformer: transformer.clone(),
        }
    }
}

#[async_trait]
impl Resumable for ResumableSyntaxCase {
    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let arg = args.require_one()?;
        let arg = arg.read().await;
        match &*arg {
            Value::Syntax(syntax) => {
                let result = self.transformer.expand(syntax).unwrap();
                result
                    .compile(&self.env, cont)
                    .await?
                    .eval(&self.env, cont)
                    .await
            }
            _ => todo!(),
        }
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone().await,
            transformer: self.transformer.clone(),
        })
    }
}

#[derive(Trace)]
pub struct ResumableSet {
    env: Env,
    var: Identifier,
}

impl ResumableSet {
    pub fn new(env: &Env, var: &Identifier) -> Self {
        Self {
            env: env.clone(),
            var: var.clone(),
        }
    }
}

#[async_trait]
impl Resumable for ResumableSet {
    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        // TODO: Add try_unwrap to GC to avoid the clone of the inner value
        let arg = args.require_one()?;
        let val = arg.read().await.clone();
        *self
            .env
            .fetch_var(&self.var)
            .await
            .ok_or_else(|| RuntimeError::undefined_variable(self.var.clone()))?
            .write()
            .await = val;
        Ok(vec![Gc::new(Value::Null)])
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone().await,
            var: self.var.clone(),
        })
    }
}

#[derive(Trace)]
pub struct ResumableAnd {
    env: Env,
    args: ArcSlice<Arc<dyn Eval>>,
}

impl ResumableAnd {
    pub fn new(env: &Env, args: &ArcSlice<Arc<dyn Eval>>) -> Self {
        Self {
            env: env.clone(),
            args: args.clone(),
        }
    }
}

#[async_trait]
impl Resumable for ResumableAnd {
    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        // This situation should never occur, because we don't create a new continuation
        // for the last argument
        let Some(last) = self.args.last() else {
            return Ok(args);
        };
        let arg = args.require_one()?;
        if !arg.read().await.is_true() {
            return Ok(vec![Gc::new(Value::Boolean(false))]);
        }
        for (arg, tail) in self.args.skip_last() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableAnd::new(&self.env, &tail)),
                cont,
            ));
            if !arg
                .eval(&self.env, &Some(cont))
                .await?
                .require_one()?
                .read()
                .await
                .is_true()
            {
                return Ok(vec![Gc::new(Value::Boolean(false))]);
            }
        }
        last.eval(&self.env, cont).await
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone().await,
            args: self.args.clone(),
        })
    }
}

#[derive(Trace)]
pub struct ResumableOr {
    env: Env,
    args: ArcSlice<Arc<dyn Eval>>,
}

impl ResumableOr {
    pub fn new(env: &Env, args: &ArcSlice<Arc<dyn Eval>>) -> Self {
        Self {
            env: env.clone(),
            args: args.clone(),
        }
    }
}

#[async_trait]
impl Resumable for ResumableOr {
    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        // This situation should never occur, because we don't create a new continuation
        // for the last argument
        let Some(last) = self.args.last() else {
            return Ok(args);
        };
        let arg = args.require_one()?;
        if arg.read().await.is_true() {
            return Ok(vec![Gc::new(Value::Boolean(true))]);
        }
        for (arg, tail) in self.args.skip_last() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableAnd::new(&self.env, &tail)),
                cont,
            ));
            if arg
                .eval(&self.env, &Some(cont))
                .await?
                .require_one()?
                .read()
                .await
                .is_true()
            {
                return Ok(vec![Gc::new(Value::Boolean(true))]);
            }
        }
        last.eval(&self.env, cont).await
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone().await,
            args: self.args.clone(),
        })
    }
}

#[derive(Trace)]
pub struct ResumableLet {
    scope: Gc<LexicalContour>,
    curr: Identifier,
    remaining_bindings: ArcSlice<(Identifier, Arc<dyn Eval>)>,
    body: ast::Body,
}

impl ResumableLet {
    pub fn new(
        scope: &Gc<LexicalContour>,
        curr: &Identifier,
        remaining_bindings: ArcSlice<(Identifier, Arc<dyn Eval>)>,
        body: &ast::Body,
    ) -> Self {
        Self {
            scope: scope.clone(),
            curr: curr.clone(),
            remaining_bindings,
            body: body.clone(),
        }
    }
}

#[async_trait]
impl Resumable for ResumableLet {
    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let arg = args.require_one()?;
        let up = {
            let mut scope = self.scope.write().await;
            scope.def_var(&self.curr, arg);
            scope.up.clone()
        };
        for ((ident, expr), remaining) in self.remaining_bindings.iter() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableLet::new(&self.scope, ident, remaining, &self.body)),
                cont,
            ));
            let val = expr.eval(&up, &Some(cont)).await?.require_one()?;
            self.scope.write().await.def_var(ident, val);
        }
        self.body.eval(&Env::from(self.scope.clone()), cont).await
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        Arc::new(Self {
            scope: Gc::new(self.scope.read().await.deep_clone().await),
            curr: self.curr.clone(),
            remaining_bindings: self.remaining_bindings.clone(),
            body: self.body.clone(),
        })
    }
}

#[derive(Trace)]
pub struct ResumableIf {
    env: Env,
    success: Arc<dyn Eval>,
    failure: Option<Arc<dyn Eval>>,
}

impl ResumableIf {
    pub fn new(env: &Env, success: &Arc<dyn Eval>, failure: &Option<Arc<dyn Eval>>) -> Self {
        Self {
            env: env.clone(),
            success: success.clone(),
            failure: failure.clone(),
        }
    }
}

#[async_trait]
impl Resumable for ResumableIf {
    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let arg = args.require_one()?;
        if arg.read().await.is_true() {
            self.success.eval(&self.env, cont).await
        } else if let Some(ref failure) = self.failure {
            failure.eval(&self.env, cont).await
        } else {
            Ok(vec![Gc::new(Value::Null)])
        }
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone().await,
            success: self.success.clone(),
            failure: self.failure.clone(),
        })
    }
}

#[derive(Trace)]
pub struct ResumableDefineVar {
    env: Env,
    name: Identifier,
}

impl ResumableDefineVar {
    pub fn new(env: &Env, name: &Identifier) -> Self {
        Self {
            env: env.clone(),
            name: name.clone(),
        }
    }
}

#[async_trait]
impl Resumable for ResumableDefineVar {
    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let arg = args.require_one()?;
        self.env.def_var(&self.name, arg).await;
        Ok(vec![Gc::new(Value::Null)])
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone().await,
            name: self.name.clone(),
        })
    }
}

#[derive(Trace)]
pub struct ResumableCall {
    env: Env,
    // TODO: Making this a SmallVec of around 10 would probably be a
    // performance improvement.
    collected: Vec<Gc<Value>>,
    remaining: ArcSlice<Arc<dyn Eval>>,
    proc_name: String,
    location: Span,
}

impl ResumableCall {
    pub fn new(
        proc_name: &str,
        location: &Span,
        env: &Env,
        collected: &[Gc<Value>],
        remaining: ArcSlice<Arc<dyn Eval>>,
    ) -> Self {
        Self {
            env: env.clone(),
            collected: collected.to_owned(),
            remaining,
            proc_name: proc_name.to_string(),
            location: location.clone(),
        }
    }
}

#[async_trait]
impl Resumable for ResumableCall {
    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let arg = args.require_one()?;
        let mut collected = self.collected.clone();
        collected.push(arg);
        for (arg, remaining) in self.remaining.iter() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableCall::new(
                    &self.proc_name,
                    &self.location,
                    &self.env,
                    &collected,
                    remaining,
                )),
                cont,
            ));
            let arg = arg
                .eval(&self.env, &Some(cont))
                .await
                .map_err(|mut err| {
                    err.push_frame(self.proc_name.clone(), self.location.clone());
                    err
                })?
                .require_one()?;
            collected.push(arg);
        }
        PreparedCall::prepare(collected, Some(ProcDebugInfo::new(&self.proc_name, &self.location)))
            .eval(cont)
            .await
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone().await,
            collected: self.collected.clone(),
            remaining: self.remaining.clone(),
            proc_name: self.proc_name.clone(),
            location: self.location.clone(),
        })
    }
}

async fn clone_cont_stack(cont: &Option<Arc<Continuation>>) -> Option<Arc<Continuation>> {
    if let Some(ref cont) = cont {
        Some(cont.clone_stack().await)
    } else {
        None
    }
}

#[builtin("call/cc")]
pub async fn call_cc(
    cont: &Option<Arc<Continuation>>,
    proc: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let callable = {
        let proc = proc.read().await;
        proc.as_callable()
            .ok_or_else(|| RuntimeError::invalid_type("procedure", proc.type_name()))?
    };
    callable
        .call(
            vec![Gc::new(Value::Continuation(clone_cont_stack(cont).await))],
            cont,
        )
        .await?
        .eval(cont)
        .await
}

#[derive(Trace, Clone)]
pub struct CallWithValues {
    min_args: usize,
    max_args: Option<usize>,
    consumer: Gc<Value>,
}

#[async_trait]
impl Resumable for CallWithValues {
    fn min_args(&self) -> usize {
        self.min_args
    }

    fn max_args(&self) -> Option<usize> {
        self.max_args
    }

    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let callable = {
            let proc = self.consumer.read().await;
            proc.as_callable().unwrap()
        };
        callable.call(args, cont).await?.eval(cont).await
    }

    async fn clone_stack(&self) -> Arc<dyn Resumable> {
        Arc::new(self.clone())
    }
}

#[builtin("call-with-values")]
pub async fn call_with_values(
    cont: &Option<Arc<Continuation>>,
    producer: &Gc<Value>,
    consumer: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let producer_callable = {
        let proc = producer.read().await;
        proc.as_callable()
            .ok_or_else(|| RuntimeError::invalid_type("procedure", proc.type_name()))?
    };
    let consumer_callable = {
        let proc = consumer.read().await;
        proc.as_callable()
            .ok_or_else(|| RuntimeError::invalid_type("procedure", proc.type_name()))?
    };
    let producer_cont = Arc::new(Continuation::new(
        Arc::new(CallWithValues {
            min_args: consumer_callable.min_args(),
            max_args: consumer_callable.max_args(),
            consumer: consumer.clone(),
        }),
        &clone_cont_stack(cont).await,
    ));

    let producer_result = producer_callable
        .call(Vec::new(), &Some(producer_cont.clone()))
        .await?
        .eval(&Some(producer_cont))
        .await?;

    // Is it correct to use the calling continuation? Probably, whatever.
    consumer_callable
        .call(producer_result, cont)
        .await?
        .eval(cont)
        .await
}
