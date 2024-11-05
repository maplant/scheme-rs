use async_trait::async_trait;
use proc_macros::builtin;

use crate::{
    ast,
    env::{Env, LexicalContour},
    error::{RuntimeError, RuntimeErrorKind},
    eval::{Eval, ValueOrPreparedCall},
    expand::Transformer,
    gc::Gc,
    proc::{Callable, PreparedCall},
    syntax::{Identifier, Syntax},
    util::ArcSlice,
    value::Value,
};
use std::sync::Arc;

#[async_trait]
pub trait Resumable: Send + Sync {
    async fn resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError>;
}

#[derive(Clone)]
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
}

#[async_trait]
impl Resumable for Continuation {
    async fn resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        if let Some(ref remaining) = self.remaining {
            let new_cont = Some(Arc::new(Continuation::new(remaining.clone(), cont)));
            let resume_result = self.resume_point.resume(arg, &new_cont).await?;
            remaining.resume(resume_result, cont).await
        } else {
            self.resume_point.resume(arg, cont).await
        }
    }
}

#[async_trait]
impl Callable for Option<Arc<Continuation>> {
    fn min_args(&self) -> usize {
        1
    }

    fn max_args(&self) -> Option<usize> {
        Some(1)
    }

    async fn call(
        &self,
        args: Vec<Gc<Value>>,
        _calling_cont: &Self,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        Err(RuntimeError::abandon_current_continuation(
            args,
            self.clone(),
        ))
    }
}

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
    ) -> Result<Gc<Value>, RuntimeError> {
        let mut inner = self.inner.eval(env, cont).await;
        while let Err(RuntimeError {
            kind: RuntimeErrorKind::AbandonCurrentContinuation { mut args, new_cont },
            ..
        }) = inner
        {
            // Abandon the current continuation and evaluate the newly returned one
            // TODO: Retain the backtrace for errors
            let arg = args.pop().unwrap();
            if let Some(new_cont) = new_cont {
                inner = new_cont.resume(arg, cont).await;
            } else {
                return Ok(arg);
            }
        }
        inner
    }
}

pub struct ResumableBody {
    env: Env,
    remaining: ArcSlice<Syntax>,
}

impl ResumableBody {
    pub fn new(env: &Env, remaining: &ArcSlice<Syntax>) -> Self {
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
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        let Some(last) = self.remaining.last() else {
            return Ok(arg);
        };
        for (expr, tail) in self.remaining.skip_last() {
            let cont = Some(Arc::new(Continuation::new(
                Arc::new(ResumableBody::new(&self.env, &tail)),
                cont,
            )));
            expr.compile(&self.env, &cont)
                .await?
                .eval(&self.env, &cont)
                .await?;
        }
        last.compile(&self.env, cont)
            .await?
            .eval(&self.env, cont)
            .await
    }
}

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
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
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
}

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
        arg: Gc<Value>,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        // TODO: Add try_unwrap to GC to avoid the clone of the inner value
        let val = arg.read().await.clone();
        *self
            .env
            .fetch_var(&self.var)
            .await
            .ok_or_else(|| RuntimeError::undefined_variable(self.var.clone()))?
            .write()
            .await = val;
        Ok(Gc::new(Value::Null))
    }
}

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
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        // This situation should never occur, because we don't create a new continuation
        // for the last argument
        let Some(last) = self.args.last() else {
            return Ok(arg);
        };
        if !arg.read().await.is_true() {
            return Ok(Gc::new(Value::Boolean(false)));
        }
        for (arg, tail) in self.args.skip_last() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableAnd::new(&self.env, &tail)),
                cont,
            ));
            if !arg
                .eval(&self.env, &Some(cont))
                .await?
                .read()
                .await
                .is_true()
            {
                return Ok(Gc::new(Value::Boolean(false)));
            }
        }
        last.eval(&self.env, cont).await
    }
}

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
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        // This situation should never occur, because we don't create a new continuation
        // for the last argument
        let Some(last) = self.args.last() else {
            return Ok(arg);
        };
        if arg.read().await.is_true() {
            return Ok(Gc::new(Value::Boolean(true)));
        }
        for (arg, tail) in self.args.skip_last() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableAnd::new(&self.env, &tail)),
                cont,
            ));
            if arg
                .eval(&self.env, &Some(cont))
                .await?
                .read()
                .await
                .is_true()
            {
                return Ok(Gc::new(Value::Boolean(true)));
            }
        }
        last.eval(&self.env, cont).await
    }
}

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
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
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
            let val = expr.eval(&up, &Some(cont)).await?;
            self.scope.write().await.def_var(ident, val);
        }
        self.body.eval(&Env::from(self.scope.clone()), cont).await
    }
}

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
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        if arg.read().await.is_true() {
            self.success.eval(&self.env, cont).await
        } else if let Some(ref failure) = self.failure {
            failure.eval(&self.env, cont).await
        } else {
            Ok(Gc::new(Value::Null))
        }
    }
}

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
        arg: Gc<Value>,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        self.env.def_var(&self.name, arg).await;
        Ok(Gc::new(Value::Null))
    }
}

pub struct ResumableDefineSyntax {
    env: Env,
    name: Identifier,
}

impl ResumableDefineSyntax {
    pub fn new(env: &Env, name: &Identifier) -> Self {
        Self {
            env: env.clone(),
            name: name.clone(),
        }
    }
}

#[async_trait]
impl Resumable for ResumableDefineSyntax {
    async fn resume(
        &self,
        arg: Gc<Value>,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        self.env.def_macro(&self.name, arg).await;
        Ok(Gc::new(Value::Null))
    }
}

pub struct ResumableCall {
    env: Env,
    // TODO: Making this a SmallVec of around 10 would probably be a
    // performance improvement.
    collected: Vec<Gc<Value>>,
    remaining: ArcSlice<Arc<dyn Eval>>,
}

impl ResumableCall {
    pub fn new(env: &Env, collected: &[Gc<Value>], remaining: ArcSlice<Arc<dyn Eval>>) -> Self {
        Self {
            env: env.clone(),
            collected: collected.to_owned(),
            remaining,
        }
    }
}

#[async_trait]
impl Resumable for ResumableCall {
    async fn resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Gc<Value>, RuntimeError> {
        let mut collected = self.collected.clone();
        collected.push(arg);
        for (arg, remaining) in self.remaining.iter() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableCall::new(&self.env, &collected, remaining)),
                cont,
            ));
            let arg = arg.eval(&self.env, &Some(cont)).await?;
            collected.push(arg);
        }
        PreparedCall::prepare(collected).eval(cont).await
    }
}

#[builtin("call/cc")]
pub async fn call_cc(
    cont: &Option<Arc<Continuation>>,
    proc: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let callable = {
        let proc = proc.read().await;
        proc.as_callable()
            .ok_or_else(|| RuntimeError::invalid_type("procedure", proc.type_name()))?
    };
    callable
        .call(vec![Gc::new(Value::Continuation(cont.clone()))], cont)
        .await?
        .eval(cont)
        .await
}
