use async_trait::async_trait;
use proc_macros::builtin;

use crate::{
    ast,
    env::{Env, LexicalContour},
    error::RuntimeError,
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

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError>;
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
        let new_cont = Arc::new(Continuation::new(Arc::new(self.clone()), cont));
        if let Some(ref remaining) = self.remaining {
            let resume_result = self.resume_point.resume(arg, &Some(new_cont)).await?;
            let new_cont = Arc::new(Continuation::new(remaining.clone(), cont));
            remaining.resume(resume_result, &Some(new_cont)).await
        } else {
            self.resume_point.resume(arg, &Some(new_cont)).await
        }
    }

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        let new_cont = Arc::new(Continuation::new(Arc::new(self.clone()), cont));
        if let Some(ref remaining) = self.remaining {
            let resume_result = self.resume_point.resume(arg, &Some(new_cont)).await?;
            let new_cont = Arc::new(Continuation::new(remaining.clone(), cont));
            remaining.tail_resume(resume_result, &Some(new_cont)).await
        } else {
            self.resume_point.tail_resume(arg, &Some(new_cont)).await
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
        mut args: Vec<Gc<Value>>,
        calling_cont: &Self,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        let arg = args.pop().unwrap();
        if let Some(cont) = self {
            cont.tail_resume(arg, calling_cont).await
        } else {
            println!("Continuation is empty");
            Ok(ValueOrPreparedCall::Value(arg))
        }
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
        self.tail_resume(arg, cont).await?.eval(cont).await
    }

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        for (head, tail) in self.remaining.iter() {
            let cont = Some(Arc::new(Continuation::new(
                Arc::new(ResumableBody::new(&self.env, &tail)),
                cont,
            )));
            let compiled = head.compile(&self.env, &cont).await?;
            if tail.is_empty() {
                return compiled.tail_eval(&self.env, &cont).await;
            } else {
                compiled.eval(&self.env, &cont).await?;
            }
        }

        // Only reached if body is empty
        Ok(ValueOrPreparedCall::Value(arg))
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

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        Ok(ValueOrPreparedCall::Value(self.resume(arg, cont).await?))
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
        *self
            .env
            .fetch_var(&self.var)
            .await
            .ok_or_else(|| RuntimeError::undefined_variable(self.var.clone()))?
            .write()
            .await = arg.read().await.clone();
        Ok(Gc::new(Value::Nil))
    }

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        Ok(ValueOrPreparedCall::Value(self.resume(arg, cont).await?))
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
        self.tail_resume(arg, cont).await?.eval(cont).await
    }

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        // This situation should never occur, because we don't create a new continuation
        // for the last argument
        let Some(last) = self.args.last() else {
            return Ok(ValueOrPreparedCall::Value(arg));
        };
        if !arg.read().await.is_true() {
            return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(false))));
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
                return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(false))));
            }
        }
        last.tail_eval(&self.env, cont).await
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
        self.tail_resume(arg, cont).await?.eval(cont).await
    }

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        // This situation should never occur, because we don't create a new continuation
        // for the last argument
        let Some(last) = self.args.last() else {
            return Ok(ValueOrPreparedCall::Value(arg));
        };
        if arg.read().await.is_true() {
            return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(true))));
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
                return Ok(ValueOrPreparedCall::Value(Gc::new(Value::Boolean(true))));
            }
        }
        last.tail_eval(&self.env, cont).await
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
        self.tail_resume(arg, cont).await?.eval(cont).await
    }

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
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
        self.body
            .tail_eval(&Env::from(self.scope.clone()), cont)
            .await
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
        self.tail_resume(arg, cont).await?.eval(cont).await
    }

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        if arg.read().await.is_true() {
            self.success.tail_eval(&self.env, cont).await
        } else if let Some(ref failure) = self.failure {
            failure.tail_eval(&self.env, cont).await
        } else {
            Ok(ValueOrPreparedCall::Value(Gc::new(Value::Nil)))
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
        Ok(Gc::new(Value::Nil))
    }

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        Ok(ValueOrPreparedCall::Value(self.resume(arg, cont).await?))
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
        Ok(Gc::new(Value::Nil))
    }

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
        Ok(ValueOrPreparedCall::Value(self.resume(arg, cont).await?))
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
        self.tail_resume(arg, cont).await?.eval(cont).await
    }

    async fn tail_resume(
        &self,
        arg: Gc<Value>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValueOrPreparedCall, RuntimeError> {
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
        Ok(ValueOrPreparedCall::PreparedCall(PreparedCall::prepare(
            collected,
        )))
    }
}

#[builtin("call/cc")]
pub async fn call_cc(
    cont: &Option<Arc<Continuation>>,
    proc: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let proc = proc.read().await;
    let callable = proc
        .as_callable()
        .ok_or_else(|| RuntimeError::invalid_type("procedure", proc.type_name()))?;
    callable
        .call(vec![Gc::new(Value::Continuation(cont.clone()))], cont)
        .await?
        .eval(cont)
        .await
}
