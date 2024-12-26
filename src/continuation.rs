use async_trait::async_trait;
use proc_macros::builtin;

use crate::{
    ast::{self, AstNode, Body, Expression},
    env::{Env, ExpansionEnv, Ref},
    error::{RuntimeError, RuntimeErrorKind},
    expand::Transformer,
    gc::{Gc, Trace},
    proc::{Callable, PreparedCall, ProcCallDebugInfo, ValuesOrPreparedCall},
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

    /// Clone the contents of the resumable; necessary to ensure the continuation
    /// is unique when we make a continuation first-class.
    fn clone_stack(&self) -> Arc<dyn Resumable>;
}

#[derive(Trace)]
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

    fn clone_stack(&self) -> Arc<Self> {
        Arc::new(Self {
            resume_point: self.resume_point.clone_stack(),
            remaining: self.remaining.as_ref().map(|cont| cont.clone_stack()),
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

    fn clone_stack(&self) -> Arc<dyn Resumable> {
        self.clone_stack()
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

/*
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
 */

#[derive(Trace)]
pub struct ResumableDefineVar {
    env: Gc<Env>,
    name: Identifier,
}

impl ResumableDefineVar {
    pub fn new(env: &Gc<Env>, name: &Identifier) -> Self {
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
        self.env.write().def_local_var(&self.name, arg);
        Ok(vec![])
    }

    fn clone_stack(&self) -> Arc<dyn Resumable> {
        /*
        Arc::new(Self {
            env: self.env.deep_clone(),
            name: self.name.clone(),
        })
         */
        todo!()
    }
}

#[derive(Trace)]
pub struct ResumableBody {
    env: Gc<Env>,
    remaining: ArcSlice<AstNode>,
}

impl ResumableBody {
    pub fn new(env: &Gc<Env>, remaining: &ArcSlice<AstNode>) -> Self {
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
        for (form, tail) in self.remaining.skip_last() {
            let cont = Some(Arc::new(Continuation::new(
                Arc::new(ResumableBody::new(&self.env, &tail)),
                cont,
            )));
            form.eval(&self.env, &cont).await?;
        }
        last.eval(&self.env, cont).await
    }

    fn clone_stack(&self) -> Arc<dyn Resumable> {
        /*
        Arc::new(Self {
            env: self.env.deep_clone(),
            remaining: self.remaining.clone(),
        })
         */
        todo!()
    }
}

#[derive(Trace)]
pub struct ResumableLet {
    scope: Gc<Env>,
    curr: Identifier,
    remaining_bindings: ArcSlice<(Identifier, Expression)>,
    body: Body,
}

impl ResumableLet {
    pub fn new(
        scope: &Gc<Env>,
        curr: &Identifier,
        remaining_bindings: ArcSlice<(Identifier, Expression)>,
        body: &Body,
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
            let mut scope = self.scope.write();
            scope.def_local_var(&self.curr, arg);
            scope.up.as_ref().unwrap().clone()
        };
        for ((ident, expr), remaining) in self.remaining_bindings.iter() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableLet::new(&self.scope, ident, remaining, &self.body)),
                cont,
            ));
            let val = expr.eval(&up, &Some(cont)).await?.require_one()?;
            self.scope.write().def_local_var(ident, val);
        }
        self.body.eval(&self.scope, cont).await
    }

    fn clone_stack(&self) -> Arc<dyn Resumable> {
        /*
        Arc::new(Self {
            scope: Gc::new(self.scope.read().deep_clone()),
            curr: self.curr.clone(),
            remaining_bindings: self.remaining_bindings.clone(),
            body: self.body.clone(),
        })
         */
        todo!()
    }
}

#[derive(Trace)]
pub struct ResumableIf {
    env: Gc<Env>,
    success: Arc<Expression>,
    failure: Option<Arc<Expression>>,
}

impl ResumableIf {
    pub fn new(
        env: &Gc<Env>,
        success: &Arc<Expression>,
        failure: &Option<Arc<Expression>>,
    ) -> Self {
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
        if arg.read().is_true() {
            self.success.eval(&self.env, cont).await
        } else if let Some(ref failure) = self.failure {
            failure.eval(&self.env, cont).await
        } else {
            Ok(Vec::new())
        }
    }

    fn clone_stack(&self) -> Arc<dyn Resumable> {
        /*
        Arc::new(Self {
            env: self.env.deep_clone(),
            success: self.success.clone(),
            failure: self.failure.clone(),
        })
         */
        todo!()
    }
}

#[derive(Trace)]
pub struct ResumableAnd {
    env: Gc<Env>,
    args: ArcSlice<Expression>,
}

impl ResumableAnd {
    pub fn new(env: &Gc<Env>, args: &ArcSlice<Expression>) -> Self {
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
        if !arg.read().is_true() {
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
                .is_true()
            {
                return Ok(vec![Gc::new(Value::Boolean(false))]);
            }
        }
        last.eval(&self.env, cont).await
    }

    fn clone_stack(&self) -> Arc<dyn Resumable> {
        /*
            Arc::new(Self {
                env: self.env.deep_clone(),
                args: self.args.clone(),
        })*/
        todo!()
    }
}

#[derive(Trace)]
pub struct ResumableOr {
    env: Gc<Env>,
    args: ArcSlice<Expression>,
}

impl ResumableOr {
    pub fn new(env: &Gc<Env>, args: &ArcSlice<Expression>) -> Self {
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
        if arg.read().is_true() {
            return Ok(vec![Gc::new(Value::Boolean(true))]);
        }
        for (arg, tail) in self.args.skip_last() {
            let cont = Arc::new(Continuation::new(
                Arc::new(ResumableOr::new(&self.env, &tail)),
                cont,
            ));
            if arg
                .eval(&self.env, &Some(cont))
                .await?
                .require_one()?
                .read()
                .is_true()
            {
                return Ok(vec![Gc::new(Value::Boolean(true))]);
            }
        }
        last.eval(&self.env, cont).await
    }

    fn clone_stack(&self) -> Arc<dyn Resumable> {
        /*
            Arc::new(Self {
                env: self.env.deep_clone(),
                args: self.args.clone(),
        })
             */
        todo!()
    }
}

#[derive(Trace)]
pub struct ResumableSet {
    env: Gc<Env>,
    var: Ref,
}

impl ResumableSet {
    pub fn new(env: &Gc<Env>, var: &Ref) -> Self {
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
        let arg = args.require_one()?;
        let val = arg.read().clone();
        self.var.set(&self.env, &Gc::new(val));
        Ok(Vec::new())
    }

    fn clone_stack(&self) -> Arc<dyn Resumable> {
        /*
            Arc::new(Self {
                env: self.env.deep_clone(),
                var: self.var.clone(),
        })
             */
        todo!()
    }
}

#[derive(Trace)]
pub struct ResumableSyntaxCase {
    env: Gc<Env>,
    transformer: Transformer,
}

impl ResumableSyntaxCase {
    pub fn new(env: &Gc<Env>, transformer: &Transformer) -> Self {
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
        let transformed = {
            let arg = arg.read();
            match &*arg {
                Value::Syntax(syntax) => self.transformer.expand(syntax).unwrap(),
                _ => todo!(),
            }
        };
        let expansion_env = ExpansionEnv::from_env(&self.env);
        Expression::parse(transformed, &expansion_env, cont)
            .await
            .expect("fixme")
            .eval(&self.env, cont)
            .await
    }

    fn clone_stack(&self) -> Arc<dyn Resumable> {
        /*
        Arc::new(Self {
            env: self.env.deep_clone(),
            transformer: self.transformer.clone(),
        })
         */
        todo!()
    }
}

#[derive(Trace)]
pub struct ResumableCall {
    env: Gc<Env>,
    // TODO: Making this a SmallVec of around 10 would probably be a
    // performance improvement.
    collected: Vec<Gc<Value>>,
    remaining: ArcSlice<Expression>,
    proc_name: String,
    location: Span,
}

impl ResumableCall {
    pub fn new(
        proc_name: &str,
        location: &Span,
        env: &Gc<Env>,
        collected: &[Gc<Value>],
        remaining: ArcSlice<Expression>,
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
        PreparedCall::prepare(
            collected,
            Some(ProcCallDebugInfo::new(&self.proc_name, &self.location)),
        )
        .eval(cont)
        .await
    }

    fn clone_stack(&self) -> Arc<dyn Resumable> {
        /*
        Arc::new(Self {
            env: self.env.deep_clone(),
            collected: self.collected.clone(),
            remaining: self.remaining.clone(),
            proc_name: self.proc_name.clone(),
            location: self.location.clone(),
        })
         */
        todo!()
    }
}

#[builtin("call/cc")]
pub async fn call_cc(
    cont: &Option<Arc<Continuation>>,
    proc: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let callable = {
        let proc = proc.read();
        proc.as_callable()
            .ok_or_else(|| RuntimeError::invalid_type("procedure", proc.type_name()))?
    };
    callable
        .call(
            vec![Gc::new(Value::Continuation(
                cont.as_ref().map(|cont| cont.clone_stack()),
            ))],
            cont,
        )
        .await?
        .eval(cont)
        .await
}


#[derive(Clone, Trace)]
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
            let proc = self.consumer.read();
            proc.as_callable().unwrap()
        };
        callable.call(args, cont).await?.eval(cont).await
    }

    fn clone_stack(&self) -> Arc<dyn Resumable> {
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
        let proc = producer.read();
        proc.as_callable()
            .ok_or_else(|| RuntimeError::invalid_type("procedure", proc.type_name()))?
    };
    let consumer_callable = {
        let proc = consumer.read();
        proc.as_callable()
            .ok_or_else(|| RuntimeError::invalid_type("procedure", proc.type_name()))?
    };
    let producer_cont = Arc::new(Continuation::new(
        Arc::new(CallWithValues {
            min_args: consumer_callable.min_args(),
            max_args: consumer_callable.max_args(),
            consumer: consumer.clone(),
        }),
        &cont.as_ref().map(|cont| cont.clone_stack()),
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
