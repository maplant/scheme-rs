use async_trait::async_trait;
use proc_macros::builtin;

use crate::{
    ast::{AstNode, Body, Expression},
    env::{Env, ExpansionEnv, Ref},
    error::RuntimeError,
    expand::Transformer,
    gc::{Gc, Trace},
    proc::{Callable, PreparedCall, ProcCallDebugInfo, ValuesOrPreparedCall},
    syntax::{Identifier, Span},
    util::{ArcSlice, RequireOne},
    value::Value,
};
use std::{collections::HashMap, sync::Arc};

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
    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable>;
}

#[derive(Clone, Trace)]
struct DiscardResumeArgs {
    replacement: Result<Vec<Gc<Value>>, RuntimeError>,
}

#[async_trait]
impl Resumable for DiscardResumeArgs {
    async fn resume(
        &self,
        _args: Vec<Gc<Value>>,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        self.replacement.clone()
    }

    fn clone_stack(&self, _cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        Arc::new(self.clone())
    }
}

#[derive(Trace)]
pub struct Continuation {
    resume_point: Arc<dyn Resumable>,
    remaining: Option<Arc<Continuation>>,
    dynamic_wind: Option<DynamicWind>,
}

impl Continuation {
    pub fn new(resume_point: Arc<dyn Resumable>, remaining: &Option<Arc<Continuation>>) -> Self {
        Self {
            resume_point,
            remaining: remaining.clone(),
            dynamic_wind: remaining
                .as_ref()
                .and_then(|cont| cont.dynamic_wind.clone()),
        }
    }

    pub fn with_dynamic_wind(
        resume_point: Arc<dyn Resumable>,
        remaining: &Option<Arc<Continuation>>,
        dynamic_wind: DynamicWind,
    ) -> Self {
        Self {
            resume_point,
            remaining: remaining.clone(),
            dynamic_wind: Some(dynamic_wind),
        }
    }

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<Self> {
        Arc::new(Self {
            resume_point: self.resume_point.clone_stack(cloned),
            remaining: self.remaining.as_ref().map(|cont| cont.clone_stack(cloned)),
            dynamic_wind: self.dynamic_wind.clone(),
        })
    }

    pub async fn enter_extent(
        self: &Arc<Self>,
        args: Vec<Gc<Value>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        if let Some(ref dynamic_wind) = self.dynamic_wind {
            let in_cont = Some(Arc::new(Continuation::with_dynamic_wind(
                Arc::new(ResumableDynamicWind {
                    args: args.clone(),
                    body_thunk: Arc::new(Some(self.clone())),
                    dynamic_wind: dynamic_wind.clone(),
                    stage: Stage::In,
                }),
                &None,
                dynamic_wind.clone(),
            )));

            let _ = dynamic_wind
                .in_thunk
                .call(Vec::new(), &in_cont)
                .await?
                .eval(&in_cont)
                .await?;

            let body_cont = Some(Arc::new(Continuation::with_dynamic_wind(
                Arc::new(ResumableDynamicWind {
                    args: args.clone(),
                    body_thunk: Arc::new(Some(self.clone())),
                    dynamic_wind: dynamic_wind.clone(),
                    stage: Stage::Body,
                }),
                &None,
                dynamic_wind.clone(),
            )));

            self.resume(args, &body_cont).await
        } else {
            self.resume(args, &None).await
        }
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
            let resume_result = self.resume_point.resume(args, &new_cont).await;
            if let Some(out_thunk) =
                leaving_dynamic_extent(&self.dynamic_wind, &remaining.dynamic_wind)
            {
                let out_cont = Some(Arc::new(Continuation::new(
                    Arc::new(DiscardResumeArgs {
                        replacement: resume_result.clone(),
                    }),
                    &self.remaining,
                )));
                let _ = out_thunk
                    .call(Vec::new(), &out_cont)
                    .await?
                    .eval(&out_cont)
                    .await?;
            }
            remaining.resume(resume_result?, cont).await
        } else {
            self.resume_point.resume(args, cont).await
        }
    }

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        self.clone_stack(cloned)
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
            // Cloning the stack is _extremely_ slow. This needs to be fixed at some point.
            self.as_ref()
                .map(|cont| cont.clone_stack(&mut HashMap::default())),
        ))
    }
}

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
        Ok(Vec::new())
    }

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone(cloned),
            name: self.name.clone(),
        })
    }
}

#[derive(Trace, Debug)]
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
    fn min_args(&self) -> usize {
        0
    }

    fn max_args(&self) -> Option<usize> {
        None
    }

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

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone(cloned),
            remaining: self.remaining.clone(),
        })
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

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        Arc::new(Self {
            scope: self.scope.deep_clone(cloned),
            curr: self.curr.clone(),
            remaining_bindings: self.remaining_bindings.clone(),
            body: self.body.clone(),
        })
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

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone(cloned),
            success: self.success.clone(),
            failure: self.failure.clone(),
        })
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

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone(cloned),
            args: self.args.clone(),
        })
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

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone(cloned),
            args: self.args.clone(),
        })
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
        self.var.set(&self.env, &Gc::new(val)).await;
        Ok(Vec::new())
    }

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone(cloned),
            var: self.var.clone(),
        })
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

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone(cloned),
            transformer: self.transformer.clone(),
        })
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

    fn clone_stack(&self, cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        Arc::new(Self {
            env: self.env.deep_clone(cloned),
            collected: self.collected.clone(),
            remaining: self.remaining.clone(),
            proc_name: self.proc_name.clone(),
            location: self.location.clone(),
        })
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
        .call(vec![Gc::new(Value::Continuation(cont.clone()))], cont)
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

    fn clone_stack(&self, _cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
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
        &cont
            .as_ref()
            .map(|cont| cont.clone_stack(&mut HashMap::default())),
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

#[derive(Trace, Clone)]
pub struct DynamicWind {
    in_thunk: Arc<dyn Callable>,
    out_thunk: Arc<dyn Callable>,
}

fn leaving_dynamic_extent<'a>(
    curr_wind: &'a Option<DynamicWind>,
    containing_wind: &Option<DynamicWind>,
) -> Option<&'a dyn Callable> {
    if curr_wind.as_ref().map(|wind| Arc::as_ptr(&wind.out_thunk))
        != containing_wind
            .as_ref()
            .map(|wind| Arc::as_ptr(&wind.out_thunk))
    {
        curr_wind.as_ref().map(|wind| wind.out_thunk.as_ref())
    } else {
        None
    }
}

#[derive(derive_more::Debug, Clone, Trace)]
pub struct ResumableDynamicWind {
    args: Vec<Gc<Value>>,
    #[debug(skip)]
    body_thunk: Arc<dyn Callable>,
    #[debug(skip)]
    dynamic_wind: DynamicWind,
    stage: Stage,
}

#[derive(Debug, Clone, Trace)]
enum Stage {
    In,
    Body,
}

#[async_trait]
impl Resumable for ResumableDynamicWind {
    fn min_args(&self) -> usize {
        0
    }

    fn max_args(&self) -> Option<usize> {
        None
    }

    async fn resume(
        &self,
        args: Vec<Gc<Value>>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        match self.stage {
            Stage::In => {
                let mut body = self.clone();
                body.stage = Stage::Body;
                let body = Arc::new(body);

                let body_cont = Some(Arc::new(Continuation::with_dynamic_wind(
                    body.clone(),
                    cont,
                    self.dynamic_wind.clone(),
                )));

                let vals = self
                    .body_thunk
                    .call(self.args.clone(), &body_cont)
                    .await?
                    .eval(&body_cont)
                    .await?;

                Ok(vals)
            }
            Stage::Body => Ok(args),
        }
    }

    fn clone_stack(&self, _cloned: &mut HashMap<Gc<Env>, Gc<Env>>) -> Arc<dyn Resumable> {
        // Do I need to clone the closures of the dynamic wind? I don't think so, but maybe.
        Arc::new(self.clone())
    }
}

#[builtin("dynamic-wind")]
pub async fn dynamic_wind(
    cont: &Option<Arc<Continuation>>,
    in_thunk: &Gc<Value>,
    body_thunk: &Gc<Value>,
    out_thunk: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let in_thunk = {
        let in_thunk = in_thunk.read();
        in_thunk
            .as_callable()
            .ok_or_else(|| RuntimeError::invalid_type("procedure", in_thunk.type_name()))?
            .clone()
    };

    let body_thunk = {
        let body_thunk = body_thunk.read();
        body_thunk
            .as_callable()
            .ok_or_else(|| RuntimeError::invalid_type("procedure", body_thunk.type_name()))?
            .clone()
    };

    let out_thunk = {
        let out_thunk = out_thunk.read();
        out_thunk
            .as_callable()
            .ok_or_else(|| RuntimeError::invalid_type("procedure", out_thunk.type_name()))?
            .clone()
    };

    let dynamic_wind = DynamicWind {
        in_thunk: in_thunk.clone(),
        out_thunk: out_thunk.clone(),
    };

    let in_cont = Some(Arc::new(Continuation::with_dynamic_wind(
        Arc::new(ResumableDynamicWind {
            args: Vec::new(),
            body_thunk: body_thunk.clone(),
            dynamic_wind: dynamic_wind.clone(),
            stage: Stage::In,
        }),
        cont,
        dynamic_wind.clone(),
    )));

    let _ = in_thunk
        .call(Vec::new(), &in_cont)
        .await?
        .eval(&in_cont)
        .await?;

    let body_cont = Some(Arc::new(Continuation::with_dynamic_wind(
        Arc::new(ResumableDynamicWind {
            args: Vec::new(),
            body_thunk: body_thunk.clone(),
            dynamic_wind: dynamic_wind.clone(),
            stage: Stage::Body,
        }),
        cont,
        dynamic_wind.clone(),
    )));

    let res = body_thunk
        .call(Vec::new(), &body_cont)
        .await?
        .eval(&body_cont)
        .await?;

    let out_cont = Some(Arc::new(Continuation::with_dynamic_wind(
        Arc::new(DiscardResumeArgs {
            replacement: Ok(res.clone()),
        }),
        cont,
        dynamic_wind.clone(),
    )));

    let _ = out_thunk
        .call(Vec::new(), &out_cont)
        .await?
        .eval(&out_cont)
        .await?;

    Ok(res)
}
