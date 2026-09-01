//! Dynamic evaluation.

use std::{collections::BTreeSet, sync::Arc};

use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    HashSet,
    ast::{Expression, ImportSet, ParseContext, discard_for},
    cps::compile::Compiler,
    env::{Environment, TopLevelEnvironment},
    exceptions::Exception,
    proc::{Application, Args, ContBarrier},
    records::{Embeddable, Embedded, RecordTypeDescriptor, rtd},
    registry::bridge,
    runtime::Runtime,
    syntax::{Span, Syntax},
    value::Value,
};

#[maybe_async]
#[bridge(name = "eval", lib = "(rnrs eval (6))")]
pub fn eval(
    expression: Value,
    environment: Embedded<Environment>,
    barrier: &mut ContBarrier<'_>,
) -> Result<Application, Exception> {
    let env = environment;
    let expr = Syntax::datum_to_syntax(&env.get_scope_set(), expression, &Span::default());
    let ctxt = ParseContext::new(false);
    let mut mutable_vars = HashSet::default();
    let expr = maybe_await!(Expression::parse(&ctxt, expr, &env, &mut mutable_vars))?;
    let result = maybe_await!(Compiler::new(mutable_vars).compile(Runtime::handle(), &expr))?;
    Ok(barrier.call_cont(Args::pack(result)))
}

unsafe impl Embeddable for Environment {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(ty: Environment, name: "environment", sealed: true, opaque: true)
    }
}

#[maybe_async]
#[bridge(name = "environment", lib = "(rnrs eval (6))")]
pub fn environment(
    #[rest_args] import_spec: Value,
    barrier: &mut ContBarrier<'_>,
) -> Result<Application, Exception> {
    let import_sets = crate::lists::iter_list(&import_spec)
        .map(|spec| {
            let syntax = Syntax::datum_to_syntax(&BTreeSet::default(), spec, &Span::default());
            ImportSet::parse(discard_for(&syntax))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let env = Environment::Top(TopLevelEnvironment::new_repl());
    for import_set in import_sets {
        maybe_await!(env.import(import_set))?;
    }
    let env = Value::from(env);
    Ok(barrier.call_cont(Args::pack([env])))
}
