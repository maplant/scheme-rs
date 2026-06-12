//! Dynamic evaluation.

use std::{collections::BTreeSet, sync::Arc};

use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    HashSet,
    ast::{Expression, ImportSet, ParseContext, discard_for},
    cps::compile::Compiler,
    env::{Environment, TopLevelEnvironment},
    exceptions::Exception,
    proc::{Application, ContBarrier, Procedure},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::cps_bridge,
    runtime::Runtime,
    syntax::{Span, Syntax},
    value::Value,
};

#[maybe_async]
#[cps_bridge(def = "eval expression environment", lib = "(rnrs eval (6))")]
pub fn eval(
    runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier<'_>,
) -> Result<Application, Exception> {
    let [expression, environment] = args else {
        unreachable!()
    };
    let env = environment.try_to_rust_type::<Environment>()?;
    let expr = Syntax::datum_to_syntax(&env.get_scope_set(), expression.clone(), &Span::default());
    let ctxt = ParseContext::new(runtime, false);
    let mut mutable_vars = HashSet::default();
    let expr = maybe_await!(Expression::parse(&ctxt, expr, &env, &mut mutable_vars))?;
    let proc = maybe_await!(Compiler::new(mutable_vars).compile(runtime, &expr))?;
    let result = maybe_await!(proc.call(&[], &mut ContBarrier::new()))?;
    Ok(Application::new(k, None, result))
}

impl SchemeCompatible for Environment {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "environment", sealed: true, opaque: true)
    }
}

#[maybe_async]
#[cps_bridge(def = "environment . import-spec", lib = "(rnrs eval (6))")]
pub fn environment(
    runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    _args: &[Value],
    import_spec: &[Value],
    _barrier: &mut ContBarrier<'_>,
) -> Result<Application, Exception> {
    let import_sets = import_spec
        .iter()
        .cloned()
        .map(|spec| {
            let syntax = Syntax::datum_to_syntax(&BTreeSet::default(), spec, &Span::default());
            ImportSet::parse(discard_for(&syntax))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let env = Environment::Top(TopLevelEnvironment::new_repl(runtime));
    for import_set in import_sets {
        maybe_await!(env.import(import_set))?;
    }
    let env = Value::from(Record::from_rust_type(env));
    Ok(Application::new(k, None, vec![env]))
}
