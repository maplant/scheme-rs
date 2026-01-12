//! Support for dynamic evaluation library (rnrs eval (6))

use std::{collections::BTreeSet, sync::Arc};

use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    ast::{Expression, ImportSet, ParseContext, discard_for},
    cps::Compile,
    env::{Environment, TopLevelEnvironment},
    exceptions::Exception,
    proc::{Application, DynamicState},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::cps_bridge,
    runtime::Runtime,
    syntax::Syntax,
    value::Value,
};

#[maybe_async]
#[cps_bridge(def = "eval expression environment", lib = "(rnrs eval (6))")]
pub fn eval(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let [expression, environment] = args else {
        unreachable!()
    };
    let env = environment.try_to_rust_type::<Environment>()?;
    let expr = Syntax::syntax_from_datum(&BTreeSet::default(), expression.clone());
    let ctxt = ParseContext::new(runtime, false);
    let expr = maybe_await!(Expression::parse(&ctxt, expr, &env))?;
    let compiled = expr.compile_top_level();
    let proc = maybe_await!(runtime.compile_expr(compiled));
    Ok(Application::new(proc, vec![k]))
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
    _args: &[Value],
    import_spec: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let marks = BTreeSet::default();
    let import_sets = import_spec
        .iter()
        .cloned()
        .map(|spec| {
            let syntax = Syntax::syntax_from_datum(&marks, spec);
            ImportSet::parse(discard_for(&syntax))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let env = Environment::Top(TopLevelEnvironment::new_repl(runtime));
    for import_set in import_sets {
        maybe_await!(env.import(import_set))?;
    }
    let env = Value::from(Record::from_rust_type(env));
    Ok(Application::new(k.try_into().unwrap(), vec![env]))
}
