use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use scheme_rs_macros::bridge;

use crate::{
    exceptions::Exception,
    gc::{Gc, Trace},
    proc::{Application, ContBarrier, Procedure},
    records::{RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::cps_bridge,
    runtime::Runtime,
    value::Value,
};

#[derive(Clone, Trace)]
pub struct Parameter {
    id: usize,
    default: Value,
    converter: Value,
}

impl std::fmt::Debug for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<parameter>")
    }
}

impl Parameter {
    pub fn new(default: Value, converter: Value) -> Self {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);
        Self {
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            default,
            converter,
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn default_value(&self) -> Value {
        self.default.clone()
    }

    pub fn converter(&self) -> Value {
        self.converter.clone()
    }
}

impl SchemeCompatible for Parameter {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "parameter", sealed: true, opaque: true)
    }
}

#[bridge(name = "%make-parameter", lib = "(rnrs parameters bridge)")]
pub fn make_parameter_bridge(init: &Value, converter: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from_rust_type(Parameter::new(
        init.clone(),
        converter.clone(),
    ))])
}

#[cps_bridge(def = "%parameter-ref param", lib = "(rnrs parameters bridge)")]
pub fn parameter_ref_bridge(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [param_val] = args else {
        return Err(Exception::wrong_num_of_args(1, args.len()));
    };
    let param: Gc<Parameter> = param_val.try_to_rust_type::<Parameter>()?;
    let val = barrier.parameter_ref(&param);
    Ok(Application::new(k, None, vec![val]))
}

#[cps_bridge(def = "%parameter-set! param val", lib = "(rnrs parameters bridge)")]
pub fn parameter_set_bridge(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [param_val, new_val] = args else {
        return Err(Exception::wrong_num_of_args(2, args.len()));
    };
    let param: Gc<Parameter> = param_val.try_to_rust_type::<Parameter>()?;
    barrier.parameter_set(param.id(), new_val.clone());
    Ok(Application::new(k, None, vec![]))
}

#[bridge(name = "%parameter-converter", lib = "(rnrs parameters bridge)")]
pub fn parameter_converter_bridge(param_val: &Value) -> Result<Vec<Value>, Exception> {
    let param: Gc<Parameter> = param_val.try_to_rust_type::<Parameter>()?;
    Ok(vec![param.converter()])
}

fn find_parameter_in_env(val: &Value) -> Option<Value> {
    let proc: Procedure = val.clone().try_into().ok()?;
    proc.0
        .env
        .iter()
        .find(|v| v.try_to_rust_type::<Parameter>().is_ok())
        .cloned()
}

#[bridge(name = "%parameter-extract", lib = "(rnrs parameters bridge)")]
pub fn parameter_extract(val: &Value) -> Result<Vec<Value>, Exception> {
    find_parameter_in_env(val)
        .ok_or_else(|| Exception::error("not a parameter"))
        .map(|v| vec![v])
}

#[bridge(name = "parameter?", lib = "(rnrs parameters bridge)")]
pub fn is_parameter(val: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(find_parameter_in_env(val).is_some())])
}
