use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock};

use scheme_rs_macros::bridge;

use crate::{
    exceptions::Exception,
    gc::{OpaqueGcPtr, Trace},
    lists::list_to_vec,
    proc::{
        Application, BridgePtr, ContBarrier, ContPtr, DynStackElem, Procedure, parameter_ref,
        parameter_set, pop_dyn_stack_cont, push_dyn_stack,
    },
    records::{Embeddable, Embedded, RecordTypeDescriptor, rtd},
    registry::cps_bridge,
    value::{Cell, Value},
};

#[derive(Clone)]
pub struct Parameter {
    id: usize,
    default: Value,
    converter: Value,
    companion: OnceLock<Procedure>,
}

unsafe impl Trace for Parameter {
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        unsafe {
            self.default.visit_children(visitor);
            self.converter.visit_children(visitor);
            if let Some(proc) = self.companion.get() {
                proc.visit_children(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.default.finalize();
            self.converter.finalize();
            if let Some(proc) = self.companion.get_mut() {
                proc.finalize();
            }
        }
    }
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
            companion: OnceLock::new(),
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

    pub fn companion(&self) -> &Procedure {
        self.companion
            .get()
            .expect("companion must be initialized before use")
    }

    pub fn set_companion(&self, proc: Procedure) {
        self.companion
            .set(proc)
            .unwrap_or_else(|_| panic!("companion already initialized"));
    }
}

unsafe impl Embeddable for Parameter {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(ty: Parameter, name: "parameter", sealed: true, opaque: true)
    }
}

/// The companion procedure's body: dispatches 0-arg (ref) and 1-arg (set)
/// calls on a parameter. env[0] is the Embedded<Parameter> value.
#[cps_bridge]
fn parameter_companion(
    env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let param: Embedded<Parameter> = env[0].try_to()?;
    let total_args = args.len() + rest_args.len();
    match total_args {
        0 => {
            let val = parameter_ref(&param);
            Ok(barrier.call_cont(vec![val]))
        }
        1 => {
            let new_val = if !args.is_empty() {
                args[0].clone()
            } else {
                rest_args[0].clone()
            };
            if param.converter().is_true() {
                let converter: Procedure = param.converter().try_into()?;
                barrier.push_cont(
                    [Value::from(param.clone())],
                    ContPtr::Continuation(set_after_convert_k),
                    1,
                    false,
                );
                Ok(Application::new(converter, vec![new_val]))
            } else {
                parameter_set(&param, new_val);
                Ok(barrier.call_cont(Vec::new()))
            }
        }
        _ => Err(Exception::error("parameter accepts zero or one arguments")),
    }
}

/// Continuation that receives the converter's result and performs the
/// actual parameter_set. env[0] is the parameter.
unsafe extern "C" fn set_after_convert_k(
    env: *const Value,
    args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        let param: Embedded<Parameter> = env.as_ref().unwrap().try_to().unwrap();
        let converted_val = args.as_ref().unwrap().clone();
        let barrier = barrier.as_mut().unwrap_unchecked();
        parameter_set(&param, converted_val);
        (*out).write(barrier.call_cont(Vec::new()));
    }
}

#[cps_bridge(
    def = "%make-parameter init converter",
    lib = "(rnrs parameters bridge)"
)]
pub fn make_parameter_bridge(
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [init, converter] = args else {
        return Err(Exception::wrong_num_of_args(2, args.len()));
    };
    let param = Parameter::new(init.clone(), converter.clone());
    let embedded = Embedded::new(param);
    let param_val = Value::from(embedded.clone());
    let companion = Procedure::new(
        vec![param_val.clone()],
        parameter_companion as BridgePtr,
        0,
        true,
    );
    embedded.set_companion(companion);
    Ok(barrier.call_cont(vec![param_val]))
}

#[cps_bridge(def = "%parameter-ref param", lib = "(rnrs parameters bridge)")]
pub fn parameter_ref_bridge(
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [param_val] = args else {
        return Err(Exception::wrong_num_of_args(1, args.len()));
    };
    let param: Embedded<Parameter> = param_val.try_into()?;
    let val = parameter_ref(&param);
    Ok(barrier.call_cont(vec![val]))
}

#[cps_bridge(def = "%parameter-set! param val", lib = "(rnrs parameters bridge)")]
pub fn parameter_set_bridge(
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [param_val, new_val] = args else {
        return Err(Exception::wrong_num_of_args(2, args.len()));
    };
    let param: Embedded<Parameter> = param_val.try_into()?;
    parameter_set(&param, new_val.clone());
    Ok(barrier.call_cont(Vec::new()))
}

/// Runs `thunk` with `params` rebound to `vals` for its dynamic extent: a
/// fresh cell per parameter is pushed as a `Parameterization` entry, popped
/// again (uncovering the outer bindings) once `thunk` returns.
#[cps_bridge(
    def = "%call-with-parameterization params vals thunk",
    lib = "(rnrs parameters bridge)"
)]
pub fn call_with_parameterization(
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [params, vals, thunk] = args else {
        return Err(Exception::wrong_num_of_args(3, args.len()));
    };
    let mut params_vec = Vec::new();
    list_to_vec(params, &mut params_vec);
    let mut vals_vec = Vec::new();
    list_to_vec(vals, &mut vals_vec);
    if params_vec.len() != vals_vec.len() {
        return Err(Exception::error(
            "parameterize: parameter/value length mismatch",
        ));
    }
    let cells = params_vec
        .iter()
        .zip(vals_vec)
        .map(|(p, v)| {
            let param: Embedded<Parameter> = p.try_into()?;
            Ok((param.id(), Cell::new(v)))
        })
        .collect::<Result<HashMap<_, _>, Exception>>()?;

    push_dyn_stack(DynStackElem::Parameterization(cells));

    let thunk: Procedure = thunk.clone().try_into()?;
    let (req_args, var) = barrier.cont_formals();
    barrier.push_cont([], ContPtr::Continuation(pop_dyn_stack_cont), req_args, var);
    Ok(Application::new(thunk, Vec::new()))
}

#[bridge(name = "%parameter-converter", lib = "(rnrs parameters bridge)")]
pub fn parameter_converter_bridge(param_val: &Value) -> Result<Vec<Value>, Exception> {
    let param: Embedded<Parameter> = param_val.try_into()?;
    Ok(vec![param.converter()])
}

#[bridge(name = "parameter?", lib = "(rnrs parameters bridge)")]
pub fn is_parameter(val: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(val.is_a::<Embedded<Parameter>>())])
}
