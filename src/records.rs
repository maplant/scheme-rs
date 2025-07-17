//! Rudimentary structure support.

use std::{
    any::Any,
    sync::{Arc, LazyLock},
};

use by_address::ByAddress;
use futures::future::BoxFuture;

use crate::{
    exception::{Condition, ExceptionHandler},
    gc::{Gc, GcInner, Trace},
    num::Number,
    proc::{Application, Closure, DynamicWind, FuncPtr},
    registry::{BridgeFn, BridgeFnDebugInfo, bridge},
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    value::{UnpackedValue, Value, ValueType},
    vectors,
};

/// Type declaration for a record.
#[derive(Debug, Trace, Clone)]
#[repr(align(16))]
pub struct RecordTypeDescriptor {
    name: String, // Make Arc<AlignedString>?
    sealed: bool,
    opaque: bool,
    /// Parent is most recently inserted record type, if one exists.
    inherits: indexmap::IndexSet<ByAddress<Arc<RecordTypeDescriptor>>>,
    field_index_offset: usize,
    fields: Vec<Field>,
}

impl RecordTypeDescriptor {
    pub fn is_base_record_type(&self) -> bool {
        self.inherits.is_empty()
    }
}

#[derive(Debug, Trace, Clone)]
pub enum Field {
    Immutable(Symbol),
    Mutable(Symbol),
}

impl Field {
    fn parse(field: &Value) -> Result<Self, Condition> {
        let (mutability, field_name) = field.clone().try_into()?;
        let mutability: Symbol = mutability.try_into()?;
        let (field_name, _) = field_name.clone().try_into()?;
        let field_name: Symbol = field_name.try_into()?;
        match &*mutability.to_str() {
            "mutable" => Ok(Field::Mutable(field_name)),
            "immutable" => Ok(Field::Immutable(field_name)),
            _ => Err(Condition::Error),
        }
    }

    fn parse_fields(fields: &Value) -> Result<Vec<Self>, Condition> {
        let fields: Gc<vectors::AlignedVector<Value>> = fields.clone().try_into()?;
        fields.read().iter().map(Self::parse).collect()
    }
}

/// The record type descriptor for the "record type descriptor" type.
pub static RECORD_TYPE_DESCRIPTOR_RTD: LazyLock<Arc<RecordTypeDescriptor>> = LazyLock::new(|| {
    Arc::new(RecordTypeDescriptor {
        name: "rt".to_string(),
        sealed: true,
        opaque: true,
        inherits: indexmap::IndexSet::new(),
        field_index_offset: 0,
        fields: vec![],
    })
});

#[bridge(name = "make-record-type-descriptor", lib = "(base)")]
pub async fn make_record_type_descriptor(
    name: &Value,
    parent: &Value,
    _uid: &Value,
    sealed: &Value,
    opaque: &Value,
    fields: &Value,
) -> Result<Vec<Value>, Condition> {
    let name: Symbol = name.clone().try_into()?;
    let parent: Option<Arc<RecordTypeDescriptor>> = parent
        .is_true()
        .then(|| parent.clone().try_into())
        .transpose()?;
    let inherits = if let Some(parent) = parent {
        let mut inherits = parent.inherits.clone();
        inherits.insert(ByAddress(parent));
        inherits
    } else {
        indexmap::IndexSet::new()
    };
    let field_index_offset = inherits.last().map_or(0, |last_parent| {
        last_parent.field_index_offset + last_parent.fields.len()
    });
    let sealed = sealed.is_true();
    let opaque = opaque.is_true();
    let fields = Field::parse_fields(fields)?;
    Ok(vec![Value::from(Arc::new(RecordTypeDescriptor {
        name: name.to_string(),
        sealed,
        opaque,
        inherits,
        field_index_offset,
        fields,
    }))])
}

#[bridge(name = "record-type-descriptor?", lib = "(base)")]
pub async fn record_type_descriptor_pred(obj: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(
        obj.type_of() == ValueType::RecordTypeDescriptor,
    )])
}

#[derive(Trace, Clone)]
pub struct RecordConstructorDescriptor {
    parent: Option<Gc<RecordConstructorDescriptor>>,
    rtd: Arc<RecordTypeDescriptor>,
    protocol: Gc<Closure>,
}

fn make_default_record_constructor_descriptor(
    runtime: Runtime,
    rtd: Arc<RecordTypeDescriptor>,
) -> Gc<RecordConstructorDescriptor> {
    let parent = rtd.inherits.last().map(|parent| {
        make_default_record_constructor_descriptor(runtime.clone(), parent.0.clone())
    });
    let protocol = Gc::new(Closure::new(
        runtime,
        vec![Gc::new(Value::from(rtd.clone()))],
        Vec::new(),
        FuncPtr::Bridge(default_protocol),
        1,
        false,
        None,
    ));
    Gc::new(RecordConstructorDescriptor {
        parent,
        rtd,
        protocol,
    })
}

pub fn make_record_constructor_descriptor<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    _env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let [rtd, parent_rcd, protocol] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };

        let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
        let parent_rcd = if parent_rcd.is_true() {
            let Some(parent_rtd) = rtd.inherits.last() else {
                return Err(Condition::error("RTD is a base type".to_string()).into());
            };
            let any: Gc<Gc<dyn Any>> = parent_rcd.clone().try_into()?;
            let parent_rcd: Gc<RecordConstructorDescriptor> = any
                .read()
                .clone()
                .downcast()
                .map_err(|_| Condition::Error)?;
            if !Arc::ptr_eq(&parent_rcd.read().rtd, parent_rtd) {
                return Err(
                    Condition::error("Parent RTD does not match parent RCD".to_string()).into(),
                );
            }
            Some(parent_rcd)
        } else if !rtd.is_base_record_type() {
            Some(make_default_record_constructor_descriptor(
                cont.read().runtime.clone(),
                rtd.inherits.last().unwrap().clone().0,
            ))
        } else {
            None
        };

        let protocol = if protocol.is_true() {
            protocol.clone().try_into()?
        } else {
            Gc::new(Closure::new(
                cont.read().runtime.clone(),
                vec![Gc::new(Value::from(rtd.clone()))],
                Vec::new(),
                FuncPtr::Bridge(default_protocol),
                1,
                false,
                None,
            ))
        };

        let rcd = RecordConstructorDescriptor {
            parent: parent_rcd,
            rtd,
            protocol,
        };

        Ok(Application::new(
            cont,
            vec![Value::from(Gc::new(Gc::into_any(Gc::new(rcd))))],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

inventory::submit! {
    BridgeFn::new(
        "make-record-constructor-descriptor",
        "(base)",
        3,
        false,
        make_record_constructor_descriptor,
        BridgeFnDebugInfo::new(
            "records.rs",
            0,
            0,
            0,
            &[ "rtd", "parent-constructor-descriptor", "protocol" ],
        )
    )
}

pub fn record_constructor<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    _env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let [rcd] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };
        let rcd = {
            let any: Gc<Gc<dyn Any>> = rcd.clone().try_into()?;
            any.read()
                .clone()
                .downcast()
                .map_err(|_| Condition::Error)?
        };

        let (protocols, rtds) = rcd_to_protocols_and_rtds(&rcd);

        let chain_protocols = Value::from(Closure::new(
            cont.read().runtime.clone(),
            vec![
                Gc::new(Value::from(protocols)),
                Gc::new(Value::from(cont.clone())),
            ],
            Vec::new(),
            FuncPtr::Continuation(chain_protocols),
            1,
            false,
            None,
        ));

        chain_constructors(
            &[],
            &[],
            &chain_protocols,
            &[Gc::new(Value::from(rtds))],
            exception_handler,
            dynamic_wind,
        )
        .await
    })
}

inventory::submit! {
    BridgeFn::new(
        "record-constructor",
        "(base)",
        1,
        false,
        record_constructor,
        BridgeFnDebugInfo::new(
            "records.rs",
            0,
            0,
            0,
            &[ "rcd" ],
        )
    )
}

fn rcd_to_protocols_and_rtds(rcd: &Gc<RecordConstructorDescriptor>) -> (Vec<Value>, Vec<Value>) {
    let rcd = rcd.read();
    let (mut protocols, mut rtds) = if let Some(ref parent) = rcd.parent {
        rcd_to_protocols_and_rtds(parent)
    } else {
        (Vec::new(), Vec::new())
    };
    protocols.push(Value::from(rcd.protocol.clone()));
    rtds.push(Value::from(rcd.rtd.clone()));
    (protocols, rtds)
}

pub(crate) unsafe extern "C" fn chain_protocols(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    unsafe {
        // env[0] is a vector of protocols
        let protocols: Gc<vectors::AlignedVector<Value>> = Gc::from_raw_inc_rc(env.read())
            .read()
            .clone()
            .try_into()
            .unwrap();
        // env[1] is k, the continuation
        let k = Gc::from_raw_inc_rc(env.add(1).read());

        let mut protocols = protocols.read().clone();
        let remaining_protocols = protocols.split_off(1);
        let curr_protocol: Gc<Closure> = protocols[0].clone().try_into().unwrap();

        // If there are no more remaining protocols after the current, call the
        // protocol with arg[0] and the continuation.
        if remaining_protocols.is_empty() {
            return Box::into_raw(Box::new(Ok(Application::new(
                curr_protocol,
                vec![args.as_ref().unwrap().clone(), k.read().clone()],
                ExceptionHandler::from_ptr(exception_handler),
                dynamic_wind.as_ref().unwrap().clone(),
                None,
            ))));
        }

        // Otherwise, turn the remaining chain into the continuation:
        let new_k = Closure::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![Gc::new(Value::from(remaining_protocols)), k],
            Vec::new(),
            FuncPtr::Continuation(chain_protocols),
            1,
            false,
            None,
        );

        Box::into_raw(Box::new(Ok(Application::new(
            curr_protocol,
            vec![args.as_ref().unwrap().clone(), Value::from(new_k)],
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            None,
        ))))
    }
}

pub fn chain_constructors<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        // env[0] is a vector of RTDs
        let rtds: Gc<vectors::AlignedVector<Value>> = env[0].read().clone().try_into()?;
        let mut rtds = rtds.read().clone();
        let remaining_rtds = rtds.split_off(1);
        let curr_rtd: Arc<RecordTypeDescriptor> = rtds[0].clone().try_into()?;
        let rtds_remain = !remaining_rtds.is_empty();
        let num_args = curr_rtd.fields.len();
        let env = if rtds_remain {
            Some(Gc::new(Value::from(remaining_rtds)))
        } else {
            Some(Gc::new(Value::from(curr_rtd)))
        }
        .into_iter()
        // Chain the current environment:
        .chain(env[1..].iter().cloned())
        // Chain the arguments passed to this function:
        .chain(args.iter().cloned().map(Gc::new))
        .collect::<Vec<_>>();
        let next_closure = Closure::new(
            cont.read().runtime.clone(),
            env,
            Vec::new(),
            if rtds_remain {
                FuncPtr::Bridge(chain_constructors)
            } else {
                FuncPtr::Bridge(constructor)
            },
            num_args,
            false,
            None,
        );
        Ok(Application::new(
            cont,
            vec![Value::from(Gc::new(next_closure))],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

pub fn constructor<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let rtd: Arc<RecordTypeDescriptor> = env[0].read().clone().try_into()?;
        // The fields of the record are all of the env variables chained with
        // the arguments to this function.
        let fields = env[1..]
            .iter()
            .map(|var| var.read().clone())
            .chain(args.iter().cloned())
            .collect::<Vec<_>>();
        let record = Value::from(Gc::new(Record { rtd, fields }));
        Ok(Application::new(
            cont,
            vec![record],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

pub fn default_protocol<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let rtd: Arc<RecordTypeDescriptor> = env[0].read().clone().try_into()?;
        let num_args = rtd.field_index_offset + rtd.fields.len();

        let constructor = Closure::new(
            cont.read().runtime.clone(),
            vec![Gc::new(args[0].clone()), Gc::new(Value::from(rtd))],
            Vec::new(),
            FuncPtr::Bridge(default_protocol_constructor),
            num_args,
            false,
            None,
        );

        Ok(Application::new(
            cont.clone(),
            vec![Value::from(Gc::new(constructor))],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

pub fn default_protocol_constructor<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let constructor: Gc<Closure> = env[0].read().clone().try_into()?;
        let rtd: Arc<RecordTypeDescriptor> = env[1].read().clone().try_into()?;
        let mut args = args.to_vec();

        let cont = if let Some(parent) = rtd.inherits.last() {
            let remaining = args.split_off(parent.field_index_offset + parent.fields.len());
            let runtime = { cont.read().runtime.clone() };
            Value::from(Closure::new(
                runtime,
                vec![Gc::new(Value::from(remaining)), Gc::new(Value::from(cont))],
                Vec::new(),
                FuncPtr::Continuation(call_constructor_continuation),
                1,
                false,
                None,
            ))
        } else {
            Value::from(cont)
        };

        args.push(cont.clone());
        Ok(Application::new(
            constructor,
            args,
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

pub(crate) unsafe extern "C" fn call_constructor_continuation(
    _runtime: *mut GcInner<RuntimeInner>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    unsafe {
        let constructor: Gc<Closure> = args.as_ref().unwrap().clone().try_into().unwrap();
        let args: Gc<vectors::AlignedVector<Value>> = Gc::from_raw_inc_rc(env.read())
            .read()
            .clone()
            .try_into()
            .unwrap();
        let mut args = args.read().clone();
        let cont = Gc::from_raw_inc_rc(env.add(1).read()).read().clone();
        args.push(cont);

        // Call the constructor
        Box::into_raw(Box::new(Ok(Application::new(
            constructor,
            args,
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            None,
        ))))
    }
}

#[derive(Debug, Trace, Clone)]
#[repr(align(16))]
pub struct Record {
    // Possibly need the following:
    // pub(crate) opaque_parent: Option<Gc<dyn Any>>,
    pub(crate) rtd: Arc<RecordTypeDescriptor>,
    pub(crate) fields: Vec<Value>,
}

pub fn is_subtype_of(val: &Value, rt: &Value) -> Result<bool, Condition> {
    let UnpackedValue::Record(rec) = val.clone().unpack() else {
        return Ok(false);
    };
    let rec_read = rec.read();
    let rt: Arc<RecordTypeDescriptor> = rt.clone().try_into()?;
    Ok(Arc::ptr_eq(&rec_read.rtd, &rt) || rec_read.rtd.inherits.contains(&ByAddress::from(rt)))
}

fn record_predicate_fn<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let [val] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };
        // RTD is the first environment variable:
        Ok(Application::new(
            cont,
            vec![Value::from(is_subtype_of(val, &env[0].read())?)],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

pub fn record_predicate<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    _env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let [rtd] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };
        // TODO: Check if RTD is a record type.
        let pred_fn = Closure::new(
            cont.read().runtime.clone(),
            vec![Gc::new(rtd.clone())],
            Vec::new(),
            FuncPtr::Bridge(record_predicate_fn),
            1,
            false,
            None,
        );
        Ok(Application::new(
            cont,
            vec![Value::from(pred_fn)],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

inventory::submit! {
    BridgeFn::new(
        "record-predicate",
        "(base)",
        1,
        false,
        record_predicate,
        BridgeFnDebugInfo::new(
            "records.rs",
            0,
            0,
            0,
            &[ "rtd" ],
        )
    )
}

fn record_accessor_fn<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let [val] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };
        let record: Gc<Record> = val.clone().try_into()?;
        // RTD is the first environment variable, field index is the second
        if !is_subtype_of(val, &env[0].read())? {
            return Err(Condition::error("not a child of this record type".to_string()).into());
        }
        let k: Arc<Number> = env[1].read().clone().try_into()?;
        let k: usize = k.as_ref().try_into().map_err(Condition::from)?;
        let val = record.read().fields[k].clone();
        Ok(Application::new(
            cont,
            vec![val],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

pub fn record_accessor<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    _env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let [rtd, k] = args else {
            return Err(Condition::wrong_num_of_args(2, args.len()).into());
        };
        let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
        let k: Arc<Number> = k.clone().try_into()?;
        let k: usize = k.as_ref().try_into().map_err(Condition::from)?;
        if k > rtd.fields.len() {
            return Err(Condition::Assertion.into());
        }
        let k = k + rtd.field_index_offset;
        let accessor_fn = Closure::new(
            cont.read().runtime.clone(),
            vec![
                Gc::new(Value::from(rtd)),
                Gc::new(Value::from(Number::from(k))),
            ],
            Vec::new(),
            FuncPtr::Bridge(record_accessor_fn),
            1,
            false,
            None,
        );
        Ok(Application::new(
            cont,
            vec![Value::from(accessor_fn)],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

inventory::submit! {
    BridgeFn::new(
        "record-accessor",
        "(base)",
        2,
        false,
        record_accessor,
        BridgeFnDebugInfo::new(
            "records.rs",
            0,
            0,
            0,
            &[ "rtd", "k" ],
        )
    )
}

fn record_mutator_fn<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let [rec, new_val] = args else {
            return Err(Condition::wrong_num_of_args(1, args.len()).into());
        };
        let record: Gc<Record> = rec.clone().try_into()?;
        // RTD is the first environment variable, field index is the second
        if !is_subtype_of(rec, &env[0].read())? {
            return Err(Condition::error("not a child of this record type".to_string()).into());
        }
        let k: Arc<Number> = env[1].read().clone().try_into()?;
        let k: usize = k.as_ref().try_into().map_err(Condition::from)?;
        record.write().fields[k] = new_val.clone();
        Ok(Application::new(
            cont,
            vec![],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

pub fn record_mutator<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    _env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async move {
        let cont: Gc<Closure> = cont.clone().try_into()?;
        let [rtd, k] = args else {
            return Err(Condition::wrong_num_of_args(2, args.len()).into());
        };
        let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
        let k: Arc<Number> = k.clone().try_into()?;
        let k: usize = k.as_ref().try_into().map_err(Condition::from)?;
        if k > rtd.fields.len() || matches!(rtd.fields[k], Field::Immutable(_)) {
            return Err(Condition::Assertion.into());
        }
        let k = k + rtd.field_index_offset;
        let mutator_fn = Closure::new(
            cont.read().runtime.clone(),
            vec![
                Gc::new(Value::from(rtd)),
                Gc::new(Value::from(Number::from(k))),
            ],
            Vec::new(),
            FuncPtr::Bridge(record_mutator_fn),
            2,
            false,
            None,
        );
        Ok(Application::new(
            cont,
            vec![Value::from(mutator_fn)],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        ))
    })
}

inventory::submit! {
    BridgeFn::new(
        "record-mutator",
        "(base)",
        2,
        false,
        record_mutator,
        BridgeFnDebugInfo::new(
            "records.rs",
            0,
            0,
            0,
            &[ "rtd", "k" ],
        )
    )
}
