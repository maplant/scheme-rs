//! Rudimentary structure support.

use std::{any::Any, cell::LazyCell, sync::Arc};

use by_address::ByAddress;
use futures::future::BoxFuture;

use crate::{
    exception::{Condition, ExceptionHandler},
    gc::{Gc, Trace},
    num::Number,
    proc::{Application, Closure, DynamicWind, FuncPtr},
    registry::{BridgeFn, BridgeFnDebugInfo, bridge},
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
    Immutable(String),
    Mutable(String),
}

impl Field {
    fn parse(field: &Value) -> Result<Self, Condition> {
        let (mutability, field_name) = field.clone().try_into()?;
        let mutability = mutability.try_into_sym()?;
        let field_name = field_name.try_into_sym()?;
        match mutability.as_str() {
            "mutable" => Ok(Field::Mutable(field_name.0.clone())),
            "immutable" => Ok(Field::Immutable(field_name.0.clone())),
            _ => Err(Condition::Error),
        }
    }

    fn parse_fields(fields: &Value) -> Result<Vec<Self>, Condition> {
        let fields: Gc<vectors::AlignedVector<Value>> = fields.clone().try_into()?;
        fields
            .read()
            .iter()
            .map(|field| Self::parse(field))
            .collect()
    }
}

/// The record type descriptor for the "record type descriptor" type.
pub const RECORD_TYPE_DESCRIPTOR_RTD: LazyCell<Arc<RecordTypeDescriptor>> = LazyCell::new(|| {
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
    let name = name.clone().try_into_sym()?;
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
    rtd: Arc<RecordTypeDescriptor>,
    protocol: Closure,
}

#[bridge(name = "make-record-constructor-descriptor", lib = "(base)")]
pub async fn make_record_constructor_descriptor(
    rtd: &Value,
    parent_constructor_descriptor: &Value,
    protocol: &Value,
) -> Result<Vec<Value>, Condition> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    if rtd.is_base_record_type() && protocol.is_true() {
        if parent_constructor_descriptor.is_true() {
            return Err(Condition::error(
                "parent-constructor-descriptor is not false with protocol and base record type"
                    .to_string(),
            ));
        }
        let protocol: Gc<Closure> = protocol.clone().try_into()?;
        /*
        let default_constructor = Gc::new(Closure::new(
            protocol.read().runtime.clone(),
            vec![Gc::new(Value::from(rtd.clone()))],
            vec![],
            FuncPtr::Bridge(default_constructor),
            0,
            false,
            None,
        ));
        */
        let constructor_type_descriptor = Gc::new(RecordConstructorDescriptor {
            rtd,
            protocol: protocol.read().clone(),
        });
        return Ok(vec![Value::from(Gc::new(Gc::into_any(
            constructor_type_descriptor,
        )))]);
    }
    todo!()
}

pub fn chain_protocols<'a>(
    args: &'a [Value],
    _rest_args: &'a [Value],
    cont: &'a Value,
    env: &'a [Gc<Value>],
    exception_handler: &'a Option<Gc<ExceptionHandler>>,
    dynamic_wind: &'a DynamicWind,
) -> BoxFuture<'a, Result<Application, Value>> {
    Box::pin(async { todo!() })
}

#[bridge(name = "record-constructor", lib = "(base)")]
pub async fn record_constructor(constructor_descriptor: &Value) -> Result<Vec<Value>, Condition> {
    let any: Gc<Gc<dyn Any>> = constructor_descriptor.clone().try_into()?;
    let rcd: Gc<RecordConstructorDescriptor> = any
        .read()
        .clone()
        .downcast()
        .map_err(|_| Condition::Error)?;

    todo!()
}

pub fn default_constructor<'a>(
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
        let num_args = rtd.field_index_offset + rtd.fields.len();
        if args.len() != num_args {
            return Err(Condition::wrong_num_of_args(num_args, args.len()).into());
        }
        let fields = args.iter().take(num_args).cloned().collect();
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

/*
impl RecordType {
    pub fn new(name: &str, parent: Option<&RecordType>, sealed: bool, opaque: bool) -> Self {
        Self {
            name: name.to_string(),
            inherits: indexmap::IndexSet::new(),
            fields: Vec::new(),
        }
    }
}
*/

#[derive(Debug, Trace, Clone)]
#[repr(align(16))]
pub struct Record {
    // Possibly need the following:
    // pub(crate) opaque_parent: Option<Gc<dyn Any>>,
    pub(crate) rtd: Arc<RecordTypeDescriptor>,
    pub(crate) fields: Vec<Value>,
}

/*
fn is_subtype_of(lhs: &Gc<RecordType>, rhs: &Gc<RecordType>) -> bool {
    lhs == rhs || {
        let lhs = lhs.read();
        lhs.inherits.contains(rhs)
    }
}
 */

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
            vec![Value::from(is_subtype_of(&val, &env[0].read())?)],
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
        if !is_subtype_of(&val, &env[0].read())? {
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
        1,
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
        if !is_subtype_of(&rec, &env[0].read())? {
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
        let k = k + rtd.field_index_offset;
        let accessor_fn = Closure::new(
            cont.read().runtime.clone(),
            vec![
                Gc::new(Value::from(rtd)),
                Gc::new(Value::from(Number::from(k))),
            ],
            Vec::new(),
            FuncPtr::Bridge(record_mutator_fn),
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
        "record-mutator",
        "(base)",
        1,
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
