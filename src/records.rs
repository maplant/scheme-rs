//! Rudimentary structure support.

use std::{cell::LazyCell, sync::Arc};

use by_address::ByAddress;
use futures::future::BoxFuture;

use crate::{
    ast::ParseAstError,
    exception::{Condition, ExceptionHandler},
    gc::{Gc, Trace},
    num::Number,
    proc::{Application, Closure, DynamicWind, FuncPtr},
    registry::{BridgeFn, BridgeFnDebugInfo, bridge},
    syntax::{Identifier, Span, Syntax},
    value::{UnpackedValue, Value, ValueType},
    vectors,
};

/// Type declaration for a record.
#[derive(Debug, Trace, Clone)]
#[repr(align(16))]
pub struct RecordType {
    name: String, // Make Arc<AlignedString>?
    sealed: bool,
    opaque: bool,
    /// Parent is most recently inserted record type, if one exists.
    inherits: indexmap::IndexSet<ByAddress<Arc<RecordType>>>,
    field_index_offset: usize,
    fields: Vec<Field>,
}

#[derive(Debug, Trace, Clone)]
pub enum Field {
    Immutable(Identifier),
    Mutable(Identifier),
}

impl Field {
    fn parse(field: &Syntax, span: &Span) -> Result<Self, ParseAstError> {
        match field.as_list() {
            Some(
                [
                    Syntax::Identifier {
                        ident: mutability, ..
                    },
                    Syntax::Identifier {
                        ident: field_name, ..
                    },
                    Syntax::Null { .. },
                ],
            ) => match mutability.name.as_str() {
                "mutable" => Ok(Field::Mutable(field_name.clone())),
                "immutable" => Ok(Field::Immutable(field_name.clone())),
                _ => Err(ParseAstError::BadForm(span.clone())),
            },
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }

    fn parse_fields(fields: &Syntax) -> Result<Vec<Self>, ParseAstError> {
        let span = fields.span();
        if let Some([fields @ .., Syntax::Null { .. }]) = fields.as_list() {
            fields
                .iter()
                .map(|field| Self::parse(field, span))
                .collect()
        } else {
            Err(ParseAstError::BadForm(span.clone()))
        }
    }
}

/// The record type for the "record type" type.
pub const RECORD_TYPE_RT: LazyCell<Arc<RecordType>> = LazyCell::new(|| {
    Arc::new(RecordType {
        name: "rt".to_string(),
        sealed: true,
        opaque: true,
        inherits: indexmap::IndexSet::new(),
        field_index_offset: 0,
        fields: vec![],
    })
});

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
    pub(crate) record_type: Arc<RecordType>,
    pub(crate) fields: Vec<Value>,
}

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
    let parent: Option<Arc<RecordType>> = parent
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
    let fields: Gc<vectors::AlignedVector<Value>> = fields.clone().try_into()?;
    Ok(vec![Value::from(Arc::new(RecordType {
        name: name.to_string(),
        sealed,
        opaque,
        inherits,
        field_index_offset,
        fields: vec![],
        // fields: Field::parse_fields(&fields)?,
    }))])
}

#[bridge(name = "record-type-descriptor?", lib = "(base)")]
pub async fn record_type_descriptor_pred(obj: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(obj.type_of() == ValueType::RecordType)])
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
    let rt: Arc<RecordType> = rt.clone().try_into()?;
    Ok(Arc::ptr_eq(&rec_read.record_type, &rt)
        || rec_read.record_type.inherits.contains(&ByAddress::from(rt)))
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
        let rtd: Arc<RecordType> = rtd.clone().try_into()?;
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
        let rtd: Arc<RecordType> = rtd.clone().try_into()?;
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
