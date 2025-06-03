//! Rudimentary structure support.

use std::{cell::LazyCell, sync::Arc};

use by_address::ByAddress;
use futures::future::BoxFuture;
use indexmap::IndexMap;

use crate::{
    ast::ParseAstError,
    cps::{ClosureArgs, Cps, PrimOp, Value as CpsValue},
    env::Local,
    exception::{Condition, ExceptionHandler},
    gc::{Gc, Trace},
    proc::{Application, Closure, DynamicWind},
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
const RECORD_TYPE_RT: LazyCell<Arc<RecordType>> = LazyCell::new(|| {
    Arc::new(RecordType {
        name: "rt".to_string(),
        sealed: true,
        opaque: true,
        inherits: indexmap::IndexSet::new(),
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
    let sealed = sealed.is_true();
    let opaque = opaque.is_true();
    let fields: Gc<vectors::AlignedVector<Value>> = fields.clone().try_into()?;
    Ok(vec![Value::from(Arc::new(RecordType {
        name: name.to_string(),
        sealed,
        opaque,
        inherits,
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

pub fn is_subtype_of(val: &Value, rt: &Value) -> bool {
    let UnpackedValue::Record(rec) = val.clone().unpack() else {
        return false;
    };
    let rec_read = rec.read();
    let rt: Arc<RecordType> = rt.clone().try_into().unwrap();
    Arc::ptr_eq(&rec_read.record_type, &rt)
        || rec_read.record_type.inherits.contains(&ByAddress::from(rt))
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
        let [rtd] = args else { unreachable!() };
        let rtd: Arc<RecordType> = rtd.clone().try_into()?;
        let arg = Local::gensym();
        let k = Local::gensym();
        let pred = Local::gensym();
        let pred_fn = Cps::Closure {
            args: ClosureArgs::new(vec![arg], false, Some(k)),
            body: Box::new(Cps::PrimOp(
                PrimOp::IsSubType,
                vec![CpsValue::from(arg), CpsValue::from(Value::from(rtd))],
                pred,
                Box::new(Cps::App(
                    CpsValue::from(k),
                    vec![CpsValue::from(pred)],
                    None,
                )),
            )),
            val: pred,
            cexp: Box::new(Cps::Halt(CpsValue::from(pred))),
            debug: None,
        };
        // Use the runtime from the continuation to compile the pred function
        let runtime = cont.read().runtime.clone();
        let compiled = runtime
            .compile_expr_with_env(pred_fn, IndexMap::default())
            .await
            .unwrap();
        let pred_fn = compiled.call(&[]).await?[0].clone();
        let app = Application::new(
            cont,
            vec![pred_fn],
            exception_handler.clone(),
            dynamic_wind.clone(),
            None,
        );
        Ok(app)
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

#[derive(Debug, Trace, Clone)]
#[repr(align(16))]
pub struct Record {
    record_type: Arc<RecordType>,
    fields: Vec<Value>,
}
