//! Rudimentary structure support.

use std::{
    any::Any,
    collections::HashMap,
    fmt,
    mem::ManuallyDrop,
    ptr::NonNull,
    sync::{Arc, LazyLock, Mutex},
};

use by_address::ByAddress;

use crate::{
    exceptions::{Condition, ExceptionHandler, ExceptionHandlerInner},
    gc::{Gc, GcInner, Trace},
    num::Number,
    proc::{Application, DynamicWind, FuncPtr, Procedure},
    registry::{bridge, cps_bridge},
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    value::{UnpackedValue, Value, ValueType},
    vectors,
};

pub use scheme_rs_macros::rtd;

/// Type declaration for a record.
#[derive(Trace, Clone)]
#[repr(align(16))]
pub struct RecordTypeDescriptor {
    pub name: Symbol,
    pub sealed: bool,
    pub opaque: bool,
    pub uid: Option<Symbol>,
    pub rust_parent_constructor: Option<RustParentConstructor>,
    /// Parent is most recently inserted record type, if one exists.
    pub inherits: indexmap::IndexSet<ByAddress<Arc<RecordTypeDescriptor>>>,
    pub field_index_offset: usize,
    pub fields: Vec<Field>,
}

impl RecordTypeDescriptor {
    pub fn is_base_record_type(&self) -> bool {
        self.inherits.is_empty()
    }

    pub fn is_subtype_of(self: &Arc<Self>, rtd: &Arc<Self>) -> bool {
        Arc::ptr_eq(self, rtd) || self.inherits.contains(&ByAddress(rtd.clone()))
    }
}

impl fmt::Debug for RecordTypeDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "#<rtd name: {} sealed: {} opaque: {} rust: {} ",
            self.name,
            self.sealed,
            self.opaque,
            self.rust_parent_constructor.is_some()
        )?;
        if !self.inherits.is_empty() {
            let parent = self.inherits.last().unwrap();
            write!(f, "parent: {} ", parent.name)?;
        }
        write!(f, "fields: (")?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            field.fmt(f)?;
        }
        write!(f, ")>")?;
        Ok(())
    }
}

#[derive(Trace, Clone)]
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
            _ => Err(Condition::error(
                "mutability specifier must be mutable or immutable".to_string(),
            )),
        }
    }

    fn parse_fields(fields: &Value) -> Result<Vec<Self>, Condition> {
        let fields: Gc<vectors::AlignedVector<Value>> = fields.clone().try_into()?;
        fields.read().iter().map(Self::parse).collect()
    }

    fn name(&self) -> Symbol {
        match self {
            Self::Immutable(sym) | Self::Mutable(sym) => *sym,
        }
    }
}

impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Immutable(sym) => write!(f, "(immutable {sym})"),
            Self::Mutable(sym) => write!(f, "(mutable {sym})"),
        }
    }
}

/// The record type descriptor for the "record type descriptor" type.
pub static RECORD_TYPE_DESCRIPTOR_RTD: LazyLock<Arc<RecordTypeDescriptor>> = LazyLock::new(|| {
    Arc::new(RecordTypeDescriptor {
        name: Symbol::intern("rtd"),
        sealed: true,
        opaque: true,
        uid: None,
        rust_parent_constructor: None,
        inherits: indexmap::IndexSet::new(),
        field_index_offset: 0,
        fields: vec![],
    })
});

type NonGenerativeStore = LazyLock<Arc<Mutex<HashMap<Symbol, Arc<RecordTypeDescriptor>>>>>;

pub static NONGENERATIVE: NonGenerativeStore =
    LazyLock::new(|| Arc::new(Mutex::new(HashMap::new())));

#[bridge(
    name = "make-record-type-descriptor",
    lib = "(rnrs records procedural (6))"
)]
pub fn make_record_type_descriptor(
    name: &Value,
    parent: &Value,
    uid: &Value,
    sealed: &Value,
    opaque: &Value,
    fields: &Value,
) -> Result<Vec<Value>, Condition> {
    let uid: Option<Symbol> = if uid.is_true() {
        Some(uid.clone().try_into()?)
    } else {
        None
    };

    // If the record is non-generative, check to see if it has already been
    // instanciated.
    if let Some(ref uid) = uid
        && let Some(rtd) = NONGENERATIVE.lock().unwrap().get(uid)
    {
        return Ok(vec![Value::from(rtd.clone())]);
    }

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
    let rtd = Arc::new(RecordTypeDescriptor {
        name,
        sealed,
        opaque,
        uid,
        rust_parent_constructor: None,
        inherits,
        field_index_offset,
        fields,
    });

    if let Some(uid) = uid {
        NONGENERATIVE.lock().unwrap().insert(uid, rtd.clone());
    }

    Ok(vec![Value::from(rtd)])
}

#[bridge(
    name = "record-type-descriptor?",
    lib = "(rnrs records procedural (6))"
)]
pub fn record_type_descriptor_pred(obj: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(
        obj.type_of() == ValueType::RecordTypeDescriptor,
    )])
}

#[derive(Trace, Clone)]
pub struct RecordConstructorDescriptor {
    parent: Option<Gc<RecordConstructorDescriptor>>,
    rtd: Arc<RecordTypeDescriptor>,
    protocol: Procedure,
}

impl SchemeCompatible for RecordConstructorDescriptor {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "record-constructor-descriptor")
    }
}

impl fmt::Debug for RecordConstructorDescriptor {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

fn make_default_record_constructor_descriptor(
    runtime: Runtime,
    rtd: Arc<RecordTypeDescriptor>,
) -> Gc<RecordConstructorDescriptor> {
    let parent = rtd.inherits.last().map(|parent| {
        make_default_record_constructor_descriptor(runtime.clone(), parent.0.clone())
    });
    let protocol = Procedure::new(
        runtime,
        vec![Value::from(rtd.clone())],
        FuncPtr::SyncBridge(default_protocol),
        1,
        false,
        None,
    );
    Gc::new(RecordConstructorDescriptor {
        parent,
        rtd,
        protocol,
    })
}

#[cps_bridge(
    name = "make-record-constructor-descriptor",
    lib = "(rnrs records procedural (6))",
    args = "rtd parent-constructor-descriptor protocol"
)]
pub fn make_record_constructor_descriptor(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let [rtd, parent_rcd, protocol] = args else {
        unreachable!();
    };

    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    let parent_rcd = if parent_rcd.is_true() {
        let Some(parent_rtd) = rtd.inherits.last() else {
            return Err(Condition::error("RTD is a base type".to_string()));
        };
        let parent_rcd = parent_rcd.try_into_rust_type::<RecordConstructorDescriptor>()?;
        if !Arc::ptr_eq(&parent_rcd.read().rtd, parent_rtd) {
            return Err(Condition::error(
                "Parent RTD does not match parent RCD".to_string(),
            ));
        }
        Some(parent_rcd)
    } else if !rtd.is_base_record_type() {
        Some(make_default_record_constructor_descriptor(
            runtime.clone(),
            rtd.inherits.last().unwrap().clone().0,
        ))
    } else {
        None
    };

    let protocol = if protocol.is_true() {
        protocol.clone().try_into()?
    } else {
        Procedure::new(
            runtime.clone(),
            vec![Value::from(rtd.clone())],
            FuncPtr::SyncBridge(default_protocol),
            1,
            false,
            None,
        )
    };

    let rcd = RecordConstructorDescriptor {
        parent: parent_rcd,
        rtd,
        protocol,
    };

    Ok(Application::new(
        cont,
        vec![Value::from(Record::from_rust_type(rcd))],
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    ))
}

#[cps_bridge(
    name = "record-constructor",
    lib = "(rnrs records procedural (6))",
    args = "rcd"
)]
pub fn record_constructor(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let [rcd] = args else {
        unreachable!();
    };
    let rcd = rcd.try_into_rust_type::<RecordConstructorDescriptor>()?;

    let (protocols, rtds) = rcd_to_protocols_and_rtds(&rcd);

    // See if there is a rust contrustor available
    let rust_constructor = rtds
        .iter()
        .find(|rtd| rtd.rust_parent_constructor.is_some())
        .map_or_else(|| Value::from(false), |rtd| Value::from(rtd.clone()));

    let protocols = protocols.into_iter().map(Value::from).collect::<Vec<_>>();
    let rtds = rtds.into_iter().map(Value::from).collect::<Vec<_>>();
    let chain_protocols = Value::from(Procedure::new(
        runtime.clone(),
        vec![Value::from(protocols), Value::from(cont.clone())],
        FuncPtr::Continuation(chain_protocols),
        1,
        false,
        None,
    ));

    Ok(chain_constructors(
        runtime,
        &[Value::from(rtds), rust_constructor],
        &[],
        &[],
        &chain_protocols,
        exception_handler,
        dynamic_wind,
    ))
}

fn rcd_to_protocols_and_rtds(
    rcd: &Gc<RecordConstructorDescriptor>,
) -> (Vec<Procedure>, Vec<Arc<RecordTypeDescriptor>>) {
    let rcd = rcd.read();
    let (mut protocols, mut rtds) = if let Some(ref parent) = rcd.parent {
        rcd_to_protocols_and_rtds(parent)
    } else {
        (Vec::new(), Vec::new())
    };
    protocols.push(rcd.protocol.clone());
    rtds.push(rcd.rtd.clone());
    (protocols, rtds)
}

pub(crate) unsafe extern "C" fn chain_protocols(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] is a vector of protocols
        let protocols: Gc<vectors::AlignedVector<Value>> =
            env.as_ref().unwrap().clone().try_into().unwrap();
        // env[1] is k, the continuation
        let k = env.add(1).as_ref().unwrap().clone();

        let mut protocols = protocols.read().clone();
        let remaining_protocols = protocols.split_off(1);
        let curr_protocol: Procedure = protocols[0].clone().try_into().unwrap();

        // If there are no more remaining protocols after the current, call the
        // protocol with arg[0] and the continuation.
        if remaining_protocols.is_empty() {
            return Box::into_raw(Box::new(Application::new(
                curr_protocol,
                vec![args.as_ref().unwrap().clone(), k.clone()],
                ExceptionHandler::from_ptr(exception_handler),
                dynamic_wind.as_ref().unwrap().clone(),
                None,
            )));
        }

        // Otherwise, turn the remaining chain into the continuation:
        let new_k = Procedure::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![Value::from(remaining_protocols), k],
            FuncPtr::Continuation(chain_protocols),
            1,
            false,
            None,
        );

        Box::into_raw(Box::new(Application::new(
            curr_protocol,
            vec![args.as_ref().unwrap().clone(), Value::from(new_k)],
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            None,
        )))
    }
}

#[cps_bridge]
fn chain_constructors(
    runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    // env[0] is a vector of RTDs
    let rtds: Gc<vectors::AlignedVector<Value>> = env[0].clone().try_into()?;
    // env[1] is the possible rust constructor
    let rust_constructor = env[1].clone();
    let mut rtds = rtds.read().clone();
    let remaining_rtds = rtds.split_off(1);
    let curr_rtd: Arc<RecordTypeDescriptor> = rtds[0].clone().try_into()?;
    let rtds_remain = !remaining_rtds.is_empty();
    let num_args = curr_rtd.fields.len();
    let env = if rtds_remain {
        vec![Value::from(remaining_rtds), rust_constructor]
    } else {
        vec![Value::from(curr_rtd), rust_constructor]
    }
    .into_iter()
    // Chain the current environment:
    .chain(env[2..].iter().cloned())
    // Chain the arguments passed to this function:
    .chain(args.iter().cloned())
    .collect::<Vec<_>>();
    let next_proc = Procedure::new(
        runtime.clone(),
        env,
        if rtds_remain {
            FuncPtr::SyncBridge(chain_constructors)
        } else {
            FuncPtr::SyncBridge(constructor)
        },
        num_args,
        false,
        None,
    );
    Ok(Application::new(
        cont,
        vec![Value::from(next_proc)],
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    ))
}

#[cps_bridge]
fn constructor(
    _runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let rtd: Arc<RecordTypeDescriptor> = env[0].clone().try_into()?;
    // The fields of the record are all of the env variables chained with
    // the arguments to this function.
    let mut fields = env[2..]
        .iter()
        .cloned()
        .chain(args.iter().cloned())
        .collect::<Vec<_>>();
    // Check for a rust constructor
    let rust_constructor = env[1].clone();
    let (rust_parent, fields) = if rust_constructor.is_true() {
        let rust_rtd: Arc<RecordTypeDescriptor> = rust_constructor.try_into()?;
        let num_fields: usize = rust_rtd
            .inherits
            .iter()
            .map(|parent| parent.fields.len())
            .sum();
        let remaining_fields = fields.split_off(num_fields + rust_rtd.fields.len());
        (
            Some((rust_rtd.rust_parent_constructor.unwrap().constructor)(&fields)?),
            remaining_fields,
        )
    } else {
        (None, fields)
    };
    let record = Value::from(Record(Gc::new(RecordInner {
        rust_parent,
        rtd,
        fields,
    })));
    Ok(Application::new(
        cont,
        vec![record],
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    ))
}

#[cps_bridge]
fn default_protocol(
    runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let rtd: Arc<RecordTypeDescriptor> = env[0].clone().try_into()?;
    let num_args = rtd.field_index_offset + rtd.fields.len();

    let constructor = Procedure::new(
        runtime.clone(),
        vec![args[0].clone(), Value::from(rtd)],
        FuncPtr::SyncBridge(default_protocol_constructor),
        num_args,
        false,
        None,
    );

    Ok(Application::new(
        cont.clone(),
        vec![Value::from(constructor)],
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    ))
}

#[cps_bridge]
fn default_protocol_constructor(
    runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let constructor: Procedure = env[0].clone().try_into()?;
    let rtd: Arc<RecordTypeDescriptor> = env[1].clone().try_into()?;
    let mut args = args.to_vec();

    let cont = if let Some(parent) = rtd.inherits.last() {
        let remaining = args.split_off(parent.field_index_offset + parent.fields.len());
        Value::from(Procedure::new(
            runtime.clone(),
            vec![Value::from(remaining), Value::from(cont)],
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
}

pub(crate) unsafe extern "C" fn call_constructor_continuation(
    _runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        let constructor: Procedure = args.as_ref().unwrap().clone().try_into().unwrap();
        let args: Gc<vectors::AlignedVector<Value>> =
            env.as_ref().unwrap().clone().try_into().unwrap();
        let mut args = args.read().clone();
        let cont = env.add(1).as_ref().unwrap().clone();
        args.push(cont);

        // Call the constructor
        Box::into_raw(Box::new(Application::new(
            constructor,
            args,
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
            None,
        )))
    }
}

/// A Scheme record type. Effectively a tuple of a fixed size array and some type
/// information.
#[derive(Trace, Clone)]
pub struct Record(pub(crate) Gc<RecordInner>);

impl Record {
    /// Convert any Rust type that implements [SchemeCompatible] into an opaque
    /// record.
    pub fn from_rust_type<T: SchemeCompatible>(t: T) -> Self {
        let opaque_parent = Some(into_scheme_compatible(Gc::new(t)));
        let rtd = T::rtd();
        Self(Gc::new(RecordInner {
            rust_parent: opaque_parent,
            rtd,
            fields: Vec::new(),
        }))
    }

    /// Attempt to convert the record into a Rust type that implements
    /// [SchemeCompatible].
    pub fn try_into_rust_type<T: SchemeCompatible>(&self) -> Option<Gc<T>> {
        let this = self.0.read();
        let opaque_parent = this.rust_parent.as_ref()?;

        // Attempt to extract any embedded records
        let rtd = T::rtd();
        let mut t = opaque_parent.clone();
        while let Some(embedded) = { t.read().extract_embedded_record(&rtd) } {
            t = embedded;
        }

        let t = ManuallyDrop::new(t);

        // Second, convert the opaque_parent type into a Gc<dyn Any>
        let any: NonNull<GcInner<dyn Any>> = t.ptr;
        let gc_any = Gc {
            ptr: any,
            marker: std::marker::PhantomData,
        };

        // Then, convert that back into the desired type
        Gc::downcast::<T>(gc_any).ok()
    }
}

impl fmt::Debug for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.read().fmt(f)
    }
}

#[derive(Trace, Clone)]
#[repr(align(16))]
pub(crate) struct RecordInner {
    pub(crate) rust_parent: Option<Gc<dyn SchemeCompatible>>,
    pub(crate) rtd: Arc<RecordTypeDescriptor>,
    pub(crate) fields: Vec<Value>,
}

impl fmt::Debug for RecordInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<{}", self.rtd.name)?;
        if let Some(parent) = &self.rust_parent {
            write!(f, "{parent:?}")?;
        }
        let mut field_names = self
            .rtd
            .inherits
            .iter()
            .cloned()
            .chain(Some(ByAddress(self.rtd.clone())))
            .flat_map(|rtd| rtd.fields.clone());
        for field in &self.fields {
            let name = field_names.next().unwrap().name();
            write!(f, " {name}: {field:?}")?;
        }
        write!(f, ">")
    }
}

/// A Rust value that can present itself as a Scheme record.
pub trait SchemeCompatible: fmt::Debug + Trace + Any {
    /// The Record Type Descriptor of the value. Can be constructed at runtime,
    /// but cannot change.
    fn rtd() -> Arc<RecordTypeDescriptor>
    where
        Self: Sized;

    /// Extract the embedded record type with the matching record type
    /// descriptor if it exists.
    fn extract_embedded_record(
        &self,
        _rtd: &Arc<RecordTypeDescriptor>,
    ) -> Option<Gc<dyn SchemeCompatible>> {
        None
    }

    /// Fetch the kth field of the record.
    fn get_field(&self, k: usize) -> Value {
        panic!("{k} is out of bounds")
    }

    /// Set the kth field of the record.
    fn set_field(&mut self, k: usize, _val: Value) {
        panic!("{k} is out of bounds")
    }
}

pub fn into_scheme_compatible(t: Gc<impl SchemeCompatible>) -> Gc<dyn SchemeCompatible> {
    // Convert t into a Gc<dyn SchemeCompatible>. This has to be done
    // manually since [CoerceUnsized] is unstable.
    let t = ManuallyDrop::new(t);
    let any: NonNull<GcInner<dyn SchemeCompatible>> = t.ptr;
    Gc {
        ptr: any,
        marker: std::marker::PhantomData,
    }
}

#[derive(Copy, Clone, Debug)]
pub struct RustParentConstructor {
    constructor: ParentConstructor,
}

impl RustParentConstructor {
    pub fn new(constructor: ParentConstructor) -> Self {
        Self { constructor }
    }
}

type ParentConstructor =
    fn(&[Value]) -> Result<Gc<dyn SchemeCompatible>, Condition>;

unsafe impl Trace for RustParentConstructor {
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(crate::gc::OpaqueGcPtr)) {}
}

pub fn is_subtype_of(val: &Value, rt: &Value) -> Result<bool, Condition> {
    let UnpackedValue::Record(rec) = val.clone().unpack() else {
        return Ok(false);
    };
    let rec_read = rec.0.read();
    let rt: Arc<RecordTypeDescriptor> = rt.clone().try_into()?;
    Ok(Arc::ptr_eq(&rec_read.rtd, &rt) || rec_read.rtd.inherits.contains(&ByAddress::from(rt)))
}

#[cps_bridge]
fn record_predicate_fn(
    _runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let [val] = args else {
        unreachable!();
    };
    // RTD is the first environment variable:
    Ok(Application::new(
        cont,
        vec![Value::from(is_subtype_of(val, &env[0])?)],
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    ))
}

#[cps_bridge(
    name = "record-predicate",
    lib = "(rnrs records procedural (6))",
    args = "rtd"
)]
pub fn record_predicate(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let [rtd] = args else {
        unreachable!();
    };
    // TODO: Check if RTD is a record type.
    let pred_fn = Procedure::new(
        runtime.clone(),
        vec![rtd.clone()],
        FuncPtr::SyncBridge(record_predicate_fn),
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
}

#[cps_bridge]
fn record_accessor_fn(
    _runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let [val] = args else {
        unreachable!();
    };
    let record: Record = val.clone().try_into()?;
    // RTD is the first environment variable, field index is the second
    if !is_subtype_of(val, &env[0])? {
        return Err(Condition::error(
            "not a child of this record type".to_string(),
        ));
    }
    let k: Arc<Number> = env[1].clone().try_into()?;
    let k: usize = k.as_ref().try_into().map_err(Condition::from)?;
    let val = record.0.read().fields[k].clone();
    Ok(Application::new(
        cont,
        vec![val],
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    ))
}

#[cps_bridge(
    name = "record-accessor",
    lib = "(rnrs records procedural (6))",
    args = "rtd k"
)]
pub fn record_accessor(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let [rtd, k] = args else {
        unreachable!();
    };
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    let k: Arc<Number> = k.clone().try_into()?;
    let k: usize = k.as_ref().try_into().map_err(Condition::from)?;
    if k > rtd.fields.len() {
        return Err(Condition::error(format!(
            "{k} is out of range {}",
            rtd.fields.len()
        )));
    }
    let k = k + rtd.field_index_offset;
    let accessor_fn = Procedure::new(
        runtime.clone(),
        vec![Value::from(rtd), Value::from(Number::from(k))],
        FuncPtr::SyncBridge(record_accessor_fn),
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
}

#[cps_bridge]
fn record_mutator_fn(
    _runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let [rec, new_val] = args else {
        unreachable!();
    };
    let record: Record = rec.clone().try_into()?;
    // RTD is the first environment variable, field index is the second
    if !is_subtype_of(rec, &env[0])? {
        return Err(Condition::error(
            "not a child of this record type".to_string(),
        ));
    }
    let k: Arc<Number> = env[1].clone().try_into()?;
    let k: usize = k.as_ref().try_into().map_err(Condition::from)?;
    record.0.write().fields[k] = new_val.clone();
    Ok(Application::new(
        cont,
        vec![],
        exception_handler.clone(),
        dynamic_wind.clone(),
        None,
    ))
}

#[cps_bridge(
    name = "record-mutator",
    lib = "(rnrs records procedural (6))",
    args = "rtd k"
)]
pub fn record_mutator(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let cont: Procedure = cont.clone().try_into()?;
    let [rtd, k] = args else {
        unreachable!();
    };
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    let k: Arc<Number> = k.clone().try_into()?;
    let k: usize = k.as_ref().try_into().map_err(Condition::from)?;
    if k > rtd.fields.len() {
        return Err(Condition::error(format!(
            "{k} is out of range {}",
            rtd.fields.len()
        )));
    }
    if matches!(rtd.fields[k], Field::Immutable(_)) {
        return Err(Condition::error(format!("{k} is immutable")));
    }
    let k = k + rtd.field_index_offset;
    let mutator_fn = Procedure::new(
        runtime.clone(),
        vec![Value::from(rtd), Value::from(Number::from(k))],
        FuncPtr::SyncBridge(record_mutator_fn),
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
}

// Inspection library:

#[bridge(name = "record?", lib = "(rnrs records inspection (6))")]
pub fn record_pred(obj: &Value) -> Result<Vec<Value>, Condition> {
    match &*obj.unpacked_ref() {
        UnpackedValue::Record(rec) => Ok(vec![Value::from(!rec.0.read().rtd.opaque)]),
        _ => Ok(vec![Value::from(false)]),
    }
}

#[bridge(name = "record-rtd", lib = "(rnrs records inspection (6))")]
pub fn record_rtd(record: &Value) -> Result<Vec<Value>, Condition> {
    match &*record.unpacked_ref() {
        UnpackedValue::Record(rec) if !rec.0.read().rtd.opaque => {
            Ok(vec![Value::from(rec.0.read().rtd.clone())])
        }
        _ => Err(Condition::error(
            "expected a non-opaque record type".to_string(),
        )),
    }
}

#[bridge(name = "record-type-name", lib = "(rnrs records inspection (6))")]
pub fn record_type_name(rtd: &Value) -> Result<Vec<Value>, Condition> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    Ok(vec![Value::from(rtd.name)])
}

#[bridge(name = "record-type-parent", lib = "(rnrs records inspection (6))")]
pub fn record_type_parent(rtd: &Value) -> Result<Vec<Value>, Condition> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    if let Some(parent) = rtd.inherits.last() {
        Ok(vec![Value::from(parent.0.clone())])
    } else {
        Ok(vec![Value::from(false)])
    }
}

#[bridge(name = "record-type-uid", lib = "(rnrs records inspection (6))")]
pub fn record_type_uid(rtd: &Value) -> Result<Vec<Value>, Condition> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    if let Some(uid) = rtd.uid {
        Ok(vec![Value::from(uid)])
    } else {
        Ok(vec![Value::from(false)])
    }
}

#[bridge(
    name = "record-type-generative?",
    lib = "(rnrs records inspection (6))"
)]
pub fn record_type_generative_pred(rtd: &Value) -> Result<Vec<Value>, Condition> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    Ok(vec![Value::from(rtd.uid.is_none())])
}

#[bridge(name = "record-type-sealed?", lib = "(rnrs records inspection (6))")]
pub fn record_type_sealed_pred(rtd: &Value) -> Result<Vec<Value>, Condition> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    Ok(vec![Value::from(rtd.sealed)])
}

#[bridge(name = "record-type-opaque?", lib = "(rnrs records inspection (6))")]
pub fn record_type_opaque_pred(rtd: &Value) -> Result<Vec<Value>, Condition> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    Ok(vec![Value::from(rtd.opaque)])
}

#[bridge(
    name = "record-type-field-names",
    lib = "(rnrs records inspection (6))"
)]
pub fn record_type_field_names(rtd: &Value) -> Result<Vec<Value>, Condition> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    let fields = rtd
        .fields
        .iter()
        .map(Field::name)
        .map(Value::from)
        .collect::<Vec<_>>();
    Ok(vec![Value::from(fields)])
}

#[bridge(name = "record-field-mutable?", lib = "(rnrs records inspection (6))")]
pub fn record_field_mutable_pred(rtd: &Value, k: &Value) -> Result<Vec<Value>, Condition> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    let k: Arc<Number> = k.clone().try_into()?;
    let k: usize = k.as_ref().try_into()?;

    if k >= rtd.fields.len() {
        return Err(Condition::invalid_index(k, rtd.fields.len()));
    }

    Ok(vec![Value::from(matches!(
        rtd.fields[k],
        Field::Mutable(_)
    ))])
}
