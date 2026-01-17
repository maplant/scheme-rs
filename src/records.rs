//! Records (also known as structs).
//!
//! [`Records`](Record) are the mechanism by which new types are introduced to
//! scheme and the method by which custom Rust types are stored and accessible
//! to scheme code.
//!
//! Each records is described by its [`RecordTypeDescriptor`], which includes
//! the names of its name and fields among other properties.
//!
//! # Implementing [`SchemeCompatible`]
//!
//! Any type that implements [`Trace`] and [`Debug`](std::fmt::Debug) is
//! eligible to implement `SchemeCompatible`. Once this criteria is fulfilled,
//! we first need to use the [`rtd`] proc macro to fill in the type descriptor.
//!
//! For example, let's say that we have `Enemy` struct that we want to have two
//! immutable fields and one mutable field:
//!
//! ```rust
//! # use scheme_rs::gc::Trace;
//! #[derive(Trace, Debug)]
//! struct Enemy {
//!   // pos_x and pos_y will be immutable from Scheme code (although they can
//!   // can still be modified from Rust):
//!   pos_x: f64,
//!   pos_y: f64,
//!   // health will be mutable in both Scheme and Rust code:
//!   health: f64,
//! }
//! ```
//!
//! We can now fill in the `rtd` for the type:
//!
//! ```rust
//! # use std::sync::Arc;
//! # use scheme_rs::{gc::Trace, records::{rtd, SchemeCompatible, RecordTypeDescriptor},
//! # exceptions::Exception };
//! # #[derive(Debug, Trace)]
//! # struct Enemy {
//! #   pos_x: f64,
//! #   pos_y: f64,
//! #   health: f64,
//! # }
//! impl SchemeCompatible for Enemy {
//!     fn rtd() -> Arc<RecordTypeDescriptor> {
//!         rtd!(
//!             name: "enemy",
//!             fields: [ "pos-x", "pos-y", mutable("health") ],
//!             constructor: |pos_x, pos_y, health| {
//!                 Ok(Enemy {
//!                     pos_x: pos_x.try_to_scheme_type()?,
//!                     pos_y: pos_y.try_to_scheme_type()?,
//!                     health: health.try_to_scheme_type()?,
//!                 })
//!             }
//!         )
//!     }
//! }
//! ```
//!
//! It's important to note that you need to provide an argument in the
//! constructor for every field specified in `fields` and every parent field;
//! however, this does not preclude you from omitting fields that are present in
//! your data type from the `fields` list.
//!
//! Technically, [`rtd`](SchemeCompatible::rtd) is the only required method to
//! implement `SchemeCompatible`, but since we populated `fields` it will be
//! possible for the [`get_field`](SchemeCompatible::get_field) and
//! [`set_field`](SchemeCompatible::set_field) functions to be called, which by
//! default panic.
//!
//! Thus, we need to provide getters and setters for each field. We only need to
//! provide setters for the mutable fields. Fields are indexed by their position
//! in the `fields` array passed to `rtd`:
//!
//! ```rust
//! # use std::sync::Arc;
//! # use scheme_rs::{gc::Trace, value::Value, records::{rtd, SchemeCompatible, RecordTypeDescriptor}};
//! # #[derive(Debug, Trace)]
//! # struct Enemy {
//! #   pos_x: f64,
//! #   pos_y: f64,
//! #   health: f64,
//! # }
//! impl SchemeCompatible for Enemy {
//! #    fn rtd() -> Arc<RecordTypeDescriptor> {
//! #        rtd!(name: "enemy", sealed: true)
//! #    }
//!     fn get_field(&self, k: usize) -> Value {
//!         match k {
//!             0 => Value::from(self.pos_x),
//!             1 => Value::from(self.pos_y),
//!             2 => Value::from(self.health),
//!             _ => unreachable!(),
//!         }
//!     }
//!
//!     fn set_field(&mut self, _k: usize, val: Value) {
//!         let Some(health) = f64::try_from(val).ok() else { return; };
//!         self.health = health;
//!     }
//! }
//! ```
//!
//! ## Expressing subtyping relationships
//!
//! It is possible to express the classic child/parent relationship in structs
//! by embedding the parent in the child and implementing the
//! [`extract_embedded_record`](SchemeCompatible::extract_embedded_record)
//! function with the [`into_scheme_compatible`] function:
//!
//! ```rust
//! # use std::sync::Arc;
//! # use scheme_rs::{gc::{Trace, Gc}, value::Value, records::{rtd, SchemeCompatible, RecordTypeDescriptor, into_scheme_compatible}};
//! # #[derive(Debug, Trace)]
//! # struct Enemy {
//! #   pos_x: f64,
//! #   pos_y: f64,
//! #   health: f64,
//! # }
//! # impl SchemeCompatible for Enemy {
//! #    fn rtd() -> Arc<RecordTypeDescriptor> {
//! #        rtd!(name: "enemy", sealed: true)
//! #    }
//! # }
//! #[derive(Debug, Trace)]
//! struct SpecialEnemy {
//!     parent: Gc<Enemy>,
//!     special: u64,
//! }
//!
//! impl SchemeCompatible for SpecialEnemy {
//!     fn rtd() -> Arc<RecordTypeDescriptor> {
//!         rtd!(
//!             name: "enemy",
//!             parent: Enemy,
//!             fields: ["special"],
//!             constructor: |pos_x, pos_y, health, special| {
//!                 Ok(SpecialEnemy {
//!                     parent: Gc::new(Enemy {
//!                         pos_x: pos_x.try_to_scheme_type()?,
//!                         pos_y: pos_y.try_to_scheme_type()?,
//!                         health: health.try_to_scheme_type()?,
//!                     }),
//!                     special: special.try_to_scheme_type()?,
//!                 })
//!             }
//!         )
//!     }
//!
//!     fn get_field(&self, _k: usize) -> Value {
//!         Value::from(self.special)
//!     }
//!
//!     fn extract_embedded_record(
//!         &self,
//!         rtd: &Arc<RecordTypeDescriptor>
//!     ) -> Option<Gc<dyn SchemeCompatible>> {
//!         Enemy::rtd()
//!             .is_subtype_of(rtd)
//!             .then(|| into_scheme_compatible(self.parent.clone()))
//!     }
//! }
//! ```

use std::{
    any::Any,
    collections::HashMap,
    fmt,
    mem::ManuallyDrop,
    ptr::NonNull,
    sync::{Arc, LazyLock, Mutex},
};

use by_address::ByAddress;
use parking_lot::RwLock;

use crate::{
    exceptions::Exception,
    gc::{Gc, GcInner, Trace},
    proc::{Application, DynamicState, FuncPtr, Procedure},
    registry::{bridge, cps_bridge},
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    value::{UnpackedValue, Value, ValueType},
    vectors::Vector,
};

pub use scheme_rs_macros::rtd;

/// Type declaration for a record.
#[derive(Trace, Clone)]
#[repr(align(16))]
pub struct RecordTypeDescriptor {
    /// The name of the record.
    pub name: Symbol,
    /// Whether or not the record is "sealed". Sealed records cannot be made the
    /// parent of other records.
    pub sealed: bool,
    /// Whether or not the record is "opaque". Opaque records are not considered
    /// to be records proper and fail the `record?` predicate.
    pub opaque: bool,
    /// An optional universal identifier for the record. Prevents the record
    /// from being "generative," i.e. unique upon each call to
    /// `define-record-type`.
    pub uid: Option<Symbol>,
    /// Whether or not the type being described is a Rust type.
    pub rust_type: bool,
    /// The Rust parent of the record type, if it exists.
    pub rust_parent_constructor: Option<RustParentConstructor>,
    /// Parent is most recently inserted record type, if one exists.
    pub inherits: indexmap::IndexSet<ByAddress<Arc<RecordTypeDescriptor>>>,
    /// The index into `fields` where this record's fields proper begin. All of
    /// the previous fields belong to a parent.
    pub field_index_offset: usize,
    /// The fields of the record, including all of the ones inherited from
    /// parents.
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
            self.name, self.sealed, self.opaque, self.rust_type,
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

/// Description of a Record field.
#[derive(Trace, Clone)]
pub enum Field {
    Immutable(Symbol),
    Mutable(Symbol),
}

impl Field {
    fn parse(field: &Value) -> Result<Self, Exception> {
        let (mutability, field_name) = field.clone().try_into()?;
        let mutability: Symbol = mutability.try_into()?;
        let (field_name, _) = field_name.clone().try_into()?;
        let field_name: Symbol = field_name.try_into()?;
        match &*mutability.to_str() {
            "mutable" => Ok(Field::Mutable(field_name)),
            "immutable" => Ok(Field::Immutable(field_name)),
            _ => Err(Exception::error(
                "mutability specifier must be mutable or immutable".to_string(),
            )),
        }
    }

    fn parse_fields(fields: &Value) -> Result<Vec<Self>, Exception> {
        let fields: Vector = fields.clone().try_into()?;
        fields.0.vec.read().iter().map(Self::parse).collect()
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

/*
/// The record type descriptor for the "record type descriptor" type.
static RECORD_TYPE_DESCRIPTOR_RTD: LazyLock<Arc<RecordTypeDescriptor>> = LazyLock::new(|| {
    Arc::new(RecordTypeDescriptor {
        name: Symbol::intern("rtd"),
        sealed: true,
        opaque: true,
        uid: None,
        rust_type: false,
        rust_parent_constructor: None,
        inherits: indexmap::IndexSet::new(),
        field_index_offset: 0,
        fields: vec![],
    })
});
*/

type NonGenerativeStore = LazyLock<Arc<Mutex<HashMap<Symbol, Arc<RecordTypeDescriptor>>>>>;

static NONGENERATIVE: NonGenerativeStore = LazyLock::new(|| Arc::new(Mutex::new(HashMap::new())));

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
) -> Result<Vec<Value>, Exception> {
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
        rust_type: false,
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
pub fn record_type_descriptor_pred(obj: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        obj.type_of() == ValueType::RecordTypeDescriptor,
    )])
}

/// A description of a record's constructor.
#[derive(Trace, Clone)]
pub struct RecordConstructorDescriptor {
    parent: Option<Gc<RecordConstructorDescriptor>>,
    rtd: Arc<RecordTypeDescriptor>,
    protocol: Procedure,
}

impl SchemeCompatible for RecordConstructorDescriptor {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "record-constructor-descriptor", sealed: true, opaque: true)
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
        FuncPtr::Bridge(default_protocol),
        1,
        false,
    );
    Gc::new(RecordConstructorDescriptor {
        parent,
        rtd,
        protocol,
    })
}

#[cps_bridge(
    def = "make-record-constructor-descriptor rtd parent-constructor-descriptor protocol",
    lib = "(rnrs records procedural (6))"
)]
pub fn make_record_constructor_descriptor(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let k: Procedure = k.try_into()?;
    let [rtd, parent_rcd, protocol] = args else {
        unreachable!();
    };

    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;

    if rtd.rust_type && rtd.rust_parent_constructor.is_none() {
        return Err(Exception::error(format!(
            "cannot create a record-constructor-descriptor for rust type without a constructor {}",
            rtd.name
        )));
    }

    let parent_rcd = if parent_rcd.is_true() {
        let Some(parent_rtd) = rtd.inherits.last() else {
            return Err(Exception::error("RTD is a base type".to_string()));
        };
        let parent_rcd = parent_rcd.try_to_rust_type::<RecordConstructorDescriptor>()?;
        if !Arc::ptr_eq(&parent_rcd.rtd, parent_rtd) {
            return Err(Exception::error(
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
            FuncPtr::Bridge(default_protocol),
            1,
            false,
        )
    };

    let rcd = RecordConstructorDescriptor {
        parent: parent_rcd,
        rtd,
        protocol,
    };

    Ok(Application::new(
        k,
        vec![Value::from(Record::from_rust_type(rcd))],
    ))
}

#[cps_bridge(def = "record-constructor rcd", lib = "(rnrs records procedural (6))")]
pub fn record_constructor(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let [rcd] = args else {
        unreachable!();
    };
    let rcd = rcd.try_to_rust_type::<RecordConstructorDescriptor>()?;

    let (protocols, rtds) = rcd_to_protocols_and_rtds(&rcd);

    // See if there is a rust constructor available
    let rust_constructor = rtds
        .iter()
        .find(|rtd| rtd.rust_parent_constructor.is_some())
        .map_or_else(|| Value::from(false), |rtd| Value::from(rtd.clone()));

    let protocols = protocols.into_iter().map(Value::from).collect::<Vec<_>>();
    let rtds = rtds.into_iter().map(Value::from).collect::<Vec<_>>();
    let chain_protocols = Value::from(dyn_state.new_k(
        runtime.clone(),
        vec![Value::from(protocols), k],
        chain_protocols,
        1,
        false,
    ));

    Ok(chain_constructors(
        runtime,
        &[Value::from(rtds), rust_constructor],
        &[],
        &[],
        dyn_state,
        chain_protocols,
    ))
}

fn rcd_to_protocols_and_rtds(
    rcd: &Gc<RecordConstructorDescriptor>,
) -> (Vec<Procedure>, Vec<Arc<RecordTypeDescriptor>>) {
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
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        // env[0] is a vector of protocols
        let protocols: Vector = env.as_ref().unwrap().clone().try_into().unwrap();
        // env[1] is k, the continuation
        let k = env.add(1).as_ref().unwrap().clone();

        let mut protocols = protocols.0.vec.read().clone();
        let remaining_protocols = protocols.split_off(1);
        let curr_protocol: Procedure = protocols[0].clone().try_into().unwrap();

        // If there are no more remaining protocols after the current, call the
        // protocol with arg[0] and the continuation.
        if remaining_protocols.is_empty() {
            return Box::into_raw(Box::new(Application::new(
                curr_protocol,
                vec![args.as_ref().unwrap().clone(), k.clone()],
            )));
        }

        // Otherwise, turn the remaining chain into the continuation:
        let new_k = dyn_state.as_mut().unwrap().new_k(
            Runtime::from_raw_inc_rc(runtime),
            vec![Value::from(remaining_protocols), k],
            chain_protocols,
            1,
            false,
        );

        Box::into_raw(Box::new(Application::new(
            curr_protocol,
            vec![args.as_ref().unwrap().clone(), Value::from(new_k)],
        )))
    }
}

#[cps_bridge]
fn chain_constructors(
    runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let k: Procedure = k.try_into()?;
    // env[0] is a vector of RTDs
    let rtds: Vector = env[0].clone().try_into()?;
    // env[1] is the possible rust constructor
    let rust_constructor = env[1].clone();
    let mut rtds = rtds.0.vec.read().clone();
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
            FuncPtr::Bridge(chain_constructors)
        } else {
            FuncPtr::Bridge(constructor)
        },
        num_args,
        false,
    );
    Ok(Application::new(k, vec![Value::from(next_proc)]))
}

#[cps_bridge]
fn constructor(
    _runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let k: Procedure = k.try_into()?;
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
            Some((rust_rtd.rust_parent_constructor.unwrap().constructor)(
                &fields,
            )?),
            remaining_fields,
        )
    } else {
        (None, fields)
    };
    let record = Value::from(Record(Gc::new(RecordInner {
        rust_parent,
        rtd,
        fields: fields.into_iter().map(RwLock::new).collect(),
    })));
    Ok(Application::new(k, vec![record]))
}

#[cps_bridge]
fn default_protocol(
    runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let k: Procedure = k.try_into()?;
    let rtd: Arc<RecordTypeDescriptor> = env[0].clone().try_into()?;
    let num_args = rtd.field_index_offset + rtd.fields.len();

    let constructor = Procedure::new(
        runtime.clone(),
        vec![args[0].clone(), Value::from(rtd)],
        FuncPtr::Bridge(default_protocol_constructor),
        num_args,
        false,
    );

    Ok(Application::new(k, vec![Value::from(constructor)]))
}

#[cps_bridge]
fn default_protocol_constructor(
    runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let constructor: Procedure = env[0].clone().try_into()?;
    let rtd: Arc<RecordTypeDescriptor> = env[1].clone().try_into()?;
    let mut args = args.to_vec();

    let k = if let Some(parent) = rtd.inherits.last() {
        let remaining = args.split_off(parent.field_index_offset + parent.fields.len());
        Value::from(dyn_state.new_k(
            runtime.clone(),
            vec![Value::from(remaining), k],
            call_constructor_continuation,
            1,
            false,
        ))
    } else {
        k
    };

    args.push(k);
    Ok(Application::new(constructor, args))
}

pub(crate) unsafe extern "C" fn call_constructor_continuation(
    _runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    _dyn_state: *mut DynamicState,
) -> *mut Application {
    unsafe {
        let constructor: Procedure = args.as_ref().unwrap().clone().try_into().unwrap();
        let args: Vector = env.as_ref().unwrap().clone().try_into().unwrap();
        let mut args = args.0.vec.read().clone();
        let cont = env.add(1).as_ref().unwrap().clone();
        args.push(cont);

        // Call the constructor
        Box::into_raw(Box::new(Application::new(constructor, args)))
    }
}

/// A Scheme record type. Effectively a tuple of a fixed size array and some type
/// information.
#[derive(Trace, Clone)]
pub struct Record(pub(crate) Gc<RecordInner>);

impl Record {
    pub fn rtd(&self) -> Arc<RecordTypeDescriptor> {
        self.0.rtd.clone()
    }

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
    pub fn cast<T: SchemeCompatible>(&self) -> Option<Gc<T>> {
        let rust_parent = self.0.rust_parent.as_ref()?;

        // Attempt to extract any embedded records
        let rtd = T::rtd();
        let mut t = rust_parent.clone();
        while let Some(embedded) = { t.extract_embedded_record(&rtd) } {
            t = embedded;
        }

        let t = ManuallyDrop::new(t);

        // Second, convert the opaque_parent type into a Gc<dyn Any>
        let any: NonNull<GcInner<dyn Any + Send + Sync>> = t.ptr;
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
        self.0.fmt(f)
    }
}

#[derive(Trace)]
#[repr(align(16))]
pub(crate) struct RecordInner {
    pub(crate) rust_parent: Option<Gc<dyn SchemeCompatible>>,
    pub(crate) rtd: Arc<RecordTypeDescriptor>,
    pub(crate) fields: Vec<RwLock<Value>>,
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
            let field = field.read();
            let name = field_names.next().unwrap().name();
            write!(f, " {name}: {field:?}")?;
        }
        write!(f, ">")
    }
}

/// A Rust value that can present itself as a Scheme record.
pub trait SchemeCompatible: fmt::Debug + Trace + Any + Send + Sync + 'static {
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

/// Convenience function for converting a `Gc<T>` into a
/// `Gc<dyn SchemeCompatible>`.
///
/// This isn't as simple as using the `as` keyword in Rust due to the
/// instability of the `CoerceUnsized` trait.
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

#[derive(Copy, Clone, Debug, Trace)]
pub struct RustParentConstructor {
    #[trace(skip)]
    constructor: ParentConstructor,
}

impl RustParentConstructor {
    pub fn new(constructor: ParentConstructor) -> Self {
        Self { constructor }
    }
}

type ParentConstructor = fn(&[Value]) -> Result<Gc<dyn SchemeCompatible>, Exception>;

pub(crate) fn is_subtype_of(val: &Value, rt: &Value) -> Result<bool, Exception> {
    let UnpackedValue::Record(rec) = val.clone().unpack() else {
        return Ok(false);
    };
    let rt: Arc<RecordTypeDescriptor> = rt.clone().try_into()?;
    Ok(Arc::ptr_eq(&rec.0.rtd, &rt) || rec.0.rtd.inherits.contains(&ByAddress::from(rt)))
}

#[cps_bridge]
fn record_predicate_fn(
    _runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let k: Procedure = k.try_into()?;
    let [val] = args else {
        unreachable!();
    };
    // RTD is the first environment variable:
    Ok(Application::new(
        k,
        vec![Value::from(is_subtype_of(val, &env[0])?)],
    ))
}

#[cps_bridge(def = "record-predicate rtd", lib = "(rnrs records procedural (6))")]
pub fn record_predicate(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let k: Procedure = k.try_into()?;
    let [rtd] = args else {
        unreachable!();
    };
    // TODO: Check if RTD is a record type.
    let pred_fn = Procedure::new(
        runtime.clone(),
        vec![rtd.clone()],
        FuncPtr::Bridge(record_predicate_fn),
        1,
        false,
    );
    Ok(Application::new(k, vec![Value::from(pred_fn)]))
}

#[cps_bridge]
fn record_accessor_fn(
    _runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let k: Procedure = k.try_into()?;
    let [val] = args else {
        unreachable!();
    };
    let record: Record = val.clone().try_into()?;
    // RTD is the first environment variable, field index is the second
    if !is_subtype_of(val, &env[0])? {
        return Err(Exception::error(
            "not a child of this record type".to_string(),
        ));
    }
    let idx: usize = env[1].clone().try_into()?;
    let val = record.0.fields[idx].read().clone();
    Ok(Application::new(k, vec![val]))
}

#[cps_bridge(def = "record-accessor rtd k", lib = "(rnrs records procedural (6))")]
pub fn record_accessor(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let k: Procedure = k.try_into()?;
    let [rtd, idx] = args else {
        unreachable!();
    };
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    let idx: usize = idx.clone().try_into()?;
    if idx > rtd.fields.len() {
        return Err(Exception::error(format!(
            "{idx} is out of range {}",
            rtd.fields.len()
        )));
    }
    let idx = idx + rtd.field_index_offset;
    let accessor_fn = Procedure::new(
        runtime.clone(),
        vec![Value::from(rtd), Value::from(idx)],
        FuncPtr::Bridge(record_accessor_fn),
        1,
        false,
    );
    Ok(Application::new(k, vec![Value::from(accessor_fn)]))
}

#[cps_bridge]
fn record_mutator_fn(
    _runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let k: Procedure = k.try_into()?;
    let [rec, new_val] = args else {
        unreachable!();
    };
    let record: Record = rec.clone().try_into()?;
    // RTD is the first environment variable, field index is the second
    if !is_subtype_of(rec, &env[0])? {
        return Err(Exception::error(
            "not a child of this record type".to_string(),
        ));
    }
    let idx: usize = env[1].clone().try_into()?;
    *record.0.fields[idx].write() = new_val.clone();
    Ok(Application::new(k, vec![]))
}

#[cps_bridge(def = "record-mutator rtd k", lib = "(rnrs records procedural (6))")]
pub fn record_mutator(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_state: &mut DynamicState,
    k: Value,
) -> Result<Application, Exception> {
    let k: Procedure = k.try_into()?;
    let [rtd, idx] = args else {
        unreachable!();
    };
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    let idx: usize = idx.clone().try_into()?;
    if idx > rtd.fields.len() {
        return Err(Exception::error(format!(
            "{idx} is out of range {}",
            rtd.fields.len()
        )));
    }
    if matches!(rtd.fields[idx], Field::Immutable(_)) {
        return Err(Exception::error(format!("{idx} is immutable")));
    }
    let idx = idx + rtd.field_index_offset;
    let mutator_fn = Procedure::new(
        runtime.clone(),
        vec![Value::from(rtd), Value::from(idx)],
        FuncPtr::Bridge(record_mutator_fn),
        2,
        false,
    );
    Ok(Application::new(k, vec![Value::from(mutator_fn)]))
}

// Inspection library:

#[bridge(name = "record?", lib = "(rnrs records inspection (6))")]
pub fn record_pred(obj: &Value) -> Result<Vec<Value>, Exception> {
    match &*obj.unpacked_ref() {
        UnpackedValue::Record(rec) => Ok(vec![Value::from(!rec.0.rtd.opaque)]),
        _ => Ok(vec![Value::from(false)]),
    }
}

#[bridge(name = "record-rtd", lib = "(rnrs records inspection (6))")]
pub fn record_rtd(record: &Value) -> Result<Vec<Value>, Exception> {
    match &*record.unpacked_ref() {
        UnpackedValue::Record(rec) if !rec.0.rtd.opaque => Ok(vec![Value::from(rec.0.rtd.clone())]),
        _ => Err(Exception::error(
            "expected a non-opaque record type".to_string(),
        )),
    }
}

#[bridge(name = "record-type-name", lib = "(rnrs records inspection (6))")]
pub fn record_type_name(rtd: &Value) -> Result<Vec<Value>, Exception> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    Ok(vec![Value::from(rtd.name)])
}

#[bridge(name = "record-type-parent", lib = "(rnrs records inspection (6))")]
pub fn record_type_parent(rtd: &Value) -> Result<Vec<Value>, Exception> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    if let Some(parent) = rtd.inherits.last() {
        Ok(vec![Value::from(parent.0.clone())])
    } else {
        Ok(vec![Value::from(false)])
    }
}

#[bridge(name = "record-type-uid", lib = "(rnrs records inspection (6))")]
pub fn record_type_uid(rtd: &Value) -> Result<Vec<Value>, Exception> {
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
pub fn record_type_generative_pred(rtd: &Value) -> Result<Vec<Value>, Exception> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    Ok(vec![Value::from(rtd.uid.is_none())])
}

#[bridge(name = "record-type-sealed?", lib = "(rnrs records inspection (6))")]
pub fn record_type_sealed_pred(rtd: &Value) -> Result<Vec<Value>, Exception> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    Ok(vec![Value::from(rtd.sealed)])
}

#[bridge(name = "record-type-opaque?", lib = "(rnrs records inspection (6))")]
pub fn record_type_opaque_pred(rtd: &Value) -> Result<Vec<Value>, Exception> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    Ok(vec![Value::from(rtd.opaque)])
}

#[bridge(
    name = "record-type-field-names",
    lib = "(rnrs records inspection (6))"
)]
pub fn record_type_field_names(rtd: &Value) -> Result<Vec<Value>, Exception> {
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
pub fn record_field_mutable_pred(rtd: &Value, k: &Value) -> Result<Vec<Value>, Exception> {
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    let k: usize = k.try_to_scheme_type()?;

    if k >= rtd.fields.len() {
        return Err(Exception::invalid_index(k, rtd.fields.len()));
    }

    Ok(vec![Value::from(matches!(
        rtd.fields[k],
        Field::Mutable(_)
    ))])
}
