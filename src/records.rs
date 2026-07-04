//! Records (also known as structs).
//!
//! [`Records`](Record) are the mechanism by which new types are introduced to
//! scheme and the method by which custom Rust types are stored and accessible
//! to scheme code.
//!
//! Each records is described by its [`RecordTypeDescriptor`], which includes
//! the names of its name and fields among other properties.
//!
//! # Implementing [`Embeddable`]
//!
//! Any type that implements [`Trace`] and [`Debug`](std::fmt::Debug) is
//! eligible to implement `Embeddable`, which allows a value of that type to be
//! embedded directly inside the record's allocation. Once this criteria is
//! fulfilled, we first need to use the [`rtd`] proc macro to fill in the type
//! descriptor.
//!
//! For example, let's say that we have `Enemy` struct that we want to have two
//! immutable fields and one mutable field:
//!
//! ```rust
//! # use std::sync::Mutex;
//! # use scheme_rs::gc::Trace;
//! #[derive(Trace, Debug)]
//! struct Enemy {
//!   // pos_x and pos_y will be immutable
//!   pos_x: f64,
//!   pos_y: f64,
//!   // health will be mutable (thus the mutex)
//!   health: Mutex<f64>,
//! }
//! ```
//!
//! We can now fill in the `rtd` for the type. Note that the `ty` field tells the
//! macro which Rust type is being embedded:
//!
//! ```rust
//! # use std::sync::{Arc, Mutex};
//! # use scheme_rs::{gc::Trace, records::{rtd, Embeddable, RecordTypeDescriptor},
//! # exceptions::Exception };
//! # #[derive(Debug, Trace)]
//! # struct Enemy {
//! #   pos_x: f64,
//! #   pos_y: f64,
//! #   health: Mutex<f64>,
//! # }
//! impl Embeddable for Enemy {
//!     fn rtd() -> Arc<RecordTypeDescriptor> {
//!         rtd!(
//!             ty: Enemy,
//!             name: "enemy",
//!             fields: [ "pos-x", "pos-y", mutable("health") ],
//!             constructor: |pos_x, pos_y, health| {
//!                 Ok(Enemy {
//!                     pos_x: pos_x.try_to()?,
//!                     pos_y: pos_y.try_to()?,
//!                     health: Mutex::new(health.try_to()?),
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
//! Technically, [`rtd`](Embeddable::rtd) is the only required method to
//! implement `Embeddable`, but since we populated `fields` it will be
//! possible for the [`get_field`](Embeddable::get_field) and
//! [`set_field`](Embeddable::set_field) functions to be called, which by
//! default error.
//!
//! Thus, we need to provide getters and setters for each field. We only need to
//! provide setters for the mutable fields. Fields are indexed by their position
//! in the `fields` array passed to `rtd`:
//!
//! ```rust
//! # use std::sync::{Arc, Mutex};
//! # use scheme_rs::{gc::Trace, value::Value, records::{rtd, Embeddable, RecordTypeDescriptor}, exceptions::Exception};
//! # #[derive(Debug, Trace)]
//! # struct Enemy {
//! #   pos_x: f64,
//! #   pos_y: f64,
//! #   health: Mutex<f64>,
//! # }
//! impl Embeddable for Enemy {
//! #    fn rtd() -> Arc<RecordTypeDescriptor> {
//! #        rtd!(ty: Enemy, name: "enemy", sealed: true)
//! #    }
//!     fn get_field(&self, k: usize) -> Result<Value, Exception> {
//!         match k {
//!             0 => Ok(Value::from(self.pos_x)),
//!             1 => Ok(Value::from(self.pos_y)),
//!             2 => Ok(Value::from(*self.health.lock().unwrap())),
//!             _ => Err(Exception::invalid_record_index(k)),
//!         }
//!     }
//!
//!     fn set_field(&self, k: usize, new_health: Value) -> Result<(), Exception> {
//!         if k != 2 { return Err(Exception::invalid_record_index(k)); }
//!         let new_health = f64::try_from(new_health)?;
//!         *self.health.lock().unwrap() = new_health;
//!         Ok(())
//!     }
//! }
//! ```
//!
//! ## Expressing subtyping relationships
//!
//! It is possible to express the classic child/parent relationship in structs
//! by embedding the parent in the child by value and implementing the
//! [`parent_record`](Embeddable::parent_record) function:
//!
//! ```rust
//! # use std::sync::Arc;
//! # use scheme_rs::{gc::Trace, value::Value, records::{rtd, Embeddable, RecordTypeDescriptor}, exceptions::Exception};
//! # #[derive(Debug, Trace)]
//! # struct Enemy {
//! #   pos_x: f64,
//! #   pos_y: f64,
//! #   health: f64,
//! # }
//! # impl Embeddable for Enemy {
//! #    fn rtd() -> Arc<RecordTypeDescriptor> {
//! #        rtd!(ty: Enemy, name: "enemy", sealed: true)
//! #    }
//! # }
//! #[derive(Debug, Trace)]
//! struct SpecialEnemy {
//!     parent: Enemy,
//!     special: u64,
//! }
//!
//! impl Embeddable for SpecialEnemy {
//!     fn rtd() -> Arc<RecordTypeDescriptor> {
//!         rtd!(
//!             ty: SpecialEnemy,
//!             name: "enemy",
//!             parent: Enemy,
//!             fields: ["special"],
//!             // The constructor must take all of the arguments
//!             // required by all of the parent objects, in order.
//!             constructor: |pos_x, pos_y, health, special| {
//!                 Ok(SpecialEnemy {
//!                     parent: Enemy {
//!                         pos_x: pos_x.try_to()?,
//!                         pos_y: pos_y.try_to()?,
//!                         health: health.try_to()?,
//!                     },
//!                     special: special.try_to()?,
//!                 })
//!             }
//!         )
//!     }
//!
//!     fn get_field(&self, _k: usize) -> Result<Value, Exception> {
//!         Ok(Value::from(self.special))
//!     }
//!
//!     fn parent_record(
//!         &self,
//!         rtd: &Arc<RecordTypeDescriptor>
//!     ) -> Option<&dyn Embeddable> {
//!         Enemy::rtd()
//!             .is_subtype_of(rtd)
//!             .then(|| &self.parent as &dyn Embeddable)
//!     }
//! }
//! ```
//!
//! ## Defining Rust types as Scheme records
//!
//! There is still a little bit more work to do in order to have our Rust type
//! appear fully as a record in scheme. First, we can use the `lib` keyword in
//! the `rtd!` macro to specify a location to put a procedure that returns our
//! type's rtd:
//!
//! ```rust
//! # use std::sync::Arc;
//! # use scheme_rs::{gc::Trace, records::{rtd, Embeddable, RecordTypeDescriptor},
//! # exceptions::Exception };
//! # #[derive(Debug, Trace)]
//! # struct Enemy {}
//! impl Embeddable for Enemy {
//!     fn rtd() -> Arc<RecordTypeDescriptor> {
//!         rtd!(
//!             ty: Enemy,
//!             lib: "(enemies (1))",
//!             // ...
//! #           name: "enemy",
//! #           sealed: true, opaque: true,
//!         )
//!     }
//! }
//! ```
//!
//! This will register the procedure `enemy-rtd` in the `(enemies (1))` scheme
//! library. We can expand that library using the `define-rust-type` macro
//! provided by the `(rust (1))` library to define enemy fully as a scheme
//! record:
//!
//! ```scheme
//! (library (enemies (1))
//!  (export enemy make-enemy enemy?)
//!  (import (rust (1)))
//!
//!  (define-rust-type enemy (enemy-rtd) make-enemy enemy?))
//! ```

use std::{
    alloc::{self, Layout},
    any::{Any, TypeId},
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem::align_of,
    ops::Deref,
    ptr::{self, NonNull},
    slice,
    sync::{Arc, LazyLock, Mutex},
};

use by_address::ByAddress;
use indexmap::{IndexMap, IndexSet};
use parking_lot::RwLock;

use crate::{
    exceptions::Exception,
    gc::{Gc, GcInner, OpaqueGcPtr, Trace},
    proc::{Application, ContBarrier, FuncPtr, Procedure},
    registry::{bridge, cps_bridge},
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    value::{Cell, UnpackedValue, Value, ValueType},
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
    /// Embedded type's VTable. Some if the record contains an embedded type.
    /// Child RTDs inherit the `embedded_vtable` of their Parent.
    pub embedded_vtable: Option<EmbeddableVTable>,
    /// The Rust constructor of the embedded type, if it exists. Child RTDs
    /// inherit the `embedded_constructor` of their
    pub embedded_constructor: Option<RustParentConstructor>,
    /// Parent is most recently inserted record type, if one exists.
    pub inherits: indexmap::IndexSet<ByAddress<Arc<RecordTypeDescriptor>>>,
    /// The number of fields inherited by this record.
    pub num_inherited_fields: usize,
    /// The fields of the record, not including any of the ones inherited from
    /// parents.
    pub fields: Vec<Field>,
}

impl RecordTypeDescriptor {
    // TODO: Add a constructor that sets everything up from a parent.

    pub fn is_rust_type(&self) -> bool {
        self.embedded_vtable.is_some()
    }

    pub fn is_base_record_type(&self) -> bool {
        self.inherits.is_empty()
    }

    pub fn is_subtype_of(self: &Arc<Self>, rtd: &Arc<Self>) -> bool {
        Arc::ptr_eq(self, rtd) || self.inherits.contains(&ByAddress(rtd.clone()))
    }

    pub fn num_fields(&self) -> usize {
        self.fields.len() + self.num_inherited_fields
    }
}

impl fmt::Debug for RecordTypeDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "#<rtd name: {} sealed: {} opaque: {} ", // rust: {} ",
            self.name, self.sealed, self.opaque,
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

    fn is_mutable(&self) -> bool {
        matches!(self, Self::Mutable(_))
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

/// A Scheme record type. Effectively a tuple of a fixed size array and some type
/// information.
#[derive(Trace, Clone)]
pub struct Record(pub(crate) Gc<RecordInner>);

impl Record {
    pub fn rtd(&self) -> Arc<RecordTypeDescriptor> {
        self.0.rtd.clone()
    }

    /// Embed a rust value into a record.
    pub fn embed<E: Embeddable>(e: E) -> Self {
        let (layout, embed_offset) = Layout::from_size_align(
            RecordInner::fields_offset(),
            align_of::<GcInner<RecordInner>>(),
        )
        .unwrap()
        .extend(Layout::new::<E>())
        .unwrap();
        let layout = layout.pad_to_align();

        let ptr = unsafe {
            let record = alloc::alloc(layout) as *mut GcInner<RecordInner>;
            ptr::write(
                record,
                GcInner::new(RecordInner {
                    rtd: E::rtd(),
                    fields: [],
                }),
            );
            ptr::write(record.byte_add(embed_offset) as *mut E, e);
            record
        };

        let inner = Gc {
            ptr: NonNull::new(ptr).unwrap(),
            marker: PhantomData,
        };

        unsafe {
            crate::gc::unroot(&inner, layout);
        }

        Self(inner)
    }

    pub fn cast<E: Embeddable>(&self) -> Option<Embedded<E>> {
        let embedded_ptr = self.0.embedded_ptr()?;
        let embedded_vtable = self.0.rtd.embedded_vtable.as_ref()?;

        if TypeId::of::<E>() == embedded_vtable.type_id {
            return Some(Embedded::from_raw_parts(
                NonNull::new(embedded_ptr as *mut E).unwrap(),
                self.clone(),
            ));
        }

        let rtd = E::rtd();
        let mut embedded = embedded_vtable.ptr_to_parent(embedded_ptr, &rtd)?;
        while let Some(parent) = embedded.parent_record(&rtd) {
            embedded = parent;
        }

        let downcast_embedded = (embedded as &dyn Any).downcast_ref::<E>()?;

        Some(Embedded {
            embedded_ptr: NonNull::from_ref(downcast_embedded),
            record: self.clone(),
        })
    }

    /*

    /// Get the kth field of the Record
    pub fn get_field(&self, k: usize) -> Result<Value, Exception> {
        self.get_parent_field(&self.rtd(), k)
    }

    /// Get the kth field of a parent Record
    pub fn get_parent_field(
        &self,
        rtd: &Arc<RecordTypeDescriptor>,
        k: usize,
    ) -> Result<Value, Exception> {
        /*
        if !self.0.rtd.is_subtype_of(rtd) {
            Err(Exception::error(format!("not a subtype of {}", rtd.name)))
        } else if let Some(mut t) = self.0.rust_parent.clone() {
            while let Some(embedded) = { t.extract_embedded_record(rtd) } {
                t = embedded;
            }
            t.get_field(rtd.field_index_offset + k)
        } else {
            Ok(self.0.fields[rtd.field_index_offset + k].read().clone())
        }
         */
        todo!()
    }

    /// Set the kth field of the Record
    pub fn set_field(&self, k: usize, new_value: Value) -> Result<(), Exception> {
        self.set_parent_field(&self.rtd(), k, new_value)
    }

    /// Set the kth field of a parent Record
    pub fn set_parent_field(
        &self,
        rtd: &Arc<RecordTypeDescriptor>,
        k: usize,
        new_value: Value,
    ) -> Result<(), Exception> {
        /*
        if !self.0.rtd.is_subtype_of(rtd) {
            Err(Exception::error(format!("not a subtype of {}", rtd.name)))
        } else if let Some(mut t) = self.0.rust_parent.clone() {
            while let Some(embedded) = { t.extract_embedded_record(rtd) } {
                t = embedded;
            }
            t.set_field(rtd.field_index_offset + k, new_value)
        } else {
            *self.0.fields[rtd.field_index_offset + k].write() = new_value;
            Ok(())
        }
         */
        todo!()
    }
     */

    pub fn display_fmt(
        &self,
        circular_values: &mut IndexMap<Value, bool>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        if let Some(vtable) = self.0.rtd.embedded_vtable {
            (vtable.display_fmt)(self.0.embedded_ptr().unwrap(), circular_values, fmt)
        } else {
            Ok(())
        }
    }

    pub fn debug_fmt(
        &self,
        circular_values: &mut IndexMap<Value, bool>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        if let Some(vtable) = self.0.rtd.embedded_vtable {
            (vtable.debug_fmt)(self.0.embedded_ptr().unwrap(), circular_values, fmt)
        } else {
            write!(fmt, "#<{}", self.0.rtd.name)?;
            for (Field::Mutable(name) | Field::Immutable(name), field) in self
                .0
                .rtd
                .inherits
                .iter()
                .cloned()
                .chain(Some(ByAddress(self.0.rtd.clone())))
                .flat_map(|rtd| rtd.fields.clone())
                .zip(self.0.fields())
            {
                write!(fmt, " {name}: ")?;
                field.debug_fmt(circular_values, fmt)?;
            }
            write!(fmt, ">")
        }
    }

    pub fn eq(&self, rhs: &Record) -> bool {
        if let Some(vtable) = self.0.rtd.embedded_vtable {
            (vtable.eq)(self.0.embedded_ptr().unwrap(), rhs)
        } else {
            Gc::ptr_eq(&self.0, &rhs.0)
        }
    }

    pub fn eqv(&self, rhs: &Record) -> bool {
        if let Some(vtable) = self.0.rtd.embedded_vtable {
            (vtable.eqv)(self.0.embedded_ptr().unwrap(), rhs)
        } else {
            Gc::ptr_eq(&self.0, &rhs.0)
        }
    }

    pub fn equal(&self, rhs: &Record) -> bool {
        if let Some(vtable) = self.0.rtd.embedded_vtable {
            (vtable.equal)(self.0.embedded_ptr().unwrap(), rhs)
        } else {
            Gc::ptr_eq(&self.0, &rhs.0)
        }
    }

    pub fn eq_hash<H: Hasher>(&self, hasher: &mut H) {
        if let Some(vtable) = self.0.rtd.embedded_vtable {
            (vtable.eq_hash)(self.0.embedded_ptr().unwrap(), hasher)
        } else {
            Gc::as_ptr(&self.0).hash(hasher)
        }
    }

    pub fn eqv_hash<H: Hasher>(&self, hasher: &mut H) {
        if let Some(vtable) = self.0.rtd.embedded_vtable {
            (vtable.eqv_hash)(self.0.embedded_ptr().unwrap(), hasher)
        } else {
            Gc::as_ptr(&self.0).hash(hasher)
        }
    }

    pub fn equal_hash<H: Hasher>(&self, recursive: &mut IndexSet<Value>, hasher: &mut H) {
        if let Some(vtable) = self.0.rtd.embedded_vtable {
            (vtable.equal_hash)(self.0.embedded_ptr().unwrap(), recursive, hasher)
        } else {
            Gc::as_ptr(&self.0).hash(hasher)
        }
    }
}

impl fmt::Display for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_fmt(&mut IndexMap::default(), f)
    }
}

impl fmt::Debug for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.debug_fmt(&mut IndexMap::default(), f)
    }
}

// #[derive(Trace)]
#[repr(C, align(16))]
pub(crate) struct RecordInner {
    // pub(crate) rust_parent: Option<Gc<dyn SchemeCompatible>>,
    rtd: Arc<RecordTypeDescriptor>,
    /// Pointer to the first field. If the record contains an embedded value it
    /// will be stored after the last field.
    fields: [Value; 0],
}

impl RecordInner {
    pub(crate) fn num_embedded_fields(&self) -> usize {
        self.rtd
            .embedded_vtable
            .as_ref()
            .map_or(0, |vtable| vtable.embedded_fields)
    }

    pub(crate) fn num_unembedded_fields(&self) -> usize {
        self.rtd.num_fields() - self.num_embedded_fields()
    }

    pub(crate) fn fields(&self) -> &[Value] {
        unsafe { slice::from_raw_parts(self.fields_ptr(), self.num_unembedded_fields()) }
    }

    pub(crate) const fn fields_offset() -> usize {
        GcInner::<RecordInner>::data_offset() + std::mem::offset_of!(RecordInner, fields)
    }

    pub(crate) fn fields_ptr(&self) -> *const Value {
        &self.fields as *const Value
    }

    pub(crate) fn fields_ptr_mut(&mut self) -> *mut Value {
        &mut self.fields as *mut Value
    }

    pub(crate) fn embedded_ptr(&self) -> Option<*const ()> {
        let vtable = self.rtd.embedded_vtable?;
        unsafe {
            let fields_end = self
                .fields_ptr()
                .add(self.rtd.num_fields() - vtable.embedded_fields);
            Some(fields_end.add(fields_end.align_offset(vtable.layout.align())) as *const ())
        }
    }

    pub(crate) fn embedded_ptr_mut(&mut self) -> Option<*mut ()> {
        let vtable = self.rtd.embedded_vtable?;
        unsafe {
            let fields_end = self
                .fields_ptr_mut()
                .add(self.rtd.num_fields() - vtable.embedded_fields);
            Some(fields_end.add(fields_end.align_offset(vtable.layout.align())) as *mut ())
        }
    }
}

unsafe impl Trace for RecordInner {
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(crate::gc::OpaqueGcPtr)) {
        let num_fields = self.num_unembedded_fields();
        let fields_ptr = self.fields_ptr();
        for i in 0..num_fields {
            unsafe {
                fields_ptr.add(i).as_ref().unwrap().visit_children(visitor);
            }
        }
        if let Some(embedded_vtable) = self.rtd.embedded_vtable {
            unsafe {
                (embedded_vtable.visit_children)(self.embedded_ptr().unwrap(), visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            self.rtd.finalize();
        }
        let num_fields = self.num_unembedded_fields();
        let fields_ptr = self.fields_ptr_mut();
        for i in 0..num_fields {
            unsafe {
                fields_ptr.add(i).as_mut().unwrap().finalize();
            }
        }
        if let Some(embedded_vtable) = self.rtd.embedded_vtable {
            unsafe {
                (embedded_vtable.finalize)(self.embedded_ptr_mut().unwrap());
            }
        }
    }
}

/// A Rust type that can be embedded safely in a Scheme record.
///
/// # Safety:
///
/// The [rtd] function cannot return a RecordTypeDescriptor created for a
/// different type than the type implementing that function. Doing so is
/// undefined behavior.
///
/// scheme-rs uses the layout of the type to compactly allocate types within
/// scheme records. Those layouts are stored in the RTD.
pub unsafe trait Embeddable: Trace + Any + Send + Sync {
    /// The Record Type Descriptor of the value. Can be constructed at runtime,
    /// but cannot change.
    fn rtd() -> Arc<RecordTypeDescriptor>
    where
        Self: Sized;

    /// Returns any parent records embedded in the Rust type.
    fn parent_record(&self, _rtd: &Arc<RecordTypeDescriptor>) -> Option<&dyn Embeddable> {
        None
    }

    /// Fetch the kth field of the record.
    fn get_field(&self, k: usize) -> Result<Value, Exception> {
        Err(Exception::error(format!("invalid record field: {k}")))
    }

    /// Set the kth field of the record.
    fn set_field(&self, k: usize, _val: Value) -> Result<(), Exception> {
        Err(Exception::error(format!("invalid record field: {k}")))
    }

    fn debug_fmt(
        &self,
        _circular_values: &mut IndexMap<Value, bool>,
        _fmt: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        Ok(())
    }

    fn display_fmt(
        &self,
        circular_values: &mut IndexMap<Value, bool>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        self.debug_fmt(circular_values, fmt)
    }

    fn eq(&self, rhs: &Record) -> bool
    where
        Self: Sized,
    {
        let Some(rhs) = rhs.cast::<Self>() else {
            return false;
        };
        std::ptr::eq(
            self as *const Self as *const (),
            rhs.embedded_ptr.as_ptr().cast::<()>(),
        )
    }

    fn eqv(&self, rhs: &Record) -> bool
    where
        Self: Sized,
    {
        self.eq(rhs)
    }

    fn equal(&self, rhs: &Record) -> bool
    where
        Self: Sized,
    {
        self.eqv(rhs)
    }

    fn eq_hash(&self, hasher: &mut dyn Hasher)
    where
        Self: Sized,
    {
        hasher.write_usize(self as *const Self as usize)
    }

    fn eqv_hash(&self, hasher: &mut dyn Hasher)
    where
        Self: Sized,
    {
        self.eq_hash(hasher)
    }

    fn equal_hash(&self, _recursive: &mut IndexSet<Value>, hasher: &mut dyn Hasher)
    where
        Self: Sized,
    {
        self.eqv_hash(hasher)
    }
}

// TODO: add trace(skip_all) attribute
#[derive(Copy, Clone, Trace)]
pub struct EmbeddableVTable {
    #[trace(skip)]
    pub type_id: TypeId,
    #[trace(skip)]
    layout: Layout,
    pub embedded_fields: usize,
    #[trace(skip)]
    pub visit_children: unsafe fn(*const (), &mut dyn FnMut(OpaqueGcPtr)),
    #[trace(skip)]
    pub finalize: unsafe fn(*mut ()),
    #[trace(skip)]
    pub parent_record: fn(*const (), &Arc<RecordTypeDescriptor>) -> Option<*const dyn Embeddable>,
    #[trace(skip)]
    pub get_field: fn(*const (), usize) -> Result<Value, Exception>,
    #[trace(skip)]
    pub set_field: fn(*const (), usize, Value) -> Result<(), Exception>,
    #[trace(skip)]
    pub display_fmt:
        fn(*const (), &mut IndexMap<Value, bool>, &mut fmt::Formatter<'_>) -> fmt::Result,
    #[trace(skip)]
    pub debug_fmt:
        fn(*const (), &mut IndexMap<Value, bool>, &mut fmt::Formatter<'_>) -> fmt::Result,
    #[trace(skip)]
    pub eq: for<'a> fn(*const (), &'a Record) -> bool,
    #[trace(skip)]
    pub eqv: for<'a> fn(*const (), &'a Record) -> bool,
    #[trace(skip)]
    pub equal: for<'a> fn(*const (), &'a Record) -> bool,
    #[trace(skip)]
    pub eq_hash: for<'a> fn(*const (), &'a mut dyn Hasher),
    #[trace(skip)]
    pub eqv_hash: for<'a> fn(*const (), &'a mut dyn Hasher),
    #[trace(skip)]
    pub equal_hash: for<'a> fn(*const (), &'a mut IndexSet<Value>, &'a mut dyn Hasher),
}

impl EmbeddableVTable {
    pub const fn new<E: Embeddable>(embedded_fields: usize) -> Self {
        Self {
            type_id: TypeId::of::<E>(),
            layout: Layout::new::<E>(),
            embedded_fields,
            visit_children: |this, visitor| unsafe {
                E::visit_children(this.cast::<E>().as_ref().unwrap(), visitor);
            },
            finalize: |this| unsafe {
                E::finalize(this.cast::<E>().as_mut().unwrap());
            },
            parent_record: |this, rtd| {
                E::parent_record(unsafe { this.cast::<E>().as_ref().unwrap() }, rtd)
                    .map(|r| r as *const dyn Embeddable)
            },
            get_field: |this, k| E::get_field(unsafe { this.cast::<E>().as_ref().unwrap() }, k),
            set_field: |this, k, val| {
                E::set_field(unsafe { this.cast::<E>().as_ref().unwrap() }, k, val)
            },
            display_fmt: |this, circ, fmt| {
                E::display_fmt(unsafe { this.cast::<E>().as_ref().unwrap() }, circ, fmt)
            },
            debug_fmt: |this, circ, fmt| {
                E::debug_fmt(unsafe { this.cast::<E>().as_ref().unwrap() }, circ, fmt)
            },
            eq: |lhs, rhs| E::eq(unsafe { lhs.cast::<E>().as_ref().unwrap() }, rhs),
            eqv: |lhs, rhs| E::eqv(unsafe { lhs.cast::<E>().as_ref().unwrap() }, rhs),
            equal: |lhs, rhs| E::equal(unsafe { lhs.cast::<E>().as_ref().unwrap() }, rhs),
            eq_hash: |this, hasher| {
                E::eq_hash(unsafe { this.cast::<E>().as_ref().unwrap() }, hasher)
            },
            eqv_hash: |this, hasher| {
                E::eqv_hash(unsafe { this.cast::<E>().as_ref().unwrap() }, hasher)
            },
            equal_hash: |this, rec, hasher| {
                E::equal_hash(unsafe { this.cast::<E>().as_ref().unwrap() }, rec, hasher)
            },
        }
    }

    fn ptr_to_parent(
        &self,
        ptr: *const (),
        rtd: &Arc<RecordTypeDescriptor>,
    ) -> Option<&dyn Embeddable> {
        (self.parent_record)(ptr, rtd).and_then(|ptr| unsafe { ptr.as_ref() })
    }
}

#[derive(Trace)]
pub struct Embedded<T> {
    #[trace(skip)]
    embedded_ptr: NonNull<T>,
    record: Record,
}

unsafe impl<T> Send for Embedded<T> {}
unsafe impl<T> Sync for Embedded<T> {}

impl<T> Clone for Embedded<T> {
    fn clone(&self) -> Self {
        Self {
            embedded_ptr: self.embedded_ptr,
            record: self.record.clone(),
        }
    }
}

impl<T: Embeddable> Embedded<T> {
    pub fn new(embeddable: T) -> Self {
        Record::embed(embeddable)
            .cast::<T>()
            .expect("freshly embedded value must cast back to its own type")
    }
}

impl<T> Embedded<T> {
    pub fn ptr_eq(lhs: &Self, rhs: &Self) -> bool {
        lhs.embedded_ptr == rhs.embedded_ptr
    }

    pub(crate) fn from_raw_parts(embedded_ptr: NonNull<T>, record: Record) -> Self {
        Self {
            embedded_ptr,
            record,
        }
    }
}

impl<T> Deref for Embedded<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.embedded_ptr.as_ref() }
    }
}

impl<T> AsRef<T> for Embedded<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.embedded_ptr.as_ref() }
    }
}

impl<T> fmt::Debug for Embedded<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl<T> From<&Value> for Option<Embedded<T>>
where
    T: Embeddable,
{
    fn from(value: &Value) -> Self {
        let UnpackedValue::Record(record) = value.clone().unpack() else {
            return None;
        };
        record.cast::<T>()
    }
}

impl<T> TryFrom<&Value> for Embedded<T>
where
    T: Embeddable,
{
    type Error = Exception;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        let type_name = T::rtd().name.to_str();
        let UnpackedValue::Record(record) = value.clone().unpack() else {
            return Err(Exception::type_error(&type_name, &value.type_name()));
        };
        let record_name = record.rtd().name.to_str();
        record
            .cast::<T>()
            .ok_or_else(|| Exception::type_error(&type_name, &record_name))
    }
}

impl<T> From<T> for Value
where
    T: Embeddable,
{
    fn from(value: T) -> Self {
        Value::from(Record::embed(value))
    }
}

impl<T> From<Embedded<T>> for Value
where
    T: Embeddable,
{
    fn from(value: Embedded<T>) -> Self {
        Value::from(value.record)
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

type ParentConstructor = fn(&[Value]) -> Result<ParentWriter, Exception>;

type ParentWriter = Box<dyn FnOnce(*mut ())>;

pub(crate) fn is_subtype_of(val: &Value, rt: Arc<RecordTypeDescriptor>) -> Result<bool, Exception> {
    let UnpackedValue::Record(rec) = val.clone().unpack() else {
        return Ok(false);
    };
    Ok(Arc::ptr_eq(&rec.0.rtd, &rt) || rec.0.rtd.inherits.contains(&ByAddress::from(rt)))
}

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
    let num_inherited_fields = inherits.last().map_or(0, |last_parent| {
        last_parent.num_inherited_fields + last_parent.fields.len()
    });
    let sealed = sealed.is_true();
    let opaque = opaque.is_true();
    let fields = Field::parse_fields(fields)?;

    // Inherit any embedded vtable or constructors:
    let (embedded_vtable, embedded_constructor) = inherits
        .last()
        .map(|rtd| (rtd.embedded_vtable, rtd.embedded_constructor))
        .unzip();

    let rtd = Arc::new(RecordTypeDescriptor {
        name,
        sealed,
        opaque,
        uid,
        inherits,
        num_inherited_fields,
        fields,
        embedded_vtable: embedded_vtable.flatten(),
        embedded_constructor: embedded_constructor.flatten(),
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
    parent: Option<Embedded<RecordConstructorDescriptor>>,
    rtd: Arc<RecordTypeDescriptor>,
    protocol: Procedure,
}

unsafe impl Embeddable for RecordConstructorDescriptor {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            ty: RecordConstructorDescriptor,
            name: "record-constructor-descriptor",
            sealed: true,
            opaque: true
        )
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
) -> Embedded<RecordConstructorDescriptor> {
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
    Embedded::new(RecordConstructorDescriptor {
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
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [rtd, parent_rcd, protocol] = args else {
        unreachable!();
    };

    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;

    if rtd.is_rust_type() && rtd.embedded_constructor.is_none() {
        return Err(Exception::error(format!(
            "cannot create a record-constructor-descriptor for rust type without a constructor {}",
            rtd.name
        )));
    }

    let parent_rcd = if parent_rcd.is_true() {
        let Some(parent_rtd) = rtd.inherits.last() else {
            return Err(Exception::error("rtd is a base type"));
        };
        let parent_rcd = parent_rcd.try_to::<Embedded<RecordConstructorDescriptor>>()?;
        if !Arc::ptr_eq(&parent_rcd.rtd, parent_rtd) {
            return Err(Exception::error("parent rtd does not match parent rcd"));
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

    Ok(Application::new(k, None, vec![Value::from(rcd)]))
}

#[cps_bridge(def = "record-constructor rcd", lib = "(rnrs records procedural (6))")]
pub fn record_constructor(
    runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [rcd] = args else {
        unreachable!();
    };
    let rcd = rcd.try_to::<Embedded<RecordConstructorDescriptor>>()?;

    let (protocols, rtds) = rcd_to_protocols_and_rtds(&rcd);

    let protocols = protocols.into_iter().map(Value::from).collect::<Vec<_>>();
    let rtds = rtds.into_iter().map(Value::from).collect::<Vec<_>>();
    let chain_protocols = Procedure::new_cont(
        runtime.clone(),
        vec![Value::from(protocols), Value::from(k)],
        chain_protocols,
        1,
        false,
        barrier,
    );

    Ok(chain_constructors(
        runtime,
        &[Value::from(rtds)],
        chain_protocols,
        &[],
        &[],
        barrier,
    ))
}

fn rcd_to_protocols_and_rtds(
    rcd: &Embedded<RecordConstructorDescriptor>,
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
    barrier: *mut ContBarrier,
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
                k.cast_to(),
                vec![args.as_ref().unwrap().clone()],
            )));
        }

        // Otherwise, turn the remaining chain into the continuation:
        let k1 = Procedure::new_cont(
            Runtime::from_raw_inc_rc(runtime),
            vec![Value::from(remaining_protocols), k],
            chain_protocols,
            1,
            false,
            barrier.as_mut().unwrap(),
        );

        Box::into_raw(Box::new(Application::new(
            curr_protocol,
            Some(k1),
            vec![args.as_ref().unwrap().clone()],
        )))
    }
}

#[cps_bridge]
fn chain_constructors(
    runtime: &Runtime,
    env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    // env[0] is a vector of RTDs
    let rtds: Vector = env[0].clone().try_into()?;
    let mut rtds = rtds.0.vec.read().clone();
    let remaining_rtds = rtds.split_off(1);
    let curr_rtd: Arc<RecordTypeDescriptor> = rtds[0].clone().try_into()?;
    let rtds_remain = !remaining_rtds.is_empty();
    let num_args = curr_rtd.fields.len();
    let env = if rtds_remain {
        vec![Value::from(remaining_rtds)]
    } else {
        vec![Value::from(curr_rtd)]
    }
    .into_iter()
    // Chain the current environment:
    .chain(env[1..].iter().cloned())
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
    Ok(Application::new(k, None, vec![Value::from(next_proc)]))
}

#[cps_bridge]
fn constructor(
    _runtime: &Runtime,
    env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let rtd: Arc<RecordTypeDescriptor> = env[0].clone().try_into()?;
    // The fields of the record are all of the env variables chained with
    // the arguments to this function.
    let mut fields = env[1..]
        .iter()
        .cloned()
        .chain(args.iter().cloned())
        .collect::<Vec<_>>();
    let (embedded_vtable_and_writer, fields) =
        if let Some(embedded_constructor) = rtd.embedded_constructor {
            let embedded_vtable = rtd.embedded_vtable.unwrap();
            let remaining_fields = fields.split_off(embedded_vtable.embedded_fields);
            // Call the rust constructor for the embedded type
            let writer = (embedded_constructor.constructor)(&fields)?;
            (Some((embedded_vtable, writer)), remaining_fields)
        } else {
            (None, fields)
        };

    let prefix = Layout::from_size_align(
        RecordInner::fields_offset(),
        align_of::<GcInner<RecordInner>>(),
    )
    .unwrap();
    let (layout, fields_offset) = prefix
        .extend(Layout::array::<Value>(fields.len()).unwrap())
        .unwrap();

    let (layout, embedded_offset_and_writer) =
        if let Some((embedded_vtable, writer)) = embedded_vtable_and_writer {
            let (layout, embed_offset) = layout.extend(embedded_vtable.layout).unwrap();
            (layout, Some((embed_offset, writer)))
        } else {
            (layout, None)
        };

    let layout = layout.pad_to_align();

    let record = unsafe {
        let record = alloc::alloc(layout) as *mut GcInner<RecordInner>;
        ptr::write(
            record,
            GcInner::new(RecordInner {
                rtd: rtd.clone(),
                fields: [],
            }),
        );

        let fields_ptr = record.byte_add(fields_offset) as *mut Value;

        let field_mutability = rtd
            .inherits
            .iter()
            .flat_map(|parent| parent.fields.iter())
            .chain(rtd.fields.iter())
            .map(Field::is_mutable)
            .skip(rtd.embedded_vtable.map_or(0, |vt| vt.embedded_fields));

        for (i, (field, mutable)) in fields.into_iter().zip(field_mutability).enumerate() {
            let field = if mutable {
                Value::from(Cell::new(field))
            } else {
                field
            };
            fields_ptr.add(i).write(field);
        }

        if let Some((embedded_offset, writer)) = embedded_offset_and_writer {
            (writer)(record.byte_add(embedded_offset) as *mut ());
        }

        let inner = Gc {
            ptr: NonNull::new(record).unwrap(),
            marker: PhantomData,
        };

        crate::gc::unroot(&inner, layout);

        Record(inner)
    };

    Ok(Application::new(k, None, vec![Value::from(record)]))
}

#[cps_bridge]
fn default_protocol(
    runtime: &Runtime,
    env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let rtd: Arc<RecordTypeDescriptor> = env[0].clone().try_into()?;
    let num_args = rtd.num_fields();

    let constructor = Procedure::new(
        runtime.clone(),
        vec![args[0].clone(), Value::from(rtd)],
        FuncPtr::Bridge(default_protocol_constructor),
        num_args,
        false,
    );

    Ok(Application::new(k, None, vec![Value::from(constructor)]))
}

#[cps_bridge]
fn default_protocol_constructor(
    runtime: &Runtime,
    env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let constructor: Procedure = env[0].clone().try_into()?;
    let rtd: Arc<RecordTypeDescriptor> = env[1].clone().try_into()?;

    let mut args = args.to_vec();
    let k = if let Some(parent) = rtd.inherits.last() {
        let remaining = args.split_off(parent.num_fields());
        Procedure::new_cont(
            runtime.clone(),
            vec![Value::from(remaining), Value::from(k)],
            call_constructor_continuation,
            1,
            false,
            barrier,
        )
    } else {
        k
    };

    Ok(Application::new(constructor, Some(k), args))
}

pub(crate) unsafe extern "C" fn call_constructor_continuation(
    _runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    _barrier: *mut ContBarrier,
) -> *mut Application {
    unsafe {
        let constructor: Procedure = args.as_ref().unwrap().clone().try_into().unwrap();
        let args: Vector = env.as_ref().unwrap().clone().try_into().unwrap();
        let args = args.0.vec.read().clone();
        let cont = env.add(1).as_ref().unwrap();

        // Call the constructor
        Box::into_raw(Box::new(Application::new(
            constructor,
            cont.cast_to(),
            args,
        )))
    }
}

#[cps_bridge]
fn record_predicate_fn(
    _runtime: &Runtime,
    env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [val] = args else {
        unreachable!();
    };
    // RTD is the first environment variable:
    let rtd: Arc<RecordTypeDescriptor> = env[0].try_to()?;
    Ok(Application::new(
        k,
        None,
        vec![Value::from(is_subtype_of(val, rtd)?)],
    ))
}

#[cps_bridge(def = "record-predicate rtd", lib = "(rnrs records procedural (6))")]
pub fn record_predicate(
    runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
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
    Ok(Application::new(k, None, vec![Value::from(pred_fn)]))
}

#[cps_bridge]
fn record_accessor_fn(
    _runtime: &Runtime,
    env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [val] = args else {
        unreachable!();
    };
    let record: Record = val.clone().try_into()?;
    let rtd: Arc<RecordTypeDescriptor> = env[0].try_to()?;
    if !is_subtype_of(val, rtd.clone())? {
        return Err(Exception::error("not a child of this record type"));
    }
    let local_idx: usize = env[1].clone().try_into()?;
    let abs_idx = local_idx + rtd.num_inherited_fields;
    let val = if abs_idx < record.0.num_embedded_fields() {
        // The field lives inside the embedded Rust value.
        let embedded_ptr = record.0.embedded_ptr().unwrap();
        let embedded_vtable = record.0.rtd.embedded_vtable.as_ref().unwrap();
        if rtd.embedded_vtable.unwrap().type_id == embedded_vtable.type_id {
            (embedded_vtable.get_field)(embedded_ptr, local_idx)?
        } else {
            let mut embedded = embedded_vtable.ptr_to_parent(embedded_ptr, &rtd).unwrap();
            while let Some(parent) = embedded.parent_record(&rtd) {
                embedded = parent;
            }
            embedded.get_field(local_idx)?
        }
    } else {
        let k = abs_idx - record.0.num_embedded_fields();
        if let Some(cell) = record.0.fields()[k].cast_to::<Cell>() {
            cell.get()
        } else {
            record.0.fields()[k].clone()
        }
    };
    if val.is_undefined() {
        return Err(Exception::error(format!(
            "failed to get field: {}, {local_idx}",
            rtd.name
        )));
    }
    Ok(Application::new(k, None, vec![val]))
}

#[cps_bridge(def = "record-accessor rtd k", lib = "(rnrs records procedural (6))")]
pub fn record_accessor(
    runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [rtd, idx] = args else {
        unreachable!();
    };
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    let idx: usize = idx.clone().try_into()?;
    if idx >= rtd.fields.len() {
        return Err(Exception::error(format!(
            "{idx} is out of range 0..{}",
            rtd.fields.len()
        )));
    }
    // Store the local (within-rtd) index; `record_accessor_fn` resolves it to
    // either the embed or an inline slot.
    let accessor_fn = Procedure::new(
        runtime.clone(),
        vec![Value::from(rtd), Value::from(idx)],
        FuncPtr::Bridge(record_accessor_fn),
        1,
        false,
    );
    Ok(Application::new(k, None, vec![Value::from(accessor_fn)]))
}

#[cps_bridge]
fn record_mutator_fn(
    _runtime: &Runtime,
    env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [rec, new_val] = args else {
        unreachable!();
    };
    let record: Record = rec.clone().try_into()?;
    // RTD is the first environment variable, the field's local index the second.
    let rtd: Arc<RecordTypeDescriptor> = env[0].try_to()?;
    if !is_subtype_of(rec, rtd.clone())? {
        return Err(Exception::error("not a child of this record type"));
    }
    let local_idx: usize = env[1].clone().try_into()?;
    let abs_idx = local_idx + rtd.num_inherited_fields;
    if abs_idx < record.0.num_embedded_fields() {
        // The field lives inside the embedded Rust value.
        let embedded_ptr = record.0.embedded_ptr().unwrap();
        let embedded_vtable = record.0.rtd.embedded_vtable.as_ref().unwrap();
        if rtd.embedded_vtable.unwrap().type_id == embedded_vtable.type_id {
            (embedded_vtable.set_field)(embedded_ptr, local_idx, new_val.clone())?;
        } else {
            let mut embedded = embedded_vtable.ptr_to_parent(embedded_ptr, &rtd).unwrap();
            while let Some(parent) = embedded.parent_record(&rtd) {
                embedded = parent;
            }
            embedded.set_field(local_idx, new_val.clone())?;
        }
    } else {
        let slot = abs_idx - record.0.num_embedded_fields();
        record.0.fields()[slot]
            .try_to::<Cell>()?
            .set(new_val.clone());
    }
    Ok(Application::new(k, None, Vec::new()))
}

#[cps_bridge(def = "record-mutator rtd k", lib = "(rnrs records procedural (6))")]
pub fn record_mutator(
    runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [rtd, idx] = args else {
        unreachable!();
    };
    let rtd: Arc<RecordTypeDescriptor> = rtd.clone().try_into()?;
    let idx: usize = idx.clone().try_into()?;
    if idx >= rtd.fields.len() {
        return Err(Exception::error(format!(
            "{idx} is out of range {}",
            rtd.fields.len()
        )));
    }
    if matches!(rtd.fields[idx], Field::Immutable(_)) {
        return Err(Exception::error(format!("{idx} is immutable")));
    }
    let mutator_fn = Procedure::new(
        runtime.clone(),
        vec![Value::from(rtd), Value::from(idx)],
        FuncPtr::Bridge(record_mutator_fn),
        2,
        false,
    );
    Ok(Application::new(k, None, vec![Value::from(mutator_fn)]))
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
pub fn record_type_name(rtd: Arc<RecordTypeDescriptor>) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(rtd.name)])
}

#[bridge(name = "record-type-parent", lib = "(rnrs records inspection (6))")]
pub fn record_type_parent(rtd: Arc<RecordTypeDescriptor>) -> Result<Vec<Value>, Exception> {
    if let Some(parent) = rtd.inherits.last() {
        Ok(vec![Value::from(parent.0.clone())])
    } else {
        Ok(vec![Value::from(false)])
    }
}

#[bridge(name = "record-type-uid", lib = "(rnrs records inspection (6))")]
pub fn record_type_uid(rtd: Arc<RecordTypeDescriptor>) -> Result<Vec<Value>, Exception> {
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
pub fn record_type_generative_pred(
    rtd: Arc<RecordTypeDescriptor>,
) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(rtd.uid.is_none())])
}

#[bridge(name = "record-type-sealed?", lib = "(rnrs records inspection (6))")]
pub fn record_type_sealed_pred(rtd: Arc<RecordTypeDescriptor>) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(rtd.sealed)])
}

#[bridge(name = "record-type-opaque?", lib = "(rnrs records inspection (6))")]
pub fn record_type_opaque_pred(rtd: Arc<RecordTypeDescriptor>) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(rtd.opaque)])
}

#[bridge(
    name = "record-type-field-names",
    lib = "(rnrs records inspection (6))"
)]
pub fn record_type_field_names(rtd: Arc<RecordTypeDescriptor>) -> Result<Vec<Value>, Exception> {
    let fields = rtd
        .fields
        .iter()
        .map(Field::name)
        .map(Value::from)
        .collect::<Vec<_>>();
    Ok(vec![Value::from(fields)])
}

#[bridge(name = "record-field-mutable?", lib = "(rnrs records inspection (6))")]
pub fn record_field_mutable_pred(
    rtd: Arc<RecordTypeDescriptor>,
    k: usize,
) -> Result<Vec<Value>, Exception> {
    if k >= rtd.fields.len() {
        return Err(Exception::invalid_index(k, rtd.fields.len()));
    }

    Ok(vec![Value::from(matches!(
        rtd.fields[k],
        Field::Mutable(_)
    ))])
}
