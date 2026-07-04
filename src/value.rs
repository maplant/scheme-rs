//! Scheme values.
//!
//! Scheme values are dynamic and can contain essentially any value, similar to
//! an [`Arc<dyn Any>`](std::any::Any). Any type that has a valid scheme
//! representation (see [scheme types](#scheme-types) for more information) can
//! be converted easily to a Scheme `Value`. Converting a Rust primitive or
//! standard library type can be done simply with the `From` trait:
//!
//! ```
//! # use scheme_rs::value::Value;
//! let value = Value::from(3.1415926f64);
//! ```
//!
//! Converting a Value back to a concrete rust type can be done via the
//! [`TryFrom`] trait or by obtaining an enum through the
//! [`unpack`](Value::unpack) or [`unpacked_ref`](Value::unpacked_ref)
//! functions:
//!
//! ```
//! # use scheme_rs::value::{Value, UnpackedValue};
//! # let value = Value::from(3.1415926f64);
//! let float: f64 = value.clone().try_into().unwrap();
//! let float: f64 = match value.unpack() {
//!     UnpackedValue::Number(num) => num.try_into().unwrap(),
//!     _ => unreachable!(),
//! };
//! ```
//!
//! It is generally preferrable to use `try_into` as opposed to `unpack` since
//! `UnpackedValue` is an enumeration that is subject to change.
//!
//! Alternatively, the [`cast_to`](Value::cast_to)
//! method can be used to obtain an Option from a reference for more ergonomic
//! casting. There is also the [`try_to`](Value::try_to)
//! function that is similarly more ergonomic:
//!
//! ```
//! # use scheme_rs::value::{Value, UnpackedValue};
//! let value = Value::from(3);
//! let float = value.cast_to::<f64>().unwrap();
//! let int = value.cast_to::<i64>().unwrap();
//! ```
//!
//! ## Converting to and from arbitrary Rust types
//!
//! Besides primitives and standard library types, scheme-rs supports converting
//! arbitrary Rust types (such as structs and enums) to `Values` by embedding
//! them inside [`Records`](Record). To do this, the type must implement the
//! [`Embeddable`](records::Embeddable) trait (see [`records`](scheme_rs::records)
//! for more information).
//!
//! These conversions are performed with the standard conversion traits:
//! - `Value::from(t)`: embed an [`Embeddable`](records::Embeddable) value into a
//!   record, and then convert it to a `Value`.
//! - [`try_to`](Value::try_to)`::<Embedded<T>>()`: attempt to convert the value
//!   to an [`Embedded<T>`](records::Embedded), providing a detailed error on
//!   failure.
//! - [`cast_to`](Value::cast_to)`::<Embedded<T>>()`: attempt to convert the
//!   value to an [`Embedded<T>`](records::Embedded), returning `None` on
//!   failure.
//!
//! ## Scheme types
//!
//! Scheme values can inhabit at most one of any of the following types:
//! - **Undefined**: Variables with this value throw an error upon being read.
//! - **Null**: Can only be one possible value which is itself. Conceptually the
//!   same as the [`()`](https://doc.rust-lang.org/std/primitive.unit.html) type.
//! - **Pair**: A [collection of two Values](Pair). Conceptually similar to a
//!   Rust [two-tuple](https://doc.rust-lang.org/std/primitive.tuple.html).
//! - **Boolean**: Can either be `true` or `false`.
//! - **Character**: A unicode code point. Same thing as a [`char`](std::char).
//! - **Number**: A numerical value on the numerical tower. Represented by a
//!   [`Arc<Number>`](crate::num::Number).
//! - **String**: An array of [`chars`](std::char).
//! - **Symbol**: A [`Symbol`].
//! - **Vector**: A [`Vector`].
//! - **Syntax**: A [`Syntax`].
//! - **Procedure**: A [`Procedure`].
//! - **Record**: A [`Record`], which can possibly embed an
//!   [`Embeddable`](records::Embeddable) Rust value.
//! - **Record Type Descriptor**: A [descriptor of a record's type](RecordTypeDescriptor).
//! - **Cell**: A mutable reference to another Value. This type is completely
//!   transparent and impossible to observe.

use indexmap::{IndexMap, IndexSet};
use malachite::Integer;
use parking_lot::RwLock;

use crate::{
    exceptions::Exception,
    gc::{Gc, GcInner, Trace},
    lists::{self, Pair, PairInner},
    num::{ComplexNumber, Number, NumberInner, SimpleNumber},
    proc::{Procedure, ProcedureInner},
    records::{Embedded, Record, RecordInner, RecordTypeDescriptor},
    registry::bridge,
    strings::WideString,
    symbols::Symbol,
    syntax::{Identifier, Syntax},
    vectors::{self, ByteVector, Vector, VectorInner},
};
use std::{
    collections::HashMap,
    convert::Infallible,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem::ManuallyDrop,
    ops::Deref,
    ptr::null,
    sync::Arc,
};

const ALIGNMENT: usize = 16;
const TAG_BITS: usize = ALIGNMENT.ilog2() as usize;
pub(crate) const TAG: usize = 0b1111;
pub(crate) const SYMBOL_CHAR: u32 = 0x110000;
pub(crate) const NULL_VALUE: usize = Tag::Pair as usize;
pub(crate) const TRUE_VALUE: usize = Tag::Boolean as usize | 1 << TAG_BITS;
pub(crate) const FALSE_VALUE: usize = Tag::Boolean as usize;

/// A Scheme value. See [the module documentation](scheme_rs::value) for more
/// information.
#[repr(transparent)]
pub struct Value(*const ());

impl Value {
    /// Create a new `Value` from an `UnpackedValue`.
    ///
    /// This is generally discouraged as it's cumbersome; try using `From`
    /// instead.
    pub fn new(v: UnpackedValue) -> Self {
        v.into_value()
    }

    /// #f is false, everything else is true
    pub fn is_true(&self) -> bool {
        self.0 as usize != FALSE_VALUE
    }

    pub fn is_null(&self) -> bool {
        self.0 as usize == Tag::Pair as usize
    }

    pub fn is_undefined(&self) -> bool {
        self.type_of() == ValueType::Undefined
    }

    /// Creates a new Value from a raw u64.
    ///
    /// # Safety
    /// Calling this function is undefined behavior if the raw u64 was not obtained
    /// via [into_raw](Value::into_raw)
    pub unsafe fn from_raw(raw: *const ()) -> Self {
        Self(raw)
    }

    /// Creates a new Value from a raw u64, incrementing the reference count.
    ///
    /// # Safety
    /// Calling this function is undefined behavior if the raw u64 was not obtained
    /// via [into_raw](Value::into_raw)
    pub unsafe fn from_raw_inc_rc(raw: *const ()) -> Self {
        let tag = Tag::from(raw as usize & TAG);
        let untagged = raw.map_addr(|raw| raw & !TAG);
        unsafe {
            match tag {
                Tag::Number => Arc::increment_strong_count(untagged as *const NumberInner),
                Tag::Procedure => {
                    Gc::increment_reference_count(untagged as *mut GcInner<ProcedureInner>)
                }
                Tag::Record => {
                    if !untagged.is_null() {
                        Gc::increment_reference_count(untagged as *mut GcInner<RecordInner>)
                    }
                }
                Tag::RecordTypeDescriptor => {
                    Arc::increment_strong_count(untagged as *const RecordTypeDescriptor)
                }
                Tag::Pair => {
                    if !untagged.is_null() {
                        Gc::increment_reference_count(untagged as *mut GcInner<PairInner>)
                    }
                }
                Tag::Cell => {
                    Gc::increment_reference_count(untagged as *mut GcInner<Value>);
                }
                Tag::Boolean | Tag::CharacterOrSymbol => (),
            }
        }
        Self(raw)
    }

    /// Creates a raw u64 from a Value. Does not decrement the reference count.
    /// Calling this function without turning the raw value into a Value via
    /// [from_raw](Value::from_raw) is equivalent to calling mem::forget on the
    /// value.
    pub fn into_raw(val: Self) -> *const () {
        ManuallyDrop::new(val).0
    }

    /// Creates a raw u64 from the Value. Does not decrement the reference count.
    pub fn as_raw(this: &Self) -> *const () {
        this.0
    }

    fn from_ptr_and_tag<T: Send + Sync>(ptr: *const T, tag: Tag) -> Self {
        Self(ptr.map_addr(|raw| raw | tag as usize) as *const ())
    }

    pub(crate) fn from_mut_ptr_and_tag<T: Send + Sync>(ptr: *mut T, tag: Tag) -> Self {
        Self(ptr.map_addr(|raw| raw | tag as usize) as *const ())
    }

    pub fn undefined() -> Self {
        Self(null::<()>().map_addr(|raw| raw | Tag::Record as usize))
    }

    pub fn null() -> Self {
        Self(null::<()>().map_addr(|raw| raw | Tag::Pair as usize))
    }

    /// Convert a [`Syntax`] into its corresponding datum representation.
    pub fn datum_from_syntax(syntax: &Syntax) -> Self {
        match syntax {
            Syntax::Wrapped { value, .. } => value.clone(),
            Syntax::List { list, .. } => {
                let mut curr = Self::datum_from_syntax(list.last().unwrap());
                for item in list[..list.len() - 1].iter().rev() {
                    curr = Self::from(Pair::immutable(Self::datum_from_syntax(item), curr));
                }
                curr
            }
            Syntax::Vector { vector, .. } => Self::from(
                vector
                    .iter()
                    .map(Self::datum_from_syntax)
                    .collect::<Vec<_>>(),
            ),
            Syntax::Identifier { ident, .. } => Self::new(UnpackedValue::Symbol(ident.sym)),
        }
    }

    pub fn type_of(&self) -> ValueType {
        self.unpacked_ref().type_of()
    }

    pub fn type_name(&self) -> Arc<str> {
        self.unpacked_ref().type_name()
    }

    /// Attempt to cast the value.
    pub fn cast_to<T>(&self) -> Option<T>
    where
        for<'a> &'a Self: Into<Option<T>>,
    {
        self.into()
    }

    pub fn is_a<T>(&self) -> bool
    where
        for<'a> &'a Self: Into<Option<T>>,
    {
        self.into().is_some()
    }

    /// Attempt to cast the value and return a descriptive error on failure.
    pub fn try_to<T>(&self) -> Result<T, Exception>
    where
        T: for<'a> TryFrom<&'a Self, Error = Exception>,
    {
        self.try_into()
    }

    /// Unpack the value into an enum representation.
    pub fn unpack(self) -> UnpackedValue {
        let raw = ManuallyDrop::new(self).0;
        let tag = Tag::from(raw as usize & TAG);
        let untagged = raw.map_addr(|raw| raw & !TAG);
        match tag {
            // Tag::Undefined => UnpackedValue::Undefined,
            Tag::Boolean => {
                let untagged = untagged as usize >> TAG_BITS;
                UnpackedValue::Boolean(untagged != 0)
            }
            Tag::CharacterOrSymbol => {
                let untagged_char = (untagged as usize as u32) >> TAG_BITS;
                if untagged_char == SYMBOL_CHAR {
                    // Upper 32 bits used for symbols
                    UnpackedValue::Symbol(Symbol((untagged as usize >> 32) as u32))
                } else {
                    // Lower 32 bits used for character
                    UnpackedValue::Character(char::from_u32(untagged_char).unwrap())
                }
            }
            Tag::Number => {
                let number = unsafe { Arc::from_raw(untagged as *const NumberInner) };
                UnpackedValue::Number(Number(number))
            }
            Tag::Procedure => {
                let clos = unsafe { Gc::from_raw(untagged as *mut GcInner<ProcedureInner>) };
                UnpackedValue::Procedure(Procedure(clos))
            }
            Tag::Record => {
                if untagged.is_null() {
                    UnpackedValue::Undefined
                } else {
                    let rec = unsafe { Gc::from_raw(untagged as *mut GcInner<RecordInner>) };
                    UnpackedValue::Record(Record(rec))
                }
            }
            Tag::RecordTypeDescriptor => {
                let rt = unsafe { Arc::from_raw(untagged as *const RecordTypeDescriptor) };
                UnpackedValue::RecordTypeDescriptor(rt)
            }
            Tag::Pair => {
                if untagged.is_null() {
                    UnpackedValue::Null
                } else {
                    let pair = unsafe { Gc::from_raw(untagged as *mut GcInner<PairInner>) };
                    UnpackedValue::Pair(Pair(pair))
                }
            }
            Tag::Cell => {
                let cell = unsafe { Gc::from_raw(untagged as *mut GcInner<RwLock<Value>>) };
                UnpackedValue::Cell(Cell(cell))
            }
        }
    }

    pub fn unpacked_ref(&self) -> UnpackedValueRef<'_> {
        let unpacked = ManuallyDrop::new(Value(self.0).unpack());
        UnpackedValueRef {
            unpacked,
            marker: PhantomData,
        }
    }

    pub fn display_fmt(
        &self,
        circular_values: &mut IndexMap<Value, bool>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self.clone().unpack() {
            UnpackedValue::Undefined => write!(f, "<undefined>"),
            UnpackedValue::Null => write!(f, "()"),
            UnpackedValue::Boolean(true) => write!(f, "#t"),
            UnpackedValue::Boolean(false) => write!(f, "#f"),
            UnpackedValue::Number(number) => write!(f, "{number}"),
            UnpackedValue::Character(c) => write!(f, "#\\{c}"),
            UnpackedValue::Symbol(symbol) => write!(f, "{symbol}"),
            UnpackedValue::Pair(pair) => {
                let (car, cdr) = pair.into();
                lists::write_list(&car, &cdr, Value::display_fmt, circular_values, f)
            }
            UnpackedValue::Procedure(_) => write!(f, "<procedure>"),
            UnpackedValue::Record(record) => record.display_fmt(circular_values, f),
            UnpackedValue::RecordTypeDescriptor(rtd) => write!(f, "{rtd:?}"),
            UnpackedValue::Cell(cell) => cell.0.read().display_fmt(circular_values, f),
        }
    }

    pub fn debug_fmt(
        &self,
        circular_values: &mut IndexMap<Value, bool>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self.clone().unpack() {
            UnpackedValue::Undefined => write!(f, "<undefined>"),
            UnpackedValue::Null => write!(f, "()"),
            UnpackedValue::Boolean(true) => write!(f, "#t"),
            UnpackedValue::Boolean(false) => write!(f, "#f"),
            UnpackedValue::Number(number) => write!(f, "{number}"),
            UnpackedValue::Character(c) => write!(f, "#\\{c}"),
            UnpackedValue::Symbol(symbol) => write!(f, "{symbol}"),
            UnpackedValue::Pair(pair) => {
                let (car, cdr) = pair.into();
                lists::write_list(&car, &cdr, Value::debug_fmt, circular_values, f)
            }
            UnpackedValue::Procedure(proc) => write!(f, "#<procedure {proc:?}>"),
            UnpackedValue::Record(record) => record.debug_fmt(circular_values, f),
            UnpackedValue::RecordTypeDescriptor(rtd) => write!(f, "{rtd:?}"),
            UnpackedValue::Cell(cell) => cell.0.read().debug_fmt(circular_values, f),
        }
    }

    /// The eq? predicate as defined by the R6RS specification.
    #[allow(clippy::should_implement_trait)]
    pub fn eq(&self, rhs: &Self) -> bool {
        let obj1 = self.unpacked_ref();
        let obj2 = rhs.unpacked_ref();
        obj1.eq(&obj2)
    }

    /// The eqv? predicate as defined by the R6RS specification.
    pub fn eqv(&self, rhs: &Self) -> bool {
        let obj1 = self.unpacked_ref();
        let obj2 = rhs.unpacked_ref();
        obj1.eqv(&obj2)
    }

    /// The equal? predicate as defined by the R6RS specification.
    pub fn equal(&self, rhs: &Self) -> bool {
        equal(self, rhs)
    }

    /// Performs a hash suitable for use with eq? as an equivalance function
    pub fn eq_hash<H: Hasher>(&self, state: &mut H) {
        let unpacked = self.unpacked_ref();
        std::mem::discriminant(&*unpacked).hash(state);
        match &*unpacked {
            UnpackedValue::Undefined => (),
            UnpackedValue::Null => (),
            UnpackedValue::Boolean(b) => b.hash(state),
            UnpackedValue::Character(c) => c.hash(state),
            UnpackedValue::Number(n) => Arc::as_ptr(&n.0).hash(state),
            UnpackedValue::Symbol(s) => s.hash(state),
            UnpackedValue::Procedure(c) => Gc::as_ptr(&c.0).hash(state),
            UnpackedValue::Record(r) => r.eq_hash(state),
            UnpackedValue::RecordTypeDescriptor(rt) => Arc::as_ptr(rt).hash(state),
            UnpackedValue::Pair(p) => Gc::as_ptr(&p.0).hash(state),
            UnpackedValue::Cell(c) => c.0.read().eqv_hash(state),
        }
    }

    /// Performs a hash suitable for use with eqv? as an equivalance function
    pub fn eqv_hash<H: Hasher>(&self, state: &mut H) {
        let unpacked = self.unpacked_ref();
        std::mem::discriminant(&*unpacked).hash(state);
        match &*unpacked {
            UnpackedValue::Undefined => (),
            UnpackedValue::Null => (),
            UnpackedValue::Boolean(b) => b.hash(state),
            UnpackedValue::Character(c) => c.hash(state),
            UnpackedValue::Number(n) => n.hash(state),
            UnpackedValue::Symbol(s) => s.hash(state),
            UnpackedValue::Procedure(c) => Gc::as_ptr(&c.0).hash(state),
            UnpackedValue::Record(r) => r.eqv_hash(state),
            UnpackedValue::RecordTypeDescriptor(rt) => Arc::as_ptr(rt).hash(state),
            UnpackedValue::Pair(p) => Gc::as_ptr(&p.0).hash(state),
            UnpackedValue::Cell(c) => c.0.read().eqv_hash(state),
        }
    }

    /// Performs a hash suitable for use with equal? as an equivalance function
    pub fn equal_hash<H: Hasher>(&self, recursive: &mut IndexSet<Value>, state: &mut H) {
        let unpacked = self.unpacked_ref();
        std::mem::discriminant(&*unpacked).hash(state);

        // I think this is fine, because types that would be recursive will
        // write out at least two values here where we're only writing out one.
        if let Some(index) = recursive.get_index_of(self) {
            state.write_usize(index);
            return;
        }

        match &*unpacked {
            UnpackedValue::Undefined => (),
            UnpackedValue::Null => (),
            UnpackedValue::Boolean(b) => b.hash(state),
            UnpackedValue::Character(c) => c.hash(state),
            UnpackedValue::Number(n) => n.hash(state),
            UnpackedValue::Symbol(s) => s.hash(state),
            UnpackedValue::Procedure(c) => Gc::as_ptr(&c.0).hash(state),
            UnpackedValue::Record(r) => {
                recursive.insert(self.clone());
                r.equal_hash(recursive, state);
            }
            UnpackedValue::RecordTypeDescriptor(rt) => Arc::as_ptr(rt).hash(state),
            UnpackedValue::Pair(p) => {
                recursive.insert(self.clone());
                let (car, cdr) = p.clone().into();
                car.equal_hash(recursive, state);
                cdr.equal_hash(recursive, state);
            }
            UnpackedValue::Cell(c) => c.0.read().eqv_hash(state),
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        unsafe { Self::from_raw_inc_rc(self.0) }
    }
}

impl Drop for Value {
    fn drop(&mut self) {
        // FIXME: This is a pretty dumb way to do this, do it manually!
        unsafe { ManuallyDrop::drop(&mut ManuallyDrop::new(Self(self.0).unpack())) }
    }
}

/// Default Hash implementation for Value is [Value::eqv_hash]. This produces
/// reasonable hash maps.
impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.eqv_hash(state)
    }
}

/// Default PartialEq implementation for Value is [Value::eqv]. This allows us
/// to implement [Eq].
impl PartialEq for Value {
    fn eq(&self, rhs: &Value) -> bool {
        self.eqv(rhs)
    }
}

/// Eq can be implemented reasonably for Value since (eqv? +nan.0 +nan.0) is
/// true
impl Eq for Value {}

unsafe impl Send for Value {}
unsafe impl Sync for Value {}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut circular_values = IndexSet::default();
        determine_circularity(self, &mut IndexSet::default(), &mut circular_values);
        let mut circular_values = circular_values.into_iter().map(|k| (k, false)).collect();
        write_value(self, Value::display_fmt, &mut circular_values, f)
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut circular_values = IndexSet::default();
        determine_circularity(self, &mut IndexSet::default(), &mut circular_values);
        let mut circular_values = circular_values.into_iter().map(|k| (k, false)).collect();
        write_value(self, Value::debug_fmt, &mut circular_values, f)
    }
}

unsafe impl Trace for Value {
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(crate::gc::OpaqueGcPtr)) {
        unsafe {
            self.unpacked_ref().visit_children(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe { ManuallyDrop::new(Self(self.0).unpack()).finalize() }
    }
}

/// A Cell is a value that is mutable, essentially a variable.
#[derive(Clone, Trace)]
pub struct Cell(pub(crate) Gc<RwLock<Value>>);

impl Cell {
    pub fn new(val: Value) -> Self {
        Self(Gc::new(RwLock::new(val)))
    }

    pub fn get(&self) -> Value {
        self.0.read().clone()
    }

    pub fn set(&self, new_val: Value) {
        *self.0.write() = new_val;
    }
}

impl From<&Value> for Option<Cell> {
    fn from(val: &Value) -> Option<Cell> {
        match val.clone().unpack() {
            UnpackedValue::Cell(cell) => Some(cell),
            _ => None,
        }
    }
}

/// A reference to an [`UnpackedValue`]. Allows for unpacking a `Value` without
/// cloning/modifying the reference count.
pub struct UnpackedValueRef<'a> {
    unpacked: ManuallyDrop<UnpackedValue>,
    marker: PhantomData<&'a UnpackedValue>,
}

impl Deref for UnpackedValueRef<'_> {
    type Target = UnpackedValue;

    fn deref(&self) -> &Self::Target {
        &self.unpacked
    }
}

impl AsRef<UnpackedValue> for UnpackedValueRef<'_> {
    fn as_ref(&self) -> &UnpackedValue {
        &self.unpacked
    }
}

impl<T> From<Option<T>> for Value
where
    Value: From<T>,
    Value: From<bool>,
{
    // Probably not the best way to do this, but whatever
    fn from(value: Option<T>) -> Self {
        match value {
            Some(t) => Self::from(t),
            None => Self::from(false),
        }
    }
}

impl From<Exception> for Value {
    fn from(value: Exception) -> Self {
        value.0
    }
}

/*

1: Symbol,
2: Pair,
3: Boolean,
4: Character,
5: Number,
6: Procedure,
7: Record,
8: Cell,

*/

#[repr(u64)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Tag {
    Pair = 1,
    Boolean = 2,
    CharacterOrSymbol = 3,
    Number = 4,
    Procedure = 10,
    Record = 11,
    RecordTypeDescriptor = 12,
    Cell = 15,
}

// TODO: Make TryFrom with error
impl From<usize> for Tag {
    fn from(tag: usize) -> Self {
        match tag {
            1 => Self::Pair,
            2 => Self::Boolean,
            3 => Self::CharacterOrSymbol,
            4 => Self::Number,
            10 => Self::Procedure,
            11 => Self::Record,
            12 => Self::RecordTypeDescriptor,
            15 => Self::Cell,
            tag => panic!("Invalid tag: {tag}"),
        }
    }
}

/// Different possible types that a Value can inhabit.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueType {
    Undefined,
    Null,
    Pair,
    Boolean,
    Character,
    Number,
    Symbol,
    Procedure,
    Record,
    RecordTypeDescriptor,
}

/// The external, unpacked, enumeration representation of a scheme value.
///
/// Values that are not potentially cyclical, such as syntax objects and byte
/// vectors use Arcs as they are much less expensive than Gc types.
#[non_exhaustive]
#[derive(Trace, Clone)]
pub enum UnpackedValue {
    Undefined,
    Null,
    Boolean(bool),
    Character(char),
    Number(Number),
    Symbol(Symbol),
    Procedure(Procedure),
    Record(Record),
    RecordTypeDescriptor(Arc<RecordTypeDescriptor>),
    Pair(Pair),
    Cell(Cell),
}

impl UnpackedValue {
    pub fn into_value(self) -> Value {
        match self {
            Self::Undefined => Value::undefined(),
            Self::Null => Value::null(),
            Self::Boolean(b) => {
                Value::from_ptr_and_tag(((b as usize) << TAG_BITS) as *const (), Tag::Boolean)
            }
            Self::Character(c) => {
                Value::from_ptr_and_tag(((c as usize) << TAG_BITS) as *const (), Tag::CharacterOrSymbol)
            }
            Self::Number(num) => {
                let untagged = Arc::into_raw(num.0);
                Value::from_ptr_and_tag(untagged, Tag::Number)
            }
            Self::Symbol(sym) => {
                Value::from_ptr_and_tag((((sym.0 as usize) << 32) | (SYMBOL_CHAR as usize) << TAG_BITS) as *const (), Tag::CharacterOrSymbol)
            }
            Self::Procedure(clos) => {
                let untagged = Gc::into_raw(clos.0);
                Value::from_mut_ptr_and_tag(untagged, Tag::Procedure)
            }
            Self::Record(rec) => {
                let untagged = Gc::into_raw(rec.0);
                Value::from_mut_ptr_and_tag(untagged, Tag::Record)
            }
            Self::RecordTypeDescriptor(rt) => {
                let untagged = Arc::into_raw(rt);
                Value::from_ptr_and_tag(untagged, Tag::RecordTypeDescriptor)
            }
            Self::Pair(pair) => {
                let untagged = Gc::into_raw(pair.0);
                Value::from_mut_ptr_and_tag(untagged, Tag::Pair)
            }
            Self::Cell(cell) => {
                let untagged = Gc::into_raw(cell.0);
                Value::from_mut_ptr_and_tag(untagged, Tag::Cell)
            }
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => Arc::ptr_eq(&a.0, &b.0),
            (Self::Character(a), Self::Character(b)) => a == b,
            (Self::Null, Self::Null) => true,
            (Self::Pair(a), Self::Pair(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Procedure(a), Self::Procedure(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Record(a), Self::Record(b)) => a.eq(b),
            (Self::RecordTypeDescriptor(a), Self::RecordTypeDescriptor(b)) => Arc::ptr_eq(a, b),
            (Self::Cell(a), b) => a.0.read().unpacked_ref().eq(b),
            (a, Self::Cell(b)) => a.eq(&b.0.read().unpacked_ref()),
            _ => false,
        }
    }

    pub fn eqv(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            // Undefined is equivalent to undefined since it is impossible to
            // read
            (Self::Undefined, Self::Undefined) => true,
            // boolean=?
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            // symbol=?
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a.eqv(b),
            // char=?
            (Self::Character(a), Self::Character(b)) => a == b,
            // Both obj1 and obj2 are the empty list
            (Self::Null, Self::Null) => true,
            // Everything else is pointer equivalence
            (Self::Pair(a), Self::Pair(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Procedure(a), Self::Procedure(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Record(a), Self::Record(b)) => a.eqv(b),
            (Self::RecordTypeDescriptor(a), Self::RecordTypeDescriptor(b)) => Arc::ptr_eq(a, b),
            (Self::Cell(a), b) => a.0.read().unpacked_ref().eqv(b),
            (a, Self::Cell(b)) => a.eqv(&b.0.read().unpacked_ref()),
            _ => false,
        }
    }

    pub fn type_name(&self) -> Arc<str> {
        match self {
            Self::Undefined => Symbol::intern("undefined").to_str(),
            Self::Boolean(_) => Symbol::intern("bool").to_str(),
            Self::Number(_) => Symbol::intern("number").to_str(),
            Self::Character(_) => Symbol::intern("character").to_str(),
            Self::Symbol(_) => Symbol::intern("symbol").to_str(),
            Self::Pair(_) | Self::Null => Symbol::intern("pair").to_str(),
            Self::Procedure(_) => Symbol::intern("procedure").to_str(),
            Self::Record(record) => record.rtd().name.to_str(),
            Self::RecordTypeDescriptor(_) => Symbol::intern("rtd").to_str(),
            Self::Cell(cell) => cell.0.read().type_name(),
        }
    }

    pub fn type_of(&self) -> ValueType {
        match self {
            Self::Undefined => ValueType::Undefined,
            Self::Null => ValueType::Null,
            Self::Boolean(_) => ValueType::Boolean,
            Self::Number(_) => ValueType::Number,
            Self::Character(_) => ValueType::Character,
            Self::Symbol(_) => ValueType::Symbol,
            Self::Pair(_) => ValueType::Pair,
            Self::Procedure(_) => ValueType::Procedure,
            Self::Record(_) => ValueType::Record,
            Self::RecordTypeDescriptor(_) => ValueType::RecordTypeDescriptor,
            Self::Cell(cell) => cell.0.read().type_of(),
        }
    }
}

/// Determine if two objects are equal in an extremely granular sense.
///
/// This implementation is a Rust translation of Efficient Dondestructive
/// Equality Checking for Trees and Graphs by Michael D. Adams and R. Kent
/// Dybvig.
pub fn equal(obj1: &Value, obj2: &Value) -> bool {
    interleave(&mut HashMap::default(), obj1, obj2, K0)
}

const K0: i64 = 400;
const KB: i64 = -40;

fn interleave(ht: &mut HashMap<Value, Value>, obj1: &Value, obj2: &Value, k: i64) -> bool {
    e(ht, obj1, obj2, k).is_some()
}

fn e(ht: &mut HashMap<Value, Value>, obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    match k {
        KB => fast(ht, obj1, obj2, rand::random_range(0..(K0 * 2))),
        k if k <= 0 => slow(ht, obj1, obj2, k),
        k => fast(ht, obj1, obj2, k),
    }
}

fn fast(ht: &mut HashMap<Value, Value>, obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    let k = k - 1;
    if obj1.eqv(obj2) {
        return Some(k);
    }
    match (obj1.type_of(), obj2.type_of()) {
        (ValueType::Pair, ValueType::Pair) => pair_eq(ht, obj1, obj2, k),
        (ValueType::Record, ValueType::Record) => record_equal(ht, obj1.cast_to()?, obj2.cast_to()?, k),
        _ => None,
    }
}

fn slow(ht: &mut HashMap<Value, Value>, obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    if obj1.eqv(obj2) {
        return Some(k);
    }
    match (obj1.type_of(), obj2.type_of()) {
        (ValueType::Pair, ValueType::Pair) => {
            if union_find(ht, obj1, obj2) {
                return Some(0);
            }
            pair_eq(ht, obj1, obj2, k)
        }
        // (ValueType::ByteVector, ValueType::ByteVector) => bytevector_eq(obj1, obj2, k),
        (ValueType::Record, ValueType::Record) => {
            if union_find(ht, obj1, obj2) {
                Some(0)
            } else {
                record_equal(ht, obj1.cast_to()?, obj2.cast_to()?, k)
            }
        }
        _ => None,
    }
}

fn pair_eq(ht: &mut HashMap<Value, Value>, obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    let obj1: Pair = obj1.clone().try_into().unwrap();
    let obj2: Pair = obj2.clone().try_into().unwrap();
    let (car_x, cdr_x) = obj1.into();
    let (car_y, cdr_y) = obj2.into();
    e(ht, &car_x, &car_y, k - 1).and_then(|k| e(ht, &cdr_x, &cdr_y, k))
}

fn vector_eq(ht: &mut HashMap<Value, Value>, vobj1: Embedded<VectorInner<Value>>, vobj2: Embedded<VectorInner<Value>>, k: i64) -> Option<i64> {
    let vobj1 = vobj1.vec.read();
    let vobj2 = vobj2.vec.read();
    if vobj1.len() != vobj2.len() {
        return None;
    }
    let mut k = k - 1;
    for (x, y) in vobj1.iter().zip(vobj2.iter()) {
        k = e(ht, x, y, k)?;
    }
    Some(k)
}

/*
fn bytevector_eq(obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    let obj1: ByteVector = obj1.clone().try_into().unwrap();
    let obj2: ByteVector = obj2.clone().try_into().unwrap();
    (*obj1.0.vec.read() == *obj2.0.vec.read()).then_some(k)
}
*/

fn record_equal(ht: &mut HashMap<Value, Value>, obj1: Record, obj2: Record, k: i64) -> Option<i64> {
    if let Some(vobj1) = obj1.cast::<VectorInner<Value>>() {
        obj2.cast::<VectorInner<Value>>().map_or(None, |vobj2| vector_eq(ht, vobj1, vobj2, k))
    } else {
        /*
        let obj1: Record = obj1.clone().try_into().unwrap();
        let obj2: Record = obj2.clone().try_into().unwrap();
        */
        (obj1.equal(&obj2)).then_some(k)
    }
}

fn union_find(ht: &mut HashMap<Value, Value>, x: &Value, y: &Value) -> bool {
    let bx = ht.get(x).cloned();
    let by = ht.get(y).cloned();
    match (bx, by) {
        (None, None) => {
            let b = boxv(Value::from(Number::from(1)));
            ht.insert(x.clone(), b.clone());
            ht.insert(y.clone(), b);
        }
        (None, Some(by)) => {
            let ry = find(by);
            ht.insert(x.clone(), ry);
        }
        (Some(bx), None) => {
            let rx = find(bx);
            ht.insert(y.clone(), rx);
        }
        (Some(bx), Some(by)) => {
            let rx = find(bx);
            let ry = find(by);
            if rx.eqv(&ry) {
                return true;
            }
            let nx = unbox_to_num(&rx);
            let ny = unbox_to_num(&ry);
            if nx > ny {
                set_box(&ry, rx.clone());
                set_box(&rx, nx + ny);
            } else {
                set_box(&rx, ry.clone());
                set_box(&ry, nx + ny);
            }
        }
    }
    false
}

fn find(mut b: Value) -> Value {
    let mut n = unbox(&b);
    if is_box(&n) {
        loop {
            let nn = unbox(&n);
            if !is_box(&nn) {
                return n;
            }
            set_box(&b, nn.clone());
            b = n;
            n = nn;
        }
    } else {
        b
    }
}

fn boxv(v: Value) -> Value {
    Value::from(Pair::mutable(v.clone(), Value::null()))
}

fn unbox(v: &Value) -> Value {
    let pair: Pair = v.clone().try_into().unwrap();
    pair.car()
}

fn unbox_to_num(v: &Value) -> Number {
    let pair: Pair = v.clone().try_into().unwrap();
    pair.car().try_into().unwrap()
}

fn is_box(v: &Value) -> bool {
    v.type_of() == ValueType::Pair
}

fn set_box(b: &Value, val: impl Into<Value>) {
    let pair: Pair = b.clone().try_into().unwrap();
    pair.set_car(val.into()).unwrap();
}

macro_rules! impl_try_from_value_for {
    ($ty:ty, $variant:ident, $type_name:literal) => {
        impl From<$ty> for UnpackedValue {
            fn from(v: $ty) -> Self {
                Self::$variant(v)
            }
        }

        impl From<$ty> for Value {
            fn from(v: $ty) -> Self {
                UnpackedValue::from(v).into_value()
            }
        }

        impl From<UnpackedValue> for Option<$ty> {
            fn from(v: UnpackedValue) -> Self {
                match v {
                    UnpackedValue::$variant(v) => Some(v),
                    _ => None,
                }
            }
        }

        impl From<Value> for Option<$ty> {
            fn from(v: Value) -> Self {
                v.unpack().into()
            }
        }

        impl From<&'_ Value> for Option<$ty> {
            fn from(v: &Value) -> Self {
                v.clone().unpack().into()
            }
        }

        impl TryFrom<UnpackedValue> for $ty {
            type Error = Exception;

            fn try_from(v: UnpackedValue) -> Result<Self, Self::Error> {
                match v {
                    UnpackedValue::$variant(v) => Ok(v),
                    UnpackedValue::Cell(cell) => cell.0.read().clone().try_into(),
                    e => Err(Exception::type_error($type_name, &*e.type_name())),
                }
            }
        }

        impl TryFrom<Value> for $ty {
            type Error = Exception;

            fn try_from(v: Value) -> Result<Self, Self::Error> {
                v.unpack().try_into()
            }
        }

        impl TryFrom<&Value> for $ty {
            type Error = Exception;

            fn try_from(v: &Value) -> Result<Self, Self::Error> {
                v.clone().unpack().try_into()
            }
        }
    };
}

impl From<Infallible> for Value {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

impl From<()> for UnpackedValue {
    fn from((): ()) -> Self {
        Self::Null
    }
}

impl From<()> for Value {
    fn from((): ()) -> Self {
        UnpackedValue::Null.into_value()
    }
}

impl TryFrom<UnpackedValue> for () {
    type Error = Exception;

    fn try_from(value: UnpackedValue) -> Result<Self, Self::Error> {
        match value {
            UnpackedValue::Null => Ok(()),
            e => Err(Exception::type_error("null", &e.type_name())),
        }
    }
}

impl TryFrom<Value> for () {
    type Error = Exception;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value.unpack().try_into()
    }
}

impl From<Cell> for UnpackedValue {
    fn from(cell: Cell) -> Self {
        Self::Cell(cell)
    }
}

impl From<Cell> for Value {
    fn from(cell: Cell) -> Self {
        UnpackedValue::from(cell).into_value()
    }
}

impl TryFrom<UnpackedValue> for Cell {
    type Error = Exception;

    fn try_from(v: UnpackedValue) -> Result<Self, Self::Error> {
        match v {
            UnpackedValue::Cell(cell) => Ok(cell.clone()),
            e => Err(Exception::type_error("cell", &*e.type_name())),
        }
    }
}

impl TryFrom<Value> for Cell {
    type Error = Exception;

    fn try_from(v: Value) -> Result<Self, Self::Error> {
        v.unpack().try_into()
    }
}

impl TryFrom<&Value> for Cell {
    type Error = Exception;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        v.clone().unpack().try_into()
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        value.is_true()
    }
}

impl From<bool> for UnpackedValue {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        UnpackedValue::from(value).into_value()
    }
}

// impl_try_from_value_for!(bool, Boolean, "bool");

impl_try_from_value_for!(char, Character, "char");
impl_try_from_value_for!(Number, Number, "number");
impl_try_from_value_for!(Symbol, Symbol, "symbol");
impl_try_from_value_for!(Procedure, Procedure, "procedure");
impl_try_from_value_for!(Pair, Pair, "pair");
impl_try_from_value_for!(Record, Record, "record");
impl_try_from_value_for!(Arc<RecordTypeDescriptor>, RecordTypeDescriptor, "rt");

macro_rules! impl_from_wrapped_for {
    ($ty:ty, $variant:ident, $wrapper:expr_2021) => {
        impl From<$ty> for UnpackedValue {
            fn from(v: $ty) -> Self {
                Self::$variant(($wrapper)(v))
            }
        }

        impl From<$ty> for Value {
            fn from(v: $ty) -> Self {
                UnpackedValue::from(v).into_value()
            }
        }
    };
}

impl_from_wrapped_for!((Value, Value), Pair, |(car, cdr)| Pair::immutable(car, cdr));

impl From<UnpackedValue> for Option<(Value, Value)> {
    fn from(val: UnpackedValue) -> Self {
        match val {
            UnpackedValue::Pair(pair) => Some(pair.into()),
            _ => None,
        }
    }
}

impl TryFrom<UnpackedValue> for (Value, Value) {
    type Error = Exception;

    fn try_from(val: UnpackedValue) -> Result<Self, Self::Error> {
        match val {
            UnpackedValue::Pair(pair) => Ok(pair.into()),
            e => Err(Exception::type_error("pair", &e.type_name())),
        }
    }
}

macro_rules! impl_num_conversion {
    ($ty:ty) => {
        // TODO: Can we reverse these?
        impl TryFrom<&Value> for $ty {
            type Error = Exception;

            fn try_from(value: &Value) -> Result<$ty, Self::Error> {
                match &*value.unpacked_ref() {
                    UnpackedValue::Number(num) => num.try_into(),
                    e => Err(Exception::type_error("number", &e.type_name())),
                }
            }
        }

        impl TryFrom<Value> for $ty {
            type Error = Exception;

            fn try_from(value: Value) -> Result<$ty, Self::Error> {
                (&value).try_into()
            }
        }

        impl From<&Value> for Option<$ty> {
            fn from(value: &Value) -> Self {
                match &*value.unpacked_ref() {
                    UnpackedValue::Number(num) => num.into(),
                    _ => None,
                }
            }
        }

        impl From<Value> for Option<$ty> {
            fn from(value: Value) -> Self {
                match value.unpack() {
                    UnpackedValue::Number(num) => num.into(),
                    _ => None,
                }
            }
        }

        impl From<$ty> for Value {
            fn from(n: $ty) -> Self {
                Self::from(Number::from(n))
            }
        }
    };
}

impl_num_conversion!(u8);
impl_num_conversion!(u16);
impl_num_conversion!(u32);
impl_num_conversion!(u64);
impl_num_conversion!(u128);
impl_num_conversion!(usize);
impl_num_conversion!(i8);
impl_num_conversion!(i16);
impl_num_conversion!(i32);
impl_num_conversion!(i64);
impl_num_conversion!(i128);
impl_num_conversion!(isize);
impl_num_conversion!(f64);
impl_num_conversion!(Integer);
impl_num_conversion!(SimpleNumber);
impl_num_conversion!(ComplexNumber);

impl From<Value> for Option<(Value, Value)> {
    fn from(value: Value) -> Self {
        value.unpack().into()
    }
}

impl From<&Value> for Option<(Value, Value)> {
    fn from(value: &Value) -> Self {
        value.clone().unpack().into()
    }
}

impl TryFrom<Value> for (Value, Value) {
    type Error = Exception;

    fn try_from(val: Value) -> Result<Self, Self::Error> {
        Self::try_from(val.unpack())
    }
}

impl TryFrom<&Value> for (Value, Value) {
    type Error = Exception;

    fn try_from(val: &Value) -> Result<Self, Self::Error> {
        Self::try_from(val.clone().unpack())
    }
}

impl TryFrom<Value> for String {
    type Error = Exception;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let string: WideString = value.try_into()?;
        Ok(string.into())
    }
}

/// Trait for converting vecs of values into arrays
pub trait ExpectN<T> {
    fn expect_n<const N: usize>(self) -> Result<[T; N], Exception>;
}

impl<T> ExpectN<T> for Vec<Value>
where
    Value: TryInto<T>,
    Exception: From<<Value as TryInto<T>>::Error>,
{
    fn expect_n<const N: usize>(self) -> Result<[T; N], Exception> {
        if self.len() != N {
            return Err(Exception::error("wrong number of values"));
        }
        // Safety: we've already determined that self is the correct size, so we
        // can safely use unwrap_unchecked
        Ok(unsafe {
            self.into_iter()
                .map(Value::try_into)
                .collect::<Result<Vec<_>, _>>()?
                .try_into()
                .unwrap_unchecked()
        })
    }
}

/// Trait for converting vecs of values into one type
pub trait Expect1<T> {
    fn expect1(self) -> Result<T, Exception>;
}

impl<T> Expect1<T> for Vec<Value>
where
    Value: TryInto<T>,
    Exception: From<<Value as TryInto<T>>::Error>,
{
    fn expect1(self) -> Result<T, Exception> {
        let [val] = self
            .try_into()
            .map_err(|_| Exception::error("wrong number of values"))?;
        val.try_into().map_err(Exception::from)
    }
}

/// Determines which children of the given list are circular, i.e. have children
/// that refer to back to them. This is just a depth-first search.
fn determine_circularity(
    curr: &Value,
    visited: &mut IndexSet<Value>,
    circular: &mut IndexSet<Value>,
) {
    if visited.contains(curr) {
        circular.insert(curr.clone());
        return;
    }

    visited.insert(curr.clone());

    match curr.clone().unpack() {
        UnpackedValue::Pair(pair) => {
            let (car, cdr) = pair.into();
            determine_circularity(&car, visited, circular);
            determine_circularity(&cdr, visited, circular);
        }
        UnpackedValue::Record(rec) if let Some(vec) = rec.cast::<VectorInner<Value>>() => {
            let vec_read = vec.vec.read();
            for item in vec_read.iter() {
                determine_circularity(item, visited, circular);
            }
        }
        _ => (),
    }

    visited.swap_remove(curr);
}

pub(crate) fn write_value(
    val: &Value,
    fmt: fn(&Value, &mut IndexMap<Value, bool>, &mut fmt::Formatter<'_>) -> fmt::Result,
    circular_values: &mut IndexMap<Value, bool>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    if let Some((idx, _, seen)) = circular_values.get_full_mut(val) {
        if *seen {
            write!(f, "#{idx}#")?;
            return Ok(());
        } else {
            write!(f, "#{idx}=")?;
            *seen = true;
        }
    }

    fmt(val, circular_values, f)
}

#[bridge(name = "not", lib = "(rnrs base builtins (6))")]
pub fn not(a: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(a.0 as usize == Tag::Boolean as usize)])
}

#[bridge(name = "eq?", lib = "(rnrs base builtins (6))")]
pub fn eq(a: &Value, b: &Value) -> bool {
    a.eq(b)
}
#[bridge(name = "eqv?", lib = "(rnrs base builtins (6))")]
pub fn eqv(a: &Value, b: &Value) -> bool {
    a.eqv(b)
}

#[bridge(name = "equal?", lib = "(rnrs base builtins (6))")]
pub fn equal_pred(a: &Value, b: &Value) -> bool {
    a.equal(b)
}

#[bridge(name = "boolean?", lib = "(rnrs base builtins (6))")]
pub fn boolean_pred(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Boolean)])
}

#[bridge(name = "boolean=?", lib = "(rnrs base builtins (6))")]
pub fn boolean_eq_pred(a: &Value, args: &[Value]) -> Result<Vec<Value>, Exception> {
    let res = if a.type_of() == ValueType::Boolean {
        args.iter().all(|arg| arg == a)
    } else {
        false
    };
    Ok(vec![Value::from(res)])
}

#[bridge(name = "symbol?", lib = "(rnrs base builtins (6))")]
pub fn symbol_pred(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Symbol)])
}

#[bridge(name = "char?", lib = "(rnrs base builtins (6))")]
pub fn char_pred(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Character)])
}

#[bridge(name = "null?", lib = "(rnrs base builtins (6))")]
pub fn null_pred(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Null)])
}

#[bridge(name = "pair?", lib = "(rnrs base builtins (6))")]
pub fn pair_pred(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(matches!(
        *arg.unpacked_ref(),
        UnpackedValue::Pair(_)
    ))])
}

#[bridge(name = "procedure?", lib = "(rnrs base builtins (6))")]
pub fn procedure_pred(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Procedure)])
}
