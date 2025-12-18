use indexmap::{IndexMap, IndexSet};
use parking_lot::RwLock;

use crate::{
    ast,
    exceptions::{Condition, Exception},
    gc::{Gc, GcInner, Trace},
    hashtables::{self, HashTable, HashTableInner},
    lists::{self, Pair, PairInner},
    num::Number,
    ports::{Port, PortInner},
    proc::{Procedure, ProcedureInner},
    records::{Record, RecordInner, RecordTypeDescriptor, SchemeCompatible},
    registry::bridge,
    strings::{WideString, WideStringInner},
    symbols::Symbol,
    syntax::Syntax,
    vectors::{self, ByteVector, Vector, VectorInner},
};
use std::{
    collections::HashMap, fmt, hash::Hash, io::Write, marker::PhantomData, mem::ManuallyDrop,
    ops::Deref, ptr::null, sync::Arc,
};

const ALIGNMENT: usize = 16;
const TAG_BITS: usize = ALIGNMENT.ilog2() as usize;
const TAG: usize = 0b1111;
const FALSE_VALUE: usize = Tag::Boolean as usize;

/// A Scheme value. Represented as a tagged pointer.
#[repr(transparent)]
pub struct Value(*const ());

impl Value {
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

    /// Creates a new Value from a raw u64.
    ///
    /// # Safety
    /// Calling this function is undefined behavior if the raw u64 was not obtained
    /// via [into_raw]
    pub unsafe fn from_raw(raw: *const ()) -> Self {
        Self(raw)
    }

    /// Creates a new Value from a raw u64, incrementing the reference count.
    ///
    /// # Safety
    /// Calling this function is undefined behavior if the raw u64 was not obtained
    /// via [into_raw]
    pub unsafe fn from_raw_inc_rc(raw: *const ()) -> Self {
        let tag = Tag::from(raw as usize & TAG);
        let untagged = raw.map_addr(|raw| raw & !TAG);
        unsafe {
            match tag {
                Tag::Number => Arc::increment_strong_count(untagged as *const Number),
                Tag::String => Arc::increment_strong_count(untagged as *const WideStringInner),
                Tag::Vector => {
                    Gc::increment_reference_count(untagged as *mut GcInner<VectorInner<Value>>)
                }
                Tag::ByteVector => Arc::increment_strong_count(untagged as *const VectorInner<u8>),
                Tag::Syntax => Gc::increment_reference_count(untagged as *mut GcInner<Syntax>),
                Tag::Procedure => {
                    Gc::increment_reference_count(untagged as *mut GcInner<ProcedureInner>)
                }
                Tag::Record => Gc::increment_reference_count(untagged as *mut GcInner<RecordInner>),
                Tag::RecordTypeDescriptor => {
                    Arc::increment_strong_count(untagged as *const RecordTypeDescriptor)
                }
                Tag::Pair => {
                    if !untagged.is_null() {
                        Gc::increment_reference_count(untagged as *mut GcInner<PairInner>)
                    }
                }
                Tag::Port => Arc::increment_strong_count(untagged as *const PortInner),
                Tag::HashTable => Gc::increment_reference_count(
                    untagged as *mut GcInner<hashtables::HashTableInner>,
                ),
                Tag::Cell => {
                    Gc::increment_reference_count(untagged as *mut GcInner<Value>);
                }
                Tag::Undefined | Tag::Symbol | Tag::Boolean | Tag::Character => (),
            }
        }
        Self(raw)
    }

    /// Creates a raw u64 from a Value. Does not decrement the reference count.
    /// Calling this function without turning the raw value into a Value via
    /// [from_raw] is equivalent to calling mem::forget on the value.
    pub fn into_raw(val: Self) -> *const () {
        ManuallyDrop::new(val).0
    }

    /// Creates a raw u64 from the Value. Does not decrement the reference count.
    pub fn as_raw(this: &Self) -> *const () {
        this.0
    }

    fn from_ptr_and_tag<T>(ptr: *const T, tag: Tag) -> Self {
        Self(ptr.map_addr(|raw| raw | tag as usize) as *const ())
    }

    fn from_mut_ptr_and_tag<T>(ptr: *mut T, tag: Tag) -> Self {
        Self(ptr.map_addr(|raw| raw | tag as usize) as *const ())
        // Self(ptr as u64 | tag as u64)
    }

    pub fn undefined() -> Self {
        Self(null::<()>().map_addr(|raw| raw | Tag::Undefined as usize))
    }

    pub fn null() -> Self {
        Self(null::<()>().map_addr(|raw| raw | Tag::Pair as usize))
    }

    pub fn datum_from_syntax(syntax: &Syntax) -> Self {
        match syntax {
            Syntax::Null { .. } => Self::null(),
            Syntax::List { list, .. } => {
                let mut curr = Self::datum_from_syntax(list.last().unwrap());
                for item in list[..list.len() - 1].iter().rev() {
                    curr = Self::from(Pair::new(Self::datum_from_syntax(item), curr, false));
                }
                curr
            }
            Syntax::Vector { vector, .. } => Self::from(
                vector
                    .iter()
                    .map(Self::datum_from_syntax)
                    .collect::<Vec<_>>(),
            ),
            Syntax::ByteVector { vector, .. } => Self::from(vector.clone()),
            Syntax::Literal { literal, .. } => Self::from(literal.clone()),
            Syntax::Identifier { ident, .. } => Self::new(UnpackedValue::Symbol(ident.sym)),
        }
    }

    pub fn type_of(&self) -> ValueType {
        self.unpacked_ref().type_of()
    }

    pub fn type_name(&self) -> &'static str {
        self.unpacked_ref().type_name()
    }

    pub fn try_into_rust_type<T: SchemeCompatible>(&self) -> Result<Gc<T>, Condition> {
        let this = self.clone().unpack();
        let record = match this {
            UnpackedValue::Record(record) => record,
            e => return Err(Condition::type_error("record-todo", e.type_name())),
        };

        record
            .try_into_rust_type::<T>()
            .ok_or_else(|| Condition::type_error("record-todo", "record"))
    }

    pub fn unpack(self) -> UnpackedValue {
        let raw = ManuallyDrop::new(self).0;
        let tag = Tag::from(raw as usize & TAG);
        let untagged = raw.map_addr(|raw| raw & !TAG);
        match tag {
            Tag::Undefined => UnpackedValue::Undefined,
            Tag::Boolean => {
                let untagged = untagged as usize >> TAG_BITS;
                UnpackedValue::Boolean(untagged != 0)
            }
            Tag::Character => {
                let untagged = (untagged as usize >> TAG_BITS) as u32;
                UnpackedValue::Character(char::from_u32(untagged).unwrap())
            }
            Tag::Number => {
                let number = unsafe { Arc::from_raw(untagged as *const Number) };
                UnpackedValue::Number(number)
            }
            Tag::String => {
                let str = unsafe { Arc::from_raw(untagged as *const WideStringInner) };
                UnpackedValue::String(WideString(str))
            }
            Tag::Symbol => {
                let untagged = (untagged as usize >> TAG_BITS) as u32;
                UnpackedValue::Symbol(Symbol(untagged))
            }
            Tag::Vector => {
                let vec = unsafe { Gc::from_raw(untagged as *mut GcInner<VectorInner<Self>>) };
                UnpackedValue::Vector(Vector(vec))
            }
            Tag::ByteVector => {
                let bvec = unsafe { Arc::from_raw(untagged as *const VectorInner<u8>) };
                UnpackedValue::ByteVector(ByteVector(bvec))
            }
            Tag::Syntax => {
                let syn = unsafe { Gc::from_raw(untagged as *mut GcInner<Syntax>) };
                UnpackedValue::Syntax(syn)
            }
            Tag::Procedure => {
                let clos = unsafe { Gc::from_raw(untagged as *mut GcInner<ProcedureInner>) };
                UnpackedValue::Procedure(Procedure(clos))
            }
            Tag::Record => {
                let rec = unsafe { Gc::from_raw(untagged as *mut GcInner<RecordInner>) };
                UnpackedValue::Record(Record(rec))
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
            Tag::Port => {
                let port_inner = unsafe { Arc::from_raw(untagged as *const PortInner) };
                UnpackedValue::Port(Port(port_inner))
            }
            Tag::HashTable => {
                let ht = unsafe { Gc::from_raw(untagged as *mut GcInner<HashTableInner>) };
                UnpackedValue::HashTable(HashTable(ht))
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

/// Default Hash implementation for Value is [Value::hash_eqv]. This produces
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
        write_value(self, display_value, &mut circular_values, f)
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut circular_values = IndexSet::default();
        determine_circularity(self, &mut IndexSet::default(), &mut circular_values);
        let mut circular_values = circular_values.into_iter().map(|k| (k, false)).collect();
        write_value(self, debug_value, &mut circular_values, f)
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

impl From<ast::Literal> for Value {
    fn from(lit: ast::Literal) -> Self {
        Value::new(lit.into())
    }
}

impl From<Exception> for Value {
    fn from(exception: Exception) -> Self {
        // Until we can decide on a good method for including the stack trace with
        // the new condition, just return the object.
        exception.obj
    }
}

impl From<Condition> for Value {
    fn from(value: Condition) -> Self {
        value.0
    }
}

#[repr(u64)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Tag {
    Undefined = 0,
    Pair = 1,
    Boolean = 2,
    Character = 3,
    Number = 4,
    String = 5,
    Symbol = 6,
    Vector = 7,
    ByteVector = 8,
    Syntax = 9,
    Procedure = 10,
    Record = 11,
    RecordTypeDescriptor = 12,
    HashTable = 13,
    Port = 14,
    Cell = 15,
}

// TODO: Make TryFrom with error
impl From<usize> for Tag {
    fn from(tag: usize) -> Self {
        match tag {
            0 => Self::Undefined,
            1 => Self::Pair,
            2 => Self::Boolean,
            3 => Self::Character,
            4 => Self::Number,
            5 => Self::String,
            6 => Self::Symbol,
            7 => Self::Vector,
            8 => Self::ByteVector,
            9 => Self::Syntax,
            10 => Self::Procedure,
            11 => Self::Record,
            12 => Self::RecordTypeDescriptor,
            13 => Self::HashTable,
            14 => Self::Port,
            15 => Self::Cell,
            tag => panic!("Invalid tag: {tag}"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueType {
    Undefined,
    Null,
    Pair,
    Boolean,
    Character,
    Number,
    String,
    Symbol,
    Vector,
    ByteVector,
    Syntax,
    Procedure,
    Record,
    RecordTypeDescriptor,
    HashTable,
    Port,
}

/// The external, unpacked representation of a scheme value.
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
    Number(Arc<Number>),
    String(WideString),
    Symbol(Symbol),
    Vector(Vector),
    ByteVector(ByteVector),
    Syntax(Gc<Syntax>),
    Procedure(Procedure),
    Record(Record),
    RecordTypeDescriptor(Arc<RecordTypeDescriptor>),
    Pair(Pair),
    Port(Port),
    HashTable(hashtables::HashTable),
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
                Value::from_ptr_and_tag(((c as usize) << TAG_BITS) as *const (), Tag::Character)
            }
            Self::Number(num) => {
                let untagged = Arc::into_raw(num);
                Value::from_ptr_and_tag(untagged, Tag::Number)
            }
            Self::String(str) => {
                let untagged = Arc::into_raw(str.0);
                Value::from_ptr_and_tag(untagged, Tag::String)
            }
            Self::Symbol(sym) => {
                Value::from_ptr_and_tag(((sym.0 as usize) << TAG_BITS) as *const (), Tag::Symbol)
            }
            Self::Vector(vec) => {
                let untagged = Gc::into_raw(vec.0);
                Value::from_mut_ptr_and_tag(untagged, Tag::Vector)
            }
            Self::ByteVector(b_vec) => {
                let untagged = Arc::into_raw(b_vec.0);
                Value::from_ptr_and_tag(untagged, Tag::ByteVector)
            }
            Self::Syntax(syn) => {
                let untagged = Gc::into_raw(syn);
                Value::from_mut_ptr_and_tag(untagged, Tag::Syntax)
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
            Self::Port(port) => {
                let untagged = Arc::into_raw(port.0);
                Value::from_ptr_and_tag(untagged, Tag::Port)
            }
            Self::HashTable(ht) => {
                let untagged = Gc::into_raw(ht.0);
                Value::from_ptr_and_tag(untagged, Tag::HashTable)
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
            (Self::Number(a), Self::Number(b)) => Arc::ptr_eq(a, b),
            (Self::Character(a), Self::Character(b)) => a == b,
            (Self::Null, Self::Null) => true,
            (Self::String(a), Self::String(b)) => Arc::ptr_eq(&a.0, &b.0),
            (Self::Pair(a), Self::Pair(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Vector(a), Self::Vector(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::ByteVector(a), Self::ByteVector(b)) => Arc::ptr_eq(&a.0, &b.0),
            (Self::Procedure(a), Self::Procedure(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Syntax(a), Self::Syntax(b)) => Gc::ptr_eq(a, b),
            (Self::Record(a), Self::Record(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::RecordTypeDescriptor(a), Self::RecordTypeDescriptor(b)) => Arc::ptr_eq(a, b),
            (Self::Port(a), Self::Port(b)) => Arc::ptr_eq(&a.0, &b.0),
            (Self::HashTable(a), Self::HashTable(b)) => Gc::ptr_eq(&a.0, &b.0),
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
            // Numbers are only equivalent if they're the same exactness
            // Two NaNs are also treated as equivalent
            (Self::Number(a), Self::Number(b)) => {
                (a.is_nan() && b.is_nan()) || (a.is_exact() == b.is_exact() && a == b)
            }
            // char=?
            (Self::Character(a), Self::Character(b)) => a == b,
            // Both obj1 and obj2 are the empty list
            (Self::Null, Self::Null) => true,
            // Everything else is pointer equivalence
            (Self::String(a), Self::String(b)) => Arc::ptr_eq(&a.0, &b.0),
            (Self::Pair(a), Self::Pair(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Vector(a), Self::Vector(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::ByteVector(a), Self::ByteVector(b)) => Arc::ptr_eq(&a.0, &b.0),
            (Self::Procedure(a), Self::Procedure(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Syntax(a), Self::Syntax(b)) => Gc::ptr_eq(a, b),
            (Self::Record(a), Self::Record(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::RecordTypeDescriptor(a), Self::RecordTypeDescriptor(b)) => Arc::ptr_eq(a, b),
            (Self::Port(a), Self::Port(b)) => Arc::ptr_eq(&a.0, &b.0),
            (Self::HashTable(a), Self::HashTable(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Cell(a), b) => a.0.read().unpacked_ref().eqv(b),
            (a, Self::Cell(b)) => a.eqv(&b.0.read().unpacked_ref()),
            _ => false,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Undefined => "undefined",
            Self::Boolean(_) => "bool",
            Self::Number(_) => "number",
            Self::Character(_) => "character",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Pair(_) | Self::Null => "pair",
            Self::Vector(_) => "vector",
            Self::ByteVector(_) => "byte vector",
            Self::Syntax(_) => "syntax",
            Self::Procedure(_) => "procedure",
            Self::Record(_) => "record",
            Self::RecordTypeDescriptor(_) => "rtd",
            Self::Port(_) => "port",
            Self::HashTable(_) => "hashtable",
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
            Self::String(_) => ValueType::String,
            Self::Symbol(_) => ValueType::Symbol,
            Self::Pair(_) => ValueType::Pair,
            Self::Vector(_) => ValueType::Vector,
            Self::ByteVector(_) => ValueType::ByteVector,
            Self::Syntax(syn) if matches!(syn.as_ref(), Syntax::Null { .. }) => ValueType::Null,
            Self::Syntax(_) => ValueType::Syntax,
            Self::Procedure(_) => ValueType::Procedure,
            Self::Record(_) => ValueType::Record,
            Self::RecordTypeDescriptor(_) => ValueType::RecordTypeDescriptor,
            Self::Port(_) => ValueType::Port,
            Self::HashTable(_) => ValueType::HashTable,
            Self::Cell(cell) => cell.0.read().type_of(),
        }
    }
}

/// Determine if two objects are equal in an extremely granular sense.
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
        (ValueType::Vector, ValueType::Vector) => vector_eq(ht, obj1, obj2, k),
        (ValueType::ByteVector, ValueType::ByteVector) => bytevector_eq(obj1, obj2, k),
        (ValueType::String, ValueType::String) => string_eq(obj1, obj2, k),
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
        (ValueType::Vector, ValueType::Vector) => {
            if union_find(ht, obj1, obj2) {
                return Some(0);
            }
            vector_eq(ht, obj1, obj2, k)
        }
        (ValueType::ByteVector, ValueType::ByteVector) => bytevector_eq(obj1, obj2, k),
        (ValueType::String, ValueType::String) => string_eq(obj1, obj2, k),
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

fn vector_eq(ht: &mut HashMap<Value, Value>, obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    let vobj1: Vector = obj1.clone().try_into().unwrap();
    let vobj2: Vector = obj2.clone().try_into().unwrap();
    let vobj1 = vobj1.0.vec.read();
    let vobj2 = vobj2.0.vec.read();
    if vobj1.len() != vobj2.len() {
        return None;
    }
    let mut k = k - 1;
    for (x, y) in vobj1.iter().zip(vobj2.iter()) {
        k = e(ht, x, y, k)?;
    }
    Some(k)
}

fn bytevector_eq(obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    let obj1: ByteVector = obj1.clone().try_into().unwrap();
    let obj2: ByteVector = obj2.clone().try_into().unwrap();
    (*obj1.0.vec.read() == *obj2.0.vec.read()).then_some(k)
}

fn string_eq(obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    let obj1: WideString = obj1.clone().try_into().unwrap();
    let obj2: WideString = obj2.clone().try_into().unwrap();
    (obj1 == obj2).then_some(k)
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
                set_box(&rx, nx.checked_add(&ny).unwrap());
            } else {
                set_box(&rx, ry.clone());
                set_box(&ry, nx.checked_add(&ny).unwrap());
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
    Value::from(Pair::new(v.clone(), Value::null(), true))
}

fn unbox(v: &Value) -> Value {
    let pair: Pair = v.clone().try_into().unwrap();
    pair.car()
}

fn unbox_to_num(v: &Value) -> Number {
    let pair: Pair = v.clone().try_into().unwrap();
    let num: Arc<Number> = pair.car().try_into().unwrap();
    num.as_ref().clone()
}

fn is_box(v: &Value) -> bool {
    v.type_of() == ValueType::Pair
}

fn set_box(b: &Value, val: impl Into<Value>) {
    let pair: Pair = b.clone().try_into().unwrap();
    pair.set_car(val.into()).unwrap();
}

impl From<ast::Literal> for UnpackedValue {
    fn from(lit: ast::Literal) -> Self {
        match lit {
            ast::Literal::Number(n) => Self::Number(Arc::new(n.clone())),
            ast::Literal::Boolean(b) => Self::Boolean(b),
            ast::Literal::String(s) => Self::String(WideString::from(s.clone())),
            ast::Literal::Character(c) => Self::Character(c),
        }
    }
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

        impl TryFrom<UnpackedValue> for $ty {
            type Error = Condition;

            fn try_from(v: UnpackedValue) -> Result<Self, Self::Error> {
                match v {
                    UnpackedValue::$variant(v) => Ok(v),
                    UnpackedValue::Cell(cell) => cell.0.read().clone().try_into(),
                    e => Err(Condition::type_error($type_name, e.type_name())),
                }
            }
        }

        impl TryFrom<Value> for $ty {
            type Error = Condition;

            fn try_from(v: Value) -> Result<Self, Self::Error> {
                v.unpack().try_into()
            }
        }
    };
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
    type Error = Condition;

    fn try_from(v: UnpackedValue) -> Result<Self, Self::Error> {
        match v {
            UnpackedValue::Cell(cell) => Ok(cell.clone()),
            e => Err(Condition::type_error("cell", e.type_name())),
        }
    }
}

impl TryFrom<Value> for Cell {
    type Error = Condition;

    fn try_from(v: Value) -> Result<Self, Self::Error> {
        v.unpack().try_into()
    }
}

impl_try_from_value_for!(bool, Boolean, "bool");
impl_try_from_value_for!(char, Character, "char");
impl_try_from_value_for!(Arc<Number>, Number, "number");
impl_try_from_value_for!(WideString, String, "string");
impl_try_from_value_for!(Symbol, Symbol, "symbol");
impl_try_from_value_for!(Vector, Vector, "vector");
impl_try_from_value_for!(ByteVector, ByteVector, "byte-vector");
impl_try_from_value_for!(Gc<Syntax>, Syntax, "syntax");
impl_try_from_value_for!(Procedure, Procedure, "procedure");
impl_try_from_value_for!(Pair, Pair, "pair");
impl_try_from_value_for!(Record, Record, "record");
impl_try_from_value_for!(Port, Port, "port");
impl_try_from_value_for!(HashTable, HashTable, "hashtable");
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

impl_from_wrapped_for!(Number, Number, Arc::new);
impl_from_wrapped_for!(String, String, WideString::new);
impl_from_wrapped_for!(Vec<Value>, Vector, Vector::new);
impl_from_wrapped_for!(Vec<u8>, ByteVector, ByteVector::new);
impl_from_wrapped_for!(Syntax, Syntax, Gc::new);
impl_from_wrapped_for!((Value, Value), Pair, |(car, cdr)| Pair::new(
    car, cdr, false
));

impl TryFrom<UnpackedValue> for (Value, Value) {
    type Error = Condition;

    fn try_from(val: UnpackedValue) -> Result<Self, Self::Error> {
        match val {
            UnpackedValue::Pair(pair) => Ok(pair.into()),
            e => Err(Condition::type_error("pair", e.type_name())),
        }
    }
}

macro_rules! impl_num_conversion {
    ($ty:ty) => {
        impl TryInto<$ty> for &Value {
            type Error = Condition;

            fn try_into(self) -> Result<$ty, Self::Error> {
                match &*self.unpacked_ref() {
                    UnpackedValue::Number(num) => num.as_ref().try_into(),
                    e => Err(Condition::type_error("number", e.type_name())),
                }
            }
        }

        impl TryInto<$ty> for Value {
            type Error = Condition;

            fn try_into(self) -> Result<$ty, Self::Error> {
                (&self).try_into()
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

impl TryFrom<Value> for (Value, Value) {
    type Error = Condition;

    fn try_from(val: Value) -> Result<Self, Self::Error> {
        Self::try_from(val.unpack())
    }
}

impl TryFrom<Value> for String {
    type Error = Condition;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let string: WideString = value.try_into()?;
        Ok(string.into())
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
        UnpackedValue::Vector(vec) => {
            let vec_read = vec.0.vec.read();
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

fn display_value(
    val: &Value,
    circular_values: &mut IndexMap<Value, bool>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    match val.clone().unpack() {
        UnpackedValue::Undefined => write!(f, "<undefined>"),
        UnpackedValue::Null => write!(f, "()"),
        UnpackedValue::Boolean(true) => write!(f, "#t"),
        UnpackedValue::Boolean(false) => write!(f, "#f"),
        UnpackedValue::Number(number) => write!(f, "{number}"),
        UnpackedValue::Character(c) => write!(f, "#\\{c}"),
        UnpackedValue::String(string) => write!(f, "{string}"),
        UnpackedValue::Symbol(symbol) => write!(f, "{symbol}"),
        UnpackedValue::Pair(pair) => {
            let (car, cdr) = pair.into();
            lists::write_list(&car, &cdr, display_value, circular_values, f)
        }
        UnpackedValue::Vector(v) => vectors::write_vec(&v, display_value, circular_values, f),
        UnpackedValue::ByteVector(v) => vectors::write_bytevec(&v, f),
        UnpackedValue::Procedure(_) => write!(f, "<procedure>"),
        UnpackedValue::Record(record) => write!(f, "{record:?}"),
        UnpackedValue::Syntax(syntax) => write!(f, "{syntax:#?}"),
        UnpackedValue::RecordTypeDescriptor(rtd) => write!(f, "{rtd:?}"),
        UnpackedValue::Port(_) => write!(f, "<port>"),
        UnpackedValue::HashTable(hashtable) => write!(f, "{hashtable:?}"),
        UnpackedValue::Cell(cell) => display_value(&cell.0.read(), circular_values, f),
    }
}

fn debug_value(
    val: &Value,
    circular_values: &mut IndexMap<Value, bool>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    match val.clone().unpack() {
        UnpackedValue::Undefined => write!(f, "<undefined>"),
        UnpackedValue::Null => write!(f, "()"),
        UnpackedValue::Boolean(true) => write!(f, "#t"),
        UnpackedValue::Boolean(false) => write!(f, "#f"),
        UnpackedValue::Number(number) => write!(f, "{number:?}"),
        UnpackedValue::Character(c) => write!(f, "#\\{c}"),
        UnpackedValue::String(string) => write!(f, "{string:?}"),
        UnpackedValue::Symbol(symbol) => write!(f, "{symbol}"),
        UnpackedValue::Pair(pair) => {
            let (car, cdr) = pair.into();
            lists::write_list(&car, &cdr, debug_value, circular_values, f)
        }
        UnpackedValue::Vector(v) => vectors::write_vec(&v, debug_value, circular_values, f),
        UnpackedValue::ByteVector(v) => vectors::write_bytevec(&v, f),
        UnpackedValue::Syntax(syntax) => write!(f, "{syntax:#?}"),
        UnpackedValue::Procedure(proc) => write!(f, "#<procedure {proc:?}>"),
        UnpackedValue::Record(record) => write!(f, "{record:#?}"),
        UnpackedValue::RecordTypeDescriptor(rtd) => write!(f, "{rtd:?}"),
        UnpackedValue::Port(_) => write!(f, "<port>"),
        UnpackedValue::HashTable(hashtable) => write!(f, "{hashtable:?}"),
        UnpackedValue::Cell(cell) => debug_value(&cell.0.read(), circular_values, f),
    }
}
#[bridge(name = "not", lib = "(rnrs base builtins (6))")]
pub fn not(a: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a.0 as usize == Tag::Boolean as usize)])
}

#[bridge(name = "eqv?", lib = "(rnrs base builtins (6))")]
pub fn eqv(a: &Value, b: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a.eqv(b))])
}

#[bridge(name = "eq?", lib = "(rnrs base builtins (6))")]
pub fn eq(a: &Value, b: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a.eqv(b))])
}

#[bridge(name = "equal?", lib = "(rnrs base builtins (6))")]
pub fn equal_pred(a: &Value, b: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a.equal(b))])
}

#[bridge(name = "boolean?", lib = "(rnrs base builtins (6))")]
pub fn boolean_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Boolean)])
}

#[bridge(name = "boolean=?", lib = "(rnrs base builtins (6))")]
pub fn boolean_eq_pred(a: &Value, args: &[Value]) -> Result<Vec<Value>, Condition> {
    let res = if a.type_of() == ValueType::Boolean {
        args.iter().all(|arg| arg == a)
    } else {
        false
    };
    Ok(vec![Value::from(res)])
}

#[bridge(name = "symbol?", lib = "(rnrs base builtins (6))")]
pub fn symbol_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Symbol)])
}

#[bridge(name = "char?", lib = "(rnrs base builtins (6))")]
pub fn char_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Character)])
}

#[bridge(name = "vector?", lib = "(rnrs base builtins (6))")]
pub fn vector_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Vector)])
}

#[bridge(name = "null?", lib = "(rnrs base builtins (6))")]
pub fn null_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Null)])
}

#[bridge(name = "pair?", lib = "(rnrs base builtins (6))")]
pub fn pair_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(matches!(
        *arg.unpacked_ref(),
        UnpackedValue::Pair(_)
    ))])
}

#[bridge(name = "procedure?", lib = "(rnrs base builtins (6))")]
pub fn procedure_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Procedure)])
}

#[bridge(name = "display", lib = "(rnrs base builtins (6))")]
pub fn display(arg: &Value) -> Result<Vec<Value>, Condition> {
    print!("{arg}");
    let _ = std::io::stdout().flush();
    Ok(Vec::new())
}
