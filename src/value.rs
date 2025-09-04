use indexmap::{IndexMap, IndexSet};

use crate::{
    ast,
    exception::{Condition, Exception},
    gc::{Gc, GcInner, Trace},
    lists,
    num::{Number, ReflexiveNumber},
    proc::{Closure, ClosureInner},
    records::{Record, RecordTypeDescriptor},
    registry::bridge,
    strings, symbols,
    syntax::Syntax,
    vectors,
};
use std::{
    any::Any, collections::HashMap, fmt, hash::Hash, io::Write, marker::PhantomData,
    mem::ManuallyDrop, ops::Deref, sync::Arc,
};

const ALIGNMENT: u64 = 16;
const TAG_BITS: u64 = ALIGNMENT.ilog2() as u64;
const TAG: u64 = 0b1111;
const FALSE_VALUE: u64 = ValueType::Boolean as u64;

/// A Scheme value. Represented as a tagged pointer.
#[repr(transparent)]
pub struct Value(u64);

impl Value {
    pub fn new(v: UnpackedValue) -> Self {
        v.into_value()
    }

    /// #f is false, everything else is true
    pub fn is_true(&self) -> bool {
        self.0 != FALSE_VALUE
    }

    /// Creates a new Value from a raw u64.
    ///
    /// # Safety
    /// Calling this function is undefined behavior if the raw u64 was not obtained
    /// via [into_raw]
    pub unsafe fn from_raw(raw: u64) -> Self {
        Self(raw)
    }

    /// Creates a new Value from a raw u64, incrementing the reference count.
    ///
    /// # Safety
    /// Calling this function is undefined behavior if the raw u64 was not obtained
    /// via [into_raw]
    pub unsafe fn from_raw_inc_rc(raw: u64) -> Self {
        let tag = ValueType::from(raw & TAG);
        let untagged = raw & !TAG;
        unsafe {
            match tag {
                ValueType::Number => Arc::increment_strong_count(untagged as *const Number),
                ValueType::String => {
                    Arc::increment_strong_count(untagged as *const strings::AlignedString)
                }
                ValueType::Vector => Gc::increment_reference_count(
                    untagged as *mut GcInner<vectors::AlignedVector<Self>>,
                ),
                ValueType::ByteVector => {
                    Arc::increment_strong_count(untagged as *const vectors::AlignedVector<u8>)
                }
                ValueType::Syntax => Arc::increment_strong_count(untagged as *const Syntax),
                ValueType::Closure => {
                    Gc::increment_reference_count(untagged as *mut GcInner<ClosureInner>)
                }
                ValueType::Record => {
                    Gc::increment_reference_count(untagged as *mut GcInner<Record>)
                }
                ValueType::RecordTypeDescriptor => {
                    Arc::increment_strong_count(untagged as *const RecordTypeDescriptor)
                }
                ValueType::Pair => {
                    Gc::increment_reference_count(untagged as *mut GcInner<lists::Pair>)
                }
                ValueType::Any => {
                    Gc::increment_reference_count(untagged as *mut GcInner<Gc<dyn Any>>)
                }
                ValueType::Reserved => todo!(),
                ValueType::Undefined
                | ValueType::Symbol
                | ValueType::Null
                | ValueType::Boolean
                | ValueType::Character => (),
            }
        }
        Self(raw)
    }

    /// Creates a raw u64 from a Value. Does not decrement the reference count.
    /// Calling this function without turning the raw value into a Value via
    /// [from_raw] is equivalent to calling mem::forget on the value.
    pub fn into_raw(val: Self) -> u64 {
        ManuallyDrop::new(val).0
    }

    /// Creates a raw u64 from the Value. Does not decrement the reference count.
    pub fn as_raw(this: &Self) -> u64 {
        this.0
    }

    fn from_ptr_and_tag<T>(ptr: *const T, tag: ValueType) -> Self {
        Self(ptr as u64 | tag as u64)
    }

    fn from_mut_ptr_and_tag<T>(ptr: *mut T, tag: ValueType) -> Self {
        Self(ptr as u64 | tag as u64)
    }

    pub const fn undefined() -> Self {
        Self(ValueType::Undefined as u64)
    }

    pub const fn null() -> Self {
        Self(ValueType::Null as u64)
    }

    pub fn datum_from_syntax(syntax: &Syntax) -> Self {
        match syntax {
            Syntax::Null { .. } => Self::null(),
            Syntax::List { list, .. } => {
                let mut curr = Self::datum_from_syntax(list.last().unwrap());
                for item in list[..list.len() - 1].iter().rev() {
                    curr = Self::from(Gc::new(lists::Pair(Self::datum_from_syntax(item), curr)));
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
        ValueType::from(self.0 & TAG)
    }

    pub fn type_name(&self) -> &'static str {
        match self.type_of() {
            ValueType::Undefined => "undefined",
            ValueType::Null => "null",
            ValueType::Boolean => "bool",
            ValueType::Number => "number",
            ValueType::Character => "character",
            ValueType::String => "string",
            ValueType::Symbol => "symbol",
            ValueType::Pair => "pair",
            ValueType::Vector => "vector",
            ValueType::ByteVector => "byte vector",
            ValueType::Syntax => "syntax",
            ValueType::Closure => "procedure",
            _ => "record",
        }
    }

    pub fn unpack(self) -> UnpackedValue {
        let raw = ManuallyDrop::new(self).0;
        let tag = ValueType::from(raw & TAG);
        let untagged = raw & !TAG;
        match tag {
            ValueType::Undefined => UnpackedValue::Undefined,
            ValueType::Null => UnpackedValue::Null,
            ValueType::Boolean => {
                let untagged = untagged >> TAG_BITS;
                UnpackedValue::Boolean(untagged != 0)
            }
            ValueType::Character => {
                let untagged = (untagged >> TAG_BITS) as u32;
                UnpackedValue::Character(char::from_u32(untagged).unwrap())
            }
            ValueType::Number => {
                let number = unsafe { Arc::from_raw(untagged as *const Number) };
                UnpackedValue::Number(number)
            }
            ValueType::String => {
                let str = unsafe { Arc::from_raw(untagged as *const strings::AlignedString) };
                UnpackedValue::String(str)
            }
            ValueType::Symbol => {
                let untagged = (untagged >> TAG_BITS) as u32;
                UnpackedValue::Symbol(symbols::Symbol(untagged))
            }
            ValueType::Vector => {
                let vec =
                    unsafe { Gc::from_raw(untagged as *mut GcInner<vectors::AlignedVector<Self>>) };
                UnpackedValue::Vector(vec)
            }
            ValueType::ByteVector => {
                let bvec = unsafe { Arc::from_raw(untagged as *const vectors::AlignedVector<u8>) };
                UnpackedValue::ByteVector(bvec)
            }
            ValueType::Syntax => {
                let syn = unsafe { Arc::from_raw(untagged as *const Syntax) };
                UnpackedValue::Syntax(syn)
            }
            ValueType::Closure => {
                let clos = unsafe { Gc::from_raw(untagged as *mut GcInner<ClosureInner>) };
                UnpackedValue::Closure(Closure(clos))
            }
            ValueType::Record => {
                let rec = unsafe { Gc::from_raw(untagged as *mut GcInner<Record>) };
                UnpackedValue::Record(rec)
            }
            ValueType::RecordTypeDescriptor => {
                let rt = unsafe { Arc::from_raw(untagged as *const RecordTypeDescriptor) };
                UnpackedValue::RecordTypeDescriptor(rt)
            }
            ValueType::Pair => {
                let pair = unsafe { Gc::from_raw(untagged as *mut GcInner<lists::Pair>) };
                UnpackedValue::Pair(pair)
            }
            ValueType::Any => {
                let any = unsafe { Gc::from_raw(untagged as *mut GcInner<Gc<dyn Any>>) };
                UnpackedValue::Any(any)
            }
            ValueType::Reserved => todo!(),
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
    unsafe fn visit_children(&self, visitor: unsafe fn(crate::gc::OpaqueGcPtr)) {
        unsafe {
            self.unpacked_ref().visit_children(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe { ManuallyDrop::new(Self(self.0).unpack()).finalize() }
    }
}

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

#[repr(u64)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueType {
    Undefined = 0,
    Null = 1,
    Boolean = 2,
    Character = 3,
    Number = 4,
    String = 5,
    Symbol = 6,
    Vector = 7,
    ByteVector = 8,
    Syntax = 9,
    Closure = 10,
    Record = 11,
    RecordTypeDescriptor = 12,
    Pair = 13,
    Any = 14,
    Reserved = 15,
}

// TODO: Make TryFrom with error
impl From<u64> for ValueType {
    fn from(tag: u64) -> Self {
        match tag {
            0 => Self::Undefined,
            1 => Self::Null,
            2 => Self::Boolean,
            3 => Self::Character,
            4 => Self::Number,
            5 => Self::String,
            6 => Self::Symbol,
            7 => Self::Vector,
            8 => Self::ByteVector,
            9 => Self::Syntax,
            10 => Self::Closure,
            11 => Self::Record,
            12 => Self::RecordTypeDescriptor,
            13 => Self::Pair,
            14 => Self::Any,
            tag => panic!("Invalid tag: {tag}"),
        }
    }
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
    String(Arc<strings::AlignedString>),
    Symbol(symbols::Symbol),
    Vector(Gc<vectors::AlignedVector<Value>>),
    ByteVector(Arc<vectors::AlignedVector<u8>>),
    Syntax(Arc<Syntax>),
    Closure(Closure),
    Record(Gc<Record>),
    RecordTypeDescriptor(Arc<RecordTypeDescriptor>),
    Pair(Gc<lists::Pair>),
    Any(Gc<Gc<dyn Any>>),
}

impl UnpackedValue {
    pub fn into_value(self) -> Value {
        match self {
            Self::Undefined => Value::undefined(),
            Self::Null => Value::null(),
            Self::Boolean(b) => Value((b as u64) << TAG_BITS | ValueType::Boolean as u64),
            Self::Character(c) => Value((c as u64) << TAG_BITS | ValueType::Character as u64),
            Self::Number(num) => {
                let untagged = Arc::into_raw(num);
                Value::from_ptr_and_tag(untagged, ValueType::Number)
            }
            Self::String(str) => {
                let untagged = Arc::into_raw(str);
                Value::from_ptr_and_tag(untagged, ValueType::String)
            }
            Self::Symbol(sym) => Value((sym.0 as u64) << TAG_BITS | ValueType::Symbol as u64),
            Self::Vector(vec) => {
                let untagged = Gc::into_raw(vec);
                Value::from_mut_ptr_and_tag(untagged, ValueType::Vector)
            }
            Self::ByteVector(b_vec) => {
                let untagged = Arc::into_raw(b_vec);
                Value::from_ptr_and_tag(untagged, ValueType::ByteVector)
            }
            Self::Syntax(syn) => {
                let untagged = Arc::into_raw(syn);
                Value::from_ptr_and_tag(untagged, ValueType::Syntax)
            }
            Self::Closure(clos) => {
                let untagged = Gc::into_raw(clos.0);
                Value::from_mut_ptr_and_tag(untagged, ValueType::Closure)
            }
            Self::Record(rec) => {
                let untagged = Gc::into_raw(rec);
                Value::from_mut_ptr_and_tag(untagged, ValueType::Record)
            }
            Self::RecordTypeDescriptor(rt) => {
                let untagged = Arc::into_raw(rt);
                Value::from_ptr_and_tag(untagged, ValueType::RecordTypeDescriptor)
            }
            Self::Pair(pair) => {
                let untagged = Gc::into_raw(pair);
                Value::from_mut_ptr_and_tag(untagged, ValueType::Pair)
            }
            Self::Any(any) => {
                let untagged = Gc::into_raw(any);
                Value::from_mut_ptr_and_tag(untagged, ValueType::Any)
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
            (Self::String(a), Self::String(b)) => Arc::ptr_eq(a, b),
            (Self::Pair(a), Self::Pair(b)) => Gc::ptr_eq(a, b),
            (Self::Vector(a), Self::Vector(b)) => Gc::ptr_eq(a, b),
            (Self::ByteVector(a), Self::ByteVector(b)) => Arc::ptr_eq(a, b),
            (Self::Closure(a), Self::Closure(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Syntax(a), Self::Syntax(b)) => Arc::ptr_eq(a, b),
            (Self::Record(a), Self::Record(b)) => Gc::ptr_eq(a, b),
            (Self::RecordTypeDescriptor(a), Self::RecordTypeDescriptor(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }

    pub fn eqv(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            // boolean=?
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            // symbol=?
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            // Numbers are only equivalent if they're the same exactness
            (Self::Number(a), Self::Number(b)) => a.is_exact() == b.is_exact() && a == b,
            // char=?
            (Self::Character(a), Self::Character(b)) => a == b,
            // Both obj1 and obj2 are the empty list
            (Self::Null, Self::Null) => true,
            // Everything else is pointer equivalence
            (Self::String(a), Self::String(b)) => Arc::ptr_eq(a, b),
            (Self::Pair(a), Self::Pair(b)) => Gc::ptr_eq(a, b),
            (Self::Vector(a), Self::Vector(b)) => Gc::ptr_eq(a, b),
            (Self::ByteVector(a), Self::ByteVector(b)) => Arc::ptr_eq(a, b),
            (Self::Closure(a), Self::Closure(b)) => Gc::ptr_eq(&a.0, &b.0),
            (Self::Syntax(a), Self::Syntax(b)) => Arc::ptr_eq(a, b),
            (Self::Record(a), Self::Record(b)) => Gc::ptr_eq(a, b),
            (Self::RecordTypeDescriptor(a), Self::RecordTypeDescriptor(b)) => Arc::ptr_eq(a, b),
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
            Self::Closure(_) => "procedure",
            Self::Record(_) | Self::RecordTypeDescriptor(_) | Self::Any(_) => "record",
        }
    }
}

/// The PartialEq implementation for a Value is equivalent to the equal? Scheme
/// predicate, and is therefore implemented to the standard of the R6RS
/// specification.
impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        equal(self, rhs)
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

fn interleave(ht: &mut HashMap<EqvValue, Value>, obj1: &Value, obj2: &Value, k: i64) -> bool {
    e(ht, obj1, obj2, k).is_some()
}

fn e(ht: &mut HashMap<EqvValue, Value>, obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    match k {
        KB => fast(ht, obj1, obj2, rand::random_range(0..(K0 * 2))),
        k if k <= 0 => slow(ht, obj1, obj2, k),
        k => fast(ht, obj1, obj2, k),
    }
}

fn fast(ht: &mut HashMap<EqvValue, Value>, obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
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

fn slow(ht: &mut HashMap<EqvValue, Value>, obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
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

fn pair_eq(ht: &mut HashMap<EqvValue, Value>, obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    let obj1: Gc<lists::Pair> = obj1.clone().try_into().unwrap();
    let obj2: Gc<lists::Pair> = obj2.clone().try_into().unwrap();
    let obj1 = obj1.read();
    let obj2 = obj2.read();
    let lists::Pair(car_x, cdr_x) = obj1.as_ref();
    let lists::Pair(car_y, cdr_y) = obj2.as_ref();
    e(ht, car_x, car_y, k - 1).and_then(|k| e(ht, cdr_x, cdr_y, k))
}

fn vector_eq(ht: &mut HashMap<EqvValue, Value>, obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    let vobj1: Gc<vectors::AlignedVector<Value>> = obj1.clone().try_into().unwrap();
    let vobj2: Gc<vectors::AlignedVector<Value>> = obj2.clone().try_into().unwrap();
    let vobj1 = vobj1.read();
    let vobj2 = vobj2.read();
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
    let obj1: Arc<vectors::AlignedVector<u8>> = obj1.clone().try_into().unwrap();
    let obj2: Arc<vectors::AlignedVector<u8>> = obj2.clone().try_into().unwrap();
    (obj1 == obj2).then_some(k)
}

fn string_eq(obj1: &Value, obj2: &Value, k: i64) -> Option<i64> {
    let obj1: Arc<strings::AlignedString> = obj1.clone().try_into().unwrap();
    let obj2: Arc<strings::AlignedString> = obj2.clone().try_into().unwrap();
    (obj1 == obj2).then_some(k)
}

fn union_find(ht: &mut HashMap<EqvValue, Value>, x: &Value, y: &Value) -> bool {
    let eqv_x = EqvValue(x.clone());
    let eqv_y = EqvValue(y.clone());
    let bx = ht.get(&eqv_x).cloned();
    let by = ht.get(&eqv_y).cloned();
    match (bx, by) {
        (None, None) => {
            let b = boxv(Value::from(Number::from(1)));
            ht.insert(eqv_x, b.clone());
            ht.insert(eqv_y, b);
        }
        (None, Some(by)) => {
            let ry = find(by);
            ht.insert(eqv_x, ry);
        }
        (Some(bx), None) => {
            let rx = find(bx);
            ht.insert(eqv_y, rx);
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
    Value::from((v.clone(), Value::null()))
}

fn unbox(v: &Value) -> Value {
    let pair: Gc<lists::Pair> = v.clone().try_into().unwrap();
    pair.read().0.clone()
}

fn unbox_to_num(v: &Value) -> Number {
    let pair: Gc<lists::Pair> = v.clone().try_into().unwrap();
    let num: Arc<Number> = pair.read().0.clone().try_into().unwrap();
    num.as_ref().clone()
}

fn is_box(v: &Value) -> bool {
    v.type_of() == ValueType::Pair
}

fn set_box(b: &Value, val: impl Into<Value>) {
    let pair: Gc<lists::Pair> = b.clone().try_into().unwrap();
    pair.write().0 = val.into();
}

impl From<ast::Literal> for UnpackedValue {
    fn from(lit: ast::Literal) -> Self {
        match lit {
            ast::Literal::Number(n) => Self::Number(Arc::new(n.clone())),
            ast::Literal::Boolean(b) => Self::Boolean(b),
            ast::Literal::String(s) => Self::String(Arc::new(strings::AlignedString(s.clone()))),
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
                    e => Err(Condition::invalid_type($type_name, e.type_name())),
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

impl_try_from_value_for!(bool, Boolean, "bool");
impl_try_from_value_for!(char, Character, "char");
impl_try_from_value_for!(Arc<Number>, Number, "number");
impl_try_from_value_for!(Arc<strings::AlignedString>, String, "string");
impl_try_from_value_for!(symbols::Symbol, Symbol, "symbol");
impl_try_from_value_for!(Gc<vectors::AlignedVector<Value>>, Vector, "vector");
impl_try_from_value_for!(Arc<vectors::AlignedVector<u8>>, ByteVector, "byte-vector");
impl_try_from_value_for!(Arc<Syntax>, Syntax, "syntax");
impl_try_from_value_for!(Closure, Closure, "procedure");
impl_try_from_value_for!(Gc<lists::Pair>, Pair, "pair");
impl_try_from_value_for!(Gc<Record>, Record, "record");
impl_try_from_value_for!(Arc<RecordTypeDescriptor>, RecordTypeDescriptor, "rt");
impl_try_from_value_for!(Gc<Gc<dyn Any>>, Any, "record");

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
impl_from_wrapped_for!(String, String, |str| Arc::new(strings::AlignedString::new(
    str
)));
impl_from_wrapped_for!(Vec<Value>, Vector, |vec| Gc::new(
    vectors::AlignedVector::new(vec)
));
impl_from_wrapped_for!(Vec<u8>, ByteVector, |vec| Arc::new(
    vectors::AlignedVector::new(vec)
));
impl_from_wrapped_for!(Syntax, Syntax, Arc::new);
// impl_from_wrapped_for!(ClosureInner, Closure, Gc::from);
impl_from_wrapped_for!((Value, Value), Pair, |(car, cdr)| Gc::new(
    lists::Pair::new(car, cdr)
));

macro_rules! impl_try_from_for_any {
    ($ty:ty, $name:literal) => {
        impl From<$ty> for UnpackedValue {
            fn from(val: $ty) -> Self {
                Self::Any(Gc::new(Gc::into_any(Gc::new(val))))
            }
        }

        impl From<$ty> for Value {
            fn from(val: $ty) -> Self {
                UnpackedValue::from(val).into_value()
            }
        }

        impl TryFrom<UnpackedValue> for Gc<$ty> {
            type Error = Condition;

            fn try_from(val: UnpackedValue) -> Result<Self, Self::Error> {
                let any = match val {
                    UnpackedValue::Any(v) => v,
                    e => return Err(Condition::invalid_type($name, e.type_name())),
                };
                let any = any.read().clone();
                match any.downcast::<$ty>() {
                    Ok(ok) => Ok(ok),
                    Err(_) => Err(Condition::invalid_type($name, "record")),
                }
            }
        }

        impl TryFrom<Value> for Gc<$ty> {
            type Error = Condition;

            fn try_from(val: Value) -> Result<Self, Self::Error> {
                Self::try_from(val.unpack())
            }
        }
    };
}

impl_try_from_for_any!(Condition, "condition");

impl TryFrom<UnpackedValue> for (Value, Value) {
    type Error = Condition;

    fn try_from(val: UnpackedValue) -> Result<Self, Self::Error> {
        match val {
            UnpackedValue::Pair(pair) => {
                let pair = pair.read();
                Ok((pair.0.clone(), pair.1.clone()))
            }
            e => Err(Condition::invalid_type("pair", e.type_name())),
        }
    }
}

impl TryFrom<Value> for (Value, Value) {
    type Error = Condition;

    fn try_from(val: Value) -> Result<Self, Self::Error> {
        Self::try_from(val.unpack())
    }
}

/// A Value for which the implementation of PartialEq uses eqv rather than equal
#[derive(Clone)]
pub(crate) struct EqvValue(pub(crate) Value);

impl Hash for EqvValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let unpacked = self.0.unpacked_ref();
        std::mem::discriminant(&*unpacked).hash(state);
        match &*unpacked {
            UnpackedValue::Undefined => (),
            UnpackedValue::Null => (),
            UnpackedValue::Boolean(b) => b.hash(state),
            UnpackedValue::Character(c) => c.hash(state),
            UnpackedValue::Number(n) => ReflexiveNumber(n.clone()).hash(state),
            UnpackedValue::String(s) => s.hash(state),
            UnpackedValue::Symbol(s) => s.hash(state),
            UnpackedValue::ByteVector(v) => v.hash(state),
            UnpackedValue::Syntax(s) => Arc::as_ptr(s).hash(state),
            UnpackedValue::Closure(c) => Gc::as_ptr(&c.0).hash(state),
            UnpackedValue::Record(r) => Gc::as_ptr(r).hash(state),
            UnpackedValue::RecordTypeDescriptor(rt) => Arc::as_ptr(rt).hash(state),
            UnpackedValue::Any(a) => Gc::as_ptr(a).hash(state),
            UnpackedValue::Pair(p) => Gc::as_ptr(p).hash(state),
            UnpackedValue::Vector(v) => Gc::as_ptr(v).hash(state),
        }
    }
}

impl PartialEq for EqvValue {
    fn eq(&self, rhs: &Self) -> bool {
        self.0.eqv(&rhs.0)
    }
}

impl Eq for EqvValue {}

#[derive(Clone, Debug, Trace)]
pub(crate) struct ReflexiveValue(pub(crate) Value);

impl AsRef<Value> for ReflexiveValue {
    fn as_ref(&self) -> &Value {
        &self.0
    }
}

impl Hash for ReflexiveValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let unpacked = self.0.unpacked_ref();
        std::mem::discriminant(&*unpacked).hash(state);
        match &*unpacked {
            UnpackedValue::Undefined => (),
            UnpackedValue::Null => (),
            UnpackedValue::Boolean(b) => b.hash(state),
            UnpackedValue::Character(c) => c.hash(state),
            UnpackedValue::Number(n) => ReflexiveNumber(n.clone()).hash(state),
            UnpackedValue::String(s) => s.hash(state),
            UnpackedValue::Symbol(s) => s.hash(state),
            UnpackedValue::ByteVector(v) => v.hash(state),
            UnpackedValue::Syntax(s) => Arc::as_ptr(s).hash(state),
            UnpackedValue::Closure(c) => Gc::as_ptr(&c.0).hash(state),
            UnpackedValue::Record(r) => Gc::as_ptr(r).hash(state),
            UnpackedValue::RecordTypeDescriptor(rt) => Arc::as_ptr(rt).hash(state),
            UnpackedValue::Any(a) => Gc::as_ptr(a).hash(state),
            // UnpackedValue::Condition(c) => Gc::as_ptr(c).hash(state),
            // UnpackedValue::OtherData(o) => Gc::as_ptr(o).hash(state),
            // TODO: We can make this better by checking the vectors and list
            // for equivalence reflexively. But if we do that, we need to make
            // sure that constants cannot be set.
            UnpackedValue::Pair(p) => Gc::as_ptr(p).hash(state),
            UnpackedValue::Vector(v) => Gc::as_ptr(v).hash(state),
        }
    }
}

impl PartialEq for ReflexiveValue {
    fn eq(&self, rhs: &Self) -> bool {
        let unpacked_lhs = self.0.unpacked_ref();
        let unpacked_rhs = rhs.0.unpacked_ref();
        match (&*unpacked_lhs, &*unpacked_rhs) {
            (UnpackedValue::Undefined, UnpackedValue::Undefined) => true,
            (UnpackedValue::Null, UnpackedValue::Null) => true,
            (UnpackedValue::Boolean(a), UnpackedValue::Boolean(b)) => a == b,
            (UnpackedValue::Number(a), UnpackedValue::Number(b)) => {
                ReflexiveNumber(a.clone()) == ReflexiveNumber(b.clone())
            }
            (UnpackedValue::Character(a), UnpackedValue::Character(b)) => a == b,
            (UnpackedValue::String(a), UnpackedValue::String(b)) => a == b,
            (UnpackedValue::Symbol(a), UnpackedValue::Symbol(b)) => a == b,
            // See comment above in hash function
            (UnpackedValue::Pair(a), UnpackedValue::Pair(b)) => Gc::ptr_eq(a, b),
            (UnpackedValue::Vector(a), UnpackedValue::Vector(b)) => Gc::ptr_eq(a, b),
            (UnpackedValue::ByteVector(a), UnpackedValue::ByteVector(b)) => a == b,
            (UnpackedValue::Syntax(a), UnpackedValue::Syntax(b)) => Arc::ptr_eq(a, b),
            (UnpackedValue::Closure(a), UnpackedValue::Closure(b)) => Gc::ptr_eq(&a.0, &b.0),
            (UnpackedValue::Record(a), UnpackedValue::Record(b)) => Gc::ptr_eq(a, b),
            (UnpackedValue::RecordTypeDescriptor(a), UnpackedValue::RecordTypeDescriptor(b)) => {
                Arc::ptr_eq(a, b)
            }
            (UnpackedValue::Any(a), UnpackedValue::Any(b)) => Gc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for ReflexiveValue {}

/// Determines which children of the given list are circular, i.e. have children
/// that refer to back to them. This is just a depth-first search.
fn determine_circularity(
    curr: &Value,
    visited: &mut IndexSet<EqvValue>,
    circular: &mut IndexSet<EqvValue>,
) {
    let eqv_value = EqvValue(curr.clone());
    if visited.contains(&eqv_value) {
        circular.insert(eqv_value);
        return;
    }

    visited.insert(eqv_value.clone());

    match curr.clone().unpack() {
        UnpackedValue::Pair(pair) => {
            let pair_read = pair.read();
            let lists::Pair(car, cdr) = pair_read.as_ref();
            determine_circularity(car, visited, circular);
            determine_circularity(cdr, visited, circular);
        }
        UnpackedValue::Vector(vec) => {
            let vec_read = vec.read();
            for item in &vec_read.0 {
                determine_circularity(item, visited, circular);
            }
        }
        _ => (),
    }

    visited.swap_remove(&eqv_value);
}

pub(crate) fn write_value(
    val: &Value,
    fmt: fn(&Value, &mut IndexMap<EqvValue, bool>, &mut fmt::Formatter<'_>) -> fmt::Result,
    circular_values: &mut IndexMap<EqvValue, bool>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    if let Some((idx, _, seen)) = circular_values.get_full_mut(&EqvValue(val.clone())) {
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
    circular_values: &mut IndexMap<EqvValue, bool>,
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
            let pair_read = pair.read();
            let lists::Pair(car, cdr) = pair_read.as_ref();
            lists::write_list(car, cdr, display_value, circular_values, f)
        }
        UnpackedValue::Vector(v) => vectors::write_vec(&v, display_value, circular_values, f),
        UnpackedValue::ByteVector(v) => vectors::write_bytevec(&v, f),
        UnpackedValue::Closure(_) => write!(f, "<procedure>"),
        UnpackedValue::Record(record) => write!(f, "<{record:?}>"),
        UnpackedValue::Syntax(syntax) => write!(f, "{syntax:#?}"),
        UnpackedValue::RecordTypeDescriptor(rtd) => write!(f, "<{rtd:?}>"),
        UnpackedValue::Any(any) => {
            let any = any.read().clone();
            let Ok(cond) = any.downcast::<Condition>() else {
                return write!(f, "<record>");
            };
            write!(f, "<{cond:?}>")
        }
    }
}

fn debug_value(
    val: &Value,
    circular_values: &mut IndexMap<EqvValue, bool>,
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
            let pair_read = pair.read();
            let lists::Pair(car, cdr) = pair_read.as_ref();
            lists::write_list(car, cdr, debug_value, circular_values, f)
        }
        UnpackedValue::Vector(v) => vectors::write_vec(&v, debug_value, circular_values, f),
        UnpackedValue::ByteVector(v) => vectors::write_bytevec(&v, f),
        UnpackedValue::Syntax(syntax) => write!(f, "{syntax:#?}"),
        UnpackedValue::Closure(proc) => write!(f, "#<procedure {proc:?}>"),
        UnpackedValue::Record(record) => write!(f, "<{record:#?}>"),
        UnpackedValue::RecordTypeDescriptor(rtd) => write!(f, "<{rtd:?}>"),
        UnpackedValue::Any(any) => {
            let any = any.read().clone();
            let Ok(cond) = any.downcast::<Condition>() else {
                return write!(f, "<record>");
            };
            write!(f, "<{cond:?}>")
        }
    }
}
#[bridge(name = "not", lib = "(rnrs base builtins (6))")]
pub async fn not(a: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a.0 == ValueType::Boolean as u64)])
}

#[bridge(name = "eqv?", lib = "(rnrs base builtins (6))")]
pub async fn eqv(a: &Value, b: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a.eqv(b))])
}

#[bridge(name = "eq?", lib = "(rnrs base builtins (6))")]
pub async fn eq(a: &Value, b: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a.eqv(b))])
}

#[bridge(name = "equal?", lib = "(rnrs base builtins (6))")]
pub async fn equal_pred(a: &Value, b: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a == b)])
}

#[bridge(name = "boolean?", lib = "(rnrs base builtins (6))")]
pub async fn boolean_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Boolean)])
}

#[bridge(name = "boolean=?", lib = "(rnrs base builtins (6))")]
pub async fn boolean_eq_pred(a: &Value, args: &[Value]) -> Result<Vec<Value>, Condition> {
    let res = if a.type_of() == ValueType::Boolean {
        args.iter().all(|arg| arg == a)
    } else {
        false
    };
    Ok(vec![Value::from(res)])
}

#[bridge(name = "symbol?", lib = "(rnrs base builtins (6))")]
pub async fn symbol_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Symbol)])
}

#[bridge(name = "char?", lib = "(rnrs base builtins (6))")]
pub async fn char_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Character)])
}

#[bridge(name = "vector?", lib = "(rnrs base builtins (6))")]
pub async fn vector_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Vector)])
}

#[bridge(name = "null?", lib = "(rnrs base builtins (6))")]
pub async fn null_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    let is_null = match arg.clone().unpack() {
        UnpackedValue::Null => true,
        UnpackedValue::Syntax(syn) => matches!(&*syn, Syntax::Null { .. }),
        _ => false,
    };
    Ok(vec![Value::from(is_null)])
}

#[bridge(name = "pair?", lib = "(rnrs base builtins (6))")]
pub async fn pair_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Pair)])
}

#[bridge(name = "string?", lib = "(rnrs base builtins (6))")]
pub async fn string_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::String)])
}

#[bridge(name = "procedure?", lib = "(rnrs base builtins (6))")]
pub async fn procedure_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Closure)])
}

#[bridge(name = "display", lib = "(rnrs base builtins (6))")]
pub async fn display(arg: &Value) -> Result<Vec<Value>, Condition> {
    print!("{arg}");
    let _ = std::io::stdout().flush();
    Ok(Vec::new())
}
