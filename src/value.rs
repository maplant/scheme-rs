use crate::{
    ast,
    env::CapturedEnv,
    exception::{Condition, Exception},
    expand::Transformer,
    gc::{Gc, GcInner, Trace},
    lists,
    num::{Number, ReflexiveNumber},
    proc::Closure,
    records::{Record, RecordType},
    registry::bridge,
    strings::AlignedString,
    syntax::Syntax,
    vectors,
};
// use futures::future::{BoxFuture, Shared};
use std::{
    fmt, hash::Hash, io::Write, marker::PhantomData, mem::ManuallyDrop, ops::Deref, sync::Arc,
};

// type Future = Shared<BoxFuture<'static, Result<Vec<Gc<Value>>, Gc<Value>>>>;

const ALIGNMENT: u64 = 16;
const TAG_BITS: u64 = ALIGNMENT.ilog2() as u64;
const TAG: u64 = 0b1111;

/// A Scheme value. Represented as a tagged pointer.
#[repr(transparent)]
pub struct Value(u64);

impl Value {
    pub fn new(v: UnpackedValue) -> Self {
        v.into_value()
    }

    /// #f is false, everything else is true
    pub fn is_true(&self) -> bool {
        self.0 != ValueType::Boolean as u64
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
                ValueType::String => Arc::increment_strong_count(untagged as *const AlignedString),
                ValueType::Symbol => Arc::increment_strong_count(untagged as *const AlignedString),
                ValueType::Vector => Gc::increment_reference_count(
                    untagged as *mut GcInner<vectors::AlignedVector<Self>>,
                ),
                ValueType::ByteVector => Arc::increment_strong_count(untagged as *const Vec<u8>),
                ValueType::Syntax => Arc::increment_strong_count(untagged as *const Syntax),
                ValueType::Closure => {
                    Gc::increment_reference_count(untagged as *mut GcInner<Closure>)
                }
                ValueType::Record => {
                    Gc::increment_reference_count(untagged as *mut GcInner<Record>)
                }
                ValueType::Condition => {
                    Gc::increment_reference_count(untagged as *mut GcInner<Condition>)
                }
                ValueType::Pair => {
                    Gc::increment_reference_count(untagged as *mut GcInner<lists::Pair>)
                }
                ValueType::Other => {
                    Gc::increment_reference_count(untagged as *mut GcInner<OtherData>)
                }
                ValueType::HashTable => todo!(),
                ValueType::Undefined
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

    pub fn undefined() -> Self {
        Self(ValueType::Undefined as u64)
    }

    pub fn null() -> Self {
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
            Syntax::Identifier { ident, .. } => Self::new(UnpackedValue::Symbol(Arc::new(
                AlignedString(ident.name.clone()),
            ))),
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
                let str = unsafe { Arc::from_raw(untagged as *const AlignedString) };
                UnpackedValue::String(str)
            }
            ValueType::Symbol => {
                let sym = unsafe { Arc::from_raw(untagged as *const AlignedString) };
                UnpackedValue::Symbol(sym)
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
                let clos = unsafe { Gc::from_raw(untagged as *mut GcInner<Closure>) };
                UnpackedValue::Closure(clos)
            }
            ValueType::Record => {
                let rec = unsafe { Gc::from_raw(untagged as *mut GcInner<Record>) };
                UnpackedValue::Record(rec)
            }
            ValueType::Condition => {
                let cond = unsafe { Gc::from_raw(untagged as *mut GcInner<Condition>) };
                UnpackedValue::Condition(cond)
            }
            ValueType::Pair => {
                let pair = unsafe { Gc::from_raw(untagged as *mut GcInner<lists::Pair>) };
                UnpackedValue::Pair(pair)
            }
            ValueType::Other => {
                let other = unsafe { Gc::from_raw(untagged as *mut GcInner<OtherData>) };
                UnpackedValue::OtherData(other)
            }
            ValueType::HashTable => todo!(),
        }
    }

    pub fn unpacked_ref(&self) -> UnpackedValueRef<'_> {
        let unpacked = ManuallyDrop::new(Value(self.0).unpack());
        UnpackedValueRef {
            unpacked,
            marker: PhantomData,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        *self.unpacked_ref() == *rhs.unpacked_ref()
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
        <UnpackedValue as fmt::Display>::fmt(&*self.unpacked_ref(), f)
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <UnpackedValue as fmt::Debug>::fmt(&*self.unpacked_ref(), f)
    }
}

unsafe impl Trace for Value {
    unsafe fn visit_children(&self, visitor: unsafe fn(crate::gc::OpaqueGcPtr)) {
        self.unpacked_ref().visit_children(visitor);
    }

    unsafe fn finalize(&mut self) {
        ManuallyDrop::new(Self(self.0).unpack()).finalize()
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
    Condition = 12,
    Pair = 13,
    HashTable = 14,
    Other = 15,
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
            12 => Self::Condition,
            13 => Self::Pair,
            14 => Self::HashTable,
            15 => Self::Other,
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
    String(Arc<AlignedString>),
    Symbol(Arc<AlignedString>),
    Vector(Gc<vectors::AlignedVector<Value>>),
    ByteVector(Arc<vectors::AlignedVector<u8>>),
    Syntax(Arc<Syntax>),
    Closure(Gc<Closure>),
    Record(Gc<Record>),
    Condition(Gc<Condition>),
    Pair(Gc<lists::Pair>),
    // HashTable,
    OtherData(Gc<OtherData>),
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
            Self::Symbol(sym) => {
                let untagged = Arc::into_raw(sym);
                Value::from_ptr_and_tag(untagged, ValueType::Symbol)
            }
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
                let untagged = Gc::into_raw(clos);
                Value::from_mut_ptr_and_tag(untagged, ValueType::Closure)
            }
            Self::Record(rec) => {
                let untagged = Gc::into_raw(rec);
                Value::from_mut_ptr_and_tag(untagged, ValueType::Record)
            }
            Self::Condition(cond) => {
                let untagged = Gc::into_raw(cond);
                Value::from_mut_ptr_and_tag(untagged, ValueType::Condition)
            }
            Self::Pair(pair) => {
                let untagged = Gc::into_raw(pair);
                Value::from_mut_ptr_and_tag(untagged, ValueType::Pair)
            }
            Self::OtherData(other) => {
                let untagged = Gc::into_raw(other);
                Value::from_mut_ptr_and_tag(untagged, ValueType::Other)
            }
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
            Self::Record(_) | Self::Condition(_) | Self::OtherData(_) => "record",
        }
    }
}

impl PartialEq for UnpackedValue {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::Null, Self::Null) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::Character(a), Self::Character(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::Pair(a), Self::Pair(b)) => a == b,
            (Self::Vector(a), Self::Vector(b)) => a == b,
            (Self::ByteVector(a), Self::ByteVector(b)) => a == b,
            (Self::Closure(a), Self::Closure(b)) => Gc::ptr_eq(a, b),
            // TODO: Syntax
            _ => false,
        }
    }
}

impl fmt::Display for UnpackedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undefined => write!(f, "<undefined>"),
            Self::Null => write!(f, "()"),
            Self::Boolean(true) => write!(f, "#t"),
            Self::Boolean(false) => write!(f, "#f"),
            Self::Number(number) => write!(f, "{number}"),
            Self::Character(c) => write!(f, "#\\{c}"),
            Self::String(string) => write!(f, "{string}"),
            Self::Symbol(symbol) => write!(f, "{symbol}"),
            Self::Pair(pair) => {
                let pair_read = pair.read();
                let lists::Pair(car, cdr) = pair_read.as_ref();
                lists::display_list(car, cdr, f)
            }
            Self::Vector(v) => {
                let v_read = v.read();
                vectors::display_vec("#(", v_read.as_ref(), f)
            }
            Self::ByteVector(v) => vectors::display_vec("#u8(", v, f),
            Self::Closure(_) => write!(f, "<procedure>"),
            Self::Condition(cond) => write!(f, "<{cond:?}>"),
            Self::Record(record) => write!(f, "<{record:?}>"),
            Self::Syntax(syntax) => write!(f, "{:?}", syntax),
            Self::OtherData(_) => write!(f, "<unknown record>"),
        }
    }
}

impl fmt::Debug for UnpackedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undefined => write!(f, "<undefined>"),
            Self::Null => write!(f, "()"),
            Self::Boolean(true) => write!(f, "#t"),
            Self::Boolean(false) => write!(f, "#f"),
            Self::Number(number) => write!(f, "{number:?}"),
            Self::Character(c) => write!(f, "#\\{c}"),
            Self::String(string) => write!(f, "{string:?}"),
            Self::Symbol(symbol) => write!(f, "{symbol}"),
            Self::Pair(pair) => {
                let pair_read = pair.read();
                let lists::Pair(car, cdr) = pair_read.as_ref();
                lists::debug_list(car, cdr, f)
            }
            Self::Vector(v) => {
                let v_read = v.read();
                vectors::display_vec("#(", v_read.as_ref(), f)
            }
            Self::ByteVector(v) => vectors::display_vec("#u8(", v, f),
            Self::Syntax(syntax) => write!(f, "{:?}", syntax),
            Self::Closure(proc) => write!(f, "#<procedure {proc:?}>"),
            Self::Condition(cond) => write!(f, "<{cond:?}>"),
            Self::Record(record) => write!(f, "<{record:?}>"),
            Self::OtherData(_) => write!(f, "<unknown record>"),
        }
    }
}

impl From<ast::Literal> for UnpackedValue {
    fn from(lit: ast::Literal) -> Self {
        match lit {
            ast::Literal::Number(n) => Self::Number(Arc::new(n.clone())),
            ast::Literal::Boolean(b) => Self::Boolean(b),
            ast::Literal::String(s) => Self::String(Arc::new(AlignedString(s.clone()))),
            ast::Literal::Character(c) => Self::Character(c),
            ast::Literal::ByteVector(v) => {
                Self::ByteVector(Arc::new(vectors::AlignedVector(v.clone())))
            }
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
impl_try_from_value_for!(Arc<AlignedString>, String, "string");
impl_try_from_value_for!(Gc<vectors::AlignedVector<Value>>, Vector, "vector");
impl_try_from_value_for!(Arc<vectors::AlignedVector<u8>>, ByteVector, "byte-vector");
impl_try_from_value_for!(Arc<Syntax>, Syntax, "syntax");
impl_try_from_value_for!(Gc<Closure>, Closure, "procedure");
impl_try_from_value_for!(Gc<Condition>, Condition, "condition");
impl_try_from_value_for!(Gc<lists::Pair>, Pair, "pair");
impl_try_from_value_for!(Gc<OtherData>, OtherData, "record");

macro_rules! impl_from_wrapped_for {
    ($ty:ty, $variant:ident, $wrapper:expr) => {
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
impl_from_wrapped_for!(String, String, |str| Arc::new(AlignedString::new(str)));
impl_from_wrapped_for!(Vec<Value>, Vector, |vec| Gc::new(
    vectors::AlignedVector::new(vec)
));
impl_from_wrapped_for!(Vec<u8>, ByteVector, |vec| Arc::new(
    vectors::AlignedVector::new(vec)
));
impl_from_wrapped_for!(Syntax, Syntax, Arc::new);
impl_from_wrapped_for!(Closure, Closure, Gc::from);
impl_from_wrapped_for!((Value, Value), Pair, |(car, cdr)| Gc::new(
    lists::Pair::new(car, cdr)
));
impl_from_wrapped_for!(Condition, Condition, Gc::from);
impl_from_wrapped_for!(CapturedEnv, OtherData, |env| Gc::new(
    OtherData::CapturedEnv(env)
));
impl_from_wrapped_for!(Transformer, OtherData, |trans| Gc::new(
    OtherData::Transformer(trans)
));

/// Any data that doesn't fit well with the serde data model, or is otherwise
/// uncommon or unavailable in a public API.
///
///  This enum is subject to change and is not avaiable as part of a public api.
#[derive(Clone, Trace)]
#[repr(align(16))]
pub enum OtherData {
    CapturedEnv(CapturedEnv),
    Transformer(Transformer),
    // Future(Future),
    RecordType(RecordType),
    UserData(Arc<dyn std::any::Any>),
}

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
            /*
            UnpackedValue::Vector(v) => {
                let v_read = v.read();
                for val in v_read.as_ref().iter() {
                    ReflexiveValue(val.clone()).hash(state);
                }
            }
            */
            UnpackedValue::ByteVector(v) => v.hash(state),
            UnpackedValue::Syntax(s) => Arc::as_ptr(s).hash(state),
            UnpackedValue::Closure(c) => Gc::as_ptr(c).hash(state),
            UnpackedValue::Record(r) => Gc::as_ptr(r).hash(state),
            UnpackedValue::Condition(c) => Gc::as_ptr(c).hash(state),
            UnpackedValue::OtherData(o) => Gc::as_ptr(o).hash(state),
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
            /*
            (UnpackedValue::Vector(a), UnpackedValue::Vector(b)) => {
                let a_read = a.read();
                let b_read = b.read();
                a_read.as_ref().len() == b_read.as_ref().len()
                    && a_read
                        .as_ref()
                        .iter()
                        .zip(b_read.as_ref().iter())
                        .any(|(l, r)| ReflexiveValue(l.clone()) != ReflexiveValue(r.clone()))
            }
            */
            (UnpackedValue::ByteVector(a), UnpackedValue::ByteVector(b)) => a == b,
            (UnpackedValue::Syntax(a), UnpackedValue::Syntax(b)) => Arc::ptr_eq(a, b),
            (UnpackedValue::Closure(a), UnpackedValue::Closure(b)) => Gc::ptr_eq(a, b),
            (UnpackedValue::Record(a), UnpackedValue::Record(b)) => Gc::ptr_eq(a, b),
            (UnpackedValue::Condition(a), UnpackedValue::Condition(b)) => Gc::ptr_eq(a, b),
            (UnpackedValue::OtherData(a), UnpackedValue::OtherData(b)) => Gc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for ReflexiveValue {}

#[bridge(name = "not", lib = "(base)")]
pub async fn not(a: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a.0 == ValueType::Boolean as u64)])
}

#[bridge(name = "eqv?", lib = "(base)")]
pub async fn eqv(a: &Value, b: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a == b)])
}

#[bridge(name = "eq?", lib = "(base)")]
pub async fn eq(a: &Value, b: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(a.0 == b.0)])
}

#[bridge(name = "boolean?", lib = "(base)")]
pub async fn boolean_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Boolean)])
}

#[bridge(name = "boolean=?", lib = "(base)")]
pub async fn boolean_eq_pred(a: &Value, args: &[Value]) -> Result<Vec<Value>, Condition> {
    let res = if a.type_of() == ValueType::Boolean {
        args.iter().all(|arg| arg == a)
    } else {
        false
    };
    Ok(vec![Value::from(res)])
}

#[bridge(name = "symbol?", lib = "(base)")]
pub async fn symbol_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Symbol)])
}

#[bridge(name = "char?", lib = "(base)")]
pub async fn char_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Character)])
}

#[bridge(name = "vector?", lib = "(base)")]
pub async fn vector_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Vector)])
}

#[bridge(name = "null?", lib = "(base)")]
pub async fn null_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Null)])
}

#[bridge(name = "pair?", lib = "(base)")]
pub async fn pair_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Pair)])
}

#[bridge(name = "string?", lib = "(base)")]
pub async fn string_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::String)])
}

#[bridge(name = "procedure?", lib = "(base)")]
pub async fn procedure_pred(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Closure)])
}

/*
#[bridge(name = "future?", lib = "(base)")]
pub async fn future_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Future(_)
    )))])
}
*/

#[bridge(name = "display", lib = "(base)")]
pub async fn display(arg: &Value) -> Result<Vec<Value>, Condition> {
    print!("{}", arg);
    let _ = std::io::stdout().flush();
    Ok(Vec::new())
}
