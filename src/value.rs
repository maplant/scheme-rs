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
use futures::future::{BoxFuture, Shared};
use std::{
    fmt, hash::Hash, io::Write, marker::PhantomData, mem::ManuallyDrop, ops::Deref, sync::Arc,
};

// type Future = Shared<BoxFuture<'static, Result<Vec<Gc<Value>>, Gc<Value>>>>;

const ALIGNMENT: u64 = 16;
const TAG_BITS: u64 = ALIGNMENT.ilog2() as u64;
const TAG: u64 = 0b1111;

/// A Scheme value. Represented as a tagged pointer.
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
                ValueType::HashTable | ValueType::Other => todo!(),
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
            Syntax::Vector { vector, .. } => {
                Self::from(vector.iter().map(Self::datum_from_syntax).collect::<Vec<_>>())
            }
            Syntax::ByteVector { vector, .. } => Self::from(vector.clone()),
            Syntax::Literal { literal, .. } => Self::from(literal.clone()),
            Syntax::Identifier { ident, .. } => {
                Self::new(UnpackedValue::Symbol(Arc::new(AlignedString(ident.name.clone()))))
            }
        }
    }

    pub fn type_of(&self) -> ValueType {
        ValueType::from(self.0 & TAG)
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
            ValueType::HashTable => todo!(),
            ValueType::Other => todo!(),
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
        &*self.unpacked_ref() == &*rhs.unpacked_ref()
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
    // OtherData,
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
            Self::Record(_) | Self::Condition(_) => "record",
            // Self::Transformer(_) => "transformer",
            // Self::CapturedEnv(_) => "captured-env",
            // Self::ExceptionHandler(_) => "exception-handler",
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
            /*
            // TODO: This shouldn't be debug output.
            Self::Syntax(syntax) => write!(f, "{:?}", syntax),
            Self::Future(_) => write!(f, "<future>"),
            // TODO: These two shouldn't be debug output either.
            Self::Record(record) => write!(f, "<{record:?}>"),
            Self::RecordType(record_type) => write!(f, "<{record_type:?}>"),
            Self::Transformer(_) => write!(f, "<transformer>"),
            Self::CapturedEnv(_) => write!(f, "<environment>"),
            Self::Condition(cond) => write!(f, "<{cond:?}>"),
            // Self::ExceptionHandler(_) => write!(f, "<exception-handler>"),
            */
            x => todo!("{}", x.type_name()),
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
            // Self::Record(record) => write!(f, "<{record:?}>"),
            // Self::Transformer(_) => write!(f, "<transformer>"),
            // Self::CapturedEnv(_) => write!(f, "<environment>"),
            // Self::Condition(cond) => write!(f, "<{cond:?}>"),
            // Self::ExceptionHandler(_) => write!(f, "<exception-handler>"),
            _ => todo!(),
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

/// Any data that doesn't fit well with the serde data model, or is otherwise
/// uncommon or unavailable in a public API.
///
///  This enum is subject to change and is not avaiable as part of a public api.
#[derive(Clone, Trace)]
pub(crate) enum OtherData {
    CapturedEnv(Gc<CapturedEnv>),
    Transformer(Transformer),
    // Future(Future),
    RecordType(Arc<RecordType>),
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
            UnpackedValue::Boolean(b) => b.hash(state),
            UnpackedValue::Character(c) => c.hash(state),
            UnpackedValue::Number(n) => ReflexiveNumber(n.clone()).hash(state),
            UnpackedValue::String(s) => s.hash(state),
            UnpackedValue::Symbol(s) => s.hash(state),
            UnpackedValue::Vector(v) => {
                let v_read = v.read();
                for val in v_read.as_ref().iter() {
                    ReflexiveValue(val.clone()).hash(state);
                }
            }
            UnpackedValue::ByteVector(v) => v.hash(state),
            UnpackedValue::Syntax(s) => Arc::as_ptr(s).hash(state),
            UnpackedValue::Closure(c) => Gc::as_ptr(c).hash(state),
            UnpackedValue::Record(r) => Gc::as_ptr(r).hash(state),
            UnpackedValue::Condition(c) => Gc::as_ptr(c).hash(state),
            // TODO: We can make this better by checking the list for equivalence reflexively.
            UnpackedValue::Pair(p) => Gc::as_ptr(p).hash(state),
            // UnpackedValue::Cls
            _ => (),
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
            (UnpackedValue::Pair(a), UnpackedValue::Pair(b)) => Gc::ptr_eq(a, b),
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
            (UnpackedValue::ByteVector(a), UnpackedValue::ByteVector(b)) => a == b,
            (UnpackedValue::Syntax(a), UnpackedValue::Syntax(b)) => Arc::ptr_eq(a, b),
            (UnpackedValue::Closure(a), UnpackedValue::Closure(b)) => Gc::ptr_eq(a, b),
            (UnpackedValue::Record(a), UnpackedValue::Record(b)) => Gc::ptr_eq(a, b),
            (UnpackedValue::Condition(a), UnpackedValue::Condition(b)) => Gc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for ReflexiveValue {}

/*
/// A Scheme value
#[derive(Trace)]
pub enum Value {
    /// The value is undefined. Variables before they are initialized are undefined.
    /// Any attempt to set a variable after creation to undefined results in an error.
    Undefined,
    /// An empty list:
    Null,
    /// Combination of two values. Has a head (car) and a tail (cdr):
    Pair(Gc<Value>, Gc<Value>),
    /// Value that is either True (#t) or False (#f):
    Boolean(bool),
    /// Numeric value:
    Number(Number),
    /// Unicode code point:
    Character(char),
    /// Vector of unicode code points:
    String(String),
    /// Atom of an S-Expression:
    Symbol(String),
    /// Vector of values:
    Vector(Vec<Value>),
    /// Vector of bytes:
    ByteVector(Vec<u8>),
    /// A wrapped syntax object:
    Syntax(Syntax),
    /// A procedure:
    Closure(Closure),
    /// A collection of named values:
    Record(Record),
    /// The type of a collection of named values:
    RecordType(()),
    /// A condition (which is also a type of record):
    Condition(Condition),
    /// A value that will exist in the future:
    Future(Future),
    /// A procedure that that transforms syntax objects:
    Transformer(Transformer),
    /// A captured lexical environment:
    CapturedEnv(CapturedEnv),
}

impl Value {
    pub fn is_undefined(&self) -> bool {
        !matches!(self, Self::Undefined)
    }

    /// #f is false, everything else is true
    pub fn is_true(&self) -> bool {
        !matches!(self, Self::Boolean(x) if !x)
    }

    pub fn is_variable_transformer(&self) -> bool {
        /*
        match self {
            Self::Procedure(ref proc) => proc.is_variable_transformer,
            // Self::Transformer(ref trans) => trans.is_variable_transformer,
            _ => false,
        }
         */
        todo!()
    }

    pub fn from_literal(literal: &ast::Literal) -> Self {
        match literal {
            ast::Literal::Number(n) => Value::Number(n.clone()),
            ast::Literal::Boolean(b) => Value::Boolean(*b),
            ast::Literal::String(s) => Value::String(s.clone()),
            ast::Literal::Character(c) => Value::Character(*c),
            _ => todo!("Literal evaluation not implemented"),
        }
    }

    pub fn from_syntax(syntax: &Syntax) -> Self {
        match syntax {
            Syntax::Null { .. } => Self::Null,
            Syntax::List { list, .. } => {
                let mut curr = Self::from_syntax(list.last().unwrap());
                for item in list[..list.len() - 1].iter().rev() {
                    curr = Self::Pair(Gc::new(Self::from_syntax(item)), Gc::new(curr));
                }
                curr
            }
            Syntax::Vector { vector, .. } => {
                Self::Vector(vector.iter().map(Self::from_syntax).collect())
            }
            Syntax::ByteVector { vector, .. } => Self::ByteVector(vector.clone()),
            Syntax::Literal { literal, .. } => Self::from_literal(literal),
            Syntax::Identifier { ident, .. } => Self::Symbol(ident.name.clone()),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Boolean(_) => "bool",
            Self::Number(_) => "number",
            Self::Character(_) => "character",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Pair(_, _) | Self::Null => "pair",
            Self::Vector(_) => "vector",
            Self::ByteVector(_) => "byte vector",
            Self::Syntax(_) => "syntax",
            Self::Closure(_) => "procedure",
            Self::Future(_) => "future",
            Self::Record(_) | Self::Condition(_) => "record",
            Self::RecordType(_) => "record-type",
            Self::Undefined => "undefined",
            Self::Transformer(_) => "transformer",
            Self::CapturedEnv(_) => "captured-env",
            // Self::ExceptionHandler(_) => "exception-handler",
        }
    }

    pub fn eqv(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::Character(a), Self::Character(b)) => a == b,
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::Pair(a1, a2), Self::Pair(b1, b2)) => eqv(a1, b1) && eqv(a2, b2),
            (Self::Vector(a), Self::Vector(b)) => {
                a.len() == b.len() && !a.iter().zip(b.iter()).any(|(l, r)| !l.eqv(r))
            }
            (Self::ByteVector(a), Self::ByteVector(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            // TODO: Syntax
            _ => false,
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Null => Self::Null,
            Self::Boolean(b) => Self::Boolean(*b),
            Self::Number(n) => Self::Number(n.clone()),
            Self::Character(c) => Self::Character(*c),
            Self::String(s) => Self::String(s.clone()),
            Self::Symbol(s) => Self::Symbol(s.clone()),
            Self::Pair(car, cdr) => {
                Self::Pair(Gc::new(car.read().clone()), Gc::new(cdr.read().clone()))
            }
            Self::Vector(vec) => Self::Vector(vec.clone()),
            Self::ByteVector(bvec) => Self::ByteVector(bvec.clone()),
            Self::Syntax(syn) => Self::Syntax(syn.clone()),
            Self::Closure(proc) => Self::Closure(proc.clone()),
            Self::Future(fut) => Self::Future(fut.clone()),
            Self::Record(record) => Self::Record(record.clone()),
            Self::RecordType(rt) => Self::RecordType(rt.clone()),
            Self::Undefined => Self::Undefined,
            Self::Transformer(trans) => Self::Transformer(trans.clone()),
            Self::CapturedEnv(cap) => Self::CapturedEnv(cap.clone()),
            Self::Condition(cond) => Self::Condition(cond.clone()),
            // Self::ExceptionHandler(eh) => Self::ExceptionHandler(eh.clone()),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(true) => write!(f, "#t"),
            Self::Boolean(false) => write!(f, "#f"),
            Self::Number(number) => write!(f, "{number}"),
            Self::String(string) => write!(f, "{string}"),
            Self::Symbol(symbol) => write!(f, "{symbol}"),
            Self::Pair(car, cdr) => crate::lists::display_list(car, cdr, f),
            Self::Vector(v) => display_vec("#(", v, f),
            Self::Null => write!(f, "()"),
            Self::Character(c) => write!(f, "#\\{c}"),
            Self::ByteVector(v) => display_vec("#u8(", v, f),
            // TODO: This shouldn't be debug output.
            Self::Syntax(syntax) => write!(f, "{:?}", syntax),
            Self::Closure(_) => write!(f, "<procedure>"),
            Self::Future(_) => write!(f, "<future>"),
            // TODO: These two shouldn't be debug output either.
            Self::Record(record) => write!(f, "<{record:?}>"),
            Self::RecordType(record_type) => write!(f, "<{record_type:?}>"),
            Self::Undefined => write!(f, "<undefined>"),
            Self::Transformer(_) => write!(f, "<transformer>"),
            Self::CapturedEnv(_) => write!(f, "<environment>"),
            Self::Condition(cond) => write!(f, "<{cond:?}>"),
            // Self::ExceptionHandler(_) => write!(f, "<exception-handler>"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(true) => write!(f, "#t"),
            Self::Boolean(false) => write!(f, "#f"),
            Self::Number(number) => write!(f, "{number:?}"),
            Self::String(string) => write!(f, "{string:?}"),
            Self::Symbol(symbol) => write!(f, "{symbol}"),
            Self::Pair(car, cdr) => crate::lists::debug_list(car, cdr, f),
            Self::Vector(v) => display_vec("#(", v, f),
            Self::Null => write!(f, "()"),
            Self::Character(c) => write!(f, "#\\{c}"),
            Self::ByteVector(v) => display_vec("#u8(", v, f),
            Self::Syntax(syntax) => write!(f, "{:?}", syntax),
            Self::Closure(proc) => write!(f, "#<procedure {proc:?}>"),
            Self::Future(_) => write!(f, "<future>"),
            Self::Record(record) => write!(f, "<{record:?}>"),
            Self::RecordType(record_type) => write!(f, "<{record_type:?}>"),
            Self::Undefined => write!(f, "<undefined>"),
            Self::Transformer(_) => write!(f, "<transformer>"),
            Self::CapturedEnv(_) => write!(f, "<environment>"),
            Self::Condition(cond) => write!(f, "<{cond:?}>"),
            // Self::ExceptionHandler(_) => write!(f, "<exception-handler>"),
        }
    }
}
*/

/*
impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Boolean(b)
    }
}

impl From<Condition> for Gc<Value> {
    fn from(cond: Condition) -> Gc<Value> {
        Gc::new(Value::Condition(cond))
    }
}
*/

/*

/// Create a proper list from a vector of values
impl From<Vec<Gc<Value>>> for Value {
    fn from(mut vec: Vec<Gc<Value>>) -> Value {
        if vec.is_empty() {
            Value::Null
        } else {
            // I'm not spending too much time thinking about a better way to do this
            let tail = vec.split_off(1);
            Value::Pair(vec.pop().unwrap(), Gc::new(Value::from(tail)))
        }
    }
}

impl TryFrom<Value> for (Gc<Value>, Gc<Value>) {
    type Error = Condition;

    fn try_from(v: Value) -> Result<(Gc<Value>, Gc<Value>), Self::Error> {
        match v {
            Value::Pair(head, tail) => Ok((head, tail)),
            e => Err(Condition::invalid_type("pair", e.type_name())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for (Gc<Value>, Gc<Value>) {
    type Error = Condition;

    fn try_from(v: &'a Value) -> Result<(Gc<Value>, Gc<Value>), Self::Error> {
        match v {
            Value::Pair(head, tail) => Ok((head.clone(), tail.clone())),
            e => Err(Condition::invalid_type("pair", e.type_name())),
        }
    }
}

impl TryFrom<Gc<Value>> for Closure {
    type Error = Condition;

    fn try_from(v: Gc<Value>) -> Result<Self, Self::Error> {
        let read = v.read();
        match &*read {
            Value::Closure(clos) => Ok(clos.clone()),
            e => Err(Condition::invalid_type("procedure", e.type_name())),
        }
    }
}

impl TryFrom<Gc<Value>> for Vec<Value> {
    type Error = Condition;

    fn try_from(v: Gc<Value>) -> Result<Self, Self::Error> {
        let read = v.read();
        match &*read {
            Value::Vector(vec) => Ok(vec.clone()),
            e => Err(Condition::invalid_type("procedure", e.type_name())),
        }
    }
}

macro_rules! impl_try_from_value_for {
    ($ty:ty, $enum_variant:ident, $type_name:literal) => {
        impl TryFrom<Value> for $ty {
            type Error = Condition;

            fn try_from(v: Value) -> Result<$ty, Self::Error> {
                match v {
                    Value::$enum_variant(i) => Ok(i),
                    e => Err(Condition::invalid_type($type_name, e.type_name())),
                }
            }
        }

        impl<'a> TryFrom<&'a mut Value> for &'a mut $ty {
            type Error = Condition;

            fn try_from(v: &'a mut Value) -> Result<&'a mut $ty, Self::Error> {
                match v {
                    Value::$enum_variant(i) => Ok(i),
                    e => Err(Condition::invalid_type($type_name, e.type_name())),
                }
            }
        }

        impl<'a> TryFrom<&'a Value> for &'a $ty {
            type Error = Condition;

            fn try_from(v: &'a Value) -> Result<&'a $ty, Self::Error> {
                match v {
                    Value::$enum_variant(i) => Ok(i),
                    e => Err(Condition::invalid_type($type_name, e.type_name())),
                }
            }
        }
    };

    ($ty:ty, $enum_variant:ident, $type_name:literal, copy) => {
        impl_try_from_value_for!($ty, $enum_variant, $type_name);

        impl TryFrom<&Value> for $ty {
            type Error = Condition;
            fn try_from(v: &Value) -> Result<$ty, Self::Error> {
                match v {
                    Value::$enum_variant(i) => Ok(*i),
                    e => Err(Condition::invalid_type($type_name, e.type_name())),
                }
            }
        }
    };
}

impl_try_from_value_for!(bool, Boolean, "bool", copy);
impl_try_from_value_for!(Number, Number, "number");
impl_try_from_value_for!(Closure, Closure, "procedure");
impl_try_from_value_for!(Record, Record, "record");
impl_try_from_value_for!(Gc<RecordType>, RecordType, "record-type");
impl_try_from_value_for!(Transformer, Transformer, "transformer");
impl_try_from_value_for!(CapturedEnv, CapturedEnv, "environment");
impl_try_from_value_for!(Syntax, Syntax, "syntax");
impl_try_from_value_for!(Vec<Value>, Vector, "vector");
impl_try_from_value_for!(char, Character, "char", copy);
impl_try_from_value_for!(String, String, "string");
*/

/*
pub fn eqv(a: &Gc<Value>, b: &Gc<Value>) -> bool {
    let a = a.read();
    let b = b.read();
    a.eqv(&b)
}
*/

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

/*
#[bridge(name = "boolean=?", lib = "(base)")]
pub async fn boolean_eq_pred(
    a: &Gc<Value>,
    args: &[Gc<Value>],
) -> Result<Vec<Gc<Value>>, Condition> {
    let a_val = &*a.read();

    let result = match a_val {
        Value::Boolean(_) => {
            let a_bool = a_val;
            args.iter().all(|arg| a_bool.eqv(&arg.read()))
        }
        _ => false,
    };
    Ok(vec![Gc::new(Value::Boolean(result))])
}
*/

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
