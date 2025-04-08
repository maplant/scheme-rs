use crate::{
    ast,
    env::CapturedEnv,
    exception::{Condition, Exception},
    expand::Transformer,
    gc::{Gc, Trace},
    num::Number,
    proc::Closure,
    records::{Record, RecordType},
    registry::bridge,
    syntax::Syntax,
};
use futures::future::{BoxFuture, Shared};
use std::{fmt, io::Write};

type Future = Shared<BoxFuture<'static, Result<Vec<Gc<Value>>, Gc<Value>>>>;

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
    RecordType(Gc<RecordType>),
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

fn display_vec<T: fmt::Display>(
    head: &str,
    v: &[T],
    f: &mut fmt::Formatter<'_>,
) -> Result<(), fmt::Error> {
    write!(f, "{}", head)?;

    let mut iter = v.iter().peekable();
    while let Some(next) = iter.next() {
        write!(f, "{}", next)?;
        if iter.peek().is_some() {
            write!(f, " ")?;
        }
    }

    write!(f, ")")
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

impl From<Exception> for Gc<Value> {
    fn from(exception: Exception) -> Gc<Value> {
        // Until we can decide on a good method for including the stack trace with
        // the new condition, just return the object.
        exception.obj
    }
}

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

pub fn eqv(a: &Gc<Value>, b: &Gc<Value>) -> bool {
    let a = a.read();
    let b = b.read();
    a.eqv(&b)
}

#[bridge(name = "not", lib = "(base)")]
pub async fn not(a: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let a = a.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*a,
        Value::Boolean(false)
    )))])
}

#[bridge(name = "eq?", lib = "(base)")]
pub async fn eq_pred(a: &Gc<Value>, b: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    Ok(vec![Gc::new(Value::Boolean(a == b))])
}

#[bridge(name = "eqv?", lib = "(base)")]
pub async fn eqv_pred(a: &Gc<Value>, b: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    Ok(vec![Gc::new(Value::Boolean(eqv(a, b)))])
}

#[bridge(name = "boolean?", lib = "(base)")]
pub async fn boolean_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Boolean(_)
    )))])
}

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

#[bridge(name = "symbol?", lib = "(base)")]
pub async fn symbol_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Symbol(_)
    )))])
}

#[bridge(name = "char?", lib = "(base)")]
pub async fn char_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Character(_)
    )))])
}

#[bridge(name = "vector?", lib = "(base)")]
pub async fn vector_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Vector(_)
    )))])
}

#[bridge(name = "null?", lib = "(base)")]
pub async fn null_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(&*arg, Value::Null)))])
}

#[bridge(name = "pair?", lib = "(base)")]
pub async fn pair_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Pair(_, _)
    )))])
}

#[bridge(name = "string?", lib = "(base)")]
pub async fn string_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::String(_)
    )))])
}

#[bridge(name = "procedure?", lib = "(base)")]
pub async fn procedure_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Closure(_)
    )))])
}

#[bridge(name = "future?", lib = "(base)")]
pub async fn future_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Future(_)
    )))])
}

#[bridge(name = "display", lib = "(base)")]
pub async fn display(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Condition> {
    print!("{}", arg);
    let _ = std::io::stdout().flush();
    Ok(Vec::new())
}
