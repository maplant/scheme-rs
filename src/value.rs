use crate::{
    ast,
    exception::Exception,
    gc::{Gc, Trace},
    num::Number,
    proc::Closure,
    records::{Record, RecordType},
    registry::bridge,
    syntax::Syntax,
};
use futures::future::{BoxFuture, Shared};
use std::fmt;

type Future = Shared<BoxFuture<'static, Result<Vec<Gc<Value>>, Exception>>>;

#[derive(Trace)]
pub enum Value {
    Undefined,
    Null,
    Boolean(bool),
    Number(Number),
    Character(char),
    String(String),
    Symbol(String),
    Pair(Gc<Value>, Gc<Value>),
    Vector(Vec<Value>),
    ByteVector(Vec<u8>),
    Syntax(Syntax),
    Closure(Gc<Closure>),
    Record(Record),
    RecordType(Gc<RecordType>),
    Future(Future),
}

impl Value {
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
            Self::Record(_) => "record",
            Self::RecordType(_) => "record-type",
            Self::Undefined => "undefined",
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
                for (a, b) in a.iter().zip(b.iter()) {
                    if !a.eqv(b) {
                        return false;
                    }
                }
                true
            }
            (Self::ByteVector(a), Self::ByteVector(b)) => a == b,
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
            Self::Vector(vec) => {
                write!(f, "#(")?;
                let mut iter = vec.iter().peekable();
                while let Some(item) = iter.next() {
                    write!(f, "{item}")?;
                    if iter.peek().is_some() {
                        write!(f, " ")?;
                    }
                }
                write!(f, ")")
            }
            Self::Null => write!(f, "()"),
            Self::Character(c) => write!(f, "\\x{c}"),
            Self::ByteVector(_) => write!(f, "<byte-vector>"),
            // TODO: This shouldn't be debug output.
            Self::Syntax(syntax) => write!(f, "{:?}", syntax),
            Self::Closure(_) => write!(f, "<procedure>"),
            Self::Future(_) => write!(f, "<future>"),
            // TODO: These two shouldn't be debug output either.
            Self::Record(record) => write!(f, "<{record:?}>"),
            Self::RecordType(record_type) => write!(f, "<{record_type:?}>"),
            Self::Undefined => write!(f, "<undefined>"),
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
            Self::Symbol(symbol) => write!(f, "{symbol:?}"),
            Self::Pair(car, cdr) => crate::lists::debug_list(car, cdr, f),
            Self::Vector(vec) => {
                write!(f, "#(")?;
                let mut iter = vec.iter().peekable();
                while let Some(item) = iter.next() {
                    write!(f, "{item:?}")?;
                    if iter.peek().is_some() {
                        write!(f, " ")?;
                    }
                }
                write!(f, ")")
            }
            Self::Null => write!(f, "()"),
            Self::Character(c) => write!(f, "\\x{c}"),
            Self::ByteVector(_) => write!(f, "<byte-vector>"),
            Self::Syntax(syntax) => write!(f, "{:?}", syntax),
            Self::Closure(_) => write!(f, "<procedure>"),
            Self::Future(_) => write!(f, "<future>"),
            Self::Record(record) => write!(f, "<{record:?}>"),
            Self::RecordType(record_type) => write!(f, "<{record_type:?}>"),
            Self::Undefined => write!(f, "<undefined>"),
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Boolean(b)
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

impl<'a> TryFrom<&'a Value> for bool {
    type Error = Exception;

    fn try_from(v: &'a Value) -> Result<bool, Self::Error> {
        match v {
            Value::Boolean(b) => Ok(*b),
            x => Err(Exception::invalid_type("bool", x.type_name())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a Number {
    type Error = Exception;

    fn try_from(v: &'a Value) -> Result<&'a Number, Self::Error> {
        match v {
            Value::Number(n) => Ok(n),
            x => Err(Exception::invalid_type("number", x.type_name())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a Gc<Closure> {
    type Error = Exception;

    fn try_from(v: &'a Value) -> Result<&'a Gc<Closure>, Self::Error> {
        match v {
            Value::Closure(proc) => Ok(proc),
            x => Err(Exception::invalid_type("procedure", x.type_name())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a Record {
    type Error = Exception;

    fn try_from(v: &'a Value) -> Result<&'a Record, Self::Error> {
        match v {
            Value::Record(r) => Ok(r),
            x => Err(Exception::invalid_type("record", x.type_name())),
        }
    }
}

impl<'a> TryFrom<&'a mut Value> for &'a mut Record {
    type Error = Exception;

    fn try_from(v: &'a mut Value) -> Result<&'a mut Record, Self::Error> {
        match v {
            Value::Record(r) => Ok(r),
            x => Err(Exception::invalid_type("record", x.type_name())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a Gc<RecordType> {
    type Error = Exception;

    fn try_from(v: &'a Value) -> Result<&'a Gc<RecordType>, Self::Error> {
        match v {
            Value::RecordType(rt) => Ok(rt),
            x => Err(Exception::invalid_type("record-type", x.type_name())),
        }
    }
}

pub fn eqv(a: &Gc<Value>, b: &Gc<Value>) -> bool {
    let a = a.read();
    let b = b.read();
    a.eqv(&b)
}

#[bridge(name = "not", lib = "(base)")]
pub async fn not(a: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let a = a.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*a,
        Value::Boolean(false)
    )))])
}

#[bridge(name = "eqv?", lib = "(base)")]
pub async fn eqv_pred(a: &Gc<Value>, b: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    Ok(vec![Gc::new(Value::Boolean(eqv(a, b)))])
}

#[bridge(name = "boolean?", lib = "(base)")]
pub async fn boolean_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Boolean(_)
    )))])
}

#[bridge(name = "symbol?", lib = "(base)")]
pub async fn symbol_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Symbol(_)
    )))])
}

#[bridge(name = "char?", lib = "(base)")]
pub async fn char_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Character(_)
    )))])
}

#[bridge(name = "vector?", lib = "(base)")]
pub async fn vector_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Vector(_)
    )))])
}

#[bridge(name = "null?", lib = "(base)")]
pub async fn null_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(&*arg, Value::Null)))])
}

#[bridge(name = "pair?", lib = "(base)")]
pub async fn pair_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Pair(_, _)
    )))])
}

#[bridge(name = "string?", lib = "(base)")]
pub async fn string_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::String(_)
    )))])
}

#[bridge(name = "procedure?", lib = "(base)")]
pub async fn procedure_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Closure(_)
    )))])
}

#[bridge(name = "future?", lib = "(base)")]
pub async fn future_pred(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Future(_)
    )))])
}

#[bridge(name = "display", lib = "(base)")]
pub async fn display(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    print!("{}", arg);
    Ok(Vec::new())
}
