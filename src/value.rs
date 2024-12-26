use crate::{
    ast,
    continuation::Continuation,
    error::RuntimeError,
    expand::Transformer,
    gc::Gc,
    num::Number,
    proc::{Callable, ExternalFn, Procedure},
    records::{Record, RecordType},
    syntax::Syntax,
    Trace,
};
use futures::future::{BoxFuture, Shared};
use proc_macros::builtin;
use std::sync::Arc;

#[derive(Trace, derive_more::Debug)]
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
    Procedure(Procedure),
    ExternalFn(ExternalFn),
    // Transformer(Transformer),
    Record(Record),
    RecordType(Gc<RecordType>),
    Future(#[debug(skip)] Shared<BoxFuture<'static, Result<Vec<Gc<Value>>, RuntimeError>>>),
    Continuation(#[debug(skip)] Option<Arc<Continuation>>),
}

impl Value {
    pub fn is_callable(&self) -> bool {
        matches!(
            self,
            Self::Procedure(_) | Self::ExternalFn(_)
        )
    }

    /// #f is false, everything else is true
    pub fn is_true(&self) -> bool {
        !matches!(self, Self::Boolean(x) if !x)
    }

    pub fn is_variable_transformer(&self) -> bool {
        match self {
            Self::Procedure(ref proc) => proc.is_variable_transformer,
            // Self::Transformer(ref trans) => trans.is_variable_transformer,
            _ => false,
        }
    }

    pub fn as_callable(&self) -> Option<Box<dyn Callable>> {
        match self {
            // Having to clone and box these kind of sucks. Hopefully we can
            // fix this at some point
            Self::Procedure(ref proc) => Some(Box::new(proc.clone())),
            Self::ExternalFn(ref proc) => Some(Box::new(*proc)),
            Self::Continuation(ref proc) => Some(Box::new(proc.clone())),
            _ => None,
        }
    }

    pub fn fmt(&self) -> String {
        match self {
            Self::Boolean(true) => "#t".to_string(),
            Self::Boolean(false) => "#f".to_string(),
            Self::Number(number) => number.to_string(),
            Self::String(string) => string.to_string(),
            Self::Symbol(symbol) => symbol.clone(),
            Self::Pair(car, cdr) => crate::lists::fmt_list(car, cdr),
            Self::Vector(vec) => {
                let mut iter = vec.iter().peekable();
                let mut output = String::from("#(");
                while let Some(item) = iter.next() {
                    output.push_str(&item.fmt());
                    if iter.peek().is_some() {
                        output.push(' ');
                    }
                }
                output.push(')');
                output
            }
            Self::Null => "()".to_string(),
            Self::Character(c) => format!("\\x{c}"),
            Self::ByteVector(_) => "<byte-vector>".to_string(),
            Self::Syntax(syntax) => format!("{:#?}", syntax),
            Self::Procedure(proc) => format!("<{proc:?}>"),
            Self::ExternalFn(_) => "<external-fn>".to_string(),
            Self::Future(_) => "<future>".to_string(),
            // Self::Transformer(_) => "<transformer>".to_string(),
            Self::Continuation(_) => "<continuation>".to_string(),
            Self::Record(_) => "<record>".to_string(),
            Self::RecordType(_) => "<record-type>".to_string(),
            Self::Undefined => "<undefined>".to_string(),
        }
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
            Self::Procedure(_) | Self::ExternalFn(_) => "procedure",
            Self::Future(_) => "future",
            Self::Continuation(_) => "continuation",
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
            Self::Procedure(proc) => Self::Procedure(proc.clone()),
            Self::ExternalFn(ext_fn) => Self::ExternalFn(*ext_fn),
            Self::Future(fut) => Self::Future(fut.clone()),
            Self::Continuation(cont) => Self::Continuation(cont.clone()),
            Self::Record(record) => Self::Record(record.clone()),
            Self::RecordType(rt) => Self::RecordType(rt.clone()),
            Self::Undefined => Self::Undefined,
        }
    }
}

impl From<ExternalFn> for Value {
    fn from(ef: ExternalFn) -> Self {
        Value::ExternalFn(ef)
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
    type Error = RuntimeError;

    fn try_from(v: &'a Value) -> Result<bool, Self::Error> {
        match v {
            Value::Boolean(b) => Ok(*b),
            x => Err(RuntimeError::invalid_type("bool", x.type_name())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a Number {
    type Error = RuntimeError;

    fn try_from(v: &'a Value) -> Result<&'a Number, Self::Error> {
        match v {
            Value::Number(n) => Ok(n),
            x => Err(RuntimeError::invalid_type("number", x.type_name())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a Record {
    type Error = RuntimeError;

    fn try_from(v: &'a Value) -> Result<&'a Record, Self::Error> {
        match v {
            Value::Record(r) => Ok(r),
            x => Err(RuntimeError::invalid_type("record", x.type_name())),
        }
    }
}

impl<'a> TryFrom<&'a mut Value> for &'a mut Record {
    type Error = RuntimeError;

    fn try_from(v: &'a mut Value) -> Result<&'a mut Record, Self::Error> {
        match v {
            Value::Record(r) => Ok(r),
            x => Err(RuntimeError::invalid_type("record", x.type_name())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a Gc<RecordType> {
    type Error = RuntimeError;

    fn try_from(v: &'a Value) -> Result<&'a Gc<RecordType>, Self::Error> {
        match v {
            Value::RecordType(rt) => Ok(rt),
            x => Err(RuntimeError::invalid_type("record-type", x.type_name())),
        }
    }
}

pub fn eqv(a: &Gc<Value>, b: &Gc<Value>) -> bool {
    let a = a.read();
    let b = b.read();
    a.eqv(&b)
}

#[builtin("not")]
pub async fn not(
    _cont: &Option<Arc<Continuation>>,
    a: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let a = a.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*a,
        Value::Boolean(false)
    )))])
}

#[builtin("eqv?")]
pub async fn eqv_pred(
    _cont: &Option<Arc<Continuation>>,
    a: &Gc<Value>,
    b: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    Ok(vec![Gc::new(Value::Boolean(eqv(a, b)))])
}

#[builtin("boolean?")]
pub async fn boolean_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Boolean(_)
    )))])
}

#[builtin("symbol?")]
pub async fn symbol_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Symbol(_)
    )))])
}

#[builtin("char?")]
pub async fn char_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Character(_)
    )))])
}

#[builtin("vector?")]
pub async fn vector_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Vector(_)
    )))])
}

#[builtin("null?")]
pub async fn null_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(&*arg, Value::Null)))])
}

#[builtin("pair?")]
pub async fn pair_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Pair(_, _)
    )))])
}

#[builtin("number?")]
pub async fn number_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Number(_)
    )))])
}

#[builtin("string?")]
pub async fn string_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::String(_)
    )))])
}

#[builtin("procedure?")]
pub async fn procedure_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Procedure(_) | Value::ExternalFn(_) | Value::Continuation(_)
    )))])
}

#[builtin("future?")]
pub async fn future_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read();
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Future(_)
    )))])
}

#[builtin("display")]
pub async fn disp(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    print!("{}", arg.read().fmt());
    Ok(vec![Gc::new(Value::Null)])
}
