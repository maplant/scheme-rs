use crate::{
    ast,
    continuation::Continuation,
    error::RuntimeError,
    expand::Transformer,
    gc::Gc,
    num::Number,
    proc::{Callable, ExternalFn, Procedure},
    syntax::Syntax,
    Trace,
};
use futures::future::{BoxFuture, Shared};
use proc_macros::builtin;
use std::sync::Arc;

#[derive(Clone, Trace, derive_more::Debug)]
pub enum Value {
    Null,
    Boolean(bool),
    Number(Number),
    Character(char),
    String(String),
    Symbol(String),
    Pair(#[debug(skip)] Gc<Value>, #[debug(skip)] Gc<Value>),
    Vector(#[debug(skip)] Vec<Gc<Value>>),
    ByteVector(Vec<u8>),
    Syntax(Syntax),
    Procedure(Procedure),
    ExternalFn(ExternalFn),
    Future(#[debug(skip)] Shared<BoxFuture<'static, Result<Vec<Gc<Value>>, RuntimeError>>>),
    Transformer(#[debug(skip)] Transformer),
    Continuation(#[debug(skip)] Option<Arc<Continuation>>),
    Undefined,
}

impl Value {
    pub fn is_callable(&self) -> bool {
        matches!(
            self,
            Self::Procedure(_) | Self::ExternalFn(_) | Self::Transformer(_)
        )
    }

    /// #f is false, everything else is true
    pub fn is_true(&self) -> bool {
        !matches!(self, Self::Boolean(x) if !x)
    }

    pub fn is_variable_transformer(&self) -> bool {
        match self {
            Self::Procedure(ref proc) => proc.is_variable_transformer,
            Self::Transformer(ref trans) => trans.is_variable_transformer,
            _ => false,
        }
    }

    pub fn as_callable(&self) -> Option<Box<dyn Callable>> {
        match self {
            // Having to clone and box these kind of sucks. Hopefully we can
            // fix this at some point
            Self::Procedure(ref proc) => Some(Box::new(proc.clone())),
            Self::ExternalFn(ref proc) => Some(Box::new(proc.clone())),
            Self::Continuation(ref proc) => Some(Box::new(proc.clone())),
            _ => None,
        }
    }

    pub fn fmt(&self) -> BoxFuture<'_, String> {
        Box::pin(async move {
            match self {
                Self::Boolean(true) => "#t".to_string(),
                Self::Boolean(false) => "#f".to_string(),
                Self::Number(number) => number.to_string(),
                Self::String(string) => format!("\"{string}\""),
                Self::Symbol(symbol) => symbol.clone(),
                Self::Pair(car, cdr) => crate::lists::fmt_list(car, cdr).await,
                Self::Vector(vec) => {
                    let mut iter = vec.iter().peekable();
                    let mut output = String::from("#(");
                    while let Some(item) = iter.next() {
                        output.push_str(&item.read().await.fmt().await);
                        if iter.peek().is_some() {
                            output.push(' ');
                        }
                    }
                    output.push(')');
                    output
                }
                Self::Null => "()".to_string(),
                Self::Character(c) => format!("\\x{c}"),
                Self::ByteVector(_) => "<byte_vector>".to_string(),
                Self::Syntax(syntax) => format!("{:#?}", syntax),
                Self::Procedure(proc) => format!("<{proc:?}>"),
                Self::ExternalFn(_) => "<external_fn>".to_string(),
                Self::Future(_) => "<future>".to_string(),
                Self::Transformer(_) => "<transformer>".to_string(),
                Self::Continuation(_) => "<continuation>".to_string(),
                Self::Undefined => "<undefined>".to_string(),
            }
        })
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
                Self::Vector(vector.iter().map(Self::from_syntax).map(Gc::new).collect())
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
            Self::Procedure(_) | Self::ExternalFn(_) | Self::Transformer(_) => "procedure",
            Self::Future(_) => "future",
            Self::Continuation(_) => "continuation",
            Self::Undefined => "undefined",
        }
    }

    pub async fn eqv(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::Character(a), Self::Character(b)) => a == b,
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::Pair(a1, a2), Self::Pair(b1, b2)) => eqv(a1, b1).await && eqv(a2, b2).await,
            (Self::Vector(a), Self::Vector(b)) => {
                for (a, b) in a.iter().zip(b.iter()) {
                    if !eqv(a, b).await {
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

pub fn eqv<'a>(a: &'a Gc<Value>, b: &'a Gc<Value>) -> BoxFuture<'a, bool> {
    Box::pin(async move {
        let a = a.read().await;
        let b = b.read().await;
        a.eqv(&b).await
    })
}

#[builtin("not")]
pub async fn not(
    _cont: &Option<Arc<Continuation>>,
    a: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let a = a.read().await;
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
    Ok(vec![Gc::new(Value::Boolean(eqv(a, b).await))])
}

#[builtin("boolean?")]
pub async fn boolean_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read().await;
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
    let arg = arg.read().await;
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
    let arg = arg.read().await;
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
    let arg = arg.read().await;
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
    let arg = arg.read().await;
    Ok(vec![Gc::new(Value::Boolean(matches!(&*arg, Value::Null)))])
}

#[builtin("pair?")]
pub async fn pair_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read().await;
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
    let arg = arg.read().await;
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
    let arg = arg.read().await;
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
    let arg = arg.read().await;
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Procedure(_) | Value::ExternalFn(_) | Value::Transformer(_)
    )))])
}

#[builtin("future?")]
pub async fn future_pred(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let arg = arg.read().await;
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
    println!("{}", arg.read().await.fmt().await);
    Ok(vec![Gc::new(Value::Null)])
}
