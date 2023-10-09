use crate::{
    ast,
    continuation::Continuation,
    error::RuntimeError,
    expand::Transformer,
    gc::{Gc, Trace},
    num::Number,
    proc::{Callable, ExternalFn, Procedure},
    syntax::Syntax,
};
use futures::future::{BoxFuture, Shared};
use proc_macros::builtin;
use std::sync::Arc;
#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(Number),
    Character(char),
    String(String),
    Symbol(String),
    Pair(Gc<Value>, Gc<Value>),
    Vector(Vec<Gc<Value>>),
    ByteVector(Vec<u8>),
    Syntax(Syntax),
    Procedure(Procedure),
    ExternalFn(ExternalFn),
    Future(Shared<BoxFuture<'static, Value>>),
    Transformer(Transformer),
    Continuation(Option<Arc<Continuation>>),
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
                Self::Nil => "()".to_string(),
                Self::Character(c) => format!("\\x{c}"),
                Self::ByteVector(_) => "<byte_vector>".to_string(),
                Self::Syntax(syntax) => format!("{:#?}", syntax),
                Self::Procedure(_) => "<lambda>".to_string(),
                Self::ExternalFn(_) => "<external_fn>".to_string(),
                Self::Future(_) => "<future>".to_string(),
                Self::Transformer(_) => "<transformer>".to_string(),
                Self::Continuation(_) => "<continuation>".to_string(),
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
            Syntax::Nil { .. } => Self::Nil,
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
            Self::Pair(_, _) | Self::Nil => "pair",
            Self::Vector(_) => "vector",
            Self::ByteVector(_) => "byte vector",
            Self::Syntax(_) => "syntax",
            Self::Procedure(_) | Self::ExternalFn(_) | Self::Transformer(_) => "procedure",
            Self::Future(_) => "future",
            Self::Continuation(_) => "continuation",
        }
    }
}

impl Trace for Value {}

impl From<ExternalFn> for Value {
    fn from(ef: ExternalFn) -> Self {
        Value::ExternalFn(ef)
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

#[builtin("pair?")]
pub async fn is_pair(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let arg = arg.read().await;
    Ok(Gc::new(Value::Boolean(matches!(&*arg, Value::Pair(_, _)))))
}

#[builtin("d")]
pub async fn disp(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    println!("{}", arg.read().await.fmt().await);
    Ok(Gc::new(Value::Nil))
}
