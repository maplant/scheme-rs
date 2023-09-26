use crate::{
    env::Env,
    eval::{RuntimeError, Value},
    gc::Gc,
};
use proc_macros::builtin;
use rug::{Complete, Complex, Float, Integer, Rational};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Number {
    Integer(Integer),
    Rational(Rational),
    Real(Float),
    Complex(Complex),
}

impl PartialEq for Number {
    fn eq(&self, _rhs: &Self) -> bool {
        todo!()
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{}", i),
            _ => todo!(),
        }
    }
}

impl From<Integer> for Number {
    fn from(i: Integer) -> Self {
        Self::Integer(i)
    }
}

impl From<Rational> for Number {
    fn from(r: Rational) -> Self {
        Self::Rational(r)
    }
}

impl From<Float> for Number {
    fn from(f: Float) -> Self {
        Self::Real(f)
    }
}

impl From<Complex> for Number {
    fn from(c: Complex) -> Self {
        Self::Complex(c)
    }
}

#[builtin("+")]
pub async fn add(_env: Env, l: &Gc<Value>, r: &Gc<Value>) -> Result<Gc<Value>, RuntimeError> {
    let (l, r) = (l.read().await, r.read().await);
    match (&*l, &*r) {
        (Value::Number(Number::Integer(l)), Value::Number(Number::Integer(r))) => {
            Ok(Gc::new(Value::Number(Number::from((l + r).complete()))))
        }
        _ => todo!(),
    }
}

#[builtin("-")]
pub async fn sub(_env: Env, l: &Gc<Value>, r: &Gc<Value>) -> Result<Gc<Value>, RuntimeError> {
    let (l, r) = (l.read().await, r.read().await);
    match (&*l, &*r) {
        (Value::Number(Number::Integer(l)), Value::Number(Number::Integer(r))) => {
            Ok(Gc::new(Value::Number(Number::from((l - r).complete()))))
        }
        _ => todo!(),
    }
}

#[builtin("*")]
pub async fn mult(_env: Env, l: &Gc<Value>, r: &Gc<Value>) -> Result<Gc<Value>, RuntimeError> {
    let (l, r) = (l.read().await, r.read().await);
    match (&*l, &*r) {
        (Value::Number(Number::Integer(l)), Value::Number(Number::Integer(r))) => {
            Ok(Gc::new(Value::Number(Number::from((l * r).complete()))))
        }
        _ => todo!(),
    }
}

#[builtin("=")]
pub async fn eq(_env: Env, l: &Gc<Value>, r: &Gc<Value>) -> Result<Gc<Value>, RuntimeError> {
    let (l, r) = (l.read().await, r.read().await);
    match (&*l, &*r) {
        (Value::Number(Number::Integer(l)), Value::Number(Number::Integer(r))) => {
            Ok(Gc::new(Value::Boolean(l == r)))
        }
        _ => todo!(),
    }
}

#[builtin(">")]
pub async fn gt(_env: Env, l: &Gc<Value>, r: &Gc<Value>) -> Result<Gc<Value>, RuntimeError> {
    let (l, r) = (l.read().await, r.read().await);
    match (&*l, &*r) {
        (Value::Number(Number::Integer(l)), Value::Number(Number::Integer(r))) => {
            Ok(Gc::new(Value::Boolean(l > r)))
        }
        _ => todo!(),
    }
}

#[builtin("<")]
pub async fn lt(_env: Env, l: &Gc<Value>, r: &Gc<Value>) -> Result<Gc<Value>, RuntimeError> {
    let (l, r) = (l.read().await, r.read().await);
    match (&*l, &*r) {
        (Value::Number(Number::Integer(l)), Value::Number(Number::Integer(r))) => {
            Ok(Gc::new(Value::Boolean(l < r)))
        }
        _ => todo!(),
    }
}
