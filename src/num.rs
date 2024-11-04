use crate::{continuation::Continuation, error::RuntimeError, gc::Gc, value::Value};
use num::{complex::Complex64, FromPrimitive, ToPrimitive, Zero};
use proc_macros::builtin;
use rug::{Complete, Integer, Rational};
use std::{
    cmp::Ordering,
    fmt,
    ops::{Add, Div, Mul, Sub},
    sync::Arc,
};

#[derive(Debug, Clone)]
pub enum Number {
    FixedInteger(i64),
    BigInteger(Integer),
    Rational(Rational),
    Real(f64),
    Complex(Complex64),
}

impl Number {
    fn is_zero(&self) -> bool {
        match self {
            Self::FixedInteger(i) => i.is_zero(),
            Self::BigInteger(i) => i.is_zero(),
            Self::Rational(r) => r.is_zero(),
            Self::Real(r) => r.is_zero(),
            Self::Complex(c) => c.is_zero(),
        }
    }

    fn is_complex(&self) -> bool {
        matches!(self, Self::Complex(_))
    }

    pub fn to_u64(&self) -> u64 {
        match self {
            Self::FixedInteger(i) => i.to_u64().unwrap_or(0),
            Self::BigInteger(i) => i.to_u64().unwrap_or(0),
            Self::Rational(r) => r.to_u64().unwrap_or(0),
            Self::Real(r) => r.to_u64().unwrap_or(0),
            Self::Complex(c) => c.to_u64().unwrap_or(0),
        }
    }
}

impl From<i64> for Number {
    fn from(i: i64) -> Self {
        Self::FixedInteger(i)
    }
}

impl From<Integer> for Number {
    fn from(i: Integer) -> Self {
        Self::BigInteger(i)
    }
}

impl From<Rational> for Number {
    fn from(r: Rational) -> Self {
        Self::Rational(r)
    }
}

impl From<f64> for Number {
    fn from(r: f64) -> Self {
        Self::Real(r)
    }
}

impl From<Complex64> for Number {
    fn from(c: Complex64) -> Self {
        Self::Complex(c)
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::FixedInteger(i) => write!(f, "{}", i),
            Self::BigInteger(i) => write!(f, "{}", i),
            Self::Rational(r) => write!(f, "{}", r),
            Self::Real(r) => write!(f, "{}", r),
            Self::Complex(c) => write!(f, "{}", c),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, rhs: &Self) -> bool {
        // TODO: A macro could probably greatly improve this
        match (self, rhs) {
            (Self::FixedInteger(l), Self::FixedInteger(r)) => l == r,
            (Self::FixedInteger(l), Self::BigInteger(r)) => l == r,
            (Self::FixedInteger(l), Self::Rational(r)) => l == r,
            (Self::FixedInteger(l), Self::Real(r)) => Some(*l) == r.to_i64(),
            (Self::FixedInteger(l), Self::Complex(r)) => Complex64::from_i64(*l) == Some(*r),
            (Self::BigInteger(l), Self::FixedInteger(r)) => l == r,
            (Self::BigInteger(l), Self::BigInteger(r)) => l == r,
            (Self::BigInteger(l), Self::Rational(r)) => l == r,
            (Self::BigInteger(l), Self::Real(r)) => l == r,
            (Self::BigInteger(l), Self::Complex(r)) => {
                <Integer as ToPrimitive>::to_f64(l).map(|l| Complex64::new(l, 0.0)) == Some(*r)
            }
            (Self::Rational(l), Self::FixedInteger(r)) => l == r,
            (Self::Rational(l), Self::BigInteger(r)) => l == r,
            (Self::Rational(l), Self::Rational(r)) => l == r,
            (Self::Rational(l), Self::Real(r)) => <Rational as ToPrimitive>::to_f64(l) == Some(*r),
            (Self::Rational(l), Self::Complex(r)) => {
                <Rational as ToPrimitive>::to_f64(l).map(|l| Complex64::new(l, 0.0)) == Some(*r)
            }
            (Self::Real(l), Self::FixedInteger(r)) => l.to_i64() == Some(*r),
            (Self::Real(l), Self::BigInteger(r)) => l == r,
            (Self::Real(l), Self::Rational(r)) => l == r,
            (Self::Real(l), Self::Real(r)) => l == r,
            (Self::Real(l), Self::Complex(r)) => Complex64::new(*l, 0.0) == *r,
            (Self::Complex(l), Self::FixedInteger(r)) => Some(*l) == Complex64::from_i64(*r),
            (Self::Complex(l), Self::BigInteger(r)) => {
                Some(*l) == <Integer as ToPrimitive>::to_f64(r).map(|r| Complex64::new(r, 0.0))
            }
            (Self::Complex(l), Self::Rational(r)) => {
                Some(*l) == <Rational as ToPrimitive>::to_f64(r).map(|r| Complex64::new(r, 0.0))
            }
            (Self::Complex(l), Self::Real(r)) => *l == Complex64::new(*r, 0.0),
            (Self::Complex(l), Self::Complex(r)) => l == r,
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match (self, rhs) {
            (Self::FixedInteger(l), Self::FixedInteger(r)) => l.partial_cmp(r),
            (Self::FixedInteger(l), Self::BigInteger(r)) => l.partial_cmp(r),
            (Self::FixedInteger(l), Self::Rational(r)) => l.partial_cmp(r),
            (Self::BigInteger(l), Self::FixedInteger(r)) => l.partial_cmp(r),
            (Self::BigInteger(l), Self::BigInteger(r)) => l.partial_cmp(r),
            (Self::BigInteger(l), Self::Rational(r)) => l.partial_cmp(r),
            (Self::BigInteger(l), Self::Real(r)) => l.partial_cmp(r),
            (Self::Rational(l), Self::FixedInteger(r)) => l.partial_cmp(r),
            (Self::Rational(l), Self::BigInteger(r)) => l.partial_cmp(r),
            (Self::Rational(l), Self::Rational(r)) => l.partial_cmp(r),
            (Self::Rational(l), Self::Real(r)) => l.partial_cmp(r),
            (Self::Real(l), Self::BigInteger(r)) => l.partial_cmp(r),
            (Self::Real(l), Self::Rational(r)) => l.partial_cmp(r),
            (Self::Real(l), Self::Real(r)) => l.partial_cmp(r),
            (Self::Complex(_), _) | (_, Self::Complex(_)) => None,
            // I genuinely do not know how to properly implement these. This will work for now.
            (Self::FixedInteger(l), Self::Real(r)) => Integer::from(*l).partial_cmp(r),
            (Self::Real(l), Self::FixedInteger(r)) => l.partial_cmp(&Integer::from(*r)),
        }
    }
}

macro_rules! impl_op {
    ( $trait:ident, $op:ident, $checked_op:ident ) => {
        impl<'a> $trait<&'a Number> for &'a Number {
            type Output = Number;

            fn $op(self, rhs: &'a Number) -> Number {
                // TODO: A macro could probably greatly improve this
                match (self, rhs) {
                    (Number::FixedInteger(l), Number::FixedInteger(r)) => match l.$checked_op(*r) {
                        Some(fixed) => Number::FixedInteger(fixed),
                        None => Number::BigInteger(Integer::from(*l).$op(r)),
                    },
                    (Number::FixedInteger(l), Number::BigInteger(r)) => {
                        Number::BigInteger(Integer::from(*l).$op(r))
                    }
                    (Number::FixedInteger(l), Number::Rational(r)) => {
                        Number::Rational(Rational::from((*l, 1)).$op(r))
                    }
                    (Number::FixedInteger(l), Number::Real(r)) => Number::Real((*l as f64).$op(*r)),
                    (Number::FixedInteger(l), Number::Complex(r)) => {
                        Number::Complex(Complex64::new(*l as f64, 0.0).$op(*r))
                    }
                    (Number::BigInteger(l), Number::FixedInteger(r)) => {
                        Number::BigInteger(l.$op(r).complete())
                    }
                    (Number::BigInteger(l), Number::BigInteger(r)) => {
                        Number::BigInteger(l.$op(r).complete())
                    }
                    (Number::BigInteger(l), Number::Rational(r)) => {
                        Number::Rational(Rational::from(l).$op(r))
                    }
                    (Number::BigInteger(l), Number::Real(r)) => Number::Real(l.to_f64().$op(r)),
                    (Number::BigInteger(l), Number::Complex(r)) => {
                        Number::Complex(Complex64::new(l.to_f64(), 0.0).$op(r))
                    }
                    (Number::Rational(l), Number::FixedInteger(r)) => {
                        Number::Rational(l.$op(Rational::from((*r, 1))))
                    }
                    (Number::Rational(l), Number::BigInteger(r)) => {
                        Number::Rational(l.$op(Rational::from(r)))
                    }
                    (Number::Rational(l), Number::Rational(r)) => {
                        Number::Rational(l.$op(r).complete())
                    }
                    (Number::Rational(l), Number::Real(r)) => Number::Real(l.to_f64().$op(r)),
                    (Number::Rational(l), Number::Complex(r)) => {
                        Number::Complex(Complex64::new(l.to_f64(), 0.0).$op(r))
                    }
                    (Number::Real(l), Number::FixedInteger(r)) => Number::Real(l.$op(*r as f64)),
                    (Number::Real(l), Number::BigInteger(r)) => Number::Real(l.$op(r.to_f64())),
                    (Number::Real(l), Number::Rational(r)) => Number::Real(l.$op(r.to_f64())),
                    (Number::Real(l), Number::Real(r)) => Number::Real(l.$op(r)),
                    (Number::Real(l), Number::Complex(r)) => {
                        Number::Complex(Complex64::new(*l, 0.0).$op(r))
                    }
                    (Number::Complex(l), Number::FixedInteger(r)) => {
                        Number::Complex(l.$op(Complex64::new(*r as f64, 0.0)))
                    }
                    (Number::Complex(l), Number::BigInteger(r)) => {
                        Number::Complex(l.$op(Complex64::new(r.to_f64(), 0.0)))
                    }
                    (Number::Complex(l), Number::Rational(r)) => {
                        Number::Complex(l.$op(Complex64::new(r.to_f64(), 0.0)))
                    }
                    (Number::Complex(l), Number::Real(r)) => {
                        Number::Complex(l.$op(Complex64::new(*r, 0.0)))
                    }
                    (Number::Complex(l), Number::Complex(r)) => Number::Complex(l.$op(r)),
                }
            }
        }
    };
}

impl_op!(Add, add, checked_add);
impl_op!(Sub, sub, checked_sub);
impl_op!(Mul, mul, checked_mul);

impl<'a> Div<&'a Number> for &'a Number {
    type Output = Number;

    fn div(self, rhs: &'a Number) -> Number {
        // TODO: A macro could probably greatly improve this
        match (self, rhs) {
            (Number::FixedInteger(l), Number::FixedInteger(r)) => {
                Number::Rational(Rational::from((*l, *r)))
            }
            (Number::FixedInteger(l), Number::BigInteger(r)) => {
                Number::Rational(Rational::from((*l, r)))
            }
            (Number::FixedInteger(l), Number::Rational(r)) => {
                Number::Rational(Rational::from((*l, 1)) / r)
            }
            (Number::FixedInteger(l), Number::Real(r)) => Number::Real((*l as f64) / *r),
            (Number::FixedInteger(l), Number::Complex(r)) => {
                Number::Complex(Complex64::new(*l as f64, 0.0) / *r)
            }
            (Number::BigInteger(l), Number::FixedInteger(r)) => {
                Number::Rational(Rational::from((l, *r)))
            }
            (Number::BigInteger(l), Number::BigInteger(r)) => {
                Number::Rational(Rational::from((l, r)))
            }
            (Number::BigInteger(l), Number::Rational(r)) => Number::Rational(Rational::from(l) / r),
            (Number::BigInteger(l), Number::Real(r)) => Number::Real(l.to_f64() / r),
            (Number::BigInteger(l), Number::Complex(r)) => {
                Number::Complex(Complex64::new(l.to_f64(), 0.0) / r)
            }
            (Number::Rational(l), Number::FixedInteger(r)) => {
                Number::Rational(l / Rational::from((*r, 1)))
            }
            (Number::Rational(l), Number::BigInteger(r)) => Number::Rational((l / r).complete()),

            (Number::Rational(l), Number::Rational(r)) => Number::Rational((l / r).complete()),
            (Number::Rational(l), Number::Real(r)) => Number::Real(l.to_f64() / r),
            (Number::Rational(l), Number::Complex(r)) => {
                Number::Complex(Complex64::new(l.to_f64(), 0.0) / r)
            }
            (Number::Real(l), Number::FixedInteger(r)) => Number::Real(l / *r as f64),
            (Number::Real(l), Number::BigInteger(r)) => Number::Real(l / r.to_f64()),
            (Number::Real(l), Number::Rational(r)) => Number::Real(l / r.to_f64()),
            (Number::Real(l), Number::Real(r)) => Number::Real(l / r),
            (Number::Real(l), Number::Complex(r)) => Number::Complex(Complex64::new(*l, 0.0) / r),
            (Number::Complex(l), Number::FixedInteger(r)) => {
                Number::Complex(l / Complex64::new(*r as f64, 0.0))
            }
            (Number::Complex(l), Number::BigInteger(r)) => {
                Number::Complex(l / Complex64::new(r.to_f64(), 0.0))
            }
            (Number::Complex(l), Number::Rational(r)) => {
                Number::Complex(l / Complex64::new(r.to_f64(), 0.0))
            }
            (Number::Complex(l), Number::Real(r)) => Number::Complex(l / Complex64::new(*r, 0.0)),
            (Number::Complex(l), Number::Complex(r)) => Number::Complex(l / r),
        }
    }
}

#[builtin("+")]
pub async fn add(
    _cont: &Option<Arc<Continuation>>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    let mut result = Number::FixedInteger(0);
    for arg in args {
        let arg = arg.read().await;
        let num: &Number = arg.as_ref().try_into()?;
        result = &result + num;
    }
    Ok(Gc::new(Value::Number(result)))
}

#[builtin("-")]
pub async fn sub(
    _cont: &Option<Arc<Continuation>>,
    arg1: &Gc<Value>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    let arg1 = arg1.read().await;
    let arg1: &Number = arg1.as_ref().try_into()?;
    let mut result = arg1.clone();
    for arg in args {
        let arg = arg.read().await;
        let num: &Number = arg.as_ref().try_into()?;
        result = &result - num;
    }
    Ok(Gc::new(Value::Number(result)))
}

#[builtin("*")]
pub async fn mul(
    _cont: &Option<Arc<Continuation>>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    let mut result = Number::FixedInteger(1);
    for arg in args {
        let arg = arg.read().await;
        let num: &Number = arg.as_ref().try_into()?;
        result = &result * num;
    }
    Ok(Gc::new(Value::Number(result)))
}

#[builtin("/")]
pub async fn div(
    _cont: &Option<Arc<Continuation>>,
    arg1: &Gc<Value>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    let arg1 = arg1.read().await;
    let arg1: &Number = arg1.as_ref().try_into()?;
    if arg1.is_zero() {
        return Err(RuntimeError::division_by_zero());
    }
    let mut result = &Number::FixedInteger(1) / arg1;
    for arg in args {
        let arg = arg.read().await;
        let num: &Number = arg.as_ref().try_into()?;
        if num.is_zero() {
            return Err(RuntimeError::division_by_zero());
        }
        result = &result / num;
    }
    Ok(Gc::new(Value::Number(result)))
}

#[builtin("=")]
pub async fn equals(
    _cont: &Option<Arc<Continuation>>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    if let Some((first, rest)) = args.split_first() {
        let first = first.read().await;
        let first: &Number = first.as_ref().try_into()?;
        for next in rest {
            let next = next.read().await;
            let next: &Number = next.as_ref().try_into()?;
            if first != next {
                return Ok(Gc::new(Value::Boolean(false)));
            }
        }
    }
    Ok(Gc::new(Value::Boolean(true)))
}

#[builtin(">")]
pub async fn greater(
    _cont: &Option<Arc<Continuation>>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    if let Some((head, rest)) = args.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev = prev.read().await;
                let next = next.read().await;
                let prev: &Number = prev.as_ref().try_into()?;
                let next: &Number = next.as_ref().try_into()?;
                // This is somewhat less efficient for small numbers but avoids
                // cloning big ones
                if prev.is_complex() {
                    return Err(RuntimeError::invalid_type("number", "complex"));
                }
                if next.is_complex() {
                    return Err(RuntimeError::invalid_type("number", "complex"));
                }
                if prev <= next {
                    return Ok(Gc::new(Value::Boolean(false)));
                }
            }
            prev = next.clone();
        }
    }
    Ok(Gc::new(Value::Boolean(true)))
}

#[builtin(">=")]
pub async fn greater_equal(
    _cont: &Option<Arc<Continuation>>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    if let Some((head, rest)) = args.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev = prev.read().await;
                let next = next.read().await;
                let prev: &Number = prev.as_ref().try_into()?;
                let next: &Number = next.as_ref().try_into()?;
                if prev.is_complex() {
                    return Err(RuntimeError::invalid_type("number", "complex"));
                }
                if next.is_complex() {
                    return Err(RuntimeError::invalid_type("number", "complex"));
                }
                if prev < next {
                    return Ok(Gc::new(Value::Boolean(false)));
                }
            }
            prev = next.clone();
        }
    }
    Ok(Gc::new(Value::Boolean(true)))
}

#[builtin("<")]
pub async fn lesser(
    _cont: &Option<Arc<Continuation>>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    if let Some((head, rest)) = args.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev = prev.read().await;
                let next = next.read().await;
                let prev: &Number = prev.as_ref().try_into()?;
                let next: &Number = next.as_ref().try_into()?;
                if prev.is_complex() {
                    return Err(RuntimeError::invalid_type("number", "complex"));
                }
                if next.is_complex() {
                    return Err(RuntimeError::invalid_type("number", "complex"));
                }
                if prev >= next {
                    return Ok(Gc::new(Value::Boolean(false)));
                }
            }
            prev = next.clone();
        }
    }
    Ok(Gc::new(Value::Boolean(true)))
}

#[builtin("<=")]
pub async fn lesser_equal(
    _cont: &Option<Arc<Continuation>>,
    args: Vec<Gc<Value>>,
) -> Result<Gc<Value>, RuntimeError> {
    if let Some((head, rest)) = args.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev = prev.read().await;
                let next = next.read().await;
                let prev: &Number = prev.as_ref().try_into()?;
                let next: &Number = next.as_ref().try_into()?;
                if prev.is_complex() {
                    return Err(RuntimeError::invalid_type("number", "complex"));
                }
                if next.is_complex() {
                    return Err(RuntimeError::invalid_type("number", "complex"));
                }
                if prev > next {
                    return Ok(Gc::new(Value::Boolean(false)));
                }
            }
            prev = next.clone();
        }
    }
    Ok(Gc::new(Value::Boolean(true)))
}

#[builtin("number?")]
pub async fn is_number(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let arg = arg.read().await;
    Ok(Gc::new(Value::Boolean(matches!(&*arg, Value::Number(_)))))
}

#[builtin("integer?")]
pub async fn is_integer(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let arg = arg.read().await;
    Ok(Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Number(Number::FixedInteger(_)) | Value::Number(Number::BigInteger(_))
    ))))
}

#[builtin("rational?")]
pub async fn is_rational(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let arg = arg.read().await;
    Ok(Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Number(Number::Rational(_))
    ))))
}

#[builtin("real?")]
pub async fn is_real(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let arg = arg.read().await;
    Ok(Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Number(Number::Real(_))
    ))))
}

#[builtin("complex?")]
pub async fn is_complex(
    _cont: &Option<Arc<Continuation>>,
    arg: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let arg = arg.read().await;
    Ok(Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Number(Number::Complex(_))
    ))))
}
