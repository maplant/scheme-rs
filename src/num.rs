use crate::{
    exception::Exception,
    gc::{Gc, Trace},
    registry::bridge,
    value::Value,
};
use num::{complex::Complex64, FromPrimitive, ToPrimitive, Zero};
use rug::{Complete, Integer, Rational};
use smallvec::{smallvec, SmallVec};
use std::{
    cmp::Ordering,
    fmt,
    ops::{Add, Div, Mul, Neg, Sub},
};

#[derive(Clone)]
pub enum Number {
    FixedInteger(i64),
    BigInteger(Integer),
    Rational(Rational),
    Real(f64),
    Complex(Complex64),
}

impl Number {
    #[allow(dead_code)]
    fn is_zero(&self) -> bool {
        match self {
            Self::FixedInteger(i) => i.is_zero(),
            Self::BigInteger(i) => i.is_zero(),
            Self::Rational(r) => r.is_zero(),
            Self::Real(r) => r.is_zero(),
            Self::Complex(c) => c.is_zero(),
        }
    }

    #[allow(dead_code)]
    fn is_even(&self) -> bool {
        use num::Integer;
        match self {
            Self::FixedInteger(i) => i.is_even(),
            Self::BigInteger(i) => i.is_even(),
            Self::Rational(_) => false,
            Self::Real(_) => false,
            Self::Complex(_) => false,
        }
    }

    #[allow(dead_code)]
    fn is_odd(&self) -> bool {
        use num::Integer;
        match self {
            Self::FixedInteger(i) => i.is_odd(),
            Self::BigInteger(i) => i.is_odd(),
            Self::Rational(_) => false,
            Self::Real(_) => false,
            Self::Complex(_) => false,
        }
    }

    #[allow(dead_code)]
    fn is_complex(&self) -> bool {
        matches!(self, Self::Complex(_))
    }

    /// FIXME: This code is so insanely wrong in every conceivable way that it is bordeline
    /// or even outright dangerous.
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

impl fmt::Debug for Number {
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

impl Neg for Number {
    type Output = Number;

    fn neg(self) -> Self {
        match self {
            Self::FixedInteger(i) => Self::FixedInteger(-i),
            Self::BigInteger(i) => Self::BigInteger(-i),
            Self::Rational(r) => Self::Rational(-r),
            Self::Real(r) => Self::Real(-r),
            Self::Complex(c) => Self::Complex(-c),
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

unsafe impl Trace for Number {
    unsafe fn visit_children(&self, _visitor: unsafe fn(crate::gc::OpaqueGcPtr)) {}
}

#[bridge(name = "zero?", lib = "(base)")]
pub async fn zero(arg: &Gc<Value>) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let arg = arg.read();
    let num: &Number = arg.as_ref().try_into()?;
    Ok(smallvec![Gc::new(Value::Boolean(num.is_zero()))])
}

#[bridge(name = "even?", lib = "(base)")]
pub async fn even(arg: &Gc<Value>) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let arg = arg.read();
    let num: &Number = arg.as_ref().try_into()?;
    Ok(smallvec![Gc::new(Value::Boolean(num.is_even()))])
}

#[bridge(name = "odd?", lib = "(base)")]
pub async fn odd(arg: &Gc<Value>) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let arg = arg.read();
    let num: &Number = arg.as_ref().try_into()?;
    Ok(smallvec![Gc::new(Value::Boolean(num.is_odd()))])
}

#[bridge(name = "+", lib = "(base)")]
pub async fn add(args: &[Gc<Value>]) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let mut result = Number::FixedInteger(0);
    for arg in args {
        let arg = arg.read();
        let num: &Number = arg.as_ref().try_into()?;
        result = &result + num;
    }
    Ok(smallvec![Gc::new(Value::Number(result))])
}

#[bridge(name = "-", lib = "(base)")]
pub async fn sub(
    arg1: &Gc<Value>,
    args: &[Gc<Value>],
) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let arg1 = arg1.read();
    let arg1: &Number = arg1.as_ref().try_into()?;
    if args.is_empty() {
        Ok(smallvec![Gc::new(Value::Number(-arg1.clone()))])
    } else {
        let mut result = arg1.clone();
        for arg in args {
            let arg = arg.read();
            let num: &Number = arg.as_ref().try_into()?;
            result = &result - num;
        }
        Ok(smallvec![Gc::new(Value::Number(result))])
    }
}

#[bridge(name = "*", lib = "(base)")]
pub async fn mul(args: &[Gc<Value>]) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let mut result = Number::FixedInteger(1);
    for arg in args {
        let arg = arg.read();
        let num: &Number = arg.as_ref().try_into()?;
        result = &result * num;
    }
    Ok(smallvec![Gc::new(Value::Number(result))])
}

#[bridge(name = "/", lib = "(base)")]
pub async fn div(
    arg1: &Gc<Value>,
    args: &[Gc<Value>],
) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let arg1 = arg1.read();
    let arg1: &Number = arg1.as_ref().try_into()?;
    if arg1.is_zero() {
        return Err(Exception::division_by_zero());
    }
    let mut result = &Number::FixedInteger(1) / arg1;
    for arg in args {
        let arg = arg.read();
        let num: &Number = arg.as_ref().try_into()?;
        if num.is_zero() {
            return Err(Exception::division_by_zero());
        }
        result = &result / num;
    }
    Ok(smallvec![Gc::new(Value::Number(result))])
}

#[bridge(name = "=", lib = "(base)")]
pub async fn equals(args: &[Gc<Value>]) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    if let Some((first, rest)) = args.split_first() {
        let first = first.read();
        let first: &Number = first.as_ref().try_into()?;
        for next in rest {
            let next = next.read();
            let next: &Number = next.as_ref().try_into()?;
            if first != next {
                return Ok(smallvec![Gc::new(Value::Boolean(false))]);
            }
        }
    }
    Ok(smallvec![Gc::new(Value::Boolean(true))])
}

#[bridge(name = ">", lib = "(base)")]
pub async fn greater(args: &[Gc<Value>]) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    if let Some((head, rest)) = args.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev = prev.read();
                let next = next.read();
                let prev: &Number = prev.as_ref().try_into()?;
                let next: &Number = next.as_ref().try_into()?;
                // This is somewhat less efficient for small numbers but avoids
                // cloning big ones
                if prev.is_complex() {
                    return Err(Exception::invalid_type("number", "complex"));
                }
                if next.is_complex() {
                    return Err(Exception::invalid_type("number", "complex"));
                }
                if prev <= next {
                    return Ok(smallvec![Gc::new(Value::Boolean(false))]);
                }
            }
            prev = next.clone();
        }
    }
    Ok(smallvec![Gc::new(Value::Boolean(true))])
}

#[bridge(name = ">=", lib = "(base)")]
pub async fn greater_equal(args: &[Gc<Value>]) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    if let Some((head, rest)) = args.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev = prev.read();
                let next = next.read();
                let prev: &Number = prev.as_ref().try_into()?;
                let next: &Number = next.as_ref().try_into()?;
                if prev.is_complex() {
                    return Err(Exception::invalid_type("number", "complex"));
                }
                if next.is_complex() {
                    return Err(Exception::invalid_type("number", "complex"));
                }
                if prev < next {
                    return Ok(smallvec![Gc::new(Value::Boolean(false))]);
                }
            }
            prev = next.clone();
        }
    }
    Ok(smallvec![Gc::new(Value::Boolean(true))])
}

#[bridge(name = "<", lib = "(base)")]
pub async fn lesser(args: &[Gc<Value>]) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    if let Some((head, rest)) = args.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev = prev.read();
                let next = next.read();
                let prev: &Number = prev.as_ref().try_into()?;
                let next: &Number = next.as_ref().try_into()?;
                if prev.is_complex() {
                    return Err(Exception::invalid_type("number", "complex"));
                }
                if next.is_complex() {
                    return Err(Exception::invalid_type("number", "complex"));
                }
                if prev >= next {
                    return Ok(smallvec![Gc::new(Value::Boolean(false))]);
                }
            }
            prev = next.clone();
        }
    }
    Ok(smallvec![Gc::new(Value::Boolean(true))])
}

#[bridge(name = "<=", lib = "(base)")]
pub async fn lesser_equal(args: &[Gc<Value>]) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    if let Some((head, rest)) = args.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev = prev.read();
                let next = next.read();
                let prev: &Number = prev.as_ref().try_into()?;
                let next: &Number = next.as_ref().try_into()?;
                if prev.is_complex() {
                    return Err(Exception::invalid_type("number", "complex"));
                }
                if next.is_complex() {
                    return Err(Exception::invalid_type("number", "complex"));
                }
                if prev > next {
                    return Ok(smallvec![Gc::new(Value::Boolean(false))]);
                }
            }
            prev = next.clone();
        }
    }
    Ok(smallvec![Gc::new(Value::Boolean(true))])
}

#[bridge(name = "number?", lib = "(base)")]
pub async fn is_number(arg: &Gc<Value>) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let arg = arg.read();
    Ok(smallvec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Number(_)
    )))])
}

#[bridge(name = "integer?", lib = "(base)")]
pub async fn is_integer(arg: &Gc<Value>) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let arg = arg.read();
    Ok(smallvec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Number(Number::FixedInteger(_)) | Value::Number(Number::BigInteger(_))
    )))])
}

#[bridge(name = "rational?", lib = "(base)")]
pub async fn is_rational(arg: &Gc<Value>) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let arg = arg.read();
    Ok(smallvec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Number(Number::Rational(_))
    )))])
}

#[bridge(name = "real?", lib = "(base)")]
pub async fn is_real(arg: &Gc<Value>) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let arg = arg.read();
    Ok(smallvec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Number(Number::Real(_))
    )))])
}

#[bridge(name = "complex?", lib = "(base)")]
pub async fn is_complex(arg: &Gc<Value>) -> Result<SmallVec<[Gc<Value>; 1]>, Exception> {
    let arg = arg.read();
    Ok(smallvec![Gc::new(Value::Boolean(matches!(
        &*arg,
        Value::Number(Number::Complex(_))
    )))])
}
