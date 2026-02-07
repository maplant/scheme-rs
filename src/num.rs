//! Scheme numerical tower.
//!
//! For more detailed information about the scheme numerical tower from the
//! language perspective, please [refer to the language reference](www.scheme.rs/Language Reference/#numeric-tower).
//!
//! The core numeric value for scheme-rs is [`Number`], which is a reference
//! counted numeric value that can be any of the following types:
//!
//! - **Fixed num**: [a signed 64-bit integer](std::i64),
//! - **Big num**: an [arbitrary-precision integer](malachite::Integer),
//! - **Rational**: an
//!   [arbitrary-precision rational](malachite::rational::Rational),
//! - **Real**: a [64-bit IEEE float point number](std::f64), or
//! - **Complex**: a pair of two of any of the previous value types.
//!
//! `Number` implements all of the expected Rust operators, including [`Neg`],
//! [`Add`], [`Mul`], [`Sub`], [`Div`], [`Rem`], [`PartialEq`], and
//! [`PartialOrd`], while also inheriting all of the issues of IEEE floating
//! point numbers such as [NaN not being equal to itself](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN)
//! and therefore being unable to implement [`Eq`]. To get around these issues
//! in cases where it is desirable to have strict equivalent for `Number` types,
//! such as when `Number` is a key in a hash table, the [`eqv`](Number::eqv)
//! method is provided, for which NaNs are treated as equivalent.

use crate::{
    exceptions::Exception,
    gc::Trace,
    ports::{BufferMode, Port, Transcoder},
    registry::bridge,
    strings::WideString,
    syntax::{Span, lex::Lexer},
    value::{Value, ValueType},
};
use core::f64;
use malachite::{
    Integer,
    base::{
        num::{
            arithmetic::traits::{FloorSqrt, Parity, Pow},
            conversion::traits::{ConvertibleFrom, RoundingFrom, WrappingFrom},
        },
        rounding_modes::RoundingMode,
    },
    rational::Rational,
};
use num::ToPrimitive;
use scheme_rs_macros::{maybe_async, maybe_await};
use std::{
    cmp::Ordering,
    fmt,
    hash::Hash,
    io::Cursor,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
    sync::Arc,
};

#[repr(align(16))]
pub(crate) enum NumberInner {
    Simple(SimpleNumber),
    Complex(ComplexNumber),
}

/// Scheme numeric type.
#[derive(Clone, Trace)]
pub struct Number(pub(crate) Arc<NumberInner>);

macro_rules! number_dispatch_method {
    ( $func:ident ) => {
        pub fn $func(&self) -> Self {
            match self.0.as_ref() {
                NumberInner::Simple(simple) => Number::from(simple.$func()),
                NumberInner::Complex(complex) => Number::from(complex.$func()),
            }
        }
    };
}

impl Number {
    pub fn as_simple(&self) -> Option<&SimpleNumber> {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => Some(simple),
            NumberInner::Complex(_) => None,
        }
    }

    pub fn as_complex(&self) -> Option<&ComplexNumber> {
        match self.0.as_ref() {
            NumberInner::Complex(complex) => Some(complex),
            NumberInner::Simple(_) => None,
        }
    }

    pub fn is_zero(&self) -> bool {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => simple.is_zero(),
            NumberInner::Complex(complex) => complex.re.is_zero() && complex.im.is_zero(),
        }
    }

    pub fn is_integer(&self) -> bool {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => simple.is_integer(),
            NumberInner::Complex(complex) => {
                complex.im.is_exact() && complex.im.is_zero() && complex.re.is_integer()
            }
        }
    }

    pub fn is_rational(&self) -> bool {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => simple.is_rational(),
            NumberInner::Complex(complex) => {
                complex.im.is_exact() && complex.im.is_zero() && complex.re.is_rational()
            }
        }
    }

    pub fn is_real(&self) -> bool {
        match self.0.as_ref() {
            NumberInner::Simple(_) => true,
            NumberInner::Complex(complex) => complex.im.is_exact() && complex.im.is_zero(),
        }
    }

    pub fn is_integer_valued(&self) -> bool {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => simple.is_integer(),
            NumberInner::Complex(complex) => complex.im.is_zero() && complex.re.is_integer(),
        }
    }

    pub fn is_rational_valued(&self) -> bool {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => simple.is_rational(),
            NumberInner::Complex(complex) => complex.im.is_zero() && complex.re.is_rational(),
        }
    }

    pub fn is_real_valued(&self) -> bool {
        match self.0.as_ref() {
            NumberInner::Simple(_) => true,
            NumberInner::Complex(complex) => complex.im.is_zero(),
        }
    }

    /// All numbers are complex!
    pub fn is_complex(&self) -> bool {
        true
    }

    pub fn is_exact(&self) -> bool {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => simple.is_exact(),
            NumberInner::Complex(complex) => complex.im.is_exact() && complex.re.is_exact(),
        }
    }

    pub fn is_inexact(&self) -> bool {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => !simple.is_exact(),
            NumberInner::Complex(complex) => !complex.im.is_exact() || !complex.re.is_exact(),
        }
    }

    pub fn inexact(self) -> Number {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => Number::from(simple.inexact()),
            NumberInner::Complex(complex) => Number::from(complex.inexact()),
        }
    }

    pub fn exact(self) -> Number {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => Number::from(simple.exact()),
            NumberInner::Complex(complex) => Number::from(complex.exact()),
        }
    }

    pub fn eqv(&self, rhs: &Self) -> bool {
        match (self.0.as_ref(), rhs.0.as_ref()) {
            (NumberInner::Simple(lhs), NumberInner::Simple(rhs)) => lhs.eqv(rhs),
            (NumberInner::Complex(lhs), NumberInner::Simple(rhs))
                if lhs.im.is_zero() && lhs.im.is_exact() == rhs.is_exact() =>
            {
                lhs.re.eqv(rhs)
            }
            (NumberInner::Simple(lhs), NumberInner::Complex(rhs))
                if rhs.im.is_zero() && rhs.im.is_exact() == lhs.is_exact() =>
            {
                lhs.eqv(&rhs.re)
            }
            (NumberInner::Complex(lhs), NumberInner::Complex(rhs)) => {
                lhs.re.eqv(&rhs.re) && lhs.im.eqv(&rhs.im)
            }
            _ => false,
        }
    }

    pub fn pow(&self, rhs: &Number) -> Number {
        match (self.0.as_ref(), rhs.0.as_ref()) {
            (NumberInner::Simple(base), NumberInner::Simple(exp)) => Number::from(base.pow(exp)),
            (NumberInner::Simple(base), NumberInner::Complex(exp)) => {
                Number::from(ComplexNumber::new(base.clone(), SimpleNumber::zero()).powc(exp))
            }
            (NumberInner::Complex(base), NumberInner::Simple(exp)) => {
                Number::from(base.powc(&ComplexNumber::new(exp.clone(), SimpleNumber::zero())))
            }
            (NumberInner::Complex(base), NumberInner::Complex(exp)) => Number::from(base.powc(exp)),
        }
    }

    number_dispatch_method!(exp);
    number_dispatch_method!(ln);
    number_dispatch_method!(sqrt);
    number_dispatch_method!(sin);
    number_dispatch_method!(cos);
    number_dispatch_method!(tan);
    // number_dispatch_method!(sinh);
    // number_dispatch_method!(cosh);
    // number_dispatch_method!(tanh);
    number_dispatch_method!(asin);
    number_dispatch_method!(acos);
    number_dispatch_method!(atan);
}

impl From<NumberInner> for Number {
    fn from(value: NumberInner) -> Self {
        Self(Arc::new(value))
    }
}

impl<T> From<T> for Number
where
    SimpleNumber: From<T>,
{
    fn from(value: T) -> Self {
        Self(Arc::new(NumberInner::Simple(SimpleNumber::from(value))))
    }
}

macro_rules! impl_simple_number_int_conversion_for_num {
    ($ty:ty) => {
        impl TryFrom<&Number> for $ty {
            type Error = Exception;

            fn try_from(num: &Number) -> Result<$ty, Self::Error> {
                match num.0.as_ref() {
                    NumberInner::Simple(simple) => simple.try_into(),
                    NumberInner::Complex(complex)
                        if complex.im.is_exact() && complex.im.is_zero() =>
                    {
                        <$ty as TryFrom<&SimpleNumber>>::try_from(&complex.re)
                    }
                    _ => Err(Exception::conversion_error(stringify!($ty), "complex")),
                }
            }
        }

        impl TryFrom<Number> for $ty {
            type Error = Exception;

            fn try_from(num: Number) -> Result<$ty, Self::Error> {
                <$ty as TryFrom<&Number>>::try_from(&num)
            }
        }

        impl From<&Number> for Option<$ty> {
            fn from(num: &Number) -> Option<$ty> {
                match num.0.as_ref() {
                    NumberInner::Simple(simple) => simple.into(),
                    NumberInner::Complex(complex)
                        if complex.im.is_exact() && complex.im.is_zero() =>
                    {
                        <Option<$ty> as From<&SimpleNumber>>::from(&complex.re)
                    }
                    _ => None,
                }
            }
        }

        impl From<Number> for Option<$ty> {
            fn from(num: Number) -> Option<$ty> {
                Self::from(&num)
            }
        }
    };
}

impl_simple_number_int_conversion_for_num!(u8);
impl_simple_number_int_conversion_for_num!(u16);
impl_simple_number_int_conversion_for_num!(u32);
impl_simple_number_int_conversion_for_num!(u64);
impl_simple_number_int_conversion_for_num!(u128);
impl_simple_number_int_conversion_for_num!(usize);
impl_simple_number_int_conversion_for_num!(i8);
impl_simple_number_int_conversion_for_num!(i16);
impl_simple_number_int_conversion_for_num!(i32);
impl_simple_number_int_conversion_for_num!(i64);
impl_simple_number_int_conversion_for_num!(i128);
impl_simple_number_int_conversion_for_num!(isize);
impl_simple_number_int_conversion_for_num!(Integer);

/// Scheme simple numeric type (i.e. numbers that satisfy `real?`)
#[derive(Clone)]
#[repr(align(16))]
pub enum SimpleNumber {
    FixedInteger(i64),
    BigInteger(Integer),
    Rational(Rational),
    Real(f64),
}

impl SimpleNumber {
    pub fn zero() -> Self {
        Self::FixedInteger(0)
    }

    pub fn one() -> Self {
        Self::FixedInteger(1)
    }

    pub fn infinity() -> Self {
        Self::Real(f64::INFINITY)
    }

    pub fn neg_infinity() -> Self {
        Self::Real(f64::NEG_INFINITY)
    }

    pub fn is_zero(&self) -> bool {
        match self {
            SimpleNumber::FixedInteger(i) => *i == 0,
            SimpleNumber::BigInteger(i) => *i == 0,
            SimpleNumber::Rational(r) => *r == 0,
            SimpleNumber::Real(r) => *r == 0.0,
        }
    }

    pub fn is_one(&self) -> bool {
        match self {
            SimpleNumber::FixedInteger(i) => *i == 1,
            SimpleNumber::BigInteger(i) => *i == 1,
            SimpleNumber::Rational(r) => *r == 1,
            SimpleNumber::Real(r) => *r == 1.0,
        }
    }

    pub fn is_neg_one(&self) -> bool {
        match self {
            SimpleNumber::FixedInteger(i) => *i == -1,
            SimpleNumber::BigInteger(i) => *i == -1,
            SimpleNumber::Rational(r) => *r == -1,
            SimpleNumber::Real(r) => *r == -1.0,
        }
    }

    pub fn is_exact(&self) -> bool {
        matches!(
            self,
            Self::FixedInteger(_) | Self::BigInteger(_) | Self::Rational(_)
        )
    }

    pub fn to_string(&self, radix: u32, precision: Option<usize>) -> Option<String> {
        match radix {
            2 => match self {
                SimpleNumber::FixedInteger(i) => Some(format!("{i:b}")),
                SimpleNumber::BigInteger(i) => Some(format!("{i:b}")),
                SimpleNumber::Rational(r) => {
                    let (n, d) = r.numerator_and_denominator_ref();
                    if *r < 0 {
                        Some(format!("-{n:b}/{d:b}"))
                    } else {
                        Some(format!("{n:b}/{d:b}"))
                    }
                }
                SimpleNumber::Real(_) => None,
            },
            8 => match self {
                SimpleNumber::FixedInteger(i) => Some(format!("{i:o}")),
                SimpleNumber::BigInteger(i) => Some(format!("{i:o}")),
                SimpleNumber::Rational(r) => {
                    let (n, d) = r.numerator_and_denominator_ref();
                    if *r < 0 {
                        Some(format!("-{n:o}/{d:o}"))
                    } else {
                        Some(format!("{n:o}/{d:o}"))
                    }
                }
                SimpleNumber::Real(_) => None,
            },
            10 => match self {
                SimpleNumber::FixedInteger(i) => Some(format!("{i}")),
                SimpleNumber::BigInteger(i) => Some(format!("{i}")),
                SimpleNumber::Rational(r) => Some(format!("{r}")),
                SimpleNumber::Real(r) => {
                    if let Some(precision) = precision {
                        Some(format!("{r:.precision$}"))
                    } else {
                        Some(format!("{r}"))
                    }
                }
            },
            16 => match self {
                SimpleNumber::FixedInteger(i) => Some(format!("{i:X}")),
                SimpleNumber::BigInteger(i) => Some(format!("{i:X}")),
                SimpleNumber::Rational(r) => {
                    let (n, d) = r.numerator_and_denominator_ref();
                    if *r < 0 {
                        Some(format!("-{n:X}/{d:X}"))
                    } else {
                        Some(format!("{n:X}/{d:X}"))
                    }
                }
                SimpleNumber::Real(_) => None,
            },
            _ => None,
        }
    }

    pub fn is_nan(&self) -> bool {
        if let Self::Real(real) = self {
            real.is_nan()
        } else {
            false
        }
    }

    pub fn is_finite(&self) -> bool {
        matches!(self, Self::Real(r) if r.is_finite())
    }

    pub fn is_infinite(&self) -> bool {
        if let Self::Real(real) = self {
            real.is_infinite()
        } else {
            false
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            SimpleNumber::FixedInteger(i) => i.is_negative(),
            SimpleNumber::BigInteger(i) => *i < 0i32,
            SimpleNumber::Rational(r) => *r < 0i32,
            SimpleNumber::Real(r) => r.is_sign_negative(),
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            SimpleNumber::FixedInteger(i) => i.is_positive(),
            SimpleNumber::BigInteger(i) => *i >= 0i32,
            SimpleNumber::Rational(r) => *r >= 0i32,
            SimpleNumber::Real(r) => r.is_sign_positive(),
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            SimpleNumber::FixedInteger(_) | SimpleNumber::BigInteger(_) => true,
            SimpleNumber::Rational(r) => *r.denominator_ref() == 1u32,
            SimpleNumber::Real(r) => r.fract() == 0.0,
        }
    }

    pub fn is_rational(&self) -> bool {
        match self {
            SimpleNumber::FixedInteger(_)
            | SimpleNumber::BigInteger(_)
            | SimpleNumber::Rational(_) => true,
            SimpleNumber::Real(r) => !r.is_infinite() && !r.is_nan(),
        }
    }

    pub fn to_integer(&self, rounding_mode: RoundingMode) -> Self {
        match self {
            Self::FixedInteger(i) => Self::FixedInteger(*i),
            Self::BigInteger(i) => Self::BigInteger(i.clone()),
            Self::Rational(r) => Self::BigInteger(Integer::rounding_from(r, rounding_mode).0),
            Self::Real(r) if r.is_nan() || r.is_infinite() => Self::Real(*r),
            Self::Real(r) => Self::BigInteger(Integer::rounding_from(*r, rounding_mode).0),
        }
    }

    pub fn inexact(&self) -> Self {
        match self {
            Self::FixedInteger(i) => Self::Real(*i as f64),
            Self::BigInteger(i) => Self::Real(f64::rounding_from(i, RoundingMode::Nearest).0),
            Self::Rational(r) => Self::Real(f64::rounding_from(r, RoundingMode::Nearest).0),
            Self::Real(r) => Self::Real(*r),
        }
    }

    pub fn exact(&self) -> Self {
        match self {
            // We can do better than this, but it'll do for now
            Self::Real(r) if !r.is_nan() && !r.is_infinite() && r.fract() == 0.0 => {
                Self::BigInteger(Integer::rounding_from(*r, RoundingMode::Nearest).0)
            }
            Self::Real(r) => match Rational::try_from_float_simplest(*r) {
                Ok(r) => Self::Rational(r),
                _ => self.clone(),
            },
            num => num.clone(),
        }
    }

    pub fn abs(&self) -> Self {
        if *self < SimpleNumber::zero() {
            -self
        } else {
            self.clone()
        }
    }

    pub fn powi(&self, p: i32) -> Self {
        match self {
            SimpleNumber::FixedInteger(i) => i.checked_pow(p as u32).map_or_else(
                || SimpleNumber::BigInteger(Integer::from(*i).pow(p as u64)),
                SimpleNumber::FixedInteger,
            ),
            SimpleNumber::BigInteger(i) => SimpleNumber::BigInteger(i.pow(p as u64)),
            SimpleNumber::Rational(r) => SimpleNumber::Rational(r.pow(p as u64)),
            SimpleNumber::Real(r) => SimpleNumber::Real(r.powi(p)),
        }
    }

    pub fn div_euclid(&self, rhs: &Self) -> Self {
        if rhs.is_positive() {
            (self / rhs).to_integer(RoundingMode::Floor)
        } else {
            (self / rhs).to_integer(RoundingMode::Ceiling)
        }
    }

    pub fn pow(&self, p: &SimpleNumber) -> Self {
        if self.is_zero() {
            if p.is_zero() {
                if self.is_exact() && p.is_exact() {
                    return Self::from(1);
                } else {
                    return Self::from(1.0);
                }
            } else if p.is_positive() {
                if self.is_exact() && p.is_exact() {
                    return Self::from(0);
                } else {
                    return Self::from(0.0);
                }
            } else {
                return Self::infinity();
            }
        }
        match (self, p) {
            (SimpleNumber::FixedInteger(l), SimpleNumber::FixedInteger(r)) if *r >= 0 => {
                SimpleNumber::from(Integer::from(*l).pow(*r as u64))
            }
            (SimpleNumber::FixedInteger(l), SimpleNumber::FixedInteger(r)) if *r < 0 => {
                SimpleNumber::from(Rational::from_integers(
                    Integer::from(1),
                    Integer::from(*l).pow(-*r as u64),
                ))
            }
            (SimpleNumber::FixedInteger(l), SimpleNumber::BigInteger(i)) if *i >= 0 => {
                let exp: Option<u64> = i.try_into().ok();
                if let Some(exp) = exp {
                    SimpleNumber::from(Integer::from(*l).pow(exp))
                } else {
                    SimpleNumber::infinity()
                }
            }
            (SimpleNumber::FixedInteger(l), SimpleNumber::BigInteger(i)) if *i < 0 => {
                let exp: Option<u64> = (&(-i)).try_into().ok();
                if let Some(exp) = exp {
                    SimpleNumber::from(Rational::from_integers(
                        Integer::from(1),
                        Integer::from(*l).pow(exp),
                    ))
                } else {
                    SimpleNumber::zero()
                }
            }
            (SimpleNumber::BigInteger(l), SimpleNumber::FixedInteger(r)) if *r >= 0 => {
                SimpleNumber::from(l.pow(*r as u64))
            }
            (SimpleNumber::BigInteger(l), SimpleNumber::FixedInteger(r)) if *r < 0 => {
                SimpleNumber::from(Rational::from_integers(Integer::from(1), l.pow(-*r as u64)))
            }
            (SimpleNumber::BigInteger(l), SimpleNumber::BigInteger(i)) if *i >= 0 => {
                let exp: Option<u64> = i.try_into().ok();
                if let Some(exp) = exp {
                    SimpleNumber::from(l.pow(exp))
                } else {
                    SimpleNumber::infinity()
                }
            }
            (SimpleNumber::BigInteger(l), SimpleNumber::BigInteger(i)) if *i < 0 => {
                let exp: Option<u64> = (&(-i)).try_into().ok();
                if let Some(exp) = exp {
                    SimpleNumber::from(Rational::from_integers(Integer::from(1), l.pow(exp)))
                } else {
                    SimpleNumber::zero()
                }
            }
            (SimpleNumber::Rational(r), SimpleNumber::FixedInteger(exp)) if *exp >= 0 => {
                SimpleNumber::from(r.pow(*exp as u64))
            }
            (SimpleNumber::Rational(r), SimpleNumber::FixedInteger(exp)) if *exp < 0 => {
                SimpleNumber::from((Rational::from(1) / r).pow(-exp as u64))
            }
            (l, r) => {
                let l = l.to_f64();
                let r = r.to_f64();
                SimpleNumber::from(l.powf(r))
            }
        }
    }

    pub fn sqrt(&self) -> Self {
        match self {
            SimpleNumber::FixedInteger(i) => SimpleNumber::Real((*i as f64).sqrt()),
            SimpleNumber::BigInteger(i) => {
                SimpleNumber::Real(f64::rounding_from(i, RoundingMode::Nearest).0.sqrt())
            }
            SimpleNumber::Rational(r) => {
                SimpleNumber::Real(f64::rounding_from(r, RoundingMode::Nearest).0.sqrt())
            }
            SimpleNumber::Real(r) => SimpleNumber::Real(r.sqrt()),
        }
    }

    // Numbers are only equivalent if they're the same exactness
    // Two NaNs are also treated as equivalent
    pub fn eqv(&self, rhs: &Self) -> bool {
        (self.is_nan() && rhs.is_nan()) || (self.is_exact() == rhs.is_exact() && self == rhs)
    }

    pub fn to_f64(&self) -> f64 {
        match self {
            Self::FixedInteger(i) => *i as f64,
            Self::BigInteger(i) => f64::rounding_from(i, RoundingMode::Nearest).0,
            Self::Rational(r) => f64::rounding_from(r, RoundingMode::Nearest).0,
            Self::Real(r) => *r,
        }
    }

    pub fn exp(&self) -> Self {
        Self::Real(self.to_f64().exp())
    }

    pub fn ln(&self) -> Self {
        Self::Real(self.to_f64().ln())
    }

    pub fn sin(&self) -> Self {
        Self::Real(self.to_f64().sin())
    }

    pub fn cos(&self) -> Self {
        Self::Real(self.to_f64().cos())
    }

    pub fn tan(&self) -> Self {
        Self::Real(self.to_f64().tan())
    }

    pub fn sinh(&self) -> Self {
        Self::Real(self.to_f64().sinh())
    }

    pub fn cosh(&self) -> Self {
        Self::Real(self.to_f64().cosh())
    }

    pub fn tanh(&self) -> Self {
        Self::Real(self.to_f64().tanh())
    }

    pub fn asin(&self) -> Self {
        Self::Real(self.to_f64().asin())
    }

    pub fn acos(&self) -> Self {
        Self::Real(self.to_f64().acos())
    }

    pub fn atan(&self) -> Self {
        Self::Real(self.to_f64().atan())
    }

    pub fn atan2(&self, num2: &Self) -> Self {
        Self::Real(self.to_f64().atan2(num2.to_f64()))
    }
}

impl From<&Number> for Option<SimpleNumber> {
    fn from(value: &Number) -> Self {
        match value.0.as_ref() {
            NumberInner::Simple(simple) => Some(simple.clone()),
            NumberInner::Complex(complex) if complex.im.is_exact() && complex.im.is_zero() => {
                Some(complex.re.clone())
            }
            _ => None,
        }
    }
}

impl From<Number> for Option<SimpleNumber> {
    fn from(value: Number) -> Self {
        Self::from(&value)
    }
}

impl TryFrom<&Number> for SimpleNumber {
    type Error = Exception;

    fn try_from(value: &Number) -> Result<Self, Self::Error> {
        <Option<SimpleNumber> as From<&Number>>::from(value)
            .ok_or_else(|| Exception::type_error("real?", "complex?"))
    }
}

impl TryFrom<Number> for SimpleNumber {
    type Error = Exception;

    fn try_from(value: Number) -> Result<Self, Self::Error> {
        <Option<SimpleNumber> as From<Number>>::from(value)
            .ok_or_else(|| Exception::type_error("real?", "complex?"))
    }
}

macro_rules! impl_from_int {
    ($ty:ty) => {
        impl From<$ty> for SimpleNumber {
            fn from(i: $ty) -> Self {
                SimpleNumber::FixedInteger(i as i64)
            }
        }
    };
}

impl_from_int!(i8);
impl_from_int!(i16);
impl_from_int!(i32);
impl_from_int!(i64);
impl_from_int!(u8);
impl_from_int!(u16);
impl_from_int!(u32);
impl_from_int!(u64);

macro_rules! impl_from_large_int {
    ($ty:ty) => {
        impl From<$ty> for SimpleNumber {
            fn from(u: $ty) -> Self {
                match u.try_into() {
                    Ok(i) => SimpleNumber::FixedInteger(i),
                    Err(_) => SimpleNumber::BigInteger(Integer::from(u)),
                }
            }
        }
    };
}

impl_from_large_int!(i128);
impl_from_large_int!(isize);
impl_from_large_int!(u128);
impl_from_large_int!(usize);

impl From<Integer> for SimpleNumber {
    fn from(i: Integer) -> Self {
        Self::BigInteger(i)
    }
}

impl From<Rational> for SimpleNumber {
    fn from(r: Rational) -> Self {
        Self::Rational(r)
    }
}

impl From<f64> for SimpleNumber {
    fn from(r: f64) -> Self {
        Self::Real(r)
    }
}

impl From<ComplexNumber> for Number {
    fn from(complex: ComplexNumber) -> Self {
        Self(Arc::new(NumberInner::Complex(complex)))
    }
}

macro_rules! number_try_into_impl_integer {
    ($ty:tt) => {
        impl TryFrom<&SimpleNumber> for $ty {
            type Error = Exception;

            fn try_from(num: &SimpleNumber) -> Result<$ty, Self::Error> {
                match num {
                    SimpleNumber::FixedInteger(i) => {
                        // Since FixedInteger is i64, we can just check for
                        // greater than size_of::<u32>() to know if we should just
                        // cast the value to the type or check for the right size.
                        if size_of::<$ty>() > size_of::<u32>() {
                            Ok(*i as $ty)
                        } else if *i <= $ty::MAX as i64 && *i >= $ty::MIN as i64 {
                            Ok(*i as $ty)
                        } else {
                            Err(Exception::not_representable(
                                &format!("{i}"),
                                stringify!($ty),
                            ))
                        }
                    }
                    SimpleNumber::BigInteger(bigint) => $ty::convertible_from(bigint)
                        .then(|| $ty::wrapping_from(bigint))
                        .ok_or_else(|| {
                            Exception::not_representable(&format!("{bigint}"), stringify!($ty))
                        }),
                    SimpleNumber::Rational(_) => {
                        Err(Exception::conversion_error(stringify!($ty), "rational"))
                    }
                    SimpleNumber::Real(_) => {
                        Err(Exception::conversion_error(stringify!($ty), "real"))
                    }
                }
            }
        }

        impl TryFrom<SimpleNumber> for $ty {
            type Error = Exception;

            fn try_from(num: SimpleNumber) -> Result<$ty, Self::Error> {
                $ty::try_from(&num)
            }
        }

        impl From<&SimpleNumber> for Option<$ty> {
            fn from(num: &SimpleNumber) -> Option<$ty> {
                match num {
                    SimpleNumber::FixedInteger(i) => {
                        if size_of::<$ty>() > size_of::<u32>() {
                            Some(*i as $ty)
                        } else if *i <= $ty::MAX as i64 && *i >= $ty::MIN as i64 {
                            Some(*i as $ty)
                        } else {
                            None
                        }
                    }
                    SimpleNumber::BigInteger(bigint) => {
                        $ty::convertible_from(bigint).then(|| $ty::wrapping_from(bigint))
                    }
                    _ => None,
                }
            }
        }

        impl From<SimpleNumber> for Option<$ty> {
            fn from(num: SimpleNumber) -> Option<$ty> {
                Self::from(&num)
            }
        }
    };
}

number_try_into_impl_integer!(u8);
number_try_into_impl_integer!(u16);
number_try_into_impl_integer!(u32);
number_try_into_impl_integer!(u64);
number_try_into_impl_integer!(u128);
number_try_into_impl_integer!(usize);
number_try_into_impl_integer!(i8);
number_try_into_impl_integer!(i16);
number_try_into_impl_integer!(i32);
number_try_into_impl_integer!(i64);
number_try_into_impl_integer!(i128);
number_try_into_impl_integer!(isize);

impl TryFrom<&SimpleNumber> for Integer {
    type Error = Exception;

    fn try_from(num: &SimpleNumber) -> Result<Integer, Self::Error> {
        match num {
            SimpleNumber::FixedInteger(i) => Ok(Integer::from(*i)),
            SimpleNumber::BigInteger(i) => Ok(i.clone()),
            SimpleNumber::Rational(r) if *r.denominator_ref() == 1u32 => {
                Ok(Integer::from(r.to_numerator()))
            }
            SimpleNumber::Rational(_) => Err(Exception::conversion_error("integer", "rational")),
            SimpleNumber::Real(r) if !r.is_nan() && !r.is_infinite() && r.fract() == 0.0 => {
                Ok(Integer::rounding_from(*r, RoundingMode::Nearest).0)
            }
            SimpleNumber::Real(_) => Err(Exception::conversion_error("integer", "real")),
        }
    }
}

impl TryFrom<SimpleNumber> for Integer {
    type Error = Exception;

    fn try_from(num: SimpleNumber) -> Result<Integer, Self::Error> {
        Integer::try_from(&num)
    }
}

impl From<&SimpleNumber> for Option<Integer> {
    fn from(num: &SimpleNumber) -> Option<Integer> {
        match num {
            SimpleNumber::FixedInteger(i) => Some(Integer::from(*i)),
            SimpleNumber::BigInteger(i) => Some(i.clone()),
            SimpleNumber::Rational(r) if *r.denominator_ref() == 1u32 => {
                Some(Integer::from(r.to_numerator()))
            }
            SimpleNumber::Rational(_) => None,
            SimpleNumber::Real(r) if !r.is_nan() && !r.is_infinite() && r.fract() == 0.0 => {
                Some(Integer::rounding_from(*r, RoundingMode::Nearest).0)
            }
            SimpleNumber::Real(_) => None,
        }
    }
}

impl From<SimpleNumber> for Option<Integer> {
    fn from(num: SimpleNumber) -> Option<Integer> {
        Self::from(&num)
    }
}

impl TryFrom<&SimpleNumber> for f64 {
    type Error = Exception;

    fn try_from(num: &SimpleNumber) -> Result<f64, Self::Error> {
        match num {
            SimpleNumber::FixedInteger(i) => Ok(*i as f64),
            SimpleNumber::Real(r) => Ok(*r),
            SimpleNumber::Rational(r) => {
                if let Some((float, _, _)) =
                    r.sci_mantissa_and_exponent_round_ref(RoundingMode::Nearest)
                {
                    Ok(float)
                } else {
                    Err(Exception::not_representable(&format!("{r}"), "f64"))
                }
            }
            SimpleNumber::BigInteger(_) => Err(Exception::conversion_error("f64", "integer")),
        }
    }
}

impl TryFrom<SimpleNumber> for f64 {
    type Error = Exception;

    fn try_from(num: SimpleNumber) -> Result<f64, Self::Error> {
        f64::try_from(&num)
    }
}

impl From<&SimpleNumber> for Option<f64> {
    fn from(num: &SimpleNumber) -> Option<f64> {
        match num {
            SimpleNumber::FixedInteger(i) => Some(*i as f64),
            SimpleNumber::Real(r) => Some(*r),
            SimpleNumber::Rational(r) => r
                .sci_mantissa_and_exponent_round_ref(RoundingMode::Nearest)
                .map(|(float, _, _)| float),
            SimpleNumber::BigInteger(_) => None,
        }
    }
}

impl From<SimpleNumber> for Option<f64> {
    fn from(num: SimpleNumber) -> Option<f64> {
        Self::from(&num)
    }
}

impl TryFrom<&Number> for f64 {
    type Error = Exception;

    fn try_from(num: &Number) -> Result<f64, Self::Error> {
        match num.0.as_ref() {
            NumberInner::Simple(simple) => f64::try_from(simple),
            NumberInner::Complex(complex) if !complex.im.is_exact() && complex.im.is_zero() => {
                f64::try_from(&complex.re)
            }
            _ => Err(Exception::conversion_error("f64", "complex")),
        }
    }
}

impl TryFrom<Number> for f64 {
    type Error = Exception;

    fn try_from(num: Number) -> Result<f64, Self::Error> {
        f64::try_from(&num)
    }
}

impl From<&Number> for Option<f64> {
    fn from(num: &Number) -> Option<f64> {
        match num.0.as_ref() {
            NumberInner::Simple(simple) => Self::from(simple),
            NumberInner::Complex(complex) if !complex.im.is_exact() && complex.im.is_zero() => {
                Self::from(&complex.re)
            }
            _ => None,
        }
    }
}

impl From<Number> for Option<f64> {
    fn from(num: Number) -> Option<f64> {
        Self::from(&num)
    }
}

impl fmt::Display for SimpleNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::FixedInteger(i) => write!(f, "{i}"),
            Self::BigInteger(i) => write!(f, "{i}"),
            Self::Rational(r) => write!(f, "{r}"),
            Self::Real(r) if r.is_nan() => write!(f, "+nan.0"),
            Self::Real(r) if r.is_infinite() && r.is_sign_positive() => write!(f, "+inf.0"),
            Self::Real(r) if r.is_infinite() && r.is_sign_negative() => write!(f, "-inf.0"),
            Self::Real(r) => write!(f, "{r:?}"),
        }
    }
}

impl fmt::Debug for SimpleNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::FixedInteger(i) => write!(f, "{i}"),
            Self::BigInteger(i) => write!(f, "{i}"),
            Self::Rational(r) => write!(f, "{r}"),
            Self::Real(r) if r.is_nan() => write!(f, "+nan.0"),
            Self::Real(r) if r.is_infinite() && r.is_sign_positive() => write!(f, "+inf.0"),
            Self::Real(r) if r.is_infinite() && r.is_sign_negative() => write!(f, "-inf.0"),
            Self::Real(r) => write!(f, "{r:?}"),
        }
    }
}

/// Hash implementation for SimpleNumber is the eqv-hash procedure from r6rs.
impl Hash for SimpleNumber {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            SimpleNumber::FixedInteger(i) => {
                0u8.hash(state);
                i.hash(state);
            }
            SimpleNumber::BigInteger(i) => {
                if i64::convertible_from(i) {
                    0u8.hash(state);
                    i64::wrapping_from(i).hash(state)
                } else {
                    1u8.hash(state);
                    i.hash(state)
                }
            }
            SimpleNumber::Rational(r) => {
                if *r.denominator_ref() == 1u32 {
                    let i = if *r < 0 {
                        -Integer::from(r.numerator_ref())
                    } else {
                        Integer::from(r.numerator_ref())
                    };
                    if i64::convertible_from(&i) {
                        0u8.hash(state);
                        i64::wrapping_from(&i).hash(state)
                    } else {
                        1u8.hash(state);
                        i.hash(state)
                    }
                } else {
                    2u8.hash(state);
                    r.hash(state)
                }
            }
            SimpleNumber::Real(r) => {
                3u8.hash(state);
                if r.is_nan() {
                    // Use the same bit pattern for all NaNs to mirror eqv
                    f64::NAN.to_bits().hash(state);
                } else {
                    r.to_bits().hash(state);
                }
            }
        }
    }
}

impl PartialEq for SimpleNumber {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::FixedInteger(l), Self::FixedInteger(r)) => l == r,
            (Self::BigInteger(l), Self::BigInteger(r)) => l == r,
            (Self::Rational(l), Self::Rational(r)) => l == r,
            (Self::Real(l), Self::Real(r)) => l == r,

            (Self::BigInteger(big_int), Self::FixedInteger(fixed_int))
            | (Self::FixedInteger(fixed_int), Self::BigInteger(big_int)) => fixed_int == big_int,

            (Self::Rational(rational), Self::FixedInteger(fixed_int))
            | (Self::FixedInteger(fixed_int), Self::Rational(rational)) => fixed_int == rational,

            (Self::BigInteger(big_int), Self::Rational(rational))
            | (Self::Rational(rational), Self::BigInteger(big_int)) => big_int == rational,

            (Self::BigInteger(big_int), Self::Real(float))
            | (Self::Real(float), Self::BigInteger(big_int)) => float == big_int,

            (Self::Rational(rational), Self::Real(float))
            | (Self::Real(float), Self::Rational(rational)) => float == rational,

            (Self::FixedInteger(_), Self::Real(float))
            | (Self::Real(float), Self::FixedInteger(_))
                if float.fract() != 0.0 =>
            {
                false
            }
            (Self::FixedInteger(fixed_int), Self::Real(float))
            | (Self::Real(float), Self::FixedInteger(fixed_int)) => {
                float.to_i64() == Some(*fixed_int)
            }
        }
    }
}

impl PartialOrd for SimpleNumber {
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
            // I genuinely do not know how to properly implement these without
            // the use of big nums. This will work for now.
            (Self::FixedInteger(l), Self::Real(r)) => Integer::from(*l).partial_cmp(r),
            (Self::Real(l), Self::FixedInteger(r)) => l.partial_cmp(&Integer::from(*r)),
        }
    }
}

impl Neg for SimpleNumber {
    type Output = SimpleNumber;

    fn neg(self) -> Self {
        match self {
            Self::FixedInteger(i) => Self::FixedInteger(-i),
            Self::BigInteger(i) => Self::BigInteger(-i),
            Self::Rational(r) => Self::Rational(-r),
            Self::Real(r) => Self::Real(-r),
        }
    }
}

impl Neg for &'_ SimpleNumber {
    type Output = SimpleNumber;

    fn neg(self) -> SimpleNumber {
        match self {
            SimpleNumber::FixedInteger(i) => i.checked_neg().map_or_else(
                || SimpleNumber::BigInteger(-Integer::from(*i)),
                SimpleNumber::FixedInteger,
            ),
            SimpleNumber::BigInteger(i) => SimpleNumber::BigInteger(-i),
            SimpleNumber::Rational(r) => SimpleNumber::Rational(-r),
            SimpleNumber::Real(r) => SimpleNumber::Real(-r),
        }
    }
}

macro_rules! impl_op_for_simple_numbers {
    ($trait:ident, $op:ident, $checked:ident) => {
        impl $trait for &SimpleNumber {
            type Output = SimpleNumber;

            fn $op(self, rhs: &SimpleNumber) -> SimpleNumber {
                match (self, rhs) {
                    (SimpleNumber::FixedInteger(l), SimpleNumber::FixedInteger(r)) => l
                        .$checked(*r)
                        .map(SimpleNumber::FixedInteger)
                        .unwrap_or_else(|| {
                            SimpleNumber::BigInteger(Integer::from(*l).$op(Integer::from(*r)))
                        }),
                    (SimpleNumber::BigInteger(l), SimpleNumber::BigInteger(r)) => {
                        SimpleNumber::BigInteger(l.$op(r))
                    }
                    (SimpleNumber::Rational(l), SimpleNumber::Rational(r)) => {
                        SimpleNumber::Rational(l.$op(r))
                    }
                    (SimpleNumber::Real(l), SimpleNumber::Real(r)) => SimpleNumber::Real(l.$op(r)),
                    (SimpleNumber::BigInteger(l), SimpleNumber::FixedInteger(r)) => {
                        i64::convertible_from(l)
                            .then(|| i64::wrapping_from(l).$checked(*r))
                            .flatten()
                            .map(SimpleNumber::FixedInteger)
                            .unwrap_or_else(|| SimpleNumber::BigInteger(l.$op(Integer::from(*r))))
                    }
                    (SimpleNumber::FixedInteger(l), SimpleNumber::BigInteger(r)) => {
                        i64::convertible_from(r)
                            .then(|| l.$checked(i64::wrapping_from(r)))
                            .flatten()
                            .map(SimpleNumber::FixedInteger)
                            .unwrap_or_else(|| SimpleNumber::BigInteger(Integer::from(*l).$op(r)))
                    }
                    (SimpleNumber::Rational(l), SimpleNumber::FixedInteger(r)) => {
                        SimpleNumber::Rational(l.$op(Rational::from(*r)))
                    }
                    (SimpleNumber::FixedInteger(l), SimpleNumber::Rational(r)) => {
                        SimpleNumber::Rational(Rational::from(*l).$op(r))
                    }

                    (SimpleNumber::Rational(l), SimpleNumber::BigInteger(r)) => {
                        SimpleNumber::Rational(l.$op(Rational::from(r)))
                    }
                    (SimpleNumber::BigInteger(l), SimpleNumber::Rational(r)) => {
                        SimpleNumber::Rational(Rational::from(l).$op(r))
                    }

                    (SimpleNumber::BigInteger(l), SimpleNumber::Real(r)) => {
                        SimpleNumber::Real(f64::rounding_from(l, RoundingMode::Nearest).0.$op(*r))
                    }
                    (SimpleNumber::Real(l), SimpleNumber::BigInteger(r)) => {
                        SimpleNumber::Real(l.$op(f64::rounding_from(r, RoundingMode::Nearest).0))
                    }

                    (SimpleNumber::Rational(l), SimpleNumber::Real(r)) => {
                        SimpleNumber::Real(f64::rounding_from(l, RoundingMode::Nearest).0.$op(r))
                    }
                    (SimpleNumber::Real(l), SimpleNumber::Rational(r)) => {
                        SimpleNumber::Real(l.$op(f64::rounding_from(r, RoundingMode::Nearest).0))
                    }

                    (SimpleNumber::FixedInteger(l), SimpleNumber::Real(r)) => {
                        SimpleNumber::Real((*l as f64).$op(r))
                    }

                    (SimpleNumber::Real(l), SimpleNumber::FixedInteger(r)) => {
                        SimpleNumber::Real(l.$op(*r as f64))
                    }
                }
            }
        }

        impl $trait for SimpleNumber {
            type Output = SimpleNumber;

            fn $op(self, rhs: SimpleNumber) -> SimpleNumber {
                (&self).$op(&rhs)
            }
        }
    };
}

impl_op_for_simple_numbers!(Add, add, checked_add);
impl_op_for_simple_numbers!(Sub, sub, checked_sub);
impl_op_for_simple_numbers!(Mul, mul, checked_mul);
// impl_op_for_simple_and_complex_numbers!(Div, div, checked_div);

impl Div for &SimpleNumber {
    type Output = SimpleNumber;

    fn div(self, rhs: &SimpleNumber) -> SimpleNumber {
        match (self, rhs) {
            (SimpleNumber::FixedInteger(l), SimpleNumber::FixedInteger(r)) => {
                SimpleNumber::Rational(Rational::from(*l) / Rational::from(*r))
            }
            (SimpleNumber::BigInteger(l), SimpleNumber::BigInteger(r)) => {
                SimpleNumber::Rational(Rational::from_integers_ref(l, r))
            }
            (SimpleNumber::Rational(l), SimpleNumber::Rational(r)) => SimpleNumber::Rational(l / r),
            (SimpleNumber::Real(l), SimpleNumber::Real(r)) => SimpleNumber::Real(l / r),

            (SimpleNumber::BigInteger(l), SimpleNumber::FixedInteger(r)) => {
                SimpleNumber::Rational(Rational::from(l) / Rational::from(*r))
            }
            (SimpleNumber::FixedInteger(l), SimpleNumber::BigInteger(r)) => {
                SimpleNumber::Rational(Rational::from(*l) / Rational::from(r))
            }

            (SimpleNumber::Rational(l), SimpleNumber::FixedInteger(r)) => {
                SimpleNumber::Rational(l / Rational::from(*r))
            }
            (SimpleNumber::FixedInteger(l), SimpleNumber::Rational(r)) => {
                SimpleNumber::Rational(Rational::from(*l) / r)
            }

            (SimpleNumber::BigInteger(l), SimpleNumber::Rational(r)) => {
                SimpleNumber::Rational(Rational::from(l) / r)
            }
            (SimpleNumber::Rational(l), SimpleNumber::BigInteger(r)) => {
                SimpleNumber::Rational(l / Rational::from(r))
            }

            (SimpleNumber::BigInteger(l), SimpleNumber::Real(r)) => {
                SimpleNumber::Real(f64::rounding_from(l, RoundingMode::Nearest).0 / *r)
            }
            (SimpleNumber::Real(l), SimpleNumber::BigInteger(r)) => {
                SimpleNumber::Real(*l / f64::rounding_from(r, RoundingMode::Nearest).0)
            }

            (SimpleNumber::Rational(l), SimpleNumber::Real(r)) => {
                SimpleNumber::Real(f64::rounding_from(l, RoundingMode::Nearest).0 / *r)
            }
            (SimpleNumber::Real(l), SimpleNumber::Rational(r)) => {
                SimpleNumber::Real(*l / f64::rounding_from(r, RoundingMode::Nearest).0)
            }

            (SimpleNumber::FixedInteger(l), SimpleNumber::Real(r)) => {
                SimpleNumber::Real(*l as f64 / r)
            }
            (SimpleNumber::Real(l), SimpleNumber::FixedInteger(r)) => {
                SimpleNumber::Real(l / *r as f64)
            }
        }
    }
}

impl Div for SimpleNumber {
    type Output = SimpleNumber;

    fn div(self, rhs: SimpleNumber) -> SimpleNumber {
        &self / &rhs
    }
}

impl Rem for &'_ SimpleNumber {
    type Output = SimpleNumber;

    fn rem(self, rhs: &SimpleNumber) -> SimpleNumber {
        match (self, rhs) {
            (SimpleNumber::FixedInteger(l), SimpleNumber::FixedInteger(r)) => l
                .checked_rem(*r)
                .map(SimpleNumber::FixedInteger)
                .unwrap_or_else(|| {
                    SimpleNumber::BigInteger(Integer::from(*l) % (Integer::from(*r)))
                }),
            (SimpleNumber::BigInteger(l), SimpleNumber::BigInteger(r)) => {
                SimpleNumber::BigInteger(l % r)
            }
            (SimpleNumber::Rational(l), SimpleNumber::Rational(r)) => {
                SimpleNumber::Rational(mod_rationals(l, r))
            }
            (SimpleNumber::Real(l), SimpleNumber::Real(r)) => SimpleNumber::Real(l % r),
            (SimpleNumber::BigInteger(l), SimpleNumber::FixedInteger(r)) => {
                i64::convertible_from(l)
                    .then(|| i64::wrapping_from(l).checked_rem(*r))
                    .flatten()
                    .map(SimpleNumber::FixedInteger)
                    .unwrap_or_else(|| SimpleNumber::BigInteger(l % Integer::from(*r)))
            }
            (SimpleNumber::FixedInteger(l), SimpleNumber::BigInteger(r)) => {
                i64::convertible_from(r)
                    .then(|| l.checked_rem(i64::wrapping_from(r)))
                    .flatten()
                    .map(SimpleNumber::FixedInteger)
                    .unwrap_or_else(|| SimpleNumber::BigInteger(Integer::from(*l) % r))
            }
            (SimpleNumber::Rational(l), SimpleNumber::FixedInteger(r)) => {
                SimpleNumber::Rational(mod_rationals(l, &Rational::from(*r)))
            }
            (SimpleNumber::FixedInteger(l), SimpleNumber::Rational(r)) => {
                SimpleNumber::Rational(mod_rationals(&Rational::from(*l), r))
            }

            (SimpleNumber::Rational(l), SimpleNumber::BigInteger(r)) => {
                SimpleNumber::Rational(mod_rationals(l, &Rational::from(r)))
            }
            (SimpleNumber::BigInteger(l), SimpleNumber::Rational(r)) => {
                SimpleNumber::Rational(mod_rationals(&Rational::from(l), r))
            }

            (SimpleNumber::BigInteger(l), SimpleNumber::Real(r)) => {
                SimpleNumber::Real(f64::rounding_from(l, RoundingMode::Nearest).0 % *r)
            }
            (SimpleNumber::Real(l), SimpleNumber::BigInteger(r)) => {
                SimpleNumber::Real(l % f64::rounding_from(r, RoundingMode::Nearest).0)
            }

            (SimpleNumber::Rational(l), SimpleNumber::Real(r)) => {
                SimpleNumber::Real(f64::rounding_from(l, RoundingMode::Nearest).0 % r)
            }
            (SimpleNumber::Real(l), SimpleNumber::Rational(r)) => {
                SimpleNumber::Real(l % f64::rounding_from(r, RoundingMode::Nearest).0)
            }

            (SimpleNumber::FixedInteger(l), SimpleNumber::Real(r)) => {
                SimpleNumber::Real((*l as f64) % r)
            }

            (SimpleNumber::Real(l), SimpleNumber::FixedInteger(r)) => {
                SimpleNumber::Real(l % *r as f64)
            }
        }
    }
}

fn mod_rationals(lhs: &Rational, rhs: &Rational) -> Rational {
    (lhs - rhs) * Rational::from(Integer::rounding_from(lhs / rhs, RoundingMode::Floor).0)
}

impl Rem for SimpleNumber {
    type Output = SimpleNumber;

    fn rem(self, rhs: SimpleNumber) -> SimpleNumber {
        (&self) % &rhs
    }
}

/// Scheme complex numeric type.
#[derive(Clone)]
pub struct ComplexNumber {
    re: SimpleNumber,
    im: SimpleNumber,
}

impl ComplexNumber {
    pub fn new(re: SimpleNumber, im: SimpleNumber) -> Self {
        Self { re, im }
    }

    pub fn is_zero(&self) -> bool {
        self.re.is_zero() && self.im.is_zero()
    }

    pub fn one() -> Self {
        Self {
            re: SimpleNumber::one(),
            im: SimpleNumber::zero(),
        }
    }

    pub fn i() -> Self {
        Self {
            re: SimpleNumber::zero(),
            im: SimpleNumber::one(),
        }
    }

    pub fn inexact(&self) -> Self {
        Self {
            re: self.re.inexact(),
            im: self.im.inexact(),
        }
    }

    pub fn exact(&self) -> Self {
        Self {
            re: self.re.exact(),
            im: self.im.exact(),
        }
    }

    pub fn to_string(&self, radix: u32, precision: Option<usize>) -> Option<String> {
        let mut output = self.re.to_string(radix, precision)?;
        if self.im.is_exact() && self.im.is_zero() {
            return Some(output);
        }
        if self.im.is_positive() && !self.im.is_nan() && !self.im.is_infinite() {
            output.push('+');
        }
        output.push_str(&self.im.to_string(radix, precision)?);
        output.push('i');
        Some(output)
    }

    pub fn from_polar(r: SimpleNumber, theta: SimpleNumber) -> Self {
        Self::new(&r * &theta.cos(), &r * &theta.sin())
    }

    pub fn to_polar(&self) -> (SimpleNumber, SimpleNumber) {
        (self.magnitude(), self.arg())
    }

    pub fn magnitude(&self) -> SimpleNumber {
        self.magnitude2().sqrt()
    }

    pub fn magnitude2(&self) -> SimpleNumber {
        self.re.powi(2) + self.im.powi(2)
    }

    pub fn sqrt(&self) -> Self {
        if self.im.is_zero() {
            if self.re.is_positive() {
                Self::new(self.re.sqrt(), self.im.clone())
            } else if self.im.is_positive() {
                Self::new(SimpleNumber::zero(), (-&self.re).sqrt())
            } else {
                Self::new(SimpleNumber::zero(), -(-&self.re).sqrt())
            }
        } else if self.re.is_zero() {
            let x = (self.im.abs() / SimpleNumber::from(2)).sqrt();
            if self.im.is_positive() {
                Self::new(x.clone(), x)
            } else {
                Self::new(x.clone(), -x)
            }
        } else {
            let (r, theta) = self.to_polar();
            Self::from_polar(r.sqrt(), theta / SimpleNumber::from(2))
        }
    }

    pub fn arg(&self) -> SimpleNumber {
        self.im.atan2(&self.re)
    }

    pub fn exp(&self) -> Self {
        Self::from_polar(self.re.exp(), self.im.clone())
    }

    pub fn ln(&self) -> Self {
        let (r, theta) = self.to_polar();
        Self::new(r.ln(), theta)
    }

    pub fn powc(&self, exp: &Self) -> Self {
        if exp.is_zero() {
            return Self::one();
        }
        (exp * &self.ln()).exp()
    }

    pub fn sin(&self) -> Self {
        Self::new(
            self.re.sin() * self.im.cosh(),
            self.re.cos() * self.im.sinh(),
        )
    }

    pub fn cos(&self) -> Self {
        Self::new(
            self.re.cos() * self.im.cosh(),
            -self.re.sin() * self.im.sinh(),
        )
    }

    pub fn tan(&self) -> Self {
        let scale = (&self.re + &self.re).cos() + (&self.im + &self.im).cosh();
        let re = &(&self.re + &self.re).sin() / &scale;
        let im = &(&self.im + &self.im).sinh() / &scale;
        Self::new(re, im)
    }

    pub fn asin(&self) -> Self {
        -Self::i() * ((Self::one() - self * self).sqrt() + &Self::i() * self).ln()
    }

    pub fn acos(&self) -> Self {
        -Self::i() * (&(Self::i() * (Self::one() - self * self).sqrt()) + self).ln()
    }

    pub fn atan(&self) -> Self {
        if self.re.is_zero() && self.im.is_one() {
            Self::new(SimpleNumber::zero(), SimpleNumber::infinity())
        } else if self.re.is_zero() && self.im.is_neg_one() {
            Self::new(SimpleNumber::zero(), SimpleNumber::neg_infinity())
        } else {
            ((&(Self::one() + Self::i()) * self).ln() - (Self::one() - &Self::i() * self).ln())
                / ((Self::one() + Self::one()) * Self::i())
        }
    }
}

// Any Number can be converted into a ComplexNumber

impl From<&Number> for Option<ComplexNumber> {
    fn from(value: &Number) -> Self {
        match value.0.as_ref() {
            NumberInner::Simple(simple) => {
                Some(ComplexNumber::new(simple.clone(), SimpleNumber::zero()))
            }
            NumberInner::Complex(complex) => Some(complex.clone()),
        }
    }
}

impl From<Number> for Option<ComplexNumber> {
    fn from(value: Number) -> Self {
        (&value).into()
    }
}

impl TryFrom<&Number> for ComplexNumber {
    type Error = Exception;

    fn try_from(value: &Number) -> Result<Self, Self::Error> {
        match value.0.as_ref() {
            NumberInner::Simple(simple) => {
                Ok(ComplexNumber::new(simple.clone(), SimpleNumber::zero()))
            }
            NumberInner::Complex(complex) => Ok(complex.clone()),
        }
    }
}

impl TryFrom<Number> for ComplexNumber {
    type Error = Exception;

    fn try_from(value: Number) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl From<SimpleNumber> for ComplexNumber {
    fn from(value: SimpleNumber) -> Self {
        Self {
            re: value,
            im: SimpleNumber::FixedInteger(0),
        }
    }
}

impl PartialEq for ComplexNumber {
    fn eq(&self, rhs: &ComplexNumber) -> bool {
        self.re == rhs.re && self.im == rhs.im
    }
}

impl Neg for ComplexNumber {
    type Output = ComplexNumber;

    fn neg(self) -> ComplexNumber {
        Self {
            re: -self.re,
            im: -self.im,
        }
    }
}

impl Neg for &ComplexNumber {
    type Output = ComplexNumber;

    fn neg(self) -> ComplexNumber {
        ComplexNumber {
            re: -(&self.re),
            im: -(&self.im),
        }
    }
}

impl Add<&ComplexNumber> for &ComplexNumber {
    type Output = ComplexNumber;

    fn add(self, rhs: &ComplexNumber) -> ComplexNumber {
        ComplexNumber {
            re: (&self.re).add(&rhs.re),
            im: (&self.im).add(&rhs.im),
        }
    }
}

impl Add<ComplexNumber> for ComplexNumber {
    type Output = ComplexNumber;

    fn add(self, rhs: ComplexNumber) -> ComplexNumber {
        ComplexNumber {
            re: self.re.add(rhs.re),
            im: self.im.add(rhs.im),
        }
    }
}

impl Sub<&ComplexNumber> for &ComplexNumber {
    type Output = ComplexNumber;

    fn sub(self, rhs: &ComplexNumber) -> ComplexNumber {
        ComplexNumber {
            re: (&self.re).sub(&rhs.re),
            im: (&self.im).sub(&rhs.im),
        }
    }
}

impl Sub<ComplexNumber> for ComplexNumber {
    type Output = ComplexNumber;

    fn sub(self, rhs: ComplexNumber) -> ComplexNumber {
        ComplexNumber {
            re: self.re.sub(rhs.re),
            im: self.im.sub(rhs.im),
        }
    }
}

impl Mul<&ComplexNumber> for &ComplexNumber {
    type Output = ComplexNumber;

    fn mul(self, rhs: &ComplexNumber) -> ComplexNumber {
        let re = &self.re * &rhs.re - &self.im * &rhs.im;
        let im = &self.re * &rhs.im + &self.im * &rhs.re;
        ComplexNumber::new(re, im)
    }
}

impl Mul<ComplexNumber> for ComplexNumber {
    type Output = ComplexNumber;

    fn mul(self, rhs: ComplexNumber) -> ComplexNumber {
        (&self).mul(&rhs)
    }
}

impl Div for &ComplexNumber {
    type Output = ComplexNumber;

    fn div(self, rhs: &ComplexNumber) -> ComplexNumber {
        let norm_sqr = rhs.re.powi(2) + rhs.im.powi(2);
        let re = self.re.clone() * rhs.re.clone() + self.im.clone() * rhs.im.clone();
        let im = self.im.clone() * rhs.re.clone() - self.re.clone() * rhs.im.clone();
        ComplexNumber {
            re: re / norm_sqr.clone(),
            im: im / norm_sqr,
        }
    }
}

impl Div for ComplexNumber {
    type Output = ComplexNumber;

    fn div(self, rhs: ComplexNumber) -> ComplexNumber {
        &self / &rhs
    }
}

impl fmt::Display for ComplexNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.re)?;
        if self.im.is_positive() && !self.im.is_nan() && !self.im.is_infinite() {
            write!(f, "+")?;
        }
        write!(f, "{}i", self.im)
    }
}

impl fmt::Debug for ComplexNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.re)?;
        if self.im.is_positive() && !self.im.is_nan() && !self.im.is_infinite() {
            write!(f, "+")?;
        }
        write!(f, "{:?}i", self.im)
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => write!(f, "{simple}"),
            NumberInner::Complex(complex) => write!(f, "{complex}"),
        }
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => write!(f, "{simple:?}"),
            NumberInner::Complex(complex) => write!(f, "{complex:?}"),
        }
    }
}

impl Hash for Number {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self.0.as_ref()).hash(state);
        match self.0.as_ref() {
            NumberInner::Simple(simple) => simple.hash(state),
            NumberInner::Complex(complex) => {
                complex.re.hash(state);
                complex.im.hash(state);
            }
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, rhs: &Self) -> bool {
        match (self.0.as_ref(), rhs.0.as_ref()) {
            (NumberInner::Simple(lhs), NumberInner::Simple(rhs)) => lhs.eq(rhs),
            (NumberInner::Complex(lhs), NumberInner::Simple(rhs)) if lhs.im.is_zero() => {
                lhs.re.eq(rhs)
            }
            (NumberInner::Simple(lhs), NumberInner::Complex(rhs)) if rhs.im.is_zero() => {
                lhs.eq(&rhs.re)
            }
            (NumberInner::Complex(lhs), NumberInner::Complex(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match (self.0.as_ref(), rhs.0.as_ref()) {
            (NumberInner::Simple(lhs), NumberInner::Simple(rhs)) => lhs.partial_cmp(rhs),
            (NumberInner::Complex(lhs), NumberInner::Simple(rhs)) if lhs.im.is_zero() => {
                lhs.re.partial_cmp(rhs)
            }
            (NumberInner::Simple(lhs), NumberInner::Complex(rhs)) if rhs.im.is_zero() => {
                lhs.partial_cmp(&rhs.re)
            }
            (NumberInner::Complex(lhs), NumberInner::Complex(rhs))
                if lhs.im.is_zero() && rhs.im.is_zero() =>
            {
                lhs.re.partial_cmp(&rhs.re)
            }
            _ => None,
        }
    }
}

impl Neg for Number {
    type Output = Number;

    fn neg(self) -> Self {
        match self.0.as_ref() {
            NumberInner::Simple(this) => Number::from(-this),
            NumberInner::Complex(this) => Number::from(-this),
        }
    }
}

impl Neg for &'_ Number {
    type Output = Number;

    fn neg(self) -> Number {
        match self.0.as_ref() {
            NumberInner::Simple(this) => Number::from(-this),
            NumberInner::Complex(this) => Number::from(-this),
        }
    }
}

macro_rules! impl_op_for_number {
    ($trait:ident, $op:ident) => {
        impl $trait for &Number {
            type Output = Number;

            fn $op(self, rhs: &Number) -> Self::Output {
                match (self.0.as_ref(), rhs.0.as_ref()) {
                    (NumberInner::Simple(lhs), NumberInner::Simple(rhs)) => {
                        Number::from(lhs.$op(rhs))
                    }
                    (NumberInner::Simple(lhs), NumberInner::Complex(rhs)) => {
                        Number::from((&ComplexNumber::from(lhs.clone())).$op(rhs))
                    }
                    (NumberInner::Complex(lhs), NumberInner::Simple(rhs)) => {
                        Number::from(lhs.$op(&ComplexNumber::from(rhs.clone())))
                    }
                    (NumberInner::Complex(lhs), NumberInner::Complex(rhs)) => {
                        Number::from(lhs.$op(rhs))
                    }
                }
            }
        }

        impl $trait for Number {
            type Output = Number;

            fn $op(self, rhs: Number) -> Self::Output {
                (&self).$op(&rhs)
            }
        }
    };
}

impl_op_for_number!(Add, add);
impl_op_for_number!(Sub, sub);
impl_op_for_number!(Mul, mul);
impl_op_for_number!(Div, div);

////////////////////////////////////////////////////////////////////////////////
// Numerical built-ins:

#[bridge(name = "number?", lib = "(rnrs base builtins (6))")]
pub fn is_number(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Number)])
}

#[bridge(name = "complex?", lib = "(rnrs base builtins (6))")]
pub fn is_complex(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        arg.cast_to_scheme_type::<Number>()
            .as_ref()
            .is_some_and(Number::is_complex),
    )])
}

#[bridge(name = "real?", lib = "(rnrs base builtins (6))")]
pub fn is_real(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        arg.cast_to_scheme_type::<Number>()
            .as_ref()
            .is_some_and(Number::is_real),
    )])
}

#[bridge(name = "rational?", lib = "(rnrs base builtins (6))")]
pub fn is_rational(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        arg.cast_to_scheme_type::<Number>()
            .as_ref()
            .is_some_and(Number::is_rational),
    )])
}

#[bridge(name = "integer?", lib = "(rnrs base builtins (6))")]
pub fn is_integer(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        arg.cast_to_scheme_type::<Number>()
            .as_ref()
            .is_some_and(Number::is_integer),
    )])
}

#[bridge(name = "real-valued?", lib = "(rnrs base builtins (6))")]
pub fn real_valued_pred(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        arg.cast_to_scheme_type::<Number>()
            .as_ref()
            .is_some_and(Number::is_real_valued),
    )])
}

#[bridge(name = "rational-valued?", lib = "(rnrs base builtins (6))")]
pub fn rational_valued_pred(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        arg.cast_to_scheme_type::<Number>()
            .as_ref()
            .is_some_and(Number::is_rational_valued),
    )])
}

#[bridge(name = "integer-valued?", lib = "(rnrs base builtins (6))")]
pub fn integer_valued_pred(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        arg.cast_to_scheme_type::<Number>()
            .as_ref()
            .is_some_and(Number::is_integer_valued),
    )])
}

#[bridge(name = "exact?", lib = "(rnrs base builtins (6))")]
pub fn exact_pred(z: Number) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.is_exact())])
}

#[bridge(name = "inexact?", lib = "(rnrs base builtins (6))")]
pub fn inexact_pred(z: Number) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.is_inexact())])
}

#[bridge(name = "inexact", lib = "(rnrs base builtins (6))")]
pub fn inexact(z: Number) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.inexact())])
}

#[bridge(name = "exact", lib = "(rnrs base builtins (6))")]
pub fn exact(z: Number) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.exact())])
}

#[bridge(name = "=", lib = "(rnrs base builtins (6))")]
pub fn equal(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(equal_prim(args)?)])
}

pub(crate) fn equal_prim(vals: &[Value]) -> Result<bool, Exception> {
    if let Some((first, rest)) = vals.split_first() {
        let first: Number = first.try_to_scheme_type()?;
        for next in rest {
            let next: Number = next.try_to_scheme_type()?;
            if !(first == next) {
                return Ok(false);
            }
        }
    }
    Ok(true)
}

#[bridge(name = "<", lib = "(rnrs base builtins (6))")]
pub fn lesser(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(lesser_prim(args)?)])
}

pub(crate) fn lesser_prim(vals: &[Value]) -> Result<bool, Exception> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            let prev_num: Number = prev.try_to_scheme_type()?;
            let next_num: Number = next.try_to_scheme_type()?;
            if !prev_num.is_real() {
                return Err(Exception::type_error("real", "complex"));
            }
            if !next_num.is_real() {
                return Err(Exception::type_error("real", "complex"));
            }
            if !matches!(prev_num.partial_cmp(&next_num), Some(Ordering::Less)) {
                return Ok(false);
            }
            prev = next.clone();
        }
    }
    Ok(true)
}

#[bridge(name = ">", lib = "(rnrs base builtins (6))")]
pub fn greater(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(greater_prim(args)?)])
}

pub(crate) fn greater_prim(vals: &[Value]) -> Result<bool, Exception> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            let prev_num: Number = prev.try_to_scheme_type()?;
            let next_num: Number = next.try_to_scheme_type()?;
            // This is somewhat less efficient for small numbers but avoids
            // cloning big ones
            if !prev_num.is_real() {
                return Err(Exception::type_error("real", "complex"));
            }
            if !next_num.is_real() {
                return Err(Exception::type_error("real", "complex"));
            }
            if !matches!(prev_num.partial_cmp(&next_num), Some(Ordering::Greater)) {
                return Ok(false);
            }
            prev = next.clone();
        }
    }
    Ok(true)
}

#[bridge(name = "<=", lib = "(rnrs base builtins (6))")]
pub fn lesser_equal(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(lesser_equal_prim(args)?)])
}

pub(crate) fn lesser_equal_prim(vals: &[Value]) -> Result<bool, Exception> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            let prev_num: Number = prev.try_to_scheme_type()?;
            let next_num: Number = next.try_to_scheme_type()?;
            if !prev_num.is_real() {
                return Err(Exception::type_error("real", "complex"));
            }
            if !next_num.is_real() {
                return Err(Exception::type_error("real", "complex"));
            }
            if !matches!(
                prev_num.partial_cmp(&next_num),
                Some(Ordering::Equal | Ordering::Less)
            ) {
                return Ok(false);
            }
            prev = next.clone();
        }
    }
    Ok(true)
}

#[bridge(name = ">=", lib = "(rnrs base builtins (6))")]
pub fn greater_equal(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(greater_equal_prim(args)?)])
}

pub(crate) fn greater_equal_prim(vals: &[Value]) -> Result<bool, Exception> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            let prev_num: Number = prev.try_to_scheme_type()?;
            let next_num: Number = next.try_to_scheme_type()?;
            if !prev_num.is_real() {
                return Err(Exception::type_error("real", "complex"));
            }
            if !next_num.is_real() {
                return Err(Exception::type_error("real", "complex"));
            }
            if !matches!(
                prev_num.partial_cmp(&next_num),
                Some(Ordering::Equal | Ordering::Greater)
            ) {
                return Ok(false);
            }
            prev = next.clone();
        }
    }
    Ok(true)
}

#[bridge(name = "zero?", lib = "(rnrs base builtins (6))")]
pub fn zero(arg: &Value) -> Result<Vec<Value>, Exception> {
    let num: Number = arg.try_to_scheme_type()?;
    Ok(vec![Value::from(num.is_zero())])
}

#[bridge(name = "odd?", lib = "(rnrs base builtins (6))")]
pub fn odd(arg: &Value) -> Result<Vec<Value>, Exception> {
    let int: Integer = arg.try_to_scheme_type()?;
    Ok(vec![Value::from(int.odd())])
}

#[bridge(name = "even?", lib = "(rnrs base builtins (6))")]
pub fn even(arg: &Value) -> Result<Vec<Value>, Exception> {
    let int: Integer = arg.try_to_scheme_type()?;
    Ok(vec![Value::from(int.even())])
}

#[bridge(name = "finite?", lib = "(rnrs base builtins (6))")]
pub fn is_finite(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        !arg.try_to_scheme_type::<SimpleNumber>()?.is_infinite(),
    )])
}

#[bridge(name = "infinite?", lib = "(rnrs base builtins (6))")]
pub fn is_infinite(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        arg.try_to_scheme_type::<SimpleNumber>()?.is_infinite(),
    )])
}

#[bridge(name = "nan?", lib = "(rnrs base builtins (6))")]
pub fn is_nan(arg: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        arg.try_to_scheme_type::<SimpleNumber>()?.is_nan(),
    )])
}

#[bridge(name = "+", lib = "(rnrs base builtins (6))")]
pub fn add(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(add_prim(args)?)])
}

pub(crate) fn add_prim(vals: &[Value]) -> Result<Number, Exception> {
    let mut result = Number::from(0i64);
    for val in vals {
        let num: Number = val.try_to_scheme_type()?;
        result = result + num;
    }
    Ok(result)
}

#[bridge(name = "*", lib = "(rnrs base builtins (6))")]
pub fn mul(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(mul_prim(args)?)])
}

pub(crate) fn mul_prim(vals: &[Value]) -> Result<Number, Exception> {
    let mut result = Number::from(1i64);
    for val in vals {
        let num: Number = val.try_to_scheme_type()?;
        result = result * num;
    }
    Ok(result)
}

#[bridge(name = "-", lib = "(rnrs base builtins (6))")]
pub fn sub(arg1: &Value, args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(sub_prim(arg1, args)?)])
}

pub(crate) fn sub_prim(val1: &Value, vals: &[Value]) -> Result<Number, Exception> {
    let val1: Number = val1.try_to_scheme_type()?;
    let mut val1 = val1.clone();
    if vals.is_empty() {
        Ok(-val1)
    } else {
        for val in vals {
            let num: Number = val.try_to_scheme_type()?;
            val1 = val1 - num;
        }
        Ok(val1)
    }
}

#[bridge(name = "/", lib = "(rnrs base builtins (6))")]
pub fn div(arg1: &Value, args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(div_prim(arg1, args)?)])
}

pub(crate) fn div_prim(val1: &Value, vals: &[Value]) -> Result<Number, Exception> {
    let mut is_exact = true;
    let val1: Number = val1.try_to_scheme_type()?;
    is_exact &= val1.is_exact();
    if vals.is_empty() {
        if val1.is_zero() && is_exact {
            return Err(Exception::error("division by zero"));
        } else {
            return Ok(Number::from(1) / val1);
        }
    }
    let mut result = val1.clone();
    for val in vals {
        let num: Number = val.try_to_scheme_type()?;
        is_exact &= num.is_exact();
        if num.is_zero() && is_exact {
            return Err(Exception::error("division by zero"));
        }
        result = result / num;
    }
    Ok(result)
}

#[bridge(name = "div-and-mod", lib = "(rnrs base builtins (6))")]
pub fn div_mod(x1: SimpleNumber, x2: SimpleNumber) -> Result<Vec<Value>, Exception> {
    if x2.is_zero() {
        return Err(Exception::error("division by zero"));
    }
    let nd = x1.div_euclid(&x2);
    let nd_x2 = &x2 * &nd;
    let modulo = if nd_x2 < x1 { x1 - nd_x2 } else { nd_x2 - x1 };
    Ok(vec![Value::from(nd), Value::from(modulo)])
}

#[bridge(name = "div", lib = "(rnrs base builtins (6))")]
pub fn integer_division(x1: SimpleNumber, x2: SimpleNumber) -> Result<Vec<Value>, Exception> {
    if x2.is_zero() {
        return Err(Exception::error("division by zero"));
    }
    let nd = x1.div_euclid(&x2);
    Ok(vec![Value::from(nd)])
}

#[bridge(name = "mod", lib = "(rnrs base builtins (6))")]
pub fn modulo(x1: SimpleNumber, x2: SimpleNumber) -> Result<Vec<Value>, Exception> {
    if x2.is_zero() {
        return Err(Exception::error("modulo by zero"));
    }
    let nd = x1.div_euclid(&x2);
    let nd_x2 = &x2 * &nd;
    if nd_x2 < x1 {
        Ok(vec![Value::from(x1 - nd_x2)])
    } else {
        Ok(vec![Value::from(nd_x2 - x1)])
    }
}

#[bridge(name = "numerator", lib = "(rnrs base builtins (6))")]
pub fn numerator(obj: &Value) -> Result<Vec<Value>, Exception> {
    match obj.try_to_scheme_type::<SimpleNumber>()? {
        SimpleNumber::Rational(r) => Ok(vec![Value::from(Integer::from_sign_and_abs(
            r >= 0i64,
            r.into_numerator(),
        ))]),
        _ => Ok(vec![obj.clone()]),
    }
}

#[bridge(name = "denominator", lib = "(rnrs base builtins (6))")]
pub fn denominator(obj: &Value) -> Result<Vec<Value>, Exception> {
    match obj.try_to_scheme_type::<SimpleNumber>()? {
        SimpleNumber::Rational(r) => Ok(vec![Value::from(Integer::from(r.into_denominator()))]),
        SimpleNumber::Real(r) => Ok(vec![Value::from(
            f64::rounding_from(
                &Rational::try_from_float_simplest(r)
                    .map_err(|_| Exception::error("not a rational"))?
                    .into_denominator(),
                RoundingMode::Nearest,
            )
            .0,
        )]),
        _ => Ok(vec![Value::from(1)]),
    }
}

#[bridge(name = "floor", lib = "(rnrs base builtins (6))")]
pub fn floor(obj: &Value) -> Result<Vec<Value>, Exception> {
    match obj.try_to_scheme_type::<SimpleNumber>()? {
        SimpleNumber::Rational(r) => Ok(vec![Value::from(
            Integer::rounding_from(r, RoundingMode::Floor).0,
        )]),
        SimpleNumber::Real(r) => Ok(vec![Value::from(r.floor())]),
        _ => Ok(vec![obj.clone()]),
    }
}

#[bridge(name = "ceiling", lib = "(rnrs base builtins (6))")]
pub fn ceiling(obj: &Value) -> Result<Vec<Value>, Exception> {
    match obj.try_to_scheme_type::<SimpleNumber>()? {
        SimpleNumber::Rational(r) => Ok(vec![Value::from(
            Integer::rounding_from(r, RoundingMode::Ceiling).0,
        )]),
        SimpleNumber::Real(r) => Ok(vec![Value::from(r.ceil())]),
        _ => Ok(vec![obj.clone()]),
    }
}

#[bridge(name = "truncate", lib = "(rnrs base builtins (6))")]
pub fn truncate(obj: &Value) -> Result<Vec<Value>, Exception> {
    match obj.try_to_scheme_type::<SimpleNumber>()? {
        SimpleNumber::Rational(r) => Ok(vec![Value::from(
            Integer::rounding_from(r, RoundingMode::Down).0,
        )]),
        SimpleNumber::Real(r) => Ok(vec![Value::from(r.trunc())]),
        _ => Ok(vec![obj.clone()]),
    }
}

#[bridge(name = "round", lib = "(rnrs base builtins (6))")]
pub fn round(obj: &Value) -> Result<Vec<Value>, Exception> {
    match obj.try_to_scheme_type::<SimpleNumber>()? {
        SimpleNumber::Rational(r) => Ok(vec![Value::from(
            Integer::rounding_from(r, RoundingMode::Nearest).0,
        )]),
        SimpleNumber::Real(r) => Ok(vec![Value::from(r.round_ties_even())]),
        _ => Ok(vec![obj.clone()]),
    }
}

#[bridge(name = "exp", lib = "(rnrs base builtins (6))")]
pub fn exp(z: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.try_to_scheme_type::<Number>()?.exp())])
}

#[bridge(name = "log", lib = "(rnrs base builtins (6))")]
pub fn log(z: &Value, base: &[Value]) -> Result<Vec<Value>, Exception> {
    let base = match base {
        [] => None,
        [base] => Some(base.try_to_scheme_type::<f64>()?),
        _ => return Err(Exception::error("too many arguments")),
    };
    let num = match z.try_to_scheme_type::<SimpleNumber>()? {
        SimpleNumber::FixedInteger(i) => i as f64,
        SimpleNumber::BigInteger(i) => f64::rounding_from(&i, RoundingMode::Nearest).0,
        SimpleNumber::Rational(r) => f64::rounding_from(&r, RoundingMode::Nearest).0,
        SimpleNumber::Real(r) => r,
    };
    if let Some(base) = base {
        Ok(vec![Value::from(num.log(base))])
    } else {
        Ok(vec![Value::from(num.ln())])
    }
}

#[bridge(name = "sin", lib = "(rnrs base builtins (6))")]
pub fn sin(z: Number) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.sin())])
}

#[bridge(name = "cos", lib = "(rnrs base builtins (6))")]
pub fn cos(z: Number) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.cos())])
}

#[bridge(name = "tan", lib = "(rnrs base builtins (6))")]
pub fn tan(z: Number) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.tan())])
}

#[bridge(name = "asin", lib = "(rnrs base builtins (6))")]
pub fn asin(z: Number) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.asin())])
}

#[bridge(name = "acos", lib = "(rnrs base builtins (6))")]
pub fn acos(z: Number) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.acos())])
}

#[bridge(name = "atan", lib = "(rnrs base builtins (6))")]
pub fn atan(z: Number) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.atan())])
}

#[bridge(name = "sqrt", lib = "(rnrs base builtins (6))")]
pub fn sqrt(z: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(z.try_to_scheme_type::<Number>()?.sqrt())])
}

#[bridge(name = "exact-integer-sqrt", lib = "(rnrs base builtins (6))")]
pub fn exact_integer_sqrt(arg: Integer) -> Result<Vec<Value>, Exception> {
    let s = (&arg).floor_sqrt();
    let r = arg - &s * &s;
    Ok(vec![Value::from(s), Value::from(r)])
}

#[bridge(name = "expt", lib = "(rnrs base builtins (6))")]
pub fn expt(z1: &Value, z2: &Value) -> Result<Vec<Value>, Exception> {
    let z1 = z1.try_to_scheme_type::<Number>()?;
    let z2 = z2.try_to_scheme_type::<Number>()?;
    Ok(vec![Value::from(z1.pow(&z2))])
}

#[bridge(name = "make-rectangular", lib = "(rnrs base builtins (6))")]
pub fn make_rectangular(x1: SimpleNumber, x2: SimpleNumber) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(ComplexNumber::new(x1, x2))])
}

#[bridge(name = "make-polar", lib = "(rnrs base builtins (6))")]
pub fn make_polar(x1: SimpleNumber, x2: SimpleNumber) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(ComplexNumber::from_polar(x1, x2))])
}

#[bridge(name = "real-part", lib = "(rnrs base builtins (6))")]
pub fn real_part(arg: &Value) -> Result<Vec<Value>, Exception> {
    let num: Number = arg.try_to_scheme_type()?;
    if let Some(complex) = num.as_complex() {
        Ok(vec![Value::from(complex.re.clone())])
    } else {
        Ok(vec![arg.clone()])
    }
}

#[bridge(name = "imag-part", lib = "(rnrs base builtins (6))")]
pub fn imag_part(arg: &Value) -> Result<Vec<Value>, Exception> {
    let num: Number = arg.try_to_scheme_type()?;
    if let Some(complex) = num.as_complex() {
        Ok(vec![Value::from(complex.im.clone())])
    } else {
        Err(Exception::error("expected complex number"))
    }
}

#[bridge(name = "magnitude", lib = "(rnrs base builtins (6))")]
pub fn magnitude(arg: &Value) -> Result<Vec<Value>, Exception> {
    let num: Number = arg.try_to_scheme_type()?;
    if let Some(complex) = num.as_complex() {
        Ok(vec![Value::from(complex.magnitude())])
    } else {
        Ok(vec![arg.clone()])
    }
}

#[bridge(name = "angle", lib = "(rnrs base builtins (6))")]
pub fn angle(z: ComplexNumber) -> Result<Vec<Value>, Exception> {
    let (_, angle) = z.to_polar();
    Ok(vec![Value::from(angle)])
}

#[bridge(name = "number->string", lib = "(rnrs base builtins (6))")]
pub fn number_to_string(z: ComplexNumber, rest_args: &[Value]) -> Result<Vec<Value>, Exception> {
    let (radix, precision) = match rest_args {
        [] => (10, None),
        [radix] => (radix.try_to_scheme_type::<u32>()?, None),
        [radix, precision] => (
            radix.try_to_scheme_type::<u32>()?,
            Some(precision.try_to_scheme_type::<usize>()?),
        ),
        _ => return Err(Exception::wrong_num_of_var_args(2..3, 1 + rest_args.len())),
    };
    if !matches!(radix, 2 | 8 | 10 | 16) {
        return Err(Exception::error(format!(
            "invalid radix ({radix}) must be 2, 8, 10 or 16"
        )));
    }
    let result = z.to_string(radix, precision).ok_or_else(|| {
        Exception::implementation_restriction(format!("could not format {z} with radix {radix}"))
    })?;
    Ok(vec![Value::from(result)])
}

#[maybe_async]
#[bridge(name = "string->number", lib = "(rnrs base builtins (6))")]
pub fn string_to_number(s: WideString, rest_args: &[Value]) -> Result<Vec<Value>, Exception> {
    let radix = match rest_args {
        [] => 10,
        [radix] => match radix.try_to_scheme_type::<u32>()? {
            radix @ (2 | 8 | 10 | 16) => radix,
            radix => {
                return Err(Exception::error(format!(
                    "invalid radix ({radix}) must be 2, 8, 10 or 16"
                )));
            }
        },
        _ => return Err(Exception::wrong_num_of_var_args(1..2, 1 + rest_args.len())),
    };
    // TODO: This is not ideal
    let s = s.to_string();
    let bytes = Cursor::new(s.as_bytes().to_vec());
    let port = Port::new("", bytes, BufferMode::Block, Some(Transcoder::native()));
    let info = &port.0.info;
    #[cfg(not(feature = "async"))]
    let mut data = port.0.data.lock().unwrap();
    #[cfg(feature = "tokio")]
    let mut data = port.0.data.lock().await;
    let mut lexer = Lexer::new(&mut data, info, Span::default());
    let Some(number) = maybe_await!(lexer.number(radix)).ok().flatten() else {
        return Ok(vec![Value::from(false)]);
    };

    if maybe_await!(lexer.take()).ok().flatten().is_some() {
        return Ok(vec![Value::from(false)]);
    }

    let Ok(number) = Number::try_from(number) else {
        return Ok(vec![Value::from(false)]);
    };

    Ok(vec![Value::from(number)])
}

/// R6RS Fixnums
pub struct Fixnum(pub i64);

impl From<&Value> for Option<Fixnum> {
    fn from(value: &Value) -> Option<Fixnum> {
        if let Some(num) = value.cast_to_scheme_type::<Number>()
            && let NumberInner::Simple(simple) = &*num.0
            && let SimpleNumber::FixedInteger(fixnum) = simple
        {
            Some(Fixnum(*fixnum))
        } else {
            None
        }
    }
}

impl TryFrom<&Value> for Fixnum {
    type Error = Exception;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        if let Some(num) = value.cast_to_scheme_type::<Number>()
            && let NumberInner::Simple(simple) = &*num.0
            && let SimpleNumber::FixedInteger(fixnum) = simple
        {
            Ok(Fixnum(*fixnum))
        } else {
            Err(Exception::error("value is not a fixnum"))
        }
    }
}

#[bridge(name = "fixnum?", lib = "(rnrs arithmetic fixnums (6))")]
pub fn fixnum_pred(obj: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        obj.cast_to_scheme_type::<Fixnum>().is_some(),
    )])
}

#[bridge(name = "fixnum-width", lib = "(rnrs arithmetic fixnums (6))")]
pub fn fixnum_width() -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(64)])
}

#[bridge(name = "least-fixnum", lib = "(rnrs arithmetic fixnums (6))")]
pub fn least_fixnum() -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(i64::MIN)])
}

#[bridge(name = "greatest-fixnum", lib = "(rnrs arithmetic fixnums (6))")]
pub fn greatest_fixnum() -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(i64::MAX)])
}

/// R6RS Flonums
pub struct Flonum(pub f64);

impl From<&Value> for Option<Flonum> {
    fn from(value: &Value) -> Option<Flonum> {
        if let Some(num) = value.cast_to_scheme_type::<Number>()
            && let NumberInner::Simple(simple) = &*num.0
            && let SimpleNumber::Real(flonum) = simple
        {
            Some(Flonum(*flonum))
        } else {
            None
        }
    }
}

impl TryFrom<&Value> for Flonum {
    type Error = Exception;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        if let Some(num) = value.cast_to_scheme_type::<Number>()
            && let NumberInner::Simple(simple) = &*num.0
            && let SimpleNumber::Real(flonum) = simple
        {
            Ok(Flonum(*flonum))
        } else {
            Err(Exception::error("value is not a flonum"))
        }
    }
}

#[bridge(name = "flonum?", lib = "(rnrs arithmetic flonums (6))")]
pub fn flonum_pred(obj: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        obj.cast_to_scheme_type::<Flonum>().is_some(),
    )])
}
