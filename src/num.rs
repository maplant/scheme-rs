//! Numerical tower.

use crate::{
    exceptions::Exception,
    gc::Trace,
    registry::bridge,
    value::{Value, ValueType},
};
use core::f64;
use malachite::{
    Integer,
    base::{
        num::{
            arithmetic::traits::{Parity, Pow},
            conversion::traits::{ConvertibleFrom, RoundingFrom, WrappingFrom},
        },
        rounding_modes::RoundingMode,
    },
    rational::Rational,
};
use num::{ToPrimitive, Zero};
use std::{
    cmp::Ordering,
    fmt,
    hash::Hash,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
    sync::Arc,
};

#[repr(align(16))]
pub(crate) enum NumberInner {
    Simple(SimpleNumber),
    Complex(Complex),
}

#[derive(Clone, Trace)]
pub struct Number(pub(crate) Arc<NumberInner>);

impl Number {
    pub fn as_simple(&self) -> Option<&SimpleNumber> {
        match self.0.as_ref() {
            NumberInner::Simple(simple) => Some(simple),
            NumberInner::Complex(_) => None,
        }
    }

    pub fn as_complex(&self) -> Option<&Complex> {
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
            NumberInner::Complex(complex) => complex.im.is_zero(),
        }
    }

    pub fn is_complex(&self) -> bool {
        match self.0.as_ref() {
            NumberInner::Simple(_) => false,
            NumberInner::Complex(complex) => !complex.im.is_zero(),
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

#[derive(Clone)]
#[repr(align(16))]
pub enum SimpleNumber {
    FixedInteger(i64),
    BigInteger(Integer),
    Rational(Rational),
    Real(f64),
}

impl SimpleNumber {
    pub fn is_zero(&self) -> bool {
        match self {
            SimpleNumber::FixedInteger(i) => i.is_zero(),
            SimpleNumber::BigInteger(i) => i.eq(&0),
            SimpleNumber::Rational(r) => r.eq(&0),
            SimpleNumber::Real(r) => r.is_zero(),
        }
    }

    pub fn is_exact(&self) -> bool {
        matches!(
            self,
            Self::FixedInteger(_) | Self::BigInteger(_) | Self::Rational(_)
        )
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

impl From<f64> for SimpleNumber {
    fn from(r: f64) -> Self {
        Self::Real(r)
    }
}

impl From<Complex> for Number {
    fn from(complex: Complex) -> Self {
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
            SimpleNumber::Real(r) if r.fract() == 0.0 => {
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
            SimpleNumber::Real(r) if r.fract() == 0.0 => {
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
            Self::Real(r) => write!(f, "{r}"),
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
            Self::Real(r) => write!(f, "{r}"),
        }
    }
}

impl Hash for SimpleNumber {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            SimpleNumber::FixedInteger(i) => i.hash(state),
            SimpleNumber::BigInteger(i) => i.hash(state),
            SimpleNumber::Rational(r) => r.hash(state),
            SimpleNumber::Real(r) if r.is_nan() => {
                // Use the same bit pattern for all NaNs to mirror eqv
                f64::NAN.to_bits().hash(state)
            }
            SimpleNumber::Real(r) => r.to_bits().hash(state),
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

            (Self::FixedInteger(fixed_int), Self::Real(float))
            | (Self::Real(float), Self::FixedInteger(fixed_int))
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
            SimpleNumber::FixedInteger(i) => SimpleNumber::FixedInteger(-i),
            SimpleNumber::BigInteger(i) => SimpleNumber::BigInteger(-i),
            SimpleNumber::Rational(r) => SimpleNumber::Rational(-r),
            SimpleNumber::Real(r) => SimpleNumber::Real(-r),
        }
    }
}

macro_rules! impl_op_for_simple_and_complex_numbers {
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

        impl $trait for &Complex {
            type Output = Complex;

            fn $op(self, rhs: &Complex) -> Complex {
                Complex {
                    re: (&self.re).$op(&rhs.re),
                    im: (&self.im).$op(&rhs.im),
                }
            }
        }

        impl $trait for Complex {
            type Output = Complex;

            fn $op(self, rhs: Complex) -> Complex {
                Complex {
                    re: self.re.$op(rhs.re),
                    im: self.im.$op(rhs.im),
                }
            }
        }
    };
}

impl_op_for_simple_and_complex_numbers!(Add, add, checked_add);
impl_op_for_simple_and_complex_numbers!(Sub, sub, checked_sub);
impl_op_for_simple_and_complex_numbers!(Mul, mul, checked_mul);
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

#[derive(Clone)]
pub struct Complex {
    re: SimpleNumber,
    im: SimpleNumber,
}

impl Complex {
    pub fn new(re: SimpleNumber, im: SimpleNumber) -> Self {
        Self { re, im }
    }
}

impl From<SimpleNumber> for Complex {
    fn from(value: SimpleNumber) -> Self {
        Self {
            re: value,
            im: SimpleNumber::FixedInteger(0),
        }
    }
}

impl PartialEq for Complex {
    fn eq(&self, rhs: &Complex) -> bool {
        self.re == rhs.re && self.im == rhs.im
    }
}

impl Neg for Complex {
    type Output = Complex;

    fn neg(self) -> Complex {
        Self {
            re: -self.re,
            im: -self.im,
        }
    }
}

impl Neg for &Complex {
    type Output = Complex;

    fn neg(self) -> Complex {
        Complex {
            re: -(&self.re),
            im: -(&self.im),
        }
    }
}

impl Div for &Complex {
    type Output = Complex;

    fn div(self, rhs: &Complex) -> Complex {
        let norm_sqr = rhs.re.powi(2) + rhs.im.powi(2);
        let re = self.re.clone() * rhs.re.clone() + self.im.clone() * rhs.im.clone();
        let im = self.im.clone() * rhs.re.clone() - self.re.clone() * rhs.im.clone();
        Complex {
            re: re / norm_sqr.clone(),
            im: im / norm_sqr,
        }
    }
}

impl Div for Complex {
    type Output = Complex;

    fn div(self, rhs: Complex) -> Complex {
        &self / &rhs
    }
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.re)?;
        if self.im.is_positive() && !self.im.is_nan() && !self.im.is_infinite() {
            write!(f, "+")?;
        }
        write!(f, "{}i", self.im)
    }
}

impl fmt::Debug for Complex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}+{:?}i", self.re, self.im)
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
                        Number::from((&Complex::from(lhs.clone())).$op(rhs))
                    }
                    (NumberInner::Complex(lhs), NumberInner::Simple(rhs)) => {
                        Number::from(lhs.$op(&Complex::from(rhs.clone())))
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

// real-valued?
// rational-valued?
// integer-valued?
// exact?
// inexact?
// inexact
// exact

#[bridge(name = "=", lib = "(rnrs base builtins (6))")]
pub fn equal_builtin(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(equal(args)?)])
}

pub(crate) fn equal(vals: &[Value]) -> Result<bool, Exception> {
    if let Some((first, rest)) = vals.split_first() {
        let first: Number = first.try_to_scheme_type()?;
        for next in rest {
            let next: Number = next.try_to_scheme_type()?;
            if first != next {
                return Ok(false);
            }
        }
    }
    Ok(true)
}

#[bridge(name = "<", lib = "(rnrs base builtins (6))")]
pub fn lesser_builtin(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(lesser(args)?)])
}

pub(crate) fn lesser(vals: &[Value]) -> Result<bool, Exception> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev: Number = prev.try_to_scheme_type()?;
                let next: Number = next.try_to_scheme_type()?;
                if prev.is_complex() {
                    return Err(Exception::type_error("number", "complex"));
                }
                if next.is_complex() {
                    return Err(Exception::type_error("number", "complex"));
                }
                if prev >= next {
                    return Ok(false);
                }
            }
            prev = next.clone();
        }
    }
    Ok(true)
}

#[bridge(name = ">", lib = "(rnrs base builtins (6))")]
pub fn greater_builtin(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(greater(args)?)])
}

pub(crate) fn greater(vals: &[Value]) -> Result<bool, Exception> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev: Number = prev.try_to_scheme_type()?;
                let next: Number = next.try_to_scheme_type()?;
                // This is somewhat less efficient for small numbers but avoids
                // cloning big ones
                if prev.is_complex() {
                    return Err(Exception::type_error("real", "complex"));
                }
                if next.is_complex() {
                    return Err(Exception::type_error("real", "complex"));
                }
                if prev <= next {
                    return Ok(false);
                }
            }
            prev = next.clone();
        }
    }
    Ok(true)
}

#[bridge(name = "<=", lib = "(rnrs base builtins (6))")]
pub fn lesser_equal_builtin(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(lesser_equal(args)?)])
}

pub(crate) fn lesser_equal(vals: &[Value]) -> Result<bool, Exception> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev: Number = prev.try_to_scheme_type()?;
                let next: Number = next.try_to_scheme_type()?;
                if prev.is_complex() {
                    return Err(Exception::type_error("number", "complex"));
                }
                if next.is_complex() {
                    return Err(Exception::type_error("number", "complex"));
                }
                if prev > next {
                    return Ok(false);
                }
            }
            prev = next.clone();
        }
    }
    Ok(true)
}

#[bridge(name = ">=", lib = "(rnrs base builtins (6))")]
pub fn greater_equal_builtin(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(greater_equal(args)?)])
}

pub(crate) fn greater_equal(vals: &[Value]) -> Result<bool, Exception> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev: Number = prev.try_to_scheme_type()?;
                let next: Number = next.try_to_scheme_type()?;
                if prev.is_complex() {
                    return Err(Exception::type_error("real", "complex"));
                }
                if next.is_complex() {
                    return Err(Exception::type_error("real", "complex"));
                }
                if prev < next {
                    return Ok(false);
                }
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
pub fn add_builtin(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(add(args)?)])
}

pub(crate) fn add(vals: &[Value]) -> Result<Number, Exception> {
    let mut result = Number::from(0i64);
    for val in vals {
        let num: Number = val.try_to_scheme_type()?;
        result = result + num;
    }
    Ok(result)
}

#[bridge(name = "*", lib = "(rnrs base builtins (6))")]
pub fn mul_builtin(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(mul(args)?)])
}

pub(crate) fn mul(vals: &[Value]) -> Result<Number, Exception> {
    let mut result = Number::from(1i64);
    for val in vals {
        let num: Number = val.try_to_scheme_type()?;
        result = result * num;
    }
    Ok(result)
}

#[bridge(name = "-", lib = "(rnrs base builtins (6))")]
pub fn sub_builtin(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(add(args)?)])
}

pub(crate) fn sub(val1: &Value, vals: &[Value]) -> Result<Number, Exception> {
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
pub fn div_builtin(arg1: &Value, args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(div(arg1, args)?)])
}

pub(crate) fn div(val1: &Value, vals: &[Value]) -> Result<Number, Exception> {
    let val1: Number = val1.try_to_scheme_type()?;
    if vals.is_empty() {
        if val1.is_zero() {
            return Err(Exception::error("division by zero"));
        } else {
            return Ok(Number::from(1) / val1);
        }
    }
    let mut result = val1.clone();
    for val in vals {
        let num: Number = val.try_to_scheme_type()?;
        if num.is_zero() {
            return Err(Exception::error("division by zero"));
        }
        result = result / num;
    }
    Ok(result)
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
        SimpleNumber::Real(r) => Ok(vec![Value::from(Integer::from(
            Rational::try_from_float_simplest(r)
                .map_err(|_| Exception::error("not a rational"))?
                .into_denominator(),
        ))]),
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

#[bridge(name = "magnitude", lib = "(rnrs base builtins (6))")]
pub fn magnitude(arg: &Value) -> Result<Vec<Value>, Exception> {
    let num: Number = arg.try_to_scheme_type()?;
    if let Some(complex) = num.as_complex() {
        Ok(vec![Value::from(
            (complex.re.powi(2) + complex.im.powi(2)).sqrt(),
        )])
    } else {
        Ok(vec![arg.clone()])
    }
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
