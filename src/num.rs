use crate::{
    exceptions::Condition,
    gc::Trace,
    registry::bridge,
    value::{Value, ValueType},
};
use malachite::{
    Integer,
    base::{
        num::{
            arithmetic::traits::Parity,
            conversion::traits::{ConvertibleFrom, RoundingFrom, WrappingFrom},
        },
        rounding_modes::RoundingMode,
    },
    rational::{Rational, conversion::from_primitive_float::RationalFromPrimitiveFloatError},
};
use num::{Complex, FromPrimitive, Zero, complex::Complex64};
use ordered_float::OrderedFloat;
use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
    hash::Hash,
    ops::{Add, Mul, Neg, Sub},
    sync::Arc,
};

#[derive(Clone, Trace)]
#[repr(align(16))]
pub enum Number {
    FixedInteger(#[trace(skip)] i64),
    BigInteger(#[trace(skip)] Integer),
    Rational(#[trace(skip)] Rational),
    Real(#[trace(skip)] f64),
    Complex(#[trace(skip)] Complex64),
}

impl Number {
    pub fn is_exact(&self) -> bool {
        matches!(
            self,
            Self::FixedInteger(_) | Self::BigInteger(_) | Self::Rational(_)
        )
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Self::FixedInteger(i) => i.is_zero(),
            Self::BigInteger(i) => i.eq(&0),
            Self::Rational(r) => r.eq(&0),
            Self::Real(r) => r.is_zero(),
            Self::Complex(c) => c.is_zero(),
        }
    }

    pub fn is_even(&self) -> bool {
        match self {
            Self::FixedInteger(i) => i.even(),
            Self::BigInteger(i) => i.even(),
            Self::Rational(_) => false,
            Self::Real(_) => false,
            Self::Complex(_) => false,
        }
    }

    pub fn is_odd(&self) -> bool {
        match self {
            Self::FixedInteger(i) => i.odd(),
            Self::BigInteger(i) => i.odd(),
            Self::Rational(_) => false,
            Self::Real(_) => false,
            Self::Complex(_) => false,
        }
    }

    pub fn is_complex(&self) -> bool {
        matches!(self, Self::Complex(_))
    }
}

impl TryFrom<&Number> for usize {
    type Error = NumberToUsizeError;

    fn try_from(number: &Number) -> Result<usize, NumberToUsizeError> {
        let make_err = |kind| NumberToUsizeError::new(number.clone(), kind);

        // using `<` operator would require dereferencing
        if matches!(
            number.partial_cmp(&Number::FixedInteger(0)),
            Some(Ordering::Less) | None
        ) {
            return Err(make_err(NumberToUsizeErrorKind::Negative));
        }

        match number {
            Number::FixedInteger(i) => <i64 as TryInto<usize>>::try_into(*i)
                .map_err(|_| make_err(NumberToUsizeErrorKind::TooLarge)),
            Number::BigInteger(i) => usize::convertible_from(i)
                .then(|| usize::wrapping_from(i))
                .ok_or_else(|| make_err(NumberToUsizeErrorKind::TooLarge)),
            _ => Err(make_err(NumberToUsizeErrorKind::NotInteger)),
        }
    }
}

impl From<usize> for Number {
    fn from(u: usize) -> Self {
        match u.try_into() {
            Ok(i) => Number::FixedInteger(i),
            Err(_) => Number::BigInteger(Integer::from(u)),
        }
    }
}

impl From<i32> for Number {
    fn from(i: i32) -> Self {
        Self::FixedInteger(i as i64)
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

macro_rules! number_try_into_impl_integer {
    ($ty:tt) => {
        impl TryInto<$ty> for Number {
            type Error = Condition;
            fn try_into(self) -> Result<$ty, Self::Error> {
                match self {
                    Number::FixedInteger(i) => {
                        if i <= $ty::MAX as i64 && i >= $ty::MIN as i64 {
                            Ok(i as $ty)
                        } else {
                            Err(Condition::not_representable(&format!("{i}"), stringify!($ty)))
                        }
                    }
                    Number::BigInteger(bigint) => {
                        if bigint <= $ty::MAX && bigint >= $ty::MIN {
                            let vec = bigint.into_twos_complement_limbs_asc();
                            Ok(vec[0] as $ty)
                        } else {
                            Err(Condition::not_representable(&format!("{bigint}"), stringify!($ty)))
                        }
                    }
                    Number::Rational(r) => {
                        Err(Condition::conversion_error(stringify!($ty), "Rational"))
                    }
                    Number::Real(r) => {
                        Err(Condition::conversion_error(stringify!($ty), "Real"))
                    }
                    Number::Complex(c) => {
                        Err(Condition::conversion_error(stringify!($ty), "Complex"))
                    }
                }
            }
        }
    };
}

number_try_into_impl_integer!(u8);
number_try_into_impl_integer!(u16);
number_try_into_impl_integer!(u32);
number_try_into_impl_integer!(u64);
number_try_into_impl_integer!(u128);
number_try_into_impl_integer!(i8);
number_try_into_impl_integer!(i16);
number_try_into_impl_integer!(i32);
number_try_into_impl_integer!(i64);
number_try_into_impl_integer!(i128);



impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::FixedInteger(i) => write!(f, "{i}"),
            Self::BigInteger(i) => write!(f, "{i}"),
            Self::Rational(r) => write!(f, "{r}"),
            Self::Real(r) => write!(f, "{r}"),
            Self::Complex(c) => write!(f, "{c}"),
        }
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::FixedInteger(i) => write!(f, "{i}"),
            Self::BigInteger(i) => write!(f, "{i}"),
            Self::Rational(r) => write!(f, "{r}"),
            Self::Real(r) => write!(f, "{r}"),
            Self::Complex(c) => write!(f, "{c}"),
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
            (Self::BigInteger(l), Self::BigInteger(r)) => l == r,
            (Self::Rational(l), Self::Rational(r)) => l == r,
            (Self::Complex(_), _) | (_, Self::Complex(_)) => false,
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
            | (Self::Real(float), Self::FixedInteger(fixed_int)) => {
                <i64 as TryInto<i32>>::try_into(*fixed_int)
                    .map(f64::from)
                    .map(|fixed_int| fixed_int == *float)
                    .unwrap_or(false)
            }
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

macro_rules! impl_checked_op_for_number {
    ($trait:ident, $unchecked:ident, $checked:ident) => {
        impl Number {
            pub fn $checked(&self, rhs: &Number) -> Result<Number, Box<ArithmeticError>> {
                Ok(match (&self, rhs) {
                    (Self::FixedInteger(l), Self::FixedInteger(r)) => {
                        l.$checked(*r).map(Self::FixedInteger).unwrap_or_else(|| {
                            Self::BigInteger(Integer::from(*l).$unchecked(Integer::from(*r)))
                        })
                    }
                    (Self::BigInteger(l), Self::BigInteger(r)) => Self::BigInteger(l.$unchecked(r)),
                    (Self::Rational(l), Self::Rational(r)) => Self::Rational(l.$unchecked(r)),
                    (Self::Complex(l), Self::Complex(r)) => Self::Complex(l.$unchecked(r)),
                    (Self::Real(l), Self::Real(r)) => Self::Real(l.$unchecked(r)),

                    (Self::BigInteger(l), Self::FixedInteger(r)) => i64::convertible_from(l)
                        .then(|| i64::wrapping_from(l).$checked(*r))
                        .flatten()
                        .map(Self::FixedInteger)
                        .unwrap_or_else(|| Self::BigInteger(l.$unchecked(Integer::from(*r)))),
                    (Self::FixedInteger(l), Self::BigInteger(r)) => i64::convertible_from(r)
                        .then(|| l.$checked(i64::wrapping_from(r)))
                        .flatten()
                        .map(Self::FixedInteger)
                        .unwrap_or_else(|| Self::BigInteger(Integer::from(*l).$unchecked(r))),

                    (Self::Rational(l), Self::FixedInteger(r)) => {
                        Self::Rational(l.$unchecked(Rational::from(*r)))
                    }
                    (Self::FixedInteger(l), Self::Rational(r)) => {
                        Self::Rational(Rational::from(*l).$unchecked(r))
                    }

                    (Self::Rational(l), Self::BigInteger(r)) => {
                        Self::Rational(l.$unchecked(Rational::from(r)))
                    }
                    (Self::BigInteger(l), Self::Rational(r)) => {
                        Self::Rational(Rational::from(l).$unchecked(r))
                    }

                    (Self::BigInteger(l), Self::Real(r)) => Self::Real(
                        f64::rounding_from(l, RoundingMode::Nearest)
                            .0
                            .$unchecked(*r),
                    ),
                    (Self::Real(l), Self::BigInteger(r)) => {
                        Self::Real(l.$unchecked(f64::rounding_from(r, RoundingMode::Nearest).0))
                    }

                    (Self::Rational(l), Self::Real(r)) => {
                        Self::Real(f64::rounding_from(l, RoundingMode::Nearest).0.$unchecked(r))
                    }
                    (Self::Real(l), Self::Rational(r)) => {
                        Self::Real(l.$unchecked(f64::rounding_from(r, RoundingMode::Nearest).0))
                    }

                    (Self::FixedInteger(l), Self::Real(r)) => Self::Real((*l as f64).$unchecked(r)),
                    (Self::Real(l), Self::FixedInteger(r)) => Self::Real(l.$unchecked(*r as f64)),

                    (Self::FixedInteger(l), Self::Complex(r)) => Self::Complex(
                        Complex64::from_i64(*l)
                            .ok_or_else(|| {
                                Box::new(ArithmeticError::Overflow(
                                    Operation::$trait,
                                    self.clone(),
                                    rhs.clone(),
                                ))
                            })?
                            .$unchecked(r),
                    ),
                    (Self::Complex(l), Self::FixedInteger(r)) => {
                        Self::Complex(l.$unchecked(Complex64::from_i64(*r).ok_or_else(|| {
                            Box::new(ArithmeticError::Overflow(
                                Operation::$trait,
                                self.clone(),
                                rhs.clone(),
                            ))
                        })?))
                    }

                    (Self::BigInteger(l), Self::Complex(r)) => Self::Complex(
                        Complex64::from_f64(f64::rounding_from(l, RoundingMode::Nearest).0)
                            .ok_or_else(|| {
                                Box::new(ArithmeticError::Overflow(
                                    Operation::$trait,
                                    self.clone(),
                                    rhs.clone(),
                                ))
                            })?
                            .$unchecked(r),
                    ),
                    (Self::Complex(l), Self::BigInteger(r)) => {
                        Self::Complex(l.$unchecked(f64::rounding_from(r, RoundingMode::Nearest).0))
                    }

                    (Self::Rational(l), Self::Complex(r)) => Self::Complex(
                        Complex64::from_f64(f64::rounding_from(l, RoundingMode::Nearest).0)
                            .ok_or_else(|| {
                                Box::new(ArithmeticError::Overflow(
                                    Operation::$trait,
                                    self.clone(),
                                    rhs.clone(),
                                ))
                            })?
                            .$unchecked(r),
                    ),
                    (Self::Complex(l), Self::Rational(r)) => {
                        Self::Complex(l.$unchecked(f64::rounding_from(r, RoundingMode::Nearest).0))
                    }

                    (Number::Real(l), Number::Complex(r)) => Self::Complex(
                        Complex64::from_f64(*l)
                            .ok_or_else(|| {
                                Box::new(ArithmeticError::Overflow(
                                    Operation::$trait,
                                    self.clone(),
                                    rhs.clone(),
                                ))
                            })?
                            .$unchecked(r),
                    ),
                    (Number::Complex(l), Number::Real(r)) => Self::Complex(l.$unchecked(r)),
                })
            }
        }
    };
}

impl_checked_op_for_number!(Add, add, checked_add);
impl_checked_op_for_number!(Sub, sub, checked_sub);
impl_checked_op_for_number!(Mul, mul, checked_mul);

impl Number {
    pub fn checked_div(&self, rhs: &Self) -> Result<Self, Box<ArithmeticError>> {
        let overflow = || ArithmeticError::Overflow(Operation::Div, self.clone(), rhs.clone());
        if rhs.is_zero() {
            return Err(Box::new(ArithmeticError::DivisionByZero));
        }

        Ok(match (self, rhs) {
            (Self::FixedInteger(l), Self::FixedInteger(r)) => {
                Self::Rational(Rational::from(*l) / Rational::from(*r))
            }
            (Self::BigInteger(l), Self::BigInteger(r)) => {
                Self::Rational(Rational::from_integers_ref(l, r))
            }
            (Self::Rational(l), Self::Rational(r)) => Self::Rational(l / r),
            (Self::Complex(l), Self::Complex(r)) => Self::Complex(l / r),
            (Self::Real(l), Self::Real(r)) => Self::Real(l / r),

            (Self::BigInteger(l), Self::FixedInteger(r)) => {
                Self::Rational(Rational::from(l) / Rational::from(*r))
            }
            (Self::FixedInteger(l), Self::BigInteger(r)) => {
                Self::Rational(Rational::from(*l) / Rational::from(r))
            }

            (Self::Rational(l), Self::FixedInteger(r)) => Self::Rational(l / Rational::from(*r)),
            (Self::FixedInteger(l), Self::Rational(r)) => Self::Rational(Rational::from(*l) / r),

            (Self::BigInteger(l), Self::Rational(r)) => Self::Rational(Rational::from(l) / r),
            (Self::Rational(l), Self::BigInteger(r)) => Self::Rational(l / Rational::from(r)),

            (Self::BigInteger(l), Self::Real(r)) => {
                Self::Real(f64::rounding_from(l, RoundingMode::Nearest).0 / *r)
            }
            (Self::Real(l), Self::BigInteger(r)) => {
                Self::Real(*l / f64::rounding_from(r, RoundingMode::Nearest).0)
            }

            (Self::Rational(l), Self::Real(r)) => {
                Self::Real(f64::rounding_from(l, RoundingMode::Nearest).0 / *r)
            }
            (Self::Real(l), Self::Rational(r)) => {
                Self::Real(*l / f64::rounding_from(r, RoundingMode::Nearest).0)
            }

            (Self::FixedInteger(l), Self::Real(r)) => Self::Real(*l as f64 / r),
            (Self::Real(l), Self::FixedInteger(r)) => Self::Real(l / *r as f64),

            (Self::FixedInteger(l), Self::Complex(r)) => {
                Self::Complex(Complex64::from_i64(*l).ok_or_else(overflow)? / r)
            }
            (Self::Complex(l), Self::FixedInteger(r)) => {
                Self::Complex(l / Complex64::from_i64(*r).ok_or_else(overflow)?)
            }

            (Self::BigInteger(l), Self::Complex(r)) => {
                Self::Complex(Complex64::from(f64::rounding_from(l, RoundingMode::Nearest).0) / r)
            }
            (Self::Complex(l), Self::BigInteger(r)) => Self::Complex(Complex64::from(
                l / f64::rounding_from(r, RoundingMode::Nearest).0,
            )),

            (Self::Rational(l), Self::Complex(r)) => {
                Self::Complex(Complex64::from(f64::rounding_from(l, RoundingMode::Nearest).0) / r)
            }
            (Self::Complex(l), Self::Rational(r)) => {
                Self::Complex(l / Complex64::from(f64::rounding_from(r, RoundingMode::Nearest).0))
            }

            (Number::Real(l), Number::Complex(r)) => Self::Complex(Complex64::from(l) / r),
            (Number::Complex(l), Number::Real(r)) => Self::Complex(l / r),
        })
    }
}

#[derive(Debug)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => '+',
                Self::Sub => '-',
                Self::Mul => '*',
                Self::Div => '/',
            }
        )
    }
}

#[derive(Debug)]
pub enum ArithmeticError {
    DivisionByZero,
    Overflow(Operation, Number, Number),
    RationalFromPrimitiveFloat(RationalFromPrimitiveFloatError),
}

impl Display for ArithmeticError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::DivisionByZero => write!(f, "division by zero"),
            Self::Overflow(op, l, r) => write!(f, "overflow when calculating ({op} {l} {r})"),
            Self::RationalFromPrimitiveFloat(_) => {
                write!(f, "failed to convert imaginary float to a rational")
            }
        }
    }
}

impl From<RationalFromPrimitiveFloatError> for Box<ArithmeticError> {
    fn from(err: RationalFromPrimitiveFloatError) -> Self {
        Box::new(ArithmeticError::RationalFromPrimitiveFloat(err))
    }
}

#[derive(Debug)]
pub struct NumberToUsizeError {
    number: Number,
    kind: NumberToUsizeErrorKind,
}

impl NumberToUsizeError {
    const fn new(number: Number, kind: NumberToUsizeErrorKind) -> Self {
        Self { number, kind }
    }
}

impl Display for NumberToUsizeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.kind {
            NumberToUsizeErrorKind::NotInteger => write!(
                f,
                "expected integer, got {}",
                match self.number {
                    Number::FixedInteger(_) | Number::BigInteger(_) => "integer",
                    Number::Rational(_) => "rational",
                    Number::Real(_) => "float",
                    Number::Complex(_) => "complex",
                }
            ),
            NumberToUsizeErrorKind::Negative => write!(f, "number `{}` is a negative", self.number),
            NumberToUsizeErrorKind::TooLarge => write!(f, "number `{}` is too large", self.number),
        }
    }
}

#[derive(Debug)]
enum NumberToUsizeErrorKind {
    NotInteger,
    Negative,
    TooLarge,
}

pub(crate) struct ReflexiveNumber(pub(crate) Arc<Number>);

impl PartialEq for ReflexiveNumber {
    fn eq(&self, rhs: &Self) -> bool {
        match (self.0.as_ref(), rhs.0.as_ref()) {
            (Number::FixedInteger(lhs), Number::FixedInteger(rhs)) => lhs == rhs,
            (Number::BigInteger(lhs), Number::BigInteger(rhs)) => lhs == rhs,
            (Number::Rational(lhs), Number::Rational(rhs)) => lhs == rhs,
            (Number::Real(lhs), Number::Real(rhs)) => OrderedFloat(*lhs) == OrderedFloat(*rhs),
            (Number::Complex(lhs), Number::Complex(rhs)) => {
                let Complex { im, re } = *lhs;
                let lhs = Complex::new(OrderedFloat(im), OrderedFloat(re));
                let Complex { im, re } = *rhs;
                let rhs = Complex::new(OrderedFloat(im), OrderedFloat(re));
                lhs == rhs
            }
            _ => false,
        }
    }
}

impl Eq for ReflexiveNumber {}

impl Hash for ReflexiveNumber {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self.0.as_ref()).hash(state);
        match self.0.as_ref() {
            Number::FixedInteger(i) => i.hash(state),
            Number::BigInteger(i) => i.hash(state),
            Number::Rational(r) => r.hash(state),
            Number::Real(r) => OrderedFloat(*r).hash(state),
            Number::Complex(c) => {
                let Complex { im, re } = *c;
                Complex::new(OrderedFloat(im), OrderedFloat(re)).hash(state);
            }
        }
    }
}

#[bridge(name = "zero?", lib = "(rnrs base builtins (6))")]
pub fn zero(arg: &Value) -> Result<Vec<Value>, Condition> {
    let num: Arc<Number> = arg.clone().try_into()?;
    Ok(vec![Value::from(num.is_zero())])
}

#[bridge(name = "even?", lib = "(rnrs base builtins (6))")]
pub fn even(arg: &Value) -> Result<Vec<Value>, Condition> {
    let num: Arc<Number> = arg.clone().try_into()?;
    Ok(vec![Value::from(num.is_even())])
}

#[bridge(name = "odd?", lib = "(rnrs base builtins (6))")]
pub fn odd(arg: &Value) -> Result<Vec<Value>, Condition> {
    let num: Arc<Number> = arg.clone().try_into()?;
    Ok(vec![Value::from(num.is_odd())])
}

#[bridge(name = "+", lib = "(rnrs base builtins (6))")]
pub fn add_builtin(args: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(add(args)?)])
}

pub(crate) fn add(vals: &[Value]) -> Result<Number, Condition> {
    let mut result = Number::FixedInteger(0);
    for val in vals {
        let num: Arc<Number> = val.clone().try_into()?;
        result = result.checked_add(&num)?;
    }
    Ok(result)
}

#[bridge(name = "-", lib = "(rnrs base builtins (6))")]
pub fn sub_builtin(arg1: &Value, args: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(sub(arg1, args)?)])
}

pub(crate) fn sub(val1: &Value, vals: &[Value]) -> Result<Number, Condition> {
    let val1: Arc<Number> = val1.clone().try_into()?;
    let mut val1 = val1.as_ref().clone();
    if vals.is_empty() {
        Ok(-val1)
    } else {
        for val in vals {
            let num: Arc<Number> = val.clone().try_into()?;
            val1 = val1.checked_sub(&num)?;
        }
        Ok(val1)
    }
}

#[bridge(name = "*", lib = "(rnrs base builtins (6))")]
pub fn mul_builtin(args: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(mul(args)?)])
}

pub(crate) fn mul(vals: &[Value]) -> Result<Number, Condition> {
    let mut result = Number::FixedInteger(1);
    for val in vals {
        let num: Arc<Number> = val.clone().try_into()?;
        result = result.checked_mul(&num)?;
    }
    Ok(result)
}

#[bridge(name = "/", lib = "(rnrs base builtins (6))")]
pub fn div_builtin(arg1: &Value, args: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(div(arg1, args)?)])
}

pub(crate) fn div(val1: &Value, vals: &[Value]) -> Result<Number, Condition> {
    let val1: Arc<Number> = val1.clone().try_into()?;
    if vals.is_empty() {
        return Ok(Number::FixedInteger(1).checked_div(&val1)?);
    }
    let mut result = val1.as_ref().clone();
    for val in vals {
        let num: Arc<Number> = val.clone().try_into()?;
        result = result.checked_div(&num)?;
    }
    Ok(result)
}

#[bridge(name = "=", lib = "(rnrs base builtins (6))")]
pub fn equal_builtin(args: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(equal(args)?)])
}

pub(crate) fn equal(vals: &[Value]) -> Result<bool, Condition> {
    if let Some((first, rest)) = vals.split_first() {
        let first: Arc<Number> = first.clone().try_into()?;
        for next in rest {
            let next: Arc<Number> = next.clone().try_into()?;
            if first != next {
                return Ok(false);
            }
        }
    }
    Ok(true)
}

#[bridge(name = ">", lib = "(rnrs base builtins (6))")]
pub fn greater_builtin(args: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(greater(args)?)])
}

pub(crate) fn greater(vals: &[Value]) -> Result<bool, Condition> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev: Arc<Number> = prev.clone().try_into()?;
                let next: Arc<Number> = next.clone().try_into()?;
                // This is somewhat less efficient for small numbers but avoids
                // cloning big ones
                if prev.is_complex() {
                    return Err(Condition::type_error("number", "complex"));
                }
                if next.is_complex() {
                    return Err(Condition::type_error("number", "complex"));
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

#[bridge(name = ">=", lib = "(rnrs base builtins (6))")]
pub fn greater_equal_builtin(args: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(greater_equal(args)?)])
}

pub(crate) fn greater_equal(vals: &[Value]) -> Result<bool, Condition> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev: Arc<Number> = prev.clone().try_into()?;
                let next: Arc<Number> = next.clone().try_into()?;
                if prev.is_complex() {
                    return Err(Condition::type_error("number", "complex"));
                }
                if next.is_complex() {
                    return Err(Condition::type_error("number", "complex"));
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

#[bridge(name = "<", lib = "(rnrs base builtins (6))")]
pub fn lesser_builtin(args: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(lesser(args)?)])
}

pub(crate) fn lesser(vals: &[Value]) -> Result<bool, Condition> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev: Arc<Number> = prev.clone().try_into()?;
                let next: Arc<Number> = next.clone().try_into()?;
                if prev.is_complex() {
                    return Err(Condition::type_error("number", "complex"));
                }
                if next.is_complex() {
                    return Err(Condition::type_error("number", "complex"));
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

#[bridge(name = "<=", lib = "(rnrs base builtins (6))")]
pub fn lesser_equal_builtin(args: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(lesser_equal(args)?)])
}

pub(crate) fn lesser_equal(vals: &[Value]) -> Result<bool, Condition> {
    if let Some((head, rest)) = vals.split_first() {
        let mut prev = head.clone();
        for next in rest {
            {
                let prev: Arc<Number> = prev.clone().try_into()?;
                let next: Arc<Number> = next.clone().try_into()?;
                if prev.is_complex() {
                    return Err(Condition::type_error("number", "complex"));
                }
                if next.is_complex() {
                    return Err(Condition::type_error("number", "complex"));
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

#[bridge(name = "number?", lib = "(rnrs base builtins (6))")]
pub fn is_number(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(arg.type_of() == ValueType::Number)])
}

#[bridge(name = "integer?", lib = "(rnrs base builtins (6))")]
pub fn is_integer(arg: &Value) -> Result<Vec<Value>, Condition> {
    let arg: Arc<Number> = match arg.clone().try_into() {
        Ok(arg) => arg,
        Err(_) => return Ok(vec![Value::from(false)]),
    };
    Ok(vec![Value::from(matches!(
        arg.as_ref(),
        Number::FixedInteger(_) | Number::BigInteger(_)
    ))])
}

#[bridge(name = "rational?", lib = "(rnrs base builtins (6))")]
pub fn is_rational(arg: &Value) -> Result<Vec<Value>, Condition> {
    let arg: Arc<Number> = match arg.clone().try_into() {
        Ok(arg) => arg,
        Err(_) => return Ok(vec![Value::from(false)]),
    };
    Ok(vec![Value::from(matches!(
        arg.as_ref(),
        Number::Rational(_)
    ))])
}

#[bridge(name = "real?", lib = "(rnrs base builtins (6))")]
pub fn is_real(arg: &Value) -> Result<Vec<Value>, Condition> {
    let arg: Arc<Number> = match arg.clone().try_into() {
        Ok(arg) => arg,
        Err(_) => return Ok(vec![Value::from(false)]),
    };
    Ok(vec![Value::from(matches!(arg.as_ref(), Number::Real(_)))])
}

#[bridge(name = "complex?", lib = "(rnrs base builtins (6))")]
pub fn is_complex(arg: &Value) -> Result<Vec<Value>, Condition> {
    let arg: Arc<Number> = match arg.clone().try_into() {
        Ok(arg) => arg,
        Err(_) => return Ok(vec![Value::from(false)]),
    };
    Ok(vec![Value::from(matches!(
        arg.as_ref(),
        Number::Complex(_)
    ))])
}
