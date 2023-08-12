use rug::{Complete, Complex, Float, Integer, Rational};

pub enum Number {
    Integer(Integer),
    Rational(Rational),
    Real(Float),
    Complex(Complex),
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

pub fn add(l: &Number, r: &Number) -> Number {
    match (l, r) {
        (Number::Integer(l), Number::Integer(r)) => Number::from((l + r).complete()),
        _ => todo!(),
    }
}
