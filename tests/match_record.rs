mod common;

use scheme_rs::{Result, gc::Trace, records::Embeddable, value::Value};

#[derive(Embeddable, Trace)]
#[scheme(lib = "(test)", constructor = |x, y| Vec2::new(x, y))]
pub struct Vec2 {
    #[immutable]
    x: f64,
    #[immutable]
    y: f64,
}

impl Vec2 {
    fn new(x: Value, y: Value) -> Result<Self> {
        Ok(Self {
            x: x.try_into()?,
            y: y.try_into()?,
        })
    }
}

#[derive(Embeddable, Trace)]
#[scheme(lib = "(test)", constructor = |x, y, z| Vec3::new(x, y, z))]
pub struct Vec3 {
    #[parent]
    xy: Vec2,
    #[immutable]
    z: f64,
}

impl Vec3 {
    fn new(x: Value, y: Value, z: Value) -> Result<Self> {
        Ok(Self {
            xy: Vec2::new(x, y)?,
            z: z.try_into()?,
        })
    }
}

common::run_test!(match_record);
