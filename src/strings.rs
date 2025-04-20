//! String builtins and data types

use std::{
    fmt,
    ops::{Deref, DerefMut},
};

#[repr(align(16))]
pub struct AlignedString(pub String);

impl AlignedString {
    pub fn new(str: String) -> Self {
        AlignedString(str)
    }
}

impl Deref for AlignedString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for AlignedString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialEq<str> for AlignedString {
    fn eq(&self, rhs: &str) -> bool {
        self.0 == rhs
    }
}

impl PartialEq for AlignedString {
    fn eq(&self, rhs: &Self) -> bool {
        self.0 == rhs.0
    }
}

impl fmt::Display for AlignedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for AlignedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
