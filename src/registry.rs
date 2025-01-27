//! A Registry is a collection of libraries.

use std::collections::HashMap;

use crate::env::Library;

#[derive(Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub struct LibraryName {
    idents: Vec<String>,
}

#[allow(dead_code)]
pub struct Version {
    version: Vec<u32>,
}

#[allow(dead_code)]
pub struct Registry {
    libs: HashMap<LibraryName, Library>,
}
