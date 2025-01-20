//! A Registry is a collection of libraries.

use std::collections::HashMap;

use crate::env::Library;

#[derive(Clone, PartialEq, Eq, Hash)]
struct LibraryName {
    idents: Vec<String>,
}

struct Version {
    version: Vec<u32>,
}

struct Registry {
    libs: HashMap<LibraryName, Library>,
}


