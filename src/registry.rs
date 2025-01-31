//! A Registry is a collection of libraries.

use std::{collections::HashMap, str::FromStr};

use crate::{
    env::{Library, Top},
    gc::Gc,
    proc::{AsyncFuncPtr, Closure, FuncPtr},
    runtime::Runtime,
    value::Value,
};
pub use proc_macros::bridge;

#[derive(Clone, Defuault, PartialEq, Eq, Hash)]
pub struct LibraryName {
    idents: Vec<String>,
    version: Version,
}

impl FromStr for LibraryName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // FIXME:
        Ok(Self::default())
    }
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Default)]
pub struct Version {
    version: Vec<u32>,
}

impl FromStr for Version {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        todo!()
    }
}

enum VersionReference {
    SubVersions(Vec<SubVersionReference>),
    And(Vec<VersionReference>),
    Or(Vec<VersionReference>),
    Not(Box<VersionReference>),
}

enum SubVersionReference {
    SubVersion(u32),
    Gte(Vec<SubVersionReference>),
    Lte(Vec<SubVersionReference>),
    And(Vec<SubVersionReference>),
    Or(Vec<SubVersionReference>),
    Not(Box<SubVersionReference>),
}

pub struct BridgeFn {
    name: &'static str,
    lib_name: &'static str,
    num_args: usize,
    variadic: bool,
    wrapper: AsyncFuncPtr,
}

impl BridgeFn {
    pub const fn new(
        name: &'static str,
        lib_name: &'static str,
        num_args: usize,
        variadic: bool,
        wrapper: AsyncFuncPtr,
    ) -> Self {
        Self {
            name,
            lib_name,
            num_args,
            variadic,
            wrapper,
        }
    }
}

inventory::collect!(BridgeFn);

pub struct Registry {
    libs: HashMap<LibraryName, Gc<Library>>,
}

impl Registry {
    /// Construct a Registry with all of the available bridge functions present but no external libraries imported.
    pub fn new(runtime: &Gc<Runtime>) -> Self {
        let mut libs = HashMap::<LibraryName, Gc<Library>>::default();

        for bridge_fn in inventory::iter::<BridgeFn>() {
            let lib_name = bridge_fn.lib_name.parse::<LibraryName>().unwrap();
            let lib = libs
                .entry(lib_name)
                .or_insert_with(|| Gc::new(Library::default()));
            let mut lib = lib.write();
            lib.def_var(bridge_fn.name.to_string());
            lib.set_var(
                bridge_fn.name,
                Value::Closure(Gc::new(Closure::new(
                    runtime.clone(),
                    Vec::new(),
                    Vec::new(),
                    FuncPtr::AsyncFunc(bridge_fn.wrapper),
                    bridge_fn.num_args,
                    bridge_fn.variadic,
                ))),
            );
        }

        Self { libs }
    }
}

/*
impl Registry {
    pub fn import(&self,
}
*/
