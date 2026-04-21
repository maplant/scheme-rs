//! Scheme enumerations and enumeration sets.

use std::{collections::HashSet, sync::Arc};

use indexmap::IndexSet;
use scheme_rs_macros::{bridge, cps_bridge};

use crate::{
    exceptions::Exception,
    gc::{Gc, Trace},
    lists::List,
    proc::{Application, ContBarrier, FuncPtr, Procedure},
    records::{RecordTypeDescriptor, SchemeCompatible, rtd},
    runtime::Runtime,
    symbols::Symbol,
    value::Value,
};

#[derive(Trace, Debug)]
pub struct EnumerationType {
    symbols: IndexSet<Symbol>,
}

impl EnumerationType {
    pub fn new(symbols: impl IntoIterator<Item = Symbol>) -> Self {
        Self {
            symbols: symbols.into_iter().collect(),
        }
    }
}

impl SchemeCompatible for EnumerationType {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "enum-universe", sealed: true, opaque: true)
    }
}

#[derive(Trace, Debug)]
pub struct EnumerationSet {
    enum_type: Gc<EnumerationType>,
    set: IndexSet<Symbol>,
}

impl EnumerationSet {
    pub fn new(enum_type: &Gc<EnumerationType>, set: impl IntoIterator<Item = Symbol>) -> Self {
        Self {
            enum_type: enum_type.clone(),
            set: set.into_iter().collect(),
        }
    }

    pub fn type_check(&self, ty: &Gc<EnumerationType>) -> Result<(), Exception> {
        if !Gc::ptr_eq(&self.enum_type, ty) {
            Err(Exception::error("wrong enumeration type"))
        } else {
            Ok(())
        }
    }

    /// Checks for membership in the set
    pub fn contains(&self, sym: &str) -> bool {
        self.set.contains(&Symbol::intern(sym))
    }
}

impl SchemeCompatible for EnumerationSet {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "enum-set", sealed: true, opaque: true)
    }
}

#[bridge(name = "make-enumeration", lib = "(rnrs enums (6))")]
pub fn make_enumeration(symbols: List) -> Result<Vec<Value>, Exception> {
    let symbols = symbols
        .into_iter()
        .map(|item| item.try_to_scheme_type())
        .collect::<Result<IndexSet<Symbol>, Exception>>()?;
    let set = EnumerationSet {
        set: symbols.clone(),
        enum_type: Gc::new(EnumerationType { symbols }),
    };
    Ok(vec![Value::from_rust_type(set)])
}

#[bridge(name = "enum-set-universe", lib = "(rnrs enums (6))")]
pub fn enum_set_universe(enum_set: &Value) -> Result<Vec<Value>, Exception> {
    let enum_set: Gc<EnumerationSet> = enum_set.try_to_rust_type()?;
    let new_set = EnumerationSet {
        enum_type: Gc::new(EnumerationType {
            symbols: enum_set.enum_type.symbols.clone(),
        }),
        set: enum_set.enum_type.symbols.clone(),
    };
    Ok(vec![Value::from_rust_type(new_set)])
}

#[cps_bridge(def = "enum-set-constructor enum-set", lib = "(rnrs enums (6))")]
pub fn enum_set_constructor(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
    k: Value,
) -> Result<Application, Exception> {
    args[0].try_to_rust_type::<EnumerationSet>()?;
    let constructor = Procedure::new(
        runtime.clone(),
        vec![args[0].clone()],
        FuncPtr::Bridge(enum_set_constructor_fn),
        1,
        false,
    );
    Ok(Application::new(
        k.try_into()?,
        vec![Value::from(constructor)],
    ))
}

#[cps_bridge]
fn enum_set_constructor_fn(
    _runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _barrier: &mut ContBarrier,
    k: Value,
) -> Result<Application, Exception> {
    // env[0] is the universe:
    let enum_type: Gc<EnumerationType> = env[0].try_to_rust_type()?;
    let set = args[0]
        .try_to_scheme_type::<List>()?
        .into_iter()
        .map(|symbol| {
            let symbol = symbol.try_to_scheme_type::<Symbol>()?;
            if !enum_type.symbols.contains(&symbol) {
                Err(Exception::error(format!(
                    "universe does not contain {symbol}"
                )))
            } else {
                Ok(symbol)
            }
        })
        .collect::<Result<IndexSet<_>, _>>()?;
    let enum_set = EnumerationSet { enum_type, set };
    Ok(Application::new(
        k.try_into()?,
        vec![Value::from_rust_type(enum_set)],
    ))
}
