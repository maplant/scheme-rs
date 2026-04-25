//! Scheme enumerations and enumeration sets.

use std::{fmt, sync::Arc};

use indexmap::IndexSet;
use scheme_rs_macros::{bridge, cps_bridge};

use crate::{
    exceptions::Exception,
    gc::{Gc, Trace},
    lists::List,
    proc::{Application, ContBarrier, FuncPtr, Procedure},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
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

#[derive(Trace)]
pub struct EnumerationSet {
    enum_type: Gc<EnumerationType>,
    set: IndexSet<Symbol>,
}

impl fmt::Debug for EnumerationSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for sym in &self.set {
            write!(f, " {sym}")?;
        }
        Ok(())
    }
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
    let set = args[0].try_to_rust_type::<EnumerationSet>()?;
    let universe = Value::from(Record::from_rust_gc_type(set.enum_type.clone()));
    let constructor = Procedure::new(
        runtime.clone(),
        vec![universe],
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

#[bridge(name = "enum-set->list", lib = "(rnrs enums (6))")]
pub fn enum_set_to_list(enum_set: &Value) -> Result<Vec<Value>, Exception> {
    let enum_set: Gc<EnumerationSet> = enum_set.try_to_rust_type()?;
    let mut set = enum_set
        .set
        .iter()
        .map(|symbol| {
            let idx = enum_set.enum_type.symbols.get_index_of(symbol).unwrap();
            (idx, *symbol)
        })
        .collect::<Vec<_>>();
    set.sort_by_key(|(idx, _)| *idx);
    let list = set.into_iter().map(|(_, sym)| sym).collect::<List>();
    Ok(vec![Value::from(list)])
}

#[bridge(name = "enum-set-member?", lib = "(rnrs enums (6))")]
pub fn enum_set_member_pred(symbol: Symbol, enum_set: &Value) -> Result<Vec<Value>, Exception> {
    let enum_set: Gc<EnumerationSet> = enum_set.try_to_rust_type()?;
    Ok(vec![Value::from(enum_set.set.contains(&symbol))])
}

#[bridge(name = "enum-set-subset?", lib = "(rnrs enums (6))")]
pub fn enum_set_subset_pred(enum_set1: &Value, enum_set2: &Value) -> Result<Vec<Value>, Exception> {
    let enum_set1: Gc<EnumerationSet> = enum_set1.try_to_rust_type()?;
    let enum_set2: Gc<EnumerationSet> = enum_set2.try_to_rust_type()?;
    let is_subset = enum_set1
        .enum_type
        .symbols
        .is_subset(&enum_set2.enum_type.symbols)
        && enum_set1.set.is_subset(&enum_set2.set);
    Ok(vec![Value::from(is_subset)])
}

#[bridge(name = "enum-set=?", lib = "(rnrs enums (6))")]
pub fn enum_set_equal(enum_set1: &Value, enum_set2: &Value) -> Result<Vec<Value>, Exception> {
    let enum_set1: Gc<EnumerationSet> = enum_set1.try_to_rust_type()?;
    let enum_set2: Gc<EnumerationSet> = enum_set2.try_to_rust_type()?;
    let is_equal = enum_set1.enum_type.symbols == enum_set2.enum_type.symbols
        && enum_set1.set == enum_set2.set;
    Ok(vec![Value::from(is_equal)])
}

#[bridge(name = "enum-set-union", lib = "(rnrs enums (6))")]
pub fn enum_set_union(enum_set1: &Value, enum_set2: &Value) -> Result<Vec<Value>, Exception> {
    let enum_set1: Gc<EnumerationSet> = enum_set1.try_to_rust_type()?;
    let enum_set2: Gc<EnumerationSet> = enum_set2.try_to_rust_type()?;
    if !Gc::ptr_eq(&enum_set1.enum_type, &enum_set2.enum_type) {
        return Err(Exception::error("enum sets must be of the same enum type"));
    }
    let union = enum_set1
        .set
        .union(&enum_set2.set)
        .copied()
        .collect::<IndexSet<_>>();
    let set = Value::from_rust_type(EnumerationSet {
        enum_type: enum_set1.enum_type.clone(),
        set: union,
    });
    Ok(vec![set])
}

#[bridge(name = "enum-set-intersection", lib = "(rnrs enums (6))")]
pub fn enum_set_intersection(
    enum_set1: &Value,
    enum_set2: &Value,
) -> Result<Vec<Value>, Exception> {
    let enum_set1: Gc<EnumerationSet> = enum_set1.try_to_rust_type()?;
    let enum_set2: Gc<EnumerationSet> = enum_set2.try_to_rust_type()?;
    if !Gc::ptr_eq(&enum_set1.enum_type, &enum_set2.enum_type) {
        return Err(Exception::error("enum sets must be of the same enum type"));
    }
    let intersection = enum_set1
        .set
        .intersection(&enum_set2.set)
        .copied()
        .collect::<IndexSet<_>>();
    let set = Value::from_rust_type(EnumerationSet {
        enum_type: enum_set1.enum_type.clone(),
        set: intersection,
    });
    Ok(vec![set])
}

#[bridge(name = "enum-set-difference", lib = "(rnrs enums (6))")]
pub fn enum_set_difference(enum_set1: &Value, enum_set2: &Value) -> Result<Vec<Value>, Exception> {
    let enum_set1: Gc<EnumerationSet> = enum_set1.try_to_rust_type()?;
    let enum_set2: Gc<EnumerationSet> = enum_set2.try_to_rust_type()?;
    if !Gc::ptr_eq(&enum_set1.enum_type, &enum_set2.enum_type) {
        return Err(Exception::error("enum sets must be of the same enum type"));
    }
    let difference = enum_set1
        .set
        .difference(&enum_set2.set)
        .copied()
        .collect::<IndexSet<_>>();
    let set = Value::from_rust_type(EnumerationSet {
        enum_type: enum_set1.enum_type.clone(),
        set: difference,
    });
    Ok(vec![set])
}

#[bridge(name = "enum-set-complement", lib = "(rnrs enums (6))")]
pub fn enum_set_complement(enum_set: &Value) -> Result<Vec<Value>, Exception> {
    let enum_set: Gc<EnumerationSet> = enum_set.try_to_rust_type()?;
    let complement = enum_set
        .enum_type
        .symbols
        .difference(&enum_set.set)
        .copied()
        .collect::<IndexSet<_>>();
    let set = Value::from_rust_type(EnumerationSet {
        enum_type: enum_set.enum_type.clone(),
        set: complement,
    });
    Ok(vec![set])
}

#[bridge(name = "enum-set-projection", lib = "(rnrs enums (6))")]
pub fn enum_set_projection(enum_set1: &Value, enum_set2: &Value) -> Result<Vec<Value>, Exception> {
    let enum_set1: Gc<EnumerationSet> = enum_set1.try_to_rust_type()?;
    let enum_set2: Gc<EnumerationSet> = enum_set2.try_to_rust_type()?;
    let projection = enum_set1
        .set
        .iter()
        .filter(|sym| enum_set2.enum_type.symbols.contains(*sym))
        .copied()
        .collect::<IndexSet<_>>();
    let set = Value::from_rust_type(EnumerationSet {
        enum_type: enum_set2.enum_type.clone(),
        set: projection,
    });
    Ok(vec![set])
}
