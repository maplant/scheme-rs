//! Scheme enumerations and enumeration sets.

use std::{fmt, sync::Arc};

use indexmap::IndexSet;
use scheme_rs_macros::{bridge, cps_bridge};

use crate::{
    exceptions::Exception,
    gc::Trace,
    lists::List,
    proc::{Application, ContBarrier, FuncPtr, Procedure},
    records::{Embeddable, Embedded, RecordTypeDescriptor, rtd},
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

unsafe impl Embeddable for EnumerationType {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(ty: EnumerationType, name: "enum-universe", sealed: true, opaque: true)
    }
}

#[derive(Trace)]
pub struct EnumerationSet {
    enum_type: Embedded<EnumerationType>,
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
    pub fn new(
        enum_type: &Embedded<EnumerationType>,
        set: impl IntoIterator<Item = Symbol>,
    ) -> Self {
        Self {
            enum_type: enum_type.clone(),
            set: set.into_iter().collect(),
        }
    }

    pub fn type_check(&self, ty: &Embedded<EnumerationType>) -> Result<(), Exception> {
        if !Embedded::ptr_eq(&self.enum_type, ty) {
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

unsafe impl Embeddable for EnumerationSet {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(ty: EnumerationSet, name: "enum-set", sealed: true, opaque: true)
    }
}

#[bridge(name = "make-enumeration", lib = "(rnrs enums (6))")]
pub fn make_enumeration(symbols: List) -> Result<Vec<Value>, Exception> {
    let symbols = symbols
        .into_iter()
        .map(|item| item.try_to())
        .collect::<Result<IndexSet<Symbol>, Exception>>()?;
    let set = EnumerationSet {
        set: symbols.clone(),
        enum_type: Embedded::new(EnumerationType { symbols }),
    };
    Ok(vec![Value::from(set)])
}

#[bridge(name = "enum-set-universe", lib = "(rnrs enums (6))")]
pub fn enum_set_universe(enum_set: Embedded<EnumerationSet>) -> Result<Vec<Value>, Exception> {
    let new_set = EnumerationSet {
        enum_type: Embedded::new(EnumerationType {
            symbols: enum_set.enum_type.symbols.clone(),
        }),
        set: enum_set.enum_type.symbols.clone(),
    };
    Ok(vec![Value::from(new_set)])
}

#[cps_bridge(def = "enum-set-constructor enum-set", lib = "(rnrs enums (6))")]
pub fn enum_set_constructor(
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let set = args[0].try_to::<Embedded<EnumerationSet>>()?;
    let universe = Value::from(set.enum_type.clone());
    let constructor = Procedure::new(
        vec![universe],
        FuncPtr::Bridge(enum_set_constructor_fn),
        1,
        false,
    );
    Ok(barrier.call_cont(vec![Value::from(constructor)]))
}

#[cps_bridge]
fn enum_set_constructor_fn(
    env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    // env[0] is the universe:
    let enum_type: Embedded<EnumerationType> = env[0].try_to()?;
    let set = args[0]
        .try_to::<List>()?
        .into_iter()
        .map(|symbol| {
            let symbol = symbol.try_to::<Symbol>()?;
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
    Ok(barrier.call_cont(vec![Value::from(enum_set)]))
}

#[bridge(name = "enum-set->list", lib = "(rnrs enums (6))")]
pub fn enum_set_to_list(enum_set: Embedded<EnumerationSet>) -> Result<Vec<Value>, Exception> {
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
pub fn enum_set_member_pred(
    symbol: Symbol,
    enum_set: Embedded<EnumerationSet>,
) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(enum_set.set.contains(&symbol))])
}

#[bridge(name = "enum-set-subset?", lib = "(rnrs enums (6))")]
pub fn enum_set_subset_pred(
    enum_set1: Embedded<EnumerationSet>,
    enum_set2: Embedded<EnumerationSet>,
) -> Result<Vec<Value>, Exception> {
    let is_subset = enum_set1
        .enum_type
        .symbols
        .is_subset(&enum_set2.enum_type.symbols)
        && enum_set1.set.is_subset(&enum_set2.set);
    Ok(vec![Value::from(is_subset)])
}

#[bridge(name = "enum-set=?", lib = "(rnrs enums (6))")]
pub fn enum_set_equal(
    enum_set1: Embedded<EnumerationSet>,
    enum_set2: Embedded<EnumerationSet>,
) -> Result<Vec<Value>, Exception> {
    let is_equal = enum_set1.enum_type.symbols == enum_set2.enum_type.symbols
        && enum_set1.set == enum_set2.set;
    Ok(vec![Value::from(is_equal)])
}

#[bridge(name = "enum-set-union", lib = "(rnrs enums (6))")]
pub fn enum_set_union(
    enum_set1: Embedded<EnumerationSet>,
    enum_set2: Embedded<EnumerationSet>,
) -> Result<Vec<Value>, Exception> {
    if !Embedded::ptr_eq(&enum_set1.enum_type, &enum_set2.enum_type) {
        return Err(Exception::error("enum sets must be of the same enum type"));
    }
    let union = enum_set1
        .set
        .union(&enum_set2.set)
        .copied()
        .collect::<IndexSet<_>>();
    let set = Value::from(EnumerationSet {
        enum_type: enum_set1.enum_type.clone(),
        set: union,
    });
    Ok(vec![set])
}

#[bridge(name = "enum-set-intersection", lib = "(rnrs enums (6))")]
pub fn enum_set_intersection(
    enum_set1: Embedded<EnumerationSet>,
    enum_set2: Embedded<EnumerationSet>,
) -> Result<Vec<Value>, Exception> {
    if !Embedded::ptr_eq(&enum_set1.enum_type, &enum_set2.enum_type) {
        return Err(Exception::error("enum sets must be of the same enum type"));
    }
    let intersection = enum_set1
        .set
        .intersection(&enum_set2.set)
        .copied()
        .collect::<IndexSet<_>>();
    let set = Value::from(EnumerationSet {
        enum_type: enum_set1.enum_type.clone(),
        set: intersection,
    });
    Ok(vec![set])
}

#[bridge(name = "enum-set-difference", lib = "(rnrs enums (6))")]
pub fn enum_set_difference(
    enum_set1: Embedded<EnumerationSet>,
    enum_set2: Embedded<EnumerationSet>,
) -> Result<Vec<Value>, Exception> {
    if !Embedded::ptr_eq(&enum_set1.enum_type, &enum_set2.enum_type) {
        return Err(Exception::error("enum sets must be of the same enum type"));
    }
    let difference = enum_set1
        .set
        .difference(&enum_set2.set)
        .copied()
        .collect::<IndexSet<_>>();
    let set = Value::from(EnumerationSet {
        enum_type: enum_set1.enum_type.clone(),
        set: difference,
    });
    Ok(vec![set])
}

#[bridge(name = "enum-set-complement", lib = "(rnrs enums (6))")]
pub fn enum_set_complement(enum_set: Embedded<EnumerationSet>) -> Result<Vec<Value>, Exception> {
    let complement = enum_set
        .enum_type
        .symbols
        .difference(&enum_set.set)
        .copied()
        .collect::<IndexSet<_>>();
    let set = Value::from(EnumerationSet {
        enum_type: enum_set.enum_type.clone(),
        set: complement,
    });
    Ok(vec![set])
}

#[bridge(name = "enum-set-projection", lib = "(rnrs enums (6))")]
pub fn enum_set_projection(
    enum_set1: Embedded<EnumerationSet>,
    enum_set2: Embedded<EnumerationSet>,
) -> Result<Vec<Value>, Exception> {
    let projection = enum_set1
        .set
        .iter()
        .filter(|sym| enum_set2.enum_type.symbols.contains(*sym))
        .copied()
        .collect::<IndexSet<_>>();
    let set = Value::from(EnumerationSet {
        enum_type: enum_set2.enum_type.clone(),
        set: projection,
    });
    Ok(vec![set])
}
