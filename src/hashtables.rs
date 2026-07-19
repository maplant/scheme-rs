//! Scheme compatible hashtables

use indexmap::IndexSet;
use parking_lot::RwLock;
use scheme_rs_macros::rtd;
use std::{
    collections::HashSet,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    sync::Arc,
};

use crate::{
    exceptions::Exception,
    gc::Trace,
    proc::{Application, ContBarrier, Procedure},
    records::{Embeddable, Embedded, RecordTypeDescriptor},
    registry::{bridge, cps_bridge},
    runtime::Runtime,
    strings::WideString,
    symbols::Symbol,
    value::{Expect1, Value},
};

#[derive(Clone, Trace)]
struct TableEntry {
    key: Value,
    val: Value,
    hash: u64,
}

impl TableEntry {
    fn get_hash(&self) -> u64 {
        self.hash
    }
}

#[derive(Trace)]
pub(crate) struct HashTableInner {
    /// Inner table of values. This uses an inner RwLock to ensure that we can
    /// access eq and hash even if the table is locked.
    ///
    /// We can't use the std library hashmap since we don't want to bundle the
    /// eq and hash functions with the key Value, so we use hashbrown's
    /// HashTable
    table: RwLock<hashbrown::HashTable<TableEntry>>,
    /// Equivalence function.
    eq: Procedure,
    /// Hash function.
    hash: Procedure,
    /// Whether or not the hashtable is mutable
    mutable: bool,
}

unsafe impl Embeddable for HashTableInner {
    fn rtd() -> Arc<RecordTypeDescriptor>
    where
        Self: Sized,
    {
        rtd!(ty: HashTableInner, name: "%hash-table", sealed: true, opaque: true)
    }
}

impl HashTableInner {
    pub fn size(&self) -> usize {
        self.table.read().len()
    }

    #[cfg(not(feature = "async"))]
    pub fn hash(&self, val: Value, barrier: &mut ContBarrier<'_>) -> Result<u64, Exception> {
        self.hash.call(&[val], barrier)?.expect1()
    }

    #[cfg(feature = "async")]
    pub fn hash(&self, val: Value, barrier: &mut ContBarrier<'_>) -> Result<u64, Exception> {
        self.hash.call_sync(&[val], barrier)?.expect1()
    }

    #[cfg(not(feature = "async"))]
    pub fn eq(
        &self,
        lhs: Value,
        rhs: Value,
        barrier: &mut ContBarrier<'_>,
    ) -> Result<bool, Exception> {
        self.eq.call(&[lhs, rhs], barrier)?.expect1()
    }

    #[cfg(feature = "async")]
    pub fn eq(
        &self,
        lhs: Value,
        rhs: Value,
        barrier: &mut ContBarrier<'_>,
    ) -> Result<bool, Exception> {
        self.eq.call_sync(&[lhs, rhs], barrier)?.expect1()
    }

    /// Equivalent to `hashtable-ref`
    pub fn get(
        &self,
        key: &Value,
        default: &Value,
        barrier: &mut ContBarrier<'_>,
    ) -> Result<Value, Exception> {
        let table = self.table.read();
        let hash = self.hash(key.clone(), barrier)?;
        for entry in table.iter_hash(hash) {
            if entry.hash == hash && self.eq(key.clone(), entry.key.clone(), barrier)? {
                return Ok(entry.val.clone());
            }
        }
        Ok(default.clone())
    }

    pub fn set(
        &self,
        key: &Value,
        val: &Value,
        barrier: &mut ContBarrier<'_>,
    ) -> Result<(), Exception> {
        if !self.mutable {
            return Err(Exception::error("hashtable is immutable"));
        }

        let mut table = self.table.write();
        let hash = self.hash(key.clone(), barrier)?;
        for entry in table.iter_hash_mut(hash) {
            if entry.hash == hash && self.eq(key.clone(), entry.key.clone(), barrier)? {
                entry.val = val.clone();
                return Ok(());
            }
        }

        // Insert the new entry, guaranteed to be unique.
        table.insert_unique(
            hash,
            TableEntry {
                key: key.clone(),
                val: val.clone(),
                hash,
            },
            TableEntry::get_hash,
        );

        Ok(())
    }

    pub fn delete(&self, key: &Value, barrier: &mut ContBarrier<'_>) -> Result<(), Exception> {
        if !self.mutable {
            return Err(Exception::error("hashtable is immutable"));
        }

        let mut table = self.table.write();
        let hash = self.hash(key.clone(), barrier)?;
        let buckets = table.iter_hash_buckets(hash).collect::<Vec<_>>();
        for bucket in buckets.into_iter() {
            if let Ok(entry) = table.get_bucket_entry(bucket)
                && let inner = entry.get()
                && inner.hash == hash
                && self.eq(key.clone(), inner.key.clone(), barrier)?
            {
                entry.remove();
                return Ok(());
            }
        }

        Ok(())
    }

    pub fn contains(&self, key: &Value, barrier: &mut ContBarrier<'_>) -> Result<bool, Exception> {
        let table = self.table.write();
        let hash = self.hash(key.clone(), barrier)?;
        for entry in table.iter_hash(hash) {
            if entry.hash == hash && self.eq(key.clone(), entry.key.clone(), barrier)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    pub fn update(
        &self,
        key: &Value,
        proc: &Procedure,
        default: &Value,
        barrier: &mut ContBarrier<'_>,
    ) -> Result<(), Exception> {
        use std::slice;

        if !self.mutable {
            return Err(Exception::error("hashtable is immutable"));
        }

        let mut table = self.table.write();
        let hash = self.hash(key.clone(), barrier)?;
        for entry in table.iter_hash_mut(hash) {
            if entry.hash == hash && self.eq(key.clone(), entry.key.clone(), barrier)? {
                #[cfg(not(feature = "async"))]
                let updated = proc.call(slice::from_ref(&entry.val), barrier)?[0].clone();

                #[cfg(feature = "async")]
                let updated = proc.call_sync(slice::from_ref(&entry.val), barrier)?[0].clone();

                entry.val = updated;
                return Ok(());
            }
        }

        #[cfg(not(feature = "async"))]
        let updated = proc.call(slice::from_ref(default), barrier)?[0].clone();

        #[cfg(feature = "async")]
        let updated = proc.call_sync(slice::from_ref(default), barrier)?[0].clone();

        table.insert_unique(
            hash,
            TableEntry {
                key: key.clone(),
                val: updated,
                hash,
            },
            TableEntry::get_hash,
        );

        Ok(())
    }

    pub fn copy(&self, mutable: bool) -> Self {
        Self {
            table: RwLock::new(self.table.read().clone()),
            eq: self.eq.clone(),
            hash: self.hash.clone(),
            mutable,
        }
    }

    pub fn clear(&self) -> Result<(), Exception> {
        if !self.mutable {
            return Err(Exception::error("hashtable is immutable"));
        }

        self.table.write().clear();

        Ok(())
    }

    pub fn keys(&self) -> Vec<Value> {
        self.table
            .read()
            .iter()
            .map(|entry| entry.key.clone())
            .collect()
    }

    pub fn entries(&self) -> (Vec<Value>, Vec<Value>) {
        self.table
            .read()
            .iter()
            .map(|entry| (entry.key.clone(), entry.val.clone()))
            .unzip()
    }
}

#[derive(Clone, Trace, Debug)]
pub struct HashTable(pub(crate) Embedded<HashTableInner>);

impl HashTable {
    /*
    pub fn new_eq() -> Self {
        todo!()
    }

    pub fn new_eqv() -> Self {
        todo!()
    }

    pub fn new_equal() -> Self {
        todo!()
    }
    */

    pub fn new(hash: Procedure, eq: Procedure) -> Self {
        Self(Embedded::new(HashTableInner {
            table: RwLock::new(hashbrown::HashTable::new()),
            eq,
            hash,
            mutable: true,
        }))
    }

    pub fn with_capacity(hash: Procedure, eq: Procedure, cap: usize) -> Self {
        Self(Embedded::new(HashTableInner {
            table: RwLock::new(hashbrown::HashTable::with_capacity(cap)),
            eq,
            hash,
            mutable: true,
        }))
    }

    pub fn size(&self) -> usize {
        self.0.size()
    }

    pub fn get(
        &self,
        key: &Value,
        default: &Value,
        barrier: &mut ContBarrier<'_>,
    ) -> Result<Value, Exception> {
        self.0.get(key, default, barrier)
    }

    pub fn set(
        &self,
        key: &Value,
        val: &Value,
        barrier: &mut ContBarrier<'_>,
    ) -> Result<(), Exception> {
        self.0.set(key, val, barrier)
    }

    pub fn delete(&self, key: &Value, barrier: &mut ContBarrier<'_>) -> Result<(), Exception> {
        self.0.delete(key, barrier)
    }

    pub fn contains(&self, key: &Value, barrier: &mut ContBarrier<'_>) -> Result<bool, Exception> {
        self.0.contains(key, barrier)
    }

    pub fn update(
        &self,
        key: &Value,
        proc: &Procedure,
        default: &Value,
        barrier: &mut ContBarrier<'_>,
    ) -> Result<(), Exception> {
        self.0.update(key, proc, default, barrier)
    }

    pub fn copy(&self, mutable: bool) -> Self {
        Self(Embedded::new(self.0.copy(mutable)))
    }

    pub fn clear(&self) -> Result<(), Exception> {
        self.0.clear()
    }

    pub fn keys(&self) -> Vec<Value> {
        self.0.keys()
    }

    pub fn entries(&self) -> (Vec<Value>, Vec<Value>) {
        self.0.entries()
    }
}

impl fmt::Debug for HashTableInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#hash(")?;
        for (i, entry) in self.table.read().iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "({:?} . {:?})", entry.key, entry.val)?;
        }
        write!(f, ")")
    }
}

#[derive(Default, Trace)]
pub struct EqualHashSet {
    set: HashSet<EqualValue>,
}

impl EqualHashSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, new_value: Value) {
        let new_value = EqualValue(new_value);
        if !self.set.contains(&new_value) {
            self.set.insert(new_value);
        }
    }

    pub fn get(&mut self, val: &Value) -> &Value {
        let val = EqualValue(val.clone());
        &self.set.get(&val).unwrap().0
    }
}

#[derive(Clone, Eq, Trace)]
pub struct EqualValue(pub Value);

impl PartialEq for EqualValue {
    fn eq(&self, rhs: &Self) -> bool {
        self.0.equal(&rhs.0)
    }
}

impl Hash for EqualValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.equal_hash(&mut IndexSet::new(), state)
    }
}

impl From<HashTable> for Value {
    fn from(value: HashTable) -> Self {
        Value::from(value.0)
    }
}

impl TryFrom<Value> for HashTable {
    type Error = Exception;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Ok(Self((&value).try_into()?))
    }
}

impl From<&Value> for Option<HashTable> {
    fn from(value: &Value) -> Self {
        Some(HashTable(value.cast()?))
    }
}

impl TryFrom<&Value> for HashTable {
    type Error = Exception;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        Ok(Self(value.try_into()?))
    }
}

#[bridge(name = "make-hashtable", lib = "(rnrs hashtables builtins (6))")]
pub fn make_hashtable(
    hash_function: &Value,
    equiv: &Value,
    rest: &[Value],
) -> Result<Vec<Value>, Exception> {
    let hash: Procedure = hash_function.clone().try_into()?;
    let equiv: Procedure = equiv.clone().try_into()?;
    let k = match rest {
        [] => None,
        [k] => Some(k.try_into()?),
        x => return Err(Exception::wrong_num_of_args(3, 2 + x.len())),
    };
    let hashtable = if let Some(k) = k {
        HashTable::with_capacity(hash, equiv, k)
    } else {
        HashTable::new(hash, equiv)
    };
    Ok(vec![Value::from(hashtable)])
}

#[bridge(name = "hashtable?", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_pred(obj: &Value) -> bool {
    obj.is_a::<Embedded<HashTableInner>>()
}

#[bridge(name = "hashtable-size", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_size(hashtable: HashTable) -> usize {
    hashtable.size()
}

// hashtable-ref, hashtable-set!, hashtable-delete!, hashtable-contains?, and
// hashtable-update! call the table's hash/eq (and, for update!, the update
// procedure) as ordinary Scheme calls, so unlike the rest of this file they
// need the caller's barrier: promoted from `#[bridge]` to `#[cps_bridge]` to
// get it. This loses the "known function" fast path the plain-arg bridges
// above get (`#[bridge]` can shortcut functions of <=3 non-variadic args
// straight to typed Rust args; `cps_bridge` always goes through the general
// `(runtime, env, k, args, rest_args, barrier)` calling convention).

#[cps_bridge(
    def = "hashtable-ref hashtable key default",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_ref(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let hashtable: HashTable = (&args[0]).try_into()?;
    let val = hashtable.get(&args[1], &args[2], barrier)?;
    Ok(Application::new(k, None, vec![val]))
}

#[cps_bridge(
    def = "hashtable-set! hashtable key obj",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_set_bang(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let hashtable: HashTable = (&args[0]).try_into()?;
    hashtable.set(&args[1], &args[2], barrier)?;
    Ok(Application::new(k, None, Vec::new()))
}

#[cps_bridge(
    def = "hashtable-delete! hashtable key",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_delete_bang(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let hashtable: HashTable = (&args[0]).try_into()?;
    hashtable.delete(&args[1], barrier)?;
    Ok(Application::new(k, None, Vec::new()))
}

#[cps_bridge(
    def = "hashtable-contains? hashtable key",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_contains_pred(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let hashtable: HashTable = (&args[0]).try_into()?;
    let contains = hashtable.contains(&args[1], barrier)?;
    Ok(Application::new(k, None, vec![Value::from(contains)]))
}

#[cps_bridge(
    def = "hashtable-update! hashtable key proc default",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_update_bang(
    _runtime: &Runtime,
    _env: &[Value],
    k: Procedure,
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let hashtable: HashTable = (&args[0]).try_into()?;
    let proc: Procedure = args[2].clone().try_into()?;
    hashtable.update(&args[1], &proc, &args[3], barrier)?;
    Ok(Application::new(k, None, Vec::new()))
}

#[bridge(name = "hashtable-copy", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_copy(hashtable: HashTable, rest: &[Value]) -> Result<Vec<Value>, Exception> {
    let mutable = match rest {
        [] => false,
        [mutable] => mutable.is_true(),
        x => return Err(Exception::wrong_num_of_args(2, 1 + x.len())),
    };
    let new_hashtable = hashtable.copy(mutable);
    Ok(vec![Value::from(new_hashtable)])
}

#[bridge(name = "hashtable-clear!", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_clear_bang(hashtable: HashTable, rest: &[Value]) -> Result<Vec<Value>, Exception> {
    let k = match rest {
        [] => None,
        [k] => Some(k.try_into()?),
        x => return Err(Exception::wrong_num_of_args(3, 2 + x.len())),
    };

    hashtable.clear()?;

    if let Some(k) = k {
        let mut table = hashtable.0.table.write();
        if table.capacity() < k {
            table.shrink_to(k, TableEntry::get_hash);
        } else {
            table.reserve(k, TableEntry::get_hash);
        }
    }

    Ok(Vec::new())
}

#[bridge(name = "hashtable-keys", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_keys(hashtable: HashTable) -> Result<Vec<Value>, Exception> {
    let keys = Value::from(hashtable.keys());
    Ok(vec![keys])
}

#[bridge(name = "hashtable-entries", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_entries(hashtable: HashTable) -> Result<Vec<Value>, Exception> {
    let (keys, values) = hashtable.entries();
    Ok(vec![Value::from(keys), Value::from(values)])
}

#[bridge(
    name = "hashtable-equivalence-function",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_equivalence_function(hashtable: HashTable) -> Result<Vec<Value>, Exception> {
    let eqv_func = Value::from(hashtable.0.eq.clone());
    Ok(vec![eqv_func])
}

#[bridge(
    name = "hashtable-hash-function",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_hash_function(hashtable: HashTable) -> Result<Vec<Value>, Exception> {
    let hash_func = Value::from(hashtable.0.hash.clone());
    Ok(vec![hash_func])
}

#[bridge(name = "hashtable-mutable?", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_mutable_pred(hashtable: HashTable) -> Result<Vec<Value>, Exception> {
    let is_mutable = Value::from(hashtable.0.mutable);
    Ok(vec![is_mutable])
}

#[bridge(name = "eq-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn eq_hash(obj: &Value) -> Result<Vec<Value>, Exception> {
    let mut hasher = DefaultHasher::new();
    obj.eq_hash(&mut hasher);
    Ok(vec![Value::from(hasher.finish())])
}

#[bridge(name = "eqv-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn eqv_hash(obj: &Value) -> Result<Vec<Value>, Exception> {
    let mut hasher = DefaultHasher::new();
    obj.eqv_hash(&mut hasher);
    Ok(vec![Value::from(hasher.finish())])
}

#[bridge(name = "equal-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn equal_hash(obj: &Value) -> Result<Vec<Value>, Exception> {
    let mut hasher = DefaultHasher::new();
    obj.equal_hash(&mut IndexSet::default(), &mut hasher);
    Ok(vec![Value::from(hasher.finish())])
}

#[bridge(name = "string-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn string_hash(string: &Value) -> Result<Vec<Value>, Exception> {
    let string: WideString = string.clone().try_into()?;
    let mut hasher = DefaultHasher::new();
    string.hash(&mut hasher);
    Ok(vec![Value::from(hasher.finish())])
}

#[bridge(name = "string-ci-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn string_ci_hash(string: &Value) -> Result<Vec<Value>, Exception> {
    let string: WideString = string.clone().try_into()?;
    let mut hasher = DefaultHasher::new();
    let chars = string.0.chars.read();
    hasher.write_usize(chars.len());
    for lowercase in chars.iter().copied().flat_map(char::to_lowercase) {
        lowercase.hash(&mut hasher);
    }
    Ok(vec![Value::from(hasher.finish())])
}

#[bridge(name = "symbol-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn symbol_hash(symbol: &Value) -> Result<Vec<Value>, Exception> {
    let symbol: Symbol = symbol.clone().try_into()?;
    let mut hasher = DefaultHasher::new();
    symbol.hash(&mut hasher);
    Ok(vec![Value::from(hasher.finish())])
}
