//! R6RS compatible hashtables

use indexmap::IndexSet;
use parking_lot::RwLock;
use std::{
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    sync::Arc,
};

use crate::{
    exceptions::Condition,
    gc::{Gc, Trace},
    proc::Procedure,
    registry::bridge,
    strings,
    symbols::Symbol,
    value::{UnpackedValue, Value, ValueType},
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
    /// Equivalence function
    eq: Procedure,
    /// Hash function
    hash: Procedure,
    /// Whether or not the hashtable is mutable
    mutable: bool,
}

impl HashTableInner {
    pub fn size(&self) -> usize {
        self.table.read().len()
    }

    #[cfg(not(feature = "async"))]
    pub fn hash(&self, val: Value) -> Result<u64, Condition> {
        self.hash.call(&[val])?[0].clone().try_into()
    }

    #[cfg(feature = "async")]
    pub fn hash(&self, val: Value) -> Result<u64, Condition> {
        self.hash.call_sync(&[val])?[0].clone().try_into()
    }

    #[cfg(not(feature = "async"))]
    pub fn eq(&self, lhs: Value, rhs: Value) -> Result<bool, Condition> {
        Ok(self.eq.call(&[lhs, rhs])?[0].clone().is_true())
    }

    #[cfg(feature = "async")]
    pub fn eq(&self, lhs: Value, rhs: Value) -> Result<bool, Condition> {
        Ok(self.eq.call_sync(&[lhs, rhs])?[0].clone().is_true())
    }

    /// Equivalent to `hashtable-ref`
    pub fn get(&self, key: &Value, default: &Value) -> Result<Value, Condition> {
        let table = self.table.read();
        let hash = self.hash(key.clone())?;
        for entry in table.iter_hash(hash) {
            if entry.hash == hash && self.eq(key.clone(), entry.key.clone())? {
                return Ok(entry.val.clone());
            }
        }
        Ok(default.clone())
    }

    pub fn set(&self, key: &Value, val: &Value) -> Result<(), Condition> {
        if !self.mutable {
            return Err(Condition::error("hashtable is immutable"));
        }

        let mut table = self.table.write();
        let hash = self.hash(key.clone())?;
        for entry in table.iter_hash_mut(hash) {
            if entry.hash == hash && self.eq(key.clone(), entry.key.clone())? {
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

    pub fn delete(&self, key: &Value) -> Result<(), Condition> {
        if !self.mutable {
            return Err(Condition::error("hashtable is immutable"));
        }

        let mut table = self.table.write();
        let hash = self.hash(key.clone())?;
        let buckets = table.iter_hash_buckets(hash).collect::<Vec<_>>();
        for bucket in buckets.into_iter() {
            if let Ok(entry) = table.get_bucket_entry(bucket)
                && let inner = entry.get()
                && inner.hash == hash
                && self.eq(key.clone(), inner.key.clone())?
            {
                entry.remove();
                return Ok(());
            }
        }

        Ok(())
    }

    pub fn contains(&self, key: &Value) -> Result<bool, Condition> {
        let table = self.table.write();
        let hash = self.hash(key.clone())?;
        for entry in table.iter_hash(hash) {
            if entry.hash == hash && self.eq(key.clone(), entry.key.clone())? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    pub fn update(&self, key: &Value, proc: &Procedure, default: &Value) -> Result<(), Condition> {
        use std::slice;

        if !self.mutable {
            return Err(Condition::error("hashtable is immutable"));
        }

        let mut table = self.table.write();
        let hash = self.hash(key.clone())?;
        for entry in table.iter_hash_mut(hash) {
            if entry.hash == hash && self.eq(key.clone(), entry.key.clone())? {
                #[cfg(not(feature = "async"))]
                let updated = proc.call(slice::from_ref(&entry.val))?[0].clone();

                #[cfg(feature = "async")]
                let updated = proc.call_sync(slice::from_ref(&entry.val))?[0].clone();

                entry.val = updated;
                return Ok(());
            }
        }

        #[cfg(not(feature = "async"))]
        let updated = proc.call(slice::from_ref(default))?[0].clone();

        #[cfg(feature = "async")]
        let updated = proc.call_sync(slice::from_ref(default))?[0].clone();

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

    pub fn clear(&self) {
        self.table.write().clear();
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

#[derive(Clone, Trace)]
pub struct HashTable(pub(crate) Gc<HashTableInner>);

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
        Self(Gc::new(HashTableInner {
            table: RwLock::new(hashbrown::HashTable::new()),
            eq,
            hash,
            mutable: true,
        }))
    }

    pub fn with_capacity(hash: Procedure, eq: Procedure, cap: usize) -> Self {
        Self(Gc::new(HashTableInner {
            table: RwLock::new(hashbrown::HashTable::with_capacity(cap)),
            eq,
            hash,
            mutable: true,
        }))
    }

    pub fn size(&self) -> usize {
        self.0.read().size()
    }

    pub fn get(&self, key: &Value, default: &Value) -> Result<Value, Condition> {
        self.0.read().get(key, default)
    }

    pub fn set(&self, key: &Value, val: &Value) -> Result<(), Condition> {
        self.0.read().set(key, val)
    }

    pub fn delete(&self, key: &Value) -> Result<(), Condition> {
        self.0.read().delete(key)
    }

    pub fn contains(&self, key: &Value) -> Result<bool, Condition> {
        self.0.read().contains(key)
    }

    pub fn update(&self, key: &Value, proc: &Procedure, default: &Value) -> Result<(), Condition> {
        self.0.read().update(key, proc, default)
    }

    pub fn copy(&self, mutable: bool) -> Self {
        Self(Gc::new(self.0.read().copy(mutable)))
    }

    pub fn clear(&self) {
        self.0.read().clear();
    }

    pub fn keys(&self) -> Vec<Value> {
        self.0.read().keys()
    }

    pub fn entries(&self) -> (Vec<Value>, Vec<Value>) {
        self.0.read().entries()
    }
}

impl fmt::Debug for HashTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#hash(")?;
        for (i, entry) in self.0.read().table.read().iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "({:?} . {:?})", entry.key, entry.val)?;
        }
        write!(f, ")")
    }
}

#[bridge(name = "make-hashtable", lib = "(rnrs hashtables builtins (6))")]
pub fn make_hashtable(
    hash_function: &Value,
    equiv: &Value,
    rest: &[Value],
) -> Result<Vec<Value>, Condition> {
    let hash: Procedure = hash_function.clone().try_into()?;
    let equiv: Procedure = equiv.clone().try_into()?;
    let k = match rest {
        [] => None,
        [k] => Some(k.try_into()?),
        x => return Err(Condition::wrong_num_of_args(3, 2 + x.len())),
    };
    let hashtable = if let Some(k) = k {
        HashTable::with_capacity(hash, equiv, k)
    } else {
        HashTable::new(hash, equiv)
    };
    Ok(vec![Value::from(hashtable)])
}

#[bridge(name = "hashtable?", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_pred(hashtable: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(
        hashtable.type_of() == ValueType::HashTable,
    )])
}

#[bridge(name = "hashtable-size", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_size(hashtable: &Value) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    Ok(vec![Value::from(hashtable.size())])
}

#[bridge(name = "hashtable-ref", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_ref(
    hashtable: &Value,
    key: &Value,
    default: &Value,
) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    Ok(vec![hashtable.get(key, default)?])
}

#[bridge(name = "hashtable-set!", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_set_bang(
    hashtable: &Value,
    key: &Value,
    obj: &Value,
) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    hashtable.set(key, obj)?;
    Ok(Vec::new())
}

#[bridge(name = "hashtable-delete!", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_delete_bang(hashtable: &Value, key: &Value) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    hashtable.delete(key)?;
    Ok(Vec::new())
}

#[bridge(name = "hashtable-contains?", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_contains_pred(hashtable: &Value, key: &Value) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    Ok(vec![Value::from(hashtable.contains(key)?)])
}

#[bridge(name = "hashtable-update!", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_update_bang(
    hashtable: &Value,
    key: &Value,
    proc: &Value,
    default: &Value,
) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    let proc: Procedure = proc.clone().try_into()?;
    hashtable.update(key, &proc, default)?;
    Ok(Vec::new())
}

#[bridge(name = "hashtable-copy", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_copy(hashtable: &Value, rest: &[Value]) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    let mutable = match rest {
        [] => false,
        [mutable] => mutable.is_true(),
        x => return Err(Condition::wrong_num_of_args(2, 1 + x.len())),
    };
    let new_hashtable = hashtable.copy(mutable);
    Ok(vec![Value::from(new_hashtable)])
}

#[bridge(name = "hashtable-clear!", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_clear_bang(hashtable: &Value, rest: &[Value]) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    let k = match rest {
        [] => None,
        [k] => Some(k.try_into()?),
        x => return Err(Condition::wrong_num_of_args(3, 2 + x.len())),
    };

    hashtable.clear();

    if let Some(k) = k {
        let inner = hashtable.0.read();
        let mut table = inner.table.write();
        if table.capacity() < k {
            table.shrink_to(k, TableEntry::get_hash);
        } else {
            table.reserve(k, TableEntry::get_hash);
        }
    }

    Ok(Vec::new())
}

#[bridge(name = "hashtable-keys", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_keys(hashtable: &Value) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    let keys = Value::from(hashtable.keys());
    Ok(vec![keys])
}

#[bridge(name = "hashtable-entries", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_entries(hashtable: &Value) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    let (keys, values) = hashtable.entries();
    Ok(vec![Value::from(keys), Value::from(values)])
}

#[bridge(
    name = "hashtable-equivalence-function",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_equivalence_function(hashtable: &Value) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    let eqv_func = Value::from(hashtable.0.read().eq.clone());
    Ok(vec![eqv_func])
}

#[bridge(
    name = "hashtable-hash-function",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_hash_function(hashtable: &Value) -> Result<Vec<Value>, Condition> {
    let hashtable: HashTable = hashtable.clone().try_into()?;
    let hash_func = Value::from(hashtable.0.read().hash.clone());
    Ok(vec![hash_func])
}

impl Value {
    /// Performs a hash suitable for use with eq? as an equivalance function
    pub fn eq_hash<H: Hasher>(&self, state: &mut H) {
        let unpacked = self.unpacked_ref();
        std::mem::discriminant(&*unpacked).hash(state);
        match &*unpacked {
            UnpackedValue::Undefined => (),
            UnpackedValue::Null => (),
            UnpackedValue::Boolean(b) => b.hash(state),
            UnpackedValue::Character(c) => c.hash(state),
            UnpackedValue::Number(n) => Arc::as_ptr(n).hash(state),
            UnpackedValue::String(s) => Arc::as_ptr(s).hash(state),
            UnpackedValue::Symbol(s) => s.hash(state),
            UnpackedValue::ByteVector(v) => Arc::as_ptr(v).hash(state),
            UnpackedValue::Syntax(s) => Arc::as_ptr(s).hash(state),
            UnpackedValue::Procedure(c) => Gc::as_ptr(&c.0).hash(state),
            UnpackedValue::Record(r) => Gc::as_ptr(&r.0).hash(state),
            UnpackedValue::RecordTypeDescriptor(rt) => Arc::as_ptr(rt).hash(state),
            UnpackedValue::Pair(p) => Gc::as_ptr(p).hash(state),
            UnpackedValue::Vector(v) => Gc::as_ptr(v).hash(state),
            UnpackedValue::Port(p) => Arc::as_ptr(&p.0).hash(state),
            UnpackedValue::HashTable(ht) => Gc::as_ptr(&ht.0).hash(state),
            UnpackedValue::Cell(c) => c.0.read().eqv_hash(state),
        }
    }

    /// Performs a hash suitable for use with eqv? as an equivalance function
    pub fn eqv_hash<H: Hasher>(&self, state: &mut H) {
        let unpacked = self.unpacked_ref();
        std::mem::discriminant(&*unpacked).hash(state);
        match &*unpacked {
            UnpackedValue::Undefined => (),
            UnpackedValue::Null => (),
            UnpackedValue::Boolean(b) => b.hash(state),
            UnpackedValue::Character(c) => c.hash(state),
            UnpackedValue::Number(n) => n.as_ref().hash(state),
            UnpackedValue::String(s) => Arc::as_ptr(s).hash(state),
            UnpackedValue::Symbol(s) => s.hash(state),
            UnpackedValue::ByteVector(v) => Arc::as_ptr(v).hash(state),
            UnpackedValue::Syntax(s) => Arc::as_ptr(s).hash(state),
            UnpackedValue::Procedure(c) => Gc::as_ptr(&c.0).hash(state),
            UnpackedValue::Record(r) => Gc::as_ptr(&r.0).hash(state),
            UnpackedValue::RecordTypeDescriptor(rt) => Arc::as_ptr(rt).hash(state),
            UnpackedValue::Pair(p) => Gc::as_ptr(p).hash(state),
            UnpackedValue::Vector(v) => Gc::as_ptr(v).hash(state),
            UnpackedValue::Port(p) => Arc::as_ptr(&p.0).hash(state),
            UnpackedValue::HashTable(ht) => Gc::as_ptr(&ht.0).hash(state),
            UnpackedValue::Cell(c) => c.0.read().eqv_hash(state),
        }
    }

    /// Performs a hash suitable for use with equal? as an equivalance function
    pub fn equal_hash<H: Hasher>(&self, recursive: &mut IndexSet<Value>, state: &mut H) {
        let unpacked = self.unpacked_ref();
        std::mem::discriminant(&*unpacked).hash(state);

        // I think this is fine, because types that would be recursive will
        // write out at least two values here where we're only writing out one.
        if let Some(index) = recursive.get_index_of(self) {
            state.write_usize(index);
            return;
        }

        match &*unpacked {
            UnpackedValue::Undefined => (),
            UnpackedValue::Null => (),
            UnpackedValue::Boolean(b) => b.hash(state),
            UnpackedValue::Character(c) => c.hash(state),
            UnpackedValue::Number(n) => n.as_ref().hash(state),
            UnpackedValue::String(s) => s.hash(state),
            UnpackedValue::Symbol(s) => s.hash(state),
            UnpackedValue::ByteVector(v) => v.hash(state),
            UnpackedValue::Syntax(s) => Arc::as_ptr(s).hash(state),
            UnpackedValue::Procedure(c) => Gc::as_ptr(&c.0).hash(state),
            UnpackedValue::Record(r) => Gc::as_ptr(&r.0).hash(state),
            UnpackedValue::RecordTypeDescriptor(rt) => Arc::as_ptr(rt).hash(state),
            UnpackedValue::Pair(p) => {
                recursive.insert(self.clone());
                let p_read = p.read();
                p_read.0.equal_hash(recursive, state);
                p_read.1.equal_hash(recursive, state);
            }
            UnpackedValue::Vector(v) => {
                recursive.insert(self.clone());
                let v_read = v.read();
                state.write_usize(v_read.len());
                for val in v_read.iter() {
                    val.equal_hash(recursive, state);
                }
            }
            UnpackedValue::Port(p) => Arc::as_ptr(&p.0).hash(state),
            UnpackedValue::HashTable(ht) => Gc::as_ptr(&ht.0).hash(state),
            UnpackedValue::Cell(c) => c.0.read().eqv_hash(state),
        }
    }
}

#[bridge(name = "eq-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn eq_hash(obj: &Value) -> Result<Vec<Value>, Condition> {
    let mut hasher = DefaultHasher::new();
    obj.eq_hash(&mut hasher);
    Ok(vec![Value::from(hasher.finish())])
}

#[bridge(name = "eqv-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn eqv_hash(obj: &Value) -> Result<Vec<Value>, Condition> {
    let mut hasher = DefaultHasher::new();
    obj.eqv_hash(&mut hasher);
    Ok(vec![Value::from(hasher.finish())])
}

#[bridge(name = "equal-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn equal_hash(obj: &Value) -> Result<Vec<Value>, Condition> {
    let mut hasher = DefaultHasher::new();
    obj.equal_hash(&mut IndexSet::default(), &mut hasher);
    Ok(vec![Value::from(hasher.finish())])
}

#[bridge(name = "string-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn string_hash(string: &Value) -> Result<Vec<Value>, Condition> {
    let string: Arc<strings::AlignedString> = string.clone().try_into()?;
    let mut hasher = DefaultHasher::new();
    string.hash(&mut hasher);
    Ok(vec![Value::from(hasher.finish())])
}

#[bridge(name = "symbol-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn symbol_hash(symbol: &Value) -> Result<Vec<Value>, Condition> {
    let symbol: Symbol = symbol.clone().try_into()?;
    let mut hasher = DefaultHasher::new();
    symbol.hash(&mut hasher);
    Ok(vec![Value::from(hasher.finish())])
}
