//! Scheme compatible hashtables

use indexmap::IndexSet;
use parking_lot::RwLock;
use scheme_rs_gc::{HeapArray, HeapSlot};
use std::{
    collections::HashSet,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
};

use crate::{
    exceptions::Exception,
    gc::{HeapValue, Trace, heap_array, heap_array_with_len},
    proc::{ContBarrier, Procedure},
    records::{Embeddable, Embedded},
    registry::bridge,
    symbols::Symbol,
    value::{Expect1, Value},
};

#[derive(Trace)]
struct TableEntry {
    key: HeapValue,
    val: HeapValue,
}

#[derive(Clone)]
struct IndexEntry {
    slot: usize,
    hash: u64,
}

#[derive(Clone, Default)]
struct Index {
    map: hashbrown::HashTable<IndexEntry>,
    next_free: usize,
    live: usize,
}

impl IndexEntry {
    pub(crate) fn get_hash(&self) -> u64 {
        self.hash
    }
}

#[derive(Trace, Embeddable)]
#[scheme(name = "hash-table", sealed = true, opaque = true)]
pub(crate) struct HashTableInner {
    /// Inner table that keeps track of indexes in to an array.
    /// TODO: Move some of the code to scheme.
    ///
    /// We can't use the std library hashmap since we don't want to bundle the
    /// eq and hash functions with the key Value, so we use hashbrown's
    /// HashTable
    #[trace(skip)]
    index: RwLock<Index>,
    entries: HeapSlot<HeapArray<TableEntry>>,
    /// Equivalence function.
    eq: Procedure,
    /// Hash function.
    hash: Procedure,
    /// Whether or not the hashtable is mutable
    mutable: bool,
}

impl HashTableInner {
    pub fn size(&self) -> usize {
        self.index.read().map.len()
    }

    #[cfg(not(feature = "async"))]
    pub fn hash(&self, val: Value) -> Result<u64, Exception> {
        self.hash.call(&[val], &mut ContBarrier::new())?.expect1()
    }

    #[cfg(feature = "async")]
    pub fn hash(&self, val: Value) -> Result<u64, Exception> {
        self.hash
            .call_sync(&[val], &mut ContBarrier::new())?
            .expect1()
    }

    #[cfg(not(feature = "async"))]
    pub fn eq(&self, lhs: Value, rhs: Value) -> Result<bool, Exception> {
        self.eq
            .call(&[lhs, rhs], &mut ContBarrier::new())?
            .expect1()
    }

    #[cfg(feature = "async")]
    pub fn eq(&self, lhs: Value, rhs: Value) -> Result<bool, Exception> {
        self.eq
            .call_sync(&[lhs, rhs], &mut ContBarrier::new())?
            .expect1()
    }

    /// Equivalent to `hashtable-ref`
    pub fn get(&self, key: &Value, default: &Value) -> Result<Value, Exception> {
        let hash = self.hash(key.clone())?;
        let index = self.index.read();

        if let Some(slot) = self.find_slot(&index, hash, key)? {
            return Ok(self.entries.read()[slot].val.get());
        }

        Ok(default.clone())
    }

    fn find_slot(&self, index: &Index, hash: u64, key: &Value) -> Result<Option<usize>, Exception> {
        for index_entry in index.map.iter_hash(hash) {
            if index_entry.hash != hash {
                continue;
            }

            let slot = index_entry.slot;
            let entry = &self.entries.read()[slot];
            if self.eq(key.clone(), entry.key.get())? {
                return Ok(Some(slot));
            }
        }

        Ok(None)
    }

    fn insert(&self, index: &mut Index, hash: u64, key: &Value, val: &Value) {
        if index.next_free == self.entries.get().len() {
            self.grow(index)
        }

        let slot = index.next_free;

        let next_entry = &self.entries.read()[slot];
        next_entry.key.set(key.clone());
        next_entry.val.set(val.clone());

        // Insert the new entry, guaranteed to be unique.
        index.map.insert_unique(
            hash,
            IndexEntry {
                slot: index.next_free,
                hash,
            },
            IndexEntry::get_hash,
        );

        index.next_free += 1;
        index.live += 1;
    }

    pub fn set(&self, key: &Value, val: &Value) -> Result<(), Exception> {
        if !self.mutable {
            return Err(Exception::error("hashtable is immutable"));
        }

        let hash = self.hash(key.clone())?;
        let mut index = self.index.write();

        if let Some(slot) = self.find_slot(&index, hash, key)? {
            self.entries.read()[slot].val.set(val.clone());
            return Ok(());
        }

        self.insert(&mut index, hash, key, val);
        Ok(())
    }

    pub fn delete(&self, key: &Value) -> Result<(), Exception> {
        if !self.mutable {
            return Err(Exception::error("hashtable is immutable"));
        }

        let hash = self.hash(key.clone())?;
        let mut index = self.index.write();

        let Some(slot) = self.find_slot(&index, hash, key)? else {
            return Ok(());
        };

        if let Ok(entry) = index
            .map
            .find_entry(hash, |index_entry| index_entry.slot == slot)
        {
            entry.remove();
        }

        let entries = self.entries.read();
        entries[slot].key.set(Value::undefined());
        entries[slot].val.set(Value::undefined());
        index.live -= 1;

        Ok(())
    }

    pub fn contains(&self, key: &Value) -> Result<bool, Exception> {
        Ok(self
            .find_slot(&self.index.read(), self.hash(key.clone())?, key)?
            .is_some())
    }

    pub fn update(&self, key: &Value, proc: &Procedure, default: &Value) -> Result<(), Exception> {
        use std::slice;

        if !self.mutable {
            return Err(Exception::error("hashtable is immutable"));
        }

        let hash = self.hash(key.clone())?;
        let mut index = self.index.write();

        if let Some(slot) = self.find_slot(&index, hash, key)? {
            let entry = &self.entries.read()[slot];

            #[cfg(not(feature = "async"))]
            let updated =
                proc.call(slice::from_ref(&entry.val.get()), &mut ContBarrier::new())?[0].clone();

            #[cfg(feature = "async")]
            let updated = proc
                .call_sync(slice::from_ref(&entry.val.get()), &mut ContBarrier::new())?[0]
                .clone();

            entry.val.set(updated);
            return Ok(());
        }

        #[cfg(not(feature = "async"))]
        let updated = proc.call(slice::from_ref(default), &mut ContBarrier::new())?[0].clone();

        #[cfg(feature = "async")]
        let updated = proc.call_sync(slice::from_ref(default), &mut ContBarrier::new())?[0].clone();

        self.insert(&mut index, hash, key, &updated);
        Ok(())
    }

    pub fn copy(&self, mutable: bool) -> Self {
        let (index, entries) = self.rebuild(&self.index.read(), self.entries.get().len());
        Self {
            index: RwLock::new(index),
            eq: self.eq.clone(),
            hash: self.hash.clone(),
            entries: HeapSlot::new(entries),
            mutable,
        }
    }

    pub fn clear(&self, k: Option<usize>) -> Result<(), Exception> {
        if !self.mutable {
            return Err(Exception::error("hashtable is immutable"));
        }

        let mut index = self.index.write();

        if let Some(new_size) = k {
            let (new_index, array) = self.rebuild(&Index::default(), new_size);
            *index = new_index;
            self.entries.set(array);
            return Ok(());
        }

        let entries = self.entries.read();
        for index_entry in index.map.drain() {
            let slot = index_entry.slot;
            let entry = &entries[slot];
            entry.key.set(Value::undefined());
            entry.val.set(Value::undefined());
        }

        index.live = 0;
        index.next_free = 0;

        Ok(())
    }

    pub fn keys(&self) -> Vec<Value> {
        let index = self.index.read();
        let entries = self.entries.read();
        index
            .map
            .iter()
            .map(|index_entry| entries[index_entry.slot].key.get())
            .collect()
    }

    pub fn entries(&self) -> (Vec<Value>, Vec<Value>) {
        let index = self.index.read();
        let entries = self.entries.read();
        index
            .map
            .iter()
            .map(|index_entry| {
                let entry = &entries[index_entry.slot];
                (entry.key.get(), entry.val.get())
            })
            .unzip()
    }

    fn grow(&self, index: &mut Index) {
        let capacity = self.entries.read().len();
        let new_cap = if capacity > 0 && index.live <= capacity / 2 {
            capacity
        } else {
            (capacity * 2).max(8)
        };
        let (new_index, new_entries) = self.rebuild(index, new_cap);
        *index = new_index;
        self.entries.set(new_entries);
    }

    fn rebuild(&self, index: &Index, capacity: usize) -> (Index, HeapArray<TableEntry>) {
        let capacity = capacity.max(index.live);
        let entries = self.entries.read();
        let live = index.map.len();
        let mut map = hashbrown::HashTable::with_capacity(capacity);

        let copied = index.map.iter().enumerate().map(|(slot, ie)| {
            map.insert_unique(
                ie.hash,
                IndexEntry {
                    slot,
                    hash: ie.hash,
                },
                IndexEntry::get_hash,
            );
            let old = &entries[ie.slot];
            TableEntry {
                key: HeapValue::new(old.key.get()),
                val: HeapValue::new(old.val.get()),
            }
        });
        let empty = (live..capacity).map(|_| TableEntry {
            key: HeapValue::new(Value::undefined()),
            val: HeapValue::new(Value::undefined()),
        });
        let array = heap_array_with_len(capacity, copied.chain(empty));

        (
            Index {
                map,
                next_free: live,
                live,
            },
            array,
        )
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
            index: RwLock::new(Index {
                map: hashbrown::HashTable::new(),
                next_free: 0,
                live: 0,
            }),
            eq,
            hash,
            mutable: true,
            entries: HeapSlot::new(heap_array([])),
        }))
    }

    pub fn with_capacity(hash: Procedure, eq: Procedure, cap: usize) -> Self {
        Self(Embedded::new(HashTableInner {
            index: RwLock::new(Index {
                map: hashbrown::HashTable::with_capacity(cap),
                next_free: 0,
                live: 0,
            }),
            eq,
            hash,
            mutable: true,
            entries: HeapSlot::new(heap_array((0..cap).map(|_| TableEntry {
                key: HeapValue::new(Value::undefined()),
                val: HeapValue::new(Value::undefined()),
            }))),
        }))
    }

    pub fn size(&self) -> usize {
        self.0.size()
    }

    pub fn get(&self, key: &Value, default: &Value) -> Result<Value, Exception> {
        self.0.get(key, default)
    }

    pub fn set(&self, key: &Value, val: &Value) -> Result<(), Exception> {
        self.0.set(key, val)
    }

    pub fn delete(&self, key: &Value) -> Result<(), Exception> {
        self.0.delete(key)
    }

    pub fn contains(&self, key: &Value) -> Result<bool, Exception> {
        self.0.contains(key)
    }

    pub fn update(&self, key: &Value, proc: &Procedure, default: &Value) -> Result<(), Exception> {
        self.0.update(key, proc, default)
    }

    pub fn copy(&self, mutable: bool) -> Self {
        Self(Embedded::new(self.0.copy(mutable)))
    }

    pub fn clear(&self, k: Option<usize>) -> Result<(), Exception> {
        self.0.clear(k)
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
        let index = self.index.read();
        let entries = self.entries.read();
        write!(f, "#hash(")?;
        for (i, ie) in index.map.iter().enumerate() {
            let entry = &entries[ie.slot];
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
    hash_function: Value,
    equiv: Value,
    k: Option<usize>,
) -> Result<Value, Exception> {
    let hash: Procedure = hash_function.clone().try_into()?;
    let equiv: Procedure = equiv.clone().try_into()?;
    let hashtable = if let Some(k) = k {
        HashTable::with_capacity(hash, equiv, k)
    } else {
        HashTable::new(hash, equiv)
    };
    Ok(Value::from(hashtable))
}

#[bridge(name = "hashtable?", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_pred(obj: Value) -> bool {
    obj.is_a::<Embedded<HashTableInner>>()
}

#[bridge(name = "hashtable-size", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_size(hashtable: HashTable) -> usize {
    hashtable.size()
}

#[bridge(name = "hashtable-ref", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_ref(hashtable: HashTable, key: Value, default: Value) -> Result<Value, Exception> {
    hashtable.get(&key, &default)
}

#[bridge(name = "hashtable-set!", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_set_bang(hashtable: HashTable, key: Value, obj: Value) -> Result<(), Exception> {
    hashtable.set(&key, &obj)?;
    Ok(())
}

#[bridge(name = "hashtable-delete!", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_delete_bang(hashtable: HashTable, key: Value) -> Result<(), Exception> {
    hashtable.delete(&key)?;
    Ok(())
}

#[bridge(name = "hashtable-contains?", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_contains_pred(hashtable: HashTable, key: Value) -> Result<bool, Exception> {
    Ok(hashtable.contains(&key)?)
}

#[bridge(name = "hashtable-update!", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_update_bang(
    hashtable: HashTable,
    key: Value,
    proc: Procedure,
    default: Value,
) -> Result<(), Exception> {
    hashtable.update(&key, &proc, &default)?;
    Ok(())
}

#[bridge(name = "hashtable-copy", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_copy(hashtable: HashTable, mutable: Option<bool>) -> Result<Value, Exception> {
    let mutable = mutable.unwrap_or(false);
    let new_hashtable = hashtable.copy(mutable);
    Ok(Value::from(new_hashtable))
}

#[bridge(name = "hashtable-clear!", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_clear_bang(hashtable: HashTable, k: Option<usize>) -> Result<(), Exception> {
    hashtable.clear(k)
}

#[bridge(name = "hashtable-keys", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_keys(hashtable: HashTable) -> Vec<Value> {
    hashtable.keys()
}

#[bridge(name = "hashtable-entries", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_entries(hashtable: HashTable) -> (Vec<Value>, Vec<Value>) {
    let (keys, values) = hashtable.entries();
    (keys, values)
}

#[bridge(
    name = "hashtable-equivalence-function",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_equivalence_function(hashtable: HashTable) -> Procedure {
    hashtable.0.eq.clone()
}

#[bridge(
    name = "hashtable-hash-function",
    lib = "(rnrs hashtables builtins (6))"
)]
pub fn hashtable_hash_function(hashtable: HashTable) -> Procedure {
    hashtable.0.hash.clone()
}

#[bridge(name = "hashtable-mutable?", lib = "(rnrs hashtables builtins (6))")]
pub fn hashtable_mutable_pred(hashtable: HashTable) -> bool {
    hashtable.0.mutable
}

#[bridge(name = "eq-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn eq_hash(obj: Value) -> u64 {
    let mut hasher = DefaultHasher::new();
    obj.eq_hash(&mut hasher);
    hasher.finish()
}

#[bridge(name = "eqv-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn eqv_hash(obj: Value) -> u64 {
    let mut hasher = DefaultHasher::new();
    obj.eqv_hash(&mut hasher);
    hasher.finish()
}

#[bridge(name = "equal-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn equal_hash(obj: Value) -> u64 {
    let mut hasher = DefaultHasher::new();
    obj.equal_hash(&mut IndexSet::default(), &mut hasher);
    hasher.finish()
}

#[bridge(name = "string-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn string_hash(string: Embedded<String>) -> u64 {
    let mut hasher = DefaultHasher::new();
    string.hash(&mut hasher);
    hasher.finish()
}

#[bridge(name = "string-ci-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn string_ci_hash(string: Embedded<String>) -> u64 {
    let mut hasher = DefaultHasher::new();
    // I hope this is okay, otherwise we have to collect the chars to a vec
    hasher.write_usize(string.len());
    for lowercase in string.chars().flat_map(char::to_lowercase) {
        lowercase.hash(&mut hasher);
    }
    hasher.finish()
}

#[bridge(name = "symbol-hash", lib = "(rnrs hashtables builtins (6))")]
pub fn symbol_hash(symbol: Symbol) -> u64 {
    let mut hasher = DefaultHasher::new();
    symbol.hash(&mut hasher);
    hasher.finish()
}
