//! Growable mutable vectors.

#[cfg(target_endian = "little")]
use crate::symbols::Symbol;
use crate::{
    DynHasher,
    exceptions::Exception,
    gc::{HeapValue, Trace},
    lists::{List, heap_values_to_list, iter_list},
    records::{Embeddable, Embedded, Record},
    registry::bridge,
    value::{Value, write_value},
};
use indexmap::{IndexMap, IndexSet};
use parking_lot::{
    MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
};
use scheme_rs_macros::rtd;
use std::{clone::Clone, fmt, hash::Hash, ops::Range, sync::Arc};

#[derive(Trace)]
#[repr(align(16))]
pub(crate) struct VectorInner<T: Trace> {
    /// Inner vector.
    pub(crate) vec: RwLock<Vec<T>>,
    /// Whether or not the vector is mutable
    pub(crate) mutable: bool,
}

/// TEMPORARY, later we will add a HeapArray, an immix allocated heap array.
/// And convert the vector types to it.
#[derive(Trace)]
pub(crate) struct TmpVecInner {
    pub(crate) vec: Box<[HeapValue]>,
    pub(crate) mutable: bool,
}

unsafe impl Embeddable for TmpVecInner {
    fn rtd() -> Arc<crate::records::RecordTypeDescriptor>
    where
        Self: Sized,
    {
        rtd!(ty: TmpVecInner, name: "vector", sealed: true, opaque: true)
    }

    fn display_fmt(
        &self,
        circular_values: &mut IndexMap<Value, bool>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write_vec(self, Value::display_fmt, circular_values, fmt)
    }

    fn debug_fmt(
        &self,
        circular_values: &mut IndexMap<Value, bool>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write_vec(self, Value::debug_fmt, circular_values, fmt)
    }

    fn equal(&self, rhs: &Record) -> bool
    where
        Self: Sized,
    {
        let Some(rhs) = rhs.cast::<Self>() else {
            return false;
        };
        self.vec.len() == rhs.vec.len()
            && self.vec.iter().zip(rhs.vec.iter()).all(|(a, b)| a.get() == b.get())
    }

    fn equal_hash(&self, recursive: &mut IndexSet<Value>, hasher: &mut dyn std::hash::Hasher)
    where
        Self: Sized,
    {
        hasher.write_usize(self.vec.len());
        for val in self.vec.iter() {
            val.get().equal_hash(recursive, &mut DynHasher(hasher));
        }
    }
}

/// A vector of values
#[derive(Clone, Trace)]
pub struct Vector(pub(crate) Embedded<TmpVecInner>);

impl Vector {
    pub fn immutable(vec: Vec<Value>) -> Self {
        Self::from(vec)
    }

    pub fn mutable(vec: Vec<Value>) -> Self {
        Self(Embedded::new(TmpVecInner {
            vec: vec.into_iter().map(HeapValue::new).collect(),
            mutable: true,
        }))
    }

    pub fn get(&self, index: usize) -> Option<Value> {
        self.0.vec.get(index).map(HeapValue::get)
    }

    pub fn first(&self) -> Option<Value> {
        self.get(0)
    }

    pub fn last(&self) -> Option<Value> {
        self.0.vec.last().map(HeapValue::get)
    }

    // TODO: Add more convenience functions here
    pub fn to_list(&self) -> Value {
        heap_values_to_list(&self.0.vec)
    }

    pub fn clone_inner_vec(&self) -> Vec<Value> {
        self.0.vec.iter().map(HeapValue::get).collect()
    }

    pub fn iter(&self) -> impl Iterator<Item = Value> {
        self.0.vec.iter().map(HeapValue::get)
    }

    pub fn is_empty(&self) -> bool {
        self.0.vec.is_empty()
    }

    pub(crate) fn from_slots(vec: Box<[HeapValue]>) -> Self {
        Self(Embedded::new(TmpVecInner { vec, mutable: true }))
    }
}

impl TryFrom<Value> for Vector {
    type Error = Exception;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Ok(Self((&value).try_into()?))
    }
}

impl From<&Value> for Option<Vector> {
    fn from(value: &Value) -> Self {
        Some(Vector(value.cast()?))
    }
}

impl TryFrom<&Value> for Vector {
    type Error = Exception;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        Ok(Self(value.try_into()?))
    }
}

impl From<Vec<Value>> for Vector {
    fn from(vec: Vec<Value>) -> Self {
        Self(Embedded::new(TmpVecInner {
            vec: vec.into_iter().map(HeapValue::new).collect(),
            mutable: false,
        }))
    }
}

impl From<Vec<Value>> for Value {
    fn from(vec: Vec<Value>) -> Self {
        Value::from(Vector::from(vec))
    }
}

impl From<Vector> for Value {
    fn from(vec: Vector) -> Self {
        Value::from(vec.0)
    }
}

unsafe impl Embeddable for VectorInner<u8> {
    fn rtd() -> Arc<crate::records::RecordTypeDescriptor>
    where
        Self: Sized,
    {
        rtd!(ty: VectorInner<u8>, name: "bytevector", sealed: true, opaque: true)
    }

    fn debug_fmt(
        &self,
        _circular_values: &mut IndexMap<Value, bool>,
        fmt: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write_bytevec(self, fmt)
    }

    fn equal(&self, rhs: &Record) -> bool
    where
        Self: Sized,
    {
        let Some(rhs) = rhs.cast::<Self>() else {
            return false;
        };
        *self.vec.read() == *rhs.vec.read()
    }

    fn equal_hash(&self, _recursive: &mut IndexSet<Value>, hasher: &mut dyn std::hash::Hasher)
    where
        Self: Sized,
    {
        self.vec.read().hash(&mut DynHasher(hasher))
    }
}

/// A vector of bytes
#[derive(Clone, Trace)]
pub struct ByteVector(pub(crate) Embedded<VectorInner<u8>>);

impl ByteVector {
    pub fn immutable(vec: Vec<u8>) -> Self {
        Self::from(vec)
    }

    pub fn mutable(vec: Vec<u8>) -> Self {
        Self(Embedded::new(VectorInner {
            vec: RwLock::new(vec),
            mutable: true,
        }))
    }

    pub fn as_slice(&self) -> MappedRwLockReadGuard<'_, [u8]> {
        RwLockReadGuard::map(self.0.vec.read(), |vec| vec.as_slice())
    }

    pub fn as_mut_slice(&self) -> Result<MappedRwLockWriteGuard<'_, [u8]>, Exception> {
        if self.0.mutable {
            Ok(RwLockWriteGuard::map(self.0.vec.write(), |vec| {
                vec.as_mut_slice()
            }))
        } else {
            Err(Exception::error("bytevector is not mutable"))
        }
    }

    pub fn as_mut_vec(&self) -> Result<RwLockWriteGuard<'_, Vec<u8>>, Exception> {
        if self.0.mutable {
            Ok(self.0.vec.write())
        } else {
            Err(Exception::error("bytevector is not mutable"))
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.0.vec.read().len()
    }

    pub fn clear(&self) -> Result<(), Exception> {
        if self.0.mutable {
            self.0.vec.write().clear();
            Ok(())
        } else {
            Err(Exception::error("bytevector is not mutable"))
        }
    }

    pub fn get(&self, idx: usize) -> Option<u8> {
        self.0.vec.read().get(idx).copied()
    }
}

impl From<Vec<u8>> for ByteVector {
    fn from(vec: Vec<u8>) -> Self {
        Self(Embedded::new(VectorInner {
            vec: RwLock::new(vec),
            mutable: false,
        }))
    }
}

impl Hash for ByteVector {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.vec.read().hash(state)
    }
}

impl PartialEq<[u8]> for ByteVector {
    fn eq(&self, rhs: &[u8]) -> bool {
        *self.0.vec.read() == rhs
    }
}

impl PartialEq for ByteVector {
    fn eq(&self, rhs: &Self) -> bool {
        *self.0.vec.read() == *rhs.0.vec.read()
    }
}

impl TryFrom<Value> for ByteVector {
    type Error = Exception;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Ok(Self((&value).try_into()?))
    }
}

impl From<&Value> for Option<ByteVector> {
    fn from(value: &Value) -> Self {
        Some(ByteVector(value.cast()?))
    }
}

impl TryFrom<&Value> for ByteVector {
    type Error = Exception;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        Ok(Self(value.try_into()?))
    }
}

impl From<Vec<u8>> for Value {
    fn from(value: Vec<u8>) -> Self {
        Value::from(ByteVector::from(value))
    }
}

impl From<ByteVector> for Value {
    fn from(vec: ByteVector) -> Self {
        Value::from(vec.0)
    }
}

pub(crate) fn write_vec(
    v: &TmpVecInner,
    fmt: fn(&Value, &mut IndexMap<Value, bool>, &mut fmt::Formatter<'_>) -> fmt::Result,
    circular_values: &mut IndexMap<Value, bool>,
    f: &mut fmt::Formatter<'_>,
) -> Result<(), fmt::Error> {
    write!(f, "#(")?;

    for (i, value) in v.vec.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write_value(&value.get(), fmt, circular_values, f)?;
    }

    write!(f, ")")
}

pub(crate) fn write_bytevec(
    v: &VectorInner<u8>,
    f: &mut fmt::Formatter<'_>,
) -> Result<(), fmt::Error> {
    write!(f, "#vu8(")?;

    let bytes = v.vec.read();
    for (i, byte) in bytes.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{byte}")?;
    }

    write!(f, ")")
}

fn try_make_range(start: usize, end: usize) -> Result<Range<usize>, Exception> {
    if end < start {
        Err(Exception::error(format!(
            "range end {end} is less than start {start}",
        )))
    } else {
        Ok(start..end)
    }
}

trait Indexer {
    type Collection: TryFrom<Value, Error = Exception>;

    fn get_len(_: &Self::Collection) -> usize;

    fn get_range(_: &Self::Collection, _: Range<usize>) -> Self::Collection;

    fn index(
        from: &Value,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Result<Self::Collection, Exception> {
        let collection = Self::Collection::try_from(from.clone())?;
        let len = Self::get_len(&collection);

        let start = start.unwrap_or(0);
        let end = end.unwrap_or(len);

        let range = try_make_range(start, end)?;
        if range.end > len {
            return Err(Exception::invalid_range(range, len));
        }

        Ok(Self::get_range(&collection, range))
    }
}

struct VectorIndexer;

impl Indexer for VectorIndexer {
    // type Collection = Gc<RwLock<AlignedVector<Value>>>;
    type Collection = Vector;

    fn get_len(vec: &Self::Collection) -> usize {
        vec.0.vec.len()
    }

    fn get_range(vec: &Self::Collection, range: Range<usize>) -> Self::Collection {
        Vector(Embedded::new(TmpVecInner {
            vec: vec.0.vec[range].iter().cloned().collect(),
            mutable: true,
        }))
    }
}

#[bridge(name = "vector?", lib = "(rnrs base builtins (6))")]
pub fn vector_pred(arg: Value) -> bool {
    arg.is_a::<Vector>()
}

#[bridge(name = "make-vector", lib = "(rnrs base builtins (6))")]
pub fn make_vector(n: usize, with: Option<Value>) -> Result<Vector, Exception> {
    let with = with.unwrap_or_else(Value::null);
    Ok(Vector::mutable(
        (0..n).map(|_| with.clone()).collect::<Vec<_>>(),
    ))
}

#[bridge(name = "vector", lib = "(rnrs base builtins (6))")]
pub fn vector(#[rest_args] args: Value) -> Vector {
    Vector::mutable(iter_list(&args).collect())
}

#[bridge(name = "vector-ref", lib = "(rnrs base builtins (6))")]
pub fn vector_ref(vec: Vector, index: usize) -> Result<Value, Exception> {
    vec.0
        .vec
        .get(index)
        .map(HeapValue::get)
        .ok_or(Exception::invalid_index(index, vec.0.vec.len()))
}

#[bridge(name = "vector-length", lib = "(rnrs base builtins (6))")]
pub fn vector_length(vec: Vector) -> usize {
    vec.0.vec.len()
}

#[bridge(name = "bytevector-length", lib = "(rnrs base builtins (6))")]
pub fn bytevector_len(vec: ByteVector) -> usize {
    vec.0.vec.read().len()
}

#[bridge(name = "vector-set!", lib = "(rnrs base builtins (6))")]
pub fn vector_set_bang(vec: Vector, index: usize, with: Value) -> Result<(), Exception> {
    if !vec.0.mutable {
        return Err(Exception::error("vector is immutable"));
    }

    vec.0
        .vec
        .get(index)
        .ok_or_else(|| Exception::invalid_index(index, vec.0.vec.len()))?
        .set(with);

    Ok(())
}

#[bridge(name = "vector->list", lib = "(rnrs base builtins (6))")]
pub fn vector_to_list(
    from: Value,
    start: Option<usize>,
    end: Option<usize>,
) -> Result<Value, Exception> {
    let vec = VectorIndexer::index(&from, start, end)?;
    let vec_read = &vec.0.vec;
    Ok(heap_values_to_list(vec_read))
}

#[bridge(name = "vector->string", lib = "(rnrs base builtins (6))")]
pub fn vector_to_string(
    from: Value,
    start: Option<usize>,
    end: Option<usize>,
) -> Result<String, Exception> {
    let vec = VectorIndexer::index(&from, start, end)?;
    vec.0
        .vec
        .iter()
        .map(HeapValue::get)
        .map(<Value as TryInto<char>>::try_into)
        .collect::<Result<String, _>>()
}

#[bridge(name = "vector-copy", lib = "(rnrs base builtins (6))")]
pub fn vector_copy(
    from: Value,
    start: Option<usize>,
    end: Option<usize>,
) -> Result<Value, Exception> {
    Ok(Value::from(VectorIndexer::index(&from, start, end)?))
}

#[bridge(name = "vector-copy!", lib = "(rnrs base builtins (6))")]
pub fn vector_copy_to(
    to: Vector,
    at: usize,
    from: Value,
    start: Option<usize>,
    end: Option<usize>,
) -> Result<(), Exception> {
    let to = &to.0.vec;

    if at >= to.len() {
        return Err(Exception::invalid_index(at, to.len()));
    }

    let copies = VectorIndexer::index(&from, start, end)?;
    let copies = &copies.0.vec;
    if copies.len() + at > to.len() {
        return Err(Exception::invalid_range(at..at + copies.len(), to.len()));
    }

    for (dst, src) in to[at..].iter().zip(copies.iter()) {
        dst.set(src.get());
    }

    Ok(())
}

#[bridge(name = "vector-append", lib = "(rnrs base builtins (6))")]
pub fn vector_append(#[rest_args] args: Value) -> Result<Value, Exception> {
    if args.is_null() {
        return Err(Exception::wrong_num_of_var_args(1..usize::MAX, 0));
    }

    let mut appended = Vec::new();
    for arg in iter_list(&args) {
        let vec: Vector = arg.try_into()?;
        appended.extend(vec.0.vec.iter().cloned());
    }
    Ok(Value::from(Vector::from_slots(appended.into_boxed_slice())))
}

#[bridge(name = "vector-fill!", lib = "(rnrs base builtins (6))")]
pub fn vector_fill(
    vector: Vector,
    with: Value,
    start: Value,
    end: Option<usize>,
) -> Result<(), Exception> {
    let vector = &vector.0.vec;

    let start: usize = start.clone().try_into()?;
    let end = match end {
        Some(end) => end,
        None => vector.len(),
    };

    let range = try_make_range(start, end)?;
    if range.end > vector.len() {
        return Err(Exception::invalid_range(range, vector.len()));
    }

    for slot in &vector[range] {
        slot.set(with.clone())
    }

    Ok(())
}

#[bridge(name = "native-endianness", lib = "(rnrs bytevectors (6))")]
pub fn native_endianness() -> Symbol {
    #[cfg(target_endian = "little")]
    {
        Symbol::intern("little")
    }
    #[cfg(target_endian = "big")]
    {
        Symbol::intern("big")
    }
}

#[bridge(name = "bytevector?", lib = "(rnrs bytevectors (6))")]
pub fn bytevector_pred(arg: Value) -> bool {
    arg.is_a::<Embedded<VectorInner<u8>>>()
}

#[bridge(name = "make-bytevector", lib = "(rnrs bytevectors (6))")]
pub fn make_bytevector(k: usize, fill: Option<u8>) -> Result<ByteVector, Exception> {
    let fill = fill.unwrap_or(0);
    Ok(ByteVector::mutable(vec![fill; k]))
}

#[bridge(name = "bytevector-length", lib = "(rnrs bytevectors (6))")]
pub fn bytevector_length(bytevector: ByteVector) -> usize {
    bytevector.len()
}

#[bridge(name = "bytevector=?", lib = "(rnrs bytevectors (6))")]
pub fn bytevector_equal_pred(lhs: ByteVector, rhs: ByteVector) -> bool {
    lhs == rhs
}

#[bridge(name = "bytevector-u8-ref", lib = "(rnrs bytevectors (6))")]
pub fn bytevector_u8_ref(bytevector: ByteVector, k: usize) -> Result<u8, Exception> {
    Ok(bytevector
        .get(k)
        .ok_or_else(|| Exception::invalid_index(k, bytevector.len()))?)
}

#[bridge(name = "bytevector-u8-set!", lib = "(rnrs bytevectors (6))")]
pub fn bytevector_u8_set(bytevector: ByteVector, k: usize, octet: u8) -> Result<(), Exception> {
    let mut slice = bytevector.as_mut_slice()?;
    let len = slice.len();
    *slice
        .get_mut(k)
        .ok_or_else(|| Exception::invalid_index(k, len))? = octet;
    Ok(())
}

#[bridge(name = "u8-list->bytevector", lib = "(rnrs bytevectors (6))")]
pub fn u8_list_to_bytevector(list: List) -> Result<ByteVector, Exception> {
    Ok(ByteVector::mutable(
        list.into_iter()
            .map(u8::try_from)
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

#[bridge(name = "bytevector-push!", lib = "(rnrs bytevectors (6))")]
pub fn bytevector_push(bytevector: ByteVector, byte: u8) -> Result<(), Exception> {
    // Temporary, gets its own gc'd array later
    bytevector.as_mut_vec()?.push(byte);
    Ok(())
}

#[bridge(name = "bytevector-insert!", lib = "(rnrs bytevectors (6))")]
pub fn bytevector_insert(bytevector: ByteVector, idx: usize, byte: u8) -> Result<(), Exception> {
    let mut bv = bytevector.as_mut_vec()?;
    if idx > bv.len() {
        return Err(Exception::invalid_index(idx, bv.len()));
    } else {
        bv.insert(idx, byte);
    }
    Ok(())
}

#[bridge(name = "bytevector-take!", lib = "(rnrs bytevectors (6))")]
pub fn bytevector_take(bytevector: ByteVector) -> Result<ByteVector, Exception> {
    Ok(ByteVector::mutable(std::mem::take(
        &mut *bytevector.as_mut_vec()?,
    )))
}

#[cfg(all(test, feature = "barrier_count", not(feature = "async")))]
mod vector_tests {
    use crate::{ast::ImportPolicy, env::TopLevelEnvironment};
    use scheme_rs_gc::barrier_count;

    /// Runs `setup`, then counts the barriers while `code` runs.
    fn barriers_during(setup: &str, code: &str) -> usize {
        let env = TopLevelEnvironment::new_repl();
        env.eval(ImportPolicy::Allow, "(import (rnrs))").unwrap();
        env.eval(ImportPolicy::Allow, setup).unwrap();
        let before = barrier_count();
        env.eval(ImportPolicy::Allow, code).unwrap();
        barrier_count() - before
    }

    #[test]
    pub fn vector_set_triggers_barrier() {
        let setup = "(define x (make-vector 2 0))";
        let code = "(vector-set! x 0 1)";
        assert_eq!(barriers_during(setup, code), 1);
    }

    #[test]
    pub fn vector_fill_triggers_barrier() {
        let setup = "(define x (make-vector 2 0))";
        let code = "(vector-fill! x 2 0 1)";
        assert_eq!(barriers_during(setup, code), 1);
    }

    #[test]
    pub fn vector_copy_triggers_barrier() {
        let setup = "(define x (make-vector 2 0)) (define y (make-vector 2 1))";
        let code = "(vector-copy! x 0 y)";
        assert_eq!(barriers_during(setup, code), 2);
    }
}
