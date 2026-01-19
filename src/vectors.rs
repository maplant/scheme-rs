//! Growable mutable vectors.

use crate::{
    exceptions::Exception,
    gc::{Gc, Trace},
    lists::slice_to_list,
    registry::bridge,
    value::{Value, write_value},
};
use indexmap::IndexMap;
use parking_lot::{
    MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
};
use std::{clone::Clone, fmt, hash::Hash, ops::Range, sync::Arc};

#[derive(Trace)]
#[repr(align(16))]
pub(crate) struct VectorInner<T: Trace> {
    /// Inner vector.
    pub(crate) vec: RwLock<Vec<T>>,
    /// Whether or not the vector is mutable
    pub(crate) mutable: bool,
}

/// A vector of values
#[derive(Clone, Trace)]
pub struct Vector(pub(crate) Gc<VectorInner<Value>>);

impl Vector {
    pub fn new(vec: Vec<Value>) -> Self {
        Self::from(vec)
    }

    // TODO: Add more convenience functions here

    pub fn to_list(&self) -> Value {
        slice_to_list(&self.0.vec.read())
    }

    pub fn clone_inner_vec(&self) -> Vec<Value> {
        self.0.vec.read().clone()
    }
}

impl From<Vec<Value>> for Vector {
    fn from(vec: Vec<Value>) -> Self {
        Self(Gc::new(VectorInner {
            vec: RwLock::new(vec),
            mutable: false,
        }))
    }
}

/// A vector of bytes
#[derive(Clone, Trace)]
pub struct ByteVector(pub(crate) Arc<VectorInner<u8>>);

impl ByteVector {
    pub fn new(vec: Vec<u8>) -> Self {
        Self::from(vec)
    }

    pub fn as_slice(&self) -> MappedRwLockReadGuard<'_, [u8]> {
        RwLockReadGuard::map(self.0.vec.read(), |vec| vec.as_slice())
    }

    pub fn as_mut_slice(&self) -> MappedRwLockWriteGuard<'_, [u8]> {
        RwLockWriteGuard::map(self.0.vec.write(), |vec| vec.as_mut_slice())
    }

    pub fn as_mut_vec(&self) -> RwLockWriteGuard<'_, Vec<u8>> {
        self.0.vec.write()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.0.vec.read().len()
    }

    pub fn clear(&self) {
        self.0.vec.write().clear();
    }

    pub fn get(&self, idx: usize) -> Option<u8> {
        self.0.vec.read().get(idx).copied()
    }
}

impl From<Vec<u8>> for ByteVector {
    fn from(vec: Vec<u8>) -> Self {
        Self(Arc::new(VectorInner {
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

pub(crate) fn write_vec(
    v: &Vector,
    fmt: fn(&Value, &mut IndexMap<Value, bool>, &mut fmt::Formatter<'_>) -> fmt::Result,
    circular_values: &mut IndexMap<Value, bool>,
    f: &mut fmt::Formatter<'_>,
) -> Result<(), fmt::Error> {
    write!(f, "#(")?;

    let values = v.0.vec.read();

    for (i, value) in values.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write_value(value, fmt, circular_values, f)?;
    }

    write!(f, ")")
}

pub(crate) fn write_bytevec(v: &ByteVector, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
    write!(f, "#vu8(")?;

    let bytes = v.0.vec.read();
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
            "Range end {end} cannot be less than start {start}",
        )))
    } else {
        Ok(start..end)
    }
}

trait Indexer {
    type Collection: TryFrom<Value, Error = Exception>;

    fn get_len(_: &Self::Collection) -> usize;

    fn get_range(_: &Self::Collection, _: Range<usize>) -> Self::Collection;

    fn index(from: &Value, range: &[Value]) -> Result<Self::Collection, Exception> {
        let collection = Self::Collection::try_from(from.clone())?;
        let len = Self::get_len(&collection);

        let start: usize = range
            .first()
            .cloned()
            .map(Value::try_into)
            .transpose()?
            .unwrap_or(0);
        let end: usize = range
            .get(1)
            .cloned()
            .map(Value::try_into)
            .transpose()?
            .unwrap_or(len);

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
        vec.0.vec.read().len()
    }

    fn get_range(vec: &Self::Collection, range: Range<usize>) -> Self::Collection {
        let subvec: Vec<Value> = vec
            .0
            .vec
            .read()
            .iter()
            .skip(range.start)
            .take(range.end - range.start)
            .cloned()
            .collect();
        Vector(Gc::new(VectorInner {
            vec: RwLock::new(subvec),
            mutable: true,
        }))
    }
}

#[bridge(name = "make-vector", lib = "(rnrs base builtins (6))")]
pub fn make_vector(n: &Value, with: &[Value]) -> Result<Vec<Value>, Exception> {
    let n: usize = n.try_to_scheme_type()?;

    Ok(vec![Value::from(Vector(Gc::new(VectorInner {
        vec: RwLock::new(
            (0..n)
                .map(|_| with.first().cloned().unwrap_or_else(Value::null))
                .collect::<Vec<_>>(),
        ),
        mutable: true,
    })))])
}

#[bridge(name = "vector", lib = "(rnrs base builtins (6))")]
pub fn vector(args: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(Vector(Gc::new(VectorInner {
        vec: RwLock::new(args.to_vec()),
        mutable: true,
    })))])
}

#[bridge(name = "vector-ref", lib = "(rnrs base builtins (6))")]
pub fn vector_ref(vec: &Value, index: &Value) -> Result<Vec<Value>, Exception> {
    let vec: Vector = vec.clone().try_into()?;
    let index: usize = index.clone().try_into()?;
    let vec_read = vec.0.vec.read();

    Ok(vec![
        vec_read
            .get(index)
            .ok_or_else(|| Exception::invalid_index(index, vec_read.len()))?
            .clone(),
    ])
}

#[bridge(name = "vector-length", lib = "(rnrs base builtins (6))")]
pub fn vector_len(vec: &Value) -> Result<Vec<Value>, Exception> {
    let vec: Vector = vec.clone().try_into()?;
    let len = vec.0.vec.read().len();

    Ok(vec![Value::from(len)])
}

#[bridge(name = "bytevector-length", lib = "(rnrs base builtins (6))")]
pub fn bytevector_len(vec: &Value) -> Result<Vec<Value>, Exception> {
    let vec: ByteVector = vec.clone().try_into()?;
    let len = vec.0.vec.read().len();

    Ok(vec![Value::from(len)])
}

#[bridge(name = "vector-set!", lib = "(rnrs base builtins (6))")]
pub fn vector_set_bang(vec: &Value, index: &Value, with: &Value) -> Result<Vec<Value>, Exception> {
    let vec: Vector = vec.clone().try_into()?;

    if !vec.0.mutable {
        return Err(Exception::error("vector is immutable"));
    }

    let mut vec_write = vec.0.vec.write();
    let vec_len = vec_write.len();
    let index: usize = index.clone().try_into()?;

    *vec_write
        .get_mut(index)
        .ok_or_else(|| Exception::invalid_index(index, vec_len))? = with.clone();

    Ok(vec![])
}

#[bridge(name = "vector->list", lib = "(rnrs base builtins (6))")]
pub fn vector_to_list(from: &Value, range: &[Value]) -> Result<Vec<Value>, Exception> {
    let vec = VectorIndexer::index(from, range)?;
    let vec_read = vec.0.vec.read();
    Ok(vec![slice_to_list(&vec_read)])
}

#[bridge(name = "vector->string", lib = "(rnrs base builtins (6))")]
pub fn vector_to_string(from: &Value, range: &[Value]) -> Result<Vec<Value>, Exception> {
    let vec = VectorIndexer::index(from, range)?;
    let vec_read = vec.0.vec.read();
    Ok(vec![Value::from(
        vec_read
            .iter()
            .cloned()
            .map(<Value as TryInto<char>>::try_into)
            .collect::<Result<String, _>>()?,
    )])
}

#[bridge(name = "vector-copy", lib = "(rnrs base builtins (6))")]
pub fn vector_copy(from: &Value, range: &[Value]) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(VectorIndexer::index(from, range)?)])
}

#[bridge(name = "vector-copy!", lib = "(rnrs base builtins (6))")]
pub fn vector_copy_to(
    to: &Value,
    at: &Value,
    from: &Value,
    range: &[Value],
) -> Result<Vec<Value>, Exception> {
    let to: Vector = to.clone().try_into()?;
    let mut to = to.0.vec.write();

    let at: usize = at.clone().try_into()?;

    if at >= to.len() {
        return Err(Exception::invalid_index(at, to.len()));
    }

    let copies = VectorIndexer::index(from, range)?;
    let copies = copies.0.vec.read();
    if copies.len() + at >= to.len() {
        return Err(Exception::invalid_range(at..at + copies.len(), to.len()));
    }

    copies
        .iter()
        .enumerate()
        .map(|(i, copy)| (i + at, copy))
        .for_each(|(i, copy)| {
            if let Some(i) = to.get_mut(i) {
                *i = copy.clone();
            }
        });

    Ok(Vec::new())
}

#[bridge(name = "vector-append", lib = "(rnrs base builtins (6))")]
pub fn vector_append(args: &[Value]) -> Result<Vec<Value>, Exception> {
    if args.is_empty() {
        return Err(Exception::wrong_num_of_var_args(1..usize::MAX, 0));
    }

    Ok(vec![Value::from(
        args.iter()
            .map(|arg| {
                let vec: Vector = arg.clone().try_into()?;
                let vec_read = vec.0.vec.read();
                Ok(vec_read.clone())
            })
            .collect::<Result<Vec<_>, Exception>>()?
            .into_iter()
            .flatten()
            .collect::<Vec<_>>(),
    )])
}

#[bridge(name = "vector-fill!", lib = "(rnrs base builtins (6))")]
pub fn vector_fill(
    vector: &Value,
    with: &Value,
    start: &Value,
    end: &[Value],
) -> Result<Vec<Value>, Exception> {
    let vector: Vector = vector.clone().try_into()?;
    let mut vector = vector.0.vec.write();

    let start: usize = start.clone().try_into()?;
    let end = match end.first() {
        Some(end) => end.clone().try_into()?,
        None => vector.len(),
    };

    let range = try_make_range(start, end)?;
    if range.end > vector.len() {
        return Err(Exception::invalid_range(range, vector.len()));
    }

    range.for_each(|i| {
        if let Some(slot) = vector.get_mut(i) {
            *slot = with.clone()
        }
    });

    Ok(vec![])
}
