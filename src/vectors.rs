use crate::{
    exceptions::Condition,
    gc::{Gc, Trace},
    lists::slice_to_list,
    num::Number,
    registry::bridge,
    value::{Value, write_value},
};
use indexmap::IndexMap;
use malachite::Integer;
use parking_lot::RwLock;
// use parking_lot::RwLock;
use std::{
    clone::Clone,
    fmt,
    ops::{Deref, DerefMut, Range},
    sync::Arc,
};

/*
#[derive(Trace)]
#[repr(align(16))]
pub(crate) struct VectorInner<T: Trace> {
    /// Inner vector
    pub(crate) vec: RwLock<Vec<T>>,
    /// Whether or not the vector is mutable
    pub(crate) mutable: bool,
}
*/

/// A vector aligned to 16 bytes.
#[derive(Trace)]
#[repr(align(16))]
pub struct AlignedVector<T: Trace>(pub Vec<T>);

impl<T: Trace> AlignedVector<T> {
    pub fn new(v: Vec<T>) -> Self {
        Self(v)
    }
}

impl<T: Trace> Deref for AlignedVector<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Trace> DerefMut for AlignedVector<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Trace + PartialEq> PartialEq for AlignedVector<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.0 == rhs.0
    }
}

pub(crate) fn write_vec(
    v: &Gc<RwLock<AlignedVector<Value>>>,
    fmt: fn(&Value, &mut IndexMap<Value, bool>, &mut fmt::Formatter<'_>) -> fmt::Result,
    circular_values: &mut IndexMap<Value, bool>,
    f: &mut fmt::Formatter<'_>,
) -> Result<(), fmt::Error> {
    write!(f, "#(")?;

    let v = v.read();
    let mut iter = v.iter().peekable();
    while let Some(next) = iter.next() {
        write_value(next, fmt, circular_values, f)?;
        if iter.peek().is_some() {
            write!(f, " ")?;
        }
    }

    write!(f, ")")
}

pub(crate) fn write_bytevec(
    v: &AlignedVector<u8>,
    f: &mut fmt::Formatter<'_>,
) -> Result<(), fmt::Error> {
    write!(f, "#u8(")?;

    let mut iter = v.iter().peekable();
    while let Some(next) = iter.next() {
        write!(f, "{next}")?;
        if iter.peek().is_some() {
            write!(f, " ")?;
        }
    }

    write!(f, ")")
}

fn try_make_range(start: usize, end: usize) -> Result<Range<usize>, Condition> {
    if end < start {
        Err(Condition::error(format!(
            "Range end {end} cannot be less than start {start}",
        )))
    } else {
        Ok(start..end)
    }
}

fn try_to_usize(n: &Value) -> Result<usize, Condition> {
    n.clone()
        .try_into()
        .and_then(|n: Arc<Number>| n.as_ref().try_into())
}

trait Indexer {
    type Collection;

    fn get_len(_: &Self::Collection) -> usize;

    fn get_range(_: &Self::Collection, _: Range<usize>) -> Self::Collection;

    fn try_get(_: &Value) -> Result<Self::Collection, Condition>;

    fn index(from: &Value, range: &[Value]) -> Result<Self::Collection, Condition> {
        let collection = Self::try_get(from)?;
        let len = Self::get_len(&collection);

        let start: usize = range.first().map(try_to_usize).transpose()?.unwrap_or(0);
        let end: usize = range.get(1).map(try_to_usize).transpose()?.unwrap_or(len);

        let range = try_make_range(start, end)?;
        if range.end > len {
            return Err(Condition::invalid_range(range, len));
        }

        Ok(Self::get_range(&collection, range))
    }
}

struct VectorIndexer;

impl Indexer for VectorIndexer {
    type Collection = Gc<RwLock<AlignedVector<Value>>>;

    fn get_len(vec: &Self::Collection) -> usize {
        vec.read().len()
    }

    fn get_range(vec: &Self::Collection, range: Range<usize>) -> Self::Collection {
        let subvec: Vec<Value> = vec
            .read()
            .iter()
            .skip(range.start)
            .take(range.end - range.start)
            .cloned()
            .collect();
        Gc::new(RwLock::new(AlignedVector::new(subvec)))
    }

    fn try_get(val: &Value) -> Result<Self::Collection, Condition> {
        val.clone().try_into()
    }
}

#[bridge(name = "make-vector", lib = "(rnrs base builtins (6))")]
pub fn make_vector(n: &Value, with: &[Value]) -> Result<Vec<Value>, Condition> {
    let n: Arc<Number> = n.clone().try_into()?;
    let n: usize = n.as_ref().try_into()?;

    Ok(vec![Value::from(
        (0..n)
            .map(|_| with.first().cloned().unwrap_or_else(Value::null))
            .collect::<Vec<_>>(),
    )])
}

#[bridge(name = "vector", lib = "(rnrs base builtins (6))")]
pub fn vector(args: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(args.to_vec())])
}

#[bridge(name = "vector-ref", lib = "(rnrs base builtins (6))")]
pub fn vector_ref(vec: &Value, index: &Value) -> Result<Vec<Value>, Condition> {
    let vec: Gc<RwLock<AlignedVector<Value>>> = vec.clone().try_into()?;
    let index: usize = try_to_usize(index)?;
    let vec_read = vec.read();

    Ok(vec![
        vec_read
            .get(index)
            .ok_or_else(|| Condition::invalid_index(index, vec_read.len()))?
            .clone(),
    ])
}

#[bridge(name = "vector-length", lib = "(rnrs base builtins (6))")]
pub fn vector_len(vec: &Value) -> Result<Vec<Value>, Condition> {
    let vec: Gc<RwLock<AlignedVector<Value>>> = vec.clone().try_into()?;
    let len = vec.read().len();

    Ok(vec![Value::from(match i64::try_from(len) {
        Ok(len) => Number::FixedInteger(len),
        Err(_) => Number::BigInteger(Integer::from(len)),
    })])
}

#[bridge(name = "bytevector-length", lib = "(rnrs base builtins (6))")]
pub fn bytevector_len(vec: &Value) -> Result<Vec<Value>, Condition> {
    let vec: Arc<AlignedVector<u8>> = vec.clone().try_into()?;
    let len = vec.len();

    Ok(vec![Value::from(match i64::try_from(len) {
        Ok(len) => Number::FixedInteger(len),
        Err(_) => Number::BigInteger(Integer::from(len)),
    })])
}

#[bridge(name = "vector-set!", lib = "(rnrs base builtins (6))")]
pub fn vector_set(vec: &Value, index: &Value, with: &Value) -> Result<Vec<Value>, Condition> {
    let vec: Gc<RwLock<AlignedVector<Value>>> = vec.clone().try_into()?;
    let vec_len = vec.read().len();

    let index: usize = try_to_usize(index)?;

    *vec.write()
        .get_mut(index)
        .ok_or_else(|| Condition::invalid_index(index, vec_len))? = with.clone();

    Ok(vec![])
}

#[bridge(name = "vector->list", lib = "(rnrs base builtins (6))")]
pub fn vector_to_list(from: &Value, range: &[Value]) -> Result<Vec<Value>, Condition> {
    let vec = VectorIndexer::index(from, range)?;
    let vec_read = vec.read();
    Ok(vec![slice_to_list(&vec_read)])
}

#[bridge(name = "vector->string", lib = "(rnrs base builtins (6))")]
pub fn vector_to_string(from: &Value, range: &[Value]) -> Result<Vec<Value>, Condition> {
    let vec = VectorIndexer::index(from, range)?;
    let vec_read = vec.read();
    Ok(vec![Value::from(
        vec_read
            .iter()
            .cloned()
            .map(<Value as TryInto<char>>::try_into)
            .collect::<Result<String, _>>()?,
    )])
}

#[bridge(name = "vector-copy", lib = "(rnrs base builtins (6))")]
pub fn vector_copy(from: &Value, range: &[Value]) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(VectorIndexer::index(from, range)?)])
}

#[bridge(name = "vector-copy!", lib = "(rnrs base builtins (6))")]
pub fn vector_copy_to(
    to: &Value,
    at: &Value,
    from: &Value,
    range: &[Value],
) -> Result<Vec<Value>, Condition> {
    let to: Gc<RwLock<AlignedVector<Value>>> = to.clone().try_into()?;
    let mut to = to.write();

    let at: usize = try_to_usize(at)?;

    if at >= to.len() {
        return Err(Condition::invalid_index(at, to.len()));
    }

    let copies = VectorIndexer::index(from, range)?;
    let copies = copies.read();
    if copies.len() + at >= to.len() {
        return Err(Condition::invalid_range(at..at + copies.len(), to.len()));
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

    Ok(vec![])
}

#[bridge(name = "vector-append", lib = "(rnrs base builtins (6))")]
pub fn vector_append(args: &[Value]) -> Result<Vec<Value>, Condition> {
    if args.is_empty() {
        return Err(Condition::wrong_num_of_var_args(1..usize::MAX, 0));
    }

    Ok(vec![Value::from(
        args.iter()
            .map(|arg| {
                let vec: Gc<RwLock<AlignedVector<Value>>> = arg.clone().try_into()?;
                let vec_read = vec.read();
                Ok(vec_read.iter().cloned().collect::<Vec<_>>())
            })
            .collect::<Result<Vec<_>, Condition>>()?
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
) -> Result<Vec<Value>, Condition> {
    let vector: Gc<RwLock<AlignedVector<Value>>> = vector.clone().try_into()?;
    let mut vector = vector.write();

    let start: usize = try_to_usize(start)?;
    let end = match end.first() {
        Some(end) => try_to_usize(end)?,
        None => vector.len(),
    };

    let range = try_make_range(start, end)?;
    if range.end > vector.len() {
        return Err(Condition::invalid_range(range, vector.len()));
    }

    range.for_each(|i| {
        if let Some(slot) = vector.get_mut(i) {
            *slot = with.clone()
        }
    });

    Ok(vec![])
}
