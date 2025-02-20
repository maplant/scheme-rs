use crate::{
    exception::Exception, gc::Gc, lists::slice_to_list, num::Number, registry::bridge, value::Value,
};
use rug::Integer;
use std::{clone::Clone, cmp::Ord, error::Error as StdError, fmt::Display, ops::Range};

fn range(start: usize, end: usize) -> Result<Range<usize>, Exception> {
    if end < start {
        Err(Exception::error(format!(
            "Invalid range: start {start} is lesser than end {end}"
        )))
    } else {
        Ok(start..end)
    }
}

trait Indexer {
    type Collection;

    fn get_len(&self, _: &Self::Collection) -> usize;
    fn get_range(&self, _: &Self::Collection, _: Range<usize>) -> Self::Collection;
    fn try_get<'a>(&self, _: &'a Value) -> Result<&'a Self::Collection, Exception>;

    fn index(&self, args: &[Gc<Value>]) -> Result<Self::Collection, Exception> {
        fn try_to_u64(n: &Gc<Value>) -> Result<u64, Exception> {
            n.read().as_ref().try_into().map(|n: &Number| n.to_u64())
        }

        let arg = args
            .first()
            .ok_or_else(|| Exception::wrong_num_of_variadic_args(1..3, 0))?;
        let arg = arg.read();
        let collection = self.try_get(&arg)?;
        let len = self.get_len(collection);

        let start: usize = args
            .get(1)
            .map(try_to_u64)
            .transpose()?
            .unwrap_or(0)
            .try_into()?;
        let end: usize = args
            .get(2)
            .map(try_to_u64)
            .transpose()?
            .map(|end| <u64 as TryInto<usize>>::try_into(end))
            .transpose()?
            .unwrap_or(len.try_into()?);

        if end < start {
            return Err(Exception::error(format!(
                "Range end {} cannot be less than start {}",
                end, start
            )));
        }
        if start >= len {
            return Err(Exception::invalid_range(
                start.try_into()?..end.try_into()?,
                len,
            ));
        }

        Ok(self.get_range(collection, start..end))
    }
}

struct StringIndexer;
impl Indexer for StringIndexer {
    type Collection = String;

    fn get_len(&self, string: &String) -> usize {
        string.chars().count()
    }
    fn get_range(&self, string: &String, range: Range<usize>) -> String {
        string.chars()
            .skip(range.start)
            .take(range.end - range.start)
            .collect()
    }
    fn try_get<'a>(&self, val: &'a Value) -> Result<&'a String, Exception> {
        val.try_into()
    }
}
struct VectorIndexer;
impl Indexer for VectorIndexer {
    type Collection = Vec<Value>;

    fn get_len(&self, vec: &Vec<Value>) -> usize {
        vec.len()
    }
    fn get_range(&self, vec: &Vec<Value>, range: Range<usize>) -> Self::Collection {
        vec.iter()
            .skip(range.start)
            .take(range.end - range.start)
            .cloned()
            .collect()
    }
    fn try_get<'a>(&self, val: &'a Value) -> Result<&'a Vec<Value>, Exception> {
        val.try_into()
    }
}

#[bridge(name = "vector?", lib = "(base)")]
pub async fn is_vector(arg: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    Ok(vec![Gc::new(Value::Boolean(matches!(
        &*arg.read(),
        Value::Vector(_)
    )))])
}

#[bridge(name = "make-vector", lib = "(base)")]
pub async fn make_vector(args: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Exception> {
    let n = args
        .first()
        .ok_or_else(|| Exception::wrong_num_of_variadic_args(1..2, 0))?
        .read();
    let n: &Number = n.as_ref().try_into()?;
    let n = n.to_u64();

    let with = args.get(1);

    Ok((0..n)
        .map(|_| with.cloned().unwrap_or(Gc::new(Value::Null)))
        .collect())
}

#[bridge(name = "vector", lib = "(base)")]
pub async fn vector(args: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Exception> {
    Ok(vec![Gc::new(Value::Vector(
        args.iter()
            .map(Gc::read)
            .map(|guard| guard.as_ref().clone())
            .collect(),
    ))])
}

#[bridge(name = "vector-ref", lib = "(base)")]
pub async fn vector_ref(vec: &Gc<Value>, index: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let vec = vec.read();
    let vec: &Vec<Value> = vec.as_ref().try_into()?;

    let index = index.read();
    let index: &Number = index.as_ref().try_into()?;
    let index: usize = index.to_u64().try_into()?;

    Ok(vec![Gc::new(
        vec.get(index)
            .ok_or_else(|| Exception::invalid_index(index, vec.len()))?
            .clone(),
    )])
}

#[bridge(name = "vector-length", lib = "(base)")]
pub async fn vector_len(vec: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let vec = vec.read();
    let vec: &Vec<Value> = vec.as_ref().try_into()?;

    Ok(vec![Gc::new(Value::Number(
        match i64::try_from(vec.len()) {
            Ok(len) => Number::FixedInteger(len),
            Err(_) => Number::BigInteger(Integer::from(vec.len())),
        },
    ))])
}

#[bridge(name = "vector-set!", lib = "(base)")]
pub async fn vector_set(
    vec: &Gc<Value>,
    index: &Gc<Value>,
    with: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, Exception> {
    let mut vec = vec.write();
    let vec: &mut Vec<Value> = vec.as_mut().try_into()?;
    let vec_len = vec.len();

    let index = index.read();
    let index: &Number = index.as_ref().try_into()?;
    let index: usize = index.to_u64().try_into()?;

    let index = vec
        .get_mut(index)
        .ok_or_else(|| Exception::invalid_index(index, vec_len))?;
    *index = with.read().clone();

    Ok(vec![])
}

#[bridge(name = "vector->list", lib = "(base)")]
pub async fn vector_to_list(args: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Exception> {
    let vec: Vec<Gc<Value>> = VectorIndexer.index(args)?.into_iter().map(Gc::new).collect();
    Ok(vec![Gc::new(slice_to_list(vec.as_slice()))])
}

#[bridge(name = "vector->string", lib = "(base)")]
pub async fn vector_to_string(args: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Exception> {
    Ok(vec![Gc::new(Value::String(
        VectorIndexer.index(args)?
            .into_iter()
            .map(|val| <Value as TryInto<char>>::try_into(val))
            .collect::<Result<String, _>>()?,
    ))])
}

#[bridge(name = "string->vector", lib = "(base)")]
pub async fn string_to_vector(args: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Exception> {
    Ok(vec![Gc::new(
        Value::Vector(StringIndexer.index(args)?.chars().map(|c| Value::Character(c)).collect())
    )])
}
