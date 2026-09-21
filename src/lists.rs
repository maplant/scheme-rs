//! Scheme pairs and lists.

use hashbrown::HashSet;
use indexmap::IndexMap;
use parking_lot::RwLock;

use crate::{
    exceptions::Exception,
    gc::{Gc, Trace},
    proc::{Application, Args, ContBarrier, Procedure},
    registry::bridge,
    strings::WideString,
    value::{UnpackedValue, Value, ValueType, write_value},
};
use std::fmt;

#[derive(Trace)]
#[repr(align(16))]
pub(crate) struct PairInner {
    /// The head of the pair
    pub(crate) car: RwLock<Value>,
    /// The tail of the pair
    pub(crate) cdr: RwLock<Value>,
    /// Whether or not the pair can be modified post creation
    mutable: bool,
}

/// A pair of Scheme [Values](Value). Has a head (the [car](Pair::car())) and a
/// tail (the [cdr](Pair::cdr())).
#[derive(Clone, Trace)]
pub struct Pair(pub(crate) Gc<PairInner>);

impl Pair {
    /// Construct a new mutable Pair from a car and cdr
    pub fn mutable(car: Value, cdr: Value) -> Self {
        Self(Gc::new(PairInner {
            car: RwLock::new(car),
            cdr: RwLock::new(cdr),
            mutable: true,
        }))
    }

    /// Construct a new immutable Pair from a car and cdr
    pub fn immutable(car: Value, cdr: Value) -> Self {
        Self(Gc::new(PairInner {
            car: RwLock::new(car),
            cdr: RwLock::new(cdr),
            mutable: false,
        }))
    }

    /// Extract the car (aka the head) from the Pair.
    pub fn car(&self) -> Value {
        self.0.car.read().clone()
    }

    /// Alias for [`car`](Pair::car())
    pub fn head(&self) -> Value {
        self.car()
    }

    /// Extract the cdr (aka the tail) from the Pair.
    pub fn cdr(&self) -> Value {
        self.0.cdr.read().clone()
    }

    /// Alias for [`cdr`](Pair::cdr())
    pub fn tail(&self) -> Value {
        self.cdr()
    }

    /// Set the car of the Pair. Returns an error if pair is immutable.
    pub fn set_car(&self, new_car: Value) -> Result<(), Exception> {
        if self.0.mutable {
            *self.0.car.write() = new_car;
            Ok(())
        } else {
            Err(Exception::error("pair is not mutable"))
        }
    }

    /// Set the cdr of the Pair. Returns an error if pair is immutable.
    pub fn set_cdr(&self, new_cdr: Value) -> Result<(), Exception> {
        if self.0.mutable {
            *self.0.cdr.write() = new_cdr;
            Ok(())
        } else {
            Err(Exception::error("pair is not mutable"))
        }
    }
}

impl From<Pair> for (Value, Value) {
    fn from(value: Pair) -> Self {
        (value.car(), value.cdr())
    }
}

pub(crate) fn write_list(
    car: &Value,
    cdr: &Value,
    fmt: fn(&Value, &mut IndexMap<Value, bool>, &mut fmt::Formatter<'_>) -> fmt::Result,
    circular_values: &mut IndexMap<Value, bool>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    match cdr.type_of() {
        ValueType::Pair | ValueType::Null => (),
        _ => {
            // This is not a proper list
            write!(f, "(")?;
            write_value(car, fmt, circular_values, f)?;
            write!(f, " . ")?;
            write_value(cdr, fmt, circular_values, f)?;
            write!(f, ")")?;
            return Ok(());
        }
    }

    write!(f, "(")?;
    write_value(car, fmt, circular_values, f)?;
    let mut stack = vec![cdr.clone()];

    while let Some(head) = stack.pop() {
        if let Some((idx, _, seen)) = circular_values.get_full_mut(&head) {
            if *seen {
                write!(f, " . #{idx}#")?;
                continue;
            } else {
                write!(f, " #{idx}=")?;
                *seen = true;
            }
        }
        match &*head.unpacked_ref() {
            UnpackedValue::Null => {
                if !stack.is_empty() {
                    write!(f, " ()")?;
                }
            }
            UnpackedValue::Pair(pair) => {
                let (car, cdr) = pair.clone().into();
                write!(f, " ")?;
                write_value(&car, fmt, circular_values, f)?;
                stack.push(cdr);
            }
            x => {
                let val = x.clone().into_value();
                write!(f, " ")?;
                if stack.is_empty() {
                    write!(f, ". ")?;
                }
                write_value(&val, fmt, circular_values, f)?;
            }
        }
    }

    write!(f, ")")
}

/// A proper list.
///
/// Conversion to this type guarantees that a type is a proper list and allows
/// for fast retrieval of the length or any individual element of the list.
///
/// # Performance
///
/// This is done by copying the list into a `Vec`, which can be a quite
/// expensive operation, so only use this if you need all elements of the list.
pub struct List {
    head: Value,
    items: Vec<Value>,
}

impl List {
    pub fn as_slice(&self) -> &[Value] {
        self.items.as_slice()
    }

    pub fn into_vec(self) -> Vec<Value> {
        self.items
    }

    pub fn len(&self) -> usize {
        self.items.len() - 1
    }

    pub fn is_empty(&self) -> bool {
        self.items.len() == 1
    }
}

impl IntoIterator for List {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl From<List> for Value {
    fn from(value: List) -> Self {
        value.head
    }
}

impl From<&Value> for Option<List> {
    fn from(value: &Value) -> Self {
        let mut seen = HashSet::new();
        let mut cdr = value.clone();
        let mut items = Vec::new();
        while !cdr.is_null() {
            if !seen.insert(cdr.clone()) {
                return None;
            }
            let (car, new_cdr) = cdr.cast()?;
            items.push(car);
            cdr = new_cdr;
        }
        Some(List {
            head: value.clone(),
            items,
        })
    }
}

impl<V> FromIterator<V> for List
where
    V: Into<Value>,
{
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        let items = iter.into_iter().map(Into::into).collect::<Vec<_>>();
        let mut head = Value::null();
        for item in items.iter().rev() {
            head = Value::cons(item.clone(), head);
        }
        Self { head, items }
    }
}

impl From<Vec<Value>> for List {
    fn from(items: Vec<Value>) -> Self {
        let mut head = Value::null();
        for item in items.iter().rev() {
            head = Value::cons(item.clone(), head);
        }
        Self { head, items }
    }
}

impl TryFrom<&Value> for List {
    type Error = Exception;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        value
            .cast::<List>()
            .ok_or_else(|| Exception::error("value is not a proper list"))
    }
}

impl TryFrom<Value> for List {
    type Error = Exception;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

/// An iterator over the elements of a scheme list, fetching each element with
/// car/cdr. Iteration ends at the first non-pair value.
pub struct ListIterator {
    curr: Value,
}

impl Iterator for ListIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        let pair = self.curr.cast::<Pair>()?;
        let car = pair.car();
        self.curr = pair.cdr();
        Some(car)
    }
}

/// Iterate over the elements of a scheme list without collecting them.
pub fn iter_list(list: &Value) -> ListIterator {
    ListIterator { curr: list.clone() }
}

/// Return the number of elements in a scheme list.
pub fn list_len(list: &Value) -> usize {
    iter_list(list).count()
}

/// Split a list at index `at`, returning the first `at` elements as a newly
/// allocated list along with the remainder. The remainder shares structure
/// with the input list.
pub fn split_list(list: Value, at: usize) -> Result<(Value, Value), Exception> {
    if at == 0 {
        return Ok((Value::null(), list));
    }
    let pair: Pair = list.try_to()?;
    let (prefix, tail) = split_list(pair.cdr(), at - 1)?;
    Ok((Value::from(Pair::immutable(pair.car(), prefix)), tail))
}

/// Convert a slice of values to a proper list
pub fn slice_to_list(items: &[Value]) -> Value {
    match items {
        [] => Value::null(),
        [head, tail @ ..] => Value::from(Pair::immutable(head.clone(), slice_to_list(tail))),
    }
}

pub fn list_to_vec(curr: &Value, out: &mut Vec<Value>) {
    match &*curr.unpacked_ref() {
        UnpackedValue::Pair(pair) => {
            let (car, cdr) = pair.clone().into();
            out.push(car);
            list_to_vec(&cdr, out);
        }
        UnpackedValue::Null => (),
        _ => out.push(curr.clone()),
    }
}

pub fn list_to_vec_with_null(curr: &Value, out: &mut Vec<Value>) {
    match &*curr.unpacked_ref() {
        UnpackedValue::Pair(pair) => {
            let (car, cdr) = pair.clone().into();
            out.push(car);
            list_to_vec_with_null(&cdr, out);
        }
        _ => out.push(curr.clone()),
    }
}

pub fn is_list(curr: &Value, seen: &mut HashSet<Value>) -> bool {
    if curr.is_null() {
        return true;
    }

    if !seen.insert(curr.clone()) {
        return false;
    }

    let Some(curr) = curr.cast::<Pair>() else {
        return false;
    };

    is_list(&curr.cdr(), seen)
}

#[bridge(name = "list?", lib = "(rnrs base builtins (6))")]
pub fn list_pred(arg: Value) -> bool {
    is_list(&arg, &mut HashSet::default())
}

#[bridge(name = "list", lib = "(rnrs base builtins (6))")]
pub fn list(#[rest_args] args: Value) -> Value {
    // Rebuild the rest args as a fresh mutable list:
    fn rebuild(args: &Value) -> Value {
        match args.cast::<Pair>() {
            Some(pair) => Value::from(Pair::mutable(pair.car(), rebuild(&pair.cdr()))),
            None => Value::null(),
        }
    }
    rebuild(&args)
}

#[bridge(name = "cons", lib = "(rnrs base builtins (6))")]
pub fn cons(car: Value, cdr: Value, _: &mut ContBarrier) -> Pair {
    Pair::mutable(car.clone(), cdr.clone())
}

#[bridge(name = "car", lib = "(rnrs base builtins (6))")]
pub fn car(val: Pair, _: &mut ContBarrier) -> Value {
    val.car()
}

#[bridge(name = "cdr", lib = "(rnrs base builtins (6))")]
pub fn cdr(val: Pair, _: &mut ContBarrier) -> Value {
    val.cdr()
}

#[bridge(name = "set-car!", lib = "(rnrs mutable-pairs (6))")]
pub fn set_car(pair: Pair, val: Value) -> Result<(), Exception> {
    pair.set_car(val.clone())?;
    Ok(())
}

#[bridge(name = "set-cdr!", lib = "(rnrs mutable-pairs (6))")]
pub fn set_cdr(pair: Pair, val: Value) -> Result<(), Exception> {
    pair.set_cdr(val.clone())?;
    Ok(())
}

#[bridge(name = "length", lib = "(rnrs base builtins (6))")]
pub fn length_builtin(arg: Value) -> Result<usize, Exception> {
    length(&arg)
}

pub fn length(arg: &Value) -> Result<usize, Exception> {
    let mut length = 0usize;
    let mut arg = arg.clone();
    loop {
        arg = {
            match &*arg.unpacked_ref() {
                UnpackedValue::Pair(pair) => pair.cdr(),
                UnpackedValue::Null => break,
                _ => return Err(Exception::error("list must be proper")),
            }
        };
        length += 1;
    }
    Ok(length)
}

#[bridge(name = "list->vector", lib = "(rnrs base builtins (6))")]
pub fn list_to_vector(List { items, .. }: List) -> Value {
    Value::from(items)
}

#[bridge(name = "list->string", lib = "(rnrs base builtins (6))")]
pub fn list_to_string(List { items, .. }: List) -> Result<WideString, Exception> {
    let chars = items
        .into_iter()
        .map(char::try_from)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(WideString::mutable(chars))
}

pub(crate) fn append_list(list: &Value, tail: Value) -> Value {
    match &*list.unpacked_ref() {
        UnpackedValue::Pair(pair) => {
            let (car, cdr) = pair.clone().into();
            Value::from(Pair::mutable(car, append_list(&cdr, tail)))
        }
        UnpackedValue::Null => tail,
        _ => Value::from(Pair::mutable(list.clone(), tail)),
    }
}

#[bridge(name = "append", lib = "(rnrs base builtins (6))")]
pub fn append(#[rest_args] lists: Value) -> Result<Value, Exception> {
    fn append_lists(lists: &Value) -> Value {
        let Some(pair) = lists.cast::<Pair>() else {
            return Value::null();
        };
        let cdr = pair.cdr();
        if cdr.is_null() {
            pair.car()
        } else {
            append_list(&pair.car(), append_lists(&cdr))
        }
    }

    Ok(append_lists(&lists))
}

fn split_heads(inputs: &Value) -> Result<Option<(Value, Value)>, Exception> {
    let Some(inputs) = inputs.cast::<Pair>() else {
        return Ok(Some((Value::null(), Value::null())));
    };
    let input = inputs.car();
    if input.type_of() == ValueType::Null {
        // TODO: Check if the rest are also empty
        return Ok(None);
    }
    let (car, cdr) = input.try_to::<Pair>()?.into();
    let Some((cars, cdrs)) = split_heads(&inputs.cdr())? else {
        return Ok(None);
    };
    Ok(Some((
        Value::from(Pair::immutable(car, cars)),
        Value::from(Pair::immutable(cdr, cdrs)),
    )))
}

/// Append a mapper result to the output list, returning the updated head and
/// tail. The list is built in order with mutable pairs; a null tail means the
/// list is empty.
fn push_map_output(head: Value, tail: Value, val: Value) -> (Value, Value) {
    let new_tail = Value::from(Pair::mutable(val, Value::null()));
    if let Some(tail) = tail.cast::<Pair>() {
        tail.set_cdr(new_tail.clone()).unwrap();
        (head, new_tail)
    } else {
        (new_tail.clone(), new_tail)
    }
}

#[bridge(name = "map", lib = "(rnrs base builtins (6))")]
pub fn map(
    mapper: Value,
    list_1: Value,
    #[rest_args] list_n: Value,
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let mapper_proc: Procedure = mapper.clone().try_into()?;

    if list_n.is_null() {
        if list_1.type_of() == ValueType::Null {
            return Ok(barrier.call_cont(Args::pack([Value::null()])));
        }
        let (car, rest) = list_1.try_to::<Pair>()?.into();
        barrier.push_cont([mapper, rest, Value::null(), Value::null()], map1_k);
        return Ok(Application::new(mapper_proc, Args::pack([car])));
    }

    let inputs = Value::from(Pair::immutable(list_1, list_n));
    let Some((args, next_inputs)) = split_heads(&inputs)? else {
        return Ok(barrier.call_cont(Args::pack([Value::null()])));
    };

    barrier.push_cont([mapper, next_inputs, Value::null(), Value::null()], mapn_k);
    Ok(Application::new(mapper_proc, Args::from_list(args)))
}

fn map1_k(env: [Value; 4], mapped: Value, barrier: &mut ContBarrier) -> Application {
    let [mapper, input, head, tail] = env;

    let (head, tail) = push_map_output(head, tail, mapped);

    if input.type_of() == ValueType::Null {
        return barrier.call_cont(Args::pack([head]));
    }
    match input.try_to::<Pair>() {
        Ok(pair) => {
            let (car, rest) = pair.into();
            let mapper_proc = mapper.cast::<Procedure>().unwrap();
            barrier.push_cont([mapper, rest, head, tail], map1_k);
            Application::new(mapper_proc, Args::pack([car]))
        }
        Err(err) => crate::exceptions::raise(err.into(), barrier),
    }
}

fn mapn_k(env: [Value; 4], mapped: Value, barrier: &mut ContBarrier) -> Application {
    let [mapper, inputs, head, tail] = env;

    let (head, tail) = push_map_output(head, tail, mapped);

    match split_heads(&inputs) {
        Ok(Some((args, next_inputs))) => {
            let mapper_proc = mapper.cast::<Procedure>().unwrap();
            barrier.push_cont([mapper, next_inputs, head, tail], mapn_k);
            Application::new(mapper_proc, Args::from_list(args))
        }
        Ok(None) => barrier.call_cont(Args::pack([head])),
        Err(err) => crate::exceptions::raise(err.into(), barrier),
    }
}

#[bridge(name = "zip", lib = "(rnrs base builtins (6))")]
pub fn zip(list1: Value, #[rest_args] listn: Value) -> Result<Value, Exception> {
    fn zip_one(list: &Value, output: &mut Option<Vec<Value>>) -> Result<(), Exception> {
        let List { items, .. } = list.try_to()?;
        if let Some(output) = &output {
            if output.len() != items.len() {
                return Err(Exception::error("lists do not have the same length"));
            }
        } else {
            *output = Some(vec![Value::null(); items.len()]);
        }

        let output = output.as_mut().unwrap();
        for (i, item) in items.into_iter().enumerate() {
            output[i] = Value::cons(item, output[i].clone());
        }
        Ok(())
    }

    // The lists are processed back to front:
    fn zip_rev(lists: &Value, output: &mut Option<Vec<Value>>) -> Result<(), Exception> {
        let Some(pair) = lists.cast::<Pair>() else {
            return Ok(());
        };
        zip_rev(&pair.cdr(), output)?;
        zip_one(&pair.car(), output)
    }

    let mut output: Option<Vec<Value>> = None;
    zip_rev(&listn, &mut output)?;
    zip_one(&list1, &mut output)?;

    if let Some(output) = output {
        Ok(slice_to_list(&output))
    } else {
        Ok(Value::null())
    }
}
