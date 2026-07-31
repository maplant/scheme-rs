//! Scheme pairs and lists.

use hashbrown::HashSet;
use indexmap::IndexMap;
use parking_lot::RwLock;

use crate::{
    exceptions::Exception,
    gc::{Gc, Trace},
    proc::{Application, ContBarrier, ContPtr, Procedure},
    registry::{bridge, cps_bridge},
    strings::WideString,
    value::{UnpackedValue, Value, ValueType, write_value},
    vectors::Vector,
};
use std::fmt;
use std::mem::MaybeUninit;

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
            head = Value::from((item.clone(), head));
        }
        Self { head, items }
    }
}

impl From<Vec<Value>> for List {
    fn from(items: Vec<Value>) -> Self {
        let mut head = Value::null();
        for item in items.iter().rev() {
            head = Value::from((item.clone(), head));
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
pub fn list_pred(arg: &Value) -> bool {
    is_list(arg, &mut HashSet::default())
}

#[bridge(name = "list", lib = "(rnrs base builtins (6))")]
pub fn list(args: &[Value]) -> Value {
    // Construct the list in reverse
    let mut cdr = Value::null();
    for arg in args.iter().rev() {
        cdr = Value::from(Pair::mutable(arg.clone(), cdr));
    }
    cdr
}

#[bridge(name = "cons", lib = "(rnrs base builtins (6))")]
pub fn cons(car: &Value, cdr: &Value) -> Pair {
    Pair::mutable(car.clone(), cdr.clone())
}

#[bridge(name = "car", lib = "(rnrs base builtins (6))")]
pub fn car(val: Pair) -> Value {
    val.car()
}

#[bridge(name = "cdr", lib = "(rnrs base builtins (6))")]
pub fn cdr(val: Pair) -> Value {
    val.cdr()
}

#[bridge(name = "set-car!", lib = "(rnrs mutable-pairs (6))")]
pub fn set_car(pair: Pair, val: &Value) -> Result<(), Exception> {
    pair.set_car(val.clone())?;
    Ok(())
}

#[bridge(name = "set-cdr!", lib = "(rnrs mutable-pairs (6))")]
pub fn set_cdr(pair: Pair, val: &Value) -> Result<(), Exception> {
    pair.set_cdr(val.clone())?;
    Ok(())
}

#[bridge(name = "length", lib = "(rnrs base builtins (6))")]
pub fn length_builtin(arg: &Value) -> Result<usize, Exception> {
    length(arg)
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

#[bridge(name = "append", lib = "(rnrs base builtins (6))")]
pub fn append(lists: &[Value]) -> Result<Value, Exception> {
    if lists.is_empty() {
        return Ok(Value::null());
    }
    if lists.len() == 1 {
        return Ok(lists[0].clone());
    }
    let mut result = lists.last().unwrap().clone();
    for list in lists[..lists.len() - 1].iter().rev() {
        let mut vec = Vec::new();
        list_to_vec(list, &mut vec);
        for item in vec.into_iter().rev() {
            result = Value::from(Pair::mutable(item, result));
        }
    }
    Ok(result)
}

#[cps_bridge(def = "map proc list1 . listn", lib = "(rnrs base builtins (6))")]
pub fn map(
    _env: &[Value],
    args: &[Value],
    list_n: &[Value],
    barrier: &mut ContBarrier,
) -> Result<Application, Exception> {
    let [mapper, list_1] = args else {
        unreachable!()
    };
    let mapper_proc: Procedure = mapper.clone().try_into()?;
    let mut inputs = Some(list_1.clone())
        .into_iter()
        .chain(list_n.iter().cloned())
        .collect::<Vec<_>>();

    let mut args = Vec::new();
    for input in inputs.iter_mut() {
        if input.type_of() == ValueType::Null {
            // TODO: Check if the rest are also empty and args is empty
            return Ok(barrier.call_cont(vec![Value::null()]));
        }

        let (car, cdr) = input.try_to::<Pair>()?.into();

        args.push(car);
        *input = cdr;
    }

    // The return continuation `map_k` is pushed onto the barrier; the outer
    // continuation (where the final list is returned) stays implicit below it.
    barrier.push_cont(
        vec![
            Value::from(Vec::<Value>::new()),
            Value::from(inputs),
            mapper.clone(),
        ],
        ContPtr::Continuation(map_k),
        1,
        false,
    );

    Ok(Application::new(mapper_proc, args))
}

unsafe extern "C" fn map_k(
    env: *const Value,
    args: *const Value,
    barrier: *mut ContBarrier,
    out: *mut MaybeUninit<Application>,
) {
    unsafe {
        // TODO: Probably need to do this in a way that avoids mutable variables

        // env[0] is the output list
        let output: Vector = env.as_ref().unwrap().clone().try_into().unwrap();

        output.0.vec.write().push(args.as_ref().unwrap().clone());

        // env[1] is the input lists
        let inputs: Vector = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        // env[2] is the mapper function
        let mapper: Procedure = env.add(2).as_ref().unwrap().clone().try_into().unwrap();

        let mut args = Vec::new();

        // TODO: We need to collect a new list
        for input in inputs.0.vec.write().iter_mut() {
            if input.type_of() == ValueType::Null {
                // TODO: Check if the rest are also empty and args is empty
                let output = slice_to_list(&output.0.vec.read());
                let app = barrier.as_mut().unwrap().call_cont(vec![output]);
                (*out).write(app);
                return;
            }

            let (car, cdr) = input.cast::<Pair>().unwrap().into();
            args.push(car);
            *input = cdr;
        }

        barrier.as_mut().unwrap().push_cont(
            vec![
                Value::from(output),
                Value::from(inputs),
                Value::from(mapper.clone()),
            ],
            ContPtr::Continuation(map_k),
            1,
            false,
        );

        (*out).write(Application::new(mapper, args));
    }
}

#[bridge(name = "zip", lib = "(rnrs base builtins (6))")]
pub fn zip(list1: &Value, listn: &[Value]) -> Result<Value, Exception> {
    let mut output: Option<Vec<Value>> = None;
    for list in Some(list1).into_iter().chain(listn.iter()).rev() {
        let List { items, .. } = list.try_to()?;
        if let Some(output) = &output {
            if output.len() != items.len() {
                return Err(Exception::error("lists do not have the same length"));
            }
        } else {
            output = Some(vec![Value::null(); items.len()]);
        }

        let output = output.as_mut().unwrap();
        for (i, item) in items.into_iter().enumerate() {
            output[i] = Value::from((item, output[i].clone()));
        }
    }

    if let Some(output) = output {
        Ok(slice_to_list(&output))
    } else {
        Ok(Value::null())
    }
}
