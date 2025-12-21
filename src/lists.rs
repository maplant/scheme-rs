use indexmap::IndexMap;
use parking_lot::RwLock;

use crate::{
    exceptions::Condition,
    gc::{Gc, GcInner, Trace},
    num::Number,
    proc::{Application, DynStack, Procedure},
    registry::{bridge, cps_bridge},
    runtime::{Runtime, RuntimeInner},
    value::{UnpackedValue, Value, ValueType, write_value},
    vectors::Vector,
};
use std::fmt;

#[derive(Trace)]
#[repr(align(16))]
pub(crate) struct PairInner {
    /// The head of the pair
    car: RwLock<Value>,
    /// The tail of the pair
    cdr: RwLock<Value>,
    /// Whether or not the pair can be modified post creation
    mutable: bool,
}

/// A pair of Scheme [Values](Value). Has a head (the [car](Value::car)) and a
/// tail (the [cdr](Value::cdr)).
#[derive(Clone, Trace)]
pub struct Pair(pub(crate) Gc<PairInner>);

impl Pair {
    /// Construct a new Pair from a car and cdr
    pub fn new(car: Value, cdr: Value, mutable: bool) -> Self {
        Self(Gc::new(PairInner {
            car: RwLock::new(car),
            cdr: RwLock::new(cdr),
            mutable,
        }))
    }

    /// Extract the car (aka the head) from the Pair.
    pub fn car(&self) -> Value {
        self.0.car.read().clone()
    }

    /// Alias for [car]
    pub fn head(&self) -> Value {
        self.car()
    }

    /// Extract the cdr (aka the tail) from the Pair.
    pub fn cdr(&self) -> Value {
        self.0.cdr.read().clone()
    }

    /// Alias for [cdr]
    pub fn tail(&self) -> Value {
        self.cdr()
    }

    /// Set the car of the Pair. Returns an error if pair is immutable.
    pub fn set_car(&self, new_car: Value) -> Result<(), Condition> {
        if self.0.mutable {
            *self.0.car.write() = new_car;
            Ok(())
        } else {
            Err(Condition::error("pair is not mutable"))
        }
    }

    /// Set the cdr of the Pair. Returns an error if pair is immutable.
    pub fn set_cdr(&self, new_cdr: Value) -> Result<(), Condition> {
        if self.0.mutable {
            *self.0.cdr.write() = new_cdr;
            Ok(())
        } else {
            Err(Condition::error("pair is not mutable"))
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

pub fn slice_to_list(items: &[Value]) -> Value {
    match items {
        [] => Value::null(),
        [head, tail @ ..] => Value::from(Pair::new(head.clone(), slice_to_list(tail), false)),
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

#[bridge(name = "list", lib = "(rnrs base builtins (6))")]
pub fn list(args: &[Value]) -> Result<Vec<Value>, Condition> {
    // Construct the list in reverse
    let mut cdr = Value::null();
    for arg in args.iter().rev() {
        cdr = Value::from(Pair::new(arg.clone(), cdr, true));
    }
    Ok(vec![cdr])
}

#[bridge(name = "cons", lib = "(rnrs base builtins (6))")]
pub fn cons(car: &Value, cdr: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(Pair::new(car.clone(), cdr.clone(), true))])
}

#[bridge(name = "car", lib = "(rnrs base builtins (6))")]
pub fn car(val: &Value) -> Result<Vec<Value>, Condition> {
    match &*val.unpacked_ref() {
        UnpackedValue::Pair(pair) => Ok(vec![pair.car()]),
        UnpackedValue::Syntax(syn) => Ok(vec![Value::from(syn.car()?)]),
        _ => Err(Condition::type_error("list", val.type_name())),
    }
}

#[bridge(name = "cdr", lib = "(rnrs base builtins (6))")]
pub fn cdr(val: &Value) -> Result<Vec<Value>, Condition> {
    match &*val.unpacked_ref() {
        UnpackedValue::Pair(pair) => Ok(vec![pair.cdr()]),
        UnpackedValue::Syntax(syn) => Ok(vec![Value::from(syn.cdr()?)]),
        _ => Err(Condition::type_error("list", val.type_name())),
    }
}

#[bridge(name = "set-car!", lib = "(rnrs base builtins (6))")]
pub fn set_car(var: &Value, val: &Value) -> Result<Vec<Value>, Condition> {
    let pair: Pair = var.clone().try_into()?;
    pair.set_car(val.clone())?;
    Ok(Vec::new())
}

#[bridge(name = "set-cdr!", lib = "(rnrs base builtins (6))")]
pub fn set_cdr(var: &Value, val: &Value) -> Result<Vec<Value>, Condition> {
    let pair: Pair = var.clone().try_into()?;
    pair.set_cdr(val.clone())?;
    Ok(Vec::new())
}

#[bridge(name = "length", lib = "(rnrs base builtins (6))")]
pub fn length_builtin(arg: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(Number::from(length(arg)?))])
}

pub fn length(arg: &Value) -> Result<usize, Condition> {
    let mut length = 0usize;
    let mut arg = arg.clone();
    loop {
        arg = {
            match &*arg.unpacked_ref() {
                UnpackedValue::Pair(pair) => pair.cdr(),
                UnpackedValue::Null => break,
                _ => return Err(Condition::error("list must be proper".to_string())),
            }
        };
        length += 1;
    }
    Ok(length)
}

#[bridge(name = "list->vector", lib = "(rnrs base builtins (6))")]
pub fn list_to_vector(list: &Value) -> Result<Vec<Value>, Condition> {
    let mut vec = Vec::new();
    list_to_vec(list, &mut vec);

    Ok(vec![Value::from(vec)])
}

#[bridge(name = "append", lib = "(rnrs base builtins (6))")]
pub fn append(list: &Value, to_append: &Value) -> Result<Vec<Value>, Condition> {
    let mut vec = Vec::new();
    list_to_vec(list, &mut vec);
    let mut list = to_append.clone();
    for item in vec.into_iter().rev() {
        list = Value::from(Pair::new(item, list, true));
    }
    Ok(vec![list])
}

#[cps_bridge(def = "map proc list1 . listn", lib = "(rnrs base builtins (6))")]
pub fn map(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    list_n: &[Value],
    _params: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
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
            return Ok(Application::new(k.try_into()?, vec![Value::null()], None));
        }

        let (car, cdr) = match &*input.unpacked_ref() {
            UnpackedValue::Pair(pair) => pair.clone().into(),
            UnpackedValue::Syntax(syn) => (Value::from(syn.car()?), Value::from(syn.cdr()?)),
            _ => return Err(Condition::type_error("list", input.type_name())),
        };

        args.push(car);
        *input = cdr;
    }

    let map_k = Procedure::new(
        runtime.clone(),
        vec![
            Value::from(Vec::<Value>::new()),
            Value::from(inputs),
            mapper.clone(),
            k,
        ],
        crate::proc::FuncPtr::Continuation(map_k),
        1,
        false,
        None,
    );

    args.push(Value::from(map_k));

    Ok(Application::new(mapper_proc, args, None))
}

unsafe extern "C" fn map_k(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    env: *const Value,
    args: *const Value,
    _params: *mut DynStack,
) -> *mut Application {
    unsafe {
        // TODO: Probably need to do this in a way that avoids mutable variables

        // env[0] is the output list
        let output: Vector = env.as_ref().unwrap().clone().try_into().unwrap();

        output.0.vec.write().push(args.as_ref().unwrap().clone());

        // env[1] is the input lists
        let inputs: Vector = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        // env[2] is the mapper function
        let mapper: Procedure = env.add(2).as_ref().unwrap().clone().try_into().unwrap();

        // env[3] is the continuation
        let k: Procedure = env.add(3).as_ref().unwrap().clone().try_into().unwrap();

        let mut args = Vec::new();

        // TODO: We need to collect a new list
        for input in inputs.0.vec.write().iter_mut() {
            if input.type_of() == ValueType::Null {
                // TODO: Check if the rest are also empty and args is empty
                let output = slice_to_list(&output.0.vec.read());
                let app = Application::new(k, vec![output], None);
                return Box::into_raw(Box::new(app));
            }

            let (car, cdr) = match &*input.unpacked_ref() {
                UnpackedValue::Pair(pair) => pair.clone().into(),
                UnpackedValue::Syntax(syn) => (
                    Value::from(syn.car().unwrap()),
                    Value::from(syn.cdr().unwrap()),
                ),
                _ => unreachable!(),
            };

            args.push(car);
            *input = cdr;
        }

        let map_k = Procedure::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![
                Value::from(output),
                Value::from(inputs),
                Value::from(mapper.clone()),
                Value::from(k),
            ],
            crate::proc::FuncPtr::Continuation(map_k),
            1,
            false,
            None,
        );

        args.push(Value::from(map_k));

        let app = Application::new(mapper, args, None);

        Box::into_raw(Box::new(app))
    }
}
