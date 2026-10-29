use std::fmt::{self, Write};

use indexmap::{IndexMap, IndexSet};

use crate::{
    value::{UnpackedValue, Value},
    vectors::VectorInner,
};

pub enum Mode {
    Display,
    Write,
}

pub struct Formatter<'a> {
    f: &'a mut (dyn Write + 'a),
    labels: IndexMap<Value, bool>,
    mode: Mode,
    // TODO: depth
}

impl<'a> Formatter<'a> {
    fn new<'b>(f: &'a mut Formatter<'b>, mode: Mode, root: &Value) -> Self {
        todo!()
    }

    pub fn value(&mut self, value: &Value) -> fmt::Result {
        if let Some((idx, _, seen)) = self.labels.get_full_mut(value) {
            if *seen {
                write!(self.f, "#{idx}#")?;
                return Ok(());
            } else {
                write!(self.f, "#{idx}=")?;
                *seen = true;
            }
        }

        match self.mode {
            Mode::Display => display_value(value, &mut self.labels, self.f),
            Mode::Write => write_value(value, &mut self.labels, self.f),
        }
    }
}

fn display_value(
    value: &Value,
    labels: &mut IndexMap<Value, bool>,
    f: &mut dyn Write,
) -> fmt::Result {
    match &*value.unpacked_ref() {
        UnpackedValue::Undefined => write!(f, "<undefined>"),
        UnpackedValue::Null => write!(f, "()"),
        UnpackedValue::Boolean(true) => write!(f, "#t"),
        UnpackedValue::Boolean(false) => write!(f, "#f"),
        UnpackedValue::Number(number) => write!(f, "{number}"),
        UnpackedValue::Character(c) => write!(f, "#\\{c}"),
        UnpackedValue::Symbol(symbol) => write!(f, "{symbol}"),
        // UnpackedValue::Pair(pair) => {
        //     let (car, cdr) = pair.into();
        //     lists::write_list(&car, &cdr, Value::display, circular_values, f)
        // }
        UnpackedValue::Procedure(_) => write!(f, "<procedure>"),
        // UnpackedValue::Record(record) => record.display_fmt(circular_values, f),
        UnpackedValue::RecordTypeDescriptor(rtd) => write!(f, "{rtd:?}"),
        // UnpackedValue::Cell(cell) => cell.0.read().display(circular_values, f),
        UnpackedValue::ReturnAddress(_) => write!(f, "%return-address"),
        _ => todo!(),
    }
}

fn write_value(
    value: &Value,
    labels: &mut IndexMap<Value, bool>,
    f: &mut dyn Write,
) -> fmt::Result {
    match &*value.unpacked_ref() {
        UnpackedValue::Undefined => write!(f, "<undefined>"),
        UnpackedValue::Null => write!(f, "()"),
        UnpackedValue::Boolean(true) => write!(f, "#t"),
        UnpackedValue::Boolean(false) => write!(f, "#f"),
        UnpackedValue::Number(number) => write!(f, "{number}"),
        UnpackedValue::Character(c) => write!(f, "#\\{c}"),
        UnpackedValue::Symbol(symbol) => write!(f, "{symbol}"),
        // UnpackedValue::Pair(pair) => {
        //     let (car, cdr) = pair.into();
        //     lists::write_list(&car, &cdr, Value::write, circular_values, f)
        // }
        UnpackedValue::Procedure(proc) => write!(f, "#<procedure {proc:?}>"),
        // UnpackedValue::Record(record) => record.debug_fmt(circular_values, f),
        UnpackedValue::RecordTypeDescriptor(rtd) => write!(f, "{rtd:?}"),
        // UnpackedValue::Cell(cell) => cell.0.read().write(circular_values, f),
        UnpackedValue::ReturnAddress(_) => write!(f, "%return-address"),
        _ => todo!(),
    }
}

pub struct DisplayRecord<'a, 'b> {
    f: &'a mut Formatter<'b>,
}

/// Determines which children of the given list are circular, i.e. have children
/// that refer to back to them. This is just a depth-first search.
fn determine_circularity(
    curr: &Value,
    visited: &mut IndexSet<Value>,
    circular: &mut IndexSet<Value>,
) {
    if visited.contains(curr) {
        circular.insert(curr.clone());
        return;
    }

    visited.insert(curr.clone());

    match curr.clone().unpack() {
        UnpackedValue::Pair(pair) => {
            let (car, cdr) = pair.into();
            determine_circularity(&car, visited, circular);
            determine_circularity(&cdr, visited, circular);
        }
        UnpackedValue::Record(rec) if let Some(vec) = rec.cast::<VectorInner<Value>>() => {
            let vec_read = vec.vec.read();
            for item in vec_read.iter() {
                determine_circularity(item, visited, circular);
            }
        }
        UnpackedValue::Record(rec) => {
            for value in rec.iter_fields() {
                determine_circularity(value, visited, circular);
            }
        }
        _ => (),
    }

    visited.swap_remove(curr);
}

/*
pub(crate) fn write_value(
    val: &Value,
    fmt: fn(&Value, &mut IndexMap<Value, bool>, &mut fmt::Formatter<'_>) -> fmt::Result,
    circular_values: &mut IndexMap<Value, bool>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    if let Some((idx, _, seen)) = circular_values.get_full_mut(val) {
        if *seen {
            write!(f, "#{idx}#")?;
            return Ok(());
        } else {
            write!(f, "#{idx}=")?;
            *seen = true;
        }
    }

    fmt(val, circular_values, f)
}
*/

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut circular_values = IndexSet::default();
        determine_circularity(self, &mut IndexSet::default(), &mut circular_values);
        let mut circular_values = circular_values.into_iter().map(|k| (k, false)).collect();
        write_value(self, Value::display_fmt, &mut circular_values, f)
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut circular_values = IndexSet::default();
        determine_circularity(self, &mut IndexSet::default(), &mut circular_values);
        let mut circular_values = circular_values.into_iter().map(|k| (k, false)).collect();
        write_value(self, Value::debug_fmt, &mut circular_values, f)
    }
}
