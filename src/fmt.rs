use std::fmt::{self, Write};

use indexmap::{IndexMap, IndexSet};

use crate::{
    symbols::Symbol,
    value::{UnpackedValue, Value, ValueType},
    vectors::VectorInner,
};

#[derive(Copy, Clone, Debug)]
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
    pub fn new(root: &Value, mode: Mode, f: &'a mut std::fmt::Formatter<'_>) -> Self {
        let mut circular_values = IndexSet::default();
        determine_circularity(root, &mut IndexSet::default(), &mut circular_values);
        let labels = circular_values.into_iter().map(|k| (k, false)).collect();
        Self { mode, labels, f }
    }

    pub fn mode(&self) -> Mode {
        self.mode
    }

    pub fn fmt_list(&mut self, car: &Value, cdr: &Value) -> fmt::Result {
        match cdr.type_of() {
            ValueType::Pair | ValueType::Null => (),
            _ => {
                // This is not a proper list
                write!(self.f, "(")?;
                self.fmt_value(car)?;
                write!(self.f, " . ")?;
                self.fmt_value(cdr)?;
                write!(self.f, ")")?;
                return Ok(());
            }
        }

        write!(self.f, "(")?;
        self.fmt_value(car)?;
        let mut stack = vec![cdr.clone()];

        while let Some(head) = stack.pop() {
            if let Some((idx, _, seen)) = self.labels.get_full_mut(&head) {
                if *seen {
                    write!(self.f, " . #{idx}#")?;
                    continue;
                } else {
                    write!(self.f, " #{idx}=")?;
                    *seen = true;
                }
            }
            match &*head.unpacked_ref() {
                UnpackedValue::Null => {
                    if !stack.is_empty() {
                        write!(self.f, " ()")?;
                    }
                }
                UnpackedValue::Pair(pair) => {
                    let (car, cdr) = pair.clone().into();
                    write!(self.f, " ")?;
                    self.fmt_value(&car)?;
                    stack.push(cdr);
                }
                x => {
                    let val = x.clone().into_value();
                    write!(self.f, " ")?;
                    if stack.is_empty() {
                        write!(self.f, ". ")?;
                    }
                    self.fmt_value(&val)?;
                }
            }
        }

        write!(self.f, ")")
    }

    pub fn fmt_record<'b>(&'b mut self, name: Symbol) -> FmtRecord<'b, 'a> {
        FmtRecord {
            name: Some(name),
            f: self,
        }
    }

    pub fn fmt_value(&mut self, value: &Value) -> fmt::Result {
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
            Mode::Display => self.display_value(value),
            Mode::Write => self.write_value(value),
        }
    }

    fn display_value(&mut self, value: &Value) -> fmt::Result {
        match &*value.unpacked_ref() {
            UnpackedValue::Undefined => write!(self.f, "<undefined>"),
            UnpackedValue::Null => write!(self.f, "()"),
            UnpackedValue::Boolean(true) => write!(self.f, "#t"),
            UnpackedValue::Boolean(false) => write!(self.f, "#f"),
            UnpackedValue::Number(number) => write!(self.f, "{number}"),
            UnpackedValue::Character(c) => write!(self.f, "#\\{c}"),
            UnpackedValue::Symbol(symbol) => write!(self.f, "{symbol}"),
            UnpackedValue::Pair(pair) => {
                let (car, cdr) = pair.clone().into();
                self.fmt_list(&car, &cdr)
            }
            UnpackedValue::Procedure(_) => write!(self.f, "<procedure>"),
            UnpackedValue::Record(record) => record.fmt(self),
            UnpackedValue::RecordTypeDescriptor(rtd) => write!(self.f, "{rtd:?}"),
            UnpackedValue::Cell(cell) => self.fmt_value(&cell.0.read()),
            UnpackedValue::ReturnAddress(_) => write!(self.f, "%return-address"),
        }
    }

    fn write_value(&mut self, value: &Value) -> fmt::Result {
        match &*value.unpacked_ref() {
            UnpackedValue::Undefined => write!(self.f, "<undefined>"),
            UnpackedValue::Null => write!(self.f, "()"),
            UnpackedValue::Boolean(true) => write!(self.f, "#t"),
            UnpackedValue::Boolean(false) => write!(self.f, "#f"),
            UnpackedValue::Number(number) => write!(self.f, "{number}"),
            UnpackedValue::Character(c) => write!(self.f, "#\\{c}"),
            UnpackedValue::Symbol(symbol) => write!(self.f, "{symbol}"),
            UnpackedValue::Pair(pair) => {
                let (car, cdr) = pair.clone().into();
                self.fmt_list(&car, &cdr)
            }
            UnpackedValue::Procedure(proc) => write!(self.f, "#<procedure {proc:?}>"),
            UnpackedValue::Record(record) => record.fmt(self),
            UnpackedValue::RecordTypeDescriptor(rtd) => write!(self.f, "{rtd:?}"),
            UnpackedValue::Cell(cell) => self.fmt_value(&cell.0.read()),
            UnpackedValue::ReturnAddress(_) => write!(self.f, "%return-address"),
        }
    }

    pub fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> fmt::Result {
        self.f.write_fmt(args)
    }
}

impl Write for Formatter<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.f.write_str(s)
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        self.f.write_char(c)
    }

    fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> fmt::Result {
        self.f.write_fmt(args)
    }
}

pub struct FmtRecord<'a, 'b> {
    name: Option<Symbol>,
    f: &'a mut Formatter<'b>,
}

impl FmtRecord<'_, '_> {
    pub fn entry(&mut self, name: Symbol, value: &Value) -> fmt::Result {
        if let Some(record_name) = self.name.take() {
            write!(self.f, "#<{record_name}")?;
        }
        write!(self.f, " {name}: ")?;
        self.f.fmt_value(value)
    }

    pub fn finish(&mut self) -> fmt::Result {
        if let Some(record_name) = self.name.take() {
            write!(self.f, "#{record_name}")
        } else {
            write!(self.f, ">")
        }
    }
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
            for (_, value) in rec.iter_all_fields() {
                determine_circularity(&value, visited, circular);
            }
        }
        _ => (),
    }

    visited.swap_remove(curr);
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt = Formatter::new(self, Mode::Display, f);
        fmt.fmt_value(self)
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt = Formatter::new(self, Mode::Write, f);
        fmt.fmt_value(self)
    }
}
