//! Rudimentary structure support. CPS will probably make a lot of this redundant.

use std::{
    cell::LazyCell,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::{
    exception::Condition,
    gc::{Gc, GcWriteGuard, Trace},
    registry::bridge,
    syntax::Identifier,
    value::{UnpackedValue, Value},
};

/// Type declaration for a record.
#[derive(Debug, Trace, Clone)]
#[repr(align(16))]
pub struct RecordType {
    name: String,
    sealed: bool,
    opaque: bool,
    /// Parent is most recently inserted record type, if one exists.
    inherits: indexmap::IndexSet<Arc<RecordType>>,
    fields: Vec<Identifier>,
}

/// The record type for the "record type" type.
const RECORD_TYPE_RT: LazyCell<Arc<RecordType>> = LazyCell::new(|| {
    Arc::new(RecordType {
        name: "rt".to_string(),
        sealed: true,
        opaque: true,
        inherits: indexmap::IndexSet::new(),
        fields: vec![],
    })
});

impl TryFrom<Value> for Arc<RecordType> {
    type Error = Condition;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl<'a> TryFrom<&'a Record> for RecordType {
    type Error = Condition;

    fn try_from(value: &'a Record) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl From<RecordType> for Record {
    fn from(value: RecordType) -> Self {
        todo!()
    }
}

impl HasRecordType for RecordType {
    fn record_type() -> Arc<RecordType> {
        RECORD_TYPE_RT.clone()
    }

    fn commit_to_record(&self, record: &mut Record) {
        todo!()
    }
}

/*
impl RecordType {
    pub fn new(name: &str, parent: Option<&RecordType>, sealed: bool, opaque: bool) -> Self {
        Self {
            name: name.to_string(),
            inherits: indexmap::IndexSet::new(),
            fields: Vec::new(),
        }
    }
}

*/

#[bridge(name = "make-record-type-descriptor", lib = "(base)")]
pub async fn make_record_type_descriptor(
    name: &Value,
    parent: &Value,
    uid: &Value,
    sealed: &Value,
    opaque: &Value,
) -> Result<Vec<Value>, Condition> {
    let name = name.clone().try_into_sym()?;
    let parent: Option<Arc<RecordType>> = (!parent.is_true()).then(|| parent.clone().try_into()).transpose()?;
    todo!()
}

/*
fn is_subtype_of(lhs: &Gc<RecordType>, rhs: &Gc<RecordType>) -> bool {
    lhs == rhs || {
        let lhs = lhs.read();
        lhs.inherits.contains(rhs)
    }
}
*/

#[derive(Debug, Trace, Clone)]
#[repr(align(16))]
pub struct Record {
    record_type: Arc<RecordType>,
    fields: Vec<Value>,
}

impl Record {
    fn try_into_mut<T>(&mut self) -> Result<RecordMut<'_, T>, Condition>
    where
        T: HasRecordType,
    {
        let rust_value = T::try_from(self)?;
        Ok(RecordMut {
            scheme_record: self,
            rust_value,
        })
    }
}

#[macro_export]
macro_rules! try_into_mut_rust_value {
    ( $val:ident as $into:ty ) => {
        let val_ty = $val.type_name();
        let temp: Gc<Record> = $val
            .try_into()
            .map_err(|_| Condition::invalid_type(&$into::record_type().name, val_ty))?;
        let temp_write = temp.write();
        let $val = temp_write.try_into_mut::<$into>();
    };
}

/*

fn try_into_record_mut<T>(&self) -> Result<GcWriteGuard<'_, T>, Condition>
where
    T: HasRecordType,
{
    match self {
        UnpackedValue::Record(record) => GcWriteGuard::try_map(record.write(), |record| record.try_into_mut()),
        e => Err(Condition::invalid_type(T::record
    }
}
 */

pub trait HasRecordType:
    Sized
    // + TryFrom<Value>
    + for<'a> TryFrom<&'a Record, Error = Condition>
    + Into<Record>
{
    fn record_type() -> Arc<RecordType>;

    fn commit_to_record(&self, record: &mut Record);
}

pub struct RecordMut<'a, T>
where
    T: HasRecordType,
{
    scheme_record: &'a mut Record,
    rust_value: T,
}

impl<T> Deref for RecordMut<'_, T>
where
    T: HasRecordType,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.rust_value
    }
}

impl<T> DerefMut for RecordMut<'_, T>
where
    T: HasRecordType,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.rust_value
    }
}

impl<T> Drop for RecordMut<'_, T>
where
    T: HasRecordType,
{
    fn drop(&mut self) {
        self.rust_value.commit_to_record(&mut self.scheme_record);
    }
}
