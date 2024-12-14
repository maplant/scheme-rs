use std::{
    collections::HashMap,
    sync::Arc,
};

use async_trait::async_trait;

use crate::{
    compile::Compile,
    continuation::Continuation,
    env::Env,
    error::RuntimeError,
    eval::Eval,
    gc::{Gc, Trace},
    syntax::{Identifier, Span, Syntax},
    value::Value,
};

/// Type declaration for a record.
#[derive(Trace, Clone)]
pub struct RecordType {
    name: String,
    /// Parent is most recently inserted record type, if one exists.
    inherits: indexmap::IndexSet<Gc<RecordType>>,
    fields: Vec<Field>,
}

async fn is_subtype_of(lhs: &Gc<RecordType>, rhs: &Gc<RecordType>) -> bool {
    lhs == rhs || {
        let lhs = lhs.read().await;
        lhs.inherits.contains(rhs)
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Trace)]
struct Field {
    record_type: Gc<RecordType>,
    name: Identifier,
}

impl Field {
    pub fn new(record_type: &Gc<RecordType>, name: &Identifier) -> Self {
        Self {
            record_type: record_type.clone(),
            name: name.clone(),
        }
    }
}

#[derive(Trace, Clone)]
pub struct Record {
    record_type: Gc<RecordType>,
    fields: HashMap<Field, Gc<Value>>,
}

#[derive(Trace)]
pub struct DefineRecordType {
    parent: Option<Identifier>,
    name: Identifier,
    constructor: Option<Identifier>,
    predicate: Option<Identifier>,
    fields: Vec<FieldDefinition>,
}

#[derive(Trace)]
struct FieldDefinition {
    field_name: Identifier,
    accessor_name: Option<Identifier>,
    kind: FieldDefinitionKind,
    span: Span,
}

impl FieldDefinition {
    fn new(name: &Identifier, accessor: Option<&Identifier>, span: &Span) -> Self {
        Self {
            field_name: name.clone(),
            accessor_name: accessor.cloned(),
            kind: FieldDefinitionKind::Immutable,
            span: span.clone(),
        }
    }

    fn new_mut(
        name: &Identifier,
        accessor: Option<&Identifier>,
        mutator: Option<&Identifier>,
        span: &Span,
    ) -> Self {
        Self {
            field_name: name.clone(),
            accessor_name: accessor.cloned(),
            kind: FieldDefinitionKind::Mutable {
                mutator_name: mutator.cloned(),
            },
            span: span.clone(),
        }
    }
}

#[derive(Trace)]
enum FieldDefinitionKind {
    Mutable { mutator_name: Option<Identifier> },
    Immutable,
}

#[derive(Debug, Clone)]
pub enum CompileDefineRecordTypeError {
    ParentSpecifiedMultiple { first: Span, second: Span },
    FieldDefinedMultipleTimes { first: Span, second: Span },
    MultipleFieldsClauses { first: Span, second: Span },
    BadForm(Span),
}

fn compile_field(
    field: &[Syntax],
    span: &Span,
) -> Result<FieldDefinition, CompileDefineRecordTypeError> {
    match field {
        [Syntax::Identifier {
            ident: mutability, ..
        }, Syntax::Identifier {
            ident: field_name, ..
        }, Syntax::Null { .. }]
            if mutability.name == "mutable" =>
        {
            Ok(FieldDefinition::new_mut(field_name, None, None, span))
        }
        [Syntax::Identifier {
            ident: mutability, ..
        }, Syntax::Identifier {
            ident: field_name, ..
        }, Syntax::Identifier {
            ident: accessor_name,
            ..
        }, Syntax::Null { .. }]
            if mutability.name == "mutable" =>
        {
            Ok(FieldDefinition::new_mut(
                field_name,
                Some(accessor_name),
                None,
                span,
            ))
        }
        [Syntax::Identifier {
            ident: mutability, ..
        }, Syntax::Identifier {
            ident: field_name, ..
        }, Syntax::Identifier {
            ident: accessor_name,
            ..
        }, Syntax::Identifier {
            ident: mutator_name,
            ..
        }, Syntax::Null { .. }]
            if mutability.name == "mutable" =>
        {
            Ok(FieldDefinition::new_mut(
                field_name,
                Some(accessor_name),
                Some(mutator_name),
                span,
            ))
        }

        [Syntax::Identifier {
            ident: mutability, ..
        }, Syntax::Identifier {
            ident: field_name, ..
        }, Syntax::Null { .. }]
            if mutability.name == "immutable" =>
        {
            Ok(FieldDefinition::new(field_name, None, span))
        }
        [Syntax::Identifier {
            ident: mutability, ..
        }, Syntax::Identifier {
            ident: field_name, ..
        }, Syntax::Identifier {
            ident: accessor_name,
            ..
        }, Syntax::Null { .. }]
            if mutability.name == "immutable" =>
        {
            Ok(FieldDefinition::new(field_name, Some(accessor_name), span))
        }
        _ => Err(CompileDefineRecordTypeError::BadForm(span.clone())),
    }
}

fn compile_fields(fields: &[Syntax]) -> Result<Vec<FieldDefinition>, CompileDefineRecordTypeError> {
    let mut compiled_fields = Vec::new();
    for field in fields {
        match field {
            Syntax::Identifier { ident, span, .. } => {
                compiled_fields.push(FieldDefinition::new(ident, None, span));
            }
            Syntax::List { list, span } => compiled_fields.push(compile_field(list, span)?),
            x => return Err(CompileDefineRecordTypeError::BadForm(x.span().clone())),
        }
    }
    Ok(compiled_fields)
}

#[async_trait]
impl Compile for DefineRecordType {
    type Error = CompileDefineRecordTypeError;

    async fn compile(
        exprs: &[Syntax],
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, CompileDefineRecordTypeError> {
        match exprs {
            [first_arg, args @ .., Syntax::Null { .. }] => {
                let (name, constructor, predicate) = match first_arg {
                    Syntax::Identifier { ident: name, .. } => (name.clone(), None, None),
                    Syntax::List { list, span, .. } => {
                        if let [Syntax::Identifier { ident: name, .. }, Syntax::Identifier {
                            ident: constructor, ..
                        }, Syntax::Identifier {
                            ident: predicate, ..
                        }, Syntax::Null { .. }] = list.as_slice()
                        {
                            (
                                name.clone(),
                                Some(constructor.clone()),
                                Some(predicate.clone()),
                            )
                        } else {
                            return Err(CompileDefineRecordTypeError::BadForm(span.clone()));
                        }
                    }
                    _ => return Err(CompileDefineRecordTypeError::BadForm(span.clone())),
                };

                let mut parent: Option<(Identifier, Span)> = None;
                let mut fields: Option<(Vec<FieldDefinition>, Span)> = None;

                for arg in args {
                    if let Syntax::List { list, span, .. } = arg {
                        match list.as_slice() {
                            [Syntax::Identifier {
                                ident,
                                span: second,
                                ..
                            }, Syntax::Identifier {
                                ident: parent_name, ..
                            }, Syntax::Null { .. }] => {
                                if ident.name == "parent" {
                                    if let Some((_, first)) = parent {
                                        return Err(
                                            CompileDefineRecordTypeError::ParentSpecifiedMultiple {
                                                first: first.clone(),
                                                second: second.clone(),
                                            },
                                        );
                                    }
                                    parent = Some((parent_name.clone(), second.clone()));
                                }
                            }
                            [Syntax::Identifier {
                                ident,
                                span: second,
                                ..
                            }, uncompiled_fields @ .., Syntax::Null { .. }]
                                if ident.name == "fields" =>
                            {
                                if let Some((_, first)) = fields {
                                    return Err(
                                        CompileDefineRecordTypeError::ParentSpecifiedMultiple {
                                            first: first.clone(),
                                            second: second.clone(),
                                        },
                                    );
                                }

                                let compiled_fields = compile_fields(uncompiled_fields)?;

                                // Check for fields with the same name:
                                let mut field_locs = HashMap::<String, Span>::new();

                                for field in &compiled_fields {
                                    if let Some(first) = field_locs.get(&field.field_name.name) {
                                        return Err(CompileDefineRecordTypeError::FieldDefinedMultipleTimes { first: first.clone(), second: field.span.clone() });
                                    }
                                    field_locs
                                        .insert(field.field_name.name.clone(), field.span.clone());
                                }

                                fields = Some((compiled_fields, span.clone()));
                            }
                            _ => return Err(CompileDefineRecordTypeError::BadForm(span.clone())),
                        }
                    } else {
                        return Err(CompileDefineRecordTypeError::BadForm(span.clone()));
                    }
                }

                Ok(Self {
                    parent: parent.map(|(x, _)| x),
                    name,
                    constructor,
                    predicate,
                    fields: fields.map(|(x, _)| x).unwrap_or_default(),
                })
            }
            _ => Err(CompileDefineRecordTypeError::BadForm(span.clone())),
        }
    }
}

#[async_trait]
impl Eval for DefineRecordType {
    async fn eval(
        &self,
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let inherits = if let Some(ref parent) = self.parent {
            let parent_gc = env
                .fetch_var(parent)
                .await
                .ok_or_else(|| RuntimeError::undefined_variable(parent.clone()))?;
            let parent = parent_gc.read().await;
            let record_type: &Gc<RecordType> = (&*parent).try_into()?;
            let mut inherits = record_type.read().await.inherits.clone();
            inherits.insert(record_type.clone());
            inherits
        } else {
            indexmap::IndexSet::new()
        };

        Ok(Vec::new())
    }
}

// These are implemented in a super weird way and I'm sure I could figure
// out something better. But they'll do for now.

#[derive(Trace)]
struct FieldMutator {
    record_type: Gc<RecordType>,
    identifier: Identifier,
}

#[async_trait]
impl Eval for FieldMutator {
    async fn eval(
        &self,
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let this_gc = env
            .fetch_var(&Identifier::new("this".to_string()))
            .await
            .unwrap();
        let this = this_gc.read().await;
        let this: &Record = (&*this).try_into()?;

        if !is_subtype_of(&this.record_type, &self.record_type).await {
            return Err(RuntimeError::invalid_type(
                &self.record_type.read().await.name,
                &this.record_type.read().await.name,
            ));
        }

        let value = env.fetch_var(&self.identifier).await.unwrap();

        let mut this = this_gc.write().await;
        let this: &mut Record = (&mut *this).try_into()?;
        this.fields
            .insert(Field::new(&self.record_type, &self.identifier), value);

        Ok(Vec::new())
    }
}

#[derive(Trace)]
struct FieldAccessor {
    record_type: Gc<RecordType>,
    identifier: Identifier,
}

#[async_trait]
impl Eval for FieldAccessor {
    async fn eval(
        &self,
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let this_gc = env
            .fetch_var(&Identifier::new("this".to_string()))
            .await
            .unwrap();
        let this = this_gc.read().await;
        let this: &Record = (&*this).try_into()?;

        if !is_subtype_of(&this.record_type, &self.record_type).await {
            return Err(RuntimeError::invalid_type(
                &self.record_type.read().await.name,
                &this.record_type.read().await.name,
            ));
        }

        Ok(vec![this
            .fields
            .get(&Field::new(&self.record_type, &self.identifier))
            .unwrap()
            .clone()])
    }
}

#[derive(Trace)]
struct RecordPredicate {
    record_type: Gc<RecordType>,
}

#[async_trait]
impl Eval for RecordPredicate {
    async fn eval(
        &self,
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let this_gc = env
            .fetch_var(&Identifier::new("this".to_string()))
            .await
            .unwrap();
        let this = this_gc.read().await;
        let this: &Record = (&*this).try_into()?;

        Ok(vec![Gc::new(Value::Boolean(
            is_subtype_of(&this.record_type, &self.record_type).await,
        ))])
    }
}
