use std::collections::HashMap;

use crate::{
    ast,
    // compile::Compile,
    continuation::Continuation,
    env::Env,
    error::RuntimeError,
    //     eval::Eval,
    gc::{Gc, Trace},
    proc::Procedure,
    syntax::{Identifier, Mark, Span, Syntax},
    value::Value,
};

/// Type declaration for a record.
#[derive(Debug, Trace, Clone)]
pub struct RecordType {
    name: String,
    /// Parent is most recently inserted record type, if one exists.
    inherits: indexmap::IndexSet<Gc<RecordType>>,
    fields: Vec<Identifier>,
}

impl RecordType {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            inherits: indexmap::IndexSet::new(),
            fields: Vec::new(),
        }
    }
}

fn is_subtype_of(lhs: &Gc<RecordType>, rhs: &Gc<RecordType>) -> bool {
    lhs == rhs || {
        let lhs = lhs.read();
        lhs.inherits.contains(rhs)
    }
}

#[derive(Debug, Trace, Clone)]
pub struct Record {
    record_type: Gc<RecordType>,
    fields: HashMap<Identifier, Gc<Value>>,
}

#[derive(Trace, Debug)]
pub struct DefineRecordType {
    parent: Option<Identifier>,
    name: Identifier,
    constructor: Option<Identifier>,
    predicate: Option<Identifier>,
    fields: Vec<FieldDefinition>,
}

#[derive(Trace, Debug)]
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

#[derive(Trace, Debug)]
enum FieldDefinitionKind {
    Mutable { mutator_name: Option<Identifier> },
    Immutable,
}

/*
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
                .ok_or_else(|| RuntimeError::undefined_variable(parent.clone()))?;
            let parent = parent_gc.read();
            let record_type: &Gc<RecordType> = (&*parent).try_into()?;
            let mut inherits = record_type.read().inherits.clone();
            inherits.insert(record_type.clone());
            inherits
        } else {
            indexmap::IndexSet::new()
        };

        let mut fields = Vec::new();

        for parent in &inherits {
            let record_type = parent.read();
            fields.extend_from_slice(record_type.fields.as_slice());
        }

        let record_type = Gc::new(RecordType::new(&self.name.name));

        for field in &self.fields {
            // In order to avoid name collisions, we mark the ident with the
            // pointer of the record type.
            // This is safe because we are never converting that mark back
            // to a pointer.
            // Once we do this, we never have to do it again, it will propagate
            // to all futures subtypes
            let mut field = field.field_name.clone();
            field.mark(Mark::from_gc(&record_type));
            fields.push(field);
        }

        // Set up the record type:
        {
            let mut rt = record_type.write();
            rt.inherits = inherits;
            // Got this code has gotten ugly. TODO: clean all of this up at some point.x
            rt.fields = fields
                .iter()
                .rev()
                .take(self.fields.len())
                .rev()
                .cloned()
                .collect();
        }

        // Set up the constructor:

        // Get the arguments for the constructor:
        let mut setters: Vec<Arc<dyn Eval>> = fields
            .iter()
            .map(|field| {
                Arc::new(UncheckedFieldMutator {
                    identifier: field.clone(),
                }) as Arc<dyn Eval>
            })
            .collect();

        // Append the return value
        setters.push(
            Arc::new(ast::Var::new(Identifier::new("this".to_string()))) as Arc<dyn Eval>,
        );

        let constructor = new_proc(
            env,
            fields.clone(),
            ast::Body::new(vec![Arc::new(ast::Let::new(
                vec![(
                    Identifier::new("this".to_string()),
                    Arc::new(Value::Record(Record {
                        record_type: record_type.clone(),
                        fields: HashMap::new(),
                    })),
                )],
                ast::Body::new(setters),
            )) as Arc<dyn Eval>]),
        );

        // Set up the predicate:
        // TODO: Extract this pattern into a function.
        let predicate = new_proc(
            env,
            vec![Identifier::new("this".to_string())],
            ast::Body::new(vec![Arc::new(RecordPredicate {
                record_type: record_type.clone(),
            }) as Arc<dyn Eval>]),
        );

        let mut new_functions = HashMap::<String, Gc<Value>>::new();

        // Set up the new field accessors and mutators:
        for field in &self.fields {
            let mut ident = field.field_name.clone();
            ident.mark(Mark::from_gc(&record_type));

            // Set up accessor:
            let accessor_name = field.accessor_name.as_ref().map_or_else(
                || format!("{}-{}", self.name.name, ident.name),
                |accessor| accessor.name.clone(),
            );
            let accessor = new_proc(
                env,
                vec![Identifier::new("this".to_string())],
                ast::Body::new(vec![Arc::new(FieldAccessor {
                    record_type: record_type.clone(),
                    identifier: ident.clone(),
                }) as Arc<dyn Eval>]),
            );

            new_functions.insert(accessor_name, accessor);

            // Set up mutator, if we should:
            if let Some(mutator_name) = match field.kind {
                FieldDefinitionKind::Mutable {
                    mutator_name: Some(ref mutator_name),
                } => Some(mutator_name.name.clone()),
                FieldDefinitionKind::Mutable { .. } => {
                    Some(format!("{}-{}-set!", self.name.name, ident.name))
                }
                _ => None,
            } {
                let mutator = new_proc(
                    env,
                    vec![Identifier::new("this".to_string()), ident.clone()],
                    ast::Body::new(vec![Arc::new(FieldMutator {
                        record_type: record_type.clone(),
                        identifier: ident.clone(),
                    }) as Arc<dyn Eval>]),
                );
                new_functions.insert(mutator_name, mutator);
            }
        }

        // Now that we have all of the appropriate functions set up, we can
        // apply them to the environment.
        // TODO: We should really figure out a way to define all of these at once. It's unlikely
        // to cause errors, but worth doing just for the sake of correctness.

        // All of this evaluations should be super simple, no need to worry about
        // the continuation being correct.

        let constructor_name = self
            .constructor
            .as_ref()
            .map_or_else(|| format!("make-{}", self.name.name), |cn| cn.name.clone());
        let constructor_name = Identifier::new(constructor_name);
        env.def_var(&constructor_name, constructor);

        let predicate_name = self
            .predicate
            .as_ref()
            .map_or_else(|| format!("{}?", self.name.name), |cn| cn.name.clone());
        let predicate_name = Identifier::new(predicate_name);
        env.def_var(&predicate_name, predicate);

        for (new_function_name, new_function) in new_functions.into_iter() {
            env.def_var(&Identifier::new(new_function_name), new_function);
        }

        env.def_var(&self.name, Gc::new(Value::RecordType(record_type)));

        Ok(Vec::new())
    }
}

fn new_proc(env: &Env, args: Vec<Identifier>, body: ast::Body) -> Gc<Value> {
    Gc::new(Value::Procedure(Procedure {
        up: env.clone(),
        args,
        remaining: None,
        body,
        is_variable_transformer: false,
    }))
}

// These are implemented in a super weird way and I'm sure I could figure
// out something better. But they'll do for now.

#[derive(Trace)]
struct UncheckedFieldMutator {
    identifier: Identifier,
}

#[async_trait]
impl Eval for UncheckedFieldMutator {
    async fn eval(
        &self,
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let this_gc = env.fetch_var(&Identifier::new("this".to_string())).unwrap();
        let value = env.fetch_var(&self.identifier).unwrap();
        let mut this = this_gc.write();
        let this: &mut Record = (&mut *this).try_into()?;
        this.fields.insert(self.identifier.clone(), value);

        Ok(Vec::new())
    }
}

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
        let this_gc = env.fetch_var(&Identifier::new("this".to_string())).unwrap();

        {
            let this = this_gc.read();
            let this: &Record = (&*this).try_into()?;

            if !is_subtype_of(&this.record_type, &self.record_type) {
                return Err(RuntimeError::invalid_type(
                    &self.record_type.read().name,
                    &this.record_type.read().name,
                ));
            }
        }

        let value = env.fetch_var(&self.identifier).unwrap();

        let mut this = this_gc.write();
        let this: &mut Record = (&mut *this).try_into()?;
        this.fields.insert(self.identifier.clone(), value);

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
        let this_gc = env.fetch_var(&Identifier::new("this".to_string())).unwrap();
        let this = this_gc.read();
        let this: &Record = (&*this).try_into()?;

        if !is_subtype_of(&this.record_type, &self.record_type) {
            return Err(RuntimeError::invalid_type(
                &self.record_type.read().name,
                &this.record_type.read().name,
            ));
        }

        Ok(vec![this.fields.get(&self.identifier).unwrap().clone()])
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
        let this_gc = env.fetch_var(&Identifier::new("this".to_string())).unwrap();
        let this = this_gc.read();
        let this = match &*this {
            Value::Record(ref rec) => rec,
            _ => return Ok(vec![Gc::new(Value::Boolean(false))]),
        };

        Ok(vec![Gc::new(Value::Boolean(is_subtype_of(
            &this.record_type,
            &self.record_type,
        )))])
    }
}
*/
