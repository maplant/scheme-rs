//! Rudimentary structure support. CPS will probably make a lot of this redundant.

use std::collections::HashMap;

use crate::{
    ast::{self, parse::ParseAstError, Body, Expression, Let},
    env::{Environment, Top, Var},
    error::RuntimeError,
    gc::{Gc, Trace},
    // proc::Procedure,
    syntax::{Identifier, Span, Syntax},
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
    fields: Vec<Gc<Value>>,
}

#[derive(Clone, Trace, Debug)]
pub struct DefineRecordType {
    parent: Option<Var>,
    name: Identifier,
    constructor: Option<Identifier>,
    predicate: Option<Identifier>,
    fields: Vec<FieldDefinition>,
}

#[derive(Trace, Debug, Clone)]
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

#[derive(Trace, Debug, Clone)]
enum FieldDefinitionKind {
    Mutable { mutator_name: Option<Identifier> },
    Immutable,
}

fn parse_field(field: &[Syntax], span: &Span) -> Result<FieldDefinition, ParseAstError> {
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
        _ => Err(ParseAstError::BadForm(span.clone())),
    }
}

fn parse_fields(fields: &[Syntax]) -> Result<Vec<FieldDefinition>, ParseAstError> {
    let mut parsed_fields = Vec::new();
    for field in fields {
        match field {
            Syntax::Identifier { ident, span, .. } => {
                parsed_fields.push(FieldDefinition::new(ident, None, span));
            }
            Syntax::List { list, span } => parsed_fields.push(parse_field(list, span)?),
            x => return Err(ParseAstError::BadForm(x.span().clone())),
        }
    }
    Ok(parsed_fields)
}

impl DefineRecordType {
    pub fn parse(
        exprs: &[Syntax],
        env: &Environment<impl Top>,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match exprs {
            [first_arg, args @ ..] => {
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
                            return Err(ParseAstError::BadForm(span.clone()));
                        }
                    }
                    _ => return Err(ParseAstError::BadForm(span.clone())),
                };

                let mut parent: Option<(Identifier, Span)> = None;
                let mut fields: Option<(Vec<FieldDefinition>, Span)> = None;

                for arg in args {
                    match arg.as_list() {
                        Some(
                            [Syntax::Identifier {
                                ident,
                                span: second,
                                ..
                            }, Syntax::Identifier {
                                ident: parent_name, ..
                            }, Syntax::Null { .. }],
                        ) if ident.name == "parent" => {
                            if let Some((_, first)) = parent {
                                return Err(ParseAstError::ParentSpecifiedMultipleTimes {
                                    first: first.clone(),
                                    second: second.clone(),
                                });
                            }
                            parent = Some((parent_name.clone(), second.clone()));
                        }
                        Some(
                            [Syntax::Identifier {
                                ident,
                                span: second,
                                ..
                            }, unparsed_fields @ .., Syntax::Null { .. }],
                        ) if ident == "fields" => {
                            if let Some((_, first)) = fields {
                                return Err(ParseAstError::ParentSpecifiedMultipleTimes {
                                    first: first.clone(),
                                    second: second.clone(),
                                });
                            }

                            let parsed_fields = parse_fields(unparsed_fields)?;

                            // Check for fields with the same name:
                            let mut field_locs = HashMap::<String, Span>::new();

                            for field in &parsed_fields {
                                if let Some(first) = field_locs.get(&field.field_name.name) {
                                    return Err(ParseAstError::NameBoundMultipleTimes {
                                        ident: field.field_name.clone(),
                                        first: first.clone(),
                                        second: field.span.clone(),
                                    });
                                }
                                field_locs
                                    .insert(field.field_name.name.clone(), field.span.clone());
                            }

                            fields = Some((parsed_fields, span.clone()));
                        }
                        _ => return Err(ParseAstError::BadForm(span.clone())),
                    }
                }

                Ok(Self {
                    parent: parent
                        .map(|(x, _)| {
                            env.fetch_var(&x)
                                .ok_or_else(|| ParseAstError::UndefinedVariable(x.clone()))
                        })
                        .transpose()?,
                    name,
                    constructor,
                    predicate,
                    fields: fields.map(|(x, _)| x).unwrap_or_default(),
                })
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }

    /*
    pub fn define(&self, env: &Gc<Env>) {
        let mut env = env.write();
        let constructor_name = self
            .constructor
            .as_ref()
            .map_or_else(|| format!("make-{}", self.name.name), |cn| cn.name.clone());
        let constructor_name = Identifier::new(constructor_name);
        env.def_local_var(&constructor_name, Gc::new(Value::Undefined));

        let predicate_name = self
            .predicate
            .as_ref()
            .map_or_else(|| format!("{}?", self.name.name), |cn| cn.name.clone());
        let predicate_name = Identifier::new(predicate_name);
        env.def_local_var(&predicate_name, Gc::new(Value::Undefined));

        for field in &self.fields {
            let ident = field.field_name.clone();

            let accessor_name = field.accessor_name.as_ref().map_or_else(
                || format!("{}-{}", self.name.name, ident.name),
                |accessor| accessor.name.clone(),
            );
            let accessor_name = Identifier::new(accessor_name);
            env.def_local_var(&accessor_name, Gc::new(Value::Undefined));

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
                let mutator_name = Identifier::new(mutator_name);
                env.def_local_var(&mutator_name, Gc::new(Value::Undefined));
            }
        }

        env.def_local_var(&self.name, Gc::new(Value::Undefined));
    }
    */

    /*
    pub fn eval(&self, env: &Gc<Env>) -> Result<(), RuntimeError> {
        let inherits = if let Some(ref parent) = self.parent {
            let parent_gc = parent.fetch(env)?;
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

        let base_offset = fields.len();

        for field in &self.fields {
            let field = field.field_name.clone();
            fields.push(field);
        }

        let record_type = Gc::new(RecordType::new(&self.name.name));

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
        let mut setters: Vec<Expression> = (0..fields.len())
            .map(|offset| {
                Expression::UncheckedFieldMutation(UncheckedFieldMutation {
                    value: DeBruijnIndex::default().offset(offset).inc_depth(),
                    offset,
                })
            })
            .collect();

        // Append the return value
        setters.push(Expression::Var(VariableRef::Lexical(DeBruijnIndex::default())));

        let constructor = new_proc(
            env,
            // Yes, argument names shouldn't matter, but they do. Oh well.
            (0..fields.len())
                .map(|i| Identifier::new(format!("a{i}")))
                .collect(),
            Body::new(
                Vec::new(),
                vec![Expression::Let(Let::new(
                    vec![(
                        Identifier::new("this".to_string()),
                        Expression::MakeRecord(MakeRecord {
                            record_type: record_type.clone(),
                            num_fields: fields.len(),
                        }),
                    )],
                    Body::new(Vec::new(), setters),
                ))],
            ),
        );

        // Set up the predicate:
        let predicate = new_proc(
            env,
            vec![Identifier::new("this".to_string())],
            Body::new(
                Vec::new(),
                vec![Expression::RecordPredicate(RecordPredicate {
                    record_type: record_type.clone(),
                })],
            ),
        );

        let mut new_functions = HashMap::<String, Gc<Value>>::new();

        // Set up the new field accessors and mutators:
        for (offset, field) in self.fields.iter().enumerate() {
            let ident = field.field_name.clone();

            // Set up accessor:
            let accessor_name = field.accessor_name.as_ref().map_or_else(
                || format!("{}-{}", self.name.name, ident.name),
                |accessor| accessor.name.clone(),
            );
            let accessor = new_proc(
                env,
                vec![Identifier::new("this".to_string())],
                Body::new(
                    Vec::new(),
                    vec![Expression::FieldProjection(FieldProjection {
                        record_type: record_type.clone(),
                        offset: base_offset + offset,
                    })],
                ),
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
                    Body::new(
                        Vec::new(),
                        vec![Expression::FieldMutation(FieldMutation {
                            value: DeBruijnIndex::default().offset(1),
                            record_type: record_type.clone(),
                            offset: base_offset + offset,
                        })],
                    ),
                );
                new_functions.insert(mutator_name, mutator);
            }
        }

        // Now that we have all of the appropriate functions set up, we can
        // apply them to the environment.

        // All of these evaluations should be super simple, no need to worry about
        // the continuation being correct.

        let mut env = env.write();

        let constructor_name = self
            .constructor
            .as_ref()
            .map_or_else(|| format!("make-{}", self.name.name), |cn| cn.name.clone());
        let constructor_name = Identifier::new(constructor_name);
        env.def_local_var(&constructor_name, constructor);

        let predicate_name = self
            .predicate
            .as_ref()
            .map_or_else(|| format!("{}?", self.name.name), |cn| cn.name.clone());
        let predicate_name = Identifier::new(predicate_name);
        env.def_local_var(&predicate_name, predicate);

        for (new_function_name, new_function) in new_functions.into_iter() {
            env.def_local_var(&Identifier::new(new_function_name), new_function);
        }

        env.def_local_var(&self.name, Gc::new(Value::RecordType(record_type)));

        Ok(())
    }
    */
}

/*
fn new_proc(env: &Gc<Env>, args: Vec<Identifier>, body: ast::Body) -> Gc<Value> {
    Gc::new(Value::Procedure(Procedure {
        up: env.clone(),
        args,
        remaining: None,
        body,
        is_variable_transformer: false,
    }))
}
*/
