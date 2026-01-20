use crate::{
    ast::{Expression, Literal, ParseContext},
    env::{EnvId, Environment, Local},
    exceptions::Exception,
    gc::{Gc, Trace},
    proc::Procedure,
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    symbols::Symbol,
    syntax::{Identifier, Span, Syntax},
    value::Value,
};
use scheme_rs_macros::{bridge, maybe_async, maybe_await, runtime_fn};
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    sync::Arc,
};

mod error {
    use crate::exceptions::{Message, SyntaxViolation};

    use super::*;

    pub(super) fn no_pattern_variables_in_template(form: &Syntax) -> Exception {
        Exception::from((
            Message::new("no pattern variables in template"),
            SyntaxViolation::new(form.clone(), None),
        ))
    }
    pub(super) fn too_few_ellipsis(sym: Symbol, form: &Syntax) -> Exception {
        Exception::from((
            Message::new(format!("too few ellipsis for pattern variable {sym}",)),
            SyntaxViolation::new(form.clone(), None),
        ))
    }

    pub(super) fn too_many_ellipsis(sym: Symbol, form: &Syntax) -> Exception {
        Exception::from((
            Message::new(format!("too many ellipsis for pattern variable {sym}")),
            SyntaxViolation::new(form.clone(), None),
        ))
    }
}

#[derive(Clone, Trace, Debug)]
pub struct SyntaxRule {
    pub pattern: Pattern,
    pub binds: Local,
    pub fender: Option<Expression>,
    pub output_expression: Expression,
}

impl SyntaxRule {
    #[maybe_async]
    pub(crate) fn compile(
        ctxt: &ParseContext,
        keywords: &HashSet<Symbol>,
        pattern: &Syntax,
        fender: Option<&Syntax>,
        output_expression: &Syntax,
        env: &Environment,
    ) -> Result<Self, Exception> {
        let mut variables = HashMap::new();
        let pattern = Pattern::compile(pattern, keywords, 0, &mut variables);
        let binds = Local::gensym();
        let env = env.new_syntax_case_expr(binds, variables);
        let fender = if let Some(fender) = fender {
            Some(maybe_await!(Expression::parse(ctxt, fender.clone(), &env))?)
        } else {
            None
        };
        let output_expression =
            maybe_await!(Expression::parse(ctxt, output_expression.clone(), &env))?;
        Ok(Self {
            pattern,
            binds,
            fender,
            output_expression,
        })
    }
}

#[derive(Copy, Clone, Debug, Trace)]
pub struct Keyword {
    sym: Symbol,
    binding_env: Option<EnvId>,
}

#[derive(Clone, Debug, Trace)]
pub enum Pattern {
    Null,
    Underscore,
    Ellipsis(Box<Pattern>),
    List(Vec<Pattern>),
    Vector(Vec<Pattern>),
    ByteVector(Vec<u8>),
    Variable(Identifier),
    Keyword(Keyword),
    Literal(Literal),
}

impl Pattern {
    pub fn compile(
        expr: &Syntax,
        keywords: &HashSet<Symbol>,
        curr_nesting_level: usize,
        variables: &mut HashMap<Identifier, usize>,
    ) -> Self {
        match expr {
            Syntax::Null { .. } => Self::Null,
            Syntax::Identifier { ident, .. } if ident.sym == "_" => Self::Underscore,
            Syntax::Identifier {
                ident, binding_env, ..
            } if keywords.contains(&ident.sym) => Self::Keyword(Keyword {
                sym: ident.sym,
                binding_env: *binding_env,
            }),
            Syntax::Identifier { ident, .. } => {
                variables.insert(ident.clone(), curr_nesting_level);
                Self::Variable(ident.clone())
            }
            Syntax::List { list, .. } => Self::List(Self::compile_slice(
                list,
                keywords,
                curr_nesting_level,
                variables,
            )),
            Syntax::Vector { vector, .. } => Self::Vector(Self::compile_slice(
                vector,
                keywords,
                curr_nesting_level,
                variables,
            )),
            Syntax::ByteVector { vector, .. } => Self::ByteVector(vector.clone()),
            Syntax::Literal { literal, .. } => Self::Literal(literal.clone()),
        }
    }

    fn compile_slice(
        mut expr: &[Syntax],
        keywords: &HashSet<Symbol>,
        curr_nesting_level: usize,
        variables: &mut HashMap<Identifier, usize>,
    ) -> Vec<Self> {
        let mut output = Vec::new();
        loop {
            match expr {
                [] => break,
                [
                    pattern,
                    Syntax::Identifier {
                        ident: ellipsis, ..
                    },
                    tail @ ..,
                ] if ellipsis.sym == "..." => {
                    output.push(Self::Ellipsis(Box::new(Pattern::compile(
                        pattern,
                        keywords,
                        curr_nesting_level + 1,
                        variables,
                    ))));
                    expr = tail;
                }
                [head, tail @ ..] => {
                    output.push(Self::compile(head, keywords, curr_nesting_level, variables));
                    expr = tail;
                }
            }
        }
        output
    }

    fn matches(&self, expr: &Syntax, expansion_level: &mut ExpansionLevel) -> bool {
        match self {
            Self::Underscore => !expr.is_null(),
            Self::Variable(sym) => {
                assert!(
                    expansion_level
                        .binds
                        .insert(sym.clone(), expr.clone())
                        .is_none()
                );
                true
            }
            Self::Literal(lhs) => {
                if let Syntax::Literal { literal: rhs, .. } = expr {
                    lhs == rhs
                } else {
                    false
                }
            }
            Self::Keyword(lhs) => {
                matches!(
                    expr,
                    Syntax::Identifier {
                        ident: rhs,
                        binding_env,
                        ..
                    } if lhs.sym == rhs.sym
                        && lhs.binding_env == *binding_env
                )
            }
            Self::List(list) => match_list(list, expr, expansion_level),
            Self::Vector(vec) => match_vec(vec, expr, expansion_level),
            Self::ByteVector(vec) => {
                if let Self::ByteVector(v) = self {
                    v == vec
                } else {
                    false
                }
            }
            // We shouldn't ever see this outside of lists
            Self::Null => expr.is_null(),
            Self::Ellipsis(_) => unreachable!(),
        }
    }
}

fn match_ellipsis(
    patterns: &[Pattern],
    exprs: &[Syntax],
    expansion_level: &mut ExpansionLevel,
) -> bool {
    // The ellipsis gets to consume any extra items, thus the difference:
    let Some(extra_items) = (exprs.len() + 1).checked_sub(patterns.len()) else {
        return false;
    };

    let mut expr_iter = exprs.iter();
    for pattern in patterns.iter() {
        if let Pattern::Ellipsis(muncher) = pattern {
            // Gobble up the extra items:
            for i in 0..extra_items {
                if expansion_level.expansions.len() <= i {
                    expansion_level.expansions.push(ExpansionLevel::default());
                }
                let expr = expr_iter.next().unwrap();
                if !muncher.matches(expr, &mut expansion_level.expansions[i]) {
                    return false;
                }
            }
        } else {
            // Otherwise, match the pattern normally
            let expr = expr_iter.next().unwrap();
            if !pattern.matches(expr, expansion_level) {
                return false;
            }
        }
    }

    assert!(expr_iter.next().is_none());

    true
}

fn match_list(patterns: &[Pattern], expr: &Syntax, expansion_level: &mut ExpansionLevel) -> bool {
    assert!(!patterns.is_empty());

    let exprs = match expr {
        Syntax::List { list, .. } => list,
        Syntax::Null { .. } => std::slice::from_ref(expr),
        _ => return false,
    };

    let contains_ellipsis = patterns.iter().any(|p| matches!(p, Pattern::Ellipsis(_)));

    match (patterns.split_last().unwrap(), contains_ellipsis) {
        ((Pattern::Null, _), false) => {
            // Proper list, no ellipsis. Match everything in order
            if patterns.len() != exprs.len() {
                return false;
            }
            for (pattern, expr) in patterns.iter().zip(exprs.iter()) {
                if !pattern.matches(expr, expansion_level) {
                    return false;
                }
            }
            true
        }
        ((cdr, head), false) => {
            // The pattern is an improper list that contains no ellipsis.
            // Match in order until the last pattern, then match that to the nth
            // cdr.
            let mut exprs = exprs.iter();
            for pattern in head.iter() {
                let Some(expr) = exprs.next() else {
                    continue;
                };
                if !pattern.matches(expr, expansion_level) {
                    return false;
                }
            }
            // Match the cdr:
            let exprs: Vec<_> = exprs.cloned().collect();
            match exprs.as_slice() {
                [] => false,
                [x] => cdr.matches(x, expansion_level),
                _ => cdr.matches(
                    &Syntax::new_list(exprs, expr.span().clone()),
                    expansion_level,
                ),
            }
        }
        (_, true) => match_ellipsis(patterns, exprs, expansion_level),
    }
}

fn match_vec(patterns: &[Pattern], expr: &Syntax, expansion_level: &mut ExpansionLevel) -> bool {
    let Syntax::Vector { vector: exprs, .. } = expr else {
        return false;
    };

    let contains_ellipsis = patterns.iter().any(|p| matches!(p, Pattern::Ellipsis(_)));

    if contains_ellipsis {
        match_ellipsis(patterns, exprs, expansion_level)
    } else {
        if patterns.len() != exprs.len() {
            return false;
        }
        for (pattern, expr) in patterns.iter().zip(exprs.iter()) {
            if !pattern.matches(expr, expansion_level) {
                return false;
            }
        }
        true
    }
}

impl SchemeCompatible for Pattern {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "pattern", sealed: true, opaque: true)
    }
}

#[derive(Clone, Debug, Default, Trace)]
pub struct ExpansionLevel {
    binds: HashMap<Identifier, Syntax>,
    expansions: Vec<ExpansionLevel>,
}

impl SchemeCompatible for ExpansionLevel {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "expansion-level", sealed: true, opaque: true)
    }
}

#[derive(Trace, Debug)]
pub struct ExpansionCombiner {
    pub(crate) uses: HashMap<Identifier, usize>,
}

impl SchemeCompatible for ExpansionCombiner {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "expansion-combiner", sealed: true, opaque: true)
    }
}

#[runtime_fn]
unsafe extern "C" fn matches(pattern: *const (), syntax: *const ()) -> *const () {
    let pattern = unsafe { Value::from_raw_inc_rc(pattern) };
    let pattern = pattern.try_to_rust_type::<Pattern>().unwrap();

    let syntax = unsafe { Value::from_raw_inc_rc(syntax) };
    // This isn't a great way to do this, but it'll work for now:
    let syntax = Syntax::syntax_from_datum(&BTreeSet::default(), syntax).unwrap();

    let mut expansions = ExpansionLevel::default();
    if pattern.matches(&syntax, &mut expansions) {
        Value::into_raw(Value::from(Record::from_rust_type(expansions)))
    } else {
        Value::into_raw(Value::from(false))
    }
}

impl ExpansionCombiner {
    fn combine_expansions(&self, expansions: &[ExpansionLevel]) -> ExpansionLevel {
        let binds = self
            .uses
            .iter()
            .filter_map(|(ident, idx)| {
                expansions[*idx]
                    .binds
                    .get(ident)
                    .map(|expansion| (ident.clone(), expansion.clone()))
            })
            .collect::<HashMap<_, _>>();
        let max_expansions = expansions
            .iter()
            .map(|exp| exp.expansions.len())
            .max()
            .unwrap_or(0);
        let expansions = (0..max_expansions)
            .map(|i| {
                let expansions = expansions
                    .iter()
                    .map(|exp| {
                        if exp.expansions.len() <= i {
                            ExpansionLevel::default()
                        } else {
                            exp.expansions[i].clone()
                        }
                    })
                    .collect::<Vec<_>>();
                self.combine_expansions(&expansions)
            })
            .collect::<Vec<_>>();
        ExpansionLevel { binds, expansions }
    }
}

#[derive(Clone, Debug, Trace)]
pub enum Template {
    Null,
    Ellipsis(Box<Template>),
    List(Vec<Template>),
    Vector(Vec<Template>),
    ByteVector(Vec<u8>),
    Identifier {
        ident: Identifier,
        binding_env: Option<EnvId>,
    },
    Variable(Identifier),
    Literal(Literal),
}

impl Template {
    pub(crate) fn compile<'a>(
        expr: &'a Syntax,
        env: &Environment,
        expansions: &mut HashMap<Identifier, Local>,
        resolved_bindings: &mut HashMap<&'a Identifier, Option<EnvId>>,
    ) -> Result<Self, Exception> {
        check_ellipsis(expr, env)?;
        Self::compile_inner(expr, env, expansions, resolved_bindings)
    }

    fn compile_inner<'a>(
        expr: &'a Syntax,
        env: &Environment,
        expansions: &mut HashMap<Identifier, Local>,
        resolved_bindings: &mut HashMap<&'a Identifier, Option<EnvId>>,
    ) -> Result<Self, Exception> {
        match expr {
            Syntax::Null { .. } => Ok(Self::Null),
            Syntax::List { list, .. } => {
                if let [
                    Syntax::Identifier {
                        ident: ellipsis, ..
                    },
                    template,
                    Syntax::Null { .. },
                ] = list.as_slice()
                    && ellipsis.sym == "..."
                {
                    Self::compile_escaped(template, env, expansions, resolved_bindings)
                } else {
                    Ok(Self::List(Self::compile_slice(
                        list,
                        env,
                        expansions,
                        resolved_bindings,
                    )?))
                }
            }
            Syntax::Vector { vector, .. } => Ok(Self::Vector(Self::compile_slice(
                vector,
                env,
                expansions,
                resolved_bindings,
            )?)),
            Syntax::ByteVector { vector, .. } => Ok(Self::ByteVector(vector.clone())),
            Syntax::Literal { literal, .. } => Ok(Self::Literal(literal.clone())),
            Syntax::Identifier { ident, .. } if ident.sym == "..." => {
                panic!("error")
            }
            Syntax::Identifier { ident, .. } => {
                if let Some((expansion, _)) = env.fetch_pattern_variable(ident) {
                    expansions.insert(ident.clone(), expansion);
                    Ok(Self::Variable(ident.clone()))
                } else {
                    Ok(Self::Identifier {
                        ident: ident.clone(),
                        binding_env: *resolved_bindings
                            .entry(ident)
                            .or_insert_with(|| env.binding_env(ident)),
                    })
                }
            }
        }
    }

    pub(crate) fn compile_escaped<'a>(
        expr: &'a Syntax,
        env: &Environment,
        expansions: &mut HashMap<Identifier, Local>,
        resolved_bindings: &mut HashMap<&'a Identifier, Option<EnvId>>,
    ) -> Result<Self, Exception> {
        match expr {
            Syntax::Null { .. } => Ok(Self::Null),
            Syntax::List { list, .. } => Ok(Self::List(Self::compile_slice_escaped(
                list,
                env,
                expansions,
                resolved_bindings,
            )?)),
            Syntax::Vector { vector, .. } => Ok(Self::Vector(Self::compile_slice_escaped(
                vector,
                env,
                expansions,
                resolved_bindings,
            )?)),
            Syntax::ByteVector { vector, .. } => Ok(Self::ByteVector(vector.clone())),
            Syntax::Literal { literal, .. } => Ok(Self::Literal(literal.clone())),
            Syntax::Identifier { ident, .. } => {
                if let Some((expansion, _)) = env.fetch_pattern_variable(ident) {
                    expansions.insert(ident.clone(), expansion);
                    Ok(Self::Variable(ident.clone()))
                } else {
                    Ok(Self::Identifier {
                        ident: ident.clone(),
                        binding_env: *resolved_bindings
                            .entry(ident)
                            .or_insert_with(|| env.binding_env(ident)),
                    })
                }
            }
        }
    }

    fn compile_slice<'a>(
        mut expr: &'a [Syntax],
        env: &Environment,
        expansions: &mut HashMap<Identifier, Local>,
        resolved_bindings: &mut HashMap<&'a Identifier, Option<EnvId>>,
    ) -> Result<Vec<Self>, Exception> {
        let mut output = Vec::new();
        loop {
            match expr {
                [] => break,
                [
                    template,
                    Syntax::Identifier {
                        ident: ellipsis, ..
                    },
                    tail @ ..,
                ] if ellipsis.sym == "..." => {
                    let mut compiled = Self::Ellipsis(Box::new(Self::compile_inner(
                        template,
                        env,
                        expansions,
                        resolved_bindings,
                    )?));
                    let mut tail = &tail[..];
                    while matches!(tail.first(), Some(Syntax::Identifier { ident, ..}) if ident.sym == "...")
                    {
                        compiled = Self::Ellipsis(Box::new(compiled));
                        tail = &tail[1..];
                    }
                    output.push(compiled);
                    expr = &tail;
                }
                [head, tail @ ..] => {
                    output.push(Self::compile_inner(
                        head,
                        env,
                        expansions,
                        resolved_bindings,
                    )?);
                    expr = tail;
                }
            }
        }
        Ok(output)
    }

    fn compile_slice_escaped<'a>(
        exprs: &'a [Syntax],
        env: &Environment,
        expansions: &mut HashMap<Identifier, Local>,
        resolved_bindings: &mut HashMap<&'a Identifier, Option<EnvId>>,
    ) -> Result<Vec<Self>, Exception> {
        exprs
            .iter()
            .map(|expr| Self::compile_escaped(expr, env, expansions, resolved_bindings))
            .collect()
    }

    fn expand(&self, binds: &Binds<'_>, curr_span: Span) -> Option<Syntax> {
        let syn = match self {
            Self::Null => Syntax::new_null(curr_span),
            Self::List(list) => expand_list(list, binds, curr_span.clone())?,
            Self::Vector(vec) => {
                Syntax::new_vector(expand_vec(vec, binds, curr_span.clone())?, curr_span)
            }
            Self::ByteVector(bvec) => Syntax::new_byte_vector(bvec.clone(), curr_span),
            Self::Identifier { ident, binding_env } => Syntax::Identifier {
                ident: ident.clone(),
                span: curr_span,
                binding_env: *binding_env,
            },
            Self::Variable(name) => binds.get_bind(name)?,
            Self::Literal(literal) => Syntax::new_literal(literal.clone(), curr_span),
            Self::Ellipsis(_) => unreachable!(),
        };
        Some(syn)
    }

    fn expand_nested(&self, binds: &Binds<'_>, curr_span: Span) -> Option<Vec<Syntax>> {
        let mut output = Vec::new();
        if let Template::Ellipsis(template) = self {
            for expansion in &binds.curr_expansion_level.expansions {
                let new_level = binds.new_level(expansion);
                let Some(result) = template.expand_nested(&new_level, curr_span.clone()) else {
                    break;
                };
                output.extend(result);
            }
        } else {
            output.push(self.expand(binds, curr_span.clone())?);
        }
        Some(output)
    }
}

fn expand_list(items: &[Template], binds: &Binds<'_>, curr_span: Span) -> Option<Syntax> {
    let mut output = Vec::new();
    for item in items {
        output.extend(item.expand_nested(binds, curr_span.clone())?);
    }
    Some(normalize_list(output, curr_span))
}

/// Because we flatten lists into vectors for syntax objects, its necessary to
/// normalize the list after expansion. Specifically, after expansion, if the
/// last element of a list is another list, the list needs to be flattened. After
/// flattening if the vec is empty or a single Null long, it can be replaced with
/// just a Null.
fn normalize_list(mut list: Vec<Syntax>, span: Span) -> Syntax {
    // Check for flattening:
    if matches!(list.as_slice(), &[.., Syntax::List { .. }]) {
        let Some(Syntax::List { list: tail, .. }) = list.pop() else {
            unreachable!()
        };
        list.extend(tail);
    }
    // We should only have to do this once, if a list is the last element after
    // flattening something has gone wrong.
    assert!(!matches!(list.last(), Some(Syntax::List { .. })));
    // Check for empty/null list:
    match list.as_slice() {
        [] | [Syntax::Null { .. }] => Syntax::Null { span },
        _ => Syntax::new_list(list, span),
    }
}

fn expand_vec(items: &[Template], binds: &Binds<'_>, curr_span: Span) -> Option<Vec<Syntax>> {
    let mut output = Vec::new();
    for item in items {
        output.extend(item.expand_nested(binds, curr_span.clone())?);
    }
    Some(output)
}

fn check_ellipsis(expr: &Syntax, env: &Environment) -> Result<(), Exception> {
    let binds = match expr {
        Syntax::Null { .. } | Syntax::ByteVector { .. } | Syntax::Literal { .. } => return Ok(()),
        Syntax::List { list, .. } => {
            if let [
                Syntax::Identifier {
                    ident: ellipsis, ..
                },
                template,
                Syntax::Null { .. },
            ] = list.as_slice()
                && ellipsis.sym == "..."
            {
                check_escaped_template(template, env)
            } else {
                check_subtemplate(&list, env)?
            }
        }
        Syntax::Vector { vector, .. } => check_subtemplate(&vector, env)?,
        Syntax::Identifier { ident, .. } => {
            if let Some((_, depth)) = env.fetch_pattern_variable(ident)
                && depth != 0
            {
                return Err(error::too_few_ellipsis(ident.sym, expr));
            } else {
                return Ok(());
            }
        }
    };

    if let Some((sym, _)) = binds.iter().find(|(_, depth)| *depth > 0) {
        return Err(error::too_few_ellipsis(*sym, expr));
    }
    let has_proper_pattern_var = binds.iter().any(|(_, depth)| *depth == 0);
    if !has_proper_pattern_var && let Some((sym, _)) = binds.iter().find(|(_, depth)| *depth < 0) {
        return Err(error::too_many_ellipsis(*sym, expr));
    }

    Ok(())
}

fn check_template(expr: &Syntax, env: &Environment) -> Result<Vec<(Symbol, isize)>, Exception> {
    match expr {
        Syntax::Null { .. } | Syntax::ByteVector { .. } | Syntax::Literal { .. } => Ok(Vec::new()),
        Syntax::List { list, .. } => {
            if let [
                Syntax::Identifier {
                    ident: ellipsis, ..
                },
                template,
                Syntax::Null { .. },
            ] = list.as_slice()
                && ellipsis.sym == "..."
            {
                Ok(check_escaped_template(template, env))
            } else {
                check_subtemplate(&list, env)
            }
        }
        Syntax::Vector { vector, .. } => check_subtemplate(&vector, env),
        Syntax::Identifier { ident, .. } => {
            if let Some((_, depth)) = env.fetch_pattern_variable(ident) {
                Ok(vec![(ident.sym, depth as isize)])
            } else {
                Ok(Vec::new())
            }
        }
    }
}

fn check_subtemplate(
    expr: &[Syntax],
    env: &Environment,
) -> Result<Vec<(Symbol, isize)>, Exception> {
    match expr {
        [] => Ok(Vec::new()),
        [template, tail @ ..] => {
            let mut num_ellipsis = 0;
            let mut tail = &tail[..];
            while matches!(tail.first(), Some(Syntax::Identifier { ident, ..}) if ident.sym == "...")
            {
                num_ellipsis += 1;
                tail = &tail[1..];
            }
            let mut patterns = check_template(template, env)?;
            if patterns.is_empty() && num_ellipsis > 0 {
                return Err(error::no_pattern_variables_in_template(template));
            }
            for pattern in &mut patterns {
                pattern.1 -= num_ellipsis;
            }
            patterns.extend(check_subtemplate(tail, env)?);
            Ok(patterns)
        }
    }
}

fn check_escaped_template(expr: &Syntax, env: &Environment) -> Vec<(Symbol, isize)> {
    match expr {
        Syntax::List { list, .. } => list
            .iter()
            .flat_map(|template| check_escaped_template(template, env))
            .collect(),
        Syntax::Vector { vector, .. } => vector
            .iter()
            .flat_map(|template| check_escaped_template(template, env))
            .collect(),
        Syntax::Identifier { ident, .. } => {
            if let Some((_, depth)) = env.fetch_pattern_variable(ident) {
                vec![(ident.sym, depth as isize)]
            } else {
                Vec::new()
            }
        }
        _ => Vec::new(),
    }
}

impl SchemeCompatible for Template {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "template", sealed: true, opaque: true)
    }
}

#[runtime_fn]
unsafe extern "C" fn expand_template(
    template: *const (),
    expansion_combiner: *const (),
    expansions: *const *const (),
    num_expansions: u32,
) -> *const () {
    // TODO: A lot of probably unnecessary cloning here, we'll need to fix it up
    // eventually

    let template = unsafe { Value::from_raw_inc_rc(template) };
    let template = template.try_to_rust_type::<Template>().unwrap();

    let expansion_combiner = unsafe { Value::from_raw_inc_rc(expansion_combiner) };
    let expansion_combiner = expansion_combiner
        .try_to_rust_type::<ExpansionCombiner>()
        .unwrap();

    let expansions = (0..num_expansions)
        .map(|i| {
            let expansion = unsafe { Value::from_raw_inc_rc(expansions.add(i as usize).read()) };
            let expansion = expansion.try_to_rust_type::<ExpansionLevel>().unwrap();
            expansion.as_ref().clone()
        })
        .collect::<Vec<_>>();

    let combined_expansions = expansion_combiner.combine_expansions(&expansions);
    let binds = Binds::new_top(&combined_expansions);

    // TODO: get a real span in here
    let expanded = template.expand(&binds, Span::default()).unwrap();

    Value::into_raw(Value::from(expanded))
}

#[derive(Debug)]
pub struct Binds<'a> {
    curr_expansion_level: &'a ExpansionLevel,
    parent_expansion_level: Option<&'a Binds<'a>>,
}

impl<'a> Binds<'a> {
    fn new_top(top_expansion_level: &'a ExpansionLevel) -> Self {
        Self {
            curr_expansion_level: top_expansion_level,
            parent_expansion_level: None,
        }
    }

    fn new_level<'b: 'a>(&'b self, next_expansion_level: &'b ExpansionLevel) -> Binds<'b> {
        Binds {
            curr_expansion_level: next_expansion_level,
            parent_expansion_level: Some(self),
        }
    }

    fn get_bind(&self, ident: &Identifier) -> Option<Syntax> {
        if let bind @ Some(_) = self.curr_expansion_level.binds.get(ident) {
            bind.cloned()
        } else if let Some(up) = self.parent_expansion_level {
            up.get_bind(ident)
        } else {
            None
        }
    }
}

#[runtime_fn]
unsafe extern "C" fn error_no_patterns_match() -> i64 {
    let condition = Exception::error("no patterns match".to_string());
    Value::into_raw(Value::from(condition)) as i64
}

#[bridge(name = "make-variable-transformer", lib = "(rnrs base builtins (6))")]
pub fn make_variable_transformer(proc: &Value) -> Result<Vec<Value>, Exception> {
    let proc: Procedure = proc.clone().try_into()?;
    let mut var_transformer = proc.0.as_ref().clone();
    var_transformer.is_variable_transformer = true;
    Ok(vec![Value::from(Procedure(Gc::new(var_transformer)))])
}
