use crate::{
    ast::{Expression, Literal, ParseAstError},
    env::{Environment, Local},
    exception::Condition,
    gc::{Gc, Trace},
    proc::{Application, Closure},
    runtime::Runtime,
    symbols::Symbol,
    syntax::{Identifier, Span, Syntax},
    value::Value,
};
use ahash::{AHashMap, AHashSet};
use scheme_rs_macros::{bridge, runtime_fn};
use std::{any::Any, collections::BTreeSet};

// TODO: This code needs _a lot_ more error checking: error checking for missing
// ellipsis, error checking for too many ellipsis. It makes debugging extremely
// confusing!

#[derive(Clone, Trace, Debug)]
pub struct SyntaxRule {
    pub pattern: Pattern,
    pub binds: Local,
    pub fender: Option<Expression>,
    pub output_expression: Expression,
}

impl SyntaxRule {
    pub async fn compile(
        rt: &Runtime,
        keywords: &AHashSet<Symbol>,
        pattern: &Syntax,
        fender: Option<&Syntax>,
        output_expression: &Syntax,
        env: &Environment,
    ) -> Result<Self, ParseAstError> {
        let mut variables = AHashSet::new();
        let pattern = Pattern::compile(pattern, keywords, &mut variables);
        let binds = Local::gensym();
        let env = env.new_syntax_case_expr(binds, variables);
        let fender = if let Some(fender) = fender {
            Some(Expression::parse(rt, fender.clone(), &env).await?)
        } else {
            None
        };
        let output_expression = Expression::parse(rt, output_expression.clone(), &env).await?;
        Ok(Self {
            pattern,
            binds,
            fender,
            output_expression,
        })
    }
}

#[derive(Clone, Debug, Trace, PartialEq)]
pub enum Pattern {
    Null,
    Underscore,
    Ellipsis(Box<Pattern>),
    List(Vec<Pattern>),
    Vector(Vec<Pattern>),
    ByteVector(Vec<u8>),
    Variable(Identifier),
    Keyword(Symbol),
    Literal(Literal),
}

impl Pattern {
    pub fn compile(
        expr: &Syntax,
        keywords: &AHashSet<Symbol>,
        variables: &mut AHashSet<Identifier>,
    ) -> Self {
        match expr {
            Syntax::Null { .. } => Self::Null,
            Syntax::Identifier { ident, .. } if ident.sym == "_" => Self::Underscore,
            Syntax::Identifier { ident, .. } if keywords.contains(&ident.sym) => {
                Self::Keyword(ident.sym)
            }
            Syntax::Identifier { ident, .. } => {
                variables.insert(ident.clone());
                Self::Variable(ident.clone())
            }
            Syntax::List { list, .. } => Self::List(Self::compile_slice(list, keywords, variables)),
            Syntax::Vector { vector, .. } => {
                Self::Vector(Self::compile_slice(vector, keywords, variables))
            }
            Syntax::ByteVector { vector, .. } => Self::ByteVector(vector.clone()),
            Syntax::Literal { literal, .. } => Self::Literal(literal.clone()),
        }
    }

    fn compile_slice(
        mut expr: &[Syntax],
        keywords: &AHashSet<Symbol>,
        variables: &mut AHashSet<Identifier>,
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
                        pattern, keywords, variables,
                    ))));
                    expr = tail;
                }
                [head, tail @ ..] => {
                    output.push(Self::compile(head, keywords, variables));
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
                matches!(expr, Syntax::Identifier { ident: rhs, bound: false, .. } if lhs == &rhs.sym)
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

#[derive(Clone, Debug, Default, Trace)]
pub struct ExpansionLevel {
    binds: AHashMap<Identifier, Syntax>,
    expansions: Vec<ExpansionLevel>,
}

#[derive(Trace, Debug)]
pub struct ExpansionCombiner {
    pub(crate) uses: AHashMap<Identifier, usize>,
}

#[runtime_fn]
unsafe extern "C" fn matches(pattern: i64, syntax: i64) -> i64 {
    let pattern = unsafe { Value::from_raw_inc_rc(pattern as u64) };
    let pattern: Gc<Gc<dyn Any>> = pattern.try_into().unwrap();
    let Ok(pattern) = pattern.read().clone().downcast::<Pattern>() else {
        panic!()
    };

    let syntax = unsafe { Value::from_raw_inc_rc(syntax as u64) };
    // This isn't a great way to do this, but it'll work for now:
    let syntax = Syntax::syntax_from_datum(&BTreeSet::default(), syntax);

    let mut expansions = ExpansionLevel::default();
    if pattern.read().matches(&syntax, &mut expansions) {
        let expansions = Gc::into_any(Gc::new(expansions));
        Value::into_raw(Value::from(Gc::new(expansions))) as i64
    } else {
        Value::into_raw(Value::from(false)) as i64
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
            .collect::<AHashMap<_, _>>();
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
    Identifier(Identifier),
    Variable(Identifier),
    Literal(Literal),
}

impl Template {
    pub fn compile(
        expr: &Syntax,
        env: &Environment,
        expansions: &mut AHashMap<Identifier, Local>,
    ) -> Self {
        match expr {
            Syntax::Null { .. } => Self::Null,
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
                    Self::compile_escaped(template, env, expansions)
                } else {
                    Self::List(Self::compile_slice(list, env, expansions))
                }
            }
            Syntax::Vector { vector, .. } => {
                Self::Vector(Self::compile_slice(vector, env, expansions))
            }
            Syntax::ByteVector { vector, .. } => Self::ByteVector(vector.clone()),
            Syntax::Literal { literal, .. } => Self::Literal(literal.clone()),
            // Syntax::Identifier { ident, .. } if variables.contains(&ident) => {
            //     Self::Variable(ident.clone())
            // }
            Syntax::Identifier { ident, .. } => {
                if let Some(expansion) = env.fetch_pattern_variable(ident) {
                    expansions.insert(ident.clone(), expansion);
                    Self::Variable(ident.clone())
                } else {
                    Self::Identifier(ident.clone())
                }
            }
        }
    }

    pub fn compile_escaped(
        expr: &Syntax,
        env: &Environment,
        expansions: &mut AHashMap<Identifier, Local>,
    ) -> Self {
        match expr {
            Syntax::Null { .. } => Self::Null,
            Syntax::List { list, .. } => {
                Self::List(Self::compile_slice_escaped(list, env, expansions))
            }
            Syntax::Vector { vector, .. } => {
                Self::Vector(Self::compile_slice_escaped(vector, env, expansions))
            }
            Syntax::ByteVector { vector, .. } => Self::ByteVector(vector.clone()),
            Syntax::Literal { literal, .. } => Self::Literal(literal.clone()),
            Syntax::Identifier { ident, .. } => {
                if let Some(expansion) = env.fetch_pattern_variable(ident) {
                    expansions.insert(ident.clone(), expansion);
                    Self::Variable(ident.clone())
                } else {
                    Self::Identifier(ident.clone())
                }
            }
        }
    }

    fn compile_slice(
        mut expr: &[Syntax],
        env: &Environment,
        expansions: &mut AHashMap<Identifier, Local>,
    ) -> Vec<Self> {
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
                    output.push(Self::Ellipsis(Box::new(Template::compile(
                        template, env, expansions,
                    ))));
                    expr = tail;
                }
                [head, tail @ ..] => {
                    output.push(Self::compile(head, env, expansions));
                    expr = tail;
                }
            }
        }
        output
    }

    fn compile_slice_escaped(
        exprs: &[Syntax],
        env: &Environment,
        expansions: &mut AHashMap<Identifier, Local>,
    ) -> Vec<Self> {
        exprs
            .iter()
            .map(|expr| Self::compile_escaped(expr, env, expansions))
            .collect()
    }

    fn expand(&self, binds: &Binds<'_>, curr_span: Span) -> Option<Syntax> {
        let syn = match self {
            Self::Null => Syntax::new_null(curr_span),
            Self::List(list) => expand_list(list, binds, curr_span.clone())?,
            Self::Vector(vec) => {
                Syntax::new_vector(expand_vec(vec, binds, curr_span.clone())?, curr_span)
            }
            Self::Identifier(ident) => Syntax::Identifier {
                ident: ident.clone(),
                span: curr_span,
                bound: false,
            },
            Self::Variable(name) => binds.get_bind(name)?,
            Self::Literal(literal) => Syntax::new_literal(literal.clone(), curr_span),
            _ => unreachable!(),
        };
        Some(syn)
    }
}

#[runtime_fn]
unsafe extern "C" fn expand_template(
    template: i64,
    expansion_combiner: i64,
    expansions: *const i64,
    num_expansions: u32,
) -> i64 {
    // TODO: A lot of probably unnecessary cloning here, we'll need to fix it up
    // eventually

    let template = unsafe { Value::from_raw_inc_rc(template as u64) };
    let template: Gc<Gc<dyn Any>> = template.try_into().unwrap();
    let Ok(template) = template.read().clone().downcast::<Template>() else {
        panic!()
    };

    let expansion_combiner = unsafe { Value::from_raw_inc_rc(expansion_combiner as u64) };
    let expansion_combiner: Gc<Gc<dyn Any>> = expansion_combiner.try_into().unwrap();
    let Ok(expansion_combiner) = expansion_combiner
        .read()
        .clone()
        .downcast::<ExpansionCombiner>()
    else {
        panic!()
    };

    let expansions = (0..num_expansions)
        .map(|i| {
            let expansion =
                unsafe { Value::from_raw_inc_rc(expansions.add(i as usize).read() as u64) };
            let expansion: Gc<Gc<dyn Any>> = expansion.clone().try_into().unwrap();
            let Ok(expansion) = expansion.read().clone().downcast::<ExpansionLevel>() else {
                panic!()
            };
            expansion.read().clone()
        })
        .collect::<Vec<_>>();

    let combined_expansions = expansion_combiner.read().combine_expansions(&expansions);
    let binds = Binds::new_top(&combined_expansions);

    // TODO: get a real span in here
    let expanded = template.read().expand(&binds, Span::default()).unwrap();

    Value::into_raw(Value::from(expanded)) as i64
}

fn expand_list(items: &[Template], binds: &Binds<'_>, curr_span: Span) -> Option<Syntax> {
    let mut output = Vec::new();
    for item in items {
        if let Template::Ellipsis(template) = item {
            for expansion in &binds.curr_expansion_level.expansions {
                let new_level = binds.new_level(expansion);
                let Some(result) = template.expand(&new_level, curr_span.clone()) else {
                    break;
                };
                output.push(result);
            }
        } else {
            output.push(item.expand(binds, curr_span.clone())?);
        }
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
        match item {
            Template::Ellipsis(template) => {
                for expansion in &binds.curr_expansion_level.expansions {
                    let new_level = binds.new_level(expansion);
                    let Some(result) = template.expand(&new_level, curr_span.clone()) else {
                        break;
                    };
                    output.push(result);
                }
            }
            item => output.push(item.expand(binds, curr_span.clone())?),
        }
    }
    Some(output)
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
    let condition = Condition::error("No patterns match!".to_string());
    Value::into_raw(Value::from(condition)) as i64
}

#[bridge(name = "make-variable-transformer", lib = "(rnrs base builtins (6))")]
pub async fn make_variable_transformer(proc: &Value) -> Result<Vec<Value>, Condition> {
    let proc: Closure = proc.clone().try_into()?;
    let mut var_transformer = proc.0.read().clone();
    var_transformer.is_variable_transformer = true;
    Ok(vec![Value::from(Closure(Gc::new(var_transformer)))])
}
