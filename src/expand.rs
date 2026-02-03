use crate::{
    ast::{Expression, ParseContext},
    env::{Binding, Environment, Local, Scope, add_binding},
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
    collections::{HashMap, HashSet},
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
    pub(crate) fn compile<'a>(
        ctxt: &ParseContext,
        keywords: &HashSet<&'a Identifier>,
        pattern: &'a Syntax,
        fender: Option<&'a Syntax>,
        output_expression: &'a Syntax,
        env: &Environment,
    ) -> Result<Self, Exception> {
        let mut variables = HashMap::new();
        let pattern_scope = Scope::new();
        let pattern = Pattern::compile(pattern, keywords, 0, pattern_scope, &mut variables);
        let binds = Local::gensym();
        let env = env.new_syntax_case_contour(pattern_scope, binds, variables);
        let fender = if let Some(fender) = fender {
            let mut fender = fender.clone();
            fender.add_scope(pattern_scope);
            Some(maybe_await!(Expression::parse(ctxt, fender, &env))?)
        } else {
            None
        };
        let mut output_expression = output_expression.clone();
        output_expression.add_scope(pattern_scope);
        let output_expression = maybe_await!(Expression::parse(ctxt, output_expression, &env))?;
        Ok(Self {
            pattern,
            binds,
            fender,
            output_expression,
        })
    }
}

#[derive(Clone, Debug, Trace)]
pub enum Pattern {
    Underscore,
    Ellipsis(Box<Pattern>),
    List(Vec<Pattern>),
    Vector(Vec<Pattern>),
    Variable(Binding),
    Keyword(Identifier),
    Literal(Value),
}

impl Pattern {
    pub fn compile<'a>(
        expr: &'a Syntax,
        keywords: &HashSet<&'a Identifier>,
        curr_nesting_level: usize,
        scope: Scope,
        variables: &mut HashMap<Binding, usize>,
    ) -> Self {
        match expr {
            Syntax::Identifier { ident, .. } if ident.sym == "_" => Self::Underscore,
            Syntax::Identifier { ident, .. } if keywords.contains(&ident) => {
                Self::Keyword(ident.clone())
            }
            Syntax::Identifier { ident, .. } => {
                let mut ident = ident.clone();
                ident.add_scope(scope);
                let binding = Binding::new();
                add_binding(ident, binding);
                variables.insert(binding, curr_nesting_level);
                Self::Variable(binding)
            }
            Syntax::List { list, .. } => Self::List(Self::compile_slice(
                list,
                keywords,
                curr_nesting_level,
                scope,
                variables,
            )),
            Syntax::Vector { vector, .. } => Self::Vector(Self::compile_slice(
                vector,
                keywords,
                curr_nesting_level,
                scope,
                variables,
            )),
            Syntax::Wrapped { value, .. } => Self::Literal(value.clone()),
        }
    }

    fn compile_slice<'a>(
        mut expr: &'a [Syntax],
        keywords: &HashSet<&'a Identifier>,
        curr_nesting_level: usize,
        scope: Scope,
        variables: &mut HashMap<Binding, usize>,
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
                        scope,
                        variables,
                    ))));
                    expr = tail;
                }
                [head, tail @ ..] => {
                    output.push(Self::compile(
                        head,
                        keywords,
                        curr_nesting_level,
                        scope,
                        variables,
                    ));
                    expr = tail;
                }
            }
        }
        output
    }

    fn matches(&self, expr: &Syntax, expansion_level: &mut ExpansionLevel) -> bool {
        match self {
            Self::Underscore => !expr.is_null(),
            Self::Variable(binding) => {
                assert!(
                    expansion_level
                        .binds
                        .insert(*binding, expr.clone())
                        .is_none()
                );
                true
            }
            Self::Literal(lhs) => {
                if let Syntax::Wrapped { value: rhs, .. } = expr {
                    lhs.equal(rhs)
                } else {
                    false
                }
            }
            Self::Keyword(lhs) => {
                if let Syntax::Identifier { ident: rhs, .. } = expr {
                    lhs.free_identifier_equal(rhs)
                } else {
                    false
                }
            }
            Self::List(list) => match_list(list, expr, expansion_level),
            Self::Vector(vec) => match_vec(vec, expr, expansion_level),
            // We shouldn't ever see this outside of lists
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
        Syntax::Wrapped { value, .. } if value.is_null() => std::slice::from_ref(expr),
        _ => return false,
    };

    if patterns.iter().any(|p| matches!(p, Pattern::Ellipsis(_))) {
        match_ellipsis(patterns, exprs, expansion_level)
    } else if let Some((cdr, head)) = patterns.split_last() {
        // The pattern is an list that contains no ellipsis.
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
    } else {
        false
    }
}

fn match_vec(patterns: &[Pattern], expr: &Syntax, expansion_level: &mut ExpansionLevel) -> bool {
    let Syntax::Vector { vector: exprs, .. } = expr else {
        return false;
    };

    let contains_ellipsis = patterns.iter().any(|p| matches!(p, Pattern::Ellipsis(_)));

    if contains_ellipsis {
        match_ellipsis(patterns, exprs, expansion_level)
    } else if patterns.len() != exprs.len() {
        false
    } else {
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
    binds: HashMap<Binding, Syntax>,
    expansions: Vec<ExpansionLevel>,
}

impl SchemeCompatible for ExpansionLevel {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "expansion-level", sealed: true, opaque: true)
    }
}

#[derive(Trace, Debug)]
pub struct ExpansionCombiner {
    pub(crate) uses: HashMap<Binding, usize>,
}

impl SchemeCompatible for ExpansionCombiner {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "expansion-combiner", sealed: true, opaque: true)
    }
}

#[runtime_fn]
unsafe extern "C" fn matches(pattern: *const (), value: *const ()) -> *const () {
    let pattern = unsafe { Value::from_raw_inc_rc(pattern) };
    let pattern = pattern.try_to_rust_type::<Pattern>().unwrap();

    let value = unsafe { Value::from_raw_inc_rc(value) };
    let syntax = Syntax::wrap(value);

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
            .filter_map(|(binding, idx)| {
                expansions[*idx]
                    .binds
                    .get(binding)
                    .map(|expansion| (*binding, expansion.clone()))
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
    // Null,
    Ellipsis(Box<Template>),
    List(Vec<Template>),
    Vector(Vec<Template>),
    Variable(Binding),
    Wrapped(Syntax),
}

impl Template {
    pub(crate) fn compile(
        expr: &Syntax,
        env: &Environment,
        expansions: &mut HashMap<Binding, Local>,
    ) -> Result<Self, Exception> {
        check_ellipsis(expr, env)?;
        Self::compile_inner(expr, env, expansions)
    }

    fn is_wrapped(&self) -> bool {
        matches!(self, Self::Wrapped(_))
    }

    fn new_list(list: Vec<Template>, span: Span) -> Self {
        if list.iter().all(Template::is_wrapped) {
            Self::Wrapped(Syntax::new_list(
                list.into_iter()
                    .map(|wrapped| match wrapped {
                        Template::Wrapped(wrapped) => wrapped,
                        _ => unreachable!(),
                    })
                    .collect(),
                span,
            ))
        } else {
            Self::List(list)
        }
    }

    fn new_vector(vec: Vec<Template>, span: Span) -> Self {
        if vec.iter().all(Template::is_wrapped) {
            Self::Wrapped(Syntax::new_vector(
                vec.into_iter()
                    .map(|wrapped| match wrapped {
                        Template::Wrapped(wrapped) => wrapped,
                        _ => unreachable!(),
                    })
                    .collect(),
                span,
            ))
        } else {
            Self::Vector(vec)
        }
    }

    fn compile_inner(
        expr: &Syntax,
        env: &Environment,
        expansions: &mut HashMap<Binding, Local>,
    ) -> Result<Self, Exception> {
        match expr {
            Syntax::List { list, span, .. } => {
                if let [
                    Syntax::Identifier {
                        ident: ellipsis, ..
                    },
                    template,
                    _,
                ] = list.as_slice()
                    && ellipsis.sym == "..."
                {
                    Self::compile_escaped(template, env, expansions)
                } else {
                    Ok(Self::new_list(
                        Self::compile_slice(list, env, expansions)?,
                        span.clone(),
                    ))
                }
            }
            Syntax::Vector { vector, span, .. } => Ok(Self::new_vector(
                Self::compile_slice(vector, env, expansions)?,
                span.clone(),
            )),
            Syntax::Identifier { ident, .. } if ident.sym == "..." => {
                Err(Exception::syntax(expr.clone(), None))
            }
            Syntax::Identifier { ident, span, .. } => {
                if let Some(binding) = ident.resolve()
                    && let Some((expansion, _)) = env.lookup_pattern_variable(binding)
                {
                    expansions.insert(binding, expansion);
                    Ok(Self::Variable(binding))
                } else {
                    Ok(Self::Wrapped(Syntax::Identifier {
                        ident: ident.clone(),
                        span: span.clone(),
                    }))
                }
            }
            wrapped => Ok(Self::Wrapped(wrapped.clone())),
        }
    }

    pub(crate) fn compile_escaped(
        expr: &Syntax,
        env: &Environment,
        expansions: &mut HashMap<Binding, Local>,
    ) -> Result<Self, Exception> {
        match expr {
            Syntax::List { list, span, .. } => Ok(Self::new_list(
                Self::compile_slice_escaped(list, env, expansions)?,
                span.clone(),
            )),
            Syntax::Vector { vector, span, .. } => Ok(Self::new_vector(
                Self::compile_slice_escaped(vector, env, expansions)?,
                span.clone(),
            )),
            Syntax::Identifier { ident, span, .. } => {
                if let Some(binding) = ident.resolve()
                    && let Some((expansion, _)) = env.lookup_pattern_variable(binding)
                {
                    expansions.insert(binding, expansion);
                    Ok(Self::Variable(binding))
                } else {
                    Ok(Self::Wrapped(Syntax::Identifier {
                        ident: ident.clone(),
                        span: span.clone(),
                    }))
                }
            }
            wrapped => Ok(Self::Wrapped(wrapped.clone())),
        }
    }

    fn compile_slice(
        mut expr: &[Syntax],
        env: &Environment,
        expansions: &mut HashMap<Binding, Local>,
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
                    let mut compiled =
                        Self::Ellipsis(Box::new(Self::compile_inner(template, env, expansions)?));
                    let mut tail = tail;
                    while matches!(tail.first(), Some(Syntax::Identifier { ident, ..}) if ident.sym == "...")
                    {
                        compiled = Self::Ellipsis(Box::new(compiled));
                        tail = &tail[1..];
                    }
                    output.push(compiled);
                    expr = tail;
                }
                [head, tail @ ..] => {
                    output.push(Self::compile_inner(head, env, expansions)?);
                    expr = tail;
                }
            }
        }
        Ok(output)
    }

    fn compile_slice_escaped(
        exprs: &[Syntax],
        env: &Environment,
        expansions: &mut HashMap<Binding, Local>,
    ) -> Result<Vec<Self>, Exception> {
        exprs
            .iter()
            .map(|expr| Self::compile_escaped(expr, env, expansions))
            .collect()
    }

    fn expand(&self, binds: &Binds<'_>, curr_span: Span) -> Option<Value> {
        match self {
            Self::List(list) => expand_list(list, binds, curr_span.clone()),
            Self::Vector(vec) => expand_vec(vec, binds, curr_span.clone()),
            Self::Variable(binding) => Some(Syntax::unwrap(binds.get_bind(*binding)?)),
            Self::Ellipsis(_) => unreachable!(),
            Self::Wrapped(Syntax::Wrapped { value, .. }) if value.is_null() => Some(Value::null()),
            Self::Wrapped(wrapped) => Some(Value::from(wrapped.clone())),
        }
    }

    fn expand_nested(&self, binds: &Binds<'_>, curr_span: Span) -> Option<Vec<Value>> {
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

fn expand_list(items: &[Template], binds: &Binds<'_>, curr_span: Span) -> Option<Value> {
    let mut expanded = Vec::new();
    for item in items {
        expanded.extend(item.expand_nested(binds, curr_span.clone())?);
    }
    let Some(mut output) = expanded.pop() else {
        return Some(Value::null());
    };
    for expanded in expanded.into_iter().rev() {
        output = Value::from((expanded, output));
    }
    Some(output)
}

fn expand_vec(items: &[Template], binds: &Binds<'_>, curr_span: Span) -> Option<Value> {
    let mut output = Vec::new();
    for item in items {
        output.extend(item.expand_nested(binds, curr_span.clone())?);
    }
    Some(Value::from(output))
}

fn check_ellipsis(expr: &Syntax, env: &Environment) -> Result<(), Exception> {
    let binds = match expr {
        Syntax::List { list, .. } => {
            if let [
                Syntax::Identifier {
                    ident: ellipsis, ..
                },
                template,
                _,
            ] = list.as_slice()
                && ellipsis.sym == "..."
            {
                check_escaped_template(template, env)
            } else {
                check_subtemplate(list, env)?
            }
        }
        Syntax::Vector { vector, .. } => check_subtemplate(vector, env)?,
        Syntax::Identifier { ident, .. } => {
            if let Some(binding) = ident.resolve()
                && let Some((_, depth)) = env.lookup_pattern_variable(binding)
                && depth != 0
            {
                return Err(error::too_few_ellipsis(ident.sym, expr));
            } else {
                return Ok(());
            }
        }
        _ => return Ok(()),
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
        Syntax::List { list, .. } => {
            if let [
                Syntax::Identifier {
                    ident: ellipsis, ..
                },
                template,
                _,
            ] = list.as_slice()
                && ellipsis.sym == "..."
            {
                Ok(check_escaped_template(template, env))
            } else {
                check_subtemplate(list, env)
            }
        }
        Syntax::Vector { vector, .. } => check_subtemplate(vector, env),
        Syntax::Identifier { ident, .. } => {
            // TODO: Probably should cache these resolves at some point...
            if let Some(binding) = ident.resolve()
                && let Some((_, depth)) = env.lookup_pattern_variable(binding)
            {
                Ok(vec![(ident.sym, depth as isize)])
            } else {
                Ok(Vec::new())
            }
        }
        _ => Ok(Vec::new()),
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
            let mut tail = tail;
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
            if let Some(binding) = ident.resolve()
                && let Some((_, depth)) = env.lookup_pattern_variable(binding)
            {
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

    Value::into_raw(expanded)
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

    fn get_bind(&self, binding: Binding) -> Option<Syntax> {
        if let bind @ Some(_) = self.curr_expansion_level.binds.get(&binding) {
            bind.cloned()
        } else if let Some(up) = self.parent_expansion_level {
            up.get_bind(binding)
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
