use crate::{
    ast::{Expression, Literal},
    cps::Compile,
    env::CapturedEnv,
    exception::Exception,
    gc::{Gc, Trace},
    proc::{Application, Closure},
    syntax::{Identifier, Span, Syntax},
    value::Value,
};
use futures::future::BoxFuture;
use indexmap::IndexMap;
use std::collections::{BTreeSet, HashMap, HashSet};

#[derive(Clone, Trace, Debug)]
pub struct Transformer {
    pub rules: Vec<SyntaxRule>,
    pub is_variable_transformer: bool,
}

impl Transformer {
    pub fn expand(&self, expr: &Syntax) -> Option<Syntax> {
        for rule in &self.rules {
            if let expansion @ Some(_) = rule.expand(expr) {
                return expansion;
            }
        }
        None
    }
}

#[derive(Clone, Debug, Trace)]
pub struct SyntaxRule {
    pub pattern: Pattern,
    pub template: Template,
}

impl SyntaxRule {
    pub fn compile(keywords: &HashSet<String>, pattern: &Syntax, template: &Syntax) -> Self {
        let mut variables = HashSet::new();
        let pattern = Pattern::compile(pattern, keywords, &mut variables);
        let template = Template::compile(template, &variables);
        Self { pattern, template }
    }

    fn expand(&self, expr: &Syntax) -> Option<Syntax> {
        let mut top_expansion_level = ExpansionLevel::default();
        let curr_span = expr.span().clone();
        self.pattern
            .matches(expr, &mut top_expansion_level)
            .then(|| {
                let binds = Binds::new_top(&top_expansion_level);
                self.template.execute(&binds, curr_span).unwrap()
            })
    }
}

#[derive(Clone, Debug, Trace)]
pub enum Pattern {
    Null,
    Underscore,
    Ellipsis(Box<Pattern>),
    List(Vec<Pattern>),
    Vector(Vec<Pattern>),
    Variable(String),
    Keyword(String),
    Literal(Literal),
}

impl Pattern {
    pub fn compile(
        expr: &Syntax,
        keywords: &HashSet<String>,
        variables: &mut HashSet<String>,
    ) -> Self {
        match expr {
            Syntax::Null { .. } => Self::Null,
            Syntax::Identifier { ident, .. } if ident.name == "_" => Self::Underscore,
            Syntax::Identifier { ident, .. } if keywords.contains(&ident.name) => {
                Self::Keyword(ident.name.clone())
            }
            Syntax::Identifier { ident, .. } => {
                variables.insert(ident.name.clone());
                Self::Variable(ident.name.clone())
            }
            Syntax::List { list, .. } => Self::List(Self::compile_slice(list, keywords, variables)),
            Syntax::Vector { vector, .. } => {
                Self::Vector(Self::compile_slice(vector, keywords, variables))
            }
            Syntax::Literal { literal, .. } => Self::Literal(literal.clone()),
        }
    }

    fn compile_slice(
        mut expr: &[Syntax],
        keywords: &HashSet<String>,
        variables: &mut HashSet<String>,
    ) -> Vec<Self> {
        let mut output = Vec::new();
        loop {
            match expr {
                [] => break,
                [pattern, Syntax::Identifier {
                    ident: ellipsis, ..
                }, tail @ ..]
                    if ellipsis.name == "..." =>
                {
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
            Self::Variable(ref name) => {
                assert!(expansion_level
                    .binds
                    .insert(name.clone(), expr.clone())
                    .is_none());
                true
            }
            Self::Literal(ref lhs) => {
                if let Syntax::Literal { literal: rhs, .. } = expr {
                    lhs == rhs
                } else {
                    false
                }
            }
            Self::Keyword(ref lhs) => {
                matches!(expr, Syntax::Identifier { ident: rhs, bound: false, .. } if lhs == &rhs.name)
            }
            Self::List(list) => match_slice(list, expr, expansion_level),
            Self::Vector(vec) => match_slice(vec, expr, expansion_level),
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

fn match_slice(patterns: &[Pattern], expr: &Syntax, expansion_level: &mut ExpansionLevel) -> bool {
    assert!(!patterns.is_empty());

    let exprs = match expr {
        Syntax::List { list, .. } => list,
        Syntax::Null { .. } => return true,
        _ => return false,
    };

    let contains_ellipsis = patterns.iter().any(|p| matches!(p, Pattern::Ellipsis(_)));

    match (patterns.split_last().unwrap(), contains_ellipsis) {
        ((Pattern::Null, _), false) => {
            // Proper list, no ellipsis. Match everything in order
            for (pattern, expr) in patterns.iter().zip(exprs.iter()) {
                if !pattern.matches(expr, expansion_level) {
                    return false;
                }
            }
            true
        }
        ((cdr, head), false) => {
            // The pattern is an improper list that contains no ellipsis.
            // Math in order until the last pattern, then match that to the nth
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

#[derive(Debug, Default)]
pub struct ExpansionLevel {
    binds: HashMap<String, Syntax>,
    expansions: Vec<ExpansionLevel>,
}

#[derive(Clone, Debug, Trace)]
pub enum Template {
    Null,
    Ellipsis(Box<Template>),
    List(Vec<Template>),
    Vector(Vec<Template>),
    Identifier(Identifier),
    Variable(Identifier),
    Literal(Literal),
}

impl Template {
    pub fn compile(expr: &Syntax, variables: &HashSet<String>) -> Self {
        match expr {
            Syntax::Null { .. } => Self::Null,
            Syntax::List { list, .. } => Self::List(Self::compile_slice(list, variables)),
            Syntax::Vector { vector, .. } => Self::Vector(Self::compile_slice(vector, variables)),
            Syntax::Literal { literal, .. } => Self::Literal(literal.clone()),
            Syntax::Identifier { ident, .. } if variables.contains(&ident.name) => {
                Self::Variable(ident.clone())
            }
            Syntax::Identifier { ident, .. } => Self::Identifier(ident.clone()),
        }
    }

    fn compile_slice(mut expr: &[Syntax], variables: &HashSet<String>) -> Vec<Self> {
        let mut output = Vec::new();
        loop {
            match expr {
                [] => break,
                [template, Syntax::Identifier {
                    ident: ellipsis, ..
                }, tail @ ..]
                    if ellipsis.name == "..." =>
                {
                    output.push(Self::Ellipsis(Box::new(Template::compile(
                        template, variables,
                    ))));
                    expr = tail;
                }
                [head, tail @ ..] => {
                    output.push(Self::compile(head, variables));
                    expr = tail;
                }
            }
        }
        output
    }

    fn execute(&self, binds: &Binds<'_>, curr_span: Span) -> Option<Syntax> {
        let syn = match self {
            Self::Null => Syntax::new_null(curr_span),
            Self::List(list) => {
                let executed = execute_slice(list, binds, curr_span.clone())?;
                Syntax::new_list(executed, curr_span).normalize()
            }
            Self::Vector(vec) => {
                Syntax::new_vector(execute_slice(vec, binds, curr_span.clone())?, curr_span)
            }
            Self::Identifier(ident) => Syntax::Identifier {
                ident: ident.clone(),
                span: curr_span,
                bound: false,
            },
            Self::Variable(ident) => binds.get_bind(&ident.name)?,
            Self::Literal(literal) => Syntax::new_literal(literal.clone(), curr_span),
            _ => unreachable!(),
        };
        Some(syn)
    }
}

fn execute_slice(items: &[Template], binds: &Binds<'_>, curr_span: Span) -> Option<Vec<Syntax>> {
    let mut output = Vec::new();
    for item in items {
        match item {
            Template::Ellipsis(template) => {
                for expansion in &binds.curr_expansion_level.expansions {
                    let new_level = binds.new_level(expansion);
                    let Some(result) = template.execute(&new_level, curr_span.clone()) else {
                        break;
                    };
                    output.push(result);
                }
            }
            Template::Null => {
                if let Some(Syntax::Null { .. }) = output.last() {
                    continue;
                } else {
                    output.push(Syntax::new_null(curr_span.clone()));
                }
            }
            _ => output.push(item.execute(binds, curr_span.clone())?),
        }
    }
    Some(output)
}

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

    fn get_bind(&self, name: &str) -> Option<Syntax> {
        if let bind @ Some(_) = self.curr_expansion_level.binds.get(name) {
            bind.cloned()
        } else if let Some(up) = self.parent_expansion_level {
            up.get_bind(name)
        } else {
            None
        }
    }
}

/*
#[builtin("make-variable-transformer")]
pub async fn make_variable_transformer(
    _cont: &Option<Arc<Continuation>>,
    proc: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let proc = proc.read();
    match &*proc {
        Value::Procedure(proc) => {
            let mut proc = proc.clone();
            proc.is_variable_transformer = true;
            Ok(vec![Gc::new(Value::Procedure(proc))])
        }
        _ => todo!(),
    }
}
 */

pub fn call_transformer<'a>(
    args: &'a [Gc<Value>],
    env: &'a [Gc<Value>],
    cont: &'a Gc<Value>,
) -> BoxFuture<'a, Result<Application, Exception>> {
    Box::pin(async move {
        let [captured_env, trans, arg] = args else {
            panic!("wrong args");
        };

        // Fetch a runtime from the continuation. It doesn't really matter
        // _which_ runtime we use, in fact we could create a new one, but it
        // behooves us to use one that already exists.
        let cont = {
            let cont = cont.read();
            let cont: &Closure = cont.as_ref().try_into()?;
            cont.clone()
        };

        let expanded = {
            let trans_read = trans.read();
            let trans: &Transformer = trans_read.as_ref().try_into().unwrap();

            let arg = Syntax::from_datum(&BTreeSet::default(), arg);

            // Expand the argument:
            trans.expand(&arg).ok_or_else(Exception::syntax_error)?
        };

        let captured_env = {
            let captured_env_read = captured_env.read();
            let captured_env: &CapturedEnv = captured_env_read.as_ref().try_into().unwrap();
            captured_env.clone()
        };

        let mut collected_env = IndexMap::new();
        for (i, local) in captured_env.captured.into_iter().enumerate() {
            collected_env.insert(local, env[i].clone());
        }

        // Parse the expression in the captured environment:
        let parsed = Expression::parse(&cont.runtime, expanded, &captured_env.env)
            .await
            .unwrap();
        let cps_expr = parsed.compile_top_level();
        let compiled = cont
            .runtime
            .compile_expr_with_env(cps_expr, collected_env)
            .await
            .unwrap();
        let transformer_result = compiled.call(&[]).await?;
        let application = Application::new(cont, transformer_result);

        Ok(application)
    })
}
