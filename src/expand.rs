use futures::future::BoxFuture;

use crate::{
    ast::Literal,
    env::Env,
    gc::Gc,
    syntax::{Identifier, Mark, Span, Syntax},
};
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Clone)]
pub struct Transformer {
    pub macro_env: Env,
    pub rules: Vec<SyntaxRule>,
}

impl Transformer {
    pub async fn expand(&self, curr_mark: Mark, expr: &Syntax, env: &Env) -> Option<Syntax> {
        for rule in &self.rules {
            if let Some(expansion) = rule.expand(curr_mark, expr, env).await {
                return Some(expansion);
            }
        }
        None
    }
}

#[derive(Clone)]
pub struct SyntaxRule {
    pub pattern: Pattern,
    pub template: Template,
}

impl SyntaxRule {
    async fn expand(&self, curr_mark: Mark, expr: &Syntax, env: &Env) -> Option<Syntax> {
        let mut var_binds = HashMap::new();
        let curr_span = expr.span().clone();
        self.pattern
            .matches(expr, env, &mut var_binds)
            .await
            .then(|| self.template.execute(curr_mark, &var_binds, curr_span))
    }
}

#[derive(Clone)]
pub enum Pattern {
    Nil,
    Underscore,
    Ellipsis(String),
    List(Vec<Pattern>),
    Vector(Vec<Pattern>),
    Variable(String),
    Keyword(String),
    Literal(Literal),
}

#[derive(Debug)]
enum SyntaxOrMany {
    Syntax(Syntax),
    Many(Vec<Syntax>),
}

impl Pattern {
    pub fn compile(expr: &Syntax, keywords: &HashSet<String>) -> Self {
        match expr {
            Syntax::Nil { .. } => Self::Nil,
            Syntax::Identifier { ident, .. } if ident.name == "_" => Self::Underscore,
            Syntax::Identifier { ident, .. } if keywords.contains(&ident.name) => {
                Self::Keyword(ident.name.clone())
            }
            Syntax::Identifier { ident, .. } => Self::Variable(ident.name.clone()),
            Syntax::List { list, .. } => Self::List(Self::compile_slice(list, keywords)),
            Syntax::Vector { vector, .. } => Self::Vector(Self::compile_slice(vector, keywords)),
            Syntax::Literal { literal, .. } => Self::Literal(literal.clone()),
        }
    }

    fn compile_slice(mut expr: &[Syntax], keywords: &HashSet<String>) -> Vec<Self> {
        let mut output = Vec::new();
        loop {
            match expr {
                [] => break,
                [Syntax::Identifier { ident: var, .. }, Syntax::Identifier {
                    ident: ellipsis, ..
                }, tail @ ..]
                    if ellipsis.name == "..." =>
                {
                    output.push(Self::Ellipsis(var.name.clone()));
                    expr = tail;
                }
                [head, tail @ ..] => {
                    output.push(Self::compile(head, keywords));
                    expr = tail;
                }
            }
        }
        output
    }

    fn matches<'a>(
        &'a self,
        expr: &'a Syntax,
        env: &'a Env,
        var_binds: &'a mut HashMap<String, SyntaxOrMany>,
    ) -> BoxFuture<'a, bool> {
        Box::pin(async move {
            match self {
                Self::Underscore => !expr.is_nil(),
                Self::Variable(ref name) => {
                    var_binds.insert(name.clone(), SyntaxOrMany::Syntax(expr.clone()));
                    true
                }
                Self::Keyword(ref lhs) => {
                    matches!(expr, Syntax::Identifier { ident: rhs, .. } if lhs == &rhs.name && !env.is_bound(&rhs).await)
                }
                Self::List(list) => match_slice(list, expr, env, var_binds).await,
                Self::Vector(vec) => match_slice(vec, expr, env, var_binds).await,
                // We shouldn't ever see this outside of lists
                Self::Nil => expr.is_nil(),
                _ => todo!(),
            }
        })
    }
}

async fn match_slice(
    pattern: &[Pattern],
    expr: &Syntax,
    env: &Env,
    var_binds: &mut HashMap<String, SyntaxOrMany>,
) -> bool {
    let mut expr_iter = match expr {
        Syntax::List { list, .. } => list.iter().peekable(),
        _ => return false,
    };
    let mut pattern_iter = pattern.iter().peekable();
    while let Some(item) = pattern_iter.next() {
        if let Pattern::Ellipsis(ref name) = item {
            let exprs = if !matches!(pattern_iter.peek(), Some(Pattern::Nil)) {
                // Match backwards
                let mut rev_expr_iter = expr_iter.rev();
                let rev_pattern_iter = pattern_iter.rev();
                for pattern in rev_pattern_iter {
                    if let Some(expr) = rev_expr_iter.next() {
                        if !pattern.matches(expr, env, var_binds).await {
                            return false;
                        }
                    }
                }
                rev_expr_iter.rev().cloned().collect()
            } else {
                expr_iter.cloned().collect()
            };
            // Gobble up the rest
            var_binds.insert(name.clone(), SyntaxOrMany::Many(exprs));
            return true;
        } else if let Some(next_expr) = expr_iter.next() {
            if !item.matches(next_expr, env, var_binds).await {
                return false;
            }
        } else {
            return false;
        }
    }

    expr_iter.peek().is_none()
}

#[derive(Clone, Debug)]
pub enum Template {
    Nil,
    Ellipsis(String),
    List(Vec<Template>),
    Vector(Vec<Template>),
    Identifier(Identifier),
    Literal(Literal),
}

impl Template {
    pub fn compile(expr: &Syntax) -> Self {
        match expr {
            Syntax::Nil { .. } => Self::Nil,
            Syntax::List { list, .. } => Self::List(Self::compile_slice(list)),
            Syntax::Vector { vector, .. } => Self::Vector(Self::compile_slice(vector)),
            Syntax::Literal { literal, .. } => Self::Literal(literal.clone()),
            Syntax::Identifier { ident, .. } => Self::Identifier(ident.clone()),
        }
    }

    fn compile_slice(mut expr: &[Syntax]) -> Vec<Self> {
        let mut output = Vec::new();
        loop {
            match expr {
                [] => break,
                [Syntax::Identifier { ident: var, .. }, Syntax::Identifier {
                    ident: ellipsis, ..
                }, tail @ ..]
                    if ellipsis.name == "..." =>
                {
                    output.push(Self::Ellipsis(var.name.clone()));
                    expr = tail;
                }
                [head, tail @ ..] => {
                    output.push(Self::compile(head));
                    expr = tail;
                }
            }
        }
        output
    }

    fn execute(
        &self,
        curr_mark: Mark,
        var_binds: &HashMap<String, SyntaxOrMany>,
        curr_span: Span,
    ) -> Syntax {
        match self {
            Self::Nil => Syntax::new_nil(curr_span),
            Self::List(list) => Syntax::new_list(
                execute_slice(list, curr_mark, var_binds, curr_span.clone()),
                curr_span,
            ),
            Self::Vector(vec) => Syntax::new_vector(
                execute_slice(vec, curr_mark, var_binds, curr_span.clone()),
                curr_span,
            ),
            Self::Identifier(ident) => match var_binds.get(&ident.name) {
                // Syntax produced by the macro is marked. This will distinguish identifiers
                // that are produced by the macro and those that come from input, allowing
                // us to properly bind both.
                None => {
                    let mut syntax = Syntax::new_identifier(&ident.name, curr_span);
                    syntax.mark(curr_mark);
                    syntax
                }
                Some(SyntaxOrMany::Syntax(expr)) => expr.clone(),
                Some(SyntaxOrMany::Many(exprs)) => Syntax::new_list(exprs.clone(), curr_span),
            },
            Self::Literal(literal) => Syntax::new_literal(literal.clone(), curr_span),
            _ => unreachable!(),
        }
    }
}

fn execute_slice(
    items: &[Template],
    curr_mark: Mark,
    var_binds: &HashMap<String, SyntaxOrMany>,
    curr_span: Span,
) -> Vec<Syntax> {
    let mut output = Vec::new();
    for item in items {
        match item {
            Template::Ellipsis(name) => match var_binds.get(name).unwrap() {
                SyntaxOrMany::Syntax(expr) => output.push(expr.clone()),
                SyntaxOrMany::Many(exprs) => output.extend(exprs.clone()),
            },
            _ => output.push(item.execute(curr_mark, var_binds, curr_span.clone())),
        }
    }
    output
}
