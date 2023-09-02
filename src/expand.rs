use crate::{
    ast::{Ident, Literal},
    eval::Env,
    gc::Gc,
    lex::Span,
    sexpr::SExpr,
};
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
pub struct Transformer {
    pub env: Gc<Env>,
    pub rules: Vec<SyntaxRule>,
}

impl Transformer {
    pub fn expand<'a>(&self, expr: &SExpr<'a>, binds: &Binds<'a>) -> Option<SExpr<'a>> {
        for rule in &self.rules {
            if let Some(expansion) = rule.expand(expr, binds, &self.env) {
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
    fn expand<'a>(&self, expr: &SExpr<'a>, binds: &Binds<'_>, env: &Gc<Env>) -> Option<SExpr<'a>> {
        let mut var_binds = HashMap::new();
        let curr_span = expr.span().clone();
        self.pattern
            .matches(expr, binds, &mut var_binds)
            .then(|| self.template.execute(env, &var_binds, curr_span))
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
enum SExprOrMany<'a> {
    SExpr(SExpr<'a>),
    Many(Vec<SExpr<'a>>),
}

impl Pattern {
    pub fn compile(expr: &SExpr, keywords: &HashSet<String>) -> Self {
        match expr {
            SExpr::Nil { .. } => Self::Nil,
            SExpr::Identifier { ident, .. } if ident.sym == "_" => Self::Underscore,
            SExpr::Identifier { ident, .. } if keywords.contains(&ident.sym) => {
                Self::Keyword(ident.sym.clone())
            }
            SExpr::Identifier { ident, .. } => Self::Variable(ident.sym.clone()),
            SExpr::List { list, .. } => Self::List(Self::compile_slice(list, keywords)),
            SExpr::Vector { vector, .. } => Self::Vector(Self::compile_slice(vector, keywords)),
            SExpr::Literal { literal, .. } => Self::Literal(literal.clone()),
        }
    }

    fn compile_slice(mut expr: &[SExpr<'_>], keywords: &HashSet<String>) -> Vec<Self> {
        let mut output = Vec::new();
        loop {
            match expr {
                [] => break,
                [SExpr::Identifier { ident: var, .. }, SExpr::Identifier {
                    ident: ellipsis, ..
                }, tail @ ..]
                    if ellipsis.sym == "..." =>
                {
                    output.push(Self::Ellipsis(var.sym.clone()));
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
        &self,
        expr: &SExpr<'a>,
        binds: &Binds<'_>,
        var_binds: &mut HashMap<String, SExprOrMany<'a>>,
    ) -> bool {
        match self {
            Self::Underscore => !expr.is_nil(),
            Self::Variable(ref name) => {
                var_binds.insert(name.clone(), SExprOrMany::SExpr(expr.clone()));
                true
            }
            Self::Keyword(ref lhs) => {
                matches!(expr, SExpr::Identifier { ident: rhs, .. } if lhs == &rhs.sym && !binds.is_bound(&rhs.sym))
            }
            Self::List(list) => match_slice(list, expr, binds, var_binds),
            Self::Vector(vec) => match_slice(vec, expr, binds, var_binds),
            // We shouldn't ever see this outside of lists
            Self::Nil => expr.is_nil(),
            _ => todo!(),
        }
    }
}

fn match_slice<'a>(
    pattern: &[Pattern],
    expr: &SExpr<'a>,
    binds: &Binds<'_>,
    var_binds: &mut HashMap<String, SExprOrMany<'a>>,
) -> bool {
    let mut expr_iter = match expr {
        SExpr::List { list, .. } => list.iter().peekable(),
        _ => return false,
    };
    let mut pattern_iter = pattern.iter().peekable();
    while let Some(item) = pattern_iter.next() {
        if let Pattern::Ellipsis(ref name) = item {
            let exprs = if matches!(pattern_iter.peek(), Some(Pattern::Nil)) {
                // Match backwards
                let mut rev_expr_iter = expr_iter.rev();
                let rev_pattern_iter = pattern_iter.rev();
                for pattern in rev_pattern_iter {
                    if let Some(expr) = rev_expr_iter.next() {
                        if !pattern.matches(expr, binds, var_binds) {
                            return false;
                        }
                    }
                }
                rev_expr_iter.rev().cloned().collect()
            } else {
                expr_iter.cloned().collect()
            };
            // Gobble up the rest
            var_binds.insert(name.clone(), SExprOrMany::Many(exprs));
            return true;
        } else if let Some(next_expr) = expr_iter.next() {
            if !item.matches(next_expr, binds, var_binds) {
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
    Identifier(String),
    Literal(Literal),
}

impl Template {
    pub fn compile(expr: &SExpr) -> Self {
        match expr {
            SExpr::Nil { .. } => Self::Nil,
            SExpr::List { list, .. } => Self::List(Self::compile_slice(list)),
            SExpr::Vector { vector, .. } => Self::Vector(Self::compile_slice(vector)),
            SExpr::Literal { literal, .. } => Self::Literal(literal.clone()),
            SExpr::Identifier { ident, .. } => Self::Identifier(ident.sym.clone()),
        }
    }

    fn compile_slice(mut expr: &[SExpr<'_>]) -> Vec<Self> {
        let mut output = Vec::new();
        loop {
            match expr {
                [] => break,
                [SExpr::Identifier { ident: var, .. }, SExpr::Identifier {
                    ident: ellipsis, ..
                }, tail @ ..]
                    if ellipsis.sym == "..." =>
                {
                    output.push(Self::Ellipsis(var.sym.clone()));
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

    fn execute<'a>(
        &self,
        macro_env: &Gc<Env>,
        var_binds: &HashMap<String, SExprOrMany<'a>>,
        curr_span: Span<'a>,
    ) -> SExpr<'a> {
        match self {
            Self::Nil => SExpr::new_nil(curr_span),
            Self::List(list) => SExpr::new_list(
                execute_slice(list, macro_env, var_binds, curr_span.clone()),
                curr_span,
            ),
            Self::Vector(vec) => SExpr::new_vector(
                execute_slice(vec, macro_env, var_binds, curr_span.clone()),
                curr_span,
            ),
            Self::Identifier(ident) => match var_binds.get(ident) {
                Some(SExprOrMany::SExpr(expr)) => expr.clone(),
                Some(SExprOrMany::Many(exprs)) => SExpr::new_list(exprs.clone(), curr_span),
                None => SExpr::new_identifier(Ident::new_macro(ident, macro_env), curr_span),
            },
            Self::Literal(literal) => SExpr::new_literal(literal.clone(), curr_span),
            _ => unreachable!(),
        }
    }
}

fn execute_slice<'a>(
    items: &[Template],
    macro_env: &Gc<Env>,
    var_binds: &HashMap<String, SExprOrMany<'a>>,
    curr_span: Span<'a>,
) -> Vec<SExpr<'a>> {
    let mut output = Vec::new();
    for item in items {
        match dbg!(item) {
            Template::Ellipsis(name) => match var_binds.get(name).unwrap() {
                SExprOrMany::SExpr(expr) => output.push(expr.clone()),
                SExprOrMany::Many(exprs) => output.extend(exprs.clone()),
            },
            _ => output.push(item.execute(macro_env, var_binds, curr_span.clone())),
        }
    }
    output
}

pub struct Binds<'a> {
    up: Option<&'a Binds<'a>>,
    binds: HashSet<String>,
}

impl Binds<'static> {
    pub async fn from_global(_env: &Gc<Env>) -> Self {
        Self {
            up: None,
            binds: HashSet::default(), // todo!("Need to fetch binds from current environment"),
        }
    }
}

impl<'a> Binds<'a> {
    pub fn new_local(up: &'a Binds<'a>) -> Self {
        Self {
            up: Some(up),
            binds: HashSet::default(),
        }
    }
}

impl Binds<'_> {
    pub fn is_bound(&self, name: &str) -> bool {
        if self.binds.contains(name) {
            true
        } else if let Some(up) = self.up {
            up.is_bound(name)
        } else {
            false
        }
    }

    pub fn bind(&mut self, name: &str) {
        self.binds.insert(name.to_string());
    }
}
