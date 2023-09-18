use crate::{
    ast::{Ident, Literal},
    eval::Env,
    gc::Gc,
    syntax::{Span, Syntax},
};
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Clone)]
pub struct Transformer {
    pub env: Gc<Env>,
    pub rules: Vec<SyntaxRule>,
}

impl Transformer {
    pub fn expand(&self, expr: &Syntax, binds: Arc<Binds>) -> Option<Syntax> {
        for rule in &self.rules {
            if let Some(expansion) = rule.expand(expr, binds.clone(), &self.env) {
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
    fn expand(&self, expr: &Syntax, binds: Arc<Binds>, env: &Gc<Env>) -> Option<Syntax> {
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
enum SyntaxOrMany {
    Syntax(Syntax),
    Many(Vec<Syntax>),
}

impl Pattern {
    pub fn compile(expr: &Syntax, keywords: &HashSet<String>) -> Self {
        match expr {
            Syntax::Nil { .. } => Self::Nil,
            Syntax::Identifier { ident, .. } if ident.sym == "_" => Self::Underscore,
            Syntax::Identifier { ident, .. } if keywords.contains(&ident.sym) => {
                Self::Keyword(ident.sym.clone())
            }
            Syntax::Identifier { ident, .. } => Self::Variable(ident.sym.clone()),
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

    fn matches(
        &self,
        expr: &Syntax,
        binds: Arc<Binds>,
        var_binds: &mut HashMap<String, SyntaxOrMany>,
    ) -> bool {
        match self {
            Self::Underscore => !expr.is_nil(),
            Self::Variable(ref name) => {
                var_binds.insert(name.clone(), SyntaxOrMany::Syntax(expr.clone()));
                true
            }
            Self::Keyword(ref lhs) => {
                matches!(expr, Syntax::Identifier { ident: rhs, .. } if lhs == &rhs.sym && !binds.is_bound(&rhs.sym))
            }
            Self::List(list) => match_slice(list, expr, binds, var_binds),
            Self::Vector(vec) => match_slice(vec, expr, binds, var_binds),
            // We shouldn't ever see this outside of lists
            Self::Nil => expr.is_nil(),
            _ => todo!(),
        }
    }
}

fn match_slice(
    pattern: &[Pattern],
    expr: &Syntax,
    binds: Arc<Binds>,
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
                        if !pattern.matches(expr, binds.clone(), var_binds) {
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
            if !item.matches(next_expr, binds.clone(), var_binds) {
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
    pub fn compile(expr: &Syntax) -> Self {
        match expr {
            Syntax::Nil { .. } => Self::Nil,
            Syntax::List { list, .. } => Self::List(Self::compile_slice(list)),
            Syntax::Vector { vector, .. } => Self::Vector(Self::compile_slice(vector)),
            Syntax::Literal { literal, .. } => Self::Literal(literal.clone()),
            Syntax::Identifier { ident, .. } => Self::Identifier(ident.sym.clone()),
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

    fn execute(
        &self,
        macro_env: &Gc<Env>,
        var_binds: &HashMap<String, SyntaxOrMany>,
        curr_span: Span,
    ) -> Syntax {
        match self {
            Self::Nil => Syntax::new_nil(curr_span),
            Self::List(list) => Syntax::new_list(
                execute_slice(list, macro_env, var_binds, curr_span.clone()),
                curr_span,
            ),
            Self::Vector(vec) => Syntax::new_vector(
                execute_slice(vec, macro_env, var_binds, curr_span.clone()),
                curr_span,
            ),
            Self::Identifier(ident) => match var_binds.get(ident) {
                Some(SyntaxOrMany::Syntax(expr)) => expr.clone(),
                Some(SyntaxOrMany::Many(exprs)) => Syntax::new_list(exprs.clone(), curr_span),
                None => Syntax::new_identifier(Ident::with_hygiene(ident, macro_env), curr_span),
            },
            Self::Literal(literal) => Syntax::new_literal(literal.clone(), curr_span),
            _ => unreachable!(),
        }
    }
}

fn execute_slice(
    items: &[Template],
    macro_env: &Gc<Env>,
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
            _ => output.push(item.execute(macro_env, var_binds, curr_span.clone())),
        }
    }
    output
}

pub struct Binds {
    up: Option<Arc<Binds>>,
    binds: HashSet<String>,
}

impl Binds {
    pub async fn from_global(_env: &Gc<Env>) -> Arc<Self> {
        Arc::new(Self {
            up: None,
            binds: HashSet::default(), // todo!("Need to fetch binds from current environment"),
        })
    }

    pub fn new_local(up: &Arc<Binds>) -> Self {
        Self {
            up: Some(up.clone()),
            binds: HashSet::default(),
        }
    }
}

impl Binds {
    pub fn is_bound(&self, name: &str) -> bool {
        if self.binds.contains(name) {
            true
        } else if let Some(ref up) = self.up {
            up.is_bound(name)
        } else {
            false
        }
    }

    pub fn bind(&mut self, name: &str) {
        self.binds.insert(name.to_string());
    }
}
