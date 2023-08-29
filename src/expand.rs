use crate::{
    ast::Literal,
    eval::{Env, Eval, Value},
    gc::Gc,
    sexpr::{Ident, SExpr},
};
use futures::future::BoxFuture;
use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
};

pub struct Transformer {
    env: Gc<Env>,
    rules: Vec<SyntaxRule>,
}

impl Transformer {
    pub fn expand(&self, expr: &SExpr, binds: &Binds<'_>) -> Option<SExpr> {
        for rule in &self.rules {
            if let Some(expansion) = rule.expand(expr, binds, &self.env) {
                return Some(expansion);
            }
        }
        None
    }
}

struct SyntaxRule {
    pattern: Pattern,
    template: Template,
}

impl SyntaxRule {
    fn expand(&self, expr: &SExpr, binds: &Binds<'_>, env: &Gc<Env>) -> Option<SExpr> {
        let mut var_binds = HashMap::new();
        self.pattern
            .matches(expr, binds, &mut var_binds)
            .then(|| self.template.execute(&env, &var_binds))
    }
}

enum Pattern {
    Nil,
    Underscore,
    Ellipsis(String),
    List(Vec<Pattern>),
    Vector(Vec<Pattern>),
    Variable(String),
    Identifier(String),
    Literal(Literal),
}

enum SExprOrMany {
    SExpr(SExpr),
    Many(Vec<SExpr>),
}

impl Pattern {
    fn matches(
        &self,
        expr: &SExpr,
        binds: &Binds<'_>,
        var_binds: &mut HashMap<String, SExprOrMany>,
    ) -> bool {
        match self {
            Self::Underscore => true,
            Self::Variable(ref name) => {
                var_binds.insert(name.clone(), SExprOrMany::SExpr(expr.clone()));
                true
            }
            Self::Identifier(ref lhs) => match expr {
                SExpr::Identifier(rhs) if lhs == &rhs.sym && !binds.is_bound(&rhs.sym) => true,
                _ => false,
            },
            Self::List(list) => match_slices(&list, expr, binds, var_binds),
            Self::Vector(vec) => match_slices(&vec, expr, binds, var_binds),
            // We shouldn't ever see this outside of lists
            Self::Nil => matches!(expr, SExpr::Nil),
            _ => todo!(),
        }
    }
}

fn match_slices(
    pattern: &[Pattern],
    expr: &SExpr,
    binds: &Binds<'_>,
    var_binds: &mut HashMap<String, SExprOrMany>,
) -> bool {
    let mut expr_iter = match expr {
        SExpr::List(list) => list.iter().peekable(),
        _ => return false,
    };
    let mut pattern_iter = pattern.iter().peekable();
    while let Some(item) = pattern_iter.next() {
        if let Pattern::Ellipsis(ref name) = item {
            let exprs = if matches!(pattern_iter.peek(), Some(Pattern::Nil)) {
                // Match backwards
                let mut rev_expr_iter = expr_iter.rev();
                let mut rev_pattern_iter = pattern_iter.rev();
                while let Some(pattern) = rev_pattern_iter.next() {
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

enum Template {
    Nil,
    Ellipsis(String),
    List(Vec<Template>),
    Vector(Vec<Template>),
    Identifier(String),
    Literal(Literal),
}

impl Template {
    fn execute(&self, macro_env: &Gc<Env>, var_binds: &HashMap<String, SExprOrMany>) -> SExpr {
        match self {
            Self::Nil => SExpr::Nil,
            Self::List(list) => SExpr::List(execute_slice(list, macro_env, var_binds)),
            Self::Vector(vec) => SExpr::Vector(execute_slice(vec, macro_env, var_binds)),
            Self::Identifier(ident) => SExpr::Identifier(Ident::new_macro(ident, macro_env)),
            Self::Literal(literal) => SExpr::Literal(literal.clone()),
            _ => unreachable!(),
        }
    }
}

fn execute_slice(
    items: &[Template],
    macro_env: &Gc<Env>,
    var_binds: &HashMap<String, SExprOrMany>,
) -> Vec<SExpr> {
    let mut output = Vec::new();
    for item in items {
        match item {
            Template::Ellipsis(name) => match var_binds.get(name).unwrap() {
                SExprOrMany::SExpr(expr) => output.push(expr.clone()),
                SExprOrMany::Many(exprs) => output.extend(exprs.clone()),
            },
            _ => output.push(item.execute(macro_env, var_binds)),
        }
    }
    output
}

pub struct Binds<'a> {
    up: Option<&'a Binds<'a>>,
    binds: HashSet<String>,
}

impl Binds<'static> {
    pub async fn new(env: &Gc<Env>) -> Self {
        Self {
            up: None,
            binds: todo!("Need to fetch binds from current environment"),
        }
    }
}

impl Binds<'_> {
    fn is_bound(&self, name: &str) -> bool {
        todo!()
    }
}
