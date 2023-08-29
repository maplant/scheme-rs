use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
};

use futures::future::BoxFuture;

use crate::{
    eval::{Env, Eval, Value},
    gc::Gc,
};

#[derive(Clone)]
struct Ident {
    macro_env: Option<Gc<Env>>,
    sym: String,
}

impl Ident {
    pub fn new_free(sym: String) -> Self {
        Self {
            macro_env: None,
            sym,
        }
    }

    pub fn new_macro(sym: &str, macro_env: &Gc<Env>) -> Self {
        Self {
            macro_env: Some(macro_env.clone()),
            sym: sym.to_string(),
        }
    }

    pub async fn lookup(&self, env: &Gc<Env>) -> Gc<Value> {
        // If macro env is set, use that. Otherwise, use env.
        todo!()
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.macro_env.as_ref().map(Gc::as_ptr).hash(state);
        self.sym.hash(state);
    }
}

impl PartialEq for Ident {
    fn eq(&self, rhs: &Ident) -> bool {
        self.macro_env.as_ref().map(Gc::as_ptr) == rhs.macro_env.as_ref().map(Gc::as_ptr)
            && self.sym == rhs.sym
    }
}

impl Eq for Ident {}

#[derive(Clone)]
enum SExpr {
    Nil,
    List(Vec<SExpr>),
    Vector(Vec<SExpr>),
    Literal(Literal),
    Identifier(Ident),
}

impl SExpr {
    fn expand<'a>(self, env: &'a Gc<Env>, binds: &'a Binds<'_>) -> BoxFuture<'a, SExpr> {
        Box::pin(async move {
            match self {
                Self::List(mut list) => {
                    let (head, tail) = list.split_first().unwrap();
                    let head = match head.clone() {
                        list @ Self::List(_) => list.expand(env, binds).await,
                        x => x,
                    };
                    if !matches!(head, Self::Identifier(_)) {
                        let mut output = vec![head];
                        for item in tail {
                            output.push(item.clone().expand(env, binds).await);
                        }
                        return SExpr::List(output);
                    };
                    let mut list = vec![head];
                    list.extend(tail.iter().cloned());
                    let mut list = SExpr::List(list);
                    // Check if current ident is a macro
                    while let Value::Transformer(transformer) = todo!() {
                        list = transformer.expand(&list, binds).unwrap();
                    }
                    // TODO: Check if lambda, define or let
                    let list: Vec<_> = todo!();
                    let mut output = Vec::<SExpr>::new();
                    let mut items = list.into_iter();
                    output.push(items.next().unwrap());
                    for item in items {
                        output.push(item.expand(env, binds).await)
                    }
                    Self::List(output)
                }
                x => x,
            }
        })
    }
}

/*
impl SExpr {
    async fn expand_list(self, tail: Vec<SExpr>, env: &Gc<Env>, binds: &Binds<'_>) -> Self {
        /*
        let head = match self {
            Self::List { head, tail } => {
                head.expand_list(tail, env);
            },
            Self::Symbol(ident) => ident,
        };
         */

        let is_transformer: bool = todo!();
        if !is_transformer {
            // Check if its a let or lambda

            let mut new_tail = Vec::new();
            for expr in tail.into_iter() {
                expr.expand(env, binds);
            }

            return SExpr::List { head: Box::new(self), tail: new_tail };
        }

        let head: Ident = todo!();
        let transformer: Transformer = todo!();

        let new = transformer.expand(&head, &tail, binds).unwrap();

        todo!()
    }

    async fn expand(self, gc: &Gc<Env>, binds: &Binds<'_>) -> Self {
        todo!()
    }

    async fn compile(self, env: &Gc<Env>) -> Box<dyn Eval> {
        // Expand self
        // if let, bind
        todo!()
    }
}
*/

#[derive(Clone)]
struct Literal;

pub struct Transformer {
    env: Gc<Env>,
    rules: Vec<SyntaxRule>,
}

impl Transformer {
    fn expand(&self, expr: &SExpr, binds: &Binds<'_>) -> Option<SExpr> {
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

struct Binds<'a> {
    up: Option<&'a Binds<'a>>,
    binds: HashSet<String>,
}

impl Binds<'_> {
    fn is_bound(&self, name: &str) -> bool {
        todo!()
    }
}
