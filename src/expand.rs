use crate::{
    ast::Literal,
    continuation::Continuation,
    error::RuntimeError,
    gc::Gc,
    syntax::{Identifier, Span, Syntax},
    value::Value,
};
use proc_macros::builtin;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    sync::Arc,
};

#[derive(Clone)]
pub struct Transformer {
    pub rules: Vec<SyntaxRule>,
    pub is_variable_transformer: bool,
}

impl Transformer {
    pub fn expand(&self, expr: &Syntax) -> Option<Syntax> {
        println!("expr: {expr}");
        for rule in &self.rules {
            println!("{rule:?}");
            if let Some(expansion) = rule.expand(expr) {
                return Some(expansion);
            }
        }
        None
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxRule {
    pub pattern: Pattern,
    pub template: Template,
}

impl SyntaxRule {
    fn expand(&self, expr: &Syntax) -> Option<Syntax> {
        let mut var_binds = HashMap::new();
        self.pattern.init_var_binds(&mut var_binds);
        let curr_span = expr.span().clone();
        self.pattern
            .matches(expr, &mut var_binds)
            .then(|| self.template.execute(&mut var_binds, curr_span))
            .flatten()
    }
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Nil,
    Underscore,
    Ellipsis(Box<Pattern>),
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

    fn init_var_binds(&self, var_binds: &mut HashMap<String, VecDeque<Syntax>>) {
        match self {
            Self::Ellipsis(pattern) => pattern.init_var_binds(var_binds),
            Self::List(list) | Self::Vector(list) => {
                for item in list {
                    item.init_var_binds(var_binds);
                }
            },
            Self::Variable(var) => {let _ = var_binds.insert(var.clone(), VecDeque::default());},
            _ => (),
        }
    }

    fn compile_slice(mut expr: &[Syntax], keywords: &HashSet<String>) -> Vec<Self> {
        let mut output = Vec::new();
        loop {
            match expr {
                [] => break,
                [pattern, Syntax::Identifier {
                    ident: ellipsis, ..
                }, tail @ ..]
                    if ellipsis.name == "..." =>
                {
                    output.push(Self::Ellipsis(Box::new(Pattern::compile(pattern, keywords))));
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

    fn matches(&self, expr: &Syntax, var_binds: &mut HashMap<String, VecDeque<Syntax>>) -> bool {
        println!("matching: {expr}");
        println!("pattern: {self:?}");
        match self {
            Self::Underscore => !expr.is_nil(),
            Self::Variable(ref name) => {
                var_binds.entry(name.clone()).or_default().push_back(expr.clone());
                true
            }
            Self::Keyword(ref lhs) => {
                matches!(expr, Syntax::Identifier { ident: rhs, bound: false, .. } if lhs == &rhs.name)
            }
            Self::List(list) => match_slice(list, expr, var_binds),
            Self::Vector(vec) => match_slice(vec, expr, var_binds),
            // We shouldn't ever see this outside of lists
            Self::Nil => expr.is_nil(),
            _ => todo!(),
        }
    }
}

fn match_slice(
    pattern: &[Pattern],
    expr: &Syntax,
    var_binds: &mut HashMap<String, VecDeque<Syntax>>,
) -> bool {
    let mut expr_iter = match expr {
        Syntax::List { list, .. } => list.iter().peekable(),
        // Syntax::Nil { .. } => return true,
        _ => return false,
    };
    let mut pattern_iter = pattern.iter().peekable();
    while let Some(item) = pattern_iter.next() {
        if let Pattern::Ellipsis(ref pattern) = item {
            let exprs: Vec<_> = if !matches!(pattern_iter.peek(), Some(Pattern::Nil)) {
                // Match backwards
                let mut rev_expr_iter = expr_iter.rev();
                let rev_pattern_iter = pattern_iter.rev();
                for pattern in rev_pattern_iter {
                    if let Some(expr) = rev_expr_iter.next() {
                        if !pattern.matches(expr, var_binds) {
                            return false;
                        }
                    }
                }
                rev_expr_iter.rev().cloned().collect()
            } else {
                expr_iter.cloned().collect()
            };
            // Gobble up the rest
            for expr in exprs {
                if !expr.is_nil() && !pattern.matches(&expr, var_binds) {
                    // println!("maybe here? {pattern:?}, {expr:?}");
                    return false;
                }
            }
            return true;
        } else if let Some(next_expr) = expr_iter.next() {
            if !item.matches(next_expr, var_binds) {
                // println!("{item:?}]expr: {next_expr:?}");
                return false;
            }
        } else {
            // println!("here???");
            return false;
        }
    }

    expr_iter.peek().is_none()
}

#[derive(Clone, Debug)]
pub enum Template {
    Nil,
    Ellipsis(Box<Template>),
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
                [template, Syntax::Identifier {
                    ident: ellipsis, ..
                }, tail @ ..]
                    if ellipsis.name == "..." =>
                {
                    output.push(Self::Ellipsis(Box::new(Template::compile(template))));
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

    fn execute(&self, var_binds: &mut HashMap<String, VecDeque<Syntax>>, curr_span: Span) -> Option<Syntax> {
        let syntax = match self {
            Self::Nil => Syntax::new_nil(curr_span),
            Self::List(list) => {
                let executed = execute_slice(list, var_binds, curr_span.clone())?;
                if executed.len() == 1 {
                    Syntax::new_nil(curr_span)
                } else {
                    Syntax::new_list(executed, curr_span)
                }
            }
            Self::Vector(vec) => {
                Syntax::new_vector(execute_slice(vec, var_binds, curr_span.clone())?, curr_span)
            }
            Self::Identifier(ident) => match var_binds.get_mut(&ident.name) {
                None => Syntax::Identifier {
                    ident: ident.clone(),
                    span: curr_span,
                    bound: false,
                },
                Some(ref mut exprs) => exprs.pop_front()?,
            },
            Self::Literal(literal) => Syntax::new_literal(literal.clone(), curr_span),
            _ => unreachable!(),
        };
        Some(syntax)
    }
}

fn execute_slice(
    items: &[Template],
    var_binds: &mut HashMap<String, VecDeque<Syntax>>,
    curr_span: Span,
) -> Option<Vec<Syntax>> {
    let mut output = Vec::new();
    for item in items {
        match item {
            Template::Ellipsis(template) => {
                while let Some(val) = template.execute(var_binds, curr_span.clone()) {
                    // println!("Looping? {template:?}, var_binds {var_binds:?}");
                    output.push(val);
                }
                /*
                match var_binds.get(name).unwrap() {
                SyntaxOrMany::Syntax(expr) => output.push(expr.clone()),
                SyntaxOrMany::Many(exprs) => output.extend(exprs.clone()),
                */
            },
            Template::Nil => {
                if let Some(Syntax::Nil { .. }) = output.last() {
                    continue;
                } else {
                    output.push(Syntax::new_nil(curr_span.clone()));
                }
            }
            _ => 
                output.push(item.execute(var_binds, curr_span.clone())?),
        }
    }
    Some(output)
}

#[builtin("make-variable-transformer")]
pub async fn make_variable_transformer(
    _cont: &Option<Arc<Continuation>>,
    proc: &Gc<Value>,
) -> Result<Gc<Value>, RuntimeError> {
    let proc = proc.read().await;
    match &*proc {
        Value::Procedure(proc) => {
            let mut proc = proc.clone();
            proc.is_variable_transformer = true;
            Ok(Gc::new(Value::Procedure(proc)))
        }
        Value::Transformer(transformer) => {
            let mut transformer = transformer.clone();
            transformer.is_variable_transformer = true;
            Ok(Gc::new(Value::Transformer(transformer)))
        }
        _ => todo!(),
    }
}
