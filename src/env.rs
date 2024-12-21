use proc_macros::Trace;
use std::collections::HashMap;

use crate::{
    builtin::Builtin,
    compile::CompileError,
    error::RuntimeError,
    gc::{init_gc, Gc},
    lex::{LexError, Token},
    parse::ParseError,
    syntax::{Identifier, Mark, ParsedSyntax},
    value::Value,
};

#[derive(derive_more::Debug, Clone, Trace)]
pub struct LexicalContour {
    #[debug(skip)]
    pub up: Env,
    #[debug("{:?}", vars.keys().cloned().collect::<Vec<_>>())]
    vars: HashMap<Identifier, Gc<Value>>,
    #[debug("{:?}", macros.keys().cloned().collect::<Vec<_>>())]
    macros: HashMap<Identifier, Gc<Value>>,
}

impl LexicalContour {
    pub fn is_bound(&self, ident: &Identifier) -> bool {
        self.vars.contains_key(ident) || self.macros.contains_key(ident) || self.up.is_bound(ident)
    }

    fn fetch_var(&self, ident: &Identifier) -> Option<Gc<Value>> {
        if let Some(var) = self.vars.get(ident) {
            return Some(var.clone());
        }
        // Macros are also variables
        if let Some(var) = self.macros.get(ident) {
            return Some(var.clone());
        }
        // Check the next lexical scope up
        self.up.fetch_var(ident)
    }

    fn fetch_macro(&self, ident: &Identifier) -> Option<MacroLookup> {
        // Only check the macro definitions
        if let Some(var) = self.macros.get(ident) {
            return Some(MacroLookup::WithoutEnv(var.clone()));
        }
        self.up.fetch_macro(ident).map(MacroLookup::WithEnv)
    }

    pub fn def_var(&mut self, ident: &Identifier, value: Gc<Value>) {
        // If the identifier is defined as a macro, remove it.
        if self.macros.contains_key(ident) {
            self.macros.remove(ident);
        }
        self.vars.insert(ident.clone(), value);
    }

    pub fn def_macro(&mut self, ident: &Identifier, value: Gc<Value>) {
        // If the identifier is defined as a variable, remove it
        if self.vars.contains_key(ident) {
            self.vars.remove(ident);
        }
        self.macros.insert(ident.clone(), value);
    }
}

#[derive(derive_more::Debug, Clone, Trace)]
pub struct ExpansionContext {
    #[debug(skip)]
    up: Env,
    mark: Mark,
    #[debug(skip)]
    macro_env: Env,
}

impl ExpansionContext {
    pub fn is_bound(&self, ident: &Identifier) -> bool {
        if ident.marks.contains(&self.mark) {
            let mut stripped = ident.clone();
            stripped.mark(self.mark);
            self.macro_env.is_bound(&stripped)
        } else {
            self.up.is_bound(ident)
        }
    }

    pub fn fetch_var(&self, ident: &Identifier) -> Option<Gc<Value>> {
        // If the ident contains this mark, it comes from the macro and
        // we must fetch from the macro's environment.
        if ident.marks.contains(&self.mark) {
            let mut stripped = ident.clone();
            stripped.mark(self.mark);
            self.macro_env.fetch_var(&stripped)
        } else {
            self.up.fetch_var(ident)
        }
    }

    pub fn fetch_macro(&self, ident: &Identifier) -> Option<(Env, Gc<Value>)> {
        if ident.marks.contains(&self.mark) {
            let mut stripped = ident.clone();
            stripped.mark(self.mark);
            self.macro_env.fetch_macro(&stripped)
        } else {
            self.up.fetch_macro(ident)
        }
    }
}

#[derive(Trace, Debug)]
pub enum Env {
    /// This is the top level environment
    Top,
    /// This is an expansion context
    Expansion(Gc<ExpansionContext>),
    /// This is a lexical contour
    LexicalContour(Gc<LexicalContour>),
}

impl Env {
    pub fn is_bound(&self, ident: &Identifier) -> bool {
        match self {
            Self::Top => false,
            Self::Expansion(expansion) => expansion.read().is_bound(ident),
            Self::LexicalContour(env) => env.read().is_bound(ident),
        }
    }

    pub fn fetch_var(&self, ident: &Identifier) -> Option<Gc<Value>> {
        match self {
            Self::Top => None,
            Self::Expansion(expansion) => expansion.read().fetch_var(ident),
            Self::LexicalContour(env) => env.read().fetch_var(ident),
        }
    }

    pub fn fetch_macro(&self, ident: &Identifier) -> Option<(Env, Gc<Value>)> {
        match self {
            Self::Top => None,
            Self::Expansion(expansion) => expansion.read().fetch_macro(ident),
            Self::LexicalContour(env) => match env.read().fetch_macro(ident) {
                Some(MacroLookup::WithEnv((env, value))) => Some((env, value)),
                Some(MacroLookup::WithoutEnv(value)) => Some((self.clone(), value)),
                _ => None,
            },
        }
    }

    pub async fn top() -> Self {
        // We should probably find another place to init_gc, but this is honestly fine
        init_gc();

        let mut top = Self::Top.new_lexical_contour();
        // Install the builtins:
        for builtin in inventory::iter::<Builtin> {
            builtin.install(&mut top);
        }
        let top = Self::LexicalContour(Gc::new(top));
        // Install the stdlib:
        let _ = top.eval(include_str!("stdlib.scm")).await.unwrap();
        top
    }

    pub fn new_lexical_contour(&self) -> LexicalContour {
        LexicalContour {
            up: self.clone(),
            // mark,
            vars: HashMap::default(),
            macros: HashMap::default(),
        }
    }

    pub fn new_expansion_context(&self, mark: Mark, macro_env: Env) -> ExpansionContext {
        ExpansionContext {
            up: self.clone(),
            mark,
            macro_env,
        }
    }

    pub fn def_var(&self, ident: &Identifier, value: Gc<Value>) {
        match self {
            Self::Top => unreachable!(),
            Self::Expansion(expansion) => expansion.read().up.def_var(ident, value),
            Self::LexicalContour(contour) => contour.write().def_var(ident, value),
        }
    }

    pub fn def_macro(&self, ident: &Identifier, value: Gc<Value>) {
        match self {
            Self::Top => unreachable!(),
            Self::Expansion(expansion) => expansion.read().up.def_macro(ident, value),
            Self::LexicalContour(contour) => contour.write().def_macro(ident, value),
        }
    }

    /// Evaluate a string, returning all of the results in a Vec
    pub async fn eval<'e>(&self, exprs: &'e str) -> Result<Vec<Vec<Gc<Value>>>, EvalError<'e>> {
        let tokens = Token::tokenize_str(exprs)?;
        let sexprs = ParsedSyntax::parse(&tokens)?;
        let mut results = Vec::new();
        for sexpr in sexprs {
            let result = sexpr.compile(self, &None).await?.eval(self, &None).await?;
            results.push(result);
        }
        Ok(results)
    }
}

impl Clone for Env {
    fn clone(&self) -> Self {
        match self {
            Self::Top => Self::Top,
            Self::Expansion(expansion) => Self::Expansion(Gc::new(expansion.read().clone())),
            Self::LexicalContour(env) => Self::LexicalContour(Gc::new(env.read().clone())),
        }
    }
}

impl From<Gc<ExpansionContext>> for Env {
    fn from(env: Gc<ExpansionContext>) -> Self {
        Self::Expansion(env)
    }
}

impl From<Gc<LexicalContour>> for Env {
    fn from(env: Gc<LexicalContour>) -> Self {
        Self::LexicalContour(env)
    }
}

enum MacroLookup {
    WithEnv((Env, Gc<Value>)),
    WithoutEnv(Gc<Value>),
}

#[derive(derive_more::From, Debug)]
pub enum EvalError<'e> {
    LexError(LexError<'e>),
    ParseError(ParseError<'e>),
    CompileError(CompileError),
    RuntimeError(RuntimeError),
}
