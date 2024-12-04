use futures::future::BoxFuture;
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

#[derive(derive_more::Debug, Trace)]
pub struct LexicalContour {
    #[debug(skip)]
    pub up: Env,
    // pub mark: Mark,
    #[debug("{:?}", vars.keys().cloned().collect::<Vec<_>>())]
    vars: HashMap<Identifier, Gc<Value>>,
    #[debug("{:?}", macros.keys().cloned().collect::<Vec<_>>())]
    macros: HashMap<Identifier, Gc<Value>>,
}

impl LexicalContour {
    pub fn is_bound<'a>(&'a self, ident: &'a Identifier) -> BoxFuture<'a, bool> {
        Box::pin(async move {
            self.vars.contains_key(ident)
                || self.macros.contains_key(ident)
                || self.up.is_bound(ident).await
        })
    }

    fn fetch_var<'a>(&'a self, ident: &'a Identifier) -> BoxFuture<'a, Option<Gc<Value>>> {
        Box::pin(async move {
            if let Some(var) = self.vars.get(ident) {
                return Some(var.clone());
            }
            // Macros are also variables
            if let Some(var) = self.macros.get(ident) {
                return Some(var.clone());
            }
            // Check the next lexical scope up
            self.up.fetch_var(ident).await
        })
    }

    fn fetch_macro<'a>(&'a self, ident: &'a Identifier) -> BoxFuture<'a, Option<MacroLookup>> {
        Box::pin(async move {
            // Only check the macro definitions
            if let Some(var) = self.macros.get(ident) {
                return Some(MacroLookup::WithoutEnv(var.clone()));
            }
            self.up.fetch_macro(ident).await.map(MacroLookup::WithEnv)
        })
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

    pub fn deep_clone(&self) -> BoxFuture<'_, Self> {
        Box::pin(async move {
            Self {
                up: self.up.deep_clone().await,
                vars: self.vars.clone(),
                macros: self.macros.clone(),
            }
        })
    }
}

#[derive(derive_more::Debug, Trace)]
pub struct ExpansionContext {
    #[debug(skip)]
    up: Env,
    mark: Mark,
    #[debug(skip)]
    macro_env: Env,
}

impl ExpansionContext {
    pub fn is_bound<'a>(&'a self, ident: &'a Identifier) -> BoxFuture<'a, bool> {
        Box::pin(async move {
            if ident.marks.contains(&self.mark) {
                let mut stripped = ident.clone();
                stripped.mark(self.mark);
                self.macro_env.is_bound(&stripped).await
            } else {
                self.up.is_bound(ident).await
            }
        })
    }

    pub fn fetch_var<'a>(&'a self, ident: &'a Identifier) -> BoxFuture<'a, Option<Gc<Value>>> {
        Box::pin(async move {
            // If the ident contains this mark, it comes from the macro and
            // we must fetch from the macro's environment.
            if ident.marks.contains(&self.mark) {
                let mut stripped = ident.clone();
                stripped.mark(self.mark);
                self.macro_env.fetch_var(&stripped).await
            } else {
                self.up.fetch_var(ident).await
            }
        })
    }

    pub fn fetch_macro<'a>(
        &'a self,
        ident: &'a Identifier,
    ) -> BoxFuture<'a, Option<(Env, Gc<Value>)>> {
        Box::pin(async move {
            if ident.marks.contains(&self.mark) {
                let mut stripped = ident.clone();
                stripped.mark(self.mark);
                self.macro_env.fetch_macro(&stripped).await
            } else {
                self.up.fetch_macro(ident).await
            }
        })
    }

    pub fn deep_clone(&self) -> BoxFuture<'_, Self> {
        Box::pin(async move {
            Self {
                up: self.up.deep_clone().await,
                mark: self.mark,
                macro_env: self.macro_env.deep_clone().await,
            }
        })
    }
}

#[derive(Clone, Trace, derive_more::Debug)]
pub enum Env {
    /// This is the top level environment
    Top,
    /// This is an expansion context
    Expansion(#[debug(skip)] Gc<ExpansionContext>),
    /// This is a lexical contour
    LexicalContour(#[debug(skip)] Gc<LexicalContour>),
}

impl Env {
    pub async fn is_bound(&self, ident: &Identifier) -> bool {
        match self {
            Self::Top => false,
            Self::Expansion(expansion) => expansion.read().await.is_bound(ident).await,
            Self::LexicalContour(env) => env.read().await.is_bound(ident).await,
        }
    }

    pub async fn fetch_var(&self, ident: &Identifier) -> Option<Gc<Value>> {
        match self {
            Self::Top => None,
            Self::Expansion(expansion) => expansion.read().await.fetch_var(ident).await,
            Self::LexicalContour(env) => env.read().await.fetch_var(ident).await,
        }
    }

    pub async fn fetch_macro(&self, ident: &Identifier) -> Option<(Env, Gc<Value>)> {
        match self {
            Self::Top => None,
            Self::Expansion(expansion) => expansion.read().await.fetch_macro(ident).await,
            Self::LexicalContour(env) => match env.read().await.fetch_macro(ident).await {
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

    pub fn def_var<'a>(&'a self, ident: &'a Identifier, value: Gc<Value>) -> BoxFuture<'a, ()> {
        Box::pin(async move {
            match self {
                Self::Top => unreachable!(),
                Self::Expansion(expansion) => expansion.read().await.up.def_var(ident, value).await,
                Self::LexicalContour(contour) => contour.write().await.def_var(ident, value),
            }
        })
    }

    pub fn def_macro<'a>(&'a self, ident: &'a Identifier, value: Gc<Value>) -> BoxFuture<'a, ()> {
        Box::pin(async move {
            match self {
                Self::Top => unreachable!(),
                Self::Expansion(expansion) => {
                    expansion.read().await.up.def_macro(ident, value).await
                }
                Self::LexicalContour(contour) => contour.write().await.def_macro(ident, value),
            }
        })
    }

    pub async fn deep_clone(&self) -> Self {
        match self {
            Self::Top => Self::Top,
            Self::Expansion(expansion) => {
                Self::Expansion(Gc::new(expansion.read().await.deep_clone().await))
            }
            Self::LexicalContour(env) => {
                Self::LexicalContour(Gc::new(env.read().await.deep_clone().await))
            }
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
