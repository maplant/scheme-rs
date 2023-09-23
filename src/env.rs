use futures::future::BoxFuture;
use std::collections::HashMap;

use crate::{
    builtin::Builtin,
    eval::Value,
    gc::{Gc, Trace},
    syntax::{Identifier, Mark},
};

pub struct LexicalContour {
    pub up: Env,
    vars: HashMap<Identifier, Gc<Value>>,
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

    pub fn fetch_var<'a>(&'a self, ident: &'a Identifier) -> BoxFuture<'a, Option<Gc<Value>>> {
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

    pub fn fetch_macro<'a>(&'a self, ident: &'a Identifier) -> BoxFuture<'a, Option<Gc<Value>>> {
        Box::pin(async move {
            // Only check the macro definitions
            if let Some(var) = self.macros.get(ident) {
                return Some(var.clone());
            }
            self.up.fetch_macro(ident).await
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
}

impl Trace for LexicalContour {
    fn unroot(&self) {
        self.up.unroot();
        self.vars.unroot();
        self.macros.unroot();
    }
}

pub struct ExpansionContext {
    up: Env,
    mark: Mark,
    macro_env: Env,
}

impl ExpansionContext {
    pub fn is_bound<'a>(&'a self, ident: &'a Identifier) -> BoxFuture<'a, bool> {
        Box::pin(async move {
            if ident.marks.contains(&self.mark) {
                self.macro_env.is_bound(ident).await
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
                self.macro_env.fetch_var(ident).await
            } else {
                self.up.fetch_var(ident).await
            }
        })
    }

    pub fn fetch_macro<'a>(&'a self, ident: &'a Identifier) -> BoxFuture<'a, Option<Gc<Value>>> {
        Box::pin(async move {
            if ident.marks.contains(&self.mark) {
                self.macro_env.fetch_macro(ident).await
            } else {
                self.up.fetch_macro(ident).await
            }
        })
    }
}

impl Trace for ExpansionContext {
    fn unroot(&self) {
        self.up.unroot();
        self.macro_env.unroot();
    }
}

#[derive(Clone)]
pub enum Env {
    /// This is the top level environment
    Top,
    /// This is an expansion context
    Expansion(Gc<ExpansionContext>),
    /// This is a lexical contour
    LexicalContour(Gc<LexicalContour>),
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

    pub async fn fetch_macro(&self, ident: &Identifier) -> Option<Gc<Value>> {
        match self {
            Self::Top => None,
            Self::Expansion(expansion) => expansion.read().await.fetch_macro(ident).await,
            Self::LexicalContour(env) => env.read().await.fetch_macro(ident).await,
        }
    }

    pub fn top() -> Self {
        let mut top = Self::Top.new_lexical_contour();
        for builtin in inventory::iter::<Builtin> {
            builtin.install(&mut top);
        }
        Self::LexicalContour(Gc::new(top))
    }

    pub fn new_lexical_contour(&self) -> LexicalContour {
        LexicalContour {
            up: self.clone(),
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

    fn unroot(&self) {
        match self {
            Self::Expansion(expansion) => expansion.unroot(),
            Self::LexicalContour(contour) => contour.unroot(),
            _ => (),
        }
    }
}

impl From<Gc<LexicalContour>> for Env {
    fn from(env: Gc<LexicalContour>) -> Self {
        Self::LexicalContour(env)
    }
}

pub enum VarOrMacro {
    Var(Gc<Value>),
    Macro(Gc<Value>),
}
