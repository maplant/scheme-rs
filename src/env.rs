use indexmap::IndexSet;
use proc_macros::Trace;
use std::{borrow::Cow, collections::HashMap};
use tokio::sync::OnceCell;

use crate::{
    ast::{parse::ParseAstError, AstNode},
    builtin::Builtin,
    error::RuntimeError,
    gc::{init_gc, Gc},
    lex::{LexError, Token},
    parse::ParseError,
    syntax::{ExpansionCtx, Identifier, Mark, ParsedSyntax},
    value::Value,
};

/// An Environment, or store of variables and macros with a reference to its containing
/// lexical scope.
#[derive(Debug, Default, Trace)]
pub struct Env {
    /// The containing environment of this environment, or None if this is a top level
    /// environment.
    pub up: Option<Gc<Env>>,
    /// Variables stored in this environment. Variables are referenced via their index
    /// into an array. This array is dynamic only because a top level environment
    /// (i.e. a REPL) can define new variables over the course of evaluation.
    vars: Vec<Gc<Value>>,
    /// Names of the variables, used during parsing and compute the reference of the
    /// variable.
    var_names: IndexSet<Identifier>,
    /// Macros stored in the scope. Macros can only be used in the expansion process, so
    /// there's no reason to precompute faster lookups for thme.
    macros: HashMap<Identifier, Gc<Value>>,
}

static TOP_LEVEL_ENV: OnceCell<Gc<Env>> = OnceCell::const_new();

impl Env {
    pub async fn top() -> Gc<Self> {
        TOP_LEVEL_ENV
            .get_or_init(|| async {
                let mut top = Env::default();

                // Install the builtins:
                for builtin in inventory::iter::<Builtin> {
                    builtin.install(&mut top);
                }

                // Install the stdlib:
                let top = Gc::new(top);
                let _ = top.eval(include_str!("stdlib.scm")).await.unwrap();

                top
            })
            .await
            .clone()
    }

    pub fn new_lexical_contour(&self) -> Self {
        todo!()
    }

    /// Compute the reference of the variable. If no such reference exists, the variable
    /// is either undefined or a global.
    pub fn fetch_var_ref(&self, ident: &Identifier) -> Option<VarRef> {
        self.var_names.get_index_of(ident).map_or_else(
            || {
                self.up
                    .as_ref()
                    .map(|up| up.read().fetch_var_ref(ident).map(VarRef::inc_depth))
                    .flatten()
            },
            |offset| Some(VarRef { depth: 0, offset }),
        )
    }

    /// Fetch the value of the variable. Will panic if no variable exists.
    pub fn fetch_var(&self, var_ref: VarRef) -> Gc<Value> {
        // TODO: Convert this to an iterative function.
        if var_ref.depth == 0 {
            self.vars[var_ref.offset].clone()
        } else {
            self.up
                .as_ref()
                .unwrap()
                .read()
                .fetch_var(var_ref.dec_depth())
        }
    }

    /// Define a variable local to this scope.
    pub fn def_local_var(&mut self, ident: &Identifier, val: Gc<Value>) {
        let (idx, exists) = self.var_names.insert_full(ident.clone());
        if exists {
            self.vars[idx] = val;
        } else {
            self.vars.push(val);
        }
    }

    /// Set the value of the variable. Will panic if no variable exists.
    pub fn set_var(&mut self, var_ref: VarRef, new_val: Gc<Value>) {
        // TODO: Convert this to an iterative function.
        if var_ref.depth == 0 {
            self.vars[var_ref.offset] = new_val;
        } else {
            self.up
                .as_ref()
                .unwrap()
                .write()
                .set_var(var_ref.dec_depth(), new_val)
        }
    }

    /// Fetch a macro, along with its containing environment.
    fn fetch_macro(&self, ident: &Identifier) -> Option<MacroLookup> {
        match self.macros.get(ident) {
            Some(mac) => Some(MacroLookup::WithoutEnv(mac.clone())),
            None => match self.up.as_ref().map(|up| up.read().fetch_macro(ident))?? {
                wenv @ MacroLookup::WithEnv { .. } => Some(wenv),
                MacroLookup::WithoutEnv(value) => Some(MacroLookup::WithEnv {
                    env: self.up.as_ref().unwrap().clone(),
                    value,
                }),
            },
        }
    }

    /// Fetch a macro, along with its containing environment.
    pub fn def_local_macro(&mut self, ident: &Identifier, val: Gc<Value>) {
        self.macros.insert(ident.clone(), val);
    }
}

impl Gc<Env> {
    /// Evaluate a string, returning all of the results in a Vec
    pub async fn eval<'e>(&self, exprs: &'e str) -> Result<Vec<Vec<Gc<Value>>>, EvalError<'e>> {
        let tokens = Token::tokenize_str(exprs)?;
        let sexprs = ParsedSyntax::parse(&tokens)?;
        let mut results = Vec::new();
        for sexpr in sexprs {
            let Some(result) = AstNode::from_syntax(sexpr.syntax, self, &None).await? else {
                continue;
            };
            // TODO: Catch continuation calls
            let vals = result.eval(self, &None).await?;
            results.push(vals);
        }
        Ok(results)
    }
}

#[derive(derive_more::From, Debug)]
pub enum EvalError<'e> {
    LexError(LexError<'e>),
    ParseError(ParseError<'e>),
    ParseAstError(ParseAstError),
    RuntimeError(RuntimeError),
}

/// Reference to a variable that is accessible via the current environment. Could be
/// local or non-local depending on the depth field.
#[derive(Debug, Copy, Clone, Trace)]
pub struct VarRef {
    /// Number of up environments we need to traverse in order to reach the defining
    /// scope of the variable. Variables with a depth of 0 are local.
    depth: usize,
    /// Offset into the `vars` field of the environment.
    offset: usize,
}

impl VarRef {
    fn inc_depth(self) -> Self {
        Self {
            depth: self.depth + 1,
            offset: self.offset,
        }
    }

    fn dec_depth(self) -> Self {
        Self {
            depth: self.depth - 1,
            offset: self.offset,
        }
    }

    pub fn fetch(&self, env: &Gc<Env>) -> Gc<Value> {
        env.read().fetch_var(*self)
    }
}

/// Reference to a bound variable that has been introduced as part of a macro.
#[derive(Debug, Clone, Trace)]
pub struct MacroVarRef {
    /// The environment that the macro was defined in.
    env: Gc<Env>,
    /// Reference to the variable from within that macro.
    var_ref: VarRef,
}

impl MacroVarRef {
    pub fn fetch(&self) -> Gc<Value> {
        self.env.read().fetch_var(self.var_ref)
    }
}

/// References a variable in the global scope. Essentially, any variable that is
/// undefined must be a global.
#[derive(Debug, Clone, Trace)]
pub struct GlobalRef {
    name: Identifier,
    // TODO: Add span for debug info
}

impl GlobalRef {
    pub fn new(name: Identifier) -> Self {
        Self { name }
    }

    // Throw error when variable is undefined
    pub async fn fetch(&self) -> Option<Gc<Value>> {
        todo!()
    }
}

/// A reference that can either be global, macro, or regular
#[derive(Debug, Clone, Trace)]
pub enum Ref {
    Macro(MacroVarRef),
    Global(GlobalRef),
    Regular(VarRef),
}

impl Ref {
    pub fn set(&self, env: &Gc<Env>, value: &Gc<Value>) {
        todo!()
    }

    pub async fn fetch(&self, env: &Gc<Env>) -> Result<Gc<Value>, Identifier> {
        todo!()
    }
}

/// The result of looking up a macro. If the macro is present in the local environment,
/// `WithoutEnv` is returned (this is because we do not have the Gc of the env).
enum MacroLookup {
    WithEnv { env: Gc<Env>, value: Gc<Value> },
    WithoutEnv(Gc<Value>),
}

pub struct ExpansionEnv<'a> {
    up: Option<&'a ExpansionEnv<'a>>,
    expansion_ctxs: Vec<ExpansionCtx>,
    pub lexical_contour: Gc<Env>,
}

impl ExpansionEnv<'static> {
    pub async fn top() -> Self {
        Self {
            up: None,
            expansion_ctxs: Vec::new(),
            lexical_contour: Env::top().await,
        }
    }

    pub fn from_env(env: &Gc<Env>) -> Self {
        Self {
            up: None,
            expansion_ctxs: Vec::new(),
            lexical_contour: env.clone(),
        }
    }
}

impl ExpansionEnv<'_> {
    pub fn is_bound(&self, ident: &Identifier) -> bool {
        !matches!(self.fetch_var_ref(ident), Ref::Global(_))
    }

    pub fn new_expansion_env<'a>(&'a self, ctxs: Vec<ExpansionCtx>) -> ExpansionEnv<'a> {
        todo!()
    }

    pub fn new_lexical_contour<'a>(&'a self, gc: Gc<Env>) -> ExpansionEnv<'a> {
        todo!()
    }

    pub fn fetch_var_ref(&self, ident: &Identifier) -> Ref {
        // The very first thing we do is check the current lexical contour. We do not
        // need to do anything special here; any marks introduced via a macro expansion
        // can only be used to define variables in more local lexical contours.
        //
        // After we check the lexical scope, we can check the macro expansion contexts.
        // If any of them contain the mark of an expansion context, it belongs to that
        // macro environment.
        self.lexical_contour
            .read()
            .fetch_var_ref(ident)
            .map_or_else(|| self.fetch_macro_var_ref(ident), Ref::Regular)
    }

    /// Lexical environments are separately linked, so when we know that a variable is
    /// free in the current lexical environment, we can just check the macro envs.
    fn fetch_macro_var_ref(&self, ident: &Identifier) -> Ref {
        for expansion_ctx in &self.expansion_ctxs {
            if ident.marks.contains(&expansion_ctx.mark) {
                // Strip the mark and check the macro environment
                let mut ident = ident.clone();
                ident.mark(expansion_ctx.mark);
                // Fetch the var from the macro env:
                let Some(var_ref) = expansion_ctx.env.read().fetch_var_ref(&ident) else {
                    return Ref::Global(GlobalRef::new(ident));
                };
                return Ref::Macro(MacroVarRef {
                    env: expansion_ctx.env.clone(),
                    var_ref,
                });
            }
        }

        if let Some(up) = self.up {
            up.fetch_macro_var_ref(ident)
        } else {
            Ref::Global(GlobalRef::new(ident.clone()))
        }
    }

    pub fn fetch_macro(&self, ident: &Identifier) -> Option<(Gc<Env>, Gc<Value>)> {
        // Mechanically this works exactly the same as fetch_var_ref.
        self.lexical_contour.read().fetch_macro(ident).map_or_else(
            || self.fetch_macro_macro(ident),
            |ml| match ml {
                MacroLookup::WithEnv { env, value } => Some((env, value)),
                MacroLookup::WithoutEnv(value) => Some((self.lexical_contour.clone(), value)),
            },
        )
    }

    // Terrible name, but it's the same thing going on as `fetch_macro_var_ref`
    fn fetch_macro_macro(&self, ident: &Identifier) -> Option<(Gc<Env>, Gc<Value>)> {
        for expansion_ctx in &self.expansion_ctxs {
            if ident.marks.contains(&expansion_ctx.mark) {
                // Strip the mark and check the macro environment
                let mut ident = ident.clone();
                ident.mark(expansion_ctx.mark);
                // Fetch the macro from the macro env:
                return match expansion_ctx.env.read().fetch_macro(&ident)? {
                    MacroLookup::WithEnv { env, value } => Some((env, value)),
                    MacroLookup::WithoutEnv(value) => Some((expansion_ctx.env.clone(), value)),
                };
            }
        }

        if let Some(up) = self.up {
            up.fetch_macro_macro(ident)
        } else {
            None
        }
    }
}

/*
#[derive(derive_more::Debug, Trace)]
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

    pub fn deep_clone(&self) -> Self {
        Self {
            up: self.up.deep_clone(),
            vars: self.vars.clone(),
            macros: self.macros.clone(),
        }
    }
}

#[derive(derive_more::Debug, Trace)]
pub struct ExpansionContext {
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

    pub fn deep_clone(&self) -> Self {
        Self {
            up: self.up.deep_clone(),
            mark: self.mark,
            macro_env: self.macro_env.deep_clone(),
        }
    }
}

#[derive(Clone, Trace, Debug)]
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
        /*
        let tokens = Token::tokenize_str(exprs)?;
        let sexprs = ParsedSyntax::parse(&tokens)?;
        let mut results = Vec::new();
        for sexpr in sexprs {
            let result = sexpr.compile(self, &None).await?.eval(self, &None).await?;
            results.push(result);
        }
        Ok(results)
         */
        todo!()
    }


    pub fn deep_clone(&self) -> Self {
        match self {
            Self::Top => Self::Top,
            Self::Expansion(expansion) => Self::Expansion(Gc::new(expansion.read().deep_clone())),
            Self::LexicalContour(env) => Self::LexicalContour(Gc::new(env.read().deep_clone())),
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
    // CompileError(CompileError),
    RuntimeError(RuntimeError),
}
*/
