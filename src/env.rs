use indexmap::IndexSet;
use proc_macros::Trace;
use std::collections::HashMap;
use tokio::sync::OnceCell;

use crate::{
    ast::{parse::ParseAstError, AstNode},
    builtin::Builtin,
    continuation::Resumable,
    error::{RuntimeError, RuntimeErrorKind},
    gc::{init_gc, Gc},
    lex::{LexError, Token},
    parse::ParseError,
    syntax::{ExpansionCtx, Identifier, ParsedSyntax},
    value::Value,
};

/// An Environment, or store of variables and macros with a reference to its containing
/// lexical scope.
#[derive(derive_more::Debug, Default, Trace)]
pub struct Env {
    /// The containing environment of this environment, or None if this is a top level
    /// environment.
    #[debug(skip)]
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
                init_gc();

                let mut top = Env::default();

                // Install the builtins:
                for builtin in inventory::iter::<Builtin> {
                    builtin.install(&mut top);
                }

                // Install the stdlib:
                let top = Gc::new(top);
                let _ = top
                    .eval("stdlib.scm", include_str!("stdlib.scm"))
                    .await
                    .unwrap();

                top
            })
            .await
            .clone()
    }

    /// Compute the reference of the variable. If no such reference exists, the variable
    /// is either undefined or a global.
    pub fn fetch_var_ref(&self, ident: &Identifier) -> Option<VarRef> {
        self.var_names.get_index_of(ident).map_or_else(
            || {
                self.up
                    .as_ref()
                    .and_then(|up| up.read().fetch_var_ref(ident).map(VarRef::inc_depth))
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
        let (idx, new) = self.var_names.insert_full(ident.clone());
        if !new {
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
    /// Create a new lexical contour that is a child of self.
    pub fn new_lexical_contour(&self) -> Env {
        Env {
            up: Some(self.clone()),
            vars: Vec::with_capacity(1), // There will probably always be at least one var
            var_names: IndexSet::default(),
            macros: HashMap::new(),
        }
    }

    /// Evaluate a string, returning all of the results in a Vec
    // TODO: Add file name
    pub async fn eval<'e>(
        &self,
        file_name: &str,
        exprs: &'e str,
    ) -> Result<Vec<Vec<Gc<Value>>>, EvalError<'e>> {
        let tokens = Token::tokenize_file(file_name, exprs)?;
        let sexprs = ParsedSyntax::parse(&tokens)?;
        let mut results = Vec::new();
        for sexpr in sexprs {
            let Some(expr) = AstNode::from_syntax(sexpr.syntax, self, &None).await? else {
                continue;
            };
            // TODO: Catch continuation calls
            let vals = self.abandon_cc_trampoline(expr).await?;
            results.push(vals);
        }
        Ok(results)
    }

    async fn abandon_cc_trampoline(&self, expr: AstNode) -> Result<Vec<Gc<Value>>, RuntimeError> {
        let mut inner = expr.eval(self, &None).await;
        while let Err(RuntimeError {
            kind: RuntimeErrorKind::AbandonCurrentContinuation { args, new_cont },
            ..
        }) = inner
        {
            // Abandon the current continuation and evaluate the newly returned one
            // TODO: Retain the backtrace for errors
            // let arg = args.pop().unwrap();
            if let Some(new_cont) = new_cont {
                inner = new_cont.resume(args, &None).await;
            } else {
                return Ok(args);
            }
        }
        inner
    }

    pub fn distance(&self, rhs: &Gc<Env>) -> usize {
        if self == rhs {
            0
        } else {
            1 + self.read().up.as_ref().unwrap().distance(rhs)
        }
    }

    pub fn deep_clone(&self) -> Self {
        let this = self.read();
        Gc::new(Env {
            up: this.up.as_ref().map(|up| up.deep_clone()),
            vars: this.vars.clone(),
            var_names: this.var_names.clone(),
            macros: this.macros.clone(),
        })
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

    pub fn set(&self, value: &Gc<Value>) {
        self.env.write().set_var(self.var_ref, value.clone());
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
    pub fn fetch(&self, default: &Gc<Env>) -> Option<Gc<Value>> {
        let top = if let Some(top) = TOP_LEVEL_ENV.get() {
            top.clone()
        } else {
            // Currently installing the top level environment.
            get_top_env_from_curr(default.clone())
        };

        let top = top.read();
        let var = top.fetch_var_ref(&self.name)?;
        Some(top.fetch_var(var))
    }

    pub fn set(&self, default: &Gc<Env>, value: &Gc<Value>) {
        let top = if let Some(top) = TOP_LEVEL_ENV.get() {
            top.clone()
        } else {
            // Currently installing the top level environment.
            get_top_env_from_curr(default.clone())
        };
        top.write().def_local_var(&self.name, value.clone());
    }
}

fn get_top_env_from_curr(mut curr: Gc<Env>) -> Gc<Env> {
    while let Some(up) = {
        let curr = curr.read();
        curr.up.as_ref().cloned()
    } {
        curr = up.clone();
    }
    curr
}

/// A reference that can either be global, macro, or regular
#[derive(Debug, Clone, Trace)]
pub enum Ref {
    Macro(MacroVarRef),
    Global(GlobalRef),
    Regular(VarRef),
}

impl Ref {
    pub fn fetch(&self, env: &Gc<Env>) -> Result<Gc<Value>, Identifier> {
        match self {
            Self::Macro(m) => Ok(m.fetch()),
            Self::Global(g) => g.fetch(env).ok_or_else(|| g.name.clone()),
            Self::Regular(v) => Ok(env.read().fetch_var(*v)),
        }
    }

    pub async fn set(&self, env: &Gc<Env>, value: &Gc<Value>) {
        match self {
            Self::Macro(m) => m.set(value),
            Self::Global(g) => g.set(env, value),
            Self::Regular(v) => env.write().set_var(*v, value.clone()),
        }
    }
}

/// The result of looking up a macro. If the macro is present in the local environment,
/// `WithoutEnv` is returned (this is because we do not have the Gc of the env).
enum MacroLookup {
    WithEnv { env: Gc<Env>, value: Gc<Value> },
    WithoutEnv(Gc<Value>),
}

#[derive(derive_more::Debug)]
pub struct ExpansionEnv<'a> {
    up: Option<&'a ExpansionEnv<'a>>,
    pub expansion_ctxs: Vec<ExpansionCtx>,
    #[debug(skip)]
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

    pub fn push_expansion_env(&self, ctxs: Vec<ExpansionCtx>) -> ExpansionEnv<'_> {
        ExpansionEnv {
            lexical_contour: self.lexical_contour.clone(),
            up: Some(self),
            expansion_ctxs: ctxs,
        }
    }

    pub fn push_lexical_contour(&self, gc: Gc<Env>) -> ExpansionEnv<'_> {
        ExpansionEnv {
            up: Some(self),
            expansion_ctxs: Vec::new(),
            lexical_contour: gc,
        }
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
            .map_or_else(
                || match self.fetch_macro_var_ref(ident) {
                    Ref::Macro(m) => Ref::Regular({
                        let mut local = m.var_ref;
                        local.depth += self.lexical_contour.distance(&m.env);
                        local
                    }),
                    x => x,
                },
                Ref::Regular,
            )
    }

    /// Lexical environments are separately linked, so when we know that a variable is
    /// free in the current lexical environment, we can just check the macro envs.
    fn fetch_macro_var_ref(&self, ident: &Identifier) -> Ref {
        let mut ident = ident.clone();
        for expansion_ctx in self.expansion_ctxs.iter().rev() {
            if ident.marks.contains(&expansion_ctx.mark) {
                // Strip the mark and check the macro environment
                ident.mark(expansion_ctx.mark);

                // Fetch the var from the macro env:
                let Some(expansion_ctx_ref) = expansion_ctx.env.read().fetch_var_ref(&ident) else {
                    continue;
                };

                return Ref::Macro(MacroVarRef {
                    env: expansion_ctx.env.clone(),
                    var_ref: expansion_ctx_ref,
                });
            }
        }

        if let Some(up) = self.up {
            up.fetch_macro_var_ref(&ident)
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
        let mut ident = ident.clone();
        for expansion_ctx in &self.expansion_ctxs {
            if ident.marks.contains(&expansion_ctx.mark) {
                // Strip the mark and check the macro environment
                ident.mark(expansion_ctx.mark);
                // Fetch the macro from the macro env:
                match expansion_ctx.env.read().fetch_macro(&ident) {
                    Some(MacroLookup::WithEnv { env, value }) => return Some((env, value)),
                    Some(MacroLookup::WithoutEnv(value)) => {
                        return Some((expansion_ctx.env.clone(), value))
                    }
                    None => (),
                };
            }
        }

        if let Some(up) = self.up {
            up.fetch_macro_macro(&ident)
        } else {
            None
        }
    }
}
