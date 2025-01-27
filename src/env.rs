use either::Either;
use std::{
    collections::HashMap,
    fmt,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{
    gc::{Gc, Trace},
    syntax::{Identifier, Mark},
    value::Value,
};

/// A Top level environment.
pub trait Top: Trace + Send + Sync + 'static {
    fn def_var(&mut self, name: String) -> Var;

    fn def_macro(&mut self, name: String, val: Gc<Value>);

    fn set_var(&self, name: &str, val: Value);

    fn fetch_var(&mut self, name: &str) -> Option<Global>;

    fn fetch_macro(&self, name: &str) -> Option<Gc<Value>>;
}

/// Top level environment, or compilation unit, for a Scheme library.
#[derive(Trace)]
pub struct Library {
    vars: HashMap<String, Gc<Value>>,
    macros: HashMap<String, Gc<Value>>,
}

impl Top for Library {
    fn def_var(&mut self, name: String) -> Var {
        let global = Gc::new(Value::Undefined);
        self.vars.insert(name.clone(), global.clone());
        Var::Global(Global::new(name, global))
    }

    fn def_macro(&mut self, name: String, val: Gc<Value>) {
        self.macros.insert(name, val);
    }

    fn set_var(&self, name: &str, val: Value) {
        *self.vars.get(name).unwrap().write() = val;
    }

    fn fetch_var(&mut self, name: &str) -> Option<Global> {
        self.vars
            .get(name)
            .map(|val| Global::new(name.to_string(), val.clone()))
    }

    fn fetch_macro(&self, name: &str) -> Option<Gc<Value>> {
        self.macros.get(name).cloned()
    }
}

/// Top level environment for a Scheme program.
#[derive(Trace)]
pub struct Program {
    vars: HashMap<String, Gc<Value>>,
    macros: HashMap<String, Gc<Value>>,
}

/// Top level environment for a Scheme REPL.
#[derive(Default, Trace)]
pub struct Repl {
    vars: HashMap<String, Gc<Value>>,
    macros: HashMap<String, Gc<Value>>,
}

/// Repl's implementation of Top differs from Program and Library by assuming
/// that undefined variables are simply yet to be defined.
impl Top for Repl {
    fn def_var(&mut self, name: String) -> Var {
        let global = Gc::new(Value::Undefined);
        assert!(self.vars.insert(name.clone(), global.clone()).is_none());
        Var::Global(Global::new(name, global))
    }

    fn def_macro(&mut self, name: String, val: Gc<Value>) {
        self.macros.insert(name, val);
    }

    fn set_var(&self, name: &str, val: Value) {
        *self.vars.get(name).unwrap().write() = val;
    }

    fn fetch_var(&mut self, name: &str) -> Option<Global> {
        Some(Global::new(
            name.to_string(),
            self.vars
                .entry(name.to_string())
                .or_insert_with(|| Gc::new(Value::Undefined))
                .clone(),
        ))
    }

    fn fetch_macro(&self, name: &str) -> Option<Gc<Value>> {
        self.macros.get(name).cloned()
    }
}

#[derive(Trace)]
pub struct LexicalContour<T: Trace> {
    up: Environment<T>,
    vars: HashMap<Identifier, Local>,
    macros: HashMap<Identifier, Gc<Value>>,
}

impl<T: Top> LexicalContour<T> {
    fn new(env: &Environment<T>) -> Self {
        Self {
            up: env.clone(),
            vars: Default::default(),
            macros: Default::default(),
        }
    }
}

impl<T: Top> Gc<LexicalContour<T>> {
    pub fn def_var(&self, name: Identifier) -> Local {
        let local = Local::gensym();
        self.write().vars.insert(name, local);
        local
    }

    pub fn def_macro(&self, name: Identifier, value: Gc<Value>) {
        self.write().macros.insert(name, value);
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        if let Some(local) = self.read().vars.get(name).copied() {
            return Some(Var::Local(local));
        }
        self.read().up.fetch_var(name)
    }

    pub fn fetch_macro(&self, _name: &Identifier) -> Option<(MacroSource<T>, Gc<Value>)> {
        todo!()
    }
}

#[derive(Trace)]
pub struct MacroExpansion<T: Trace> {
    up: Environment<T>,
    mark: Mark,
    source: Either<Gc<LexicalContour<T>>, Gc<T>>,
}

pub type MacroSource<T> = Either<Gc<LexicalContour<T>>, Gc<T>>;

impl<T: Top> MacroExpansion<T> {
    pub fn new(
        env: &Environment<T>,
        mark: Mark,
        source: Either<Gc<LexicalContour<T>>, Gc<T>>,
    ) -> Self {
        Self {
            up: env.clone(),
            mark,
            source,
        }
    }
}

impl<T: Top> Gc<MacroExpansion<T>> {
    pub fn def_var(&self, name: Identifier) -> Var {
        // In the case of defining variables produced from macro expansions, pass them
        // on to the next environment up.
        self.read().up.def_var(name)
    }

    pub fn def_macro(&self, name: Identifier, val: Gc<Value>) {
        self.read().up.def_macro(name, val);
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        // Attempt to check the up scope first:
        let this = self.read();
        let var = this.up.fetch_var(name);
        if var.is_some() {
            return var;
        }
        // If the current expansion context contains the mark, remove it and check the
        // expansion source scope.
        if name.marks.contains(&this.mark) {
            match this.source {
                Either::Left(ref lex) => {
                    let mut unmarked = name.clone();
                    unmarked.mark(this.mark);
                    lex.fetch_var(&unmarked)
                }
                Either::Right(ref lib) => lib.write().fetch_var(&name.name).map(Var::Global),
            }
        } else {
            None
        }
    }

    pub fn fetch_macro(&self, _name: &Identifier) -> Option<(MacroSource<T>, Gc<Value>)> {
        todo!()
    }
}

#[derive(Trace)]
pub enum Environment<T: Trace> {
    Top(Gc<T>),
    LexicalContour(Gc<LexicalContour<T>>),
    MacroExpansion(Gc<MacroExpansion<T>>),
}

impl<T: Top> Environment<T> {
    pub fn def_var(&self, name: Identifier) -> Var {
        match self {
            Self::Top(top) => top.write().def_var(name.name),
            Self::LexicalContour(lex) => Var::Local(lex.def_var(name)),
            Self::MacroExpansion(me) => me.def_var(name),
        }
    }

    pub fn def_macro(&self, name: Identifier, val: Gc<Value>) {
        match self {
            Self::Top(top) => top.write().def_macro(name.name, val),
            Self::LexicalContour(lex) => lex.def_macro(name, val),
            Self::MacroExpansion(me) => me.def_macro(name, val),
        }
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        match self {
            Self::Top(top) => top.write().fetch_var(&name.name).map(Var::Global),
            Self::LexicalContour(lex) => lex.fetch_var(name),
            Self::MacroExpansion(me) => me.fetch_var(name),
        }
    }

    pub fn fetch_macro(&self, _name: &Identifier) -> Option<(MacroSource<T>, Gc<Value>)> {
        /*
        match self {
            Self::Top(top) => top.read().fetch_macro(&name.name).map(|mac| (Either::Right(top.clone()), mac)),
            Self::LexicalContour(lex) => lex.fetch_macro(name),
            Self::MacroExpansion(me) => me.fetch_macro(name),
        }
         */
        // For now, just None
        None
    }

    pub fn is_bound(&self, name: &Identifier) -> bool {
        self.fetch_var(name).is_some()
    }

    pub fn new_lexical_contour(&self) -> Self {
        let new_lexical_contour = LexicalContour::new(self);
        Self::LexicalContour(Gc::new(new_lexical_contour))
    }

    pub fn new_macro_expansion(
        &self,
        mark: Mark,
        source: Either<Gc<LexicalContour<T>>, Gc<T>>,
    ) -> Self {
        let new_macro_expansion = MacroExpansion::new(self, mark, source);
        Self::MacroExpansion(Gc::new(new_macro_expansion))
    }
}

impl Environment<Repl> {
    pub fn new_repl() -> Self {
        Self::Top(Gc::new(Repl::default()))
    }
}

impl<T: Top> Clone for Environment<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Top(top) => Self::Top(top.clone()),
            Self::LexicalContour(lex) => Self::LexicalContour(lex.clone()),
            Self::MacroExpansion(mac) => Self::MacroExpansion(mac.clone()),
        }
    }
}

#[derive(Copy, Clone, Trace, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Local(usize);

impl Local {
    /// Create a new temporary value.
    pub fn gensym() -> Self {
        static NEXT_SYM: AtomicUsize = AtomicUsize::new(0);
        Self(NEXT_SYM.fetch_add(1, Ordering::Relaxed))
    }

    pub fn to_func_name(&self) -> String {
        format!("f{}", self.0)
    }
}

impl ToString for Local {
    fn to_string(&self) -> String {
        format!("%{}", self.0)
    }
}

impl fmt::Debug for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Clone, Trace, Hash, PartialEq, Eq)]
pub struct Global {
    name: String,
    val: Gc<Value>,
}

impl Global {
    pub fn new(name: String, val: Gc<Value>) -> Self {
        Global { name, val }
    }

    pub fn value(self) -> Gc<Value> {
        self.val
    }
}

impl fmt::Debug for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.name)
    }
}

#[derive(Clone, Trace, Hash, PartialEq, Eq)]
pub enum Var {
    Global(Global),
    Local(Local),
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Global(global) => global.fmt(f),
            Self::Local(local) => local.fmt(f),
        }
    }
}

/*
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
    pub fn fetch_var_ref(&self, ident: &Identifier) -> Option<DeBruijnIndex> {
        self.var_names.get_index_of(ident).map_or_else(
            || {
                self.up
                    .as_ref()
                    .and_then(|up| up.read().fetch_var_ref(ident).map(DeBruijnIndex::inc_depth))
            },
            |offset| Some(DeBruijnIndex { depth: 0, offset }),
        )
    }

    /// Fetch the value of the variable. Will panic if no variable exists.
    pub fn fetch_var(&self, var_ref: DeBruijnIndex) -> Gc<Value> {
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
    pub fn set_var(&mut self, var_ref: DeBruijnIndex, new_val: Gc<Value>) {
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
                inner = new_cont.enter_extent(args).await;
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

    pub fn deep_clone(&self, cloned: &mut HashMap<Self, Self>) -> Self {
        if cloned.contains_key(self) {
            cloned.get(self).unwrap().clone()
        } else {
            let this = self.read();
            let clone = Gc::new(Env {
                up: this.up.as_ref().map(|up| up.deep_clone(cloned)),
                vars: this.vars.clone(),
                var_names: this.var_names.clone(),
                macros: this.macros.clone(),
            });
            cloned.insert(self.clone(), clone.clone());
            clone
        }
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
#[derive(Debug, Copy, Clone, Trace, Default)]
pub struct DeBruijnIndex {
    /// Number of up environments we need to traverse in order to reach the defining
    /// scope of the variable. Variables with a depth of 0 are local.
    depth: usize,
    /// Offset into the `vars` field of the environment.
    offset: usize,
}

impl DeBruijnIndex {
    pub fn inc_depth(self) -> Self {
        Self {
            depth: self.depth + 1,
            offset: self.offset,
        }
    }

    pub fn dec_depth(self) -> Self {
        Self {
            depth: self.depth - 1,
            offset: self.offset,
        }
    }

    pub fn offset(mut self, offset: usize) -> Self {
        self.offset += offset;
        self
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
    var_ref: DeBruijnIndex,
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
// TODO: Rename
pub enum VariableRef {
    Macro(MacroVarRef),
    Global(GlobalRef),
    Lexical(DeBruijnIndex),
}

impl VariableRef {
    pub fn fetch(&self, env: &Gc<Env>) -> Result<Gc<Value>, RuntimeError> {
        match self {
            Self::Macro(m) => Ok(m.fetch()),
            Self::Global(g) => g
                .fetch(env)
                .ok_or_else(|| RuntimeError::undefined_variable(g.name.clone())),
            Self::Lexical(v) => Ok(env.read().fetch_var(*v)),
        }
    }

    pub async fn set(&self, env: &Gc<Env>, value: &Gc<Value>) {
        match self {
            Self::Macro(m) => m.set(value),
            Self::Global(g) => g.set(env, value),
            Self::Lexical(v) => env.write().set_var(*v, value.clone()),
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
        !matches!(self.fetch_var_ref(ident), VariableRef::Global(_))
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

    // TODO: This can be cached.
    pub fn fetch_var_ref(&self, ident: &Identifier) -> VariableRef {
        println!("num_marks: {}", ident.marks.len());
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
                    VariableRef::Macro(m) => VariableRef::Lexical({
                        let mut local = m.var_ref;
                        local.depth += self.lexical_contour.distance(&m.env);
                        local
                    }),
                    x => x,
                },
                VariableRef::Lexical,
            )
    }

    /// Lexical environments are separately linked, so when we know that a variable is
    /// free in the current lexical environment, we can just check the macro envs.
    // TODO: This can be cached.
    fn fetch_macro_var_ref(&self, ident: &Identifier) -> VariableRef {
        let mut ident = ident.clone();
        for expansion_ctx in self.expansion_ctxs.iter().rev() {
            if ident.marks.contains(&expansion_ctx.mark) {
                // Strip the mark and check the macro environment
                ident.mark(expansion_ctx.mark);

                // Fetch the var from the macro env:
                let Some(expansion_ctx_ref) = expansion_ctx.env.read().fetch_var_ref(&ident) else {
                    continue;
                };

                return VariableRef::Macro(MacroVarRef {
                    env: expansion_ctx.env.clone(),
                    var_ref: expansion_ctx_ref,
                });
            }
        }

        if let Some(up) = self.up {
            up.fetch_macro_var_ref(&ident)
        } else {
            VariableRef::Global(GlobalRef::new(ident.clone()))
        }
    }

    // TODO: This can be cached.
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
    // TODO: This can be cached.
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
*/
