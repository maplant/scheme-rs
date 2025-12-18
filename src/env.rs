use std::{
    collections::{HashMap, HashSet, hash_map::Entry},
    fmt,
    hash::{Hash, Hasher},
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
};

use either::Either;
use parking_lot::RwLock;
use scheme_rs_macros::{maybe_async, maybe_await};

#[cfg(feature = "async")]
use futures::future::BoxFuture;

use crate::{
    ast::{ImportSet, SpecialKeyword},
    exceptions::Condition,
    gc::{Gc, Trace},
    proc::Procedure,
    registry::{Import, ImportError, Library},
    symbols::Symbol,
    syntax::{Identifier, Mark},
    value::Value,
};

// TODO: We need to aggressively add caching to basically every data structure
// in here. It's a pain, but will eventually be necessary for compiling large
// projects.

#[derive(Trace)]
pub(crate) struct LexicalContourInner {
    up: Environment,
    vars: HashMap<Identifier, Local>,
    keywords: HashMap<Identifier, Procedure>,
    imports: HashMap<Identifier, Import>,
}

impl LexicalContourInner {
    fn new(env: &Environment) -> Self {
        Self {
            up: env.clone(),
            vars: Default::default(),
            keywords: Default::default(),
            imports: Default::default(),
        }
    }
}

impl LexicalContourInner {
    pub fn def_var(&mut self, name: Identifier) -> Local {
        let local = Local::gensym_with_name(name.sym);
        self.vars.insert(name, local);
        local
    }

    pub fn def_keyword(&mut self, name: Identifier, proc: Procedure) {
        self.keywords.insert(name, proc);
    }

    #[cfg(not(feature = "async"))]
    pub fn fetch_var(&self, name: &Identifier) -> Result<Option<Var>, Condition> {
        if let Some(local) = self.vars.get(name) {
            return Ok(Some(Var::Local(*local)));
        }
        self.up.fetch_var(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Var>, Condition>> {
        if let Some(local) = self.vars.get(name) {
            let local = *local;
            return Box::pin(async move { Ok(Some(Var::Local(local))) });
        }
        let up = self.up.clone();
        Box::pin(async move { up.fetch_var(name).await })
    }

    #[cfg(not(feature = "async"))]
    pub fn fetch_special_keyword_or_var(
        &self,
        name: &Identifier,
    ) -> Result<Option<Either<SpecialKeyword, Var>>, Condition> {
        if let Some(local) = self.vars.get(name) {
            return Ok(Some(Either::Right(Var::Local(*local))));
        }
        self.up.fetch_special_keyword_or_var(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_special_keyword_or_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Either<SpecialKeyword, Var>>, Condition>> {
        if let Some(local) = self.vars.get(name) {
            let local = *local;
            return Box::pin(async move { Ok(Some(Either::Right(Var::Local(local)))) });
        }
        let up = self.up.clone();
        Box::pin(async move { up.fetch_special_keyword_or_var(name).await })
    }

    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        if let Some(local) = self.vars.get(name) {
            return Some(*local);
        }
        self.up.fetch_local(name)
    }

    pub fn fetch_top(&self) -> Library {
        self.up.fetch_top()
    }
}

#[derive(Clone, Trace)]
pub struct LexicalContour(Gc<RwLock<LexicalContourInner>>);

impl LexicalContour {
    #[cfg(not(feature = "async"))]
    pub fn fetch_keyword(&self, name: &Identifier) -> Result<Option<Keyword>, Condition> {
        self.clone().fetch_keyword_inner(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_keyword<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Keyword>, Condition>> {
        Box::pin(self.clone().fetch_keyword_inner(name))
    }

    #[maybe_async]
    fn fetch_keyword_inner(self, name: &Identifier) -> Result<Option<Keyword>, Condition> {
        let up = {
            let this = self.0.read();
            if let Some(trans) = this.keywords.get(name) {
                let trans = trans.clone();
                drop(this);
                return Ok(Some(Keyword::new(Environment::LexicalContour(self), trans)));
            }
            this.up.clone()
        };
        maybe_await!(up.fetch_keyword(name))
    }

    #[maybe_async]
    pub fn import(&self, import_set: ImportSet) -> Result<(), ImportError> {
        let (rt, registry) = {
            let top = self.0.read().fetch_top();
            let top = top.0.read();
            (top.rt.clone(), top.rt.get_registry())
        };
        let imports = maybe_await!(registry.import(&rt, import_set))?;
        let mut this = self.0.write();
        for (ident, import) in imports {
            match this.imports.entry(ident) {
                Entry::Occupied(prev_imported) if prev_imported.get().origin != import.origin => {
                    return Err(ImportError::DuplicateIdentifier(prev_imported.key().sym));
                }
                Entry::Vacant(slot) => {
                    slot.insert(import);
                }
                _ => (),
            }
        }
        Ok(())
    }

    fn binding_env(&self, name: &Identifier) -> Option<EnvId> {
        let this = self.0.read();
        if this.vars.contains_key(name) || this.keywords.contains_key(name) {
            Some(EnvId::new(&self.0))
        } else {
            this.up.binding_env(name)
        }
    }
}

#[derive(Trace)]
pub(crate) struct LetSyntaxContourInner {
    up: Environment,
    keywords: HashMap<Identifier, Procedure>,
    recursive: bool,
}

impl LetSyntaxContourInner {
    fn new(env: &Environment, recursive: bool) -> Self {
        Self {
            up: env.clone(),
            keywords: Default::default(),
            recursive,
        }
    }
}

impl LetSyntaxContourInner {
    pub fn def_var(&self, name: Identifier) -> Var {
        self.up.def_var(name)
    }

    pub fn def_keyword(&mut self, name: Identifier, proc: Procedure) {
        self.keywords.insert(name, proc);
    }

    #[cfg(not(feature = "async"))]
    pub fn fetch_var(&self, name: &Identifier) -> Result<Option<Var>, Condition> {
        self.up.fetch_var(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Var>, Condition>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_var(name).await })
    }

    #[cfg(not(feature = "async"))]
    pub fn fetch_special_keyword_or_var(
        &self,
        name: &Identifier,
    ) -> Result<Option<Either<SpecialKeyword, Var>>, Condition> {
        self.up.fetch_special_keyword_or_var(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_special_keyword_or_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Either<SpecialKeyword, Var>>, Condition>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_special_keyword_or_var(name).await })
    }

    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        self.up.fetch_local(name)
    }

    pub fn fetch_top(&self) -> Library {
        self.up.fetch_top()
    }

    #[cfg(not(feature = "async"))]
    pub fn import(&self, import_set: ImportSet) -> Result<(), ImportError> {
        self.up.import(import_set)
    }

    #[cfg(feature = "async")]
    pub fn import(&self, import_set: ImportSet) -> BoxFuture<'static, Result<(), ImportError>> {
        let up = self.up.clone();
        Box::pin(async move { up.import(import_set).await })
    }
}

#[derive(Clone, Trace)]
pub struct LetSyntaxContour(Gc<RwLock<LetSyntaxContourInner>>);

impl LetSyntaxContour {
    #[cfg(not(feature = "async"))]
    pub fn fetch_keyword(&self, name: &Identifier) -> Result<Option<Keyword>, Condition> {
        self.clone().fetch_keyword_inner(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_keyword<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Keyword>, Condition>> {
        Box::pin(self.clone().fetch_keyword_inner(name))
    }

    #[maybe_async]
    fn fetch_keyword_inner(self, name: &Identifier) -> Result<Option<Keyword>, Condition> {
        let up = {
            let this = self.0.read();
            if let Some(trans) = this.keywords.get(name) {
                let trans = trans.clone();
                let env = if this.recursive {
                    drop(this);
                    Environment::LetSyntaxContour(self)
                } else {
                    this.up.clone()
                };
                return Ok(Some(Keyword::new(env, trans)));
            }
            this.up.clone()
        };
        maybe_await!(up.fetch_keyword(name))
    }

    fn binding_env(&self, name: &Identifier) -> Option<EnvId> {
        let this = self.0.read();
        if this.keywords.contains_key(name) {
            let env = if this.recursive {
                EnvId::new(&self.0)
            } else {
                this.up.to_id()
            };
            Some(env)
        } else {
            this.up.binding_env(name)
        }
    }
}

#[derive(Trace)]
pub struct MacroExpansion {
    up: Environment,
    mark: Mark,
    source: Environment,
}

impl MacroExpansion {
    pub fn new(env: &Environment, mark: Mark, source: Environment) -> Self {
        Self {
            up: env.clone(),
            mark,
            source,
        }
    }
}

macro_rules! macro_resolver_fn {
    ( $outer:ident, $inner:ident -> $ret:ty ) => {
        #[cfg(not(feature = "async"))]
        pub fn $outer(&self, name: &Identifier) -> Result<Option<$ret>, Condition> {
            Self::$inner(&self.up, &self.source, self.mark, name)
        }

        #[cfg(feature = "async")]
        pub fn $outer<'a>(
            &self,
            name: &'a Identifier,
        ) -> BoxFuture<'a, Result<Option<$ret>, Condition>> {
            let up = self.up.clone();
            let source = self.source.clone();
            let mark = self.mark;
            Box::pin(async move { Self::$inner(&up, &source, mark, name).await })
        }

        #[maybe_async]
        fn $inner(
            up: &Environment,
            source: &Environment,
            mark: Mark,
            name: &Identifier,
        ) -> Result<Option<$ret>, Condition> {
            // Attempt to check the up scope first:
            let var = maybe_await!(up.$outer(name))?;
            if var.is_some() {
                return Ok(var);
            }

            // If the current expansion context contains the mark, remove it and check the
            // expansion source scope.
            if name.marks.contains(&mark) {
                let mut unmarked = name.clone();
                unmarked.mark(mark);
                maybe_await!(source.$outer(&unmarked))
            } else {
                Ok(None)
            }
        }
    };
}

impl MacroExpansion {
    pub fn def_var(&self, name: Identifier) -> Var {
        // In the case of defining variables produced from macro expansions, pass them
        // on to the next environment up.
        self.up.def_var(name)
    }

    pub fn def_keyword(&self, name: Identifier, proc: Procedure) {
        self.up.def_keyword(name, proc);
    }

    macro_resolver_fn! (
        fetch_var, fetch_var_inner -> Var
    );

    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        // Attempt to check the up scope first:
        let var = self.up.fetch_local(name);
        if var.is_some() {
            return var;
        }
        // If the current expansion context contains the mark, remove it and check the
        // expansion source scope.
        name.marks
            .contains(&self.mark)
            .then(|| {
                let mut unmarked = name.clone();
                unmarked.mark(self.mark);
                self.source.fetch_local(&unmarked)
            })
            .flatten()
    }

    pub fn fetch_pattern_variable(&self, name: &Identifier) -> Option<Local> {
        let var = self.up.fetch_pattern_variable(name);
        if var.is_some() {
            return var;
        }
        name.marks
            .contains(&self.mark)
            .then(|| {
                let mut unmarked = name.clone();
                unmarked.mark(self.mark);
                self.source.fetch_pattern_variable(&unmarked)
            })
            .flatten()
    }

    macro_resolver_fn!(
        fetch_keyword, fetch_keyword_inner -> Keyword
    );

    macro_resolver_fn!(
        fetch_special_keyword_or_var, fetch_special_keyword_or_var_inner -> Either<SpecialKeyword, Var>
    );

    pub fn fetch_top(&self) -> Library {
        self.up.fetch_top()
    }

    #[cfg(not(feature = "async"))]
    pub fn import(&self, import_set: ImportSet) -> Result<(), ImportError> {
        self.up.import(import_set)
    }

    #[cfg(feature = "async")]
    pub fn import(&self, import_set: ImportSet) -> BoxFuture<'static, Result<(), ImportError>> {
        let up = self.up.clone();
        Box::pin(async move { up.import(import_set).await })
    }

    pub(crate) fn binding_env(&self, name: &Identifier) -> Option<EnvId> {
        self.up.binding_env(name).or_else(|| {
            name.marks
                .contains(&self.mark)
                .then(|| {
                    let mut unmarked = name.clone();
                    unmarked.mark(self.mark);
                    self.source.binding_env(&unmarked)
                })
                .flatten()
        })
    }
}

#[derive(Trace, derive_more::Debug)]
pub struct SyntaxCaseExpr {
    #[debug(skip)]
    up: Environment,
    expansions_store: Local,
    pattern_vars: HashSet<Identifier>,
}

impl SyntaxCaseExpr {
    fn new(env: &Environment, expansions_store: Local, pattern_vars: HashSet<Identifier>) -> Self {
        Self {
            up: env.clone(),
            expansions_store,
            pattern_vars,
        }
    }

    fn fetch_top(&self) -> Library {
        self.up.fetch_top()
    }

    fn def_var(&self, name: Identifier) -> Var {
        self.up.def_var(name)
    }

    fn def_keyword(&self, name: Identifier, val: Procedure) {
        self.up.def_keyword(name, val);
    }

    #[cfg(not(feature = "async"))]
    fn fetch_var(&self, name: &Identifier) -> Result<Option<Var>, Condition> {
        self.up.fetch_var(name)
    }

    #[cfg(feature = "async")]
    fn fetch_var<'a>(&self, name: &'a Identifier) -> BoxFuture<'a, Result<Option<Var>, Condition>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_var(name).await })
    }

    fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        self.up.fetch_local(name)
    }

    #[cfg(not(feature = "async"))]
    fn fetch_keyword(&self, name: &Identifier) -> Result<Option<Keyword>, Condition> {
        self.up.fetch_keyword(name)
    }

    #[cfg(feature = "async")]
    fn fetch_keyword<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Keyword>, Condition>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_keyword(name).await })
    }

    #[cfg(not(feature = "async"))]
    fn fetch_special_keyword_or_var(
        &self,
        name: &Identifier,
    ) -> Result<Option<Either<SpecialKeyword, Var>>, Condition> {
        self.up.fetch_special_keyword_or_var(name)
    }

    #[cfg(feature = "async")]
    fn fetch_special_keyword_or_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Either<SpecialKeyword, Var>>, Condition>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_special_keyword_or_var(name).await })
    }

    #[cfg(not(feature = "async"))]
    fn import(&self, import: ImportSet) -> Result<(), ImportError> {
        self.up.import(import)
    }

    #[cfg(feature = "async")]
    fn import(&self, import: ImportSet) -> BoxFuture<'static, Result<(), ImportError>> {
        let up = self.up.clone();
        Box::pin(async move { up.import(import).await })
    }

    fn fetch_pattern_variable(&self, name: &Identifier) -> Option<Local> {
        if self.pattern_vars.contains(name) {
            Some(self.expansions_store)
        } else {
            self.up.fetch_pattern_variable(name)
        }
    }

    fn binding_env(&self, name: &Identifier) -> Option<EnvId> {
        self.up.binding_env(name)
    }
}

#[derive(Trace)]
pub enum Environment {
    Top(Library),
    LexicalContour(LexicalContour),
    LetSyntaxContour(LetSyntaxContour),
    MacroExpansion(Gc<RwLock<MacroExpansion>>),
    SyntaxCaseExpr(Gc<RwLock<SyntaxCaseExpr>>),
}

impl Environment {
    pub fn fetch_top(&self) -> Library {
        match self {
            Self::Top(top) => top.clone(),
            Self::LexicalContour(lex) => lex.0.read().fetch_top(),
            Self::LetSyntaxContour(ls) => ls.0.read().fetch_top(),
            Self::MacroExpansion(me) => me.read().fetch_top(),
            Self::SyntaxCaseExpr(me) => me.read().fetch_top(),
        }
    }

    pub fn def_var(&self, name: Identifier) -> Var {
        match self {
            Self::Top(top) => Var::Global(top.def_var(name, Value::undefined())),
            Self::LexicalContour(lex) => Var::Local(lex.0.write().def_var(name)),
            Self::LetSyntaxContour(ls) => ls.0.read().def_var(name),
            Self::MacroExpansion(me) => me.read().def_var(name),
            Self::SyntaxCaseExpr(me) => me.read().def_var(name),
        }
    }

    pub fn def_keyword(&self, name: Identifier, val: Procedure) {
        match self {
            Self::Top(top) => top.def_keyword(name, Keyword::new(self.clone(), val)),
            Self::LexicalContour(lex) => lex.0.write().def_keyword(name, val),
            Self::LetSyntaxContour(ls) => ls.0.write().def_keyword(name, val),
            Self::MacroExpansion(me) => me.read().def_keyword(name, val),
            Self::SyntaxCaseExpr(me) => me.read().def_keyword(name, val),
        }
    }

    #[maybe_async]
    pub fn fetch_var(&self, name: &Identifier) -> Result<Option<Var>, Condition> {
        let fetch_result = match self {
            Self::Top(top) => return Ok(maybe_await!(top.fetch_var(name))?.map(Var::Global)),
            Self::LexicalContour(lex) => lex.0.read().fetch_var(name),
            Self::LetSyntaxContour(ls) => ls.0.read().fetch_var(name),
            Self::MacroExpansion(me) => me.read().fetch_var(name),
            Self::SyntaxCaseExpr(sc) => sc.read().fetch_var(name),
        };
        maybe_await!(fetch_result)
    }

    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        match self {
            Self::Top(_) => None,
            Self::LexicalContour(lex) => lex.0.read().fetch_local(name),
            Self::LetSyntaxContour(ls) => ls.0.read().fetch_local(name),
            Self::MacroExpansion(me) => me.read().fetch_local(name),
            Self::SyntaxCaseExpr(sc) => sc.read().fetch_local(name),
        }
    }

    #[maybe_async]
    pub fn fetch_keyword(&self, name: &Identifier) -> Result<Option<Keyword>, Condition> {
        let fetch_result = match self {
            Self::Top(top) => top.fetch_keyword(name),
            Self::LexicalContour(lex) => lex.fetch_keyword(name),
            Self::LetSyntaxContour(ls) => ls.fetch_keyword(name),
            Self::MacroExpansion(me) => me.read().fetch_keyword(name),
            Self::SyntaxCaseExpr(sc) => sc.read().fetch_keyword(name),
        };
        maybe_await!(fetch_result)
    }

    #[maybe_async]
    pub fn fetch_special_keyword_or_var(
        &self,
        name: &Identifier,
    ) -> Result<Option<Either<SpecialKeyword, Var>>, Condition> {
        let fetch_result = match self {
            Self::Top(top) => {
                if let Some(var) = maybe_await!(top.fetch_var(name))? {
                    return Ok(Some(Either::Right(Var::Global(var))));
                }
                return Ok(top.fetch_special_keyword(name).map(Either::Left));
            }
            Self::LexicalContour(lex) => lex.0.read().fetch_special_keyword_or_var(name),
            Self::LetSyntaxContour(ls) => ls.0.read().fetch_special_keyword_or_var(name),
            Self::MacroExpansion(me) => me.read().fetch_special_keyword_or_var(name),
            Self::SyntaxCaseExpr(sc) => sc.read().fetch_special_keyword_or_var(name),
        };
        maybe_await!(fetch_result)
    }

    #[maybe_async]
    pub fn import(&self, import: ImportSet) -> Result<(), ImportError> {
        let import_result = match self {
            Self::Top(top) => return maybe_await!(top.import(import)),
            Self::LexicalContour(lex) => return maybe_await!(lex.import(import)),
            Self::LetSyntaxContour(ls) => ls.0.read().import(import),
            Self::MacroExpansion(me) => me.read().import(import),
            Self::SyntaxCaseExpr(sc) => sc.read().import(import),
        };
        maybe_await!(import_result)
    }

    pub fn fetch_pattern_variable(&self, name: &Identifier) -> Option<Local> {
        match self {
            Self::Top(_) => None,
            Self::LexicalContour(lex) => lex.0.read().up.fetch_pattern_variable(name),
            Self::LetSyntaxContour(ls) => ls.0.read().up.fetch_pattern_variable(name),
            Self::MacroExpansion(me) => me.read().fetch_pattern_variable(name),
            Self::SyntaxCaseExpr(sc) => sc.read().fetch_pattern_variable(name),
        }
    }

    /// Return the binding environment of the variable or keyword, if it exists
    pub(crate) fn binding_env(&self, name: &Identifier) -> Option<EnvId> {
        match self {
            Self::Top(top) => top.binding_env(name),
            Self::LexicalContour(lex) => lex.binding_env(name),
            Self::LetSyntaxContour(ls) => ls.binding_env(name),
            Self::MacroExpansion(me) => me.read().binding_env(name),
            // I don't think this is technically correct; it should probably
            // return the syntax case expr env if the binding variable is hit
            Self::SyntaxCaseExpr(sc) => sc.read().binding_env(name),
        }
    }

    pub fn new_lexical_contour(&self) -> Self {
        let new_lexical_contour = LexicalContourInner::new(self);
        Self::LexicalContour(LexicalContour(Gc::new(RwLock::new(new_lexical_contour))))
    }

    pub fn new_let_syntax_contour(&self, recursive: bool) -> Self {
        let new_let_syntax_contour = LetSyntaxContourInner::new(self, recursive);
        Self::LetSyntaxContour(LetSyntaxContour(Gc::new(RwLock::new(
            new_let_syntax_contour,
        ))))
    }

    pub fn new_macro_expansion(&self, mark: Mark, source: Environment) -> Self {
        let new_macro_expansion = MacroExpansion::new(self, mark, source);
        Self::MacroExpansion(Gc::new(RwLock::new(new_macro_expansion)))
    }

    pub fn new_syntax_case_expr(
        &self,
        expansions_store: Local,
        pattern_vars: HashSet<Identifier>,
    ) -> Self {
        let syntax_case_expr = SyntaxCaseExpr::new(self, expansions_store, pattern_vars);
        Self::SyntaxCaseExpr(Gc::new(RwLock::new(syntax_case_expr)))
    }

    pub(crate) fn to_id(&self) -> EnvId {
        match self {
            Self::Top(top) => EnvId::new(&top.0),
            Self::LexicalContour(lex) => EnvId::new(&lex.0),
            Self::LetSyntaxContour(ls) => EnvId::new(&ls.0),
            Self::MacroExpansion(me) => EnvId::new(me),
            Self::SyntaxCaseExpr(sc) => EnvId::new(sc),
        }
    }
}

impl From<Library> for Environment {
    fn from(top: Library) -> Self {
        Self::Top(top)
    }
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        match self {
            Self::Top(top) => Self::Top(top.clone()),
            Self::LexicalContour(lex) => Self::LexicalContour(lex.clone()),
            Self::LetSyntaxContour(ls) => Self::LetSyntaxContour(ls.clone()),
            Self::MacroExpansion(mac) => Self::MacroExpansion(mac.clone()),
            Self::SyntaxCaseExpr(sc) => Self::SyntaxCaseExpr(sc.clone()),
        }
    }
}

impl fmt::Debug for Environment {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl PartialEq for Environment {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::Top(lhs), Self::Top(rhs)) => lhs == rhs,
            (Self::LexicalContour(lhs), Self::LexicalContour(rhs)) => Gc::ptr_eq(&lhs.0, &rhs.0),
            (Self::LetSyntaxContour(lhs), Self::LetSyntaxContour(rhs)) => {
                Gc::ptr_eq(&lhs.0, &rhs.0)
            }
            (Self::MacroExpansion(lhs), Self::MacroExpansion(rhs)) => Gc::ptr_eq(lhs, rhs),
            (Self::SyntaxCaseExpr(lhs), Self::SyntaxCaseExpr(rhs)) => Gc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }
}

/// A unique identifier for a binding environment.
#[repr(transparent)]
#[derive(Copy, Clone, Debug, Trace, PartialEq)]
pub struct EnvId(#[trace(skip)] NonNull<()>);

impl EnvId {
    pub(crate) fn new<T>(value: &Gc<T>) -> Self {
        // Safety: we're converting one non-null to another, so we don't need to
        // check its validity.
        Self(unsafe { NonNull::new_unchecked(value.ptr.as_ptr() as *mut ()) })
    }
}

unsafe impl Send for EnvId {}
unsafe impl Sync for EnvId {}

#[derive(Copy, Clone, Trace)]
pub struct Local {
    pub(crate) id: usize,
    pub(crate) name: Option<Symbol>,
}

impl Hash for Local {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.id.hash(state);
    }
}

impl PartialEq for Local {
    fn eq(&self, rhs: &Self) -> bool {
        self.id == rhs.id
    }
}

impl Eq for Local {}

impl Local {
    /// Create a new temporary value.
    pub fn gensym() -> Self {
        static NEXT_SYM: AtomicUsize = AtomicUsize::new(0);
        Self {
            id: NEXT_SYM.fetch_add(1, Ordering::Relaxed),
            name: None,
        }
    }

    pub fn gensym_with_name(name: Symbol) -> Self {
        let mut sym = Self::gensym();
        sym.name = Some(name);
        sym
    }

    pub fn to_func_name(&self) -> String {
        if let Some(name) = self.name {
            format!("{name}")
        } else {
            format!("f{}", self.id)
        }
    }
}

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.name {
            write!(f, "{name}")
        } else {
            write!(f, "%{}", self.id)
        }
    }
}

impl fmt::Debug for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.name {
            write!(f, "{name}")
        } else {
            write!(f, "%{}", self.id)
        }
    }
}

// TODO: Do we need to make this pointer eq?
#[derive(Clone, Trace)]
pub struct Global {
    pub(crate) name: Identifier,
    pub(crate) val: Gc<RwLock<Value>>,
    pub(crate) mutable: bool,
}

impl Global {
    pub fn new(name: Identifier, val: Gc<RwLock<Value>>, mutable: bool) -> Self {
        Global { name, val, mutable }
    }

    pub fn value(self) -> Gc<RwLock<Value>> {
        self.val
    }

    pub fn value_ref(&self) -> &Gc<RwLock<Value>> {
        &self.val
    }
}

impl fmt::Debug for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.name.sym)
    }
}

impl PartialEq for Global {
    fn eq(&self, rhs: &Self) -> bool {
        self.name == rhs.name && Gc::ptr_eq(&self.val, &rhs.val)
    }
}

impl Eq for Global {}

impl Hash for Global {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.name.hash(state);
        Gc::as_ptr(&self.val).hash(state);
    }
}

#[derive(Clone, Trace, Hash, PartialEq, Eq)]
pub enum Var {
    Global(Global),
    Local(Local),
}

impl Var {
    pub fn symbol(&self) -> Option<Symbol> {
        match self {
            Var::Global(global) => Some(global.name.sym),
            Var::Local(local) => local.name,
        }
    }
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Global(global) => global.fmt(f),
            Self::Local(local) => local.fmt(f),
        }
    }
}

#[derive(Clone, Trace, derive_more::Debug)]
pub struct Keyword {
    #[debug(skip)]
    pub source_env: Environment,
    #[debug(skip)]
    pub transformer: Procedure,
}

impl Keyword {
    pub fn new(source_env: Environment, transformer: Procedure) -> Self {
        Self {
            source_env,
            transformer,
        }
    }
}

#[derive(Clone, Trace)]
#[repr(align(16))]
pub struct CapturedEnv {
    pub env: Environment,
    pub captured: Vec<Local>,
}

impl CapturedEnv {
    pub fn new(env: Environment, captured: Vec<Local>) -> Self {
        Self { env, captured }
    }
}
