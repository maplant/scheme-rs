//! Scheme lexical environments.

use std::{
    borrow::Cow,
    collections::{HashMap, hash_map::Entry},
    fmt,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
};

use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use scheme_rs_macros::{maybe_async, maybe_await};

#[cfg(feature = "async")]
use futures::future::BoxFuture;

use crate::{
    Either,
    ast::{
        DefinitionBody, ExportSet, ImportSet, LibraryName, LibrarySpec, ParseContext,
        SpecialKeyword,
    },
    cps::Compile,
    exceptions::Exception,
    gc::{Gc, Trace},
    proc::{Application, DynamicState, Procedure},
    runtime::Runtime,
    symbols::Symbol,
    syntax::{Identifier, Mark, Syntax},
    value::Value,
};

pub(crate) mod error {
    use crate::exceptions::Message;

    use super::*;

    pub(crate) fn name_bound_multiple_times(name: Symbol) -> Exception {
        Exception::from(Message::new(format!("`{name}` bound multiple times")))
    }
}

#[derive(Trace)]
pub(crate) struct TopLevelEnvironmentInner {
    pub(crate) rt: Runtime,
    pub(crate) kind: TopLevelKind,
    pub(crate) exports: HashMap<Identifier, Export>,
    pub(crate) imports: HashMap<Identifier, Import>,
    pub(crate) state: LibraryState,
    pub(crate) vars: HashMap<Identifier, Gc<RwLock<Value>>>,
    pub(crate) keywords: HashMap<Identifier, Keyword>,
    pub(crate) special_keywords: HashMap<Identifier, SpecialKeyword>,
}

impl TopLevelEnvironmentInner {
    pub(crate) fn new(
        rt: &Runtime,
        kind: TopLevelKind,
        imports: HashMap<Identifier, Import>,
        exports: HashMap<Identifier, Identifier>,
        vars: HashMap<Identifier, Gc<RwLock<Value>>>,
        body: Syntax,
    ) -> Self {
        let exports = exports
            .into_iter()
            .map(|(name, rename)| {
                let origin = imports.get(&name).map(|import| import.origin.clone());
                (name, Export { rename, origin })
            })
            .collect();

        Self {
            rt: rt.clone(),
            kind,
            imports,
            exports,
            state: LibraryState::Unexpanded(body),
            vars,
            keywords: HashMap::default(),
            special_keywords: HashMap::default(),
        }
    }
}

#[derive(Trace, Debug)]
pub enum TopLevelKind {
    /// A Repl is a library that does not have a name.
    Repl,
    /// A library has a name and an (optional) path.
    Libary {
        name: LibraryName,
        path: Option<PathBuf>,
    },
    /// A program has a path
    Program { path: PathBuf },
}

#[derive(Clone, Trace)]
pub(crate) struct Import {
    /// The original name of the identifier before being renamed.
    pub(crate) rename: Identifier,
    pub(crate) origin: TopLevelEnvironment,
}

#[derive(Trace, Clone)]
pub(crate) struct Export {
    pub(crate) rename: Identifier,
    pub(crate) origin: Option<TopLevelEnvironment>,
}

/// A top level environment such as a library, program, or REPL.
#[derive(Trace, Clone)]
pub struct TopLevelEnvironment(pub(crate) Gc<RwLock<TopLevelEnvironmentInner>>);

impl PartialEq for TopLevelEnvironment {
    fn eq(&self, rhs: &Self) -> bool {
        Gc::ptr_eq(&self.0, &rhs.0)
    }
}

impl TopLevelEnvironment {
    pub fn new_repl(rt: &Runtime) -> Self {
        // Repls are given the import keyword, free of charge.
        let inner = TopLevelEnvironmentInner {
            rt: rt.clone(),
            kind: TopLevelKind::Repl,
            exports: HashMap::default(),
            imports: HashMap::default(),
            state: LibraryState::Invoked,
            vars: HashMap::default(),
            keywords: HashMap::default(),
            special_keywords: [(Identifier::new("import"), SpecialKeyword::Import)]
                .into_iter()
                .collect(),
        };
        Self(Gc::new(RwLock::new(inner)))
    }

    pub(crate) fn new_program(rt: &Runtime, path: &Path) -> Self {
        // Programs are given the import keyword, free of charge.
        let inner = TopLevelEnvironmentInner {
            rt: rt.clone(),
            kind: TopLevelKind::Program {
                path: path.to_path_buf(),
            },
            exports: HashMap::default(),
            imports: HashMap::default(),
            state: LibraryState::Invoked,
            vars: HashMap::default(),
            keywords: HashMap::default(),
            special_keywords: [(Identifier::new("import"), SpecialKeyword::Import)]
                .into_iter()
                .collect(),
        };
        Self(Gc::new(RwLock::new(inner)))
    }

    pub(crate) fn get_kind(&self) -> MappedRwLockReadGuard<'_, TopLevelKind> {
        RwLockReadGuard::map(self.0.read(), |inner| &inner.kind)
    }

    pub(crate) fn get_state(&self) -> MappedRwLockReadGuard<'_, LibraryState> {
        RwLockReadGuard::map(self.0.read(), |inner| &inner.state)
    }

    /// Evaluate the scheme expression in the provided environment and return
    /// the values. If `allow_imports` is false, import expressions are
    /// disallowed and will cause an error.
    #[maybe_async]
    pub fn eval(&self, allow_imports: bool, code: &str) -> Result<Vec<Value>, Exception> {
        let sexprs = Syntax::from_str(code, None)?;
        let Some([body @ .., Syntax::Null { .. }]) = sexprs.as_list() else {
            return Err(Exception::syntax(sexprs, None));
        };
        let rt = { self.0.read().rt.clone() };
        let ctxt = ParseContext::new(&rt, allow_imports);
        let body = maybe_await!(DefinitionBody::parse(
            &ctxt,
            body,
            &Environment::Top(self.clone()),
            &sexprs
        ))?;
        let compiled = maybe_await!(rt.compile_expr(body.compile_top_level()));
        maybe_await!(Application::new(compiled, Vec::new()).eval(&mut DynamicState::new()))
    }

    #[maybe_async]
    pub fn eval_sexpr(&self, allow_imports: bool, sexpr: &Syntax) -> Result<Vec<Value>, Exception> {
        let rt = { self.0.read().rt.clone() };
        let ctxt = ParseContext::new(&rt, allow_imports);
        let body = std::slice::from_ref(sexpr);
        let body = maybe_await!(DefinitionBody::parse(
            &ctxt,
            body,
            &Environment::Top(self.clone()),
            sexpr
        ))?;
        let compiled = maybe_await!(rt.compile_expr(body.compile_top_level()));
        maybe_await!(Application::new(compiled, Vec::new()).eval(&mut DynamicState::new()))
    }

    #[maybe_async]
    pub fn from_spec(rt: &Runtime, spec: LibrarySpec, path: PathBuf) -> Result<Self, Exception> {
        maybe_await!(Self::from_spec_inner(rt, spec, path, HashMap::default()))
    }

    #[maybe_async]
    pub(crate) fn from_spec_inner(
        rt: &Runtime,
        spec: LibrarySpec,
        path: PathBuf,
        vars: HashMap<Identifier, Gc<RwLock<Value>>>,
    ) -> Result<Self, Exception> {
        let registry = rt.get_registry();

        // Import libraries:
        let mut imports = HashMap::<Identifier, Import>::default();
        let mut exports = HashMap::<Identifier, Identifier>::default();

        for lib_import in spec.imports.import_sets.into_iter() {
            for (ident, import) in maybe_await!(registry.import(rt, lib_import))? {
                match imports.entry(ident) {
                    Entry::Occupied(prev_imported)
                        if prev_imported.get().origin != import.origin =>
                    {
                        return Err(error::name_bound_multiple_times(prev_imported.key().sym));
                    }
                    Entry::Vacant(slot) => {
                        slot.insert(import);
                    }
                    _ => (),
                }
            }
        }

        for export in spec.exports.export_sets.into_iter() {
            match export {
                ExportSet::Internal { rename, ident } => {
                    let rename = if let Some(rename) = rename {
                        rename
                    } else {
                        ident.clone()
                    };
                    exports.insert(ident, rename);
                }
                ExportSet::External(lib_import) => {
                    for lib_import in lib_import.import_sets.into_iter() {
                        for (ident, import) in maybe_await!(registry.import(rt, lib_import))? {
                            match imports.entry(ident.clone()) {
                                Entry::Occupied(prev_imported)
                                    if prev_imported.get().origin != import.origin =>
                                {
                                    return Err(error::name_bound_multiple_times(ident.sym));
                                }
                                Entry::Vacant(slot) => {
                                    slot.insert(import);
                                }
                                _ => (),
                            }
                            exports.insert(ident.clone(), ident);
                        }
                    }
                }
            }
        }

        Ok(Self(Gc::new(RwLock::new(TopLevelEnvironmentInner::new(
            rt,
            TopLevelKind::Libary {
                name: spec.name,
                path: Some(path),
            },
            imports,
            exports,
            vars,
            spec.body,
        )))))
    }

    #[maybe_async]
    pub fn import(&self, import_set: ImportSet) -> Result<(), Exception> {
        let (rt, registry) = {
            let this = self.0.read();
            (this.rt.clone(), this.rt.get_registry())
        };
        let imports = maybe_await!(registry.import(&rt, import_set))?;
        let mut this = self.0.write();
        for (ident, import) in imports {
            match this.imports.entry(ident) {
                Entry::Occupied(prev_imported) if prev_imported.get().origin != import.origin => {
                    return Err(error::name_bound_multiple_times(
                        prev_imported.get().rename.sym,
                    ));
                }
                Entry::Vacant(slot) => {
                    slot.insert(import);
                }
                _ => (),
            }
        }
        Ok(())
    }

    #[maybe_async]
    pub(crate) fn maybe_expand(&self) -> Result<(), Exception> {
        let body = {
            let mut this = self.0.write();
            if let LibraryState::Unexpanded(body) = &mut this.state {
                // std::mem::take(body)
                body.clone()
            } else {
                return Ok(());
            }
        };
        let rt = { self.0.read().rt.clone() };
        let env = Environment::from(self.clone());
        let expanded = maybe_await!(DefinitionBody::parse_lib_body(&rt, &body, &env))?;
        self.0.write().state = LibraryState::Expanded(expanded);
        Ok(())
    }

    #[maybe_async]
    pub(crate) fn maybe_invoke(&self) -> Result<(), Exception> {
        maybe_await!(self.maybe_expand())?;
        let defn_body = {
            let mut this = self.0.write();
            match std::mem::replace(&mut this.state, LibraryState::Invalid) {
                LibraryState::Expanded(defn_body) => defn_body,
                x => {
                    this.state = x;
                    return Ok(());
                }
            }
        };
        let compiled = defn_body.compile_top_level();
        let rt = { self.0.read().rt.clone() };
        let proc = maybe_await!(rt.compile_expr(compiled));
        let _ = maybe_await!(Application::new(proc, Vec::new()).eval(&mut DynamicState::new()))?;
        self.0.write().state = LibraryState::Invoked;
        Ok(())
    }

    pub fn is_repl(&self) -> bool {
        matches!(self.0.read().kind, TopLevelKind::Repl)
    }

    pub(crate) fn fetch_binding(&self, name: &Identifier) -> Binding {
        let this = self.0.read();
        if let Some(global) = this.vars.get(name) {
            Binding::Global(global.clone())
        } else if let Some(keyword) = this.keywords.get(name) {
            Binding::Keyword(keyword.clone())
        } else if let Some(special_keyword) = this.special_keywords.get(name) {
            Binding::SpecialKeyword(*special_keyword)
        // } else if let Some(Import { origin, rename }) = this.imports.get(name) {
        //     origin.fetch_binding(rename)
        } else {
            Binding::Top(self.clone(), name.clone())
        }
    }

    pub fn def_var(&self, name: Identifier, value: Value) -> Global {
        let mut this = self.0.write();
        let mutable = !this.exports.contains_key(&name);
        match this.vars.entry(name.clone()) {
            Entry::Occupied(occup) => Global::new(name.sym, occup.get().clone(), mutable),
            Entry::Vacant(vacant) => Global::new(
                name.sym,
                vacant.insert(Gc::new(RwLock::new(value))).clone(),
                mutable,
            ),
        }
    }

    pub fn def_keyword(&self, keyword: Identifier, mac: Keyword) {
        let mut this = self.0.write();
        this.keywords.insert(keyword, mac);
    }

    #[cfg(not(feature = "async"))]
    pub fn fetch_var(&self, name: &Identifier) -> Result<Option<Global>, Exception> {
        self.fetch_var_inner(name)
    }

    #[cfg(feature = "async")]
    pub(crate) fn fetch_var<'a>(
        &'a self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Global>, Exception>> {
        Box::pin(self.fetch_var_inner(name))
    }

    #[maybe_async]
    fn fetch_var_inner(&self, name: &Identifier) -> Result<Option<Global>, Exception> {
        let Import { origin, rename } = {
            // Check this library
            let this = self.0.read();
            if let Some(var) = this.vars.get(name) {
                let var = var.clone();
                // Fetching this every time is kind of slow.
                let mutable = !this.exports.contains_key(name);
                return Ok(Some(Global::new(name.sym, var, mutable)));
            }

            // Check our imports
            let Some(import) = this.imports.get(name) else {
                return Ok(None);
            };

            import.clone()
        };

        maybe_await!(origin.maybe_invoke())?;
        maybe_await!(origin.fetch_var(&rename))
    }

    #[cfg(not(feature = "async"))]
    pub fn fetch_keyword(&self, keyword: &Identifier) -> Result<Option<Keyword>, Exception> {
        self.fetch_keyword_inner(keyword)
    }

    #[cfg(feature = "async")]
    pub(crate) fn fetch_keyword<'a>(
        &'a self,
        keyword: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Keyword>, Exception>> {
        Box::pin(self.fetch_keyword_inner(keyword))
    }

    #[maybe_async]
    fn fetch_keyword_inner(&self, keyword: &Identifier) -> Result<Option<Keyword>, Exception> {
        let Import { origin, rename } = {
            // Check this library
            let this = self.0.read();
            if let Some(key) = this.keywords.get(keyword) {
                let key = key.clone();
                return Ok(Some(key));
            }

            // Check our imports
            let Some(import) = this.imports.get(keyword) else {
                return Ok(None);
            };

            import.clone()
        };

        maybe_await!(origin.maybe_invoke())?;
        maybe_await!(origin.fetch_keyword(&rename))
    }

    pub(crate) fn fetch_special_keyword(&self, keyword: &Identifier) -> Option<SpecialKeyword> {
        // Check this library:
        let this = self.0.read();
        if let Some(special_keyword) = this.special_keywords.get(keyword) {
            return Some(*special_keyword);
        }

        // Check our imports:
        let Import { origin, rename } = this.imports.get(keyword)?;

        let rename = rename.clone();
        let import = origin.clone();
        drop(this);
        import.fetch_special_keyword(&rename)
    }
}

// TODO: Use these states to detect circular dependencies when we do our DFS.
// Or, alternatively, just handle circular dependencies like Guile does.
#[derive(Trace, Debug)]
pub(crate) enum LibraryState {
    Invalid,
    BridgesDefined,
    Unexpanded(Syntax),
    Expanded(DefinitionBody),
    Invoked,
}

impl LibraryState {
    pub(crate) fn is_bridges_defined(&self) -> bool {
        matches!(self, LibraryState::BridgesDefined)
    }
}

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
    pub fn fetch_var(&self, name: &Identifier) -> Result<Option<Var>, Exception> {
        if let Some(local) = self.vars.get(name) {
            return Ok(Some(Var::Local(*local)));
        }
        self.up.fetch_var(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Var>, Exception>> {
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
    ) -> Result<Option<Either<SpecialKeyword, Var>>, Exception> {
        if let Some(local) = self.vars.get(name) {
            return Ok(Some(Either::Right(Var::Local(*local))));
        }
        self.up.fetch_special_keyword_or_var(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_special_keyword_or_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Either<SpecialKeyword, Var>>, Exception>> {
        if let Some(local) = self.vars.get(name) {
            let local = *local;
            return Box::pin(async move { Ok(Some(Either::Right(Var::Local(local)))) });
        }
        let up = self.up.clone();
        Box::pin(async move { up.fetch_special_keyword_or_var(name).await })
    }

    /*
    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        if let Some(local) = self.vars.get(name) {
            return Some(*local);
        }
        self.up.fetch_local(name)
    }
    */

    pub fn fetch_top(&self) -> TopLevelEnvironment {
        self.up.fetch_top()
    }
}

#[derive(Clone, Trace)]
pub(crate) struct LexicalContour(Gc<RwLock<LexicalContourInner>>);

impl LexicalContour {
    #[cfg(not(feature = "async"))]
    pub fn fetch_keyword(&self, name: &Identifier) -> Result<Option<Keyword>, Exception> {
        self.clone().fetch_keyword_inner(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_keyword<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Keyword>, Exception>> {
        Box::pin(self.clone().fetch_keyword_inner(name))
    }

    #[maybe_async]
    fn fetch_keyword_inner(self, name: &Identifier) -> Result<Option<Keyword>, Exception> {
        let up = {
            let this = self.0.read();
            if let Some(trans) = this.keywords.get(name) {
                let trans = trans.clone();
                drop(this);
                return Ok(Some(Keyword::new(
                    name.clone(),
                    Environment::LexicalContour(self),
                    trans,
                )));
            }
            this.up.clone()
        };
        maybe_await!(up.fetch_keyword(name))
    }

    #[maybe_async]
    pub fn import(&self, import_set: ImportSet) -> Result<(), Exception> {
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
                    return Err(crate::registry::error::name_bound_multiple_times(
                        prev_imported.key().sym,
                    ));
                }
                Entry::Vacant(slot) => {
                    slot.insert(import);
                }
                _ => (),
            }
        }
        Ok(())
    }

    fn fetch_binding(&self, name: &Identifier) -> Binding {
        let this = self.0.read();
        if let Some(var) = this.vars.get(name) {
            Binding::Local(*var)
        } else if let Some(transformer) = this.keywords.get(name) {
            Binding::Keyword(Keyword::new(
                name.clone(),
                Environment::LexicalContour(self.clone()),
                transformer.clone(),
            ))
        } else {
            this.up.fetch_binding(name)
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
    pub fn fetch_var(&self, name: &Identifier) -> Result<Option<Var>, Exception> {
        self.up.fetch_var(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Var>, Exception>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_var(name).await })
    }

    #[cfg(not(feature = "async"))]
    pub fn fetch_special_keyword_or_var(
        &self,
        name: &Identifier,
    ) -> Result<Option<Either<SpecialKeyword, Var>>, Exception> {
        self.up.fetch_special_keyword_or_var(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_special_keyword_or_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Either<SpecialKeyword, Var>>, Exception>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_special_keyword_or_var(name).await })
    }

    pub fn fetch_top(&self) -> TopLevelEnvironment {
        self.up.fetch_top()
    }

    #[cfg(not(feature = "async"))]
    pub fn import(&self, import_set: ImportSet) -> Result<(), Exception> {
        self.up.import(import_set)
    }

    #[cfg(feature = "async")]
    pub fn import(&self, import_set: ImportSet) -> BoxFuture<'static, Result<(), Exception>> {
        let up = self.up.clone();
        Box::pin(async move { up.import(import_set).await })
    }
}

#[derive(Clone, Trace)]
pub(crate) struct LetSyntaxContour(Gc<RwLock<LetSyntaxContourInner>>);

impl LetSyntaxContour {
    #[cfg(not(feature = "async"))]
    pub fn fetch_keyword(&self, name: &Identifier) -> Result<Option<Keyword>, Exception> {
        self.clone().fetch_keyword_inner(name)
    }

    #[cfg(feature = "async")]
    pub fn fetch_keyword<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Keyword>, Exception>> {
        Box::pin(self.clone().fetch_keyword_inner(name))
    }

    #[maybe_async]
    fn fetch_keyword_inner(self, name: &Identifier) -> Result<Option<Keyword>, Exception> {
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
                return Ok(Some(Keyword::new(name.clone(), env, trans)));
            }
            this.up.clone()
        };
        maybe_await!(up.fetch_keyword(name))
    }

    fn fetch_binding(&self, name: &Identifier) -> Binding {
        let this = self.0.read();
        if let Some(transformer) = this.keywords.get(name) {
            let env = if this.recursive {
                Environment::LetSyntaxContour(self.clone())
            } else {
                this.up.clone()
            };
            Binding::Keyword(Keyword::new(name.clone(), env, transformer.clone()))
        } else {
            this.up.fetch_binding(name)
        }
    }
}

#[derive(Trace)]
pub(crate) struct MacroExpansion {
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
        pub fn $outer(&self, name: &Identifier) -> Result<Option<$ret>, Exception> {
            Self::$inner(&self.up, &self.source, self.mark, name)
        }

        #[cfg(feature = "async")]
        pub fn $outer<'a>(
            &self,
            name: &'a Identifier,
        ) -> BoxFuture<'a, Result<Option<$ret>, Exception>> {
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
        ) -> Result<Option<$ret>, Exception> {
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

    pub fn fetch_pattern_variable(&self, name: &Identifier) -> Option<(Local, usize)> {
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

    pub fn fetch_top(&self) -> TopLevelEnvironment {
        self.up.fetch_top()
    }

    #[cfg(not(feature = "async"))]
    pub fn import(&self, import_set: ImportSet) -> Result<(), Exception> {
        self.up.import(import_set)
    }

    #[cfg(feature = "async")]
    pub fn import(&self, import_set: ImportSet) -> BoxFuture<'static, Result<(), Exception>> {
        let up = self.up.clone();
        Box::pin(async move { up.import(import_set).await })
    }

    pub(crate) fn fetch_binding(&self, name: &Identifier) -> Binding {
        match self.up.fetch_binding(name) {
            Binding::Top(_, _) if name.marks.contains(&self.mark) => {
                let mut unmarked = name.clone();
                unmarked.mark(self.mark);
                self.source.fetch_binding(&unmarked)
            }
            binding => binding,
        }
    }
}

#[derive(Trace)]
pub(crate) struct SyntaxCaseExpr {
    up: Environment,
    expansions_store: Local,
    pattern_vars: HashMap<Identifier, usize>,
}

impl SyntaxCaseExpr {
    fn new(
        env: &Environment,
        expansions_store: Local,
        pattern_vars: HashMap<Identifier, usize>,
    ) -> Self {
        Self {
            up: env.clone(),
            expansions_store,
            pattern_vars,
        }
    }

    fn fetch_top(&self) -> TopLevelEnvironment {
        self.up.fetch_top()
    }

    fn def_var(&self, name: Identifier) -> Var {
        self.up.def_var(name)
    }

    fn def_keyword(&self, name: Identifier, val: Procedure) {
        self.up.def_keyword(name, val);
    }

    #[cfg(not(feature = "async"))]
    fn fetch_var(&self, name: &Identifier) -> Result<Option<Var>, Exception> {
        self.up.fetch_var(name)
    }

    #[cfg(feature = "async")]
    fn fetch_var<'a>(&self, name: &'a Identifier) -> BoxFuture<'a, Result<Option<Var>, Exception>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_var(name).await })
    }

    #[cfg(not(feature = "async"))]
    fn fetch_keyword(&self, name: &Identifier) -> Result<Option<Keyword>, Exception> {
        self.up.fetch_keyword(name)
    }

    #[cfg(feature = "async")]
    fn fetch_keyword<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Keyword>, Exception>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_keyword(name).await })
    }

    #[cfg(not(feature = "async"))]
    fn fetch_special_keyword_or_var(
        &self,
        name: &Identifier,
    ) -> Result<Option<Either<SpecialKeyword, Var>>, Exception> {
        self.up.fetch_special_keyword_or_var(name)
    }

    #[cfg(feature = "async")]
    fn fetch_special_keyword_or_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Either<SpecialKeyword, Var>>, Exception>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_special_keyword_or_var(name).await })
    }

    #[cfg(not(feature = "async"))]
    fn import(&self, import: ImportSet) -> Result<(), Exception> {
        self.up.import(import)
    }

    #[cfg(feature = "async")]
    fn import(&self, import: ImportSet) -> BoxFuture<'static, Result<(), Exception>> {
        let up = self.up.clone();
        Box::pin(async move { up.import(import).await })
    }

    fn fetch_pattern_variable(&self, name: &Identifier) -> Option<(Local, usize)> {
        if let Some(nesting) = self.pattern_vars.get(name) {
            Some((self.expansions_store, *nesting))
        } else {
            self.up.fetch_pattern_variable(name)
        }
    }

    fn fetch_binding(&self, name: &Identifier) -> Binding {
        self.up.fetch_binding(name)
    }
}

#[derive(Trace)]
pub(crate) enum Environment {
    Top(TopLevelEnvironment),
    LexicalContour(LexicalContour),
    LetSyntaxContour(LetSyntaxContour),
    MacroExpansion(Gc<RwLock<MacroExpansion>>),
    SyntaxCaseExpr(Gc<RwLock<SyntaxCaseExpr>>),
}

impl Environment {
    pub fn fetch_top(&self) -> TopLevelEnvironment {
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
            Self::Top(top) => top.def_keyword(name.clone(), Keyword::new(name, self.clone(), val)),
            Self::LexicalContour(lex) => lex.0.write().def_keyword(name, val),
            Self::LetSyntaxContour(ls) => ls.0.write().def_keyword(name, val),
            Self::MacroExpansion(me) => me.read().def_keyword(name, val),
            Self::SyntaxCaseExpr(me) => me.read().def_keyword(name, val),
        }
    }

    #[maybe_async]
    pub fn fetch_var(&self, name: &Identifier) -> Result<Option<Var>, Exception> {
        let fetch_result = match self {
            Self::Top(top) => return Ok(maybe_await!(top.fetch_var(name))?.map(Var::Global)),
            Self::LexicalContour(lex) => lex.0.read().fetch_var(name),
            Self::LetSyntaxContour(ls) => ls.0.read().fetch_var(name),
            Self::MacroExpansion(me) => me.read().fetch_var(name),
            Self::SyntaxCaseExpr(sc) => sc.read().fetch_var(name),
        };
        maybe_await!(fetch_result)
    }

    #[maybe_async]
    pub fn fetch_keyword(&self, name: &Identifier) -> Result<Option<Keyword>, Exception> {
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
    ) -> Result<Option<Either<SpecialKeyword, Var>>, Exception> {
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
    pub fn import(&self, import: ImportSet) -> Result<(), Exception> {
        let import_result = match self {
            Self::Top(top) => {
                return maybe_await!(top.import(import));
            }
            Self::LexicalContour(lex) => {
                return maybe_await!(lex.import(import));
            }
            Self::LetSyntaxContour(ls) => ls.0.read().import(import),
            Self::MacroExpansion(me) => me.read().import(import),
            Self::SyntaxCaseExpr(sc) => sc.read().import(import),
        };
        maybe_await!(import_result)
    }

    pub fn fetch_pattern_variable(&self, name: &Identifier) -> Option<(Local, usize)> {
        match self {
            Self::Top(_) => None,
            Self::LexicalContour(lex) => lex.0.read().up.fetch_pattern_variable(name),
            Self::LetSyntaxContour(ls) => ls.0.read().up.fetch_pattern_variable(name),
            Self::MacroExpansion(me) => me.read().fetch_pattern_variable(name),
            Self::SyntaxCaseExpr(sc) => sc.read().fetch_pattern_variable(name),
        }
    }

    /// Return the binding of the identifier
    pub(crate) fn fetch_binding(&self, name: &Identifier) -> Binding {
        match self {
            Self::Top(top) => top.fetch_binding(name),
            Self::LexicalContour(lex) => lex.fetch_binding(name),
            Self::LetSyntaxContour(ls) => ls.fetch_binding(name),
            Self::MacroExpansion(me) => me.read().fetch_binding(name),
            // I don't think this is technically correct; it should probably
            // return the syntax case expr env if the binding variable is hit
            Self::SyntaxCaseExpr(sc) => sc.read().fetch_binding(name),
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
        pattern_vars: HashMap<Identifier, usize>,
    ) -> Self {
        let syntax_case_expr = SyntaxCaseExpr::new(self, expansions_store, pattern_vars);
        Self::SyntaxCaseExpr(Gc::new(RwLock::new(syntax_case_expr)))
    }
}

impl From<TopLevelEnvironment> for Environment {
    fn from(top: TopLevelEnvironment) -> Self {
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

/// A local variable.
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
    pub(crate) fn gensym() -> Self {
        static NEXT_SYM: AtomicUsize = AtomicUsize::new(0);
        Self {
            id: NEXT_SYM.fetch_add(1, Ordering::Relaxed),
            name: None,
        }
    }

    pub(crate) fn gensym_with_name(name: Symbol) -> Self {
        let mut sym = Self::gensym();
        sym.name = Some(name);
        sym
    }

    pub(crate) fn get_func_name(&self) -> String {
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
/// A global variable, i.e. a variable that is present in a top level
/// environment.
#[derive(Clone, Trace)]
pub struct Global {
    pub(crate) name: Symbol,
    pub(crate) val: Gc<RwLock<Value>>,
    pub(crate) mutable: bool,
}

impl Global {
    pub(crate) fn new(name: Symbol, val: Gc<RwLock<Value>>, mutable: bool) -> Self {
        Global { name, val, mutable }
    }

    pub(crate) fn value_ref(&self) -> &Gc<RwLock<Value>> {
        &self.val
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn read(&self) -> Value {
        self.val.read().clone()
    }

    pub fn set(&self, new: Value) -> Result<(), Exception> {
        if !self.mutable {
            return Err(Exception::error("cannot modify immutable variable"));
        }
        *self.val.write() = new;
        Ok(())
    }
}

impl fmt::Debug for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.name)
    }
}

impl PartialEq for Global {
    fn eq(&self, rhs: &Self) -> bool {
        Gc::ptr_eq(&self.val, &rhs.val)
        /* self.name == rhs.name && */
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
pub(crate) enum Var {
    Global(Global),
    Local(Local),
}

impl Var {
    pub fn symbol(&self) -> Option<Symbol> {
        match self {
            Var::Global(global) => Some(global.name),
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

/// A keyword, i.e. a transformer defined via `define-syntax`.
#[derive(Clone, Trace)]
pub struct Keyword {
    pub name: Identifier,
    pub(crate) source_env: Environment,
    pub transformer: Procedure,
}

impl Keyword {
    pub(crate) fn new(name: Identifier, source_env: Environment, transformer: Procedure) -> Self {
        Self {
            name,
            source_env,
            transformer,
        }
    }
}

impl PartialEq for Keyword {
    fn eq(&self, rhs: &Self) -> bool {
        self.name == rhs.name && self.source_env == rhs.source_env
    }
}

/*
pub(crate) enum TopLevelBinding {
    Global(Gc<RwLock<Value>>),
    Keyword(Keyword),
    SpecialKeyword(SpecialKeyword),
}
*/

#[derive(Clone, Trace)]
pub enum Binding {
    Local(Local),
    Global(Gc<RwLock<Value>>),
    Keyword(Keyword),
    SpecialKeyword(SpecialKeyword),
    Top(TopLevelEnvironment, Identifier),
}

impl fmt::Debug for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Local(local) => write!(f, "%local<{local:?}>"),
            Self::Global(global) => write!(f, "%global<{:p}>", global.ptr),
            Self::Keyword(keyword) => write!(f, "%keyword<{}>", keyword.name.sym),
            Self::SpecialKeyword(special_keyword) => write!(f, "%keyword<{special_keyword:?}>"),
            Self::Top(_, name) => write!(f, "%top<{name:?}>"),
        }
    }
}

impl Binding {
    pub fn resolve<'a>(&'a self) -> Cow<'a, Binding> {
        match self {
            Self::Top(top, name) => match top.fetch_binding(name) {
                Binding::Top(mut top, mut name) => {
                    while let Some(Import { origin, rename }) =
                        { top.0.read().imports.get(&name).cloned() }
                    {
                        let binding = origin.fetch_binding(&rename);
                        // let resolved = binding.resolve(&rename);
                        match binding {
                            Binding::Top(new_top, new_name) => {
                                top = new_top;
                                name = new_name;
                            }
                            _ => return Cow::Owned(binding),
                        }
                    }
                    Cow::Owned(Binding::Top(top, name))
                }
                resolved => Cow::Owned(resolved),
            },
            _ => Cow::Borrowed(self),
        }
    }
}

/*
impl UnresolvedBinding {
    pub fn resolve(&self, ident: &Identifier) -> Option<ResolvedBinding> {
        match self {
            Self::Local(local) => Some(ResolvedBinding::Local(local)),
            Self::Global(global) => Some(ResolvedBinding::Global(global.clone())),
            Self::Keyword(kw) => Some(ResolvedBinding::Keyword(kw.clone())),
            Self::SpecialKeyword(kw) => Some(ResolvedBinding::SpecialKeyword(*kw)),
            Self::Top(top) => {
                let this = self.0.read();
                if let Some(global) = this.vars.get(name) {
                    UnresolvedBinding::Global(global.clone())
                } else if let Some(keyword) = this.keywords.get(name) {
                    UnresolvedBinding::Keyword(keyword.clone())
                } else if let Some(special_keyword) = this.special_keywords.get(name) {
                    UnresolvedBinding::SpecialKeyword(*special_keyword)
                } else if let Some(Import { origin, rename }) = this.imports.get(name) {
                    origin.fetch_binding(rename)
                } else {
                    None
                }
            }
        }
    }
}

pub enum ResolvedBinding {
    Local(Local),
    Global(Gc<RwLock<Value>>),
    Keyword(Keyword),
    SpecialKeyword(SpecialKeyword),
}
*/
