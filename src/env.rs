//! Scheme lexical environments.

use std::{
    collections::{BTreeSet, HashMap, hash_map::Entry},
    fmt,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    sync::{
        LazyLock,
        atomic::{AtomicUsize, Ordering},
    },
};

use parking_lot::{MappedRwLockReadGuard, Mutex, RwLock, RwLockReadGuard};
use scheme_rs_macros::{maybe_async, maybe_await};

#[cfg(feature = "async")]
use futures::future::BoxFuture;

use crate::{
    ast::{
        DefinitionBody, ExportSet, ImportSet, LibraryName, LibrarySpec, ParseContext, Primitive,
    },
    cps::Compile,
    exceptions::Exception,
    gc::{Gc, Trace},
    proc::{Application, DynamicState, Procedure},
    runtime::Runtime,
    symbols::Symbol,
    syntax::{Identifier, Syntax},
    value::{Cell, Value},
};

pub(crate) mod error {
    use crate::exceptions::Message;

    use super::*;

    pub(crate) fn name_bound_multiple_times(name: Symbol) -> Exception {
        Exception::from(Message::new(format!("`{name}` bound multiple times")))
    }
}

#[derive(Clone)]
pub(crate) enum TopLevelBinding {
    Global(Global),
    Keyword(Procedure),
    Primitive(Primitive),
}

pub(crate) static TOP_LEVEL_BINDINGS: LazyLock<Mutex<HashMap<Binding, TopLevelBinding>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

#[derive(Trace)]
pub(crate) struct TopLevelEnvironmentInner {
    pub(crate) rt: Runtime,
    pub(crate) kind: TopLevelKind,
    pub(crate) imports: HashMap<Binding, TopLevelEnvironment>,
    pub(crate) exports: HashMap<Symbol, Export>,
    pub(crate) state: LibraryState,
    pub(crate) scope: Scope,
}

impl TopLevelEnvironmentInner {
    pub(crate) fn new(
        rt: &Runtime,
        kind: TopLevelKind,
        imports: HashMap<Binding, TopLevelEnvironment>,
        exports: HashMap<Symbol, Symbol>,
        scope: Scope,
        body: Syntax,
    ) -> Self {
        let exports = exports
            .into_iter()
            .map(|(name, rename)| {
                let binding = Identifier::from_symbol(name, scope).bind();
                let origin = imports.get(&binding).cloned();
                (rename, Export { binding, origin })
            })
            .collect();

        Self {
            rt: rt.clone(),
            kind,
            imports,
            exports,
            state: LibraryState::Unexpanded(body),
            scope,
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
    /// The original binding of the identifier before being renamed.
    pub(crate) binding: Binding,
    pub(crate) origin: TopLevelEnvironment,
}

#[derive(Trace, Clone)]
pub(crate) struct Export {
    pub(crate) binding: Binding,
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
        let inner = TopLevelEnvironmentInner {
            rt: rt.clone(),
            kind: TopLevelKind::Repl,
            exports: HashMap::new(),
            imports: HashMap::new(),
            state: LibraryState::Invoked,
            scope: Scope::new(),
        };
        let repl = Self(Gc::new(RwLock::new(inner)));
        // Repls are given the import keyword, free of charge.
        repl.import("(only (rnrs base primitives) import)".parse().unwrap())
            .unwrap();
        repl
    }

    pub(crate) fn new_program(rt: &Runtime, path: &Path) -> Self {
        let inner = TopLevelEnvironmentInner {
            rt: rt.clone(),
            kind: TopLevelKind::Program {
                path: path.to_path_buf(),
            },
            exports: HashMap::new(),
            imports: HashMap::new(),
            state: LibraryState::Invoked,
            scope: Scope::new(),
        };
        let program = Self(Gc::new(RwLock::new(inner)));
        // Programs are given the import keyword, free of charge.
        program
            .import("(only (rnrs base primitives) import)".parse().unwrap())
            .unwrap();
        program
    }

    pub(crate) fn scope(&self) -> Scope {
        self.0.read().scope
    }

    #[maybe_async]
    pub fn from_spec(rt: &Runtime, spec: LibrarySpec, path: PathBuf) -> Result<Self, Exception> {
        maybe_await!(Self::from_spec_with_scope(rt, spec, path, Scope::new()))
    }

    #[maybe_async]
    pub(crate) fn from_spec_with_scope(
        rt: &Runtime,
        spec: LibrarySpec,
        path: PathBuf,
        library_scope: Scope,
    ) -> Result<Self, Exception> {
        let registry = rt.get_registry();

        // Import libraries:
        let mut bound_names = HashMap::<Symbol, Binding>::new();
        let mut imports = HashMap::<Binding, TopLevelEnvironment>::new();

        for lib_import in spec.imports.import_sets.into_iter() {
            for (name, import) in maybe_await!(registry.import(rt, lib_import))? {
                if let Some(prev_binding) = bound_names.get(&name)
                    && prev_binding != &import.binding
                {
                    return Err(error::name_bound_multiple_times(name));
                }
                bound_names.insert(name, import.binding);
                imports.insert(import.binding, import.origin);
                // Bind the new identifier in the global symbol table:
                add_binding(Identifier::from_symbol(name, library_scope), import.binding);
            }
        }

        let mut exports = HashMap::new();
        for export in spec.exports.export_sets.into_iter() {
            match export {
                ExportSet::Internal { rename, name } => {
                    let rename = if let Some(rename) = rename {
                        rename
                    } else {
                        name
                    };
                    exports.insert(name, rename);
                }
                ExportSet::External(lib_import) => {
                    for lib_import in lib_import.import_sets.into_iter() {
                        for (name, import) in maybe_await!(registry.import(rt, lib_import))? {
                            if let Some(prev_binding) = bound_names.get(&name)
                                && prev_binding != &import.binding
                            {
                                return Err(error::name_bound_multiple_times(name));
                            }
                            bound_names.insert(name, import.binding);
                            imports.insert(import.binding, import.origin);
                            // Bind the new identifier in the global symbol table:
                            add_binding(
                                Identifier::from_symbol(name, library_scope),
                                import.binding,
                            );
                            exports.insert(name, name);
                        }
                    }
                }
            }
        }

        let mut body = spec.body;
        body.add_scope(library_scope);

        Ok(Self(Gc::new(RwLock::new(TopLevelEnvironmentInner::new(
            rt,
            TopLevelKind::Libary {
                name: spec.name,
                path: Some(path),
            },
            imports,
            exports,
            library_scope,
            body,
        )))))
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
        let mut sexprs = Syntax::from_str(code, None)?;
        sexprs.add_scope(self.0.read().scope);
        let Some([body @ .., end]) = sexprs.as_list() else {
            return Err(Exception::syntax(sexprs, None));
        };
        if !end.is_null() {
            return Err(Exception::syntax(sexprs, None));
        }
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
    pub fn eval_sexpr(
        &self,
        allow_imports: bool,
        mut sexpr: Syntax,
    ) -> Result<Vec<Value>, Exception> {
        let rt = { self.0.read().rt.clone() };
        let ctxt = ParseContext::new(&rt, allow_imports);
        sexpr.add_scope(self.0.read().scope);
        let body = std::slice::from_ref(&sexpr);
        let body = maybe_await!(DefinitionBody::parse(
            &ctxt,
            body,
            &Environment::Top(self.clone()),
            &sexpr
        ))?;
        let compiled = maybe_await!(rt.compile_expr(body.compile_top_level()));
        maybe_await!(Application::new(compiled, Vec::new()).eval(&mut DynamicState::new()))
    }

    #[maybe_async]
    pub fn import(&self, import_set: ImportSet) -> Result<(), Exception> {
        let (rt, registry, scope) = {
            let this = self.0.read();
            (this.rt.clone(), this.rt.get_registry(), this.scope)
        };
        let imports = maybe_await!(registry.import(&rt, import_set))?;
        let mut this = self.0.write();
        for (sym, import) in imports {
            match this.imports.entry(import.binding) {
                Entry::Occupied(prev_imported) if *prev_imported.key() != import.binding => {
                    return Err(error::name_bound_multiple_times(sym));
                }
                Entry::Vacant(slot) => {
                    add_binding(Identifier::from_symbol(sym, scope), import.binding);
                    slot.insert(import.origin);
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

    pub fn def_var(&self, binding: Binding, name: Symbol, value: Value) -> Global {
        let mutable = self
            .0
            .read()
            .exports
            .get(&name)
            .map(|export| export.binding)
            != Some(binding);
        let mut top_level_binds = TOP_LEVEL_BINDINGS.lock();
        match top_level_binds.entry(binding) {
            Entry::Occupied(occup) => {
                if let TopLevelBinding::Global(global) = occup.get().clone() {
                    global
                } else {
                    unreachable!()
                }
            }
            Entry::Vacant(vacant) => {
                let global = Global::new(name, Cell::new(value), mutable, self.clone());
                vacant.insert(TopLevelBinding::Global(global.clone()));
                global
            }
        }
    }

    #[cfg(not(feature = "async"))]
    pub fn lookup_var(&self, binding: Binding) -> Result<Option<Global>, Exception> {
        self.lookup_var_inner(binding)
    }

    #[cfg(feature = "async")]
    pub(crate) fn lookup_var<'a>(
        &self,
        binding: Binding,
    ) -> BoxFuture<'_, Result<Option<Global>, Exception>> {
        Box::pin(self.lookup_var_inner(binding))
    }

    #[maybe_async]
    pub fn lookup_var_inner(&self, binding: Binding) -> Result<Option<Global>, Exception> {
        if let Some(TopLevelBinding::Global(global)) =
            { TOP_LEVEL_BINDINGS.lock().get(&binding).cloned() }
        {
            if *self != global.origin {
                maybe_await!(global.origin.maybe_invoke())?;
            }
            Ok(Some(global.clone()))
        } else {
            Ok(None)
        }
    }

    pub fn def_keyword(&self, binding: Binding, transformer: Procedure) {
        TOP_LEVEL_BINDINGS
            .lock()
            .insert(binding, TopLevelBinding::Keyword(transformer));
    }

    #[cfg(not(feature = "async"))]
    pub fn lookup_keyword(&self, binding: Binding) -> Result<Option<Procedure>, Exception> {
        self.lookup_keyword_inner(binding)
    }

    #[cfg(feature = "async")]
    pub(crate) fn lookup_keyword(
        &self,
        binding: Binding,
    ) -> BoxFuture<'_, Result<Option<Procedure>, Exception>> {
        Box::pin(self.lookup_keyword_inner(binding))
    }

    #[maybe_async]
    pub fn lookup_keyword_inner(&self, binding: Binding) -> Result<Option<Procedure>, Exception> {
        if let Some(origin) = self.0.read().imports.get(&binding).cloned() {
            maybe_await!(origin.maybe_expand())?;
            maybe_await!(origin.lookup_keyword(binding))
        } else if let Some(TopLevelBinding::Keyword(kw)) = TOP_LEVEL_BINDINGS.lock().get(&binding) {
            Ok(Some(kw.clone()))
        } else {
            Ok(None)
        }
    }

    pub fn lookup_primitive(&self, binding: Binding) -> Option<Primitive> {
        if let Some(TopLevelBinding::Primitive(primitive)) = TOP_LEVEL_BINDINGS.lock().get(&binding)
        {
            Some(*primitive)
        } else {
            None
        }
    }
}

impl fmt::Debug for TopLevelEnvironment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%top")
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

#[derive(Trace, PartialEq, Debug)]
enum BindingType {
    Var(Local),
    Keyword(Procedure),
    Pattern(Local, usize),
    Imported(TopLevelEnvironment),
}

#[derive(Trace, Debug)]
struct LexicalContour {
    up: Environment,
    bindings: Mutex<HashMap<Binding, BindingType>>,
    scope: Scope,
}

impl LexicalContour {
    fn def_var(&self, binding: Binding, name: Symbol) -> Local {
        let local = Local::gensym_with_name(name);
        self.bindings
            .lock()
            .insert(binding, BindingType::Var(local));
        local
    }

    fn def_keyword(&self, binding: Binding, transformer: Procedure) {
        self.bindings
            .lock()
            .insert(binding, BindingType::Keyword(transformer));
    }

    #[maybe_async]
    fn lookup_keyword(&self, binding: Binding) -> Result<Option<Procedure>, Exception> {
        if let Some(bound) = self.bindings.lock().get(&binding) {
            match bound {
                BindingType::Keyword(transformer) => Ok(Some(transformer.clone())),
                BindingType::Imported(imported) => maybe_await!(imported.lookup_keyword(binding)),
                _ => Ok(None),
            }
        } else {
            maybe_await!(self.up.lookup_keyword(binding))
        }
    }

    #[maybe_async]
    fn lookup_var(&self, binding: Binding) -> Result<Option<Var>, Exception> {
        if let Some(bound) = self.bindings.lock().get(&binding) {
            match bound {
                BindingType::Var(local) => Ok(Some(Var::Local(*local))),
                BindingType::Imported(imported) => {
                    Ok(maybe_await!(imported.lookup_var(binding))?.map(Var::Global))
                }
                _ => Ok(None),
            }
        } else {
            maybe_await!(self.up.lookup_var(binding))
        }
    }

    fn lookup_primitive(&self, binding: Binding) -> Option<Primitive> {
        if let Some(bound) = self.bindings.lock().get(&binding) {
            match bound {
                BindingType::Imported(imported) => imported.lookup_primitive(binding),
                _ => None,
            }
        } else {
            self.up.lookup_primitive(binding)
        }
    }

    fn lookup_pattern_variable(&self, binding: Binding) -> Option<(Local, usize)> {
        if let Some(bound) = self.bindings.lock().get(&binding) {
            match bound {
                BindingType::Pattern(local, depth) => Some((*local, *depth)),
                _ => None,
            }
        } else {
            self.up.lookup_pattern_variable(binding)
        }
    }

    pub fn fetch_top(&self) -> TopLevelEnvironment {
        self.up.fetch_top()
    }

    #[maybe_async]
    pub fn import(&self, _import_set: ImportSet) -> Result<(), Exception> {
        /*
        let (rt, registry) = {
            let top = self.fetch_top();
            let top = top.0.read();
            (top.rt.clone(), top.rt.get_registry())
        };
        let imports = maybe_await!(registry.import(&rt, import_set))?;
        let mut bindings = self.bindings.lock();
        for (sym, import) in imports {
            let binding_type = BindingType::Imported(import.origin.clone());
            match bindings.entry(import.binding) {
                Entry::Occupied(prev_imported) if *prev_imported.get() != binding_type => {
                    return Err(error::name_bound_multiple_times(sym));
                }
                Entry::Vacant(slot) => {
                    slot.insert(binding_type);
                }
                _ => (),
            }
        }
        Ok(())
         */
        todo!()
    }
}

/// A lexical contour
#[derive(Clone, Trace, Debug)]
pub(crate) enum Environment {
    Top(TopLevelEnvironment),
    LexicalContour(Gc<LexicalContour>),
}

impl Environment {
    pub fn new_lexical_contour(&self, scope: Scope) -> Self {
        Self::LexicalContour(Gc::new(LexicalContour {
            up: self.clone(),
            bindings: Mutex::new(HashMap::new()),
            scope,
        }))
    }

    pub fn new_syntax_case_contour(
        &self,
        scope: Scope,
        expansion: Local,
        vars: HashMap<Binding, usize>,
    ) -> Self {
        Self::LexicalContour(Gc::new(LexicalContour {
            up: self.clone(),
            bindings: Mutex::new(
                vars.into_iter()
                    .map(|(binding, depth)| (binding, BindingType::Pattern(expansion, depth)))
                    .collect(),
            ),
            scope,
        }))
    }

    pub fn def_var(&self, binding: Binding, name: Symbol) -> Var {
        match self {
            Self::Top(top) => Var::Global(top.def_var(binding, name, Value::undefined())),
            Self::LexicalContour(lc) => Var::Local(lc.def_var(binding, name)),
        }
    }

    pub fn def_keyword(&self, binding: Binding, transformer: Procedure) {
        match self {
            Self::Top(top) => top.def_keyword(binding, transformer),
            Self::LexicalContour(lc) => lc.def_keyword(binding, transformer),
        }
    }

    #[cfg(not(feature = "async"))]
    pub fn lookup_keyword(&self, binding: Binding) -> Result<Option<Procedure>, Exception> {
        match self {
            Self::Top(top) => top.lookup_keyword(binding),
            Self::LexicalContour(lc) => lc.lookup_keyword(binding),
        }
    }

    #[cfg(feature = "async")]
    pub fn lookup_keyword(
        &self,
        binding: Binding,
    ) -> BoxFuture<'_, Result<Option<Procedure>, Exception>> {
        Box::pin(async move {
            match self {
                Self::Top(top) => top.lookup_keyword(binding).await,
                Self::LexicalContour(lc) => lc.lookup_keyword(binding).await,
            }
        })
    }

    #[cfg(not(feature = "async"))]
    pub fn lookup_var(&self, binding: Binding) -> Result<Option<Var>, Exception> {
        match self {
            Self::Top(top) => Ok(top.lookup_var(binding)?.map(Var::Global)),
            Self::LexicalContour(lc) => lc.lookup_var(binding),
        }
    }

    #[cfg(feature = "async")]
    pub fn lookup_var(&self, binding: Binding) -> BoxFuture<'_, Result<Option<Var>, Exception>> {
        Box::pin(async move {
            match self {
                Self::Top(top) => top.lookup_var(binding).await,
                Self::LexicalContour(lc) => lc.lookup_bar(binding).await,
            }
        })
    }

    pub fn get_scope_set(&self) -> BTreeSet<Scope> {
        match self {
            Self::Top(top) => BTreeSet::from([top.scope()]),
            Self::LexicalContour(lc) => {
                let mut up_scopes = lc.up.get_scope_set();
                up_scopes.insert(lc.scope);
                up_scopes
            }
        }
    }

    pub fn lookup_primitive(&self, binding: Binding) -> Option<Primitive> {
        match self {
            Self::Top(top) => top.lookup_primitive(binding),
            Self::LexicalContour(lc) => lc.lookup_primitive(binding),
        }
    }

    pub fn lookup_pattern_variable(&self, binding: Binding) -> Option<(Local, usize)> {
        match self {
            Self::Top(_) => None,
            Self::LexicalContour(lc) => lc.lookup_pattern_variable(binding),
        }
    }

    pub fn fetch_top(&self) -> TopLevelEnvironment {
        match self {
            Self::Top(top) => top.clone(),
            Self::LexicalContour(lc) => lc.fetch_top(),
        }
    }

    #[maybe_async]
    pub fn import(&self, import_set: ImportSet) -> Result<(), Exception> {
        match self {
            Self::Top(top) => maybe_await!(top.import(import_set)),
            Self::LexicalContour(lc) => maybe_await!(lc.import(import_set)),
        }
    }
}

impl From<TopLevelEnvironment> for Environment {
    fn from(env: TopLevelEnvironment) -> Self {
        Self::Top(env)
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
            write!(f, "{name}:${}", self.id)
        } else {
            write!(f, "%{}", self.id)
        }
    }
}

impl fmt::Debug for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.name {
            write!(f, "{name}:${}", self.id)
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
    pub(crate) val: Cell,
    pub(crate) mutable: bool,
    pub(crate) origin: TopLevelEnvironment,
}

impl Global {
    pub(crate) fn new(name: Symbol, val: Cell, mutable: bool, origin: TopLevelEnvironment) -> Self {
        Global {
            name,
            val,
            mutable,
            origin,
        }
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn read(&self) -> Value {
        self.val.0.read().clone()
    }

    pub fn set(&self, new: Value) -> Result<(), Exception> {
        if !self.mutable {
            return Err(Exception::error("cannot modify immutable variable"));
        }
        *self.val.0.write() = new;
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
        Gc::ptr_eq(&self.val.0, &rhs.val.0)
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
        Gc::as_ptr(&self.val.0).hash(state);
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

    pub fn as_local(&self) -> Option<Local> {
        match self {
            Self::Global(_) => None,
            Self::Local(local) => Some(*local),
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

/// A scope uniquely identifies a lexical contour and is used for binding
/// resolution.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Trace)]
pub struct Scope(usize);

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl Scope {
    /// Generates a new unique scope.
    pub fn new() -> Self {
        static NEXT_SCOPE: AtomicUsize = AtomicUsize::new(0);

        Self(NEXT_SCOPE.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Trace)]
pub struct Binding(usize);

impl Binding {
    pub fn new() -> Self {
        static NEXT_LOC: AtomicUsize = AtomicUsize::new(0);

        Self(NEXT_LOC.fetch_add(1, Ordering::Relaxed))
    }
}

impl fmt::Debug for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "!{}", self.0)
    }
}

// We can probably design this much more intelligently
pub(crate) static GLOBAL_BINDING_TABLE: LazyLock<
    Mutex<HashMap<Symbol, Vec<(BTreeSet<Scope>, Binding)>>>,
> = LazyLock::new(|| Mutex::new(HashMap::new()));

pub(crate) fn add_binding(id: Identifier, binding: Binding) {
    GLOBAL_BINDING_TABLE
        .lock()
        .entry(id.sym)
        .or_default()
        .push((id.scopes, binding));
}

pub(crate) fn resolve(id: &Identifier) -> Option<Binding> {
    let candidate_ids = find_all_matching_bindings(id);
    let Some(max_id) = candidate_ids
        .iter()
        .max_by(|a, b| a.0.len().cmp(&b.0.len()))
    else {
        return None;
    };
    if is_ambiguous(&max_id.0, &candidate_ids) {
        return None;
    }
    Some(max_id.1)
}

fn is_ambiguous(max_id: &BTreeSet<Scope>, candidates: &[(BTreeSet<Scope>, Binding)]) -> bool {
    for candidate in candidates {
        if !candidate.0.is_subset(max_id) {
            return true;
        }
    }
    false
}

fn find_all_matching_bindings(id: &Identifier) -> Vec<(BTreeSet<Scope>, Binding)> {
    GLOBAL_BINDING_TABLE
        .lock()
        .get(&id.sym)
        .map_or_else(Vec::new, |vec| {
            vec.iter()
                .filter(|(scopes, _)| scopes.is_subset(&id.scopes))
                .cloned()
                .collect()
        })
}
