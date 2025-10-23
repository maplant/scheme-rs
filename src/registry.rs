//! A Registry is a collection of libraries.

use crate::{
    ast::{
        DefinitionBody, ExportSet, ImportSet, LibraryName, LibrarySpec, ParseAstError,
        SpecialKeyword, Version,
    },
    cps::Compile,
    env::{Environment, Global, Keyword},
    exceptions::{Condition, ExceptionHandler},
    gc::{Gc, Trace},
    proc::{
        Application, AsyncBridgePtr, DynamicWind, FuncDebugInfo, FuncPtr, Procedure, SyncBridgePtr,
    },
    runtime::Runtime,
    symbols::Symbol,
    syntax::{Identifier, Syntax},
    value::Value,
};
use std::{
    collections::{HashMap, HashSet, hash_map::Entry},
    path::{Path, PathBuf},
    sync::Arc,
};

pub use scheme_rs_macros::{bridge, cps_bridge};

pub enum BridgePtr {
    Sync(SyncBridgePtr),
    Async(AsyncBridgePtr),
}

pub struct BridgeFn {
    name: &'static str,
    lib_name: &'static str,
    num_args: usize,
    variadic: bool,
    wrapper: BridgePtr,
    debug_info: BridgeFnDebugInfo,
}

impl BridgeFn {
    pub const fn new(
        name: &'static str,
        lib_name: &'static str,
        num_args: usize,
        variadic: bool,
        wrapper: BridgePtr,
        debug_info: BridgeFnDebugInfo,
    ) -> Self {
        Self {
            name,
            lib_name,
            num_args,
            variadic,
            wrapper,
            debug_info,
        }
    }
}

#[derive(Copy, Clone)]
pub struct BridgeFnDebugInfo {
    pub(crate) file: &'static str,
    pub(crate) line: u32,
    pub(crate) column: u32,
    pub(crate) offset: usize,
    pub(crate) args: &'static [&'static str],
}

impl BridgeFnDebugInfo {
    pub const fn new(
        file: &'static str,
        line: u32,
        column: u32,
        offset: usize,
        args: &'static [&'static str],
    ) -> Self {
        Self {
            file,
            line,
            column,
            offset,
            args,
        }
    }
}

inventory::collect!(BridgeFn);

#[derive(rust_embed::Embed)]
#[folder = "scheme"]
pub struct Stdlib;

#[derive(Trace, Default, Debug)]
pub(crate) struct RegistryInner {
    libs: HashMap<Vec<Symbol>, Library>,
    loading: HashSet<Vec<Symbol>>,
}

impl RegistryInner {
    /// Construct an empty registry
    pub fn empty() -> Self {
        Self::default()
    }

    /// Construct a Registry with all of the available bridge functions and special keywords.
    pub fn new(rt: &Runtime) -> Self {
        struct Lib {
            version: Version,
            syms: HashMap<Identifier, Gc<Value>>,
        }
        let mut libs = HashMap::<Vec<Symbol>, Lib>::default();

        // Import the bridge functions:
        for bridge_fn in inventory::iter::<BridgeFn>() {
            let debug_info = Arc::new(FuncDebugInfo::from_bridge_fn(
                bridge_fn.name,
                bridge_fn.debug_info,
            ));
            let lib_name = LibraryName::from_str(bridge_fn.lib_name, None).unwrap();
            let lib = libs.entry(lib_name.name).or_insert_with(|| Lib {
                version: lib_name.version,
                syms: HashMap::default(),
            });

            // TODO: If version does not match, error.

            lib.syms.insert(
                Identifier::new(bridge_fn.name),
                Gc::new(Value::from(Procedure::new(
                    rt.clone(),
                    Vec::new(),
                    match bridge_fn.wrapper {
                        BridgePtr::Sync(sbridge) => FuncPtr::SyncBridge(sbridge),
                        BridgePtr::Async(abridge) => FuncPtr::AsyncBridge(abridge),
                    },
                    bridge_fn.num_args,
                    bridge_fn.variadic,
                    Some(debug_info),
                ))),
            );
        }

        // Define the special keyword libraries:
        let special_keyword_libs = [
            (
                ["rnrs", "base", "special-keywords"],
                &[
                    ("begin", SpecialKeyword::Begin),
                    ("lambda", SpecialKeyword::Lambda),
                    ("let", SpecialKeyword::Let),
                    ("let-syntax", SpecialKeyword::LetSyntax),
                    ("letrec-syntax", SpecialKeyword::LetRecSyntax),
                    ("if", SpecialKeyword::If),
                    ("and", SpecialKeyword::And),
                    ("or", SpecialKeyword::Or),
                    ("quote", SpecialKeyword::Quote),
                    ("syntax", SpecialKeyword::Syntax),
                    ("set!", SpecialKeyword::Set),
                    ("define", SpecialKeyword::Define),
                    ("define-syntax", SpecialKeyword::DefineSyntax),
                    ("import", SpecialKeyword::Import),
                    ("$undefined", SpecialKeyword::Undefined),
                ][..],
            ),
            (
                ["rnrs", "syntax-case", "special-keywords"],
                &[("syntax-case", SpecialKeyword::SyntaxCase)],
            ),
        ]
        .into_iter()
        .map(|(name, special_keywords)| {
            let name = name
                .iter()
                .map(|name| Symbol::intern(name))
                .collect::<Vec<_>>();
            (
                name.clone(),
                Library(Gc::new(LibraryInner {
                    rt: rt.clone(),
                    kind: LibraryKind::Libary {
                        name: LibraryName {
                            version: Version::from([6]),
                            name,
                        },
                        path: None,
                    },
                    imports: HashMap::default(),
                    exports: special_keywords
                        .iter()
                        .map(|(name, _)| {
                            let name = Identifier::new(name);
                            (
                                name.clone(),
                                Export {
                                    rename: name.clone(),
                                    origin: None,
                                },
                            )
                        })
                        .collect(),
                    vars: HashMap::default(),
                    keywords: HashMap::default(),
                    special_keywords: special_keywords
                        .iter()
                        .map(|(name, kw)| (Identifier::new(name), *kw))
                        .collect(),
                    state: LibraryState::Invoked,
                })),
            )
        });

        let libs = libs
            .into_iter()
            .map(|(name, lib)| {
                let exports = lib
                    .syms
                    .keys()
                    .map(|export| {
                        (
                            export.clone(),
                            Export {
                                rename: export.clone(),
                                origin: None,
                            },
                        )
                    })
                    .collect();
                let lib_inner = LibraryInner {
                    rt: rt.clone(),
                    kind: LibraryKind::Libary {
                        name: LibraryName {
                            version: lib.version,
                            name: name.clone(),
                        },
                        path: None,
                    },
                    imports: HashMap::default(),
                    exports,
                    vars: lib.syms,
                    keywords: HashMap::default(),
                    special_keywords: HashMap::default(),
                    state: LibraryState::Invoked,
                };
                (name, Library(Gc::new(lib_inner)))
            })
            .chain(special_keyword_libs)
            .collect();

        Self {
            libs,
            loading: HashSet::default(),
        }
    }
}

#[derive(Trace, Clone, Debug)]
pub struct Registry(pub(crate) Gc<RegistryInner>);

impl Registry {
    pub(crate) fn empty() -> Self {
        Self(Gc::new(RegistryInner::empty()))
    }

    pub(crate) fn new(rt: &Runtime) -> Self {
        Self(Gc::new(RegistryInner::new(rt)))
    }

    fn mark_as_loading(&self, name: &[Symbol]) {
        self.0.write().loading.insert(name.to_vec());
    }

    // TODO: This function is quite messy, so it would be nice to do a little
    // clean up on it.
    fn load_lib(&self, rt: &Runtime, name: &[Symbol]) -> Result<Library, ImportError> {
        if let Some(lib) = self.0.read().libs.get(name) {
            return Ok(lib.clone());
        }

        // Check to see that we're not currently loading the library. Circular
        // dependencies are not allowed. We should probably support them at some
        // point to some degree.
        if self.0.read().loading.contains(name) {
            return Err(ImportError::CircularDependency);
        }

        // Load the library and insert it into the registry.
        self.mark_as_loading(name);
        const DEFAULT_LOAD_PATH: &str = "~/.gouki";

        // Get the suffix:
        let path_suffix = name.iter().copied().map(Symbol::to_str).collect::<Vec<_>>();
        let path_suffix = path_suffix.join("/");

        // Check the current path first:
        let curr_path = std::env::current_dir()
            .expect("If we can't get the current working directory, we can't really do much");
        let lib = match load_lib_from_dir(rt, &curr_path, &path_suffix) {
            Ok(lib) => lib,
            Err(ImportError::LibraryNotFound) => {
                // Try from the load path
                let path = PathBuf::from(
                    std::env::var("GOUKI_LOAD_PATH")
                        .unwrap_or_else(|_| DEFAULT_LOAD_PATH.to_string()),
                );

                match load_lib_from_dir(rt, &path, &path_suffix) {
                    Ok(lib) => lib,
                    Err(ImportError::LibraryNotFound) => {
                        // Finally, try the embedded Stdlib
                        let file_name = format!("{path_suffix}.sls");
                        let Some(lib) = Stdlib::get(&file_name) else {
                            return Err(ImportError::LibraryNotFound);
                        };
                        let contents = std::str::from_utf8(&lib.data).unwrap();
                        let syntax = Syntax::from_str(contents, Some(&file_name))
                            .map_err(|err| ImportError::ParseSyntaxError(format!("{err:?}")))?;
                        let [syntax] = syntax.as_slice() else {
                            unreachable!()
                        };
                        let spec =
                            LibrarySpec::parse(syntax).map_err(ImportError::ParseAstError)?;
                        Library::from_spec(rt, spec, PathBuf::from(file_name))?
                    }
                    x => return x,
                }
            }
            x => return x,
        };
        let mut this_mut = self.0.write();
        this_mut.libs.insert(name.to_vec(), lib.clone());
        this_mut.loading.remove(name);
        Ok(lib)
    }

    /// Load a set of symbols from a library with the given import set.
    pub(crate) fn import<'b, 'a: 'b>(
        &'a self,
        rt: &'b Runtime,
        import_set: ImportSet,
    ) -> ImportIter<'b> {
        match import_set {
            ImportSet::Library(lib) => {
                let lib = self.load_lib(rt, &lib.name)?;
                let exports = {
                    lib.0
                        .read()
                        .exports
                        .iter()
                        .map(|(orign, exp)| (orign.clone(), exp.clone()))
                        .collect::<Vec<_>>()
                };
                Ok(Box::new(exports.into_iter().map(move |(orign, exp)| {
                    (
                        exp.rename,
                        Import {
                            rename: if let Some(import) = lib.0.read().imports.get(&orign) {
                                import.rename.clone()
                            } else {
                                orign
                            },
                            origin: if let Some(redirect) = exp.origin {
                                redirect.clone()
                            } else {
                                lib.clone()
                            },
                        },
                    )
                })) as DynIter<'b>)
            }
            ImportSet::Only { set, allowed } => Ok(Box::new(
                self.import(rt, *set)?
                    .filter(move |(import, _)| allowed.contains(import)),
            ) as DynIter<'b>),
            ImportSet::Except { set, disallowed } => Ok(Box::new(
                self.import(rt, *set)?
                    .filter(move |(import, _)| !disallowed.contains(import)),
            ) as DynIter<'b>),
            ImportSet::Prefix { set, prefix } => {
                let prefix = prefix.sym.to_str();
                Ok(Box::new(
                    self.import(rt, *set)?
                        .map(move |(name, import)| (name.prefix(&prefix), import)),
                ) as DynIter<'b>)
            }
            ImportSet::Rename { set, mut renames } => Ok(Box::new(
                self.import(rt, *set)?
                    .map(move |(name, import)| (renames.remove(&name).unwrap_or(name), import)),
            ) as DynIter<'b>),
        }
    }
}

type ImportIter<'a> = Result<Box<dyn Iterator<Item = (Identifier, Import)> + 'a>, ImportError>;
type DynIter<'a> = Box<dyn Iterator<Item = (Identifier, Import)> + 'a>;

fn load_lib_from_dir(rt: &Runtime, path: &Path, path_suffix: &str) -> Result<Library, ImportError> {
    for ext in ["sls", "ss", "scm"] {
        let path = path.join(format!("{path_suffix}.{ext}"));
        if let Ok(false) = path.try_exists() {
            continue;
        }
        let contents = std::fs::read_to_string(&path)?;
        let file_name = path.file_name().unwrap().to_string_lossy();
        let syntax = Syntax::from_str(&contents, Some(&file_name))
            .map_err(|err| ImportError::ParseSyntaxError(format!("{err:?}")))?;
        let [syntax] = syntax.as_slice() else {
            panic!("Has to be one item");
        };
        let spec = LibrarySpec::parse(syntax).unwrap();
        return Library::from_spec(rt, spec, path);
    }

    Err(ImportError::LibraryNotFound)
}

#[derive(Debug, thiserror::Error)]
pub enum ImportError {
    #[error("Library not found")]
    LibraryNotFound,
    #[error("Error parsing into s-expression: {0}")]
    ParseSyntaxError(String),
    #[error("Error parsing into AST")]
    ParseAstError(ParseAstError),
    #[error("Error reading library: {0}")]
    IoError(#[from] std::io::Error),
    #[error("Import identifier `{0}` bound multiple times")]
    DuplicateIdentifier(Symbol),
    #[error("Circular dependency found")]
    CircularDependency,
}

#[derive(Trace, derive_more::Debug)]
pub(crate) struct LibraryInner {
    #[debug(skip)]
    pub(crate) rt: Runtime,
    kind: LibraryKind,
    exports: HashMap<Identifier, Export>,
    imports: HashMap<Identifier, Import>,
    state: LibraryState,
    vars: HashMap<Identifier, Gc<Value>>,
    keywords: HashMap<Identifier, Keyword>,
    special_keywords: HashMap<Identifier, SpecialKeyword>,
}

impl LibraryInner {
    pub(crate) fn new(
        rt: &Runtime,
        kind: LibraryKind,
        imports: HashMap<Identifier, Import>,
        exports: HashMap<Identifier, Identifier>,
        body: Vec<Syntax>,
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
            vars: HashMap::default(),
            keywords: HashMap::default(),
            special_keywords: HashMap::default(),
        }
    }
}

#[derive(Trace, Debug)]
pub enum LibraryKind {
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

#[derive(Clone, Trace, derive_more::Debug)]
pub struct Import {
    /// The original name of the identifier before being renamed.
    pub(crate) rename: Identifier,
    #[debug(skip)]
    pub(crate) origin: Library,
}

#[derive(Trace, Clone, derive_more::Debug)]
pub struct Export {
    pub(crate) rename: Identifier,
    #[debug(skip)]
    pub(crate) origin: Option<Library>,
}

#[derive(Trace, Clone, Debug)]
pub struct Library(pub(crate) Gc<LibraryInner>);

impl PartialEq for Library {
    fn eq(&self, rhs: &Self) -> bool {
        Gc::ptr_eq(&self.0, &rhs.0)
    }
}

impl Library {
    pub fn new_repl(rt: &Runtime) -> Self {
        let mut inner = LibraryInner::new(
            rt,
            LibraryKind::Repl,
            HashMap::default(),
            HashMap::default(),
            Vec::new(),
        );
        inner.state = LibraryState::Invoked;
        Self(Gc::new(inner))
    }

    pub fn new_program(rt: &Runtime, path: &Path) -> Self {
        // Programs are given the import keyword, free of charge.
        let inner = LibraryInner {
            rt: rt.clone(),
            kind: LibraryKind::Program {
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
        Self(Gc::new(inner))
    }

    pub fn from_spec(rt: &Runtime, spec: LibrarySpec, path: PathBuf) -> Result<Self, ImportError> {
        let registry = rt.get_registry();

        // Import libraries:
        let mut imports = HashMap::<Identifier, Import>::default();
        let mut exports = HashMap::<Identifier, Identifier>::default();

        for lib_import in spec.imports.import_sets.into_iter() {
            for (ident, import) in registry.import(rt, lib_import)? {
                match imports.entry(ident) {
                    Entry::Occupied(prev_imported)
                        if prev_imported.get().origin != import.origin =>
                    {
                        return Err(ImportError::DuplicateIdentifier(prev_imported.key().sym));
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
                        for (ident, import) in registry.import(rt, lib_import)? {
                            match imports.entry(ident.clone()) {
                                Entry::Occupied(prev_imported)
                                    if prev_imported.get().origin != import.origin =>
                                {
                                    return Err(ImportError::DuplicateIdentifier(
                                        prev_imported.key().sym,
                                    ));
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

        Ok(Self(Gc::new(LibraryInner::new(
            rt,
            LibraryKind::Libary {
                name: spec.name,
                path: Some(path),
            },
            imports,
            exports,
            spec.body,
        ))))
    }

    pub fn import(&self, import_set: ImportSet) -> Result<(), ImportError> {
        let (rt, registry) = {
            let this = self.0.read();
            (this.rt.clone(), this.rt.get_registry())
        };
        let imports = registry.import(&rt, import_set)?;
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

    pub(crate) fn maybe_expand(&self) -> Result<(), ParseAstError> {
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
        let expanded = DefinitionBody::parse_lib_body(&rt, &body, &env, body[0].span())?;
        self.0.write().state = LibraryState::Expanded(expanded);
        Ok(())
    }

    pub(crate) fn maybe_invoke(&self) -> Result<(), Condition> {
        self.maybe_expand()?;
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
        let proc = rt.compile_expr(compiled);
        rt.eval_blocking(Application::new(
            proc,
            Vec::new(),
            ExceptionHandler::default(),
            DynamicWind::default(),
            None,
        ))?;
        self.0.write().state = LibraryState::Invoked;
        Ok(())
    }

    pub fn is_repl(&self) -> bool {
        matches!(self.0.read().kind, LibraryKind::Repl)
    }

    pub fn is_bound(&self, name: &Identifier) -> bool {
        let this = self.0.read();
        this.vars.contains_key(name) || this.imports.contains_key(name)
    }

    pub(crate) fn def_var(&self, name: Identifier, value: Value) -> Global {
        let mut this = self.0.write();
        let mutable = !this.exports.contains_key(&name);
        match this.vars.entry(name.clone()) {
            Entry::Occupied(occup) => Global::new(name, occup.get().clone(), mutable),
            Entry::Vacant(vacant) => {
                Global::new(name, vacant.insert(Gc::new(value)).clone(), mutable)
            }
        }
    }

    pub(crate) fn def_keyword(&self, keyword: Identifier, mac: Keyword) {
        let mut this = self.0.write();
        this.keywords.insert(keyword, mac);
    }

    pub(crate) fn fetch_var(&self, name: &Identifier) -> Result<Option<Global>, Condition> {
        // Check this library
        let Import { origin, rename } = {
            let this = self.0.read();
            if let Some(var) = this.vars.get(name) {
                let var = var.clone();
                // Fetching this every time is kind of slow.
                let mutable = !this.exports.contains_key(name);
                return Ok(Some(Global::new(name.clone(), var, mutable)));
            }

            // Check our imports
            let Some(import) = this.imports.get(name) else {
                return Ok(None);
            };

            import.clone()
        };

        origin.maybe_invoke()?;
        origin.fetch_var(&rename)
    }

    pub(crate) fn fetch_keyword(&self, keyword: &Identifier) -> Result<Option<Keyword>, Condition> {
        // Check this library
        let Import { origin, rename } = {
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

        origin.maybe_invoke()?;
        origin.fetch_keyword(&rename)
    }

    pub(crate) fn fetch_special_keyword(&self, keyword: &Identifier) -> Option<SpecialKeyword> {
        // Check this library:
        let Import { origin, rename } = {
            let this = self.0.read();
            if let Some(special_keyword) = this.special_keywords.get(keyword) {
                return Some(*special_keyword);
            }

            // Check our imports:
            this.imports.get(keyword)?.clone()
        };

        origin.fetch_special_keyword(&rename)
    }
}

// TODO: Use these states to detect circular dependencies when we do our DFS.
// Or, alternatively, just handle circular dependencies like Guile does.
#[derive(Trace, Debug)]
pub enum LibraryState {
    Invalid,
    Unexpanded(Vec<Syntax>),
    Expanded(DefinitionBody),
    Invoked,
}
