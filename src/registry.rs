//! Global collection of libraries associated with a [`Runtime`]

use crate::{
    ast::{ImportSet, LibraryName, LibrarySpec, Primitive, Version},
    env::{
        Binding, Export, Global, Import, LibraryState, Scope, TOP_LEVEL_BINDINGS, TopLevelBinding,
        TopLevelEnvironment, TopLevelEnvironmentInner, TopLevelKind, add_binding,
    },
    exceptions::{Exception, ImportError},
    gc::{Gc, Trace},
    proc::{BridgePtr, FuncPtr, KnownFunc, ProcDebugInfo, Procedure},
    symbols::Symbol,
    syntax::{Identifier, Syntax},
    value::{Cell, Value},
};

#[cfg(feature = "plugins")]
use crate::proc::{Application, ContBarrier};

use std::{
    path::{Path, PathBuf},
    sync::{Arc, LazyLock, OnceLock},
};

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

/// Re-export of the inventory crate for use with proc macros
pub use inventory;

#[cfg(feature = "async")]
use async_lock::RwLock;
#[cfg(feature = "async")]
use futures::future::BoxFuture;
#[cfg(not(feature = "async"))]
use parking_lot::RwLock;
/// Define and register a Scheme function in Rust.
pub use scheme_rs_macros::bridge;
/// Define and register a Scheme function in Rust written in continuation
/// passing style.
pub use scheme_rs_macros::cps_bridge;
use scheme_rs_macros::{maybe_async, maybe_await};

pub(crate) mod error {
    use crate::{ast::VersionReference, exceptions::Message, ports::IoError};

    use super::*;

    pub(super) fn version_mismatch(requirement: &VersionReference, lib: &LibraryName) -> Exception {
        Exception::from((
            Message::new(format!(
                "version requirement `{requirement}` does not match library version `{}`",
                lib.version
            )),
            ImportError::new(lib.name()),
        ))
    }

    pub(super) fn library_not_found() -> Exception {
        Exception::from((IoError::new(), Message::new("library not found")))
    }

    // TODO: Include dependency chain that lead to this error
    pub(super) fn circular_dependency() -> Exception {
        Exception::from(Message::new("circular dependency"))
    }
}

#[doc(hidden)]
#[derive(Copy, Clone)]
pub enum Bridge {
    Known(KnownFunc),
    Sync(BridgePtr),
    #[cfg(feature = "async")]
    Async(crate::proc::AsyncBridgePtr),
}

// BridgeFn is passed across the FFI boundary between host and plugin.
// Both sides MUST be compiled with the same rustc version and scheme-rs
// feature flags, since BridgeFn is not #[repr(C)].
#[doc(hidden)]
#[derive(Copy, Clone)]
pub struct BridgeFn {
    name: &'static str,
    lib_name: &'static str,
    num_args: usize,
    variadic: bool,
    wrapper: Bridge,
    debug_info: BridgeFnDebugInfo,
}

impl BridgeFn {
    pub const fn new(
        name: &'static str,
        lib_name: &'static str,
        num_args: usize,
        variadic: bool,
        wrapper: Bridge,
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

#[doc(hidden)]
#[derive(Copy, Clone)]
pub struct BridgeFnDebugInfo {
    pub(crate) file: &'static str,
    pub(crate) line: u32,
    pub(crate) column: u32,
    pub(crate) offset: usize,
    pub(crate) args: &'static [&'static str],
    pub(crate) docs: &'static str,
}

impl BridgeFnDebugInfo {
    pub const fn new(
        file: &'static str,
        line: u32,
        column: u32,
        offset: usize,
        args: &'static [&'static str],
        docs: &'static str,
    ) -> Self {
        Self {
            file,
            line,
            column,
            offset,
            args,
            docs,
        }
    }
}

inventory::collect!(BridgeFn);

#[cfg(feature = "plugins")]
#[unsafe(no_mangle)]
pub extern "C" fn scheme_rs_bridges() -> PluginBridges {
    use std::sync::OnceLock;
    static BRIDGES: OnceLock<Vec<BridgeFn>> = OnceLock::new();
    let bridges = BRIDGES.get_or_init(|| inventory::iter::<BridgeFn>().copied().collect());
    PluginBridges {
        version: SCHEME_RS_VERSION.as_ptr(),
        version_len: SCHEME_RS_VERSION.len(),
        ptr: bridges.as_ptr(),
        len: bridges.len(),
    }
}

#[cfg(feature = "plugins")]
pub static SCHEME_RS_VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(feature = "plugins")]
#[repr(C)]
pub struct PluginBridges {
    pub version: *const u8,
    pub version_len: usize,
    pub ptr: *const BridgeFn,
    pub len: usize,
}

#[cfg(feature = "plugins")]
#[allow(dead_code)]
#[derive(Trace)]
struct PluginHandle(#[trace(skip)] std::mem::ManuallyDrop<libloading::Library>);

#[cfg(feature = "plugins")]
unsafe impl Send for PluginHandle {}

#[cfg(feature = "plugins")]
impl PluginHandle {
    fn new(library: libloading::Library) -> Self {
        Self(std::mem::ManuallyDrop::new(library))
    }
}

#[derive(rust_embed::Embed)]
#[folder = "scheme"]
struct Stdlib;

#[derive(Default, Trace)]
pub(crate) struct RegistryInner {
    pub(crate) libs: HashMap<Vec<Symbol>, TopLevelEnvironment>,
    dep_graph: HashMap<Vec<Symbol>, HashSet<Vec<Symbol>>>,
    #[trace(skip)]
    loading: HashSet<Vec<Symbol>>,
    #[cfg(feature = "plugins")]
    #[trace(skip)]
    plugins: Vec<PluginHandle>,
    #[cfg(feature = "plugins")]
    #[trace(skip)]
    loaded_plugin_paths: HashSet<PathBuf>,
}

static PRIMITIVES_SCOPE: LazyLock<Scope> = LazyLock::new(Scope::new);
pub(crate) static PRIMITIVES_LIB: OnceLock<TopLevelEnvironment> = OnceLock::new();

pub(crate) fn primitive(name: Symbol) -> Binding {
    crate::env::resolve(&Identifier::from_symbol(name, *PRIMITIVES_SCOPE)).unwrap()
}

impl RegistryInner {
    pub(crate) fn check_for_circular_dependencies(
        &mut self,
        from: &[Symbol],
        to: &[Symbol],
    ) -> Result<(), Exception> {
        if self.reaches(to, from) {
            return Err(error::circular_dependency());
        }
        self.dep_graph
            .entry(from.to_vec())
            .or_default()
            .insert(to.to_vec());
        Ok(())
    }

    fn register_bridges<'a>(
        &mut self,
        bridges: impl Iterator<Item = &'a BridgeFn>,
    ) -> Result<(), Exception> {
        struct Lib {
            version: Version,
            syms: HashMap<Symbol, Procedure>,
        }
        let mut new_libs = HashMap::<Vec<Symbol>, Lib>::default();

        for bridge_fn in bridges {
            let debug_info = Arc::new(ProcDebugInfo::from_bridge_fn(
                bridge_fn.name,
                bridge_fn.debug_info,
            ));
            let lib_name = LibraryName::from_str(bridge_fn.lib_name, None)?;
            let lib = new_libs.entry(lib_name.name).or_insert_with(|| Lib {
                version: lib_name.version,
                syms: HashMap::default(),
            });

            lib.syms.insert(
                Symbol::intern(bridge_fn.name),
                Procedure::with_debug_info(
                    Vec::new(),
                    match bridge_fn.wrapper {
                        Bridge::Sync(func) => FuncPtr::Bridge(func),
                        Bridge::Known(known) => FuncPtr::Known(known),
                        #[cfg(feature = "async")]
                        Bridge::Async(func) => FuncPtr::AsyncBridge(func),
                    },
                    bridge_fn.num_args,
                    bridge_fn.variadic,
                    Some(debug_info),
                ),
            );
        }

        for (name, lib) in new_libs {
            let scope = Scope::new();

            let exports = lib
                .syms
                .into_iter()
                .map(|(name, proc)| {
                    let binding = Binding::new();
                    add_binding(Identifier::from_symbol(name, scope), binding);
                    (
                        name,
                        proc,
                        Export {
                            binding,
                            origin: None,
                        },
                    )
                })
                .collect::<Vec<_>>();
            let tle = TopLevelEnvironment(Gc::new(TopLevelEnvironmentInner {
                kind: TopLevelKind::Libary {
                    name: LibraryName {
                        version: lib.version,
                        name: name.clone(),
                    },
                    path: None,
                },
                imports: RwLock::new(HashMap::default()),
                exports: exports
                    .iter()
                    .map(|(name, _, export)| (*name, export.clone()))
                    .collect(),
                state: RwLock::new(LibraryState::BridgesDefined),
                scope,
            }));

            for (name, proc, export) in exports {
                TOP_LEVEL_BINDINGS.lock().insert(
                    export.binding,
                    TopLevelBinding::Global(Global::new(
                        name,
                        Cell::new(Value::from(proc)),
                        false,
                        tle.clone(),
                    )),
                );
            }

            self.libs.insert(name, tle);
        }
        Ok(())
    }

    /// Construct a Registry with all of the available bridge functions and special keywords.
    pub(crate) fn new() -> Self {
        let mut this = Self::default();

        this.register_bridges(inventory::iter::<BridgeFn>())
            .expect("statically-linked bridge has invalid lib_name");

        // Define the special keyword libraries:
        let exports = [
            ("begin", Primitive::Begin),
            ("lambda", Primitive::Lambda),
            ("let", Primitive::Let),
            ("letrec", Primitive::LetRec),
            ("let-syntax", Primitive::LetSyntax),
            ("letrec-syntax", Primitive::LetRecSyntax),
            ("if", Primitive::If),
            ("and", Primitive::And),
            ("or", Primitive::Or),
            ("quote", Primitive::Quote),
            ("syntax", Primitive::Syntax),
            ("set!", Primitive::Set),
            ("define", Primitive::Define),
            ("define-syntax", Primitive::DefineSyntax),
            ("import", Primitive::Import),
            ("%undefined", Primitive::Undefined),
            ("syntax-case", Primitive::SyntaxCase),
        ]
        .into_iter()
        .map(|(name, primitive)| {
            let name = Symbol::intern(name);
            let binding = Binding::new();
            add_binding(Identifier::from_symbol(name, *PRIMITIVES_SCOPE), binding);
            TOP_LEVEL_BINDINGS
                .lock()
                .insert(binding, TopLevelBinding::Primitive(primitive));
            (
                name,
                Export {
                    binding,
                    origin: None,
                },
            )
        })
        .collect();
        let name = vec![Symbol::intern("scheme-rs"), Symbol::intern("primitives")];
        let primitives_lib = TopLevelEnvironment(Gc::new(TopLevelEnvironmentInner {
            kind: TopLevelKind::Libary {
                name: LibraryName {
                    version: Version::from([6]),
                    name: name.clone(),
                },
                path: None,
            },
            imports: RwLock::new(HashMap::default()),
            exports,
            state: RwLock::new(LibraryState::BridgesDefined),
            scope: *PRIMITIVES_SCOPE,
        }));

        PRIMITIVES_LIB.set(primitives_lib.clone()).unwrap();

        this.libs.insert(name, primitives_lib);

        this
    }

    #[cfg(feature = "plugins")]
    unsafe fn load_plugin_locked(&mut self, library: libloading::Library) -> Result<(), Exception> {
        let bridges: &[BridgeFn] = unsafe {
            let func: libloading::Symbol<extern "C" fn() -> PluginBridges> =
                library.get(b"scheme_rs_bridges").map_err(|e| {
                    Exception::error(format!("plugin does not export scheme_rs_bridges: {e}"))
                })?;
            let result = func();

            let plugin_version = std::str::from_utf8(std::slice::from_raw_parts(
                result.version,
                result.version_len,
            ))
            .unwrap_or("<invalid utf8>");
            if plugin_version != SCHEME_RS_VERSION {
                return Err(Exception::error(format!(
                    "plugin version mismatch: plugin was built against \
                     scheme-rs {plugin_version}, host is {SCHEME_RS_VERSION}"
                )));
            }

            std::slice::from_raw_parts(result.ptr, result.len)
        };

        self.plugins.push(PluginHandle::new(library));
        self.register_bridges(bridges.iter())?;
        Ok(())
    }

    /// Determines whether or not start reaches target in the dependency graph
    fn reaches(&self, start: &[Symbol], target: &[Symbol]) -> bool {
        let mut stack = vec![start.to_vec()];
        let mut visited = HashSet::default();
        while let Some(node) = stack.pop() {
            if node == target {
                return true;
            }
            if !visited.insert(node.clone()) {
                continue;
            }
            if let Some(deps) = self.dep_graph.get(&node) {
                stack.extend(deps.iter().cloned());
            }
        }
        false
    }

    /// Attempt to load a library from the directory, returning None if no such file exists.
    #[maybe_async]
    fn load_lib_from_dir(
        &mut self,
        path: &Path,
        path_suffix: &str,
        scope: Scope,
    ) -> Result<Option<TopLevelEnvironment>, Exception> {
        for ext in ["sls", "ss", "scm"] {
            let path = path.join(format!("{path_suffix}.{ext}"));
            if let Ok(false) = maybe_await!(try_exists(&path)) {
                continue;
            }
            let contents = maybe_await!(read_to_string(&path))?;

            let file_name = path.file_name().unwrap().to_string_lossy();
            let form = Syntax::from_str(&contents, Some(&file_name))?;

            let form = match form.as_list() {
                Some([form, end]) if end.is_null() => form,
                _ => return Err(Exception::error("library is malformed")),
            };
            let spec = LibrarySpec::parse(form)?;
            return Ok(Some(maybe_await!(
                TopLevelEnvironment::from_spec_with_scope(spec, path, scope, self)
            )?));
        }

        Ok(None)
    }

    // TODO: This function is quite messy, so it would be nice to do a little
    // clean up on it.
    #[maybe_async]
    fn load_lib(&mut self, name: &[Symbol]) -> Result<TopLevelEnvironment, Exception> {
        let scope = if let Some(lib) = self.libs.get(name) {
            if !matches!(
                *maybe_await!(lib.0.state.read()),
                LibraryState::BridgesDefined
            ) {
                return Ok(lib.clone());
            }
            lib.0.scope
        } else {
            Scope::new()
        };

        // Load the library and insert it into the registry.
        const DEFAULT_LOAD_PATH: &str = "~/.scheme-rs";

        // Get the suffix:
        let path_suffix = name.iter().copied().map(Symbol::to_str).collect::<Vec<_>>();
        let path_suffix = path_suffix.join("/");

        // Check the current path first:
        let curr_path = std::env::current_dir()
            .expect("If we can't get the current working directory, we can't really do much");
        let lib = if cfg!(feature = "load-libraries-from-fs")
            && let Some(lib) =
                maybe_await!(self.load_lib_from_dir(&curr_path, &path_suffix, scope))?
        {
            lib
        } else {
            // Try from the load path
            let path = PathBuf::from(
                std::env::var("SCHEME_RS_LOAD_PATH")
                    .unwrap_or_else(|_| DEFAULT_LOAD_PATH.to_string()),
            );

            if cfg!(feature = "load-libraries-from-fs")
                && let Some(lib) = maybe_await!(self.load_lib_from_dir(&path, &path_suffix, scope))?
            {
                lib
            } else {
                // Finally, try the embedded Stdlib
                let file_name = format!("{path_suffix}.sls");
                if let Some(lib) = Stdlib::get(&file_name) {
                    let contents = std::str::from_utf8(&lib.data).unwrap();
                    let form = Syntax::from_str(contents, Some(&file_name))?;
                    let form = match form.as_list() {
                        Some([form, end]) if end.is_null() => form,
                        _ => return Err(Exception::error("library is malformed")),
                    };
                    let spec = LibrarySpec::parse(form)?;
                    maybe_await!(TopLevelEnvironment::from_spec_with_scope(
                        spec,
                        PathBuf::from(file_name),
                        scope,
                        self,
                    ))?
                } else if let Some(lib) = self.libs.get(name) {
                    *maybe_await!(lib.0.state.write()) = LibraryState::Invoked;
                    lib.clone()
                } else {
                    return Err(error::library_not_found());
                }
            }
        };
        self.libs.insert(name.to_vec(), lib.clone());
        Ok(lib)
    }

    /// Load a set of symbols from a library with the given import set.
    #[cfg(not(feature = "async"))]
    pub(crate) fn import(&mut self, import_set: ImportSet) -> ImportIter<'_> {
        self.import_inner(import_set)
    }

    /// Load a set of symbols from a library with the given import set.
    #[cfg(feature = "async")]
    pub(crate) fn import(&mut self, import_set: ImportSet) -> ImportIterFuture<'_> {
        Box::pin(self.import_inner(import_set))
    }

    #[maybe_async]
    pub(crate) fn import_inner(&mut self, import_set: ImportSet) -> ImportIter<'_> {
        match import_set {
            ImportSet::Library(lib_import) => {
                let lib = maybe_await!(self.load_lib(&lib_import.name)).map_err(|err| {
                    let lib_name = lib_import
                        .name
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>();
                    let lib_name = format!("({})", lib_name.join(" "));
                    err.add_condition(ImportError::new(lib_name))
                })?;

                let exports = {
                    if let TopLevelKind::Libary { name, .. } = &lib.0.kind
                        && !lib_import.version_ref.matches(&name.version)
                    {
                        return Err(error::version_mismatch(&lib_import.version_ref, name));
                    }

                    lib.0
                        .exports
                        .iter()
                        .map(|(origin, exp)| (*origin, exp.clone()))
                        .collect::<Vec<_>>()
                };
                Ok(Box::new(exports.into_iter().map(move |(name, exp)| {
                    (
                        name,
                        Import {
                            binding: exp.binding,
                            origin: if let Some(redirect) = exp.origin {
                                redirect.clone()
                            } else {
                                lib.clone()
                            },
                        },
                    )
                })) as DynIter<'_>)
            }
            ImportSet::Only { set, allowed } => Ok(Box::new(
                maybe_await!(self.import(*set))?
                    .filter(move |(import, _)| allowed.contains(import)),
            ) as DynIter<'_>),
            ImportSet::Except { set, disallowed } => Ok(Box::new(
                maybe_await!(self.import(*set))?
                    .filter(move |(import, _)| !disallowed.contains(import)),
            ) as DynIter<'_>),
            ImportSet::Prefix { set, prefix } => {
                let prefix = prefix.to_str();
                Ok(
                    Box::new(maybe_await!(self.import(*set))?.map(move |(name, import)| {
                        (
                            Symbol::intern(&format!("{prefix}{}", name.to_str())),
                            import,
                        )
                    })) as DynIter<'_>,
                )
            }
            ImportSet::Rename { set, mut renames } => Ok(Box::new(
                maybe_await!(self.import(*set))?
                    .map(move |(name, import)| (renames.remove(&name).unwrap_or(name), import)),
            ) as DynIter<'_>),
        }
    }
}

#[derive(Trace, Clone)]
pub(crate) struct Registry(pub(crate) Gc<RwLock<RegistryInner>>);

impl Registry {
    pub(crate) fn new() -> Self {
        Self(Gc::new(RwLock::new(RegistryInner::new())))
    }

    #[maybe_async]
    pub(crate) fn def_lib(&self, lib: &str, path: &str) -> Result<(), Exception> {
        let form = Syntax::from_str(lib, Some(path))?;
        let form = match form.as_list() {
            Some([form, end]) if end.is_null() => form,
            _ => return Err(Exception::error("library is malformed")),
        };
        let spec = LibrarySpec::parse(form)?;
        let name = spec.name.name.clone();
        let lib = maybe_await!(TopLevelEnvironment::from_spec(spec, PathBuf::from(path)))?;
        let mut this_mut = maybe_await!(self.0.write());
        this_mut.libs.insert(name, lib);
        Ok(())
    }

    /// # Safety
    ///
    /// The plugin must be built with the same `rustc` and scheme-rs
    /// feature flags as the host. Version is checked at load time but
    /// ABI drift from different compilers is not detected.
    #[maybe_async]
    #[cfg(feature = "plugins")]
    pub unsafe fn load_plugin(&self, library: libloading::Library) -> Result<(), Exception> {
        let mut inner = maybe_await!(self.0.write());
        unsafe { inner.load_plugin_locked(library) }
    }

    #[maybe_async]
    #[cfg(feature = "plugins")]
    fn load_plugin_from_path(&self, path: &str) -> Result<(), Exception> {
        let canonical = std::fs::canonicalize(path)
            .map_err(|e| Exception::error(format!("failed to resolve plugin path {path}: {e}")))?;

        let mut inner = maybe_await!(self.0.write());

        if inner.loaded_plugin_paths.contains(&canonical) {
            return Ok(());
        }

        let library = unsafe { libloading::Library::new(&canonical) }
            .map_err(|e| Exception::error(format!("failed to load plugin {path}: {e}")))?;
        unsafe { inner.load_plugin_locked(library)? };
        inner.loaded_plugin_paths.insert(canonical);
        Ok(())
    }
}

#[maybe_async]
#[cfg(feature = "plugins")]
#[cps_bridge(def = "%load-plugin path", lib = "(scheme-rs plugins builtins)")]
pub fn load_plugin(
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    barrier: &mut ContBarrier<'_>,
) -> Result<Application, Exception> {
    use crate::runtime::Runtime;

    let [path] = args else { unreachable!() };
    let path: crate::strings::WideString = path.clone().try_into()?;
    maybe_await!(
        Runtime::handle()
            .get_registry()
            .load_plugin_from_path(&path.to_string())
    )?;
    Ok(barrier.call_cont(Vec::new()))
}

type DynIter<'a> = Box<dyn Iterator<Item = (Symbol, Import)> + Send + 'a>;
type ImportIter<'b> = Result<DynIter<'b>, Exception>;
#[cfg(feature = "async")]
type ImportIterFuture<'a> = BoxFuture<'a, ImportIter<'a>>;

#[cfg(not(feature = "async"))]
fn try_exists(path: &Path) -> std::io::Result<bool> {
    path.try_exists()
}

#[cfg(feature = "tokio")]
async fn try_exists(path: &Path) -> std::io::Result<bool> {
    tokio::fs::try_exists(path).await
}

#[cfg(not(feature = "async"))]
fn read_to_string(path: &Path) -> std::io::Result<String> {
    std::fs::read_to_string(path)
}

#[cfg(feature = "tokio")]
async fn read_to_string(path: &Path) -> std::io::Result<String> {
    tokio::fs::read_to_string(path).await
}
