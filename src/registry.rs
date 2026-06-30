//! Global collection of libraries associated with a [`Runtime`]

use crate::{
    ast::{ImportSet, LibraryName, LibrarySpec, Primitive, Version},
    env::{
        Binding, Export, Global, Import, LibraryState, Scope, TOP_LEVEL_BINDINGS, TopLevelBinding,
        TopLevelEnvironment, TopLevelEnvironmentInner, TopLevelKind, add_binding,
    },
    exceptions::{Exception, ImportError},
    gc::{Gc, OpaqueGcPtr, Trace},
    proc::{BridgePtr, FuncPtr, KnownFunc, ProcDebugInfo, Procedure},
    runtime::Runtime,
    symbols::Symbol,
    syntax::{Identifier, Syntax},
    value::{Cell, Value},
};

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

/// Re-export of the inventory crate for use with proc macros
pub use inventory;

#[cfg(feature = "async")]
use futures::future::BoxFuture;
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
pub enum Bridge {
    Known(KnownFunc),
    Sync(BridgePtr),
    #[cfg(feature = "async")]
    Async(crate::proc::AsyncBridgePtr),
}

#[doc(hidden)]
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

/// Fat pointer returned by a plugin's `scheme_rs_bridges` export.
#[cfg(feature = "plugins")]
#[repr(C)]
pub struct PluginBridges {
    pub ptr: *const BridgeFn,
    pub len: usize,
}

#[cfg(feature = "plugins")]
struct PluginHandle(libloading::Library);

#[cfg(feature = "plugins")]
// SAFETY: libloading::Library contains no Gc pointers.
unsafe impl Trace for PluginHandle {
    unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {}
    unsafe fn finalize(&mut self) {
        unsafe { std::ptr::drop_in_place(self as *mut Self) }
    }
}

#[derive(rust_embed::Embed)]
#[folder = "scheme"]
struct Stdlib;

pub(crate) struct RegistryInner {
    pub(crate) libs: HashMap<Vec<Symbol>, TopLevelEnvironment>,
    loading: HashSet<Vec<Symbol>>,
    #[cfg(feature = "plugins")]
    plugins: Vec<PluginHandle>,
}

// SAFETY: Only libs contains Gc pointers. plugins and loading have no Gc refs.
unsafe impl Trace for RegistryInner {
    unsafe fn visit_children(&self, visitor: &mut dyn FnMut(OpaqueGcPtr)) {
        unsafe { self.libs.visit_children(visitor) }
    }
    unsafe fn finalize(&mut self) {
        unsafe { std::ptr::drop_in_place(self as *mut Self) }
    }
}

impl Default for RegistryInner {
    fn default() -> Self {
        Self {
            libs: HashMap::default(),
            loading: HashSet::default(),
            #[cfg(feature = "plugins")]
            plugins: Vec::new(),
        }
    }
}

impl RegistryInner {
    /// Construct an empty registry
    pub fn empty() -> Self {
        Self::default()
    }

    /// Register bridge functions into the library table.
    fn register_bridges<'a>(
        &mut self,
        rt: &Runtime,
        bridges: impl Iterator<Item = &'a BridgeFn>,
    ) {
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
            let lib_name = LibraryName::from_str(bridge_fn.lib_name, None).unwrap();
            let lib = new_libs.entry(lib_name.name).or_insert_with(|| Lib {
                version: lib_name.version,
                syms: HashMap::default(),
            });

            lib.syms.insert(
                Symbol::intern(bridge_fn.name),
                Procedure::with_debug_info(
                    rt.clone(),
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
            let tle = TopLevelEnvironment(Gc::new(RwLock::new(TopLevelEnvironmentInner {
                rt: rt.clone(),
                kind: TopLevelKind::Libary {
                    name: LibraryName {
                        version: lib.version,
                        name: name.clone(),
                    },
                    path: None,
                },
                imports: HashMap::default(),
                exports: exports
                    .iter()
                    .map(|(name, _, export)| (*name, export.clone()))
                    .collect(),
                state: LibraryState::BridgesDefined,
                scope,
            })));

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
    }

    /// Construct a Registry with all of the available bridge functions and special keywords.
    pub fn new(rt: &Runtime) -> Self {
        let mut this = Self::default();

        // Import statically-linked bridge functions:
        this.register_bridges(rt, inventory::iter::<BridgeFn>());

        // Define the special keyword libraries:
        let special_keyword_libs = [
            (
                ["rnrs", "base", "primitives"],
                &[
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
                    ("$undefined", Primitive::Undefined),
                ][..],
            ),
            (
                ["rnrs", "syntax-case", "primitives"],
                &[("syntax-case", Primitive::SyntaxCase)],
            ),
        ]
        .into_iter()
        .map(|(name, primitives)| {
            let name = name
                .iter()
                .map(|name| Symbol::intern(name))
                .collect::<Vec<_>>();
            let scope = Scope::new();
            let exports = primitives
                .iter()
                .map(|(name, primitive)| {
                    let name = Symbol::intern(name);
                    let binding = Binding::new();
                    add_binding(Identifier::from_symbol(name, scope), binding);
                    TOP_LEVEL_BINDINGS
                        .lock()
                        .insert(binding, TopLevelBinding::Primitive(*primitive));
                    (
                        name,
                        Export {
                            binding,
                            origin: None,
                        },
                    )
                })
                .collect();

            (
                name.clone(),
                TopLevelEnvironment(Gc::new(RwLock::new(TopLevelEnvironmentInner {
                    rt: rt.clone(),
                    kind: TopLevelKind::Libary {
                        name: LibraryName {
                            version: Version::from([6]),
                            name,
                        },
                        path: None,
                    },
                    imports: HashMap::default(),
                    exports,
                    state: LibraryState::BridgesDefined,
                    scope,
                }))),
            )
        });

        this.libs.extend(special_keyword_libs);
        this
    }
}

#[derive(Trace, Clone)]
pub(crate) struct Registry(pub(crate) Gc<RwLock<RegistryInner>>);

impl Registry {
    pub(crate) fn empty() -> Self {
        Self(Gc::new(RwLock::new(RegistryInner::empty())))
    }

    pub(crate) fn new(rt: &Runtime) -> Self {
        Self(Gc::new(RwLock::new(RegistryInner::new(rt))))
    }

    /// Load bridge functions from a dynamic library (plugin).
    ///
    /// The plugin must export a C function named `scheme_rs_bridges`:
    /// ```c
    /// extern "C" PluginBridges scheme_rs_bridges(void);
    /// ```
    ///
    /// The library handle is kept alive for the lifetime of the registry,
    /// ensuring that function pointers from the plugin remain valid.
    ///
    /// # Safety
    ///
    /// The caller must ensure that `library` was built against the same
    /// version of scheme-rs as the host.
    #[cfg(feature = "plugins")]
    pub fn load_plugin(
        &self,
        rt: &Runtime,
        library: libloading::Library,
    ) -> Result<(), Exception> {
        let bridges: &[BridgeFn] = unsafe {
            let func: libloading::Symbol<extern "C" fn() -> PluginBridges> = library
                .get(b"scheme_rs_bridges")
                .map_err(|e| {
                    Exception::error(format!(
                        "plugin does not export scheme_rs_bridges: {e}"
                    ))
                })?;
            let result = func();
            std::slice::from_raw_parts(result.ptr, result.len)
        };

        let mut inner = self.0.write();
        inner.register_bridges(rt, bridges.iter());
        inner.plugins.push(PluginHandle(library));
        Ok(())
    }

    fn mark_as_loading(&self, name: &[Symbol]) {
        self.0.write().loading.insert(name.to_vec());
    }

    #[maybe_async]
    pub(crate) fn def_lib(&self, rt: &Runtime, lib: &str, path: &str) -> Result<(), Exception> {
        let form = Syntax::from_str(lib, Some(path))?;
        let form = match form.as_list() {
            Some([form, end]) if end.is_null() => form,
            _ => return Err(Exception::error("library is malformed")),
        };
        let spec = LibrarySpec::parse(form)?;
        let name = spec.name.name.clone();
        let lib = maybe_await!(TopLevelEnvironment::from_spec(
            rt,
            spec,
            PathBuf::from(path),
        ))?;
        let mut this_mut = self.0.write();
        this_mut.libs.insert(name, lib);
        Ok(())
    }

    // TODO: This function is quite messy, so it would be nice to do a little
    // clean up on it.
    #[maybe_async]
    fn load_lib(&self, rt: &Runtime, name: &[Symbol]) -> Result<TopLevelEnvironment, Exception> {
        let scope = if let Some(lib) = self.0.read().libs.get(name) {
            if !matches!(*lib.get_state(), LibraryState::BridgesDefined) {
                return Ok(lib.clone());
            }
            lib.0.read().scope
        } else {
            Scope::new()
        };

        // Check to see that we're not currently loading the library. Circular
        // dependencies are not allowed. We should probably support them at some
        // point to some degree.
        if self.0.read().loading.contains(name) {
            return Err(error::circular_dependency());
        }

        // Load the library and insert it into the registry.
        self.mark_as_loading(name);
        const DEFAULT_LOAD_PATH: &str = "~/.scheme-rs";

        // Get the suffix:
        let path_suffix = name.iter().copied().map(Symbol::to_str).collect::<Vec<_>>();
        let path_suffix = path_suffix.join("/");

        // Check the current path first:
        let curr_path = std::env::current_dir()
            .expect("If we can't get the current working directory, we can't really do much");
        let lib = if cfg!(feature = "load-libraries-from-fs")
            && let Some(lib) = maybe_await!(load_lib_from_dir(rt, &curr_path, &path_suffix, scope))?
        {
            lib
        } else {
            // Try from the load path
            let path = PathBuf::from(
                std::env::var("SCHEME_RS_LOAD_PATH")
                    .unwrap_or_else(|_| DEFAULT_LOAD_PATH.to_string()),
            );

            if cfg!(feature = "load-libraries-from-fs")
                && let Some(lib) = maybe_await!(load_lib_from_dir(rt, &path, &path_suffix, scope))?
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
                        rt,
                        spec,
                        PathBuf::from(file_name),
                        scope
                    ))?
                } else if let Some(lib) = self.0.read().libs.get(name) {
                    lib.0.write().state = LibraryState::Invoked;
                    lib.clone()
                } else {
                    return Err(error::library_not_found());
                }
            }
        };
        let mut this_mut = self.0.write();
        this_mut.libs.insert(name.to_vec(), lib.clone());
        this_mut.loading.remove(name);
        Ok(lib)
    }

    /// Load a set of symbols from a library with the given import set.
    #[cfg(not(feature = "async"))]
    pub(crate) fn import<'b, 'a: 'b>(
        &'a self,
        rt: &'b Runtime,
        import_set: ImportSet,
    ) -> ImportIter<'b> {
        self.import_inner(rt, import_set)
    }

    /// Load a set of symbols from a library with the given import set.
    #[cfg(feature = "async")]
    pub(crate) fn import<'b, 'a: 'b>(
        &'a self,
        rt: &'b Runtime,
        import_set: ImportSet,
    ) -> ImportIterFuture<'b> {
        Box::pin(self.import_inner(rt, import_set))
    }

    #[maybe_async]
    pub(crate) fn import_inner<'b, 'a: 'b>(
        &'a self,
        rt: &'b Runtime,
        import_set: ImportSet,
    ) -> ImportIter<'b> {
        match import_set {
            ImportSet::Library(lib_import) => {
                let lib = maybe_await!(self.load_lib(rt, &lib_import.name)).map_err(|err| {
                    let lib_name = lib_import
                        .name
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>();
                    let lib_name = format!("({})", lib_name.join(" "));
                    err.add_condition(ImportError::new(lib_name))
                })?;

                if let TopLevelKind::Libary { name, .. } = &*lib.get_kind()
                    && !lib_import.version_ref.matches(&name.version)
                {
                    return Err(error::version_mismatch(&lib_import.version_ref, name));
                }

                let exports = {
                    lib.0
                        .read()
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
                })) as DynIter<'b>)
            }
            ImportSet::Only { set, allowed } => Ok(Box::new(
                maybe_await!(self.import(rt, *set))?
                    .filter(move |(import, _)| allowed.contains(import)),
            ) as DynIter<'b>),
            ImportSet::Except { set, disallowed } => Ok(Box::new(
                maybe_await!(self.import(rt, *set))?
                    .filter(move |(import, _)| !disallowed.contains(import)),
            ) as DynIter<'b>),
            ImportSet::Prefix { set, prefix } => {
                let prefix = prefix.to_str();
                Ok(Box::new(
                    maybe_await!(self.import(rt, *set))?.map(move |(name, import)| {
                        (
                            Symbol::intern(&format!("{prefix}{}", name.to_str())),
                            import,
                        )
                    }),
                ) as DynIter<'b>)
            }
            ImportSet::Rename { set, mut renames } => Ok(Box::new(
                maybe_await!(self.import(rt, *set))?
                    .map(move |(name, import)| (renames.remove(&name).unwrap_or(name), import)),
            ) as DynIter<'b>),
        }
    }
}

type DynIter<'a> = Box<dyn Iterator<Item = (Symbol, Import)> + 'a>;
type ImportIter<'b> = Result<DynIter<'b>, Exception>;
#[cfg(feature = "async")]
type ImportIterFuture<'b> = BoxFuture<'b, ImportIter<'b>>;

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

/// Attempt to load a library from the directory, returning None if no such file exists.
#[maybe_async]
fn load_lib_from_dir(
    rt: &Runtime,
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
            TopLevelEnvironment::from_spec_with_scope(rt, spec, path, scope,)
        )?));
    }

    Ok(None)
}
