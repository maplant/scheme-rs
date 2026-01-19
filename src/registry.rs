//! Global collection of libraries associated with a [`Runtime`]

use crate::{
    ast::{ImportSet, LibraryName, LibrarySpec, SpecialKeyword, Version},
    env::{
        Export, Import, LibraryState, TopLevelEnvironment, TopLevelEnvironmentInner, TopLevelKind,
    },
    exceptions::{Exception, ImportError},
    gc::{Gc, Trace},
    proc::{BridgePtr, FuncPtr, ProcDebugInfo, Procedure},
    runtime::Runtime,
    symbols::Symbol,
    syntax::{Identifier, Syntax},
    value::Value,
};

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::Arc,
};

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

    pub(crate) fn name_bound_multiple_times(name: Symbol) -> Exception {
        Exception::from(Message::new(format!("`{name}` bound multiple times")))
    }

    // TODO: Include dependency chain that lead to this error
    pub(super) fn circular_dependency() -> Exception {
        Exception::from(Message::new("circular dependency"))
    }
}

#[doc(hidden)]
pub enum Bridge {
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
struct Stdlib;

#[derive(Trace, Default)]
pub(crate) struct RegistryInner {
    libs: HashMap<Vec<Symbol>, TopLevelEnvironment>,
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
            syms: HashMap<Identifier, Gc<RwLock<Value>>>,
        }
        let mut libs = HashMap::<Vec<Symbol>, Lib>::default();

        // Import the bridge functions:
        for bridge_fn in inventory::iter::<BridgeFn>() {
            let debug_info = Arc::new(ProcDebugInfo::from_bridge_fn(
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
                Gc::new(RwLock::new(Value::from(Procedure::with_debug_info(
                    rt.clone(),
                    Vec::new(),
                    match bridge_fn.wrapper {
                        Bridge::Sync(func) => FuncPtr::Bridge(func),
                        #[cfg(feature = "async")]
                        Bridge::Async(func) => FuncPtr::AsyncBridge(func),
                    },
                    bridge_fn.num_args,
                    bridge_fn.variadic,
                    Some(debug_info),
                )))),
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
                }))),
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
                let lib_inner = TopLevelEnvironmentInner {
                    rt: rt.clone(),
                    kind: TopLevelKind::Libary {
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
                (name, TopLevelEnvironment(Gc::new(RwLock::new(lib_inner))))
            })
            .chain(special_keyword_libs)
            .collect();

        Self {
            libs,
            loading: HashSet::default(),
        }
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

    fn mark_as_loading(&self, name: &[Symbol]) {
        self.0.write().loading.insert(name.to_vec());
    }

    #[maybe_async]
    pub(crate) fn def_lib(&self, rt: &Runtime, lib: &str, path: &str) -> Result<(), Exception> {
        let form = Syntax::from_str(lib, Some(path))?;
        let form = match form.as_list() {
            Some([form, Syntax::Null { .. }]) => form,
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
        if let Some(lib) = self.0.read().libs.get(name) {
            return Ok(lib.clone());
        }

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
            && let Some(lib) = maybe_await!(load_lib_from_dir(rt, &curr_path, &path_suffix))?
        {
            lib
        } else {
            // Try from the load path
            let path = PathBuf::from(
                std::env::var("SCHEME_RS_LOAD_PATH")
                    .unwrap_or_else(|_| DEFAULT_LOAD_PATH.to_string()),
            );

            if cfg!(feature = "load-libraries-from-fs")
                && let Some(lib) = maybe_await!(load_lib_from_dir(rt, &path, &path_suffix))?
            {
                lib
            } else {
                // Finally, try the embedded Stdlib
                let file_name = format!("{path_suffix}.sls");
                let Some(lib) = Stdlib::get(&file_name) else {
                    return Err(error::library_not_found());
                };
                let contents = std::str::from_utf8(&lib.data).unwrap();
                let form = Syntax::from_str(contents, Some(&file_name))?;
                let form = match form.as_list() {
                    Some([form, Syntax::Null { .. }]) => form,
                    _ => return Err(Exception::error("library is malformed")),
                };
                let spec = LibrarySpec::parse(form)?;
                maybe_await!(TopLevelEnvironment::from_spec(
                    rt,
                    spec,
                    PathBuf::from(file_name)
                ))?
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
                maybe_await!(self.import(rt, *set))?
                    .filter(move |(import, _)| allowed.contains(import)),
            ) as DynIter<'b>),
            ImportSet::Except { set, disallowed } => Ok(Box::new(
                maybe_await!(self.import(rt, *set))?
                    .filter(move |(import, _)| !disallowed.contains(import)),
            ) as DynIter<'b>),
            ImportSet::Prefix { set, prefix } => {
                let prefix = prefix.sym.to_str();
                Ok(Box::new(
                    maybe_await!(self.import(rt, *set))?
                        .map(move |(name, import)| (name.prefix(&prefix), import)),
                ) as DynIter<'b>)
            }
            ImportSet::Rename { set, mut renames } => Ok(Box::new(
                maybe_await!(self.import(rt, *set))?
                    .map(move |(name, import)| (renames.remove(&name).unwrap_or(name), import)),
            ) as DynIter<'b>),
        }
    }
}

type DynIter<'a> = Box<dyn Iterator<Item = (Identifier, Import)> + 'a>;
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
            Some([form, Syntax::Null { .. }]) => form,
            _ => return Err(Exception::error("library is malformed")),
        };
        let spec = LibrarySpec::parse(form)?;
        return Ok(Some(maybe_await!(TopLevelEnvironment::from_spec(
            rt, spec, path
        ))?));
    }

    Ok(None)
}
