//! a Registry is a collection of libraries.

use crate::{
    ast::{
        Definition, DefinitionBody, ImportSet, LibraryName, LibrarySpec, Literal, ParseAstError,
    },
    cps::Compile,
    env::{self, Environment, Global, Macro},
    exception::Condition,
    gc::{Gc, Trace},
    parse::ParseSyntaxError,
    proc::{Application, BridgePtr, Closure, DynamicWind, FuncDebugInfo, FuncPtr},
    runtime::Runtime,
    symbols::Symbol,
    syntax::{Identifier, Span, Syntax},
    value::Value,
};
use std::{
    collections::{HashMap, HashSet, hash_map::Entry},
    sync::Arc,
};

use futures::future::BoxFuture;
pub use scheme_rs_macros::bridge;

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

#[derive(Copy, Clone)]
pub struct Initializer {
    lib_name: &'static str,
    initializer: fn(lib: &Library),
}

inventory::collect!(Initializer);

#[derive(Trace, Default)]
pub struct RegistryInner {
    libs: HashMap<LibraryName, Library>,
}

impl RegistryInner {
    /// Construct an empty registry
    pub fn empty() -> Self {
        Self::default()
    }

    /// Construct a Registry with all of the available bridge functions present but no external libraries imported.
    // TODO: This should probably return a result.
    pub async fn new(runtime: &Runtime) -> Self {
        /*
        // This should probably be moved to a once cell.
        let mut libs = HashMap::<LibraryName, Gc<dyn Top>>::default();

        // Import the bridge functions:

        for bridge_fn in inventory::iter::<BridgeFn>() {
            let debug_info = Arc::new(FuncDebugInfo::from_bridge_fn(
                bridge_fn.name,
                bridge_fn.debug_info,
            ));
            let lib_name = LibraryName::from_str(bridge_fn.lib_name, None).unwrap();
            let lib = libs
                .entry(lib_name)
                .or_insert_with(|| todo!());// Gc::new(Top::library()));
            let mut lib = lib.write();
            lib.def_var(
                Identifier::new(bridge_fn.name),
                Value::from(Closure::new(
                    runtime.clone(),
                    Vec::new(),
                    Vec::new(),
                    FuncPtr::Bridge(bridge_fn.wrapper),
                    bridge_fn.num_args,
                    bridge_fn.variadic,
                    Some(debug_info),
                )),
            );
        }

        // Run the initializers:

        for initializer in inventory::iter::<Initializer>() {
            let lib_name = LibraryName::from_str(initializer.lib_name, None).unwrap();
            let lib = libs
                .entry(lib_name)
                .or_insert_with(|| Gc::new(Top::library()));
            (initializer.initializer)(lib);
        }

        // Import the stdlib:

        let base_lib = libs
            .entry(LibraryName::from_str("(base)", None).unwrap())
            .or_insert_with(|| Gc::new(Top::library()));
        let base_env = Environment::Top(base_lib.clone());
        let sexprs = Syntax::from_str(include_str!("stdlib.scm"), Some("stdlib.scm")).unwrap();
        let base = DefinitionBody::parse(runtime, &sexprs, &base_env, &Span::default())
            .await
            .unwrap();
        let compiled = base.compile_top_level();
        let closure = runtime.compile_expr(compiled).await.unwrap();
        closure.call(&[]).await.unwrap();

        Self { libs }
         */
        todo!()
    }

    fn import<'b, 'a: 'b>(
        &'a self,
        import_set: ImportSet,
    ) -> Result<Box<dyn Iterator<Item = (Identifier, Import)> + 'a>, ImportError> {
        match import_set {
            ImportSet::Library(lib) => {
                // TODO: Load the library if it doesn't exist
                let lib = self.libs.get(&lib).ok_or(ImportError::LibraryNotFound)?;
                let exports = { lib.0.read().exports.values().cloned().collect::<Vec<_>>() };
                Ok(Box::new(exports.into_iter().map(move |exp| {
                    (
                        exp.rename.clone(),
                        Import {
                            rename: exp.rename.clone(),
                            origin: if let Some(redirect) = exp.origin {
                                redirect.clone()
                            } else {
                                lib.clone()
                            },
                        },
                    )
                })))
            }
            ImportSet::Only { set, allowed } => Ok(Box::new(
                self.import(*set)?
                    .filter(move |(import, _)| allowed.contains(import)),
            )),
            ImportSet::Except { set, disallowed } => Ok(Box::new(
                self.import(*set)?
                    .filter(move |(import, _)| !disallowed.contains(import)),
            )),
            ImportSet::Prefix { set, prefix } => {
                let prefix = prefix.sym.to_str();
                Ok(Box::new(
                    self.import(*set)?
                        .map(move |(name, import)| (name.prefix(&prefix), import)),
                ))
            }
            ImportSet::Rename { set, mut renames } => {
                Ok(Box::new(self.import(*set)?.map(move |(name, import)| {
                    (renames.remove(&name).unwrap_or(name), import)
                })))
            }
        }
    }

    /*
    pub fn import(&self, lib: &str) -> Option<Gc<Top>> {
        let lib_name = LibraryName::from_str(lib, None).unwrap();
        self.libs.get(&lib_name).cloned()
    }
    */
}

pub enum ImportError {
    LibraryNotFound,
}

#[derive(Trace, Clone)]
pub struct Registry(pub(crate) Gc<RegistryInner>);

/*
#[derive(Trace)]
pub struct Imports {
    // registry: Gc<Registry>,
    imports: HashMap<Identifier, Gc<Library>>,
}
*/

#[derive(Trace)]
pub(crate) struct LibraryInner {
    rt: Runtime,
    exports: HashMap<Identifier, Export>,
    imports: HashMap<Identifier, Import>,
    state: LibraryState,
    vars: HashMap<Identifier, Gc<Value>>,
    keywords: HashMap<Identifier, Macro>,
}

impl LibraryInner {
    pub(crate) fn new(
        rt: &Runtime,
        imports: HashMap<Identifier, Library>,
        exports: HashMap<Identifier, Identifier>,
        body: Vec<Syntax>,
    ) -> Self {
        let exports = exports
            .into_iter()
            .map(|(name, rename)| {
                let origin = imports.get(&name).cloned();
                (name, Export { rename, origin })
            })
            .collect();

        Self {
            rt: rt.clone(),
            imports: HashMap::default(),
            exports,
            state: LibraryState::Unexpanded(body),
            vars: HashMap::default(),
            keywords: HashMap::default(),
        }
    }
}

#[derive(Trace)]
pub struct Import {
    rename: Identifier,
    origin: Library,
}

#[derive(Trace, Clone)]
pub struct Export {
    rename: Identifier,
    origin: Option<Library>,
}

#[derive(Trace, Clone)]
pub struct Library(pub(crate) Gc<LibraryInner>);

impl Library {
    pub fn new(rt: &Runtime, spec: LibrarySpec) -> Self {
        todo!()
    }

    pub(crate) async fn maybe_expand(&self) -> Result<(), ParseAstError> {
        let body = {
            let mut this = self.0.write();
            if let LibraryState::Unexpanded(body) = &mut this.state {
                std::mem::take(body)
            } else {
                return Ok(());
            }
        };
        let rt = { self.0.read().rt.clone() };
        let env = Environment::from(self.clone());
        let expanded = DefinitionBody::parse_lib_body(&rt, &body, &env, body[0].span()).await?;
        self.0.write().state = LibraryState::Expanded(expanded);
        Ok(())
    }

    pub(crate) async fn maybe_invoke(&self) -> Result<(), Condition> {
        self.maybe_expand().await?;
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
        let closure = rt.compile_expr(compiled).await.unwrap();
        let _ = Application::new(closure, Vec::new(), None, DynamicWind::default(), None)
            .eval()
            .await?;
        self.0.write().state = LibraryState::Invoked;
        Ok(())
    }

    pub fn is_repl(&self) -> bool {
        false
    }

    pub(crate) fn def_var(&self, name: Identifier, value: Value) -> env::Global {
        let mut this = self.0.write();
        match this.vars.entry(name.clone()) {
            Entry::Occupied(occup) => Global::new(name, occup.get().clone()),
            Entry::Vacant(vacant) => Global::new(name, vacant.insert(Gc::new(value)).clone()),
        }
    }

    pub(crate) fn def_keyword(&self, keyword: Identifier, mac: Macro) {
        let mut this = self.0.write();
        this.keywords.insert(keyword, mac);
    }

    pub(crate) fn fetch_var<'a>(
        &'a self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Option<env::Global>> {
        // Check this library
        let this = self.0.read();
        if let Some(var) = this.vars.get(name) {
            let var = var.clone();
            return Box::pin(async move { Some(Global::new(name.clone(), var)) });
        }

        // Check our imports
        let Some(Import { origin, rename }) = this.imports.get(name) else {
            return Box::pin(async { None });
        };

        let rename = rename.clone();
        let import = origin.clone();
        Box::pin(async move {
            import
                .maybe_invoke()
                .await
                .expect("Oh geez, fetching variables can error now");

            let Export { rename, .. } = import.0.read().exports.get(&rename).unwrap().clone();
            import.fetch_var(&rename).await
        })
    }

    pub(crate) fn fetch_keyword<'a>(
        &'a self,
        keyword: &'a Identifier,
    ) -> BoxFuture<'a, Option<Macro>> {
        // Check this library
        let this = self.0.read();
        if let Some(mac) = this.keywords.get(keyword) {
            let mac = mac.clone();
            return Box::pin(async move { Some(mac) });
        }

        // Check our imports
        let Some(Import { origin, rename }) = this.imports.get(keyword) else {
            return Box::pin(async { None });
        };

        let rename = rename.clone();
        let import = origin.clone();
        Box::pin(async move {
            import
                .maybe_invoke()
                .await
                .expect("Oh geez, fetching variables can error now");

            let Export { rename, .. } = import.0.read().exports.get(&rename).unwrap().clone();
            import.fetch_keyword(&rename).await
        })
    }
}

// TODO: Use these states to detect circular dependencies when we do our DFS
#[derive(Trace)]
pub enum LibraryState {
    Invalid,
    Unexpanded(Vec<Syntax>),
    Expanded(DefinitionBody),
    Invoked,
}
