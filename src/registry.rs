//! A Registry is a collection of libraries.

use crate::{
    ast::{Definition, DefinitionBody, LibraryName, Literal, ParseAstError}, cps::Compile, env::{Environment, Macro, Top}, exception::Condition, gc::{Gc, Trace}, parse::ParseSyntaxError, proc::{BridgePtr, Closure, FuncDebugInfo, FuncPtr}, runtime::Runtime, symbols::Symbol, syntax::{Identifier, Span, Syntax}, value::Value
};
use std::{collections::{HashMap, HashSet}, sync::Arc};

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
    initializer: fn(lib: &Gc<Top>),
}

#[derive(Trace)]
pub struct Registry {
    libs: HashMap<LibraryName, Gc<Top>>,
}

inventory::collect!(Initializer);

impl Registry {
    /// Construct a Registry with all of the available bridge functions present but no external libraries imported.
    // TODO: This should probably return a result.
    pub async fn new(runtime: &Gc<Runtime>) -> Self {
        // This should probably be moved to a once cell.
        let mut libs = HashMap::<LibraryName, Gc<Top>>::default();

        // Import the bridge functions:

        for bridge_fn in inventory::iter::<BridgeFn>() {
            let debug_info = Arc::new(FuncDebugInfo::from_bridge_fn(
                bridge_fn.name,
                bridge_fn.debug_info,
            ));
            let lib_name = LibraryName::from_str(bridge_fn.lib_name, None).unwrap();
            let lib = libs
                .entry(lib_name)
                .or_insert_with(|| Gc::new(Top::library()));
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
    }

    pub fn import(&self, lib: &str) -> Option<Gc<Top>> {
        let lib_name = LibraryName::from_str(lib, None).unwrap();
        self.libs.get(&lib_name).cloned()
    }
}

/*
#[derive(Trace)]
pub struct Imports {
    // registry: Gc<Registry>,
    imports: HashMap<Identifier, Gc<Library>>,
}
*/

#[derive(Trace)]
pub struct Library {
    imports: HashMap<Identifier, Gc<Library>>,
    exports: HashSet<Identifier>,
    state: LibraryState, 
    vars: HashMap<Identifier, Gc<Value>>,
    keywords: HashMap<Identifier, Macro>,
}

impl Library {
    pub fn new(
        imports: HashMap<Identifier, Gc<Library>>,
        exports: HashSet<Identifier>,
        body: Vec<Syntax>
    ) -> Self {
        Self {
            imports,
            exports,
            state: LibraryState::Unexpanded(body),
            vars: HashMap::default(),
            keywords: HashMap::default(),
        }
    }

    pub async fn maybe_expand(&mut self) -> Result<(), ParseAstError> {
        if let LibraryState::Unexpanded(ref body) = self.state {
            let expanded = Definition::parse(todo!(), body.as_slice(), todo!(), todo!()).await?;
            self.state = LibraryState::Expanded(expanded);
        }
        Ok(())
    }

    pub async fn maybe_invoke(&mut self) -> Result<(), Condition> {
        self.maybe_expand()?;
        if let LibraryState::Expanded(ref defs) = self.state {
            
        }
    }
}

pub enum LibraryState {
    Unexpanded(Vec<Syntax>),
    Expanded(Definition),
    Invoked,
}
