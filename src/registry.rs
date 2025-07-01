//! A Registry is a collection of libraries.

use crate::{
    ast::{DefinitionBody, Literal, ParseAstError},
    cps::Compile,
    env::{Environment, Top},
    gc::Gc,
    parse::ParseSyntaxError,
    proc::{BridgePtr, Closure, FuncPtr},
    runtime::Runtime,
    syntax::{Identifier, Span, Syntax},
    value::Value,
};
pub use scheme_rs_macros::bridge;
use std::collections::HashMap;

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct LibraryName {
    name: Vec<String>,
    version: Version,
}

impl LibraryName {
    fn parse(syn: &Syntax) -> Result<Self, ParseAstError> {
        match syn.as_list() {
            Some(
                [
                    name @ ..,
                    Syntax::List {
                        list: version,
                        span,
                    },
                    Syntax::Null { .. },
                ],
            ) => Ok(Self {
                name: list_to_name(name)?,
                version: Version::parse(version, span)?,
            }),
            Some([name @ .., Syntax::Null { .. }]) => Ok(Self {
                name: list_to_name(name)?,
                version: Version::default(),
            }),
            _ => Err(ParseAstError::BadForm(syn.span().clone())),
        }
    }

    fn from_str<'a>(
        s: &'a str,
        file_name: Option<&str>,
    ) -> Result<Self, ParseLibraryNameError<'a>> {
        let syn = Syntax::from_str(s, file_name)?;
        Ok(Self::parse(&syn[0])?)
    }
}

fn list_to_name(name: &[Syntax]) -> Result<Vec<String>, ParseAstError> {
    name.iter()
        .map(|name| {
            if let Syntax::Identifier { ident, .. } = name {
                Ok(ident.name.clone())
            } else {
                Err(ParseAstError::ExpectedIdentifier(name.span().clone()))
            }
        })
        .collect()
}

#[derive(Debug)]
pub enum ParseLibraryNameError<'a> {
    ParseSyntaxError(ParseSyntaxError<'a>),
    ParseAstError(ParseAstError),
}

impl<'a> From<ParseSyntaxError<'a>> for ParseLibraryNameError<'a> {
    fn from(pse: ParseSyntaxError<'a>) -> Self {
        Self::ParseSyntaxError(pse)
    }
}

impl From<ParseAstError> for ParseLibraryNameError<'_> {
    fn from(pae: ParseAstError) -> Self {
        Self::ParseAstError(pae)
    }
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Default)]
pub struct Version {
    version: Vec<usize>,
}

impl Version {
    fn parse(syn: &[Syntax], span: &Span) -> Result<Self, ParseAstError> {
        match syn {
            [version @ .., Syntax::Null { .. }] => {
                let version: Result<Vec<usize>, _> = version
                    .iter()
                    .map(|subvers| {
                        if let Syntax::Literal {
                            literal: Literal::Number(num),
                            ..
                        } = subvers
                        {
                            num.try_into().map_err(ParseAstError::ExpectedInteger)
                        } else {
                            Err(ParseAstError::ExpectedNumber(subvers.span().clone()))
                        }
                    })
                    .collect();
                Ok(Self { version: version? })
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

pub enum VersionReference {
    SubVersions(Vec<SubVersionReference>),
    And(Vec<VersionReference>),
    Or(Vec<VersionReference>),
    Not(Box<VersionReference>),
}

pub enum SubVersionReference {
    SubVersion(u32),
    Gte(Vec<SubVersionReference>),
    Lte(Vec<SubVersionReference>),
    And(Vec<SubVersionReference>),
    Or(Vec<SubVersionReference>),
    Not(Box<SubVersionReference>),
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

#[derive(Copy, Clone)]
pub struct Initializer {
    lib_name: &'static str,
    initializer: fn(lib: &Gc<Top>),
}

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
            let debug_info_id = runtime.write().debug_info.new_function_debug_info(
                crate::proc::FunctionDebugInfo::from_bridge_fn(
                    bridge_fn.name,
                    bridge_fn.debug_info,
                ),
            );
            let lib_name = LibraryName::from_str(bridge_fn.lib_name, None).unwrap();
            let lib = libs
                .entry(lib_name)
                .or_insert_with(|| Gc::new(Top::library()));
            let mut lib = lib.write();
            lib.def_var(
                Identifier::new(bridge_fn.name.to_string()),
                Value::from(Closure::new(
                    runtime.clone(),
                    Vec::new(),
                    Vec::new(),
                    FuncPtr::Bridge(bridge_fn.wrapper),
                    bridge_fn.num_args,
                    bridge_fn.variadic,
                    Some(debug_info_id),
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
