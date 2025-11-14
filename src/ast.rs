//! Data structures for expanding and representing Scheme code.

use crate::{
    cps::{Compile, PrimOp},
    env::{Environment, Local, Var},
    exceptions::Condition,
    expand::{SyntaxRule, Template},
    gc::Trace,
    num::{Number, NumberToUsizeError},
    proc::Procedure,
    registry::ImportError,
    runtime::Runtime,
    symbols::Symbol,
    syntax::{FullyExpanded, Identifier, Span, Syntax, parse::ParseSyntaxError},
    value::Value,
};
use either::Either;

use derive_more::From;
use scheme_rs_macros::{maybe_async, maybe_await};
use std::{
    collections::{HashMap, HashSet},
    fmt,
    sync::Arc,
};

#[cfg(feature = "async")]
use futures::future::BoxFuture;

#[derive(Debug)]
pub enum ParseAstError {
    /// The most general error. Something just looks bad.
    ///
    /// This error type should be avoided, and instead one should use a more
    /// specific error type, or create one. Lord knows I have not really taken
    /// that advice.
    BadForm(Span),

    ExpectedArgument(Span),
    ExpectedBody(Span),
    ExpectedIdentifier(Span),
    ExpectedNumber(Span),
    ExpectedVariableTransformer,
    ExpectedInteger(NumberToUsizeError),
    ExpectedExportSpec(Span),
    ExpectedImportSpec(Span),
    ExpectedLibraryKeyword(Span),
    ExpectedList(Span),
    UnexpectedArgument(Span),
    UnexpectedDefinition(Span),
    UnexpectedImport(Span),
    UnexpectedEmptyList(Span),

    ImportError {
        span: Span,
        error: Box<ImportError>,
    },
    CannotSetImmutableVar {
        span: Span,
        name: Identifier,
    },
    NumberToUsizeError {
        span: Span,
        error: NumberToUsizeError,
    },
    UndefinedVariable {
        span: Span,
        ident: Identifier,
    },
    ParentSpecifiedMultipleTimes {
        first: Span,
        second: Span,
    },
    MultipleFieldsClauses {
        first: Span,
        second: Span,
    },
    NameBoundMultipleTimes {
        ident: Identifier,
        first: Span,
        second: Span,
    },

    RaisedValue(Value),
}

impl From<Value> for ParseAstError {
    fn from(raised: Value) -> Self {
        Self::RaisedValue(raised)
    }
}

impl From<Condition> for ParseAstError {
    fn from(raised: Condition) -> Self {
        Self::RaisedValue(Value::from(raised))
    }
}

/// Special keywords are keywords that the compiler needs to know about in order
/// to create an AST.
#[derive(Copy, Clone, Trace, Debug)]
pub enum SpecialKeyword {
    Undefined,
    Begin,
    Lambda,
    Let,
    LetSyntax,
    LetRecSyntax,
    If,
    And,
    Or,
    Quote,
    Syntax,
    SyntaxCase,
    Set,
    Define,
    DefineSyntax,
    Import,
}

pub struct LibrarySpec {
    pub(crate) name: LibraryName,
    pub(crate) exports: ExportSpec,
    pub(crate) imports: ImportSpec,
    pub(crate) body: Vec<Syntax>,
}

impl LibrarySpec {
    pub fn parse(syn: &Syntax) -> Result<Self, ParseAstError> {
        match syn.as_list() {
            Some(
                [
                    Syntax::Identifier {
                        ident: library_decl,
                        span: library_decl_span,
                        ..
                    },
                    library_name,
                    body @ ..,
                    Syntax::Null { .. },
                ],
            ) => {
                if library_decl != "library" {
                    return Err(ParseAstError::ExpectedLibraryKeyword(
                        library_decl_span.clone(),
                    ));
                }
                let mut exports = ExportSpec::default();
                let mut imports = ImportSpec::default();
                let mut body = body;
                while let Some(spec) = body.first() {
                    if spec.has_car("export") {
                        exports.join(ExportSpec::parse(spec).unwrap());
                    } else if spec.has_car("import") {
                        imports.join(ImportSpec::parse(spec).unwrap());
                    } else {
                        break;
                    }
                    body = &body[1..];
                }
                Ok(Self {
                    name: LibraryName::parse(library_name)?,
                    exports,
                    imports,
                    body: body.to_vec(),
                })
            }
            _ => Err(ParseAstError::BadForm(syn.span().clone())),
        }
    }
}

#[derive(Clone, Default, PartialEq, Eq, Hash, Trace, Debug)]
pub struct LibraryName {
    pub name: Vec<Symbol>,
    pub version: Version,
}

impl LibraryName {
    pub fn parse(syn: &Syntax) -> Result<Self, ParseAstError> {
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

    pub fn from_str(s: &str, file_name: Option<&str>) -> Result<Self, ParseLibraryNameError> {
        let syn = Syntax::from_str(s, file_name)?;
        Ok(Self::parse(&syn[0])?)
    }
}

fn list_to_name(name: &[Syntax]) -> Result<Vec<Symbol>, ParseAstError> {
    name.iter()
        .map(|name| {
            if let Syntax::Identifier { ident, .. } = name {
                Ok(ident.sym)
            } else {
                Err(ParseAstError::ExpectedIdentifier(name.span().clone()))
            }
        })
        .collect()
}

#[derive(Debug)]
pub enum ParseLibraryNameError {
    ParseSyntaxError(ParseSyntaxError),
    ParseAstError(ParseAstError),
}

impl From<ParseSyntaxError> for ParseLibraryNameError {
    fn from(pse: ParseSyntaxError) -> Self {
        Self::ParseSyntaxError(pse)
    }
}

impl From<ParseAstError> for ParseLibraryNameError {
    fn from(pae: ParseAstError) -> Self {
        Self::ParseAstError(pae)
    }
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Default, Trace, Debug)]
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

impl<const N: usize> From<[usize; N]> for Version {
    fn from(value: [usize; N]) -> Self {
        Self {
            version: Vec::from(value),
        }
    }
}

#[derive(Debug)]
pub enum VersionReference {
    SubVersions(Vec<SubVersionReference>),
    And(Vec<VersionReference>),
    Or(Vec<VersionReference>),
    Not(Box<VersionReference>),
}

impl VersionReference {
    fn parse(syn: &Syntax) -> Result<Self, ParseAstError> {
        match syn.as_list() {
            Some(
                [
                    Syntax::Identifier { ident: kw, .. },
                    version_refs @ ..,
                    Syntax::Null { .. },
                ],
            ) if kw == "and" => {
                let version_refs = version_refs
                    .iter()
                    .map(VersionReference::parse)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self::And(version_refs))
            }
            Some(
                [
                    Syntax::Identifier { ident: kw, .. },
                    version_refs @ ..,
                    Syntax::Null { .. },
                ],
            ) if kw == "or" => {
                let version_refs = version_refs
                    .iter()
                    .map(VersionReference::parse)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self::Or(version_refs))
            }
            Some(
                [
                    Syntax::Identifier { ident: kw, .. },
                    version_ref,
                    Syntax::Null { .. },
                ],
            ) if kw == "not" => {
                let version_ref = VersionReference::parse(version_ref)?;
                Ok(Self::Not(Box::new(version_ref)))
            }
            Some([subversion_refs @ .., Syntax::Null { .. }]) => {
                let subversion_refs = subversion_refs
                    .iter()
                    .map(SubVersionReference::parse)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self::SubVersions(subversion_refs))
            }
            None => Err(ParseAstError::ExpectedList(syn.span().clone())),
            _ => Err(ParseAstError::BadForm(syn.span().clone())),
        }
    }
}

#[derive(Debug)]
pub enum SubVersionReference {
    SubVersion(usize),
    Gte(usize),
    Lte(usize),
    And(Vec<SubVersionReference>),
    Or(Vec<SubVersionReference>),
    Not(Box<SubVersionReference>),
}

impl SubVersionReference {
    fn parse(syn: &Syntax) -> Result<Self, ParseAstError> {
        match syn {
            Syntax::Literal {
                literal: Literal::Number(num),
                ..
            } => Ok(Self::SubVersion(num.try_into().map_err(|error| {
                ParseAstError::NumberToUsizeError {
                    span: syn.span().clone(),
                    error,
                }
            })?)),
            _ => match syn.as_list() {
                Some(
                    [
                        Syntax::Identifier { ident: kw, .. },
                        Syntax::Literal {
                            literal: Literal::Number(num),
                            ..
                        },
                        Syntax::Null { .. },
                    ],
                ) if kw == ">=" => Ok(Self::Gte(num.try_into().map_err(|error| {
                    ParseAstError::NumberToUsizeError {
                        span: syn.span().clone(),
                        error,
                    }
                })?)),
                Some(
                    [
                        Syntax::Identifier { ident: kw, .. },
                        Syntax::Literal {
                            literal: Literal::Number(num),
                            ..
                        },
                        Syntax::Null { .. },
                    ],
                ) if kw == "<=" => Ok(Self::Lte(num.try_into().map_err(|error| {
                    ParseAstError::NumberToUsizeError {
                        span: syn.span().clone(),
                        error,
                    }
                })?)),
                Some(
                    [
                        Syntax::Identifier { ident: kw, .. },
                        subversion_refs @ ..,
                        Syntax::Null { .. },
                    ],
                ) if kw == "and" => {
                    let subversion_refs = subversion_refs
                        .iter()
                        .map(SubVersionReference::parse)
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Self::And(subversion_refs))
                }
                Some(
                    [
                        Syntax::Identifier { ident: kw, .. },
                        subversion_refs @ ..,
                        Syntax::Null { .. },
                    ],
                ) if kw == "or" => {
                    let subversion_refs = subversion_refs
                        .iter()
                        .map(SubVersionReference::parse)
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Self::Or(subversion_refs))
                }
                Some(
                    [
                        Syntax::Identifier { ident: kw, .. },
                        subversion_ref,
                        Syntax::Null { .. },
                    ],
                ) if kw == "not" => {
                    let subversion_ref = SubVersionReference::parse(subversion_ref)?;
                    Ok(Self::Not(Box::new(subversion_ref)))
                }
                None => Err(ParseAstError::ExpectedList(syn.span().clone())),
                _ => Err(ParseAstError::BadForm(syn.span().clone())),
            },
        }
    }
}

pub enum ExportSet {
    Internal {
        rename: Option<Identifier>,
        ident: Identifier,
    },
    External(ImportSpec),
}

impl ExportSet {
    pub fn parse_rename(syn: &Syntax) -> Result<Self, ParseAstError> {
        match syn.as_list() {
            Some(
                [
                    Syntax::Identifier { ident: from, .. },
                    Syntax::Identifier { ident: to, .. },
                    Syntax::Null { .. },
                ],
            ) => Ok(Self::Internal {
                rename: Some(to.clone()),
                ident: from.clone(),
            }),
            _ => Err(ParseAstError::BadForm(syn.span().clone())),
        }
    }

    pub fn parse(syn: &Syntax) -> Result<Vec<Self>, ParseAstError> {
        match syn {
            Syntax::Identifier { ident, .. } => Ok(vec![Self::Internal {
                rename: None,
                ident: ident.clone(),
            }]),
            Syntax::List { list, .. } => match list.as_slice() {
                [
                    Syntax::Identifier { ident, .. },
                    renames @ ..,
                    Syntax::Null { .. },
                ] if ident == "rename" => Ok(renames
                    .iter()
                    .map(Self::parse_rename)
                    .collect::<Result<Vec<_>, _>>()?),
                [Syntax::Identifier { ident, .. }, .., Syntax::Null { .. }]
                    if ident == "import" =>
                {
                    Ok(vec![Self::External(ImportSpec::parse(syn)?)])
                }
                _ => Err(ParseAstError::BadForm(syn.span().clone())),
            },
            _ => Err(ParseAstError::BadForm(syn.span().clone())),
        }
    }
}

#[derive(Default)]
pub struct ExportSpec {
    pub(crate) export_sets: Vec<ExportSet>,
}

impl ExportSpec {
    pub fn parse(syn: &Syntax) -> Result<Self, ParseAstError> {
        match syn.as_list() {
            Some(
                [
                    Syntax::Identifier {
                        ident: export_decl, ..
                    },
                    exports @ ..,
                    Syntax::Null { .. },
                ],
            ) if export_decl == "export" => Ok(ExportSpec {
                export_sets: exports
                    .iter()
                    .map(ExportSet::parse)
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten()
                    .collect(),
            }),
            _ => Err(ParseAstError::ExpectedExportSpec(syn.span().clone())),
        }
    }

    pub fn join(&mut self, rhs: ExportSpec) {
        self.export_sets.extend(rhs.export_sets);
    }
}

#[derive(Default)]
pub struct ImportSpec {
    pub(crate) import_sets: Vec<ImportSet>,
}

impl ImportSpec {
    pub fn parse(syn: &Syntax) -> Result<Self, ParseAstError> {
        match syn.as_list() {
            Some(
                [
                    Syntax::Identifier {
                        ident: import_decl, ..
                    },
                    imports @ ..,
                    Syntax::Null { .. },
                ],
            ) if import_decl == "import" => Ok(ImportSpec {
                import_sets: imports
                    .iter()
                    .map(|import| ImportSet::parse(discard_for(import)))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            _ => Err(ParseAstError::ExpectedImportSpec(syn.span().clone())),
        }
    }

    pub fn join(&mut self, rhs: ImportSpec) {
        self.import_sets.extend(rhs.import_sets);
    }
}

fn discard_for(syn: &Syntax) -> &Syntax {
    match syn.as_list() {
        Some(
            [
                Syntax::Identifier { ident: for_kw, .. },
                import_set,
                _import_level @ ..,
                Syntax::Null { .. },
            ],
        ) if for_kw == "for" => {
            // We should eventually check the import levels for being well
            // formed, even if we ignore them.
            import_set
        }
        _ => syn,
    }
}

#[derive(Debug)]
pub enum ImportSet {
    Library(LibraryReference),
    Only {
        set: Box<ImportSet>,
        allowed: HashSet<Identifier>,
    },
    Except {
        set: Box<ImportSet>,
        disallowed: HashSet<Identifier>,
    },
    Prefix {
        set: Box<ImportSet>,
        prefix: Identifier,
    },
    Rename {
        set: Box<ImportSet>,
        /// Imported identifiers to rename (from, to).
        renames: HashMap<Identifier, Identifier>,
    },
}

impl ImportSet {
    fn parse(syn: &Syntax) -> Result<Self, ParseAstError> {
        match syn.as_list() {
            Some(
                [
                    Syntax::Identifier {
                        ident: import_type, ..
                    },
                    lib_ref,
                    Syntax::Null { .. },
                ],
            ) if import_type == "library" => Ok(Self::Library(LibraryReference::parse(lib_ref)?)),
            Some(
                [
                    Syntax::Identifier {
                        ident: import_type, ..
                    },
                    import_set,
                    imports @ ..,
                    Syntax::Null { .. },
                ],
            ) if import_type == "only" => {
                let import_set = ImportSet::parse(import_set)?;
                let allowed = imports
                    .iter()
                    .map(|allowed| match allowed {
                        Syntax::Identifier { ident, .. } => Ok(ident.clone()),
                        _ => Err(ParseAstError::ExpectedIdentifier(allowed.span().clone())),
                    })
                    .collect::<Result<HashSet<_>, _>>()?;
                Ok(Self::Only {
                    set: Box::new(import_set),
                    allowed,
                })
            }
            Some(
                [
                    Syntax::Identifier {
                        ident: import_type, ..
                    },
                    import_set,
                    exceptions @ ..,
                    Syntax::Null { .. },
                ],
            ) if import_type == "except" => {
                let import_set = ImportSet::parse(import_set)?;
                let disallowed = exceptions
                    .iter()
                    .map(|disallowed| match disallowed {
                        Syntax::Identifier { ident, .. } => Ok(ident.clone()),
                        _ => Err(ParseAstError::ExpectedIdentifier(disallowed.span().clone())),
                    })
                    .collect::<Result<HashSet<_>, _>>()?;
                Ok(Self::Except {
                    set: Box::new(import_set),
                    disallowed,
                })
            }
            Some(
                [
                    Syntax::Identifier {
                        ident: import_type, ..
                    },
                    import_set,
                    Syntax::Identifier { ident: prefix, .. },
                    Syntax::Null { .. },
                ],
            ) if import_type == "prefix" => {
                let import_set = ImportSet::parse(import_set)?;
                Ok(Self::Prefix {
                    set: Box::new(import_set),
                    prefix: prefix.clone(),
                })
            }
            Some(
                [
                    Syntax::Identifier {
                        ident: import_type, ..
                    },
                    import_set,
                    renames @ ..,
                    Syntax::Null { .. },
                ],
            ) if import_type == "rename" => {
                let import_set = ImportSet::parse(import_set)?;
                let renames = renames
                    .iter()
                    .map(|rename| match rename.as_list() {
                        Some(
                            [
                                Syntax::Identifier { ident: from, .. },
                                Syntax::Identifier { ident: to, .. },
                                Syntax::Null { .. },
                            ],
                        ) => Ok((from.clone(), to.clone())),
                        _ => Err(ParseAstError::BadForm(rename.span().clone())),
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?;
                Ok(Self::Rename {
                    set: Box::new(import_set),
                    renames,
                })
            }
            Some(_) => Ok(Self::Library(LibraryReference::parse(syn)?)),
            _ => Err(ParseAstError::ExpectedList(syn.span().clone())),
        }
    }

    pub fn parse_from_str(s: &str) -> Result<Self, ParseImportSetError> {
        let syn = Syntax::from_str(s, None)?;
        Ok(Self::parse(&syn[0])?)
    }
}

#[derive(Debug)]
pub enum ParseImportSetError {
    ParseSyntaxError(ParseSyntaxError),
    ParseAstError(ParseAstError),
}

impl From<ParseSyntaxError> for ParseImportSetError {
    fn from(pse: ParseSyntaxError) -> Self {
        Self::ParseSyntaxError(pse)
    }
}

impl From<ParseAstError> for ParseImportSetError {
    fn from(pae: ParseAstError) -> Self {
        Self::ParseAstError(pae)
    }
}

#[derive(Debug)]
pub struct LibraryReference {
    pub(crate) name: Vec<Symbol>,
    pub(crate) _version_ref: VersionReference,
}

impl LibraryReference {
    fn parse(syn: &Syntax) -> Result<Self, ParseAstError> {
        match syn.as_list() {
            Some(
                [
                    syms @ ..,
                    version_ref @ Syntax::List { .. },
                    Syntax::Null { .. },
                ],
            ) => {
                let name = syms
                    .iter()
                    .map(|atom| match atom {
                        Syntax::Identifier { ident, .. } => Ok(ident.sym),
                        _ => Err(ParseAstError::ExpectedIdentifier(atom.span().clone())),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let _version_ref = VersionReference::parse(version_ref)?;
                Ok(LibraryReference { name, _version_ref })
            }
            Some([syms @ .., Syntax::Null { .. }]) => {
                let name = syms
                    .iter()
                    .map(|atom| match atom {
                        Syntax::Identifier { ident, .. } => Ok(ident.sym),
                        _ => Err(ParseAstError::ExpectedIdentifier(atom.span().clone())),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(LibraryReference {
                    name,
                    _version_ref: VersionReference::SubVersions(Vec::new()),
                })
            }
            None => Err(ParseAstError::ExpectedList(syn.span().clone())),
            _ => Err(ParseAstError::BadForm(syn.span().clone())),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub enum Definition {
    DefineVar(DefineVar),
    DefineFunc(DefineFunc),
}

#[derive(Debug, Clone, Trace)]
pub struct DefineVar {
    pub var: Var,
    pub val: Arc<Expression>,
    pub next: Option<Either<Box<Definition>, ExprBody>>,
}

#[derive(Debug, Clone, Trace)]
pub struct DefineFunc {
    pub var: Var,
    pub args: Formals,
    pub body: Box<DefinitionBody>,
    pub next: Option<Either<Box<Definition>, ExprBody>>,
    pub span: Span,
}

impl Definition {
    fn set_next(self, next: Either<Box<Definition>, ExprBody>) -> Self {
        match self {
            Self::DefineVar(mut def_var) => {
                def_var.next = Some(next);
                Self::DefineVar(def_var)
            }
            Self::DefineFunc(mut def_func) => {
                def_func.next = Some(next);
                Self::DefineFunc(def_func)
            }
        }
    }

    #[maybe_async]
    pub fn parse(
        runtime: &Runtime,
        syn: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match syn {
            [
                _,
                Syntax::Identifier { ident, .. },
                expr,
                Syntax::Null { .. },
            ] => Ok(Definition::DefineVar(DefineVar {
                var: maybe_await!(env.fetch_var(ident))?.unwrap(),
                val: Arc::new(maybe_await!(Expression::parse(runtime, expr.clone(), env))?),
                next: None,
            })),
            [
                _,
                Syntax::List { list, span },
                body @ ..,
                Syntax::Null { .. },
            ] => {
                if body.is_empty() {
                    return Err(ParseAstError::ExpectedBody(span.clone()));
                }
                match list.as_slice() {
                    [
                        Syntax::Identifier {
                            ident: func_name,
                            span: func_span,
                            ..
                        },
                        args @ ..,
                    ] => {
                        let var = maybe_await!(env.fetch_var(func_name))?.unwrap();

                        let mut bound = HashMap::<Identifier, Span>::new();
                        let mut fixed = Vec::new();
                        let new_env = env.new_lexical_contour();
                        let mut arg_names = Vec::new();

                        // Bind the arguments to a new environment:
                        for arg in &args[..args.len() - 1] {
                            match arg {
                                Syntax::Identifier { ident, span, .. } => {
                                    if let Some(prev_span) = bound.get(ident) {
                                        return Err(ParseAstError::NameBoundMultipleTimes {
                                            ident: ident.clone(),
                                            first: prev_span.clone(),
                                            second: span.clone(),
                                        });
                                    }
                                    let Var::Local(sym) = new_env.def_var(ident.clone()) else {
                                        unreachable!()
                                    };
                                    bound.insert(ident.clone(), span.clone());
                                    fixed.push(sym);
                                    arg_names.push(ident.sym);
                                }
                                x => {
                                    return Err(ParseAstError::ExpectedIdentifier(
                                        x.span().clone(),
                                    ));
                                }
                            }
                        }

                        let args = if let Some(last) = args.last() {
                            match last {
                                Syntax::Null { .. } => {
                                    Formals::FixedArgs(fixed.into_iter().collect())
                                }
                                Syntax::Identifier { ident, span, .. } => {
                                    if let Some(prev_span) = bound.get(ident) {
                                        return Err(ParseAstError::NameBoundMultipleTimes {
                                            ident: ident.clone(),
                                            first: prev_span.clone(),
                                            second: span.clone(),
                                        });
                                    }
                                    let Var::Local(remaining) = new_env.def_var(ident.clone())
                                    else {
                                        unreachable!()
                                    };
                                    bound.insert(ident.clone(), span.clone());
                                    arg_names.push(ident.sym);
                                    Formals::VarArgs {
                                        fixed: fixed.into_iter().collect(),
                                        remaining,
                                    }
                                }
                                x => {
                                    return Err(ParseAstError::ExpectedIdentifier(
                                        x.span().clone(),
                                    ));
                                }
                            }
                        } else {
                            // If there is no last argument, there are no arguments
                            Formals::FixedArgs(Vec::new())
                        };

                        // Parse the body:
                        let body = maybe_await!(DefinitionBody::parse(
                            runtime, body, &new_env, func_span
                        ))?;

                        Ok(Self::DefineFunc(DefineFunc {
                            var,
                            args,
                            body: Box::new(body),
                            next: None,
                            span: func_span.clone(),
                        }))
                    }
                    _ => Err(ParseAstError::BadForm(span.clone())),
                }
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

#[maybe_async]
pub(super) fn define_syntax(
    runtime: &Runtime,
    ident: Identifier,
    expr: Syntax,
    env: &Environment,
) -> Result<(), ParseAstError> {
    let FullyExpanded {
        expanded,
        expansion_env,
    } = maybe_await!(expr.expand(env))?;

    let expr = maybe_await!(Expression::parse(runtime, expanded, &expansion_env))?;
    let cps_expr = expr.compile_top_level();
    let mac = maybe_await!(maybe_await!(runtime.compile_expr(cps_expr)).call(&[]))
        .map_err(|err| ParseAstError::RaisedValue(err.into()))?;
    let transformer: Procedure = mac[0].clone().try_into()?;
    env.def_keyword(ident, transformer);

    Ok(())
}

#[derive(Debug, Clone, Trace)]
pub enum Expression {
    Undefined,
    Literal(Literal),
    Quote(Quote),
    SyntaxQuote(SyntaxQuote),
    SyntaxCase(SyntaxCase),
    Apply(Apply),
    Let(Let),
    If(If),
    And(And),
    Or(Or),
    Lambda(Lambda),
    Set(Set),
    Var(Var),
    Vector(Vector),
    ByteVector(Vec<u8>),
    Begin(ExprBody),
}

impl Expression {
    #[maybe_async]
    pub fn parse(runtime: &Runtime, syn: Syntax, env: &Environment) -> Result<Self, ParseAstError> {
        let FullyExpanded {
            expansion_env,
            expanded,
        } = maybe_await!(syn.expand(env))?;
        maybe_await!(Self::parse_expanded(runtime, expanded, &expansion_env))
    }

    #[cfg(not(feature = "async"))]
    fn parse_expanded(
        runtime: &Runtime,
        syn: Syntax,
        env: &Environment,
    ) -> Result<Self, ParseAstError> {
        Self::parse_expanded_inner(runtime, syn, env)
    }

    #[cfg(feature = "async")]
    fn parse_expanded<'a>(
        runtime: &'a Runtime,
        syn: Syntax,
        env: &'a Environment,
    ) -> BoxFuture<'a, Result<Self, ParseAstError>> {
        Box::pin(Self::parse_expanded_inner(runtime, syn, env))
    }

    #[maybe_async]
    fn parse_expanded_inner(
        runtime: &Runtime,
        syn: Syntax,
        env: &Environment,
    ) -> Result<Self, ParseAstError> {
        match syn {
            Syntax::Null { span } => Err(ParseAstError::UnexpectedEmptyList(span)),

            // Regular identifiers:
            Syntax::Identifier { ident, span, .. } => {
                match maybe_await!(env.fetch_special_keyword_or_var(&ident))? {
                    Some(Either::Left(SpecialKeyword::Undefined)) => Ok(Self::Undefined),
                    Some(Either::Left(_)) => Err(ParseAstError::BadForm(span.clone())),
                    Some(Either::Right(var)) => Ok(Self::Var(var)),
                    None => {
                        let top = env.fetch_top();
                        if top.is_repl() {
                            Ok(Self::Var(Var::Global(
                                top.def_var(ident.clone(), Value::undefined()),
                            )))
                        } else {
                            Err(ParseAstError::UndefinedVariable {
                                span: span.clone(),
                                ident: ident.clone(),
                            })
                        }
                    }
                }
            }

            // Literals:
            Syntax::Literal { literal, .. } => Ok(Self::Literal(literal)),

            // Vector literals:
            Syntax::Vector { vector, .. } => Ok(Self::Vector(Vector::parse(&vector))),
            Syntax::ByteVector { vector, .. } => Ok(Self::ByteVector(vector)),

            // Functional forms:
            Syntax::List {
                list: exprs, span, ..
            } => match exprs.as_slice() {
                // Special forms:
                [
                    Syntax::Identifier { ident, span, .. },
                    tail @ ..,
                    Syntax::Null { .. },
                ] => match maybe_await!(env.fetch_special_keyword_or_var(ident))? {
                    Some(Either::Left(SpecialKeyword::Begin)) => {
                        maybe_await!(ExprBody::parse(runtime, tail, env)).map(Expression::Begin)
                    }
                    Some(Either::Left(SpecialKeyword::Lambda)) => {
                        maybe_await!(Lambda::parse(runtime, tail, env, span))
                            .map(Expression::Lambda)
                    }
                    Some(Either::Left(SpecialKeyword::Let)) => {
                        maybe_await!(Let::parse(runtime, tail, env, span)).map(Expression::Let)
                    }
                    Some(Either::Left(SpecialKeyword::If)) => {
                        maybe_await!(If::parse(runtime, tail, env, span)).map(Expression::If)
                    }
                    Some(Either::Left(SpecialKeyword::And)) => {
                        maybe_await!(And::parse(runtime, tail, env)).map(Expression::And)
                    }
                    Some(Either::Left(SpecialKeyword::Or)) => {
                        maybe_await!(Or::parse(runtime, tail, env)).map(Expression::Or)
                    }
                    Some(Either::Left(SpecialKeyword::Quote)) => {
                        Quote::parse(tail, span).map(Expression::Quote)
                    }
                    Some(Either::Left(SpecialKeyword::Syntax)) => {
                        SyntaxQuote::parse(tail, env, span).map(Expression::SyntaxQuote)
                    }
                    Some(Either::Left(SpecialKeyword::SyntaxCase)) => {
                        maybe_await!(SyntaxCase::parse(runtime, tail, env, span))
                            .map(Expression::SyntaxCase)
                    }
                    Some(Either::Left(SpecialKeyword::Set)) => {
                        maybe_await!(Set::parse(runtime, tail, env, span)).map(Expression::Set)
                    }
                    Some(Either::Left(SpecialKeyword::LetSyntax)) if tail.len() > 1 => {
                        let new_env =
                            maybe_await!(parse_let_syntax(runtime, false, &tail[0], env))?;
                        maybe_await!(ExprBody::parse(runtime, &tail[1..], &new_env))
                            .map(Expression::Begin)
                    }
                    Some(Either::Left(SpecialKeyword::LetRecSyntax)) if tail.len() > 1 => {
                        let new_env = maybe_await!(parse_let_syntax(runtime, true, &tail[0], env))?;
                        maybe_await!(ExprBody::parse(runtime, &tail[1..], &new_env))
                            .map(Expression::Begin)
                    }
                    Some(Either::Right(var)) => {
                        maybe_await!(Apply::parse(runtime, Expression::Var(var), tail, env, span))
                            .map(Expression::Apply)
                    }
                    Some(Either::Left(SpecialKeyword::DefineSyntax)) => unreachable!(),
                    Some(Either::Left(SpecialKeyword::Import)) => {
                        Err(ParseAstError::UnexpectedImport(span.clone()))
                    }
                    Some(Either::Left(SpecialKeyword::Define)) => {
                        Err(ParseAstError::UnexpectedDefinition(span.clone()))
                    }
                    None => Err(ParseAstError::UndefinedVariable {
                        span: span.clone(),
                        ident: ident.clone(),
                    }),
                    _ => Err(ParseAstError::BadForm(span.clone())),
                },
                [expr, args @ .., Syntax::Null { .. }] => maybe_await!(Apply::parse(
                    runtime,
                    maybe_await!(Expression::parse(runtime, expr.clone(), env))?,
                    args,
                    env,
                    expr.span(),
                ))
                .map(Expression::Apply),
                _ => Err(ParseAstError::BadForm(span.clone())),
            },
        }
    }

    // These function pointer comparisons are guaranteed to be meaningful since
    // they are returned from a store.
    pub fn to_primop(&self) -> Option<PrimOp> {
        use crate::{
            lists::{cons, list},
            num::{
                add_builtin, div_builtin, equal_builtin, greater_builtin, greater_equal_builtin,
                lesser_builtin, lesser_equal_builtin, mul_builtin, sub_builtin,
            },
            proc::{BridgePtr, FuncPtr::Bridge, Procedure},
        };
        use std::ptr::fn_addr_eq;

        const PRIMOP_TAB: &[(BridgePtr, PrimOp)] = &[
            (add_builtin, PrimOp::Add),
            (sub_builtin, PrimOp::Sub),
            (mul_builtin, PrimOp::Mul),
            (div_builtin, PrimOp::Div),
            (equal_builtin, PrimOp::Equal),
            (greater_builtin, PrimOp::Greater),
            (greater_equal_builtin, PrimOp::GreaterEqual),
            (lesser_builtin, PrimOp::Lesser),
            (lesser_equal_builtin, PrimOp::LesserEqual),
            (cons, PrimOp::Cons),
            (list, PrimOp::List),
        ];

        let Expression::Var(Var::Global(global)) = self else {
            return None;
        };
        let val = global.value_ref().read().clone();
        let val: Procedure = val.try_into().ok()?;
        let val_read = val.0.read();
        let Bridge(ptr) = val_read.func else {
            return None;
        };

        for (builtin, primop) in PRIMOP_TAB.iter().copied() {
            if fn_addr_eq(ptr, builtin) {
                return Some(primop);
            }
        }

        None
    }
}

#[derive(Debug, Clone, PartialEq, Trace)]
pub enum Literal {
    Number(Number),
    Boolean(bool),
    Character(char),
    String(String),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{num}"),
            Self::Boolean(true) => write!(f, "#t"),
            Self::Boolean(false) => write!(f, "#f"),
            Self::Character(chr) => write!(f, "{chr}"),
            Self::String(str) => write!(f, "{str:?}"),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Quote {
    pub val: Value,
}

impl Quote {
    fn parse(exprs: &[Syntax], span: &Span) -> Result<Self, ParseAstError> {
        match exprs {
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [expr] => Ok(Quote {
                val: Value::datum_from_syntax(expr),
            }),
            [_, arg, ..] => Err(ParseAstError::UnexpectedArgument(arg.span().clone())),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct SyntaxQuote {
    pub template: Template,
    pub expansions: HashMap<Identifier, Local>,
}

impl SyntaxQuote {
    fn parse(exprs: &[Syntax], env: &Environment, span: &Span) -> Result<Self, ParseAstError> {
        match exprs {
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [expr] => {
                let mut expansions = HashMap::new();
                let template = Template::compile(expr, env, &mut expansions);
                Ok(SyntaxQuote {
                    template,
                    expansions,
                })
            }
            [_, arg, ..] => Err(ParseAstError::UnexpectedArgument(arg.span().clone())),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Apply {
    pub operator: Box<Expression>,
    pub args: Vec<Expression>,
    pub span: Span,
}

impl Apply {
    #[maybe_async]
    fn parse(
        rt: &Runtime,
        operator: Expression,
        args: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        let mut parsed_args = Vec::new();
        for arg in args {
            parsed_args.push(maybe_await!(Expression::parse(rt, arg.clone(), env))?);
        }
        Ok(Apply {
            operator: Box::new(operator),
            args: parsed_args,
            span: span.clone(),
        })
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Lambda {
    pub args: Formals,
    pub body: DefinitionBody,
    pub span: Span,
}

impl Lambda {
    #[maybe_async]
    fn parse(
        runtime: &Runtime,
        sexprs: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match sexprs {
            [Syntax::Null { .. }, body @ ..] => {
                maybe_await!(parse_lambda(runtime, &[], body, env, span))
            }
            [Syntax::List { list: args, .. }, body @ ..] => {
                maybe_await!(parse_lambda(runtime, args, body, env, span))
            }
            [ident @ Syntax::Identifier { .. }, body @ ..] => {
                maybe_await!(parse_lambda(
                    runtime,
                    std::slice::from_ref(ident),
                    body,
                    env,
                    span
                ))
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

#[maybe_async]
fn parse_lambda(
    runtime: &Runtime,
    args: &[Syntax],
    body: &[Syntax],
    env: &Environment,
    span: &Span,
) -> Result<Lambda, ParseAstError> {
    let mut bound = HashMap::<Identifier, Span>::new();
    let mut fixed = Vec::new();
    let new_contour = env.new_lexical_contour();
    let mut arg_names = Vec::new();

    if !args.is_empty() {
        for arg in &args[..args.len() - 1] {
            match arg {
                Syntax::Identifier { ident, span, .. } => {
                    if let Some(prev_span) = bound.get(ident) {
                        return Err(ParseAstError::NameBoundMultipleTimes {
                            ident: ident.clone(),
                            first: prev_span.clone(),
                            second: span.clone(),
                        });
                    }
                    let Var::Local(arg) = new_contour.def_var(ident.clone()) else {
                        unreachable!()
                    };
                    fixed.push(arg);
                    arg_names.push(ident.sym);
                    bound.insert(ident.clone(), span.clone());
                }
                x => return Err(ParseAstError::ExpectedIdentifier(x.span().clone())),
            }
        }
    }

    let args = if let Some(last) = args.last() {
        match last {
            Syntax::Null { .. } => Formals::FixedArgs(fixed.into_iter().collect()),
            Syntax::Identifier { ident, span, .. } => {
                if let Some(prev_span) = bound.get(ident) {
                    return Err(ParseAstError::NameBoundMultipleTimes {
                        ident: ident.clone(),
                        first: prev_span.clone(),
                        second: span.clone(),
                    });
                }
                arg_names.push(ident.sym);
                let Var::Local(remaining) = new_contour.def_var(ident.clone()) else {
                    unreachable!()
                };
                Formals::VarArgs {
                    fixed: fixed.into_iter().collect(),
                    remaining,
                }
            }
            x => return Err(ParseAstError::ExpectedIdentifier(x.span().clone())),
        }
    } else {
        // If there is no last argument, there are no arguments
        Formals::FixedArgs(Vec::new())
    };

    let body = maybe_await!(DefinitionBody::parse(runtime, body, &new_contour, span))?;

    Ok(Lambda {
        args,
        body,
        span: span.clone(),
    })
}

#[derive(Debug, Clone, Trace)]
pub struct Let {
    pub bindings: Vec<(Local, Expression)>,
    pub body: DefinitionBody,
}

impl Let {
    pub fn new(bindings: Vec<(Local, Expression)>, body: DefinitionBody) -> Self {
        Self { bindings, body }
    }

    #[maybe_async]
    fn parse(
        runtime: &Runtime,
        syn: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match syn {
            [Syntax::Null { .. }, body @ ..] => {
                maybe_await!(parse_let(runtime, &[], body, env, span))
            }
            [Syntax::List { list: bindings, .. }, body @ ..] => {
                maybe_await!(parse_let(runtime, bindings, body, env, span))
            }
            // Named let:
            [
                Syntax::Identifier { ident, .. },
                Syntax::List { list: bindings, .. },
                body @ ..,
            ] => maybe_await!(parse_named_let(runtime, ident, bindings, body, env, span)),
            [
                Syntax::Identifier { ident, .. },
                Syntax::Null { .. },
                body @ ..,
            ] => maybe_await!(parse_named_let(runtime, ident, &[], body, env, span)),
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

#[maybe_async]
fn parse_let(
    runtime: &Runtime,
    bindings: &[Syntax],
    body: &[Syntax],
    env: &Environment,
    span: &Span,
) -> Result<Let, ParseAstError> {
    let mut previously_bound = HashMap::new();
    let mut parsed_bindings = Vec::new();
    let mut binding_names = Vec::new();

    let new_contour = env.new_lexical_contour();

    match bindings {
        [] | [Syntax::Null { .. }] => (),
        [bindings @ .., Syntax::Null { .. }] => {
            for binding in bindings {
                let binding =
                    maybe_await!(LetBinding::parse(runtime, binding, env, &previously_bound))?;
                previously_bound.insert(binding.ident.clone(), binding.span.clone());
                let Var::Local(var) = new_contour.def_var(binding.ident.clone()) else {
                    unreachable!()
                };
                binding_names.push(binding.ident.sym);
                parsed_bindings.push((var, binding));
            }
        }
        _ => {
            return Err(ParseAstError::BadForm(span.clone()));
        }
    }

    let ast_body = maybe_await!(DefinitionBody::parse(runtime, body, &new_contour, span))?;

    // TODO: Lot of unnecessary cloning here, fix that.
    let bindings: Vec<_> = parsed_bindings
        .iter()
        .map(|(var, binding)| (*var, binding.expr.clone()))
        .collect();

    Ok(Let {
        bindings,
        body: ast_body,
    })
}

#[maybe_async]
fn parse_named_let(
    runtime: &Runtime,
    name: &Identifier,
    bindings: &[Syntax],
    body: &[Syntax],
    env: &Environment,
    span: &Span,
) -> Result<Let, ParseAstError> {
    let mut previously_bound = HashMap::new();
    let mut formals = Vec::new();
    let mut args = Vec::new();

    let func_contour = env.new_lexical_contour();

    let func = func_contour.def_var(name.clone());

    let body_contour = func_contour.new_lexical_contour();

    match bindings {
        [] | [Syntax::Null { .. }] => (),
        [bindings @ .., Syntax::Null { .. }] => {
            for binding in bindings {
                let binding =
                    maybe_await!(LetBinding::parse(runtime, binding, env, &previously_bound))?;
                previously_bound.insert(binding.ident.clone(), binding.span.clone());
                let Var::Local(var) = body_contour.def_var(binding.ident.clone()) else {
                    unreachable!()
                };
                formals.push(var);
                args.push(binding.expr);
            }
        }
        _ => {
            return Err(ParseAstError::BadForm(span.clone()));
        }
    }

    let body = maybe_await!(DefinitionBody::parse(runtime, body, &body_contour, span))?;

    let func = DefineFunc {
        var: func.clone(),
        args: Formals::FixedArgs(formals),
        body: Box::new(body),
        next: Some(Either::Right(ExprBody::new(vec![Expression::Apply(
            Apply {
                operator: Box::new(Expression::Var(func)),
                args,
                span: span.clone(),
            },
        )]))),
        span: span.clone(),
    };

    Ok(Let {
        bindings: Vec::new(),
        body: DefinitionBody::new(Either::Left(Definition::DefineFunc(func))),
    })
}

struct LetBinding {
    ident: Identifier,
    span: Span,
    expr: Expression,
}

impl LetBinding {
    #[maybe_async]
    fn parse(
        runtime: &Runtime,
        form: &Syntax,
        env: &Environment,
        previously_bound: &HashMap<Identifier, Span>,
    ) -> Result<LetBinding, ParseAstError> {
        if let Some(
            [
                Syntax::Identifier {
                    ident,
                    span: bind_span,
                    ..
                },
                expr,
                Syntax::Null { .. },
            ],
        ) = form.as_list()
        {
            if let Some(prev_bind) = previously_bound.get(ident) {
                return Err(ParseAstError::NameBoundMultipleTimes {
                    ident: ident.clone(),
                    first: prev_bind.clone(),
                    second: bind_span.clone(),
                });
            }

            let expr = maybe_await!(Expression::parse(runtime, expr.clone(), env))?;

            Ok(LetBinding {
                ident: ident.clone(),
                span: bind_span.clone(),
                expr,
            })
        } else {
            Err(ParseAstError::BadForm(form.span().clone()))
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Set {
    pub var: Var,
    pub val: Arc<Expression>,
}

impl Set {
    #[maybe_async]
    fn parse(
        runtime: &Runtime,
        exprs: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match exprs {
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [Syntax::Identifier { ident, span, .. }, expr] => Ok(Set {
                var: match maybe_await!(env.fetch_var(ident))? {
                    Some(Var::Global(global)) if !global.mutable => {
                        return Err(ParseAstError::CannotSetImmutableVar {
                            span: span.clone(),
                            name: ident.clone(),
                        });
                    }
                    Some(var) => var,
                    None => {
                        return Err(ParseAstError::UndefinedVariable {
                            span: span.clone(),
                            ident: ident.clone(),
                        });
                    }
                },
                val: Arc::new(maybe_await!(Expression::parse(runtime, expr.clone(), env))?),
            }),
            [arg1, _] => Err(ParseAstError::ExpectedIdentifier(arg1.span().clone())),
            [_, _, arg3, ..] => Err(ParseAstError::UnexpectedArgument(arg3.span().clone())),
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct If {
    pub cond: Arc<Expression>,
    pub success: Arc<Expression>,
    pub failure: Option<Arc<Expression>>,
}

impl If {
    #[maybe_async]
    fn parse(
        runtime: &Runtime,
        exprs: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match exprs {
            [cond, success] => Ok(If {
                cond: Arc::new(maybe_await!(Expression::parse(runtime, cond.clone(), env))?),
                success: Arc::new(maybe_await!(Expression::parse(
                    runtime,
                    success.clone(),
                    env
                ))?),
                failure: None,
            }),
            [cond, success, failure] => Ok(If {
                cond: Arc::new(maybe_await!(Expression::parse(runtime, cond.clone(), env))?),
                success: Arc::new(maybe_await!(Expression::parse(
                    runtime,
                    success.clone(),
                    env
                ))?),
                failure: Some(Arc::new(maybe_await!(Expression::parse(
                    runtime,
                    failure.clone(),
                    env
                ))?)),
            }),
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [a1] => Err(ParseAstError::ExpectedArgument(a1.span().clone())),
            [_, _, _, unexpected, ..] => {
                Err(ParseAstError::UnexpectedArgument(unexpected.span().clone()))
            }
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub enum Formals {
    FixedArgs(Vec<Local>),
    VarArgs { fixed: Vec<Local>, remaining: Local },
}

impl Formals {
    pub fn iter(&self) -> impl Iterator<Item = &'_ Local> {
        let fixed_iter = match self {
            Self::FixedArgs(fixed) => fixed.iter(),
            Self::VarArgs { fixed, .. } => fixed.iter(),
        };
        let remaining = match self {
            Self::FixedArgs(_) => None,
            Self::VarArgs { remaining, .. } => Some(remaining),
        };
        fixed_iter.chain(remaining)
    }

    pub fn is_variadic(&self) -> bool {
        matches!(self, Self::VarArgs { .. })
    }
}

#[derive(Debug, Clone, Trace)]
pub struct DefinitionBody {
    pub first: Either<Definition, ExprBody>,
}

impl DefinitionBody {
    pub fn new(first: Either<Definition, ExprBody>) -> Self {
        Self { first }
    }

    #[maybe_async]
    pub fn parse_lib_body(
        runtime: &Runtime,
        body: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        maybe_await!(Self::parse_helper(runtime, body, true, env, span))
    }

    #[maybe_async]
    pub fn parse(
        runtime: &Runtime,
        body: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        maybe_await!(Self::parse_helper(runtime, body, false, env, span))
    }

    /// Parse the body. body is expected to be a list of valid syntax objects, and should not include
    /// _any_ nulls, including one at the end.
    #[cfg(not(feature = "async"))]
    fn parse_helper(
        runtime: &Runtime,
        body: &[Syntax],
        permissive: bool,
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        Self::parse_helper_inner(runtime, body, permissive, env, span)
    }

    /// Parse the body. body is expected to be a list of valid syntax objects, and should not include
    /// _any_ nulls, including one at the end.
    #[cfg(feature = "async")]
    fn parse_helper<'a>(
        runtime: &'a Runtime,
        body: &'a [Syntax],
        permissive: bool,
        env: &'a Environment,
        span: &'a Span,
    ) -> BoxFuture<'a, Result<Self, ParseAstError>> {
        Box::pin(Self::parse_helper_inner(
            runtime, body, permissive, env, span,
        ))
    }

    #[maybe_async]
    fn parse_helper_inner(
        runtime: &Runtime,
        body: &[Syntax],
        permissive: bool,
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        let mut defs = Vec::new();
        let mut exprs = Vec::new();

        maybe_await!(splice_in(
            runtime, permissive, body, env, span, &mut defs, &mut exprs
        ))?;

        let mut defs_parsed = Vec::new();
        let mut exprs_parsed = Vec::new();

        // Mark all of the defs as defined:
        for def in defs.iter() {
            if let Some([_, def, ..]) = def.expanded.as_list() {
                let ident = match def.as_list() {
                    Some([Syntax::Identifier { ident, .. }, ..]) => ident,
                    _ => def
                        .as_ident()
                        .ok_or(ParseAstError::BadForm(def.span().clone()))?,
                };
                env.def_var(ident.clone());
            }
        }

        for def in defs.into_iter() {
            let def = maybe_await!(Definition::parse(
                runtime,
                def.expanded.as_list().unwrap(),
                &def.expansion_env,
                def.expanded.span(),
            ))?;
            defs_parsed.push(def);
        }

        for expr in exprs.into_iter() {
            exprs_parsed.push(maybe_await!(Expression::parse_expanded(
                runtime,
                expr.expanded,
                &expr.expansion_env
            ))?);
        }

        let expr_body = ExprBody::new(exprs_parsed);
        match defs_parsed.pop() {
            Some(last_def) => {
                let mut last_def = last_def.set_next(Either::Right(expr_body));
                for next_def in defs_parsed.into_iter().rev() {
                    last_def = next_def.set_next(Either::Left(Box::new(last_def)));
                }
                Ok(Self::new(Either::Left(last_def)))
            }
            _ => Ok(Self::new(Either::Right(expr_body))),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct ExprBody {
    pub exprs: Vec<Expression>,
}

impl ExprBody {
    pub fn new(exprs: Vec<Expression>) -> Self {
        Self { exprs }
    }

    /// Differs from Body by being purely expression based. No definitions allowed.
    #[maybe_async]
    fn parse(runtime: &Runtime, body: &[Syntax], env: &Environment) -> Result<Self, ParseAstError> {
        let mut exprs = Vec::new();
        for sexpr in body {
            let parsed = maybe_await!(Expression::parse(runtime, sexpr.clone(), env))?;
            exprs.push(parsed);
        }
        Ok(Self { exprs })
    }
}

#[cfg(not(feature = "async"))]
fn splice_in(
    runtime: &Runtime,
    permissive: bool,
    body: &[Syntax],
    env: &Environment,
    span: &Span,
    defs: &mut Vec<FullyExpanded>,
    exprs: &mut Vec<FullyExpanded>,
) -> Result<(), ParseAstError> {
    splice_in_inner(runtime, permissive, body, env, span, defs, exprs)
}

#[cfg(feature = "async")]
fn splice_in<'a>(
    runtime: &'a Runtime,
    permissive: bool,
    body: &'a [Syntax],
    env: &'a Environment,
    span: &'a Span,
    defs: &'a mut Vec<FullyExpanded>,
    exprs: &'a mut Vec<FullyExpanded>,
) -> BoxFuture<'a, Result<(), ParseAstError>> {
    Box::pin(splice_in_inner(
        runtime, permissive, body, env, span, defs, exprs,
    ))
}

#[maybe_async]
fn splice_in_inner(
    runtime: &Runtime,
    permissive: bool,
    body: &[Syntax],
    env: &Environment,
    span: &Span,
    defs: &mut Vec<FullyExpanded>,
    exprs: &mut Vec<FullyExpanded>,
) -> Result<(), ParseAstError> {
    if body.is_empty() {
        return Err(ParseAstError::ExpectedBody(span.clone()));
    }
    for unexpanded in body {
        let FullyExpanded {
            expansion_env,
            expanded,
        } = maybe_await!(unexpanded.clone().expand(env))?;
        let is_def = {
            if let Some(
                [
                    Syntax::Identifier { ident, span, .. },
                    tail @ ..,
                    Syntax::Null { .. },
                ],
            ) = expanded.as_list()
            {
                let keyword = maybe_await!(expansion_env.fetch_special_keyword_or_var(ident))?;
                match (keyword, tail) {
                    (Some(Either::Left(SpecialKeyword::Begin)), []) => {
                        continue;
                    }
                    (Some(Either::Left(SpecialKeyword::Begin)), body) => {
                        maybe_await!(splice_in(
                            runtime,
                            permissive,
                            body,
                            &expansion_env,
                            span,
                            defs,
                            exprs
                        ))?;
                        continue;
                    }
                    (
                        Some(Either::Left(SpecialKeyword::DefineSyntax)),
                        [Syntax::Identifier { ident: name, .. }, expr],
                    ) => {
                        maybe_await!(define_syntax(
                            runtime,
                            name.clone(),
                            expr.clone(),
                            &expansion_env
                        ))?;
                        continue;
                    }
                    (Some(Either::Left(SpecialKeyword::DefineSyntax)), _) => {
                        return Err(ParseAstError::BadForm(span.clone()));
                    }
                    (Some(Either::Left(SpecialKeyword::LetSyntax)), [bindings, form @ ..]) => {
                        let new_env = maybe_await!(parse_let_syntax(
                            runtime,
                            false,
                            bindings,
                            &expansion_env
                        ))?;
                        maybe_await!(splice_in(
                            runtime, permissive, form, &new_env, span, defs, exprs
                        ))?;
                        continue;
                    }
                    (Some(Either::Left(SpecialKeyword::LetRecSyntax)), [bindings, form @ ..]) => {
                        let new_env = maybe_await!(parse_let_syntax(
                            runtime,
                            true,
                            bindings,
                            &expansion_env
                        ))?;
                        maybe_await!(splice_in(
                            runtime, permissive, form, &new_env, span, defs, exprs
                        ))?;
                        continue;
                    }
                    (Some(Either::Left(SpecialKeyword::Import)), imports) => {
                        if !permissive && !exprs.is_empty() {
                            return Err(ParseAstError::UnexpectedImport(span.clone()));
                        }
                        for import in imports {
                            let import_set = ImportSet::parse(discard_for(import))?;
                            maybe_await!(expansion_env.import(import_set)).map_err(|err| {
                                ParseAstError::ImportError {
                                    span: span.clone(),
                                    error: Box::new(err),
                                }
                            })?;
                        }
                        continue;
                    }
                    (Some(Either::Left(SpecialKeyword::Define)), _) => {
                        if !permissive && !exprs.is_empty() {
                            return Err(ParseAstError::UnexpectedDefinition(span.clone()));
                        }
                        true
                    }
                    _ => false,
                }
            } else {
                false
            }
        };

        let expanded = FullyExpanded::new(expansion_env, expanded);
        if is_def {
            defs.push(expanded);
        } else {
            exprs.push(expanded);
        }
    }

    Ok(())
}

#[maybe_async]
fn parse_let_syntax(
    runtime: &Runtime,
    recursive: bool,
    bindings: &Syntax,
    env: &Environment,
) -> Result<Environment, ParseAstError> {
    let Some([keyword_bindings @ .., Syntax::Null { .. }]) = bindings.as_list() else {
        return Err(ParseAstError::BadForm(bindings.span().clone()));
    };

    let new_env = env.new_let_syntax_contour(recursive);

    for binding in keyword_bindings {
        if let Some(
            [
                Syntax::Identifier { ident: keyword, .. },
                expr,
                Syntax::Null { .. },
            ],
        ) = binding.as_list()
        {
            maybe_await!(define_syntax(
                runtime,
                keyword.clone(),
                expr.clone(),
                &new_env
            ))?;
        } else {
            return Err(ParseAstError::BadForm(binding.span().clone()));
        }
    }

    Ok(new_env)
}

#[derive(Debug, Clone, Trace)]
pub struct And {
    pub args: Vec<Expression>,
}

impl And {
    pub fn new(args: Vec<Expression>) -> Self {
        Self { args }
    }
}

impl And {
    #[maybe_async]
    fn parse(
        runtime: &Runtime,
        exprs: &[Syntax],
        env: &Environment,
    ) -> Result<Self, ParseAstError> {
        let mut output = Vec::new();
        for expr in exprs {
            let expr = maybe_await!(Expression::parse(runtime, expr.clone(), env))?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Or {
    pub args: Vec<Expression>,
}

impl Or {
    pub fn new(args: Vec<Expression>) -> Self {
        Self { args }
    }

    #[maybe_async]
    fn parse(
        runtime: &Runtime,
        exprs: &[Syntax],
        env: &Environment,
    ) -> Result<Self, ParseAstError> {
        let mut output = Vec::new();
        for expr in exprs {
            let expr = maybe_await!(Expression::parse(runtime, expr.clone(), env))?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Vector {
    pub vals: Vec<Value>,
}

impl Vector {
    #[allow(dead_code)]
    fn parse(exprs: &[Syntax]) -> Self {
        let mut vals = Vec::new();
        for expr in exprs {
            vals.push(Value::datum_from_syntax(expr));
        }
        Self { vals }
    }
}

#[derive(Clone, Trace, Debug)]
pub struct SyntaxCase {
    pub arg: Arc<Expression>,
    pub rules: Vec<SyntaxRule>,
}

impl SyntaxCase {
    #[maybe_async]
    fn parse(
        runtime: &Runtime,
        exprs: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        let (arg, keywords, mut rules) = match exprs {
            [arg, Syntax::List { list, .. }, rules @ ..] => {
                let mut keywords = HashSet::default();
                // TODO: ensure keywords_list is proper
                for keyword in &list[..list.len() - 1] {
                    if let Syntax::Identifier { ident, .. } = keyword {
                        keywords.insert(ident.sym);
                    } else {
                        return Err(ParseAstError::BadForm(keyword.span().clone()));
                    }
                }
                (arg, keywords, rules)
            }
            [arg, Syntax::Null { .. }, rules @ ..] => (arg, HashSet::default(), rules),
            _ => return Err(ParseAstError::BadForm(span.clone())),
        };
        let mut syntax_rules = Vec::new();
        loop {
            match rules {
                [] => break,
                [Syntax::List { list, .. }, tail @ ..] => match &list[..] {
                    [pattern, output_expression, Syntax::Null { .. }] => {
                        syntax_rules.push(maybe_await!(SyntaxRule::compile(
                            runtime,
                            &keywords,
                            pattern,
                            None,
                            output_expression,
                            env,
                        ))?);
                        rules = tail;
                    }
                    [pattern, fender, output_expression, Syntax::Null { .. }] => {
                        syntax_rules.push(maybe_await!(SyntaxRule::compile(
                            runtime,
                            &keywords,
                            pattern,
                            Some(fender),
                            output_expression,
                            env,
                        ))?);
                        rules = tail;
                    }
                    _ => return Err(ParseAstError::BadForm(span.clone())),
                },
                _ => return Err(ParseAstError::BadForm(span.clone())),
            }
        }
        Ok(SyntaxCase {
            arg: Arc::new(maybe_await!(Expression::parse(runtime, arg.clone(), env))?),
            rules: syntax_rules,
        })
    }
}
