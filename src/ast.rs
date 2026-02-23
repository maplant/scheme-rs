//! Data structures for expanding and representing Scheme code.

// TODO: All of these functions need to be improved to reduce the amount of
// cloning that occurs. I was going to add that as part of the set-of-scopes
// change, but it proved too unwieldy

use crate::{
    Either,
    cps::{Compile, PrimOp},
    env::{Binding, Environment, Local, Scope, Var},
    exceptions::Exception,
    expand::{SyntaxRule, Template},
    gc::Trace,
    proc::Procedure,
    runtime::Runtime,
    symbols::Symbol,
    syntax::{Identifier, Span, Syntax},
    value::{Expect1, Value},
};

use scheme_rs_macros::{maybe_async, maybe_await};
use std::{
    collections::{HashMap, HashSet},
    fmt,
    str::FromStr,
    sync::Arc,
};

#[cfg(feature = "async")]
use futures::future::BoxFuture;

/// Collection of errors that can occur while parsing, grouped in functions to
/// ensure consistency in their error messages.
mod error {
    use crate::exceptions::{Message, SyntaxViolation};

    use super::*;

    pub(super) fn syntax_error(
        form: &Syntax,
        subform: Option<&Syntax>,
        error: impl fmt::Display,
    ) -> Exception {
        Exception::from((
            SyntaxViolation::new(form.clone(), subform.cloned()),
            Message::new(error),
        ))
    }

    pub(super) fn bad_form(form: &Syntax, subform: Option<&Syntax>) -> Exception {
        syntax_error(form, subform, "bad form")
    }

    pub(super) fn undefined_variable(form: &Syntax, subform: Option<&Syntax>) -> Exception {
        syntax_error(form, subform, "undefined variable")
    }

    pub(super) fn immutable_variable(form: &Syntax, subform: &Syntax) -> Exception {
        syntax_error(form, Some(subform), "cannot set immutable variable")
    }

    pub(super) fn name_previously_bound(form: &Syntax, subform: &Syntax) -> Exception {
        syntax_error(form, Some(subform), "name previously bound")
    }

    pub(super) fn expected_list(form: &Syntax) -> Exception {
        syntax_error(form, None, "expected list")
    }

    pub(super) fn expected_more_arguments(form: &Syntax) -> Exception {
        syntax_error(form, None, "expected more arguments")
    }

    pub(super) fn expected_identifier(form: &Syntax, subform: Option<&Syntax>) -> Exception {
        syntax_error(form, subform, "expected identifier")
    }

    pub(super) fn expected_body(form: &Syntax) -> Exception {
        syntax_error(form, None, "expected body")
    }

    pub(super) fn expected_import_spec(form: &Syntax) -> Exception {
        syntax_error(form, None, "expected import spec")
    }

    pub(super) fn expected_export_spec(form: &Syntax) -> Exception {
        syntax_error(form, None, "expected export spec")
    }

    pub(super) fn expected_number(form: &Syntax, subform: &Syntax) -> Exception {
        syntax_error(form, Some(subform), "expected number")
    }

    pub(super) fn expected_keyword(form: &Syntax, subform: &Syntax, keyword: &str) -> Exception {
        syntax_error(form, Some(subform), format!("expected `{keyword}` keyword"))
    }

    pub(super) fn unexpected_argument(form: &Syntax, subform: &Syntax) -> Exception {
        syntax_error(form, Some(subform), "unexpected argument")
    }

    pub(super) fn unexpected_define(form: &Syntax) -> Exception {
        syntax_error(form, None, "unexpected define")
    }

    pub(super) fn unexpected_define_syntax(form: &Syntax) -> Exception {
        syntax_error(form, None, "unexpected define-syntax")
    }

    pub(super) fn unexpected_import(form: &Syntax) -> Exception {
        syntax_error(form, None, "illegal import")
    }

    pub(super) fn import_not_permitted(form: &Syntax, lib_name: &[Symbol]) -> Exception {
        let lib_name = lib_name
            .iter()
            .map(|s| s.to_str().to_string())
            .collect::<Vec<_>>()
            .join(" ");
        syntax_error(
            form,
            None,
            &format!("import of ({lib_name}) is not permitted"),
        )
    }

    pub(super) fn unexpected_empty_list(form: &Syntax, subform: Option<&Syntax>) -> Exception {
        syntax_error(form, subform, "unexpected empty list")
    }
}

/// Primitives that the compiler needs to be aware of in order to create a
/// proper AST as specified by R6RS base.
#[derive(Copy, Clone, Trace, Debug, PartialEq, Eq)]
pub enum Primitive {
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

#[derive(Debug)]
pub struct LibrarySpec {
    pub(crate) name: LibraryName,
    pub(crate) exports: ExportSpec,
    pub(crate) imports: ImportSpec,
    pub(crate) body: Syntax,
}

impl LibrarySpec {
    pub fn parse(form: &Syntax) -> Result<Self, Exception> {
        match form.as_list() {
            Some(
                [
                    library_keyword @ Syntax::Identifier {
                        ident: library_decl,
                        ..
                    },
                    library_name,
                    body @ ..,
                    end,
                ],
            ) if end.is_null() => {
                if library_decl != "library" {
                    return Err(error::expected_keyword(form, library_keyword, "library"));
                }
                let mut exports = ExportSpec::default();
                let mut imports = ImportSpec::default();
                let mut body = body;
                while let Some(spec) = body.first() {
                    if spec.has_car("export") {
                        exports.join(ExportSpec::parse(spec)?);
                    } else if spec.has_car("import") {
                        imports.join(ImportSpec::parse(spec)?);
                    } else {
                        break;
                    }
                    body = &body[1..];
                }
                let mut body = body.to_vec();
                let end = end.clone();
                let body = if body.is_empty() {
                    end
                } else {
                    body.push(end);
                    Syntax::List {
                        span: body[0].span().clone(),
                        list: body,
                    }
                };

                Ok(Self {
                    name: LibraryName::parse(library_name)?,
                    exports,
                    imports,
                    body,
                })
            }
            _ => Err(error::bad_form(form, None)),
        }
    }
}

#[derive(Clone, Default, PartialEq, Eq, Hash, Trace, Debug)]
pub struct LibraryName {
    pub name: Vec<Symbol>,
    pub version: Version,
}

impl LibraryName {
    pub fn parse(form: &Syntax) -> Result<Self, Exception> {
        match form.as_list() {
            Some([name @ .., Syntax::List { list: version, .. }, end]) if end.is_null() => {
                Ok(Self {
                    name: list_to_name(name, form)?,
                    version: Version::parse(version, form)?,
                })
            }
            Some([name @ .., end]) if end.is_null() => Ok(Self {
                name: list_to_name(name, form)?,
                version: Version::default(),
            }),
            _ => Err(error::bad_form(form, None)),
        }
    }

    pub fn from_str(s: &str, file_name: Option<&str>) -> Result<Self, Exception> {
        let form = Syntax::from_str(s, file_name)?;
        match form.as_list() {
            Some([item, end]) if end.is_null() => Self::parse(item),
            _ => Err(Exception::error(format!("bad form in '{s}'"))),
        }
    }

    pub fn name(&self) -> String {
        let lib_name = self
            .name
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        format!("({})", lib_name.join(" "))
    }
}

fn list_to_name(name: &[Syntax], form: &Syntax) -> Result<Vec<Symbol>, Exception> {
    name.iter()
        .map(|name| {
            if let Syntax::Identifier { ident, .. } = name {
                Ok(ident.sym)
            } else {
                Err(error::expected_identifier(form, Some(name)))
            }
        })
        .collect()
}

/*
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
*/

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Default, Trace, Debug)]
pub struct Version {
    version: Vec<usize>,
}

impl Version {
    fn parse(version: &[Syntax], form: &Syntax) -> Result<Self, Exception> {
        match version {
            [version @ .., end] if end.is_null() => {
                let version: Result<Vec<usize>, _> = version
                    .iter()
                    .map(|subvers| {
                        if let Syntax::Wrapped { value, .. } = subvers {
                            Ok(value.try_into()?)
                        } else {
                            Err(error::expected_number(form, subvers))
                        }
                    })
                    .collect();
                Ok(Self { version: version? })
            }
            _ => Err(error::bad_form(form, None)),
        }
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, sv) in self.version.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{sv}")?;
        }
        write!(f, ")")
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
    pub fn matches(&self, version: &Version) -> bool {
        match self {
            Self::SubVersions(subversions) if version.version.len() >= subversions.len() => {
                subversions
                    .iter()
                    .zip(version.version.iter())
                    .all(|(svr, ver)| svr.matches(*ver))
            }
            Self::SubVersions(_) => false,
            Self::And(vrs) => vrs.iter().all(|vr| vr.matches(version)),
            Self::Or(vrs) => vrs.iter().any(|vr| vr.matches(version)),
            Self::Not(vr) => !vr.matches(version),
        }
    }

    fn parse(form: &Syntax) -> Result<Self, Exception> {
        match form.as_list() {
            Some([Syntax::Identifier { ident: kw, .. }, version_refs @ .., end])
                if kw == "and" && end.is_null() =>
            {
                let version_refs = version_refs
                    .iter()
                    .map(VersionReference::parse)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self::And(version_refs))
            }
            Some([Syntax::Identifier { ident: kw, .. }, version_refs @ .., end])
                if kw == "or" && end.is_null() =>
            {
                let version_refs = version_refs
                    .iter()
                    .map(VersionReference::parse)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self::Or(version_refs))
            }
            Some([Syntax::Identifier { ident: kw, .. }, version_ref, end])
                if kw == "not" && end.is_null() =>
            {
                let version_ref = VersionReference::parse(version_ref)?;
                Ok(Self::Not(Box::new(version_ref)))
            }
            Some([subversion_refs @ .., end]) if end.is_null() => {
                let subversion_refs = subversion_refs
                    .iter()
                    .map(SubVersionReference::parse)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self::SubVersions(subversion_refs))
            }
            None => Err(error::expected_list(form)),
            _ => Err(error::bad_form(form, None)),
        }
    }
}

impl fmt::Display for VersionReference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SubVersions(svs) => {
                write!(f, "(")?;
                for (i, sv) in svs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{sv}")?;
                }
                write!(f, ")")
            }
            Self::And(svs) => {
                write!(f, "(and ")?;
                for (i, sv) in svs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{sv}")?;
                }
                write!(f, ")")
            }
            Self::Or(svs) => {
                write!(f, "(and ")?;
                for (i, sv) in svs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{sv}")?;
                }
                write!(f, ")")
            }
            Self::Not(sv) => {
                write!(f, "(not {sv})")
            }
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
    fn matches(&self, lhs: usize) -> bool {
        match self {
            Self::SubVersion(rhs) => lhs == *rhs,
            Self::Gte(rhs) => lhs >= *rhs,
            Self::Lte(rhs) => lhs <= *rhs,
            Self::And(refs) => refs.iter().all(|r| r.matches(lhs)),
            Self::Or(refs) => refs.iter().any(|r| r.matches(lhs)),
            Self::Not(svr) => !svr.matches(lhs),
        }
    }

    fn parse(form: &Syntax) -> Result<Self, Exception> {
        match form {
            Syntax::Wrapped { value, .. } => Ok(Self::SubVersion(value.try_into()?)),
            _ => match form.as_list() {
                Some(
                    [
                        Syntax::Identifier { ident: kw, .. },
                        Syntax::Wrapped { value, .. },
                        end,
                    ],
                ) if kw == ">=" && end.is_null() => Ok(Self::Gte(value.try_into()?)),
                Some(
                    [
                        Syntax::Identifier { ident: kw, .. },
                        Syntax::Wrapped { value, .. },
                        end,
                    ],
                ) if kw == "<=" && end.is_null() => Ok(Self::Lte(value.try_into()?)),
                Some(
                    [
                        Syntax::Identifier { ident: kw, .. },
                        subversion_refs @ ..,
                        end,
                    ],
                ) if kw == "and" && end.is_null() => {
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
                        end,
                    ],
                ) if kw == "or" && end.is_null() => {
                    let subversion_refs = subversion_refs
                        .iter()
                        .map(SubVersionReference::parse)
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Self::Or(subversion_refs))
                }
                Some([Syntax::Identifier { ident: kw, .. }, subversion_ref, end])
                    if kw == "not" && end.is_null() =>
                {
                    let subversion_ref = SubVersionReference::parse(subversion_ref)?;
                    Ok(Self::Not(Box::new(subversion_ref)))
                }
                None => Err(error::expected_list(form)),
                _ => Err(error::bad_form(form, None)),
            },
        }
    }
}

impl fmt::Display for SubVersionReference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SubVersion(sv) => write!(f, "{sv}"),
            Self::Gte(sv) => write!(f, "(>= {sv})"),
            Self::Lte(sv) => write!(f, "(<= {sv})"),
            Self::And(svs) => {
                write!(f, "(and ")?;
                for (i, sv) in svs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{sv}")?;
                }
                write!(f, ")")
            }
            Self::Or(svs) => {
                write!(f, "(and ")?;
                for (i, sv) in svs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{sv}")?;
                }
                write!(f, ")")
            }
            Self::Not(sv) => {
                write!(f, "(not {sv})")
            }
        }
    }
}

#[derive(Debug)]
pub enum ExportSet {
    Internal {
        rename: Option<Symbol>,
        name: Symbol,
    },
    External(ImportSpec),
}

impl ExportSet {
    pub fn parse_rename(form: &Syntax) -> Result<Self, Exception> {
        match form.as_list() {
            Some(
                [
                    Syntax::Identifier { ident: from, .. },
                    Syntax::Identifier { ident: to, .. },
                    end,
                ],
            ) if end.is_null() => Ok(Self::Internal {
                rename: Some(to.sym),
                name: from.sym,
            }),
            _ => Err(error::bad_form(form, None)),
        }
    }

    pub fn parse(form: &Syntax) -> Result<Vec<Self>, Exception> {
        match form {
            Syntax::Identifier { ident, .. } => Ok(vec![Self::Internal {
                rename: None,
                name: ident.sym,
            }]),
            Syntax::List { list, .. } => match list.as_slice() {
                [Syntax::Identifier { ident, .. }, renames @ .., end]
                    if ident == "rename" && end.is_null() =>
                {
                    Ok(renames
                        .iter()
                        .map(Self::parse_rename)
                        .collect::<Result<Vec<_>, _>>()?)
                }
                [Syntax::Identifier { ident, .. }, .., end]
                    if ident == "import" && end.is_null() =>
                {
                    Ok(vec![Self::External(ImportSpec::parse(form)?)])
                }
                _ => Err(error::bad_form(form, None)),
            },
            _ => Err(error::bad_form(form, None)),
        }
    }
}

#[derive(Default, Debug)]
pub struct ExportSpec {
    pub(crate) export_sets: Vec<ExportSet>,
}

impl ExportSpec {
    pub fn parse(form: &Syntax) -> Result<Self, Exception> {
        match form.as_list() {
            Some(
                [
                    Syntax::Identifier {
                        ident: export_decl, ..
                    },
                    exports @ ..,
                    end,
                ],
            ) if export_decl == "export" && end.is_null() => Ok(ExportSpec {
                export_sets: exports
                    .iter()
                    .map(ExportSet::parse)
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten()
                    .collect(),
            }),
            _ => Err(error::expected_export_spec(form)),
        }
    }

    pub fn join(&mut self, rhs: ExportSpec) {
        self.export_sets.extend(rhs.export_sets);
    }
}

#[derive(Default, Debug)]
pub struct ImportSpec {
    pub(crate) import_sets: Vec<ImportSet>,
}

impl ImportSpec {
    pub fn parse(form: &Syntax) -> Result<Self, Exception> {
        match form.as_list() {
            Some(
                [
                    Syntax::Identifier {
                        ident: import_decl, ..
                    },
                    imports @ ..,
                    end,
                ],
            ) if import_decl == "import" && end.is_null() => Ok(ImportSpec {
                import_sets: imports
                    .iter()
                    .map(|import| ImportSet::parse(discard_for(import)))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            _ => Err(error::expected_import_spec(form)),
        }
    }

    pub fn join(&mut self, rhs: ImportSpec) {
        self.import_sets.extend(rhs.import_sets);
    }
}

pub(crate) fn discard_for(syn: &Syntax) -> &Syntax {
    match syn.as_list() {
        Some(
            [
                Syntax::Identifier { ident: for_kw, .. },
                import_set,
                _import_level @ ..,
                end,
            ],
        ) if for_kw == "for" && end.is_null() => {
            // We should eventually check that the import levels are well
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
        allowed: HashSet<Symbol>,
    },
    Except {
        set: Box<ImportSet>,
        disallowed: HashSet<Symbol>,
    },
    Prefix {
        set: Box<ImportSet>,
        prefix: Symbol,
    },
    Rename {
        set: Box<ImportSet>,
        /// Imported identifiers to rename (from, to).
        renames: HashMap<Symbol, Symbol>,
    },
}

impl ImportSet {
    pub fn parse(form: &Syntax) -> Result<Self, Exception> {
        match form.as_list() {
            Some(
                [
                    Syntax::Identifier {
                        ident: import_type, ..
                    },
                    lib_ref,
                    end,
                ],
            ) if import_type == "library" && end.is_null() => {
                Ok(Self::Library(LibraryReference::parse(lib_ref)?))
            }
            Some(
                [
                    Syntax::Identifier {
                        ident: import_type, ..
                    },
                    import_set,
                    imports @ ..,
                    end,
                ],
            ) if import_type == "only" && end.is_null() => {
                let import_set = ImportSet::parse(import_set)?;
                let allowed = imports
                    .iter()
                    .map(|allowed| match allowed {
                        Syntax::Identifier { ident, .. } => Ok(ident.sym),
                        _ => Err(error::expected_identifier(form, Some(allowed))),
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
                    end,
                ],
            ) if import_type == "except" && end.is_null() => {
                let import_set = ImportSet::parse(import_set)?;
                let disallowed = exceptions
                    .iter()
                    .map(|disallowed| match disallowed {
                        Syntax::Identifier { ident, .. } => Ok(ident.sym),
                        _ => Err(error::expected_identifier(form, Some(disallowed))),
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
                    end,
                ],
            ) if import_type == "prefix" && end.is_null() => {
                let import_set = ImportSet::parse(import_set)?;
                Ok(Self::Prefix {
                    set: Box::new(import_set),
                    prefix: prefix.sym,
                })
            }
            Some(
                [
                    Syntax::Identifier {
                        ident: import_type, ..
                    },
                    import_set,
                    renames @ ..,
                    end,
                ],
            ) if import_type == "rename" && end.is_null() => {
                let import_set = ImportSet::parse(import_set)?;
                let renames = renames
                    .iter()
                    .map(|rename| match rename.as_list() {
                        Some(
                            [
                                Syntax::Identifier { ident: from, .. },
                                Syntax::Identifier { ident: to, .. },
                                end,
                            ],
                        ) if end.is_null() => Ok((from.sym, to.sym)),
                        _ => Err(error::bad_form(form, Some(rename))),
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?;
                Ok(Self::Rename {
                    set: Box::new(import_set),
                    renames,
                })
            }
            Some(_) => Ok(Self::Library(LibraryReference::parse(form)?)),
            _ => Err(error::expected_list(form)),
        }
    }

    /// Returns the base library name for this import set, recursively unwrapping
    /// any Only/Except/Prefix/Rename wrappers.
    pub fn library_name(&self) -> &[Symbol] {
        match self {
            ImportSet::Library(lib_ref) => &lib_ref.name,
            ImportSet::Only { set, .. }
            | ImportSet::Except { set, .. }
            | ImportSet::Prefix { set, .. }
            | ImportSet::Rename { set, .. } => set.library_name(),
        }
    }
}

impl FromStr for ImportSet {
    type Err = Exception;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let form = Syntax::from_str(s, None)?;
        match form.as_list() {
            Some([item, end]) if end.is_null() => Self::parse(item),
            _ => Err(Exception::error(format!("bad form in '{s}'"))),
        }
    }
}

#[derive(Debug)]
pub struct LibraryReference {
    pub(crate) name: Vec<Symbol>,
    pub(crate) version_ref: VersionReference,
}

impl LibraryReference {
    fn parse(form: &Syntax) -> Result<Self, Exception> {
        match form.as_list() {
            Some([syms @ .., version_ref @ Syntax::List { .. }, end]) if end.is_null() => {
                let name = syms
                    .iter()
                    .map(|atom| match atom {
                        Syntax::Identifier { ident, .. } => Ok(ident.sym),
                        _ => Err(error::expected_identifier(form, Some(atom))),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let version_ref = VersionReference::parse(version_ref)?;
                Ok(LibraryReference { name, version_ref })
            }
            Some([syms @ .., end]) if end.is_null() => {
                let name = syms
                    .iter()
                    .map(|atom| match atom {
                        Syntax::Identifier { ident, .. } => Ok(ident.sym),
                        _ => Err(error::expected_identifier(form, Some(atom))),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(LibraryReference {
                    name,
                    version_ref: VersionReference::SubVersions(Vec::new()),
                })
            }
            None => Err(error::expected_list(form)),
            _ => Err(error::bad_form(form, None)),
        }
    }
}

/// A set of library names that are permitted to be imported.
#[derive(Clone, Debug, Default)]
pub struct AllowList(HashSet<Vec<Symbol>>);

impl AllowList {
    /// Create an allowlist from string library names.
    ///
    /// Each entry is a slice of strings representing the library name
    /// components, e.g., `&["rnrs", "base"]` for `(rnrs base)`.
    pub fn from_slice(libs: &[&[&str]]) -> Self {
        let set = libs
            .iter()
            .map(|lib| lib.iter().map(|s| Symbol::intern(s)).collect())
            .collect();
        AllowList(set)
    }

    /// Add a library to the allowlist.
    pub fn add_lib(&mut self, lib: Vec<Symbol>) {
        self.0.insert(lib);
    }

    /// Returns `true` if the given library name is in the allowlist.
    pub fn contains(&self, lib_name: &[Symbol]) -> bool {
        self.0.contains(lib_name)
    }
}

impl FromStr for AllowList {
    type Err = Exception;

    /// Parse an allowlist from a string of s-expressions.
    ///
    /// The string should be a list of library names, e.g.,
    /// `"((rnrs base) (rnrs io simple))"`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let form = Syntax::from_str(s, None)?;
        // Syntax::from_str wraps in an outer program-level list; unwrap it
        let Some([inner, end]) = form.as_list() else {
            return Err(Exception::error(format!("expected list in '{s}'")));
        };
        if !end.is_null() {
            return Err(Exception::error(format!("expected single list in '{s}'")));
        }
        let Some([libs @ .., end]) = inner.as_list() else {
            return Err(Exception::error(format!("expected list in '{s}'")));
        };
        if !end.is_null() {
            return Err(Exception::error(format!("improper list in '{s}'")));
        }
        let mut set = HashSet::new();
        for lib in libs {
            let Some([syms @ .., end]) = lib.as_list() else {
                return Err(Exception::error(format!(
                    "expected library name list in '{s}'"
                )));
            };
            if !end.is_null() {
                return Err(Exception::error(format!(
                    "improper library name list in '{s}'"
                )));
            }
            let name = syms
                .iter()
                .map(|atom| match atom {
                    Syntax::Identifier { ident, .. } => Ok(ident.sym),
                    _ => Err(error::expected_identifier(inner, Some(atom))),
                })
                .collect::<Result<Vec<_>, _>>()?;
            set.insert(name);
        }
        Ok(AllowList(set))
    }
}

/// Controls which libraries may be imported during evaluation.
#[derive(Clone, Debug)]
pub enum ImportPolicy {
    /// All imports are allowed.
    Allow,
    /// Only the listed libraries may be imported. An empty set means no imports
    /// are allowed.
    AllowList(AllowList),
}

impl ImportPolicy {
    /// Returns `true` if the given import set is permitted under this policy.
    pub fn is_allowed(&self, import_set: &ImportSet) -> bool {
        match self {
            ImportPolicy::Allow => true,
            ImportPolicy::AllowList(allowed) => allowed.contains(import_set.library_name()),
        }
    }

    /// Create an import policy from an allowlist.
    pub fn allow_only(allow_list: AllowList) -> Self {
        ImportPolicy::AllowList(allow_list)
    }

    /// Create an allowlist with no entries.
    pub fn deny_all() -> Self {
        ImportPolicy::AllowList(AllowList::default())
    }
}

impl From<bool> for ImportPolicy {
    fn from(allow: bool) -> Self {
        if allow {
            ImportPolicy::Allow
        } else {
            ImportPolicy::deny_all()
        }
    }
}

pub struct ParseContext {
    runtime: Runtime,
    import_policy: ImportPolicy,
}

impl ParseContext {
    pub fn new(runtime: &Runtime, import_policy: impl Into<ImportPolicy>) -> Self {
        Self {
            runtime: runtime.clone(),
            import_policy: import_policy.into(),
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
    pub(crate) fn parse(
        ctxt: &ParseContext,
        syn: &[Syntax],
        env: &Environment,
        form: &Syntax,
    ) -> Result<Self, Exception> {
        match syn {
            [_, Syntax::Identifier { ident, .. }, expr, end] if end.is_null() => {
                Ok(Definition::DefineVar(DefineVar {
                    var: maybe_await!(env.lookup_var(ident.bind()))?.unwrap(),
                    val: Arc::new(maybe_await!(Expression::parse(ctxt, expr.clone(), env))?),
                    next: None,
                }))
            }
            [_, Syntax::List { list, .. }, body @ .., end] if end.is_null() => {
                if body.is_empty() {
                    return Err(error::expected_body(form));
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
                        let var = maybe_await!(env.lookup_var(func_name.bind()))?.unwrap();
                        let mut bound = HashSet::<&Identifier>::new();
                        let mut fixed = Vec::new();
                        let func_scope = Scope::new();
                        let new_env = env.new_lexical_contour(func_scope);
                        let mut arg_names = Vec::new();

                        // Bind the arguments to a new environment:
                        for arg in &args[..args.len() - 1] {
                            match arg {
                                Syntax::Identifier { ident, .. } => {
                                    if bound.contains(ident) {
                                        return Err(error::name_previously_bound(form, arg));
                                    }
                                    bound.insert(ident);
                                    arg_names.push(ident.sym);
                                    let mut arg = ident.clone();
                                    arg.add_scope(func_scope);
                                    let local = new_env
                                        .def_var(arg.new_bind(), arg.sym)
                                        .as_local()
                                        .unwrap();
                                    fixed.push(local);
                                }
                                x => return Err(error::expected_identifier(form, Some(x))),
                            }
                        }

                        let args = if let Some(last) = args.last() {
                            match last {
                                empty if empty.is_null() => {
                                    Formals::FixedArgs(fixed.into_iter().collect())
                                }
                                Syntax::Identifier { ident, .. } => {
                                    if bound.contains(ident) {
                                        return Err(error::name_previously_bound(form, last));
                                    }
                                    bound.insert(ident);
                                    arg_names.push(ident.sym);
                                    let mut remaining = ident.clone();
                                    remaining.add_scope(func_scope);
                                    let remaining = new_env
                                        .def_var(remaining.new_bind(), remaining.sym)
                                        .as_local()
                                        .unwrap();
                                    Formals::VarArgs {
                                        fixed: fixed.into_iter().collect(),
                                        remaining,
                                    }
                                }
                                x => return Err(error::expected_identifier(form, Some(x))),
                            }
                        } else {
                            // If there is no last argument, there are no arguments
                            Formals::FixedArgs(Vec::new())
                        };

                        // Parse the body:
                        let mut body = body.to_vec();
                        body.iter_mut().for_each(|s| s.add_scope(func_scope));
                        let body =
                            maybe_await!(DefinitionBody::parse(ctxt, &body, &new_env, form))?;

                        Ok(Self::DefineFunc(DefineFunc {
                            var,
                            args,
                            body: Box::new(body),
                            next: None,
                            span: func_span.clone(),
                        }))
                    }
                    _ => Err(error::bad_form(form, None)),
                }
            }
            _ => Err(error::bad_form(form, None)),
        }
    }
}

#[maybe_async]
pub(super) fn define_syntax(
    ctxt: &ParseContext,
    binding: Binding,
    expr: Syntax,
    env: &Environment,
) -> Result<(), Exception> {
    let expanded = maybe_await!(expr.expand(env))?;
    let expr = maybe_await!(Expression::parse(ctxt, expanded, env))?;
    let cps_expr = expr.compile_top_level();
    let values = maybe_await!(maybe_await!(ctxt.runtime.compile_expr(cps_expr)).call(&[]))?;
    let transformer: Procedure = values.expect1()?;
    env.def_keyword(binding, transformer);
    Ok(())
}

#[derive(Debug, Clone, Trace)]
pub enum Expression {
    Undefined,
    Literal(Value),
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
    Begin(ExprBody),
}

impl Expression {
    #[maybe_async]
    pub(crate) fn parse(
        ctxt: &ParseContext,
        form: Syntax,
        env: &Environment,
    ) -> Result<Self, Exception> {
        let expanded = maybe_await!(form.expand(env))?;
        maybe_await!(Self::parse_expanded(ctxt, expanded, env))
    }

    #[cfg(not(feature = "async"))]
    fn parse_expanded(
        ctxt: &ParseContext,
        form: Syntax,
        env: &Environment,
    ) -> Result<Self, Exception> {
        Self::parse_expanded_inner(ctxt, form, env)
    }

    #[cfg(feature = "async")]
    fn parse_expanded<'a>(
        ctxt: &'a ParseContext,
        form: Syntax,
        env: &'a Environment,
    ) -> BoxFuture<'a, Result<Self, Exception>> {
        Box::pin(Self::parse_expanded_inner(ctxt, form, env))
    }

    #[maybe_async]
    fn parse_expanded_inner(
        ctxt: &ParseContext,
        form: Syntax,
        env: &Environment,
    ) -> Result<Self, Exception> {
        match &form {
            syn if syn.is_null() => Err(error::unexpected_empty_list(&form, None)),

            // Regular identifiers:
            Syntax::Identifier { ident, .. } => {
                if let Some(binding) = ident.resolve() {
                    if let Some(primitive) = env.lookup_primitive(binding) {
                        match primitive {
                            Primitive::Undefined => Ok(Self::Undefined),
                            _ => Err(error::bad_form(&form, None)),
                        }
                    } else if let Some(var) = maybe_await!(env.lookup_var(binding))? {
                        Ok(Self::Var(var))
                    } else {
                        Err(error::undefined_variable(&form, None))
                    }
                } else {
                    Err(error::undefined_variable(&form, None))
                }
            }

            // Literals:
            Syntax::Wrapped { value, .. } => Ok(Self::Literal(value.clone())),

            // Vector literals:
            Syntax::Vector { vector, .. } => Ok(Self::Vector(Vector::parse(vector))),

            // Functional forms:
            Syntax::List { list: exprs, .. } => match exprs.as_slice() {
                // Special forms:
                [
                    ident_form @ Syntax::Identifier { ident, .. },
                    tail @ ..,
                    end,
                ] if end.is_null() => {
                    let Some(binding) = ident.resolve() else {
                        return Err(error::undefined_variable(&form, Some(ident_form)));
                    };
                    if let Some(primitive) = env.lookup_primitive(binding) {
                        match primitive {
                            Primitive::Begin => maybe_await!(ExprBody::parse(ctxt, tail, env))
                                .map(Expression::Begin),
                            Primitive::Lambda => {
                                maybe_await!(Lambda::parse(ctxt, tail, env, &form))
                                    .map(Expression::Lambda)
                            }
                            Primitive::Let => maybe_await!(Let::parse(ctxt, tail, env, &form))
                                .map(Expression::Let),
                            Primitive::If => {
                                maybe_await!(If::parse(ctxt, tail, env, &form)).map(Expression::If)
                            }
                            Primitive::And => {
                                maybe_await!(And::parse(ctxt, tail, env)).map(Expression::And)
                            }
                            Primitive::Or => {
                                maybe_await!(Or::parse(ctxt, tail, env)).map(Expression::Or)
                            }
                            Primitive::Quote => Quote::parse(tail, &form).map(Expression::Quote),
                            Primitive::Syntax => {
                                SyntaxQuote::parse(tail, env, &form).map(Expression::SyntaxQuote)
                            }
                            Primitive::SyntaxCase => {
                                maybe_await!(SyntaxCase::parse(ctxt, tail, env, &form))
                                    .map(Expression::SyntaxCase)
                            }
                            Primitive::Set => maybe_await!(Set::parse(ctxt, tail, env, &form))
                                .map(Expression::Set),
                            Primitive::LetSyntax if !tail.is_empty() => {
                                let (form, env) = maybe_await!(parse_let_syntax(
                                    ctxt,
                                    false,
                                    &tail[0],
                                    &tail[1..],
                                    env,
                                    &mut Vec::new(),
                                ))?;
                                maybe_await!(ExprBody::parse(ctxt, &form, &env))
                                    .map(Expression::Begin)
                            }
                            Primitive::LetRecSyntax if !tail.is_empty() => {
                                let (form, env) = maybe_await!(parse_let_syntax(
                                    ctxt,
                                    true,
                                    &tail[0],
                                    &tail[1..],
                                    env,
                                    &mut Vec::new(),
                                ))?;
                                maybe_await!(ExprBody::parse(ctxt, &form, &env))
                                    .map(Expression::Begin)
                            }
                            Primitive::Import => Err(error::unexpected_import(&form)),
                            Primitive::Define => Err(error::unexpected_define(&form)),
                            _ => Err(error::bad_form(&form, None)),
                        }
                    } else if let Some(var) = maybe_await!(env.lookup_var(binding))? {
                        maybe_await!(Apply::parse(ctxt, Expression::Var(var), tail, env, &form))
                            .map(Expression::Apply)
                    } else {
                        Err(error::undefined_variable(&form, Some(ident_form)))
                    }
                }
                [expr, args @ .., end] if end.is_null() => maybe_await!(Apply::parse(
                    ctxt,
                    maybe_await!(Expression::parse(ctxt, expr.clone(), env))?,
                    args,
                    env,
                    &form
                ))
                .map(Expression::Apply),
                _ => Err(error::bad_form(&form, None)),
            },
        }
    }

    // These function pointer comparisons are guaranteed to be meaningful since
    // they are returned from a store.
    pub fn to_primop(&self) -> Option<PrimOp> {
        use crate::{
            lists::{cons, list},
            num::{add, div, equal, greater, greater_equal, lesser, lesser_equal, mul, sub},
            proc::{BridgePtr, FuncPtr::Bridge, Procedure},
        };
        use std::ptr::fn_addr_eq;

        const PRIMOP_TAB: &[(BridgePtr, PrimOp)] = &[
            (add, PrimOp::Add),
            (sub, PrimOp::Sub),
            (mul, PrimOp::Mul),
            (div, PrimOp::Div),
            (equal, PrimOp::Equal),
            (greater, PrimOp::Greater),
            (greater_equal, PrimOp::GreaterEqual),
            (lesser, PrimOp::Lesser),
            (lesser_equal, PrimOp::LesserEqual),
            (cons, PrimOp::Cons),
            (list, PrimOp::List),
        ];

        let Expression::Var(Var::Global(global)) = self else {
            return None;
        };
        let val: Procedure = global.read().try_into().ok()?;

        let Bridge(ptr) = val.0.func else {
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

/*
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
*/

#[derive(Debug, Clone, Trace)]
pub struct Quote {
    pub val: Value,
}

impl Quote {
    fn parse(exprs: &[Syntax], form: &Syntax) -> Result<Self, Exception> {
        match exprs {
            [] => Err(error::expected_more_arguments(form)),
            [expr] => Ok(Quote {
                val: Value::datum_from_syntax(expr),
            }),
            [_, arg, ..] => Err(error::unexpected_argument(form, arg)),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct SyntaxQuote {
    pub template: Template,
    pub expansions: HashMap<Binding, Local>,
}

impl SyntaxQuote {
    fn parse(exprs: &[Syntax], env: &Environment, form: &Syntax) -> Result<Self, Exception> {
        match exprs {
            [] => Err(error::expected_more_arguments(form)),
            [expr] => {
                let mut expansions = HashMap::new();
                let template = Template::compile(expr, env, &mut expansions)?;
                Ok(SyntaxQuote {
                    template,
                    expansions,
                })
            }
            [_, arg, ..] => Err(error::unexpected_argument(form, arg)),
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
        ctxt: &ParseContext,
        operator: Expression,
        args: &[Syntax],
        env: &Environment,
        form: &Syntax,
    ) -> Result<Self, Exception> {
        let mut parsed_args = Vec::new();
        for arg in args {
            parsed_args.push(maybe_await!(Expression::parse(ctxt, arg.clone(), env))?);
        }
        Ok(Apply {
            operator: Box::new(operator),
            args: parsed_args,
            span: form.span().clone(),
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
        ctxt: &ParseContext,
        sexprs: &[Syntax],
        env: &Environment,
        form: &Syntax,
    ) -> Result<Self, Exception> {
        match sexprs {
            [null, body @ ..] if null.is_null() => {
                maybe_await!(parse_lambda(ctxt, &[], body, env, form))
            }
            [Syntax::List { list: args, .. }, body @ ..] => {
                maybe_await!(parse_lambda(ctxt, args, body, env, form))
            }
            [ident @ Syntax::Identifier { .. }, body @ ..] => {
                maybe_await!(parse_lambda(
                    ctxt,
                    std::slice::from_ref(ident),
                    body,
                    env,
                    form,
                ))
            }
            _ => Err(error::expected_more_arguments(form)),
        }
    }
}

#[maybe_async]
fn parse_lambda(
    ctxt: &ParseContext,
    args: &[Syntax],
    body: &[Syntax],
    env: &Environment,
    form: &Syntax,
) -> Result<Lambda, Exception> {
    let mut bound = HashSet::<&Identifier>::new();
    let mut fixed = Vec::new();
    let lambda_scope = Scope::new();
    let new_contour = env.new_lexical_contour(lambda_scope);
    let mut arg_names = Vec::new();

    if !args.is_empty() {
        for arg in &args[..args.len() - 1] {
            match arg {
                Syntax::Identifier { ident, .. } => {
                    if bound.contains(ident) {
                        return Err(error::name_previously_bound(form, arg));
                    }
                    arg_names.push(ident.sym);
                    bound.insert(ident);
                    let mut arg = ident.clone();
                    arg.add_scope(lambda_scope);
                    let local = new_contour
                        .def_var(arg.new_bind(), arg.sym)
                        .as_local()
                        .unwrap();
                    fixed.push(local);
                }
                x => return Err(error::expected_identifier(form, Some(x))),
            }
        }
    }

    let args = if let Some(last) = args.last() {
        match last {
            empty if empty.is_null() => Formals::FixedArgs(fixed.into_iter().collect()),
            // Syntax::Null { .. } => Formals::FixedArgs(fixed.into_iter().collect()),
            Syntax::Identifier { ident, .. } => {
                if bound.contains(ident) {
                    return Err(error::name_previously_bound(form, last));
                }
                arg_names.push(ident.sym);
                let mut remaining = ident.clone();
                remaining.add_scope(lambda_scope);
                let remaining = new_contour
                    .def_var(remaining.new_bind(), remaining.sym)
                    .as_local()
                    .unwrap();
                Formals::VarArgs {
                    fixed: fixed.into_iter().collect(),
                    remaining,
                }
            }
            x => return Err(error::expected_identifier(form, Some(x))),
        }
    } else {
        // If there is no last argument, there are no arguments
        Formals::FixedArgs(Vec::new())
    };

    let mut body = body.to_vec();
    body.iter_mut().for_each(|s| s.add_scope(lambda_scope));
    let body = maybe_await!(DefinitionBody::parse(ctxt, &body, &new_contour, form))?;

    Ok(Lambda {
        args,
        body,
        span: form.span().clone(),
    })
}

#[derive(Debug, Clone, Trace)]
pub struct Let {
    pub bindings: Vec<(Local, Expression)>,
    pub body: DefinitionBody,
}

impl Let {
    #[maybe_async]
    fn parse(
        ctxt: &ParseContext,
        syn: &[Syntax],
        env: &Environment,
        form: &Syntax,
    ) -> Result<Self, Exception> {
        match syn {
            [empty, body @ ..] if empty.is_null() => {
                maybe_await!(parse_let(ctxt, &[], body, env, form))
            }
            [Syntax::List { list: bindings, .. }, body @ ..] => {
                maybe_await!(parse_let(ctxt, bindings, body, env, form))
            }
            // Named let:
            [
                Syntax::Identifier { ident, .. },
                Syntax::List { list: bindings, .. },
                body @ ..,
            ] => maybe_await!(parse_named_let(ctxt, ident, bindings, body, env, form)),
            [Syntax::Identifier { ident, .. }, empty, body @ ..] if empty.is_null() => {
                maybe_await!(parse_named_let(ctxt, ident, &[], body, env, form))
            }
            _ => Err(error::expected_more_arguments(form)),
        }
    }
}

#[maybe_async]
fn parse_let(
    ctxt: &ParseContext,
    bindings: &[Syntax],
    body: &[Syntax],
    env: &Environment,
    form: &Syntax,
) -> Result<Let, Exception> {
    let mut previously_bound = HashSet::new();
    let mut parsed_bindings = Vec::new();

    let new_scope = Scope::new();
    let new_contour = env.new_lexical_contour(new_scope);

    match bindings {
        [] => (),
        [empty] if empty.is_null() => (),
        [bindings @ .., end] if end.is_null() => {
            for binding in bindings {
                let binding = maybe_await!(LetBinding::parse(
                    ctxt,
                    binding,
                    env,
                    &previously_bound,
                    form
                ))?;
                previously_bound.insert(binding.ident);
                let mut var = binding.ident.clone();
                var.add_scope(new_scope);
                let var = new_contour
                    .def_var(var.new_bind(), var.sym)
                    .as_local()
                    .unwrap();
                parsed_bindings.push((var, binding.expr));
            }
        }
        _ => {
            return Err(error::expected_list(form));
        }
    }

    let mut body = body.to_vec();
    body.iter_mut().for_each(|s| s.add_scope(new_scope));
    let body = maybe_await!(DefinitionBody::parse(ctxt, &body, &new_contour, form))?;

    Ok(Let {
        bindings: parsed_bindings,
        body,
    })
}

#[maybe_async]
fn parse_named_let(
    ctxt: &ParseContext,
    name: &Identifier,
    bindings: &[Syntax],
    body: &[Syntax],
    env: &Environment,
    form: &Syntax,
) -> Result<Let, Exception> {
    let mut previously_bound = HashSet::new();
    let mut formals = Vec::new();
    let mut args = Vec::new();

    let func_scope = Scope::new();
    let func_contour = env.new_lexical_contour(func_scope);

    let mut func_name = name.clone();
    func_name.add_scope(func_scope);
    let func = func_contour.def_var(func_name.new_bind(), func_name.sym);

    let body_scope = Scope::new();
    let body_contour = func_contour.new_lexical_contour(body_scope);

    match bindings {
        [] => (),
        [empty] if empty.is_null() => (),
        [bindings @ .., end] if end.is_null() => {
            for binding in bindings {
                let binding = maybe_await!(LetBinding::parse(
                    ctxt,
                    binding,
                    env,
                    &previously_bound,
                    form
                ))?;
                previously_bound.insert(binding.ident);
                args.push(binding.expr);
                let mut arg = binding.ident.clone();
                arg.add_scope(func_scope);
                arg.add_scope(body_scope);
                let var = body_contour
                    .def_var(arg.new_bind(), arg.sym)
                    .as_local()
                    .unwrap();
                formals.push(var);
            }
        }
        _ => {
            return Err(error::expected_list(form));
        }
    }

    let mut body = body.to_vec();
    body.iter_mut().for_each(|s| {
        s.add_scope(func_scope);
        s.add_scope(body_scope);
    });
    let body = maybe_await!(DefinitionBody::parse(ctxt, &body, &body_contour, form))?;

    let func = DefineFunc {
        var: func.clone(),
        args: Formals::FixedArgs(formals),
        body: Box::new(body),
        next: Some(Either::Right(ExprBody::new(vec![Expression::Apply(
            Apply {
                operator: Box::new(Expression::Var(func)),
                args,
                span: form.span().clone(),
            },
        )]))),
        span: form.span().clone(),
    };

    Ok(Let {
        bindings: Vec::new(),
        body: DefinitionBody::new(Either::Left(Definition::DefineFunc(func))),
    })
}

struct LetBinding<'a> {
    ident: &'a Identifier,
    expr: Expression,
}

impl<'a> LetBinding<'a> {
    #[maybe_async]
    fn parse(
        ctxt: &ParseContext,
        binding: &'a Syntax,
        env: &Environment,
        previously_bound: &HashSet<&'a Identifier>,
        form: &'a Syntax,
    ) -> Result<LetBinding<'a>, Exception> {
        if let Some(
            [
                subform @ Syntax::Identifier { ident, .. },
                expr,
                end, // Syntax::Null { .. },
            ],
        ) = binding.as_list()
            && end.is_null()
        {
            if previously_bound.contains(ident) {
                return Err(error::name_previously_bound(form, subform));
            }

            let expr = maybe_await!(Expression::parse(ctxt, expr.clone(), env))?;

            Ok(LetBinding { ident, expr })
        } else {
            Err(error::expected_list(binding))
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
        ctxt: &ParseContext,
        exprs: &[Syntax],
        env: &Environment,
        form: &Syntax,
    ) -> Result<Self, Exception> {
        match exprs {
            [] | [_] => Err(error::expected_more_arguments(form)),
            [subform @ Syntax::Identifier { ident, .. }, expr] => Ok(Set {
                var: {
                    if let Some(binding) = ident.resolve() {
                        match maybe_await!(env.lookup_var(binding))? {
                            Some(Var::Global(global)) if !global.mutable => {
                                return Err(error::immutable_variable(form, subform));
                            }
                            Some(var) => var,
                            None => {
                                return Err(error::undefined_variable(form, Some(subform)));
                            }
                        }
                    } else {
                        return Err(error::undefined_variable(form, Some(subform)));
                    }
                },
                val: Arc::new(maybe_await!(Expression::parse(ctxt, expr.clone(), env))?),
            }),
            [arg1, _] => Err(error::expected_identifier(form, Some(arg1))),
            [_, _, arg3, ..] => Err(error::unexpected_argument(form, arg3)),
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
        ctxt: &ParseContext,
        exprs: &[Syntax],
        env: &Environment,
        form: &Syntax,
    ) -> Result<Self, Exception> {
        match exprs {
            [cond, success] => Ok(If {
                cond: Arc::new(maybe_await!(Expression::parse(ctxt, cond.clone(), env))?),
                success: Arc::new(maybe_await!(Expression::parse(ctxt, success.clone(), env))?),
                failure: None,
            }),
            [cond, success, failure] => Ok(If {
                cond: Arc::new(maybe_await!(Expression::parse(ctxt, cond.clone(), env))?),
                success: Arc::new(maybe_await!(Expression::parse(ctxt, success.clone(), env))?),
                failure: Some(Arc::new(maybe_await!(Expression::parse(
                    ctxt,
                    failure.clone(),
                    env
                ))?)),
            }),
            [] => Err(error::expected_more_arguments(form)),
            [_] => Err(error::expected_more_arguments(form)),
            [_, _, _, unexpected, ..] => Err(error::unexpected_argument(form, unexpected)),
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
    pub(crate) fn parse_lib_body(
        runtime: &Runtime,
        form: &Syntax,
        env: &Environment,
    ) -> Result<Self, Exception> {
        let ctxt = ParseContext {
            runtime: runtime.clone(),
            import_policy: ImportPolicy::Allow,
        };
        // No explanation needed
        match form.as_list() {
            Some([list @ .., end]) if end.is_null() => {
                maybe_await!(Self::parse_helper(&ctxt, list, true, env, form))
            }
            _ => Err(error::bad_form(form, None)),
        }
    }

    #[maybe_async]
    pub(crate) fn parse(
        ctxt: &ParseContext,
        body: &[Syntax],
        env: &Environment,
        form: &Syntax,
    ) -> Result<Self, Exception> {
        maybe_await!(Self::parse_helper(ctxt, body, false, env, form))
    }

    /// Parse the body. body is expected to be a list of valid syntax objects, and should not include
    /// _any_ nulls, including one at the end.
    #[cfg(not(feature = "async"))]
    fn parse_helper(
        ctxt: &ParseContext,
        body: &[Syntax],
        permissive: bool,
        env: &Environment,
        form: &Syntax,
    ) -> Result<Self, Exception> {
        Self::parse_helper_inner(ctxt, body, permissive, env, form)
    }

    /// Parse the body. body is expected to be a list of valid syntax objects, and should not include
    /// _any_ nulls, including one at the end.
    #[cfg(feature = "async")]
    fn parse_helper<'a>(
        ctxt: &'a ParseContext,
        body: &'a [Syntax],
        permissive: bool,
        env: &'a Environment,
        form: &'a Syntax,
    ) -> BoxFuture<'a, Result<Self, Exception>> {
        Box::pin(Self::parse_helper_inner(ctxt, body, permissive, env, form))
    }

    #[maybe_async]
    fn parse_helper_inner(
        runtime: &ParseContext,
        body: &[Syntax],
        permissive: bool,
        env: &Environment,
        form: &Syntax,
    ) -> Result<Self, Exception> {
        let mut defs = Vec::new();
        let mut exprs = Vec::new();
        let mut introduced_scopes = Vec::new();

        maybe_await!(splice_in(
            runtime,
            permissive,
            body,
            env,
            form,
            &mut defs,
            &mut exprs,
            &mut introduced_scopes,
        ))?;

        let mut defs_parsed = Vec::new();
        let mut exprs_parsed = Vec::new();

        // Mark all of the defs as defined:
        for (def, _) in defs.iter() {
            if let Some([_, def, ..]) = def.as_list() {
                let ident = match def.as_list() {
                    Some([Syntax::Identifier { ident, .. }, ..]) => ident,
                    _ => def.as_ident().expect(
                        "define should have already been parsed and determined to be well-formed",
                    ),
                };
                let mut ident = ident.clone();
                // Remove any scopes introduced by a let-syntax or
                // letrec-syntax form
                for scope in &introduced_scopes {
                    ident.remove_scope(*scope);
                }
                let binding = ident.bind();
                env.def_var(binding, ident.sym);
            }
        }

        for (def, env) in defs.into_iter() {
            let def = maybe_await!(Definition::parse(
                runtime,
                def.as_list().unwrap(),
                &env,
                &def,
            ))?;
            defs_parsed.push(def);
        }

        for (expr, env) in exprs.into_iter() {
            exprs_parsed.push(maybe_await!(Expression::parse_expanded(
                runtime, expr, &env,
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
    fn parse(ctxt: &ParseContext, body: &[Syntax], env: &Environment) -> Result<Self, Exception> {
        let mut exprs = Vec::new();
        for sexpr in body {
            let parsed = maybe_await!(Expression::parse(ctxt, sexpr.clone(), env))?;
            exprs.push(parsed);
        }
        Ok(Self { exprs })
    }
}

#[cfg(not(feature = "async"))]
#[allow(clippy::too_many_arguments)]
fn splice_in(
    ctxt: &ParseContext,
    permissive: bool,
    body: &[Syntax],
    env: &Environment,
    form: &Syntax,
    defs: &mut Vec<(Syntax, Environment)>,
    exprs: &mut Vec<(Syntax, Environment)>,
    introduced_scopes: &mut Vec<Scope>,
) -> Result<(), Exception> {
    splice_in_inner(
        ctxt,
        permissive,
        body,
        env,
        form,
        defs,
        exprs,
        introduced_scopes,
    )
}

#[cfg(feature = "async")]
#[allow(clippy::too_many_arguments)]
fn splice_in<'a>(
    ctxt: &'a ParseContext,
    permissive: bool,
    body: &'a [Syntax],
    env: &'a Environment,
    form: &'a Syntax,
    defs: &'a mut Vec<(Syntax, Environment)>,
    exprs: &'a mut Vec<(Syntax, Environment)>,
    introduced_scopes: &'a mut Vec<Scope>,
) -> BoxFuture<'a, Result<(), Exception>> {
    Box::pin(splice_in_inner(
        ctxt,
        permissive,
        body,
        env,
        form,
        defs,
        exprs,
        introduced_scopes,
    ))
}

#[maybe_async]
#[allow(clippy::too_many_arguments)]
fn splice_in_inner(
    ctxt: &ParseContext,
    permissive: bool,
    body: &[Syntax],
    env: &Environment,
    form: &Syntax,
    defs: &mut Vec<(Syntax, Environment)>,
    exprs: &mut Vec<(Syntax, Environment)>,
    introduced_scopes: &mut Vec<Scope>,
) -> Result<(), Exception> {
    if body.is_empty() {
        return Err(error::expected_body(form));
    }
    for unexpanded in body {
        let expanded = maybe_await!(unexpanded.clone().expand(env))?;
        let is_def = {
            if let Some([Syntax::Identifier { ident, .. }, tail @ .., end]) = expanded.as_list()
                && end.is_null()
            {
                let primitive = ident
                    .resolve()
                    .and_then(|binding| env.lookup_primitive(binding));
                match (primitive, tail) {
                    (Some(Primitive::Begin), []) => {
                        continue;
                    }
                    (Some(Primitive::Begin), body) => {
                        maybe_await!(splice_in(
                            ctxt,
                            permissive,
                            body,
                            env,
                            &expanded,
                            defs,
                            exprs,
                            introduced_scopes,
                        ))?;
                        continue;
                    }
                    (
                        Some(Primitive::DefineSyntax),
                        [Syntax::Identifier { ident: name, .. }, expr],
                    ) => {
                        maybe_await!(define_syntax(ctxt, name.bind(), expr.clone(), env))?;
                        continue;
                    }
                    (Some(Primitive::DefineSyntax), _) => {
                        return Err(error::unexpected_define_syntax(&expanded));
                    }
                    (Some(Primitive::LetSyntax), [bindings, form @ ..]) => {
                        let (form, env) = maybe_await!(parse_let_syntax(
                            ctxt,
                            false,
                            bindings,
                            form,
                            env,
                            introduced_scopes
                        ))?;
                        if !form.is_empty() {
                            maybe_await!(splice_in(
                                ctxt,
                                permissive,
                                &form,
                                &env,
                                &expanded,
                                defs,
                                exprs,
                                introduced_scopes
                            ))?;
                        }
                        continue;
                    }
                    (Some(Primitive::LetRecSyntax), [bindings, form @ ..]) => {
                        let (form, env) = maybe_await!(parse_let_syntax(
                            ctxt,
                            true,
                            bindings,
                            form,
                            env,
                            introduced_scopes
                        ))?;
                        if !form.is_empty() {
                            maybe_await!(splice_in(
                                ctxt,
                                permissive,
                                &form,
                                &env,
                                &expanded,
                                defs,
                                exprs,
                                introduced_scopes
                            ))?;
                        }
                        continue;
                    }
                    (Some(Primitive::Import), imports) => {
                        if !permissive && !exprs.is_empty() {
                            return Err(error::unexpected_import(&expanded));
                        }
                        // First pass: parse all import sets and validate against
                        // our import policy before executing any of them.
                        let mut parsed_imports = Vec::new();
                        for import in imports {
                            let import_set = ImportSet::parse(discard_for(import))?;
                            if !ctxt.import_policy.is_allowed(&import_set) {
                                return Err(error::import_not_permitted(
                                    &expanded,
                                    import_set.library_name(),
                                ));
                            }
                            parsed_imports.push(import_set);
                        }
                        // Second pass: execute all imports (all have been validated)
                        for import_set in parsed_imports {
                            maybe_await!(env.import(import_set))?;
                        }
                        continue;
                    }
                    (Some(Primitive::Define), _) => {
                        if !permissive && !exprs.is_empty() {
                            return Err(error::unexpected_define(&expanded));
                        }
                        true
                    }
                    _ => false,
                }
            } else {
                false
            }
        };

        if is_def {
            defs.push((expanded, env.clone()));
        } else {
            exprs.push((expanded, env.clone()));
        }
    }

    Ok(())
}

#[maybe_async]
fn parse_let_syntax(
    ctxt: &ParseContext,
    recursive: bool,
    bindings: &Syntax,
    exprs: &[Syntax],
    env: &Environment,
    introduced_scopes: &mut Vec<Scope>,
) -> Result<(Vec<Syntax>, Environment), Exception> {
    let new_scope = Scope::new();
    let new_env = env.new_lexical_contour(new_scope);
    let mut body = exprs.to_vec();

    if bindings.is_null() {
        return Ok((body, new_env));
    }

    let Some([keyword_bindings @ .., end]) = bindings.as_list() else {
        return Err(error::expected_list(bindings));
    };

    if !end.is_null() {
        return Err(error::expected_list(bindings));
    }

    introduced_scopes.push(new_scope);

    for binding in keyword_bindings {
        if let Some([Syntax::Identifier { ident, .. }, expr, end]) = binding.as_list()
            && end.is_null()
        {
            let mut name = ident.clone();
            name.add_scope(new_scope);
            let mut expr = expr.clone();
            if recursive {
                expr.add_scope(new_scope);
            }
            let bind = name.new_bind();
            maybe_await!(define_syntax(ctxt, bind, expr, &new_env))?;
        } else {
            return Err(error::bad_form(bindings, Some(binding)));
        }
    }

    body.iter_mut().for_each(|s| s.add_scope(new_scope));
    if recursive {
        let disambig_scope = Scope::new();
        introduced_scopes.push(disambig_scope);
        body.iter_mut().for_each(|s| s.add_scope(disambig_scope));
    }

    Ok((body, new_env))
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
    fn parse(ctxt: &ParseContext, exprs: &[Syntax], env: &Environment) -> Result<Self, Exception> {
        let mut output = Vec::new();
        for expr in exprs {
            let expr = maybe_await!(Expression::parse(ctxt, expr.clone(), env))?;
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
    fn parse(ctxt: &ParseContext, exprs: &[Syntax], env: &Environment) -> Result<Self, Exception> {
        let mut output = Vec::new();
        for expr in exprs {
            let expr = maybe_await!(Expression::parse(ctxt, expr.clone(), env))?;
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
        ctxt: &ParseContext,
        exprs: &[Syntax],
        env: &Environment,
        form: &Syntax,
    ) -> Result<Self, Exception> {
        let (arg, keywords, mut rules) = match exprs {
            [arg, Syntax::List { list, .. }, rules @ ..] => {
                let mut keywords = HashSet::default();
                // TODO: ensure keywords_list is proper
                for keyword in &list[..list.len() - 1] {
                    if let Syntax::Identifier { ident, .. } = keyword {
                        keywords.insert(ident);
                    } else {
                        return Err(error::expected_identifier(form, Some(keyword)));
                    }
                }
                (arg, keywords, rules)
            }
            [arg, empty, rules @ ..] if empty.is_null() => (arg, HashSet::default(), rules),
            _ => return Err(error::bad_form(form, None)),
        };
        let mut syntax_rules = Vec::new();
        loop {
            match rules {
                [] => break,
                [Syntax::List { list, .. }, tail @ ..] => match &list[..] {
                    [pattern, output_expression, end] if end.is_null() => {
                        syntax_rules.push(maybe_await!(SyntaxRule::compile(
                            ctxt,
                            &keywords,
                            pattern,
                            None,
                            output_expression,
                            env,
                        ))?);
                        rules = tail;
                    }
                    [pattern, fender, output_expression, end] if end.is_null() => {
                        syntax_rules.push(maybe_await!(SyntaxRule::compile(
                            ctxt,
                            &keywords,
                            pattern,
                            Some(fender),
                            output_expression,
                            env,
                        ))?);
                        rules = tail;
                    }
                    _ => return Err(error::bad_form(form, None)),
                },
                _ => return Err(error::bad_form(form, None)),
            }
        }
        Ok(SyntaxCase {
            arg: Arc::new(maybe_await!(Expression::parse(ctxt, arg.clone(), env))?),
            rules: syntax_rules,
        })
    }
}
