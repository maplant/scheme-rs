use lsp_types::{Position, Range, Uri};
use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    ast::{ImportSpec, Primitive},
    env::{Environment, TopLevelEnvironment},
    exceptions::Exception,
    runtime::Runtime,
    syntax::{Span, Syntax, parse::ParseSyntaxError},
};

pub(super) fn parse_document(
    runtime: &Runtime,
    uri: &Uri,
    text: &str,
) -> Result<(Syntax, Environment), ParseSyntaxError> {
    let file_name = document_file_name(uri);
    let mut form = Syntax::from_str(text, Some(&file_name))?;
    let program = TopLevelEnvironment::new_program(runtime, file_name.as_ref());
    let env = Environment::Top(program.clone());
    form.add_scope(program.scope());
    Ok((form, env))
}

fn document_file_name(uri: &Uri) -> String {
    uri.as_str()
        .strip_prefix("file://")
        .unwrap_or(uri.as_str())
        .to_string()
}

#[maybe_async]
pub(super) fn import_visible_libraries(form: &Syntax, env: &Environment) -> Result<(), Exception> {
    if starts_with_import(form, env) {
        maybe_await!(import_document_imports(form, env))?;
    } else {
        maybe_await!(env.import("(library (rnrs))".parse().unwrap()))?;
    }

    Ok(())
}

fn starts_with_import(form: &Syntax, env: &Environment) -> bool {
    if let Some(first_form) = form.car()
        && let Some(Syntax::Identifier { ident, .. }) = first_form.car()
        && let Some(binding) = ident.resolve()
    {
        env.lookup_primitive(binding) == Some(Primitive::Import)
    } else {
        false
    }
}

#[maybe_async]
fn import_document_imports(form: &Syntax, env: &Environment) -> Result<(), Exception> {
    let Some(forms) = form.as_list() else {
        return Ok(());
    };

    for form in forms.iter().take_while(|form| !form.is_null()) {
        if !form.has_car("import") {
            continue;
        }
        for import_set in ImportSpec::parse(form)?.import_sets {
            maybe_await!(env.import(import_set))?;
        }
    }

    Ok(())
}

pub(super) fn completion_prefix(text: &str, position: Position) -> String {
    let Some(line) = text.lines().nth(position.line as usize) else {
        return String::new();
    };
    let end = line
        .char_indices()
        .nth(position.character as usize)
        .map(|(idx, _)| idx)
        .unwrap_or(line.len());
    line[..end]
        .split(is_token_delimiter)
        .next_back()
        .unwrap_or("")
        .to_string()
}

pub(super) fn token_range(text: &str, span: &Span) -> Range {
    let mut range = range_from_span(span);
    if let Some(token) = token_at(text, &range) {
        range.end.character = range.start.character + token.chars().count() as u32;
    }
    range
}

pub(super) fn token_at<'a>(text: &'a str, range: &Range) -> Option<&'a str> {
    let line = text.lines().nth(range.start.line as usize)?;
    let start = line
        .char_indices()
        .nth(range.start.character as usize)
        .map(|(idx, _)| idx)
        .unwrap_or(line.len());
    line[start..]
        .split(is_token_delimiter)
        .next()
        .filter(|token| !token.is_empty())
}

fn is_token_delimiter(chr: char) -> bool {
    chr.is_whitespace() || matches!(chr, '(' | ')' | '[' | ']' | '"' | ';')
}

pub(super) fn range_from_span(span: &Span) -> Range {
    let line = span.line.saturating_sub(1);
    let character = span.column as u32;

    Range::new(
        Position { line, character },
        Position {
            line,
            character: character.saturating_add(1),
        },
    )
}

pub(super) fn eof_range(text: &str) -> Range {
    let line = text.matches('\n').count() as u32;
    let character = text.rsplit('\n').next().unwrap_or("").chars().count() as u32;
    Range::new(Position { line, character }, Position { line, character })
}
