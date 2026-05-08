//! MVP Language Server Protocol support for Scheme.
//!
//! Adapted from sqleibniz's small stdio LSP: full-document sync,
//! stdio transport, and parse-on-open/change diagnostics.

mod error;

pub use error::LspError;
use lsp_server::{Connection, Message, Notification};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, Position, PublishDiagnosticsParams, Range, ServerCapabilities,
    TextDocumentSyncKind, TextDocumentSyncOptions, Uri,
    notification::{DidChangeTextDocument, DidOpenTextDocument, Notification as _},
};
use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    ast::{DefinitionBody, Primitive},
    env::{Environment, TopLevelEnvironment},
    exceptions::{Exception, Message as SchemeMessage, PrettyCondition, SyntaxViolation},
    runtime::Runtime,
    syntax::{Span, Syntax, lex::LexerError, parse::ParseSyntaxError},
};

macro_rules! lsp_log {
    ($($arg:tt)*) => {
        eprintln!("[scheme-rs-lsp]: {}", format_args!($($arg)*))
    };
}

#[maybe_async]
pub fn start() -> Result<(), LspError> {
    lsp_log!("starting language server");

    let (connection, io_threads) = Connection::stdio();
    let capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                ..Default::default()
            },
        )),
        ..Default::default()
    })
    .map_err(LspError::SerializeCapabilities)?;

    match connection.initialize(capabilities) {
        Ok(_) => {}
        Err(err) => {
            if err.channel_is_disconnected() {
                io_threads.join().map_err(|_| LspError::JoinIoThreads)?;
            }
            return Err(err.into());
        }
    };

    let runtime = Runtime::new();
    maybe_await!(event_loop(connection, &runtime))?;

    io_threads.join().map_err(|_| LspError::JoinIoThreads)?;
    lsp_log!("shutting down language server");
    Ok(())
}

#[maybe_async]
fn event_loop(connection: Connection, runtime: &Runtime) -> Result<(), LspError> {
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                lsp_log!("unsupported request: {}", req.method);
            }
            Message::Response(_) => {}
            Message::Notification(notification) => match notification.method.as_str() {
                DidOpenTextDocument::METHOD => {
                    let params = cast_notification::<DidOpenTextDocument>(notification)?;
                    maybe_await!(publish_diagnostics(
                        &connection,
                        runtime,
                        params.text_document.uri,
                        &params.text_document.text,
                    ))?;
                }
                DidChangeTextDocument::METHOD => {
                    let params = cast_notification::<DidChangeTextDocument>(notification)?;
                    if let Some(change) = params.content_changes.first() {
                        maybe_await!(publish_diagnostics(
                            &connection,
                            runtime,
                            params.text_document.uri,
                            &change.text
                        ))?;
                    }
                }
                method => lsp_log!("unsupported notification: {method}"),
            },
        }
    }

    Ok(())
}

#[maybe_async]
fn publish_diagnostics(
    connection: &Connection,
    runtime: &Runtime,
    uri: Uri,
    text: &str,
) -> Result<(), LspError> {
    let diagnostics = maybe_await!(diagnostics_for_document(runtime, &uri, text));
    connection
        .sender
        .send(Message::Notification(Notification::new(
            "textDocument/publishDiagnostics".to_string(),
            PublishDiagnosticsParams {
                diagnostics,
                uri,
                version: None,
            },
        )))
        .map_err(|_| LspError::PublishDiagnostics)?;

    Ok(())
}

#[maybe_async]
fn diagnostics_for_document(runtime: &Runtime, uri: &Uri, text: &str) -> Vec<Diagnostic> {
    let file_name = uri
        .as_str()
        .strip_prefix("file://")
        .unwrap_or(uri.as_str())
        .to_string();

    let mut form = match Syntax::from_str(text, Some(&file_name)) {
        Ok(form) => form,
        Err(err) => return vec![diagnostic_from_parse_error(err, text)],
    };

    let program = TopLevelEnvironment::new_program(runtime, file_name.as_ref());
    let env = Environment::Top(program.clone());
    form.add_scope(program.scope());

    let add_rnrs_import = if let Some(first_form) = form.car()
        && let Some(Syntax::Identifier { ident, .. }) = first_form.car()
        && let Some(binding) = ident.resolve()
    {
        env.lookup_primitive(binding) != Some(Primitive::Import)
    } else {
        true
    };

    if add_rnrs_import
        && let Err(err) = maybe_await!(env.import("(library (rnrs))".parse().unwrap()))
    {
        return vec![diagnostic_from_exception(err, text)];
    }

    match maybe_await!(DefinitionBody::parse_lib_body(runtime, &form, &env)) {
        Ok(_) => Vec::new(),
        Err(err) => vec![diagnostic_from_exception(err, text)],
    }
}

fn diagnostic_from_parse_error(err: ParseSyntaxError, text: &str) -> Diagnostic {
    diagnostic(
        err.to_string(),
        parse_error_span(&err)
            .map(range_from_span)
            .unwrap_or_else(|| eof_range(text)),
    )
}

fn diagnostic_from_exception(err: Exception, text: &str) -> Diagnostic {
    let range = err
        .condition::<SyntaxViolation>()
        .ok()
        .flatten()
        .map(|syntax| token_range(text, &syntax.span()))
        .unwrap_or_else(|| eof_range(text));
    let message = err
        .condition::<SchemeMessage>()
        .ok()
        .flatten()
        .map(|message| message.message.clone())
        .unwrap_or_else(|| err.to_string());

    let message = match (message.as_str(), token_at(text, &range)) {
        ("undefined variable", Some(token)) => format!("undefined variable `{token}`"),
        _ => message,
    };
    diagnostic(message, range)
}

fn diagnostic(message: String, range: Range) -> Diagnostic {
    Diagnostic {
        range,
        message,
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("scheme-rs".into()),
        ..Default::default()
    }
}

fn token_range(text: &str, span: &Span) -> Range {
    let mut range = range_from_span(span);
    if let Some(token) = token_at(text, &range) {
        range.end.character = range.start.character + token.chars().count() as u32;
    }
    range
}

fn token_at<'a>(text: &'a str, range: &Range) -> Option<&'a str> {
    let line = text.lines().nth(range.start.line as usize)?;
    let start = line
        .char_indices()
        .nth(range.start.character as usize)
        .map(|(idx, _)| idx)
        .unwrap_or(line.len());
    line[start..]
        .split(|chr: char| chr.is_whitespace() || matches!(chr, '(' | ')' | '[' | ']' | '"' | ';'))
        .next()
        .filter(|token| !token.is_empty())
}

fn parse_error_span(err: &ParseSyntaxError) -> Option<&Span> {
    match err {
        ParseSyntaxError::ExpectedClosingParen { span }
        | ParseSyntaxError::UnexpectedClosingParen { span }
        | ParseSyntaxError::InvalidPeriodLocation { span }
        | ParseSyntaxError::NonByte { span }
        | ParseSyntaxError::UnclosedParen { span } => Some(span),
        ParseSyntaxError::Lex(err) => match err {
            LexerError::InvalidCharacterInHexEscape { span, .. }
            | LexerError::UnexpectedCharacter { span, .. }
            | LexerError::BadEscapeCharacter { span, .. } => Some(span),
            LexerError::UnexpectedEof | LexerError::ReadError(_) => None,
        },
        ParseSyntaxError::UnexpectedToken { token } => Some(&token.span),
        ParseSyntaxError::UnexpectedEof
        | ParseSyntaxError::CharTryFrom(_)
        | ParseSyntaxError::ParseNumberError(_) => None,
    }
}

fn range_from_span(span: &Span) -> Range {
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

fn eof_range(text: &str) -> Range {
    let line = text.matches('\n').count() as u32;
    let character = text.rsplit('\n').next().unwrap_or("").chars().count() as u32;
    Range::new(Position { line, character }, Position { line, character })
}

fn cast_notification<N>(notification: Notification) -> Result<N::Params, LspError>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    notification
        .extract(N::METHOD)
        .map_err(LspError::ExtractNotification)
}
