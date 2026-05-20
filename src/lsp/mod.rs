//! MVP Language Server Protocol support for Scheme.
//!
//! Adapted from sqleibniz's small stdio LSP: full-document sync,
//! stdio transport, and parse-on-open/change diagnostics.

/// completion collection/classification.
mod completion;
/// parse/import setup, token/range helpers.
mod document;
mod error;
/// hover resolution/formatting
mod hover;

pub use error::LspError;
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};
use lsp_types::{
    CompletionOptions, CompletionResponse, Diagnostic, DiagnosticSeverity, HoverProviderCapability,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncKind,
    TextDocumentSyncOptions, Uri,
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
    },
    request::{Completion, HoverRequest, Request as _},
};
use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    ast::DefinitionBody,
    exceptions::{Exception, Message as SchemeMessage, PrettyCondition, SyntaxViolation},
    runtime::Runtime,
    syntax::{Span, lex::LexerError, parse::ParseSyntaxError},
};

use completion::completions_for_document;
use document::{
    eof_range, import_visible_libraries, parse_document, range_from_span, token_at, token_range,
};
use hover::hover_for_document;
use rustc_hash::FxHashMap as HashMap;

macro_rules! lsp_log {
    ($($arg:tt)*) => {
        eprintln!("[scheme-rs-lsp]: {}", format_args!($($arg)*))
    };
}

#[derive(Clone, Copy, Debug, Default)]
pub struct LspConfig {
    pub allow_macro_expansion: bool,
}

#[maybe_async]
pub fn start(config: LspConfig) -> Result<(), LspError> {
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
        completion_provider: Some(CompletionOptions::default()),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
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
    maybe_await!(event_loop(connection, &runtime, config))?;

    io_threads.join().map_err(|_| LspError::JoinIoThreads)?;
    lsp_log!("shutting down language server");
    Ok(())
}

#[maybe_async]
fn event_loop(
    connection: Connection,
    runtime: &Runtime,
    config: LspConfig,
) -> Result<(), LspError> {
    let mut documents = HashMap::<Uri, String>::default();

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                maybe_await!(handle_request(
                    &connection,
                    runtime,
                    config,
                    &documents,
                    req
                ))?;
            }
            Message::Response(_) => {}
            Message::Notification(notification) => match notification.method.as_str() {
                DidOpenTextDocument::METHOD => {
                    let params = cast_notification::<DidOpenTextDocument>(notification)?;
                    documents.insert(
                        params.text_document.uri.clone(),
                        params.text_document.text.clone(),
                    );
                    maybe_await!(publish_diagnostics(
                        &connection,
                        runtime,
                        config,
                        params.text_document.uri,
                        &params.text_document.text,
                    ))?;
                }
                DidChangeTextDocument::METHOD => {
                    let params = cast_notification::<DidChangeTextDocument>(notification)?;
                    if let Some(change) = params.content_changes.first() {
                        documents.insert(params.text_document.uri.clone(), change.text.clone());
                        maybe_await!(publish_diagnostics(
                            &connection,
                            runtime,
                            config,
                            params.text_document.uri,
                            &change.text
                        ))?;
                    }
                }
                DidCloseTextDocument::METHOD => {
                    let params = cast_notification::<DidCloseTextDocument>(notification)?;
                    documents.remove(&params.text_document.uri);
                }
                method => lsp_log!("unsupported notification: {method}"),
            },
        }
    }

    Ok(())
}

#[maybe_async]
fn handle_request(
    connection: &Connection,
    runtime: &Runtime,
    config: LspConfig,
    documents: &HashMap<Uri, String>,
    request: Request,
) -> Result<(), LspError> {
    match request.method.as_str() {
        Completion::METHOD => {
            let (id, params) = request
                .extract::<lsp_types::CompletionParams>(Completion::METHOD)
                .map_err(LspError::ExtractRequest)?;
            let uri = params.text_document_position.text_document.uri;
            let completions = if let Some(text) = documents.get(&uri) {
                maybe_await!(completions_for_document(
                    runtime,
                    &uri,
                    text,
                    params.text_document_position.position,
                ))
            } else {
                CompletionResponse::Array(Vec::new())
            };
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, completions)))
                .map_err(|_| LspError::SendResponse)?;
        }
        HoverRequest::METHOD => {
            let (id, params) = request
                .extract::<lsp_types::HoverParams>(HoverRequest::METHOD)
                .map_err(LspError::ExtractRequest)?;
            let uri = params.text_document_position_params.text_document.uri;
            let hover = if let Some(text) = documents.get(&uri) {
                maybe_await!(hover_for_document(
                    runtime,
                    config,
                    &uri,
                    text,
                    params.text_document_position_params.position,
                ))
            } else {
                None
            };
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, hover)))
                .map_err(|_| LspError::SendResponse)?;
        }
        method => {
            lsp_log!("unsupported request: {method}");
            connection
                .sender
                .send(Message::Response(Response::new_err(
                    request.id,
                    ErrorCode::MethodNotFound as i32,
                    format!("unsupported request: {method}"),
                )))
                .map_err(|_| LspError::SendResponse)?;
        }
    }

    Ok(())
}

#[maybe_async]
fn publish_diagnostics(
    connection: &Connection,
    runtime: &Runtime,
    config: LspConfig,
    uri: Uri,
    text: &str,
) -> Result<(), LspError> {
    let diagnostics = maybe_await!(diagnostics_for_document(runtime, config, &uri, text));
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
fn diagnostics_for_document(
    runtime: &Runtime,
    config: LspConfig,
    uri: &Uri,
    text: &str,
) -> Vec<Diagnostic> {
    let (form, env) = match parse_document(runtime, uri, text) {
        Ok(context) => context,
        Err(err) => return vec![diagnostic_from_parse_error(err, text)],
    };

    if !config.allow_macro_expansion {
        return Vec::new();
    }

    if let Err(err) = maybe_await!(import_visible_libraries(&form, &env)) {
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

fn cast_notification<N>(notification: Notification) -> Result<N::Params, LspError>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    notification
        .extract(N::METHOD)
        .map_err(LspError::ExtractNotification)
}
