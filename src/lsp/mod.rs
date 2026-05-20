//! MVP Language Server Protocol support for Scheme.
//!
//! Adapted from sqleibniz's small stdio LSP: full-document sync,
//! stdio transport, and parse-on-open/change diagnostics.

mod error;

pub use error::LspError;
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, Hover, HoverContents, HoverProviderCapability, MarkedString,
    Position, PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncKind,
    TextDocumentSyncOptions, Uri,
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
    },
    request::{HoverRequest, Request as _},
};
use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    ast::{DefinitionBody, Primitive},
    env::{Environment, TopLevelEnvironment, Var},
    exceptions::{Exception, Message as SchemeMessage, PrettyCondition, SyntaxViolation},
    proc::Procedure,
    runtime::Runtime,
    syntax::{Identifier, Span, Syntax, lex::LexerError, parse::ParseSyntaxError},
};

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

    if let Err(err) = maybe_await!(import_default_rnrs(&form, &env)) {
        return vec![diagnostic_from_exception(err, text)];
    }

    match maybe_await!(DefinitionBody::parse_lib_body(runtime, &form, &env)) {
        Ok(_) => Vec::new(),
        Err(err) => vec![diagnostic_from_exception(err, text)],
    }
}

#[maybe_async]
fn hover_for_document(
    runtime: &Runtime,
    config: LspConfig,
    uri: &Uri,
    text: &str,
    position: Position,
) -> Option<Hover> {
    let (form, env) = parse_document(runtime, uri, text).ok()?;
    maybe_await!(import_default_rnrs(&form, &env)).ok()?;
    if config.allow_macro_expansion {
        let _ = maybe_await!(DefinitionBody::parse_lib_body(runtime, &form, &env));
    }

    let (ident, range) = identifier_at(&form, text, position)?;
    let contents = maybe_await!(hover_contents_for_identifier(&ident, &env))?;

    Some(Hover {
        contents: HoverContents::Scalar(MarkedString::String(contents)),
        range: Some(range),
    })
}

fn parse_document(
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
fn import_default_rnrs(form: &Syntax, env: &Environment) -> Result<(), Exception> {
    let starts_with_import = if let Some(first_form) = form.car()
        && let Some(Syntax::Identifier { ident, .. }) = first_form.car()
        && let Some(binding) = ident.resolve()
    {
        env.lookup_primitive(binding) == Some(Primitive::Import)
    } else {
        false
    };

    if !starts_with_import {
        maybe_await!(env.import("(library (rnrs))".parse().unwrap()))?;
    }

    Ok(())
}

#[maybe_async]
fn hover_contents_for_identifier(ident: &Identifier, env: &Environment) -> Option<String> {
    let binding = ident.resolve()?;
    if let Some(primitive) = env.lookup_primitive(binding) {
        return Some(primitive_hover(ident.symbol().to_string(), primitive));
    }
    if maybe_await!(env.lookup_keyword(binding))
        .ok()
        .flatten()
        .is_some()
    {
        return Some(format!("{}\n\nsyntax keyword", ident.symbol()));
    }
    if let Some(var) = maybe_await!(env.lookup_var(binding)).ok().flatten() {
        return Some(var_hover(var));
    }
    None
}

fn identifier_at(syntax: &Syntax, text: &str, position: Position) -> Option<(Identifier, Range)> {
    match syntax {
        Syntax::Identifier { ident, span } => {
            let range = token_range(text, span);
            range_contains_position(&range, position).then(|| (ident.clone(), range))
        }
        Syntax::List { list, .. } => list
            .iter()
            .find_map(|item| identifier_at(item, text, position)),
        Syntax::Vector { vector, .. } => vector
            .iter()
            .find_map(|item| identifier_at(item, text, position)),
        Syntax::Wrapped { .. } => None,
    }
}

fn range_contains_position(range: &Range, position: Position) -> bool {
    (position.line, position.character) >= (range.start.line, range.start.character)
        && (position.line, position.character) < (range.end.line, range.end.character)
}

fn primitive_hover(name: String, primitive: Primitive) -> String {
    let kind = if primitive == Primitive::Undefined {
        "primitive value"
    } else {
        "primitive syntax"
    };
    format!("{name}\n\n{kind}")
}

fn var_hover(var: Var) -> String {
    match var {
        Var::Local(local) => format!("{local}\n\nlocal variable"),
        Var::Global(global) => {
            let name = global.name.to_string();
            let mut hover = format!("{name}\n\nglobal variable");
            let value = global.read();
            if let Some(procedure) = value.cast_to_scheme_type::<Procedure>() {
                hover = procedure_hover("global procedure", name, &procedure);
            } else if !value.is_undefined() {
                hover.push_str(&format!("\n\ntype: {}", value.type_name()));
            }
            if global.is_mutable() {
                hover.push_str("\n\nmutable");
            }
            hover
        }
    }
}

fn procedure_hover(kind: &str, fallback_name: String, procedure: &Procedure) -> String {
    let (required, variadic) = procedure.get_formals();
    let (name, args) = procedure.get_debug_info().map_or_else(
        || {
            (
                fallback_name,
                (0..required + usize::from(variadic))
                    .map(|idx| format!("${idx}"))
                    .collect(),
            )
        },
        |debug_info| {
            (
                debug_info.name.to_string(),
                debug_info.args.iter().map(ToString::to_string).collect(),
            )
        },
    );

    format!(
        "{}\n\n{kind}",
        format_signature(name, args, required, variadic)
    )
}

fn format_signature(name: String, args: Vec<String>, required: usize, variadic: bool) -> String {
    if args.is_empty() {
        return format!("({name})");
    }

    let mut output = format!("({name}");
    for (idx, arg) in args.iter().enumerate() {
        if variadic && idx == required {
            output.push_str(" .");
        }
        output.push(' ');
        output.push_str(arg);
    }
    output.push(')');
    output
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
