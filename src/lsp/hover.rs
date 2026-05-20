use lsp_types::{Hover, HoverContents, MarkedString, Position, Range, Uri};
use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    ast::{DefinitionBody, Primitive},
    env::{Environment, Var},
    proc::Procedure,
    runtime::Runtime,
    syntax::{Identifier, Syntax},
};

use super::{
    LspConfig,
    document::{import_visible_libraries, parse_document, token_range},
};

#[maybe_async]
pub(super) fn hover_for_document(
    runtime: &Runtime,
    config: LspConfig,
    uri: &Uri,
    text: &str,
    position: Position,
) -> Option<Hover> {
    let (form, env) = parse_document(runtime, uri, text).ok()?;
    maybe_await!(import_visible_libraries(&form, &env)).ok()?;
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
