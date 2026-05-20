use lsp_types::{CompletionItem, CompletionItemKind, CompletionResponse, Position, Uri};
use scheme_rs_macros::{maybe_async, maybe_await};

use crate::{
    env::{Binding, Environment, GLOBAL_BINDING_TABLE, Var},
    proc::Procedure,
    runtime::Runtime,
    symbols::Symbol,
};

use super::document::{completion_prefix, import_visible_libraries, parse_document};

#[maybe_async]
pub(super) fn completions_for_document(
    runtime: &Runtime,
    uri: &Uri,
    text: &str,
    position: Position,
) -> CompletionResponse {
    // PERF: This reparses and rebuilds imports on every completion request.
    // Cache document analysis on open/change if completion starts feeling slow.
    let prefix = completion_prefix(text, position);
    let mut items = if let Ok((form, env)) = parse_document(runtime, uri, text)
        && maybe_await!(import_visible_libraries(&form, &env)).is_ok()
    {
        maybe_await!(environment_completion_items(&env, &prefix))
    } else {
        Vec::new()
    };

    items.sort_by(|left, right| left.label.cmp(&right.label));
    items.dedup_by(|left, right| left.label == right.label);
    CompletionResponse::Array(items)
}

#[maybe_async]
fn environment_completion_items(env: &Environment, prefix: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    for (name, binding) in imported_bindings(env) {
        let label = name.to_string();
        if !label.starts_with(prefix) {
            continue;
        }

        if env.lookup_primitive(binding).is_some() {
            items.push(completion_item(
                label,
                CompletionItemKind::KEYWORD,
                "primitive syntax",
            ));
        } else if maybe_await!(env.lookup_keyword(binding))
            .ok()
            .flatten()
            .is_some()
        {
            items.push(completion_item(
                label,
                CompletionItemKind::KEYWORD,
                "syntax keyword",
            ));
        } else if let Some(var) = maybe_await!(env.lookup_var(binding)).ok().flatten() {
            items.push(var_completion_item(label, var));
        }
    }
    items
}

fn imported_bindings(env: &Environment) -> Vec<(Symbol, Binding)> {
    let top = env.fetch_top();
    let imported = top.0.read().imports.keys().copied().collect::<Vec<_>>();
    let scope_set = env.get_scope_set();

    GLOBAL_BINDING_TABLE
        .lock()
        .iter()
        .flat_map(|(symbol, bindings)| {
            bindings
                .iter()
                .filter_map(|(scopes, binding)| {
                    (imported.contains(binding) && scopes.is_subset(&scope_set))
                        .then_some((*symbol, *binding))
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn var_completion_item(label: String, var: Var) -> CompletionItem {
    match var {
        Var::Global(global) => {
            let value = global.read();
            if value.cast_to_scheme_type::<Procedure>().is_some() {
                completion_item(label, CompletionItemKind::FUNCTION, "procedure")
            } else {
                CompletionItem {
                    label,
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail: (!value.is_undefined()).then(|| value.type_name().to_string()),
                    ..Default::default()
                }
            }
        }
        Var::Local(_) => completion_item(label, CompletionItemKind::VARIABLE, "local variable"),
    }
}

fn completion_item(label: String, kind: CompletionItemKind, detail: &str) -> CompletionItem {
    CompletionItem {
        label,
        kind: Some(kind),
        detail: Some(detail.to_string()),
        ..Default::default()
    }
}
