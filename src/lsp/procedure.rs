use crate::proc::Procedure;

pub(super) struct ProcedureInfo {
    pub(super) signature: String,
    pub(super) snippet: Option<String>,
    pub(super) docs: Option<String>,
}

pub(super) fn procedure_info(fallback_name: String, procedure: &Procedure) -> ProcedureInfo {
    let (required, variadic) = procedure.get_formals();
    let (name, args, docs): (String, Vec<String>, Option<String>) =
        procedure.get_debug_info().map_or_else(
            || {
                (
                    fallback_name,
                    (0..required + usize::from(variadic))
                        .map(|idx| format!("${idx}"))
                        .collect(),
                    None,
                )
            },
            |debug_info| {
                (
                    debug_info.name.to_string(),
                    debug_info.args.iter().map(ToString::to_string).collect(),
                    debug_info.docs.clone(),
                )
            },
        );

    let signature = format_signature(&name, &args, required, variadic);
    let snippet = procedure_snippet(&name, &args);

    ProcedureInfo {
        signature,
        snippet,
        docs,
    }
}

fn format_signature(name: &str, args: &[String], required: usize, variadic: bool) -> String {
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

fn procedure_snippet(name: &str, args: &[String]) -> Option<String> {
    if args.is_empty() {
        return None;
    }

    let mut snippet = escape_snippet_text(name);
    for (idx, arg) in args.iter().enumerate() {
        let placeholder = idx + 1;
        snippet.push(' ');
        snippet.push_str(&format!(
            "${{{placeholder}:{}}}",
            escape_snippet_placeholder(arg)
        ));
    }
    Some(snippet)
}

fn escape_snippet_text(text: &str) -> String {
    text.replace('\\', "\\\\").replace('$', "\\$")
}

fn escape_snippet_placeholder(text: &str) -> String {
    escape_snippet_text(text).replace('}', "\\}")
}
