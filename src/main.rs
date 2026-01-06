use rustyline::{
    Completer, Config, Editor, Helper, Highlighter, Hinter, Validator,
    highlight::MatchingBracketHighlighter,
    history::DefaultHistory,
    validate::{ValidationContext, ValidationResult, Validator},
};
use scheme_rs::{
    ast::{DefinitionBody, ImportSet, ParseContext},
    cps::Compile,
    env::Environment,
    exceptions::Exception,
    ports::{BufferMode, Port, Prompt, Transcoder},
    proc::{Application, DynStack},
    registry::Library,
    runtime::Runtime,
    syntax::{Span, Syntax},
    value::Value,
};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::process::ExitCode;

#[derive(Default)]
struct InputValidator;

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext<'_>) -> rustyline::Result<ValidationResult> {
        let is_valid = Syntax::from_str(ctx.input(), None).is_ok();
        if is_valid {
            Ok(ValidationResult::Valid(None))
        } else {
            Ok(ValidationResult::Incomplete)
        }
    }
}

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct InputHelper {
    #[rustyline(Validator)]
    validator: InputValidator,
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
}

#[maybe_async]
#[cfg_attr(feature = "async", tokio::main)]
fn main() -> ExitCode {
    let runtime = Runtime::new();
    let repl = Library::new_repl(&runtime);
    let env = Environment::Top(repl);

    maybe_await!(env.import(ImportSet::parse_from_str("(library (rnrs))").unwrap()))
        .expect("Failed to import standard library");

    let config = Config::builder()
        .auto_add_history(true)
        .check_cursor_position(true)
        .build();
    let mut editor = match Editor::with_history(config, DefaultHistory::new()) {
        Ok(e) => e,
        Err(err) => {
            eprintln!("Error creating line editor: {err}");
            return ExitCode::FAILURE;
        }
    };

    let helper = InputHelper {
        validator: InputValidator,
        highlighter: MatchingBracketHighlighter::new(),
    };

    editor.set_helper(Some(helper));

    let prompt = Prompt::new(editor);

    let mut span = Span::new("<prompt>");
    let input_port = Port::new(
        "<prompt>",
        prompt,
        BufferMode::Block,
        Some(Transcoder::native()),
    );

    let mut n_results = 1;
    loop {
        let sexpr = match maybe_await!(input_port.get_sexpr(span)) {
            Ok(Some((sexpr, new_span))) => {
                span = new_span;
                sexpr
            }
            Ok(None) => break,
            Err(err) => {
                eprintln!("Error while reading input: {err}");
                return ExitCode::FAILURE;
            }
        };

        match maybe_await!(compile_and_run_str(&runtime, &env, sexpr)) {
            Ok(results) => {
                for result in results.into_iter() {
                    println!("${n_results} = {result:?}");
                    n_results += 1;
                }
            }
            Err(err) => {
                println!("{err:?}");
            }
        }
    }

    ExitCode::SUCCESS
}

#[maybe_async]
fn compile_and_run_str(
    runtime: &Runtime,
    repl: &Environment,
    sexpr: Syntax,
) -> Result<Vec<Value>, Exception> {
    let ctxt = ParseContext::new(runtime, true);
    let sexprs = [sexpr];
    let expr = maybe_await!(DefinitionBody::parse(&ctxt, &sexprs, repl, &sexprs[0]))?;
    let compiled = expr.compile_top_level();
    let closure = maybe_await!(runtime.compile_expr(compiled));
    let result =
        maybe_await!(Application::new(closure, Vec::new()).eval(&mut DynStack::default()))?;
    Ok(result)
}
