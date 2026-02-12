use clap::Parser;
use rustyline::{
    Completer, Config, Editor, Helper, Highlighter, Hinter, Validator,
    highlight::MatchingBracketHighlighter,
    history::DefaultHistory,
    validate::{ValidationContext, ValidationResult, Validator},
};
use scheme_rs::{
    env::TopLevelEnvironment,
    exceptions::{Exception, Message, StackTrace, SyntaxViolation},
    gc::Gc,
    ports::{BufferMode, Port, Prompt, Transcoder},
    runtime::Runtime,
    syntax::{Span, Syntax},
};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::path::Path;

#[derive(Parser, Debug)]
struct Args {
    /// Scheme programs to run
    files: Vec<String>,
    /// Force interactive mode (REPL)
    #[arg(short, long)]
    interactive: bool,
}

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
fn main() -> Result<(), Exception> {
    let args = Args::parse();

    let runtime = Runtime::new();

    // Run any programs
    for file in &args.files {
        let path = Path::new(file);
        if let Err(e) = maybe_await!(runtime.run_program(path)) {
            print_exception(e);
        };
    }

    if !args.files.is_empty() && !args.interactive {
        return Ok(());
    }

    let repl = TopLevelEnvironment::new_repl(&runtime);

    maybe_await!(repl.import("(library (rnrs))".parse().unwrap()))
        .expect("Failed to import standard library");

    let config = Config::builder()
        .auto_add_history(true)
        .check_cursor_position(true)
        .build();
    let mut editor = match Editor::with_history(config, DefaultHistory::new()) {
        Ok(e) => e,
        Err(err) => {
            return Err(Exception::error(format!(
                "Error creating line editor: {err}"
            )));
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
                return Err(Exception::error(format!(
                    "Error while reading input: {err}"
                )));
            }
        };

        match maybe_await!(repl.eval_sexpr(true, sexpr)) {
            Ok(results) => {
                for result in results.into_iter() {
                    println!("${n_results} = {result:?}");
                    n_results += 1;
                }
            }
            Err(err) => print_exception(err),
        }
    }

    Ok(())
}

fn print_exception(exception: Exception) {
    let Ok(conditions) = exception.simple_conditions() else {
        println!(
            "Exception occurred with a non-condition value: {:?}",
            exception.0
        );
        return;
    };
    println!("Uncaught exception:");
    for condition in conditions.into_iter() {
        if let Some(message) = condition.cast_to_rust_type::<Message>() {
            println!(" - Message: {}", message.message);
        } else if let Some(syntax) = condition.cast_to_rust_type::<SyntaxViolation>() {
            println!(" - Syntax error in form: {:?}", syntax.form);
            if let Some(subform) = syntax.subform.as_ref() {
                println!("   (subform: {subform:?})");
            }
        } else if let Some(trace) = condition.cast_to_rust_type::<StackTrace>() {
            println!(" - Stack trace:");
            for (i, trace) in trace.trace.iter().enumerate() {
                let syntax = trace.cast_to_scheme_type::<Gc<Syntax>>().unwrap();
                let span = syntax.span();
                let func_name = syntax.as_ident().unwrap().symbol();
                println!("{:>6}: {func_name}:{span}", i + 1);
            }
        } else {
            println!(" - Condition: {condition:?}");
        }
    }
}
