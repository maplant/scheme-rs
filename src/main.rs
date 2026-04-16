use clap::Parser;
use parking_lot::Mutex;
use rustyline::{
    Completer, Config, Editor, Helper, Highlighter, Hinter, Validator,
    highlight::MatchingBracketHighlighter,
    history::DefaultHistory,
    validate::{ValidationContext, ValidationResult, Validator},
};
use scheme_rs::{
    env::TopLevelEnvironment,
    exceptions::Exception,
    ports::{BufferMode, IntoPort, Port, Prompt, ReadFn, Transcoder},
    runtime::Runtime,
    syntax::{Span, Syntax},
};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::{path::Path, process, sync::Arc};

#[derive(Parser, Debug)]
struct Args {
    /// Scheme programs to run
    files: Vec<String>,
    /// Force interactive mode (REPL)
    #[arg(short, long)]
    interactive: bool,
}

#[cfg(not(feature = "async"))]
use rustyline::history::FileHistory;

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

pub struct TextStoringPrompt {
    #[cfg(not(feature = "async"))]
    prompt: Prompt<InputHelper, FileHistory>,
    #[cfg(feature = "async")]
    prompt: Prompt,
    text: Arc<Mutex<String>>,
}

#[cfg(not(feature = "async"))]
impl IntoPort for TextStoringPrompt {
    fn read_fn() -> Option<ReadFn> {
        let prompt_read_fn = Prompt::<InputHelper, FileHistory>::read_fn().unwrap();
        Some(Box::new(move |any, buff, start, count| {
            let this = any.downcast_mut::<Self>().unwrap();
            let written = (prompt_read_fn)(&mut this.prompt, buff, start, count)?;
            this.text
                .lock()
                .push_str(str::from_utf8(&buff.as_slice()[start..(start + written)]).unwrap());
            Ok(written)
        }))
    }
}

#[cfg(feature = "async")]
impl IntoPort for TextStoringPrompt {
    fn read_fn() -> Option<ReadFn> {
        Some(Box::new(move |any, buff, start, count| {
            Box::pin(async move {
                let prompt_read_fn = Prompt::read_fn().unwrap();
                let this = any.downcast_mut::<Self>().unwrap();
                let written = (prompt_read_fn)(&mut this.prompt, buff, start, count).await?;
                this.text
                    .lock()
                    .push_str(str::from_utf8(&buff.as_slice()[start..(start + written)]).unwrap());
                Ok(written)
            })
        }))
    }
}

/// scheme-rs entry point
#[maybe_async]
fn entry(runtime: &Runtime) -> Result<(), Exception> {
    let args = Args::parse();

    // Run any programs
    for file in &args.files {
        let path = Path::new(file);
        maybe_await!(runtime.run_program(path))?;
    }

    if !args.files.is_empty() && !args.interactive {
        return Ok(());
    }

    let repl = TopLevelEnvironment::new_repl(runtime);

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

    let text = Arc::new(Mutex::new(String::new()));

    let prompt = TextStoringPrompt {
        prompt: Prompt::new(editor),
        text: text.clone(),
    };

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
            Err(exception) => {
                let mut source_store = runtime.write_sources();
                source_store.store(
                    span.file.clone(),
                    text.lock().lines().map(|x| x.to_string()).collect(),
                );
                let mut out = String::new();
                exception.pretty_print(&source_store, &mut out).unwrap();
                print!("{out}");
            }
        }
    }

    Ok(())
}

#[maybe_async]
#[cfg_attr(feature = "async", tokio::main)]
fn main() {
    let runtime = Runtime::new();

    if let Err(exception) = maybe_await!(entry(&runtime)) {
        let mut out = String::new();
        exception
            .pretty_print(&runtime.read_sources(), &mut out)
            .unwrap();
        print!("{out}");
        process::exit(1);
    };
}
