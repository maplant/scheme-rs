use rustyline::{
    Completer, Config, Editor, Helper, Highlighter, Hinter, Validator,
    highlight::MatchingBracketHighlighter,
    history::DefaultHistory,
    validate::{ValidationContext, ValidationResult, Validator},
};
use scheme_rs::{
    ast::{DefinitionBody, ImportSet, ParseAstError},
    cps::Compile,
    env::Environment,
    exceptions::{Exception, ExceptionHandler},
    ports::{Port, ReadError},
    proc::{Application, DynamicWind},
    registry::Library,
    runtime::Runtime,
    syntax::{
        Syntax,
        parse::{LexerError, ParseSyntaxError, Parser},
    },
    value::Value,
};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::{io::Cursor, process::ExitCode};

#[derive(Default)]
struct InputValidator;

#[cfg(not(feature = "async"))]
impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext<'_>) -> rustyline::Result<ValidationResult> {
        let bytes = Cursor::new(ctx.input().as_bytes().to_vec());
        let port = Port::from_reader(bytes);
        let input_port = port.get_input_port().unwrap();
        let mut input_port = input_port.lock().unwrap();
        let mut parser = Parser::new("<prompt>", &mut input_port);
        if parser.all_datums().is_ok() {
            Ok(ValidationResult::Valid(None))
        } else {
            Ok(ValidationResult::Incomplete)
        }
    }
}

#[cfg(feature = "async")]
impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext<'_>) -> rustyline::Result<ValidationResult> {
        let bytes = Cursor::new(ctx.input().as_bytes().to_vec());
        let is_valid = futures::executor::block_on(async move {
            let port = Port::from_reader(bytes);
            let input_port = port.get_input_port().unwrap();
            let mut input_port = input_port.lock().await;
            let mut parser = Parser::new("<prompt>", &mut input_port);
            parser.all_datums().await.is_ok()
        });
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

    maybe_await!(repl.import(ImportSet::parse_from_str("(library (rnrs))").unwrap()))
        .expect("Failed to import standard library");

    let config = Config::builder().auto_add_history(true).build();
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

    let input_prompt = Port::from_prompt(editor);
    let input_port = input_prompt.get_input_port().unwrap();

    #[cfg(not(feature = "async"))]
    let mut input_port = input_port.lock().unwrap();

    #[cfg(feature = "tokio")]
    let mut input_port = input_port.lock().await;

    let mut sexpr_parser = Parser::new("<prompt>", &mut input_port);

    let mut n_results = 1;
    loop {
        let sexpr = match maybe_await!(sexpr_parser.get_datum()) {
            Ok(sexpr) => sexpr,
            Err(ParseSyntaxError::Lex(LexerError::ReadError(ReadError::Eof))) => break,
            Err(err) => {
                eprintln!("Error while reading input: {err}");
                return ExitCode::FAILURE;
            }
        };

        match maybe_await!(compile_and_run_str(&runtime, &repl, sexpr)) {
            Ok(results) => {
                for result in results.into_iter() {
                    println!("${n_results} = {result:?}");
                    n_results += 1;
                }
            }
            Err(EvalError::Exception(exception)) => {
                print!("{exception}");
            }
            Err(err) => {
                println!("Error: {err:?}");
            }
        }
    }

    ExitCode::SUCCESS
}

#[derive(derive_more::From, Debug)]
pub enum EvalError {
    ParseAstError(ParseAstError),
    Exception(Exception),
}

#[maybe_async]
fn compile_and_run_str(
    runtime: &Runtime,
    repl: &Library,
    sexpr: Syntax,
) -> Result<Vec<Value>, EvalError> {
    let env = Environment::Top(repl.clone());
    let span = sexpr.span().clone();
    let expr = maybe_await!(DefinitionBody::parse(runtime, &[sexpr], &env, &span))?;
    let compiled = expr.compile_top_level();
    let closure = maybe_await!(runtime.compile_expr(compiled));
    let result = maybe_await!(
        Application::new(
            closure,
            Vec::new(),
            ExceptionHandler::default(),
            DynamicWind::default(),
            None,
        )
        .eval()
    )?;
    Ok(result)
}
