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
use std::{io::Cursor, process::{exit, ExitCode}};

#[derive(Default)]
struct InputValidator;

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext<'_>) -> rustyline::Result<ValidationResult> {
        let bytes = Cursor::new(ctx.input().as_bytes().to_vec());
        let is_valid = futures::executor::block_on(async move {
            let port = Port::from_reader("<prompt>", bytes);
            let mut parser = Parser::new(&port).await;
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

#[tokio::main]
async fn main() -> ExitCode {
    let runtime = Runtime::new();
    let repl = Library::new_repl(&runtime);

    repl.import(ImportSet::parse_from_str("(library (rnrs))").unwrap())
        .await
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
    let mut sexpr_parser = Parser::new(&input_prompt).await;

    let mut n_results = 1;
    loop {
        let sexpr = match sexpr_parser.get_datum().await {
            Ok(sexpr) => sexpr,
            Err(ParseSyntaxError::Lex(LexerError::ReadError(ReadError::Eof))) => break,
            Err(err) => {
                eprintln!("Error while reading input: {err}");
                return ExitCode::FAILURE;
            }
        };

        match compile_and_run_str(&runtime, &repl, sexpr).await {
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

    exit(0)
}

#[derive(derive_more::From, Debug)]
pub enum EvalError {
    ParseAstError(ParseAstError),
    Exception(Exception),
}

async fn compile_and_run_str(
    runtime: &Runtime,
    repl: &Library,
    sexpr: Syntax,
) -> Result<Vec<Value>, EvalError> {
    let env = Environment::Top(repl.clone());
    let span = sexpr.span().clone();
    let expr = DefinitionBody::parse(runtime, &[sexpr], &env, &span).await?;
    let compiled = expr.compile_top_level();
    let closure = runtime.compile_expr(compiled).await;
    let result = Application::new(
        closure,
        Vec::new(),
        ExceptionHandler::default(),
        DynamicWind::default(),
        None,
    )
    .eval()
    .await?;
    Ok(result)
}
