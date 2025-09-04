use rustyline::{
    Completer, Config, Editor, Helper, Highlighter, Hinter, Validator,
    error::ReadlineError,
    highlight::MatchingBracketHighlighter,
    history::DefaultHistory,
    validate::{ValidationContext, ValidationResult, Validator},
};
use scheme_rs::{
    ast::{DefinitionBody, ImportSet, ParseAstError},
    cps::Compile,
    env::Environment,
    exception::Exception,
    lex::LexError,
    parse::ParseSyntaxError,
    proc::{Application, DynamicWind},
    registry::Library,
    runtime::Runtime,
    syntax::Syntax,
    value::Value,
};
use std::process::ExitCode;

#[derive(Default)]
struct InputValidator;

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext<'_>) -> rustyline::Result<ValidationResult> {
        match Syntax::from_str(ctx.input(), None) {
            Err(ParseSyntaxError::UnclosedParen { .. }) => Ok(ValidationResult::Incomplete),
            _ => Ok(ValidationResult::Valid(None)),
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

    let mut n_results = 1;
    let mut curr_line = 1;
    loop {
        let input = match editor.readline("> ") {
            Ok(line) => line,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Error while reading input: {err}");
                return ExitCode::FAILURE;
            }
        };

        match compile_and_run_str(&runtime, &repl, &input, curr_line).await {
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

        curr_line += input.chars().filter(|c| *c == '\n').count() as u32 + 1;
    }

    ExitCode::SUCCESS
}

#[derive(derive_more::From, Debug)]
pub enum EvalError<'e> {
    LexError(LexError<'e>),
    ParseError(ParseSyntaxError<'e>),
    ParseAstError(ParseAstError),
    Exception(Exception),
}

async fn compile_and_run_str<'e>(
    runtime: &Runtime,
    repl: &Library,
    input: &'e str,
    curr_line: u32,
) -> Result<Vec<Value>, EvalError<'e>> {
    let sexprs = Syntax::from_str_with_line_offset(input, None, curr_line)?;
    let mut output = Vec::new();
    let env = Environment::Top(repl.clone());
    for sexpr in sexprs {
        let span = sexpr.span().clone();
        let expr = DefinitionBody::parse(runtime, &[sexpr], &env, &span).await?;
        let compiled = expr.compile_top_level();
        let closure = runtime.compile_expr(compiled).await;
        let result = Application::new(closure, Vec::new(), None, DynamicWind::default(), None)
            .eval()
            .await?;
        output.extend(result)
    }
    Ok(output)
}
