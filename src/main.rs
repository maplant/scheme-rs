use rustyline::{
    error::ReadlineError,
    highlight::MatchingBracketHighlighter,
    history::DefaultHistory,
    validate::{ValidationContext, ValidationResult, Validator},
    Completer, Config, Editor, Helper, Highlighter, Hinter, Validator,
};
use scheme_rs::{
    ast::{DefinitionBody, ParseAstError},
    cps::Compile,
    env::{Environment, Top},
    exception::Exception,
    gc::Gc,
    lex::LexError,
    parse::ParseSyntaxError,
    proc::Application,
    registry::Registry,
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
    let runtime = Gc::new(Runtime::new());
    let registry = Registry::new(&runtime).await;
    let base = registry.import("(base)").unwrap();

    let config = Config::builder().auto_add_history(true).build();
    let mut editor = match Editor::with_history(config, DefaultHistory::new()) {
        Ok(e) => e,
        Err(err) => {
            eprintln!("Error creating line editor: {}", err);
            return ExitCode::FAILURE;
        }
    };

    let helper = InputHelper {
        validator: InputValidator,
        highlighter: MatchingBracketHighlighter::new(),
    };

    editor.set_helper(Some(helper));

    let mut n_results = 1;
    let mut repl = Top::repl();
    {
        let base = base.read();
        repl.import(&base);
    }
    let top = Environment::from(Gc::new(repl));

    loop {
        let input = match editor.readline("> ") {
            Ok(line) => line,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Error while reading input: {}", err);
                return ExitCode::FAILURE;
            }
        };

        //input.push('\n');
        match compile_and_run_str(&runtime, &top, &input).await {
            Ok(results) => {
                for result in results.into_iter() {
                    println!("${n_results} = {:?}", result);
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
pub enum EvalError<'e> {
    LexError(LexError<'e>),
    ParseError(ParseSyntaxError<'e>),
    ParseAstError(ParseAstError),
    Exception(Exception),
}

async fn compile_and_run_str<'e>(
    runtime: &Gc<Runtime>,
    env: &Environment,
    input: &'e str,
) -> Result<Vec<Gc<Value>>, EvalError<'e>> {
    let sexprs = Syntax::from_str(input, None)?;
    let mut output = Vec::new();
    for sexpr in sexprs {
        let span = sexpr.span().clone();
        let expr = DefinitionBody::parse(runtime, &[sexpr], env, &span).await?;

        // println!("Parsed: {expr:#?}");
        let compiled = expr.compile_top_level();
        // println!("Compiled: {compiled:#?}");

        let closure = runtime.compile_expr(compiled).await.unwrap();
        let result = Application::new(closure, Vec::new(), None, None)
            .eval()
            .await?;
        output.extend(result)
    }
    Ok(output)
}
