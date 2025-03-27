use rustyline::{
    completion::{Completer, Pair},
    error::ReadlineError,
    highlight::Highlighter,
    hint::Hinter,
    history::DefaultHistory,
    validate::{ValidationContext, ValidationResult, Validator},
    Editor, Helper,
};
use scheme_rs::{
    ast::{DefinitionBody, ParseAstError},
    cps::Compile,
    env::{Environment, Top},
    exception::Exception,
    gc::Gc,
    lex::LexError,
    parse::ParseSyntaxError,
    registry::Registry,
    runtime::Runtime,
    syntax::Syntax,
    value::Value,
};
use std::process::ExitCode;

struct InputHelper;
impl Completer for InputHelper {
    type Candidate = Pair;
}
impl Helper for InputHelper {}
impl Hinter for InputHelper {
    type Hint = String;
}
impl Highlighter for InputHelper {}
impl Validator for InputHelper {
    fn validate(&self, ctx: &mut ValidationContext<'_>) -> rustyline::Result<ValidationResult> {
        match Syntax::from_str(ctx.input(), None) {
            Err(ParseSyntaxError::UnclosedParen { .. }) => Ok(ValidationResult::Incomplete),
            _ => Ok(ValidationResult::Valid(None)),
        }
    }
}

#[tokio::main]
async fn main() -> ExitCode {
    let runtime = Gc::new(Runtime::new());
    let registry = Registry::new(&runtime).await;
    let base = registry.import("(base)").unwrap();

    let mut editor = match Editor::<InputHelper, DefaultHistory>::new() {
        Ok(e) => e,
        Err(err) => {
            eprintln!("Error creating line editor: {}", err);
            return ExitCode::FAILURE;
        }
    };
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
        let result = closure.apply(&[], None).await?.eval().await?;
        output.extend(result)
    }
    Ok(output)
}
