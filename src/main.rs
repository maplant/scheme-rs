use reedline::{Reedline, Signal, ValidationResult, Validator};
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
use std::borrow::Cow;

struct InputParser;

impl Validator for InputParser {
    fn validate(&self, line: &str) -> ValidationResult {
        let syntax = Syntax::from_str(line, None);
        match syntax {
            Err(ParseSyntaxError::UnclosedParen { .. }) => ValidationResult::Incomplete,
            _ => ValidationResult::Complete,
        }
    }
}

struct Prompt;

impl reedline::Prompt for Prompt {
    fn render_prompt_left(&self) -> Cow<str> {
        Cow::Borrowed("")
    }

    fn render_prompt_right(&self) -> Cow<str> {
        Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, _prompt_mode: reedline::PromptEditMode) -> Cow<str> {
        Cow::Borrowed("> ")
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        Cow::Borrowed("  ")
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: reedline::PromptHistorySearch,
    ) -> Cow<str> {
        Cow::Borrowed("? ")
    }
}

#[tokio::main]
async fn main() {
    let runtime = Gc::new(Runtime::new());
    let registry = Registry::new(&runtime);
    let base = registry.import("(base)").unwrap();
    let mut rl = Reedline::create().with_validator(Box::new(InputParser));
    let mut n_results = 1;
    let mut repl = Top::repl();
    {
        let base = base.read();
        repl.import(&base);
    }
    let top = Environment::from(Gc::new(repl));

    loop {
        let Ok(Signal::Success(input)) = rl.read_line(&Prompt) else {
            println!("exiting...");
            return;
        };
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
    let sexprs = Syntax::from_str(&input, None)?;
    let mut output = Vec::new();
    for sexpr in sexprs {
        let span = sexpr.span().clone();
        let expr = DefinitionBody::parse(runtime, &[sexpr], env, &span).await?;

        // println!("Parsed: {expr:#?}");
        let compiled = expr.compile_top_level();
        // println!("Compiled: {compiled:#?}");

        let closure = runtime.compile_expr(compiled).await.unwrap();
        let result = closure.apply(&[]).await?.eval().await?;
        output.extend(result)
    }
    Ok(output)
}
