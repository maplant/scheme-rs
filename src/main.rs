use reedline::{Reedline, Signal, ValidationResult, Validator};
use scheme_rs::{
    ast::{AstNode, ParseAstError}, cps::Compile, env::{Environment, Repl}, gc::Gc, lex::{LexError, Token}, parse::ParseError, registry::Registry, runtime::Runtime, syntax::{Identifier, ParsedSyntax}, value::Value
};
use std::borrow::Cow;

struct InputParser;

impl Validator for InputParser {
    fn validate(&self, line: &str) -> ValidationResult {
        let Ok(tokens) = Token::tokenize_str(line) else {
            return ValidationResult::Incomplete;
        };
        let syntax = ParsedSyntax::parse(&tokens);
        match syntax {
            Err(ParseError::UnclosedParen { .. }) => ValidationResult::Incomplete,
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
    let mut rl = Reedline::create().with_validator(Box::new(InputParser));
    let mut n_results = 1;
    let top = Environment::new_repl();
    loop {
        let Ok(Signal::Success(input)) = rl.read_line(&Prompt) else {
            println!("exiting...");
            return;
        };
        match compile_and_run_str(&runtime, &top, &input).await {
            Ok(results) => {
                for result in results.into_iter() {
                    println!("${n_results} = {}", result.read().fmt());
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
    ParseError(ParseError<'e>),
    ParseAstError(ParseAstError),
}

async fn compile_and_run_str<'e>(
    runtime: &Gc<Runtime>,
    env: &Environment<Repl>,
    input: &'e str,
) -> Result<Vec<Gc<Value>>, EvalError<'e>> {
    let tokens = Token::tokenize_str(input).unwrap();
    let sexprs = ParsedSyntax::parse(&tokens)?;
    let mut output = Vec::new();
    for sexpr in sexprs {
        let Some(expr) = AstNode::from_syntax(sexpr.syntax, env).await? else {
            continue;
        };
        // println!("Parsed: {expr:#?}");
        let compiled = expr.compile_top_level();
        // println!("Compiled: {compiled:#?}");

        let closure = runtime.compile_expr(compiled).await.unwrap();
        let result = closure.apply(&[]).await.eval().await;
        output.extend(result)
    }
    Ok(output)
}
