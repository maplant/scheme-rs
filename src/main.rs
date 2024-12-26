use reedline::{Reedline, Signal, ValidationResult, Validator};
use scheme_rs::{env::Env, lex::Token, parse::ParseError, syntax::ParsedSyntax};
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
    let mut rl = Reedline::create().with_validator(Box::new(InputParser));
    let mut n_results = 1;
    let top = Env::top().await;
    println!("installed");
    loop {
        let Ok(Signal::Success(input)) = rl.read_line(&Prompt) else {
            println!("exiting...");
            return;
        };
        match top.eval(&input).await {
            Ok(results) => {
                for result in results.into_iter().flatten() {
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
