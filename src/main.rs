use reedline::{Reedline, Signal, ValidationResult, Validator};
use scheme_rs::{env::Env, lex::Token, parse::ParseError, syntax::ParsedSyntax};
use std::{
    borrow::Cow,
    sync::{Arc, Mutex},
};

type ParsedResult = Option<Result<Vec<ParsedSyntax>, String>>;

struct InputParser {
    parsed: Arc<Mutex<ParsedResult>>,
}

impl Validator for InputParser {
    fn validate(&self, line: &str) -> ValidationResult {
        let Ok(tokens) = Token::tokenize_str(line) else {
            return ValidationResult::Incomplete;
        };
        let syntax = ParsedSyntax::parse(&tokens);
        match syntax {
            Err(ParseError::UnclosedParen { .. }) => ValidationResult::Incomplete,
            x => {
                *self.parsed.lock().unwrap() = Some(x.map_err(|e| format!("{:?}", e)));
                ValidationResult::Complete
            }
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
        Cow::Borrowed(">>> ")
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        Cow::Borrowed("... ")
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
    let parsed = Arc::new(Mutex::new(None));
    let mut rl = Reedline::create().with_validator(Box::new(InputParser {
        parsed: parsed.clone(),
    }));
    let mut n_results = 1;
    let top = Env::top().await;
    loop {
        match rl.read_line(&Prompt) {
            Ok(Signal::Success(_)) => (),
            _ => {
                println!("exiting...");
                return;
            }
        }
        let Some(parsed) = parsed.lock().unwrap().take() else {
            continue;
        };
        let parsed = match parsed {
            Err(err) => {
                println!("Error parsing: {err}");
                continue;
            }
            Ok(parsed) => parsed,
        };
        for sexpr in parsed {
            let compiled = match sexpr.compile(&top, &None).await {
                Err(err) => {
                    println!("Error compiling: {err:?}");
                    continue;
                }
                Ok(compiled) => compiled,
            };
            match compiled.eval(&top, &None).await {
                Err(err) => {
                    println!("Error: {err:?}");
                }
                Ok(result) => {
                    println!("${n_results} = {}", result.read().await.fmt().await);
                    n_results += 1;
                }
            }
        }
    }
}
