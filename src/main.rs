use reedline::{Reedline, Signal, ValidationResult, Validator};
use scheme_rs::{eval::Env, gc::Gc, lex::Token, parse::ParseError, syntax::ParsedSyntax};
use std::{
    borrow::Cow,
    sync::{Arc, Mutex},
};

struct InputParser {
    parsed: Arc<Mutex<Option<Result<Vec<ParsedSyntax>, String>>>>,
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
    let base = Gc::new(Env::base());
    loop {
        match rl.read_line(&Prompt) {
            Ok(Signal::Success(_)) => (),
            _ => {
                println!("exiting...");
                return;
            }
        }
        // let parsed_lock = parsed.lock().unwrap();
        let Some(parsed) = parsed.lock().unwrap().take() else {
            continue;
        };
        let parsed = match parsed {
            Err(err) => {
                println!("Error parsing: {}", err);
                continue;
            }
            Ok(parsed) => parsed,
        };
        for sexpr in parsed {
            let result = sexpr
                .compile(&base)
                .await
                .unwrap()
                .eval(&base)
                .await
                .unwrap();
            println!("${n_results} = {}", result.read().await.fmt().await);
            n_results += 1;
        }
    }
}
