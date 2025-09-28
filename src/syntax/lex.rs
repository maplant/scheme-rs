//! Lexical analysis of symbolic expressions

use std::borrow::Cow;

use super::Span;
use futures::future::LocalBoxFuture;
use unicode_categories::UnicodeCategories;

use crate::ports::InputPort;

pub struct Lexer {
    pos: usize,
    port: InputPort,
    curr_line: usize,
    curr_column: usize,
}

impl Lexer {
    async fn peek(&mut self) -> char {
        /*
        if self.pos >= self.buff.len() {
            self.buff.push(self.port.read_char().await);
        }
        self.buff[self.pos]
         */
        self.port.peekn(self.pos).await
    }

    async fn peekn(&mut self, idx: usize) -> char {
        self.port.peekn(self.pos + idx).await
    }

    fn skip(&mut self) {
        // if self.pos >= self.buff.len() {
        //     panic!("skipping when we didn't peek!");
        // }
        self.pos += 1;
    }

    async fn take(&mut self) -> char {
        let chr = self.peek().await;
        self.pos += 1;
        chr
    }

    /*
    async fn take(&mut self) -> char {
        let chr = self.peek().await;
        if chr == '\n' {
            self.curr_line += 1;
            self.curr_column = 0;
        } else {
            self.curr_column += 1;
        }
        self.pos += 1;
        chr
    }
    */

    async fn match_char(&mut self, chr: char) -> bool {
        self.match_pred(|peek| peek == chr).await.is_some()
    }

    async fn match_pred(&mut self, pred: impl FnOnce(char) -> bool) -> Option<char> {
        let chr = self.peek().await;
        if pred(chr) {
            if chr == '\n' {
                self.curr_line += 1;
                self.curr_column = 0;
            } else {
                self.curr_column += 1;
            }
            self.pos += 1;
            Some(chr)
        } else {
            None
        }
    }

    async fn match_tag(&mut self, tag: &str) -> bool {
        let mut offset = 0;
        for chr in tag.chars() {
            if self.port.peekn(offset + self.pos).await != chr {
                return false;
            }
            /*
            if offset + self.pos >= self.buff.len() {
                self.buff.push(self.port.read_char().await);
            }
            if self.buff[offset + self.pos] != chr {
                return false;
            }
            */
            offset += 1;
        }
        // tag cannot contain newlines
        self.curr_column += offset;
        self.pos += offset;
        true
    }

    async fn next_token(&mut self) -> Result<Token, LexerError> {
        // TODO: Check if the port is empty

        // Check for any interlexeme space:
        self.interlexeme_space().await;

        // Get the current span:
        let span = self.curr_span();

        // Check for various special characters:
        let lexeme = match self.peek().await {
            '.' => {
                self.skip();
                Lexeme::Period
            }
            '\'' => {
                self.skip();
                Lexeme::Quote
            }
            '`' => {
                self.skip();
                Lexeme::Backquote
            }
            ',' if self.match_tag(",@").await => Lexeme::CommaAt,
            ',' => {
                self.skip();
                Lexeme::Comma
            }
            '(' => {
                self.skip();
                Lexeme::LParen
            }
            ')' => {
                self.skip();
                Lexeme::RParen
            }
            '[' => {
                self.skip();
                Lexeme::LBracket
            }
            ']' => {
                self.skip();
                Lexeme::RBracket
            }
            '"' => {
                self.skip();
                Lexeme::String(self.string().await?)
            }
            '#' if self.match_tag("#(").await => Lexeme::HashParen,
            '#' if self.match_tag("#u8(").await => Lexeme::Vu8Paren,
            '#' if self.match_tag("#'").await => Lexeme::HashTick,
            '#' if self.match_tag("#`").await => Lexeme::HashBackquote,
            '#' if self.match_tag("#,@").await => Lexeme::HashCommaAt,
            '#' if self.match_tag("#,").await => Lexeme::HashComma,
            _ => todo!(),
        };

        Ok(Token { lexeme, span })
    }

    async fn interlexeme_space(&mut self) {
        loop {
            if self.match_char(';').await {
                self.comment().await
            } else if self.match_tag("#|").await {
                self.nested_comment().await;
            } else if self.match_pred(is_whitespace).await.is_some() {
                break;
            }
        }
    }

    async fn comment(&mut self) {
        while let Some(_) = self.match_pred(|chr| chr != '\n').await {}
    }

    fn nested_comment(&mut self) -> LocalBoxFuture<'_, ()> {
        Box::pin(async move {
            while !self.match_tag("|#").await {
                if self.match_tag("#|").await {
                    self.nested_comment().await;
                } else {
                    self.skip();
                }
            }
        })
    }

    async fn number(&mut self) -> Option<Number> {
        let saved_pos = self.pos;

        let (radix, exactness) = self.radix_and_exactness().await;

        let radix = radix.unwrap_or(10);

        // Need this because "10i" is not a valid number.
        let has_sign = {
            let peeked = self.peek().await;
            peeked == '+' || peeked == '-'
        };

        let first_part = self.part(radix).await;

        if first_part.is_none() {
            self.pos = saved_pos;
            return None;
        }

        let number = if self.match_char('i').await {
            if !has_sign {
                self.pos = saved_pos;
                return None;
            }
            Number {
                radix,
                exactness,
                real_part: None,
                imag_part: first_part,
            }
        } else {
            let matched_at = !self.match_char('@').await;
            let imag_part = if matched_at || {
                let peeked = self.peek().await;
                peeked == '+' || peeked == '-'
            } {
                let second_part = self.part(radix).await;
                if second_part.is_none() || !matched_at && !self.match_char('i').await {
                    self.pos = saved_pos;
                    return None;
                }
                second_part
            } else {
                None
            };
            Number {
                radix,
                exactness,
                real_part: first_part,
                imag_part,
            }
        };

        if is_subsequent(self.peek().await) {
            self.pos = saved_pos;
            return None;
        }

        Some(number)
    }

    async fn part(&mut self, radix: u32) -> Option<Part> {
        let neg = !self.match_char('+').await && self.match_char('-').await;
        let mut mantissa_width = None;

        // Check for special nan/inf
        let real = if self.match_tag("nan.0").await {
            Real::Nan
        } else if self.match_tag("inf.0").await {
            Real::Inf
        } else {
            let mut num = String::new();
            while let Some(ch) = self.match_pred(|chr| chr.is_digit(radix)).await {
                num.push(ch);
            }

            if !num.is_empty() && self.match_char('/').await {
                // Rational number
                let mut denom = String::new();
                while let Some(ch) = self.match_pred(|chr| chr.is_digit(radix)).await {
                    denom.push(ch);
                }
                if denom.is_empty() {
                    return None;
                }
                Real::Rational(num, denom)
            } else if radix == 10 {
                if self.match_char('.').await {
                    num.push('.');
                    while let Some(ch) = self.match_pred(|chr| chr.is_digit(radix)).await {
                        num.push(ch);
                    }
                }
                if num.is_empty() {
                    return None;
                }
                let suffix = self.suffix().await;
                if self.match_char('|').await {
                    let mut width = 0;
                    while let Some(chr) = self.match_pred(|chr| chr.is_ascii_digit()).await {
                        width = width * 10 + chr.to_digit(10).unwrap() as usize;
                    }
                    mantissa_width = Some(width);
                }
                Real::Decimal(num, suffix)
            } else if num.is_empty() {
                return None;
            } else {
                Real::Num(num)
            }
        };

        Some(Part {
            neg,
            real,
            mantissa_width,
        })
    }

    async fn exactness(&mut self) -> Option<Exactness> {
        if self.match_tag("#i").await || self.match_tag("#I").await {
            Some(Exactness::Inexact)
        } else if self.match_tag("#e").await || self.match_tag("#E").await {
            Some(Exactness::Exact)
        } else {
            None
        }
    }

    async fn radix(&mut self) -> Option<u32> {
        if self.match_tag("#b").await || self.match_tag("#B").await {
            Some(2)
        } else if self.match_tag("#o").await || self.match_tag("#O").await {
            Some(8)
        } else if self.match_tag("#x").await || self.match_tag("#X").await {
            Some(16)
        } else if self.match_tag("#d").await || self.match_tag("#D").await {
            Some(10)
        } else {
            None
        }
    }

    async fn radix_and_exactness(&mut self) -> (Option<u32>, Option<Exactness>) {
        let exactness = self.exactness().await;
        let radix = self.radix().await;
        if exactness.is_some() {
            (radix, exactness)
        } else {
            (radix, self.exactness().await)
        }
    }

    async fn suffix(&mut self) -> Option<isize> {
        let pos = self.pos;
        if let Some(_) = self
            .match_pred(|chr| matches!(chr.to_ascii_lowercase(), 'e' | 's' | 'f' | 'd' | 'l'))
            .await
        {
            let neg = !self.match_char('+').await && self.match_char('-').await;
            let mut suffix = String::new();
            while let Some(chr) = self.match_pred(|chr| chr.is_digit(10)).await {
                suffix.push(chr);
            }
            if !suffix.is_empty() {
                let val: isize = suffix.parse().unwrap();
                if neg {
                    return Some(-val);
                } else {
                    return Some(val);
                }
            }
        }
        self.pos = pos;
        None
    }

    async fn string(&mut self) -> Result<String, LexerError> {
        let mut output = String::new();
        while let Some(chr) = self.match_pred(|chr| chr != '"').await {
            if chr == '\\' {
                let escaped = match self.take().await {
                    'x' => todo!(),
                    'a' => '\u{07}',
                    'b' => '\u{08}',
                    't' => '\t',
                    'n' => '\n',
                    'v' => '\u{0B}',
                    'f' => '\u{0C}',
                    '"' => '"',
                    '\\' => '\\',
                    x => todo!("error, bad escape char"),
                };
                output.push(escaped);
            } else {
                output.push(chr);
            }
        }
        // Skip the terminating quote
        self.skip();
        Ok(output)
    }

    async fn identifier(&mut self) -> Result<Option<String>, LexerError> {
        let mut ident = if self.match_tag("\\x").await {
            self.inline_hex_escape().await?
        } else if self.match_tag("...").await {
            String::from("...")
        } else if self.match_tag("->").await {
            String::from("->")
        } else if let Some(initial) = self
            .match_pred(|chr| is_initial(chr) || is_peculiar_initial(chr))
            .await
        {
            String::from(initial)
        } else {
            return Ok(None);
        };

        loop {
            if self.match_tag("\\x").await {
                ident.push_str(&self.inline_hex_escape().await?);
            } else if let Some(next) = self.match_pred(is_subsequent).await {
                ident.push(next);
            } else {
                break;
            }
        }

        Ok(Some(ident))
    }

    fn curr_span(&self) -> Span {
        todo!()
    }

    async fn inline_hex_escape(&mut self) -> Result<String, LexerError> {
        let mut escaped = String::new();
        let mut buff = String::with_capacity(2);
        while let Some(chr) = self.match_pred(|chr| chr != ';').await {
            if !chr.is_ascii_hexdigit() {
                return Err(LexerError::InvalidCharacterInHexEscape {
                    chr,
                    span: self.curr_span(),
                });
            }
            buff.push(chr);
            if buff.len() == 2 {
                escaped.push(u8::from_str_radix(&buff, 16).unwrap() as char);
                buff.clear();
            }
        }
        if buff.len() > 0 {
            escaped.push(u8::from_str_radix(&buff, 16).unwrap() as char);
        }
        Ok(escaped)
        /*
        loop {
            let chr = self.take();
            if chr == ';' {
                break;
            }
        */
    }
}

enum LexerError {
    InvalidCharacterInHexEscape { chr: char, span: Span },
    InvalidCharacter { chr: char, span: Span },
    BadEscapeCharacter { chr: char, span: Span },
    UnexpectedEof { span: Span },
}

fn is_whitespace(chr: char) -> bool {
    chr.is_separator() || matches!(chr, '\t' | '\n' | '\r')
}

fn is_initial(chr: char) -> bool {
    is_constituent(chr) || is_special_initial(chr)
}

fn is_constituent(c: char) -> bool {
    c.is_ascii_alphabetic()
        || (c as u32 > 127
            && (c.is_letter()
                || c.is_mark_nonspacing()
                || c.is_number_letter()
                || c.is_number_other()
                || c.is_punctuation_dash()
                || c.is_punctuation_connector()
                || c.is_punctuation_other()
                || c.is_symbol()
                || c.is_other_private_use()))
}

fn is_special_initial(chr: char) -> bool {
    matches!(
        chr,
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~'
    )
}

fn is_peculiar_initial(chr: char) -> bool {
    matches!(chr, '+' | '-')
}

fn is_special_subsequent(chr: char) -> bool {
    matches!(chr, '+' | '-' | '.' | '@')
}

fn is_subsequent(chr: char) -> bool {
    is_initial(chr)
        || chr.is_ascii_digit()
        || chr.is_number_decimal_digit()
        || chr.is_mark_spacing_combining()
        || chr.is_mark_enclosing()
        || is_special_subsequent(chr)
}

// #[derive(Clone, Debug)]
pub struct Token {
    pub lexeme: Lexeme,
    pub span: super::Span,
}

pub enum Lexeme {
    Identifier(String),
    Boolean(bool),
    Number(Number),
    Character(Character),
    String(String),
    LParen,
    RParen,
    LBracket,
    RBracket,
    HashParen,
    Vu8Paren,
    Quote,
    Backquote,
    Comma,
    CommaAt,
    Period,
    HashQuote,
    HashTick,
    HashBackquote,
    HashComma,
    HashCommaAt,
}

// #[derive(Clone, Debug, PartialEq, Eq)]
#[derive(Clone, Debug)]
pub struct Number {
    radix: u32,
    exactness: Option<Exactness>,
    real_part: Option<Part>,
    imag_part: Option<Part>,
    /*
    negative: bool,
    integer_or_numerator: String,
    fractional_or_denominator: Option<(FractionalOrDenominator, String)>,
    */
}

#[derive(Clone, Debug)]
struct Part {
    neg: bool,
    real: Real,
    mantissa_width: Option<usize>,
}

/*
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FractionalOrDenominator {
    Fractional,
    Denominator,
}
*/

#[derive(Clone, Debug)]
enum Exactness {
    Exact,
    Inexact,
}

/*
impl Number {
    pub const fn new(radix: u32, negative: bool, integer_or_numerator: String) -> Self {
        Self {
            radix,
            negative,
            integer_or_numerator,
            fractional_or_denominator: None,
        }
    }
}
*/

#[derive(Clone, Debug)]
enum Real {
    Nan,
    Inf,
    Num(String),
    Rational(String, String),
    Decimal(String, Option<isize>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Character {
    /// `#\a` characters
    Literal(char),
    /// `#\foo` characters
    Escaped(EscapedCharacter),
    /// `#\xcafe` characters
    Unicode(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EscapedCharacter {
    Alarm,
    Backspace,
    Delete,
    Escape,
    Newline,
    Null,
    Return,
    Space,
    Tab,
}

impl From<EscapedCharacter> for char {
    fn from(c: EscapedCharacter) -> char {
        // from r7rs 6.6
        match c {
            EscapedCharacter::Alarm => '\u{0007}',
            EscapedCharacter::Backspace => '\u{0008}',
            EscapedCharacter::Delete => '\u{007F}',
            EscapedCharacter::Escape => '\u{001B}',
            EscapedCharacter::Newline => '\u{000A}',
            EscapedCharacter::Null => '\u{0000}',
            EscapedCharacter::Return => '\u{000D}',
            EscapedCharacter::Space => ' ',
            EscapedCharacter::Tab => '\u{0009}',
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Fragment {
    HexValue(String),
    Escaped(char),
    Unescaped(String),
}
