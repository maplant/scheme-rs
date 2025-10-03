//! Lexical analysis of symbolic expressions

use super::Span;
use futures::future::BoxFuture;
use malachite::{Integer, base::num::conversion::traits::*, rational::Rational};
use std::sync::Arc;
use tokio::sync::MappedMutexGuard;
use unicode_categories::UnicodeCategories;

use crate::{
    num,
    ports::{InputPort, Port, ReadError},
};

pub struct Lexer<'a> {
    pos: usize,
    port: MappedMutexGuard<'a, InputPort>,
    curr_line: u32,
    curr_column: usize,
    file: Arc<String>,
}

impl<'a> Lexer<'a> {
    pub async fn new(input_port: &'a Port) -> Self {
        Self {
            pos: 0,
            file: Arc::new(input_port.name.clone()),
            curr_line: 0,
            curr_column: 0,
            port: input_port.try_lock_input_port().await.unwrap(),
        }
    }

    async fn peek(&mut self) -> Result<char, ReadError> {
        self.port.peekn(self.pos).await
    }

    fn skip(&mut self) {
        self.pos += 1;
    }

    async fn take(&mut self) -> Result<char, ReadError> {
        let chr = self.peek().await?;
        self.pos += 1;
        Ok(chr)
    }

    async fn match_char(&mut self, chr: char) -> Result<bool, ReadError> {
        Ok(self.match_pred(|peek| peek == chr).await?.is_some())
    }

    async fn match_pred(
        &mut self,
        pred: impl FnOnce(char) -> bool,
    ) -> Result<Option<char>, ReadError> {
        let chr = self.peek().await?;
        if pred(chr) {
            if chr == '\n' {
                self.curr_line += 1;
                self.curr_column = 0;
            } else {
                self.curr_column += 1;
            }
            self.pos += 1;
            Ok(Some(chr))
        } else {
            Ok(None)
        }
    }

    async fn match_tag(&mut self, tag: &str) -> Result<bool, ReadError> {
        let mut offset = 0;
        for chr in tag.chars() {
            if self.port.peekn(offset + self.pos).await? != chr {
                return Ok(false);
            }
            offset += 1;
        }
        // tag cannot contain newlines
        self.curr_column += offset;
        self.pos += offset;
        Ok(true)
    }

    pub async fn next_token(&mut self) -> Result<Token, LexerError> {
        // TODO: Check if the port is empty

        // Check for any interlexeme space:
        self.interlexeme_space().await?;

        // Get the current span:
        let span = self.curr_span();

        // Check for various special characters:
        let lexeme = if let Some(number) = self.number().await? {
            Lexeme::Number(number)
        } else if let Some(identifier) = self.identifier().await? {
            Lexeme::Identifier(identifier)
        } else {
            match self.peek().await? {
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
                ',' if self.match_tag(",@").await? => Lexeme::CommaAt,
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
                '#' if self.match_tag("#;").await? => Lexeme::DatumComment,
                '#' if self.match_tag("#\\").await? => Lexeme::Character(self.character().await?),
                '#' if self.match_tag("#F").await? || self.match_tag("#f").await? => {
                    Lexeme::Boolean(false)
                }
                '#' if self.match_tag("#T").await? || self.match_tag("#t").await? => {
                    Lexeme::Boolean(true)
                }
                '#' if self.match_tag("#(").await? => Lexeme::HashParen,
                '#' if self.match_tag("#u8(").await? => Lexeme::Vu8Paren,
                '#' if self.match_tag("#'").await? => Lexeme::HashQuote,
                '#' if self.match_tag("#`").await? => Lexeme::HashBackquote,
                '#' if self.match_tag("#,@").await? => Lexeme::HashCommaAt,
                '#' if self.match_tag("#,").await? => Lexeme::HashComma,
                chr => return Err(LexerError::UnexpectedCharacter { chr, span }),
            }
        };

        Ok(Token { lexeme, span })
    }

    async fn interlexeme_space(&mut self) -> Result<(), ReadError> {
        loop {
            if self.match_char(';').await? {
                self.comment().await?;
            } else if self.match_tag("#|").await? {
                self.nested_comment().await?;
            } else if self.match_pred(is_whitespace).await?.is_none() {
                break;
            }
        }
        Ok(())
    }

    async fn comment(&mut self) -> Result<(), ReadError> {
        while self.match_pred(|chr| chr != '\n').await?.is_some() {}
        Ok(())
    }

    fn nested_comment(&mut self) -> BoxFuture<'_, Result<(), ReadError>> {
        Box::pin(async move {
            while !self.match_tag("|#").await? {
                if self.match_tag("#|").await? {
                    self.nested_comment().await?;
                } else {
                    self.skip();
                }
            }
            Ok(())
        })
    }

    async fn character(&mut self) -> Result<Character, LexerError> {
        let chr = if self.match_tag("alarm").await? {
            Character::Escaped(EscapedCharacter::Alarm)
        } else if self.match_tag("backspace").await? {
            Character::Escaped(EscapedCharacter::Backspace)
        } else if self.match_tag("delete").await? {
            Character::Escaped(EscapedCharacter::Delete)
        } else if self.match_tag("escape").await? {
            Character::Escaped(EscapedCharacter::Escape)
        } else if self.match_tag("newline").await? {
            Character::Escaped(EscapedCharacter::Newline)
        } else if self.match_tag("null").await? {
            Character::Escaped(EscapedCharacter::Null)
        } else if self.match_tag("return").await? {
            Character::Escaped(EscapedCharacter::Return)
        } else if self.match_tag("space").await? {
            Character::Escaped(EscapedCharacter::Space)
        } else if self.match_tag("tab").await? {
            Character::Escaped(EscapedCharacter::Tab)
        } else if self.match_char('x').await? {
            if is_delimiter(self.peek().await?) {
                Character::Literal('x')
            } else {
                let mut unicode = String::new();
                while let Some(chr) = self.match_pred(|c| c.is_ascii_hexdigit()).await? {
                    unicode.push(chr);
                }
                Character::Unicode(unicode)
            }
        } else {
            Character::Literal(self.take().await?)
        };
        let peeked = self.peek().await?;
        if !is_delimiter(peeked) {
            let span = self.curr_span();
            Err(LexerError::UnexpectedCharacter { chr: peeked, span })
        } else {
            Ok(chr)
        }
    }

    async fn number(&mut self) -> Result<Option<Number>, ReadError> {
        let saved_pos = self.pos;

        let (radix, exactness) = self.radix_and_exactness().await?;

        let radix = radix.unwrap_or(10);

        // Need this because "10i" is not a valid number.
        let has_sign = {
            let peeked = self.peek().await?;
            peeked == '+' || peeked == '-'
        };

        let first_part = self.part(radix).await?;

        if first_part.is_none() {
            self.pos = saved_pos;
            return Ok(None);
        }

        let number = if self.match_char('i').await? {
            if !has_sign {
                self.pos = saved_pos;
                return Ok(None);
            }
            Number {
                radix,
                exactness,
                real_part: None,
                imag_part: first_part,
            }
        } else {
            let matched_at = self.match_char('@').await?;
            let imag_part = if matched_at || {
                let peeked = self.peek().await?;
                peeked == '+' || peeked == '-'
            } {
                let second_part = self.part(radix).await?;
                if second_part.is_none() || !matched_at && !self.match_char('i').await? {
                    self.pos = saved_pos;
                    return Ok(None);
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

        match self.peek().await {
            Err(ReadError::Eof) => {
                self.pos = saved_pos;
                return Ok(None);
            }
            Ok(chr) if is_subsequent(chr) => {
                self.pos = saved_pos;
                return Ok(None);
            }
            Err(err) => return Err(err),
            Ok(_) => (),
        }

        Ok(Some(number))
    }

    async fn part(&mut self, radix: u32) -> Result<Option<Part>, ReadError> {
        let neg = !self.match_char('+').await? && self.match_char('-').await?;
        let mut mantissa_width = None;

        // Check for special nan/inf
        let real = if self.match_tag("nan.0").await? {
            Real::Nan
        } else if self.match_tag("inf.0").await? {
            Real::Inf
        } else {
            let mut num = String::new();
            while let Some(ch) = self.match_pred(|chr| chr.is_digit(radix)).await? {
                num.push(ch);
            }
            if !num.is_empty() && self.match_char('/').await? {
                // Rational number
                let mut denom = String::new();
                while let Some(ch) = self.match_pred(|chr| chr.is_digit(radix)).await? {
                    denom.push(ch);
                }
                if denom.is_empty() {
                    return Ok(None);
                }
                Real::Rational(num, denom)
            } else if radix == 10 {
                let mut fractional = String::new();
                if self.match_char('.').await? {
                    while let Some(ch) = self.match_pred(|chr| chr.is_digit(radix)).await? {
                        fractional.push(ch);
                    }
                }
                if num.is_empty() && fractional.is_empty() {
                    return Ok(None);
                }
                let suffix = self.suffix().await?;
                if self.match_char('|').await? {
                    let mut width = 0;
                    while let Some(chr) = self.match_pred(|chr| chr.is_ascii_digit()).await? {
                        width = width * 10 + chr.to_digit(10).unwrap() as usize;
                    }
                    mantissa_width = Some(width);
                }
                Real::Decimal(num, fractional, suffix)
            } else if num.is_empty() {
                return Ok(None);
            } else {
                Real::Num(num)
            }
        };

        Ok(Some(Part {
            neg,
            real,
            mantissa_width,
        }))
    }

    async fn exactness(&mut self) -> Result<Option<Exactness>, ReadError> {
        Ok(
            if self.match_tag("#i").await? || self.match_tag("#I").await? {
                Some(Exactness::Inexact)
            } else if self.match_tag("#e").await? || self.match_tag("#E").await? {
                Some(Exactness::Exact)
            } else {
                None
            },
        )
    }

    async fn radix(&mut self) -> Result<Option<u32>, ReadError> {
        Ok(
            if self.match_tag("#b").await? || self.match_tag("#B").await? {
                Some(2)
            } else if self.match_tag("#o").await? || self.match_tag("#O").await? {
                Some(8)
            } else if self.match_tag("#x").await? || self.match_tag("#X").await? {
                Some(16)
            } else if self.match_tag("#d").await? || self.match_tag("#D").await? {
                Some(10)
            } else {
                None
            },
        )
    }

    async fn radix_and_exactness(&mut self) -> Result<(Option<u32>, Option<Exactness>), ReadError> {
        let exactness = self.exactness().await?;
        let radix = self.radix().await?;
        if exactness.is_some() {
            Ok((radix, exactness))
        } else {
            Ok((radix, self.exactness().await?))
        }
    }

    async fn suffix(&mut self) -> Result<Option<isize>, ReadError> {
        let pos = self.pos;
        if self
            .match_pred(|chr| matches!(chr.to_ascii_lowercase(), 'e' | 's' | 'f' | 'd' | 'l'))
            .await?
            .is_some()
        {
            let neg = !self.match_char('+').await? && self.match_char('-').await?;
            let mut suffix = String::new();
            while let Some(chr) = self.match_pred(|chr| chr.is_ascii_digit()).await? {
                suffix.push(chr);
            }
            if !suffix.is_empty() {
                let val: isize = suffix.parse().unwrap();
                if neg {
                    return Ok(Some(-val));
                } else {
                    return Ok(Some(val));
                }
            }
        }
        self.pos = pos;
        Ok(None)
    }

    async fn string(&mut self) -> Result<String, LexerError> {
        let mut output = String::new();
        while let Some(chr) = self.match_pred(|chr| chr != '"').await? {
            if chr == '\\' {
                let escaped = match self.take().await? {
                    'x' => todo!(),
                    'a' => '\u{07}',
                    'b' => '\u{08}',
                    't' => '\t',
                    'n' => '\n',
                    'v' => '\u{0B}',
                    'f' => '\u{0C}',
                    '"' => '"',
                    '\\' => '\\',
                    '\n' => {
                        while self.match_pred(is_intraline_whitespace).await?.is_some() {}
                        continue;
                    }
                    chr if is_intraline_whitespace(chr) => {
                        while self
                            .match_pred(|chr| chr != '\n' && is_intraline_whitespace(chr))
                            .await?
                            .is_some()
                        {}
                        let chr = self.take().await?;
                        if chr != '\n' {
                            let span = self.curr_span();
                            return Err(LexerError::UnexpectedCharacter { chr, span });
                        }
                        while self.match_pred(is_intraline_whitespace).await?.is_some() {}
                        continue;
                    }
                    chr => {
                        let span = self.curr_span();
                        return Err(LexerError::BadEscapeCharacter { chr, span });
                    }
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
        let mut ident = if self.match_tag("\\x").await? {
            self.inline_hex_escape().await?
        } else if self.match_tag("...").await? {
            String::from("...")
        } else if self.match_tag("->").await? {
            String::from("->")
        } else if let Some(initial) = self
            .match_pred(|chr| is_initial(chr) || is_peculiar_initial(chr))
            .await?
        {
            String::from(initial)
        } else {
            return Ok(None);
        };

        loop {
            if self.match_tag("\\x").await? {
                ident.push_str(&self.inline_hex_escape().await?);
            } else if let Some(next) = self.match_pred(is_subsequent).await? {
                ident.push(next);
            } else {
                break;
            }
        }

        Ok(Some(ident))
    }

    fn curr_span(&self) -> Span {
        Span {
            line: self.curr_line,
            column: self.curr_column,
            offset: self.pos,
            file: self.file.clone(),
        }
    }

    async fn inline_hex_escape(&mut self) -> Result<String, LexerError> {
        let mut escaped = String::new();
        let mut buff = String::with_capacity(2);
        while let Some(chr) = self.match_pred(|chr| chr != ';').await? {
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
        if !buff.is_empty() {
            escaped.push(u8::from_str_radix(&buff, 16).unwrap() as char);
        }
        Ok(escaped)
    }
}

#[derive(Debug)]
pub enum LexerError {
    InvalidCharacterInHexEscape { chr: char, span: Span },
    UnexpectedCharacter { chr: char, span: Span },
    BadEscapeCharacter { chr: char, span: Span },
    ReadError(ReadError),
}

impl From<ReadError> for LexerError {
    fn from(value: ReadError) -> Self {
        Self::ReadError(value)
    }
}

fn is_delimiter(chr: char) -> bool {
    is_whitespace(chr) || matches!(chr, '(' | ')' | '[' | ']' | '"' | ';' | '#')
}

fn is_whitespace(chr: char) -> bool {
    chr.is_separator() || matches!(chr, '\t' | '\n' | '\r')
}

fn is_intraline_whitespace(chr: char) -> bool {
    chr == '\t' || chr.is_separator()
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

#[derive(Clone, Debug)]
pub struct Token {
    pub lexeme: Lexeme,
    pub span: super::Span,
}

#[derive(Clone, Debug, PartialEq)]
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
    HashBackquote,
    HashComma,
    HashCommaAt,
    DatumComment,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Number {
    radix: u32,
    exactness: Option<Exactness>,
    real_part: Option<Part>,
    imag_part: Option<Part>,
}

impl TryFrom<Number> for num::Number {
    type Error = ParseNumberError;

    fn try_from(value: Number) -> Result<Self, Self::Error> {
        // Ignore exactness for now
        if let Some(imag_part) = value.imag_part {
            // This is a complex number:
            let imag_part = imag_part
                .try_into_f64(value.radix)
                .ok_or(ParseNumberError::NoValidRepresentation)?;
            let real_part = if let Some(real_part) = value.real_part {
                real_part
                    .try_into_f64(value.radix)
                    .ok_or(ParseNumberError::NoValidRepresentation)?
            } else {
                0.0
            };
            return Ok(num::Number::Complex(::num::Complex::new(
                real_part, imag_part,
            )));
        }

        let part = value
            .real_part
            .as_ref()
            .ok_or(ParseNumberError::NoValidRepresentation)?;

        part.try_into_i64(value.radix)
            .map(num::Number::FixedInteger)
            .or_else(|| {
                part.try_into_integer(value.radix)
                    .map(num::Number::BigInteger)
            })
            .or_else(|| {
                part.try_into_rational(value.radix)
                    .map(num::Number::Rational)
            })
            .or_else(|| part.try_into_f64(value.radix).map(num::Number::Real))
            .ok_or(ParseNumberError::NoValidRepresentation)
    }
}

#[derive(Debug)]
pub enum ParseNumberError {
    NoValidRepresentation,
}

#[derive(Clone, Debug, PartialEq)]
struct Part {
    neg: bool,
    real: Real,
    mantissa_width: Option<usize>,
}

impl Part {
    fn try_into_i64(&self, radix: u32) -> Option<i64> {
        let num = match &self.real {
            Real::Num(num) => i64::from_str_radix(num, radix).ok()?,
            Real::Decimal(base, fract, None) if fract.is_empty() => base.parse().ok()?,
            Real::Decimal(base, fract, Some(exp)) if fract.is_empty() => {
                let base: i64 = base.parse().ok()?;
                let exp = 10_i64.checked_pow((*exp).try_into().ok()?)?;
                base.checked_mul(exp)?
            }
            _ => return None,
        };
        Some(if self.neg { -num } else { num })
    }

    fn try_into_integer(&self, radix: u32) -> Option<Integer> {
        let num = match &self.real {
            Real::Num(num) => Integer::from_string_base(radix as u8, num)?,
            Real::Decimal(base, fract, None) if fract.is_empty() => {
                Integer::from_string_base(10, base)?
            }
            Real::Decimal(base, fract, Some(exp)) if fract.is_empty() => {
                Integer::from_sci_string(&format!("{base}e{exp}"))?
            }
            _ => return None,
        };
        Some(if self.neg { -num } else { num })
    }

    fn try_into_rational(&self, radix: u32) -> Option<Rational> {
        let num = match &self.real {
            Real::Rational(num, denom) => {
                let num = Integer::from_string_base(radix as u8, num)?;
                let den = Integer::from_string_base(radix as u8, denom)?;
                Rational::from_integers(num, den)
            }
            _ => return None,
        };
        Some(if self.neg { -num } else { num })
    }

    fn try_into_f64(&self, radix: u32) -> Option<f64> {
        match &self.real {
            Real::Nan => Some(f64::NAN),
            Real::Inf if !self.neg => Some(f64::INFINITY),
            Real::Inf if self.neg => Some(f64::NEG_INFINITY),
            Real::Num(s) if radix == 10 => {
                let num: f64 = s.parse().ok()?;
                Some(if self.neg { -num } else { num })
            }
            Real::Rational(num, den) if radix == 10 => {
                let num: f64 = num.parse().ok()?;
                let den: f64 = den.parse().ok()?;
                let num = num / den;
                Some(if self.neg { -num } else { num })
            }
            Real::Decimal(base, fract, None) => {
                let num: f64 = format!("{base}.{fract}").parse().ok()?;
                Some(if self.neg { -num } else { num })
            }
            Real::Decimal(base, fract, Some(exp)) => {
                let num: f64 = format!("{base}.{fract}e{exp}").parse().ok()?;
                Some(if self.neg { -num } else { num })
            }
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Exactness {
    Exact,
    Inexact,
}

#[derive(Clone, Debug, PartialEq)]
enum Real {
    Nan,
    Inf,
    Num(String),
    Rational(String, String),
    Decimal(String, String, Option<isize>),
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn is_hash_identifier_char() {
        assert!(!is_initial('#') && !is_peculiar_initial('#'))
    }
}
