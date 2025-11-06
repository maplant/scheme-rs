//! Lexical analysis of symbolic expressions

use super::Span;
use malachite::{Integer, base::num::conversion::traits::*, rational::Rational};
use scheme_rs_macros::{maybe_async, maybe_await};
use unicode_categories::UnicodeCategories;

#[cfg(feature = "async")]
use futures::future::BoxFuture;

use crate::{
    num,
    ports::{PortInner, ReadError},
};

pub struct Lexer<'a> {
    port: &'a mut PortInner,
    pos: usize,
    buff: Vec<char>,
    curr_span: Span,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(port: &'a mut PortInner, span: Span) -> Self {
        Self {
            port,
            pos: 0,
            buff: Vec::new(),
            curr_span: span,
        }
    }

    pub(crate) fn curr_span(&self) -> Span {
        Span {
            line: self.curr_span.line,
            column: self.curr_span.column,
            offset: self.curr_span.offset + self.pos,
            file: self.curr_span.file.clone(),
        }
    }

    #[maybe_async]
    fn peek(&mut self) -> Result<Option<char>, ReadError> {
        if self.buff.len() > self.pos {
            return Ok(Some(self.buff[self.pos]));
        }
        while self.buff.len() < self.pos {
            let Some(chr) = maybe_await!(self.port.read_char()?) else {
                return Ok(None);
            };
            self.buff.push(chr);
        }
        maybe_await!(self.port.peekn_chars(0))
    }

    fn skip(&mut self) {
        self.pos += 1;
    }

    #[maybe_async]
    fn take(&mut self) -> Result<Option<char>, ReadError> {
        let Some(chr) = maybe_await!(self.peek())? else {
            return Ok(None);
        };
        self.pos += 1;
        Ok(Some(chr))
    }

    #[maybe_async]
    fn match_char(&mut self, chr: char) -> Result<bool, ReadError> {
        Ok(maybe_await!(self.match_pred(|peek| peek == chr))?.is_some())
    }

    #[maybe_async]
    fn match_pred(&mut self, pred: impl FnOnce(char) -> bool) -> Result<Option<char>, ReadError> {
        let chr = maybe_await!(self.peek())?;
        if let Some(chr) = chr
            && pred(chr)
        {
            if chr == '\n' {
                self.curr_span.line += 1;
                self.curr_span.column = 0;
            } else {
                self.curr_span.column += 1;
            }
            self.pos += 1;
            Ok(Some(chr))
        } else {
            Ok(None)
        }
    }

    #[maybe_async]
    fn match_tag(&mut self, tag: &str) -> Result<bool, ReadError> {
        let pos = self.pos;
        for chr in tag.chars() {
            if !maybe_await!(self.match_char(chr))? {
                self.pos = pos;
                return Ok(false);
            }
        }
        // tag cannot contain newlines
        self.curr_span.column += self.pos;
        Ok(true)
    }

    fn consume_chars(&mut self) -> Result<(), ReadError> {
        // Consume all the characters we need to
        if self.pos > self.buff.len() {
            self.port.consume_chars(self.pos - self.buff.len())?;
        }
        self.pos = 0;
        self.buff.clear();
        Ok(())
    }

    #[maybe_async]
    pub fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        // TODO: Check if the port is empty

        // Check for any interlexeme space:
        maybe_await!(self.interlexeme_space())?;

        // self.consume_chars()?;

        // Get the current span:
        let span = self.curr_span();

        // Check for various special characters:
        let lexeme = if let Some(number) = maybe_await!(self.number())? {
            Lexeme::Number(number)
        } else if let Some(identifier) = maybe_await!(self.identifier())? {
            Lexeme::Identifier(identifier)
        } else if let Some(chr) = maybe_await!(self.take())? {
            match chr {
                '.' => Lexeme::Period,
                '\'' => Lexeme::Quote,
                '`' => Lexeme::Backquote,
                ',' if maybe_await!(self.match_tag("@"))? => Lexeme::CommaAt,
                ',' => Lexeme::Comma,
                '(' => Lexeme::LParen,
                ')' => Lexeme::RParen,
                '[' => Lexeme::LBracket,
                ']' => Lexeme::RBracket,
                '"' => Lexeme::String(maybe_await!(self.string())?),
                '#' if maybe_await!(self.match_tag(";"))? => Lexeme::DatumComment,
                '#' if maybe_await!(self.match_tag("\\"))? => {
                    Lexeme::Character(maybe_await!(self.character())?)
                }
                '#' if maybe_await!(self.match_tag("F"))? || maybe_await!(self.match_tag("f"))? => {
                    Lexeme::Boolean(false)
                }
                '#' if maybe_await!(self.match_tag("T"))? || maybe_await!(self.match_tag("t"))? => {
                    Lexeme::Boolean(true)
                }
                '#' if maybe_await!(self.match_tag("("))? => Lexeme::HashParen,
                '#' if maybe_await!(self.match_tag("u8("))? => Lexeme::Vu8Paren,
                '#' if maybe_await!(self.match_tag("'"))? => Lexeme::HashQuote,
                '#' if maybe_await!(self.match_tag("`"))? => Lexeme::HashBackquote,
                '#' if maybe_await!(self.match_tag(",@"))? => Lexeme::HashCommaAt,
                '#' if maybe_await!(self.match_tag(","))? => Lexeme::HashComma,
                '#' => {
                    let next_chr = maybe_await!(self.take())?;
                    if let Some(chr) = next_chr {
                        return Err(LexerError::UnexpectedCharacter {
                            chr,
                            span: self.curr_span(),
                        });
                    } else {
                        return Err(LexerError::UnexpectedEof);
                    }
                }
                '\0' => return Ok(None),
                chr => return Err(LexerError::UnexpectedCharacter { chr, span }),
            }
        } else {
            return Ok(None);
        };

        self.consume_chars()?;

        Ok(Some(Token { lexeme, span }))
    }

    #[maybe_async]
    fn interlexeme_space(&mut self) -> Result<(), ReadError> {
        loop {
            if maybe_await!(self.match_char(';'))? {
                maybe_await!(self.comment())?;
            } else if maybe_await!(self.match_tag("#|"))? {
                maybe_await!(self.nested_comment())?;
            } else if maybe_await!(self.match_pred(is_whitespace))?.is_none() {
                break;
            }
        }
        Ok(())
    }

    #[maybe_async]
    fn comment(&mut self) -> Result<(), ReadError> {
        while maybe_await!(self.match_pred(|chr| chr != '\n'))?.is_some() {}
        Ok(())
    }

    #[cfg(feature = "async")]
    fn nested_comment(&mut self) -> BoxFuture<'_, Result<(), ReadError>> {
        Box::pin(self.nested_comment_inner())
    }

    #[cfg(not(feature = "async"))]
    fn nested_comment(&mut self) -> Result<(), ReadError> {
        self.nested_comment_inner()
    }

    #[maybe_async]
    fn nested_comment_inner(&mut self) -> Result<(), ReadError> {
        while !maybe_await!(self.match_tag("|#"))? {
            if maybe_await!(self.match_tag("#|"))? {
                maybe_await!(self.nested_comment())?;
            } else {
                self.skip();
            }
        }
        Ok(())
    }

    #[maybe_async]
    fn character(&mut self) -> Result<Character, LexerError> {
        let chr = if maybe_await!(self.match_tag("alarm"))? {
            Character::Escaped(EscapedCharacter::Alarm)
        } else if maybe_await!(self.match_tag("backspace"))? {
            Character::Escaped(EscapedCharacter::Backspace)
        } else if maybe_await!(self.match_tag("delete"))? {
            Character::Escaped(EscapedCharacter::Delete)
        } else if maybe_await!(self.match_tag("escape"))? {
            Character::Escaped(EscapedCharacter::Escape)
        } else if maybe_await!(self.match_tag("newline"))? {
            Character::Escaped(EscapedCharacter::Newline)
        } else if maybe_await!(self.match_tag("null"))? {
            Character::Escaped(EscapedCharacter::Null)
        } else if maybe_await!(self.match_tag("return"))? {
            Character::Escaped(EscapedCharacter::Return)
        } else if maybe_await!(self.match_tag("space"))? {
            Character::Escaped(EscapedCharacter::Space)
        } else if maybe_await!(self.match_tag("tab"))? {
            Character::Escaped(EscapedCharacter::Tab)
        } else if maybe_await!(self.match_char('x'))? {
            if is_delimiter(maybe_await!(self.peek())?.ok_or(LexerError::UnexpectedEof)?) {
                Character::Literal('x')
            } else {
                let mut unicode = String::new();
                while let Some(chr) = maybe_await!(self.match_pred(|c| c.is_ascii_hexdigit()))? {
                    unicode.push(chr);
                }
                Character::Unicode(unicode)
            }
        } else {
            Character::Literal(maybe_await!(self.take())?.ok_or(LexerError::UnexpectedEof)?)
        };
        let peeked = maybe_await!(self.peek())?;
        if let Some(peeked) = peeked
            && !is_delimiter(peeked)
        {
            let span = self.curr_span();
            Err(LexerError::UnexpectedCharacter { chr: peeked, span })
        } else {
            Ok(chr)
        }
    }

    #[maybe_async]
    fn number(&mut self) -> Result<Option<Number>, ReadError> {
        let saved_pos = self.pos;

        let (radix, exactness) = maybe_await!(self.radix_and_exactness())?;

        let radix = radix.unwrap_or(10);

        // Need this because "10i" is not a valid number.
        let has_sign = {
            let peeked = maybe_await!(self.peek())?;
            peeked == Some('+') || peeked == Some('-')
        };

        let first_part = maybe_await!(self.part(radix))?;

        if first_part.is_none() {
            self.pos = saved_pos;
            return Ok(None);
        }

        let number = if maybe_await!(self.match_char('i'))? {
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
            let matched_at = maybe_await!(self.match_char('@'))?;
            let imag_part = if matched_at || {
                let peeked = maybe_await!(self.peek())?;
                peeked == Some('+') || peeked == Some('-')
            } {
                let second_part = maybe_await!(self.part(radix))?;
                if second_part.is_none() || !matched_at && !maybe_await!(self.match_char('i'))? {
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

        match maybe_await!(self.peek()) {
            Ok(Some(chr)) if is_subsequent(chr) => {
                self.pos = saved_pos;
                return Ok(None);
            }
            Err(err) => return Err(err),
            Ok(_) => (),
        }

        Ok(Some(number))
    }

    #[maybe_async]
    fn part(&mut self, radix: u32) -> Result<Option<Part>, ReadError> {
        let neg = !maybe_await!(self.match_char('+'))? && maybe_await!(self.match_char('-'))?;
        let mut mantissa_width = None;

        // Check for special nan/inf
        let real = if maybe_await!(self.match_tag("nan.0"))? {
            Real::Nan
        } else if maybe_await!(self.match_tag("inf.0"))? {
            Real::Inf
        } else {
            let mut num = String::new();
            while let Some(ch) = maybe_await!(self.match_pred(|chr| chr.is_digit(radix)))? {
                num.push(ch);
            }
            if !num.is_empty() && maybe_await!(self.match_char('/'))? {
                // Rational number
                let mut denom = String::new();
                while let Some(ch) = maybe_await!(self.match_pred(|chr| chr.is_digit(radix)))? {
                    denom.push(ch);
                }
                if denom.is_empty() {
                    return Ok(None);
                }
                Real::Rational(num, denom)
            } else if radix == 10 {
                let mut fractional = String::new();
                if maybe_await!(self.match_char('.'))? {
                    while let Some(ch) = maybe_await!(self.match_pred(|chr| chr.is_digit(radix)))? {
                        fractional.push(ch);
                    }
                }
                if num.is_empty() && fractional.is_empty() {
                    return Ok(None);
                }
                let suffix = maybe_await!(self.suffix())?;
                if maybe_await!(self.match_char('|'))? {
                    let mut width = 0;
                    while let Some(chr) = maybe_await!(self.match_pred(|chr| chr.is_ascii_digit()))?
                    {
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

    #[maybe_async]
    fn exactness(&mut self) -> Result<Option<Exactness>, ReadError> {
        Ok(
            if maybe_await!(self.match_tag("#i"))? || maybe_await!(self.match_tag("#I"))? {
                Some(Exactness::Inexact)
            } else if maybe_await!(self.match_tag("#e"))? || maybe_await!(self.match_tag("#E"))? {
                Some(Exactness::Exact)
            } else {
                None
            },
        )
    }

    #[maybe_async]
    fn radix(&mut self) -> Result<Option<u32>, ReadError> {
        Ok(
            if maybe_await!(self.match_tag("#b"))? || maybe_await!(self.match_tag("#B"))? {
                Some(2)
            } else if maybe_await!(self.match_tag("#o"))? || maybe_await!(self.match_tag("#O"))? {
                Some(8)
            } else if maybe_await!(self.match_tag("#x"))? || maybe_await!(self.match_tag("#X"))? {
                Some(16)
            } else if maybe_await!(self.match_tag("#d"))? || maybe_await!(self.match_tag("#D"))? {
                Some(10)
            } else {
                None
            },
        )
    }

    #[maybe_async]
    fn radix_and_exactness(&mut self) -> Result<(Option<u32>, Option<Exactness>), ReadError> {
        let exactness = maybe_await!(self.exactness())?;
        let radix = maybe_await!(self.radix())?;
        if exactness.is_some() {
            Ok((radix, exactness))
        } else {
            Ok((radix, maybe_await!(self.exactness())?))
        }
    }

    #[maybe_async]
    fn suffix(&mut self) -> Result<Option<isize>, ReadError> {
        let pos = self.pos;
        if maybe_await!(
            self.match_pred(|chr| matches!(chr.to_ascii_lowercase(), 'e' | 's' | 'f' | 'd' | 'l'))
        )?
        .is_some()
        {
            let neg = !maybe_await!(self.match_char('+'))? && maybe_await!(self.match_char('-'))?;
            let mut suffix = String::new();
            while let Some(chr) = maybe_await!(self.match_pred(|chr| chr.is_ascii_digit()))? {
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

    #[maybe_async]
    fn string(&mut self) -> Result<String, LexerError> {
        let mut output = String::new();
        while let Some(chr) = maybe_await!(self.match_pred(|chr| chr != '"'))? {
            if chr == '\\' {
                let escaped = match maybe_await!(self.take())?.ok_or(LexerError::UnexpectedEof)? {
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
                        while maybe_await!(self.match_pred(is_intraline_whitespace))?.is_some() {}
                        continue;
                    }
                    chr if is_intraline_whitespace(chr) => {
                        while maybe_await!(
                            self.match_pred(|chr| chr != '\n' && is_intraline_whitespace(chr))
                        )?
                        .is_some()
                        {}
                        let chr = maybe_await!(self.take())?.ok_or(LexerError::UnexpectedEof)?;
                        if chr != '\n' {
                            let span = self.curr_span();
                            return Err(LexerError::UnexpectedCharacter { chr, span });
                        }
                        while maybe_await!(self.match_pred(is_intraline_whitespace))?.is_some() {}
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

    #[maybe_async]
    fn identifier(&mut self) -> Result<Option<String>, LexerError> {
        let mut ident = if maybe_await!(self.match_tag("\\x"))? {
            maybe_await!(self.inline_hex_escape())?
        } else if maybe_await!(self.match_tag("..."))? {
            String::from("...")
        } else if maybe_await!(self.match_tag("->"))? {
            String::from("->")
        } else if let Some(initial) =
            maybe_await!(self.match_pred(|chr| is_initial(chr) || is_peculiar_initial(chr)))?
        {
            String::from(initial)
        } else {
            return Ok(None);
        };

        loop {
            if maybe_await!(self.match_tag("\\x"))? {
                ident.push_str(&maybe_await!(self.inline_hex_escape())?);
            } else if let Some(next) = maybe_await!(self.match_pred(is_subsequent))? {
                ident.push(next);
            } else {
                break;
            }
        }

        Ok(Some(ident))
    }

    #[maybe_async]
    fn inline_hex_escape(&mut self) -> Result<String, LexerError> {
        let mut escaped = String::new();
        let mut buff = String::with_capacity(2);
        while let Some(chr) = maybe_await!(self.match_pred(|chr| chr != ';'))? {
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
    UnexpectedEof,
    InvalidCharacterInHexEscape { chr: char, span: Span },
    UnexpectedCharacter { chr: char, span: Span },
    BadEscapeCharacter { chr: char, span: Span },
    ReadError(ReadError),
}

impl From<ReadError> for LexerError {
    fn from(error: ReadError) -> Self {
        Self::ReadError(error)
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
