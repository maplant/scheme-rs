use crate::{
    ast::Literal,
    lex::{
        Character as LexCharacter, Fragment, InputSpan, LexError, Lexeme, Number as LexNumber,
        Token, TryFromNumberError,
    },
    num::Number,
    syntax::Syntax,
};
use rug::{Integer, Rational};
use std::{char::CharTryFromError, error::Error as StdError, fmt, num::TryFromIntError};

#[derive(Debug)]
pub enum ParseSyntaxError<'a> {
    EmptyInput,
    UnexpectedEndOfFile,
    ExpectedClosingParen { span: InputSpan<'a> },
    InvalidHexValue { value: String, span: InputSpan<'a> },
    InvalidPeriodLocation { span: InputSpan<'a> },
    NonByte { span: InputSpan<'a> },
    UnclosedParen { span: InputSpan<'a> },
    CharTryFrom(CharTryFromError),
    Lex(LexError<'a>),
    TryFromInt(TryFromIntError),
    TryFromNumber(TryFromNumberError),
}
impl fmt::Display for ParseSyntaxError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyInput => write!(f, "cannot parse an empty list"),
            Self::UnexpectedEndOfFile => write!(f, "unexpected end of file"),
            Self::ExpectedClosingParen { span } => {
                write!(f, "closing parenthesis not found at `{}`", span)
            }
            Self::InvalidHexValue { value, span } => {
                write!(f, "invalid hex value `{}` found at `{}`", value, span)
            }
            Self::InvalidPeriodLocation { span } => {
                write!(f, "invalid period found at location `{}`", span)
            }
            Self::NonByte { span } => write!(
                f,
                "non byte value found in byte vector at location `{}`",
                span
            ),
            Self::UnclosedParen { span } => {
                write!(f, "unclosed parenthesis at location `{}`", span)
            }
            Self::CharTryFrom(e) => write!(f, "{}", e),
            Self::Lex(e) => write!(f, "{}", e),
            Self::TryFromInt(e) => write!(f, "{}", e),
            Self::TryFromNumber(e) => write!(f, "{}", e),
        }
    }
}
impl StdError for ParseSyntaxError<'_> {}

impl From<TryFromIntError> for ParseSyntaxError<'_> {
    fn from(e: TryFromIntError) -> Self {
        Self::TryFromInt(e)
    }
}
impl<'a> From<LexError<'a>> for ParseSyntaxError<'a> {
    fn from(lex: LexError<'a>) -> Self {
        Self::Lex(lex)
    }
}
impl From<CharTryFromError> for ParseSyntaxError<'_> {
    fn from(e: CharTryFromError) -> Self {
        Self::CharTryFrom(e)
    }
}
impl From<TryFromNumberError> for ParseSyntaxError<'_> {
    fn from(e: TryFromNumberError) -> Self {
        Self::TryFromNumber(e)
    }
}

impl<'a> ParseSyntaxError<'a> {
    fn try_parse_hex<S: AsRef<str> + ?Sized>(hex: &S, span: InputSpan<'a>) -> Result<u32, Self> {
        u32::from_str_radix(hex.as_ref(), 16)
            .ok()
            .ok_or_else(|| Self::InvalidHexValue {
                value: hex.as_ref().to_string(),
                span,
            })
    }

    fn invalid_period(token: &Token<'a>) -> Self {
        Self::InvalidPeriodLocation {
            span: token.span.clone(),
        }
    }

    fn unclosed_paren(token: &Token<'a>) -> Self {
        Self::UnclosedParen {
            span: token.span.clone(),
        }
    }
}

macro_rules! token {
    ( $pattern:pat ) => {
        Token {
            lexeme: $pattern,
            ..
        }
    };
}

pub fn expression<'a, 'b>(
    i: &'b [Token<'a>],
) -> Result<(&'b [Token<'a>], Syntax), ParseSyntaxError<'a>> {
    match i {
        // Calling expression with an empty list is an error
        [] => Err(ParseSyntaxError::EmptyInput),
        // Literals:
        [b @ token!(Lexeme::Boolean(_)), tail @ ..] => {
            Ok((tail, Syntax::new_literal(boolean(b)?, b.span.clone())))
        }
        [c @ token!(Lexeme::Character(_)), tail @ ..] => {
            Ok((tail, Syntax::new_literal(character(c)?, c.span.clone())))
        }
        [Token {
            lexeme: Lexeme::Number(n),
            span,
        }, tail @ ..] => Ok((tail, Syntax::new_literal(number(n)?, span.clone()))),
        [s @ token!(Lexeme::String(_)), tail @ ..] => {
            Ok((tail, Syntax::new_literal(string(s)?, s.span.clone())))
        }
        // Identifiers:
        [i @ token!(Lexeme::Identifier(_)), tail @ ..] => Ok((
            tail,
            Syntax::new_identifier(i.lexeme.to_ident(), i.span.clone()),
        )),
        // Lists:
        [n @ token!(Lexeme::LParen), token!(Lexeme::RParen), tail @ ..] => {
            Ok((tail, Syntax::new_null(n.span.clone())))
        }
        [n @ token!(Lexeme::LBracket), token!(Lexeme::RBracket), tail @ ..] => {
            Ok((tail, Syntax::new_null(n.span.clone())))
        }
        [p @ token!(Lexeme::LParen), tail @ ..] => match list(tail, p.span.clone(), Lexeme::RParen)
        {
            Err(ParseListError::UnclosedParen) => Err(ParseSyntaxError::unclosed_paren(p)),
            Err(ParseListError::ParseError(err)) => Err(err),
            Ok(ok) => Ok(ok),
        },
        [p @ token!(Lexeme::LBracket), tail @ ..] => {
            match list(tail, p.span.clone(), Lexeme::RBracket) {
                Err(ParseListError::UnclosedParen) => Err(ParseSyntaxError::unclosed_paren(p)),
                Err(ParseListError::ParseError(err)) => Err(err),
                Ok(ok) => Ok(ok),
            }
        }
        // Vectors:
        [v @ token!(Lexeme::HashParen), tail @ ..] => match vector(tail, v.span.clone()) {
            Err(ParseVectorError::UnclosedParen) => Err(ParseSyntaxError::unclosed_paren(v)),
            Err(ParseVectorError::ParseError(err)) => Err(err),
            Ok(ok) => Ok(ok),
        },
        // Byte vectors:
        [v @ token!(Lexeme::Vu8Paren), tail @ ..] => match byte_vector(tail, v.span.clone()) {
            Err(ParseVectorError::UnclosedParen) => Err(ParseSyntaxError::unclosed_paren(v)),
            Err(ParseVectorError::ParseError(err)) => Err(err),
            Ok(ok) => Ok(ok),
        },
        // Quote:
        [q @ token!(Lexeme::Quote), tail @ ..] => {
            let (tail, expr) = expression(tail)?;
            let expr_span = expr.span().clone();
            Ok((
                tail,
                Syntax::new_list(
                    vec![
                        Syntax::new_identifier("quote", q.span.clone()),
                        expr,
                        Syntax::new_null(expr_span),
                    ],
                    q.span.clone(),
                ),
            ))
        }
        // Syntax:
        [s @ token!(Lexeme::HashTick), tail @ ..] => {
            let (tail, expr) = expression(tail)?;
            let expr_span = expr.span().clone();
            Ok((
                tail,
                Syntax::new_list(
                    vec![
                        Syntax::new_identifier("syntax", s.span.clone()),
                        expr,
                        Syntax::new_null(expr_span),
                    ],
                    s.span.clone(),
                ),
            ))
        }
        // Invalid locations:
        [d @ token!(Lexeme::Period), ..] => Err(ParseSyntaxError::invalid_period(d)),
        x => todo!("Not implemented: {x:#?}"),
    }
}

#[derive(Debug)]
enum ParseListError<'a> {
    UnclosedParen,
    ParseError(ParseSyntaxError<'a>),
}

impl<'a> From<ParseSyntaxError<'a>> for ParseListError<'a> {
    fn from(pe: ParseSyntaxError<'a>) -> Self {
        Self::ParseError(pe)
    }
}

fn list<'a, 'b>(
    mut i: &'b [Token<'a>],
    span: InputSpan<'a>,
    closing: Lexeme<'static>,
) -> Result<(&'b [Token<'a>], Syntax), ParseListError<'a>> {
    let mut output = Vec::new();
    loop {
        if i.is_empty() {
            return Err(ParseListError::UnclosedParen);
        }

        let (remaining, expr) = expression(i)?;
        output.push(expr);

        match remaining {
            // Proper lists:
            [token, tail @ ..] if token.lexeme == closing => {
                output.push(Syntax::new_null(token.span.clone()));
                return Ok((tail, Syntax::new_list(output, span)));
            }
            [token!(Lexeme::Period), end @ token!(Lexeme::LParen), token!(Lexeme::RParen), token, tail @ ..]
            | [token!(Lexeme::Period), end @ token!(Lexeme::LBracket), token!(Lexeme::RBracket), token, tail @ ..]
                if token.lexeme == closing =>
            {
                output.push(Syntax::new_null(end.span.clone()));
                return Ok((tail, Syntax::new_list(output, span)));
            }
            // Improper lists:
            [token!(Lexeme::Period), tail @ ..] => {
                let (remaining, expr) = expression(tail)?;
                output.push(expr);
                return match remaining {
                    [] => Err(ParseListError::ParseError(
                        ParseSyntaxError::UnexpectedEndOfFile,
                    )),
                    [token!(Lexeme::RParen), tail @ ..] => {
                        Ok((tail, Syntax::new_list(output, span)))
                    }
                    [unexpected, ..] => Err(ParseListError::ParseError(
                        ParseSyntaxError::ExpectedClosingParen {
                            span: unexpected.span.clone(),
                        },
                    )),
                };
            }
            _ => (),
        }
        i = remaining;
    }
}

#[derive(Debug)]
enum ParseVectorError<'a> {
    UnclosedParen,
    ParseError(ParseSyntaxError<'a>),
}

impl<'a> From<ParseSyntaxError<'a>> for ParseVectorError<'a> {
    fn from(pe: ParseSyntaxError<'a>) -> Self {
        Self::ParseError(pe)
    }
}

fn vector_shared<'a, 'b>(
    mut i: &'b [Token<'a>],
) -> Result<(&'b [Token<'a>], Vec<Syntax>), ParseVectorError<'a>> {
    let mut output = Vec::new();
    loop {
        match i {
            [] => return Err(ParseVectorError::UnclosedParen),
            [token!(Lexeme::RParen), tail @ ..] => return Ok((tail, output)),
            _ => (),
        }

        let (remaining, expr) = expression(i)?;
        output.push(expr);
        i = remaining;
    }
}
fn vector<'a, 'b>(
    i: &'b [Token<'a>],
    span: InputSpan<'a>,
) -> Result<(&'b [Token<'a>], Syntax), ParseVectorError<'a>> {
    let (i, vec) = vector_shared(i)?;

    Ok((i, Syntax::new_vector(vec, span)))
}

fn byte_vector<'a, 'b>(
    i: &'b [Token<'a>],
    span: InputSpan<'a>,
) -> Result<(&'b [Token<'a>], Syntax), ParseVectorError<'a>> {
    let (i, vec) = vector_shared(i)?;
    let vec = vec
        .into_iter()
        .map(|i| {
            if let Syntax::Literal {
                literal: Literal::Number(Number::FixedInteger(i)),
                ..
            } = i
            {
                Ok(i)
            } else {
                Err(ParseSyntaxError::NonByte { span: span.clone() })
            }
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(|i| u8::try_from(i).map_err(ParseSyntaxError::from))
        .collect::<Result<Vec<_>, _>>()?;

    Ok((i, Syntax::new_byte_vector(vec, span)))
}

fn boolean<'a>(i: &Token<'a>) -> Result<Literal, ParseSyntaxError<'a>> {
    Ok(Literal::Boolean(i.lexeme.to_boolean()))
}

fn character<'a>(i: &Token<'a>) -> Result<Literal, ParseSyntaxError<'a>> {
    let char = i.lexeme.to_char();
    match char {
        LexCharacter::Literal(c) => Ok(Literal::Character(*c)),
        LexCharacter::Escaped(e) => Ok(Literal::Character((*e).into())),
        LexCharacter::Unicode(u) => Ok(Literal::Character(char::try_from(
            ParseSyntaxError::try_parse_hex(u, i.span.clone())?,
        )?)),
    }
}

fn number<'a>(i: &LexNumber<'a>) -> Result<Literal, ParseSyntaxError<'a>> {
    <LexNumber as TryInto<i64>>::try_into(*i)
        .map(Number::FixedInteger)
        .or_else(|_| <LexNumber as TryInto<Integer>>::try_into(*i).map(Number::BigInteger))
        .or_else(|_| <LexNumber as TryInto<Rational>>::try_into(*i).map(Number::Rational))
        .or_else(|_| <LexNumber as TryInto<f64>>::try_into(*i).map(Number::Real))
        .map(Literal::Number)
        .map_err(ParseSyntaxError::from)
}

fn string<'a>(i: &Token<'a>) -> Result<Literal, ParseSyntaxError<'a>> {
    let fragments = i.lexeme.to_string();
    let mut output = String::new();
    for fragment in fragments {
        match fragment {
            Fragment::Escaped(c) => output.push(*c),
            Fragment::Unescaped(s) => output.push_str(s),
            Fragment::HexValue(hex) => {
                let hex_value = ParseSyntaxError::try_parse_hex(hex, i.span.clone())?;
                let Some(c) = char::from_u32(hex_value) else {
                    return Err(ParseSyntaxError::InvalidHexValue {
                        value: hex.to_string(),
                        span: i.span.clone(),
                    });
                };
                output.push(c);
            }
        }
    }
    Ok(Literal::String(output))
}
