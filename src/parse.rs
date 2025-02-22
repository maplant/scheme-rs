use crate::{
    ast::Literal,
    lex::{Character as LexCharacter, Fragment, InputSpan, LexError, Lexeme, Token},
    num::Number,
    syntax::Syntax,
};
use rug::Integer;
use std::char::CharTryFromError;

#[derive(Debug)]
pub enum ParseSyntaxError<'a> {
    EmptyInput,
    UnexpectedEndOfFile,
    ExpectedClosingParen { span: InputSpan<'a> },
    ParseNumberError { value: String, span: InputSpan<'a> },
    InvalidHexValue { value: String, span: InputSpan<'a> },
    InvalidDocCommentLocation { span: InputSpan<'a> },
    InvalidPeriodLocation { span: InputSpan<'a> },
    UnclosedParen { span: InputSpan<'a> },
    DocCommentMustPrecedeDefine,
    CharTryFrom(CharTryFromError),
    LexError(LexError<'a>),
}

impl<'a> From<LexError<'a>> for ParseSyntaxError<'a> {
    fn from(lex: LexError<'a>) -> Self {
        Self::LexError(lex)
    }
}
impl From<CharTryFromError> for ParseSyntaxError<'_> {
    fn from(e: CharTryFromError) -> Self {
        Self::CharTryFrom(e)
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
        [n @ token!(Lexeme::Number(_)), tail @ ..] => {
            Ok((tail, Syntax::new_literal(number(n)?, n.span.clone())))
        }
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

fn vector<'a, 'b>(
    mut i: &'b [Token<'a>],
    span: InputSpan<'a>,
) -> Result<(&'b [Token<'a>], Syntax), ParseVectorError<'a>> {
    let mut output = Vec::new();
    loop {
        match i {
            [] => return Err(ParseVectorError::UnclosedParen),
            [token!(Lexeme::RParen), tail @ ..] => {
                return Ok((tail, Syntax::new_vector(output, span)))
            }
            _ => (),
        }

        let (remaining, expr) = expression(i)?;
        output.push(expr);
        i = remaining;
    }
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

fn number<'a>(i: &Token<'a>) -> Result<Literal, ParseSyntaxError<'a>> {
    let number = i.lexeme.to_number();
    // TODO: Parse correctly
    let number: Integer = number.parse().unwrap();
    match number.to_i64() {
        Some(fixed) => Ok(Literal::Number(Number::FixedInteger(fixed))),
        None => Ok(Literal::Number(Number::BigInteger(number))),
    }
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
