/*use crate::{
    ast::Literal,
    lex::{
        Character as LexCharacter, Fragment, InputSpan, LexError, Lexeme, Number as LexNumber,
        Token, TryFromNumberError,
    },
    num::Number,
};
*/

use crate::ast::Literal;

use super::{
    Span, Syntax,
    lex::{Character, Lexeme, Lexer, LexerError, Token},
};
use futures::future::BoxFuture;
use malachite::{Integer, rational::Rational};
use std::{char::CharTryFromError, error::Error as StdError, fmt, num::TryFromIntError};

pub struct Parser<'a> {
    /// We only ever need one token of lookahead probably, but this is more
    /// obviously correct
    lookahead: Vec<Token>,
    lexer: Lexer<'a>,
}

macro_rules! token {
    ( $pattern:pat ) => {
        Token {
            lexeme: $pattern,
            ..
        }
    };
    ( $pattern:pat, $span:pat ) => {
        Token {
            lexeme: $pattern,
            span: $span,
        }
    };
}

impl Parser<'_> {
    // pub fn new(input_port: 
    
    async fn next_token(&mut self) -> Result<Token, LexerError> {
        if let Some(next) = self.lookahead.pop() {
            Ok(next)
        } else {
            self.lexer.next_token().await
        }
    }

    fn return_token(&mut self, token: Token) {
        self.lookahead.push(token)
    }

    pub fn expression(&mut self) -> BoxFuture<'_, Result<Option<Syntax>, ParseSyntaxError>> {
        Box::pin(async move {
            match self.next_token().await? {
                // Literals:
                token!(Lexeme::Boolean(b), span) => {
                    Ok(Some(Syntax::new_literal(Literal::Boolean(b), span.clone())))
                }
                token!(Lexeme::Character(Character::Literal(c)), span) => Ok(Some(
                    Syntax::new_literal(Literal::Character(c), span.clone()),
                )),
                token!(Lexeme::Character(Character::Escaped(e)), span) => Ok(Some(
                    Syntax::new_literal(Literal::Character(e.into()), span.clone()),
                )),

                // Identifiers:
                token!(Lexeme::Identifier(ident), span) => {
                    Ok(Some(Syntax::new_identifier(&ident, span)))
                }

                // Lists:
                token!(Lexeme::LParen, span) => Ok(Some(self.list(span, Lexeme::RParen).await?)),
                token!(Lexeme::LBracket, span) => {
                    Ok(Some(self.list(span, Lexeme::RBracket).await?))
                }

                // Vectors:
                token!(Lexeme::HashParen, span) => Ok(Some(self.vector(span).await?)),
                token!(Lexeme::Vu8Paren, span) => todo!(),
                
                // Datum comments:
                token!(Lexeme::DatumComment) => {
                    // Discard next expression:
                    let _ = self.expression().await?;
                    Ok(None)
                }

                _ => todo!(),
            }
        })
    }

    async fn require_expression(&mut self) -> Result<Syntax, ParseSyntaxError> {
        loop {
            if let Some(expr) = self.expression().await? {
                return Ok(expr);
            }
        }
    }

    async fn list(&mut self, span: Span, closing: Lexeme) -> Result<Syntax, ParseSyntaxError> {
        match self.next_token().await? {
            // We allow for (. expr) to resolve to expr, just because it's
            // easier. Maybe we'll disallow this eventualy
            token!(Lexeme::Period) => return self.require_expression().await,
            // If the first token is a closing paren, then this is an empty
            // list
            token if token.lexeme == closing => return Ok(Syntax::new_null(token.span)),
            // Otherwise, push the token back and continue
            token => {
                self.return_token(token);
            }
        }

        let mut output = Vec::new();
        loop {
            if let Some(expr) = self.expression().await? {
                output.push(expr);
            }
            match self.next_token().await? {
                token if token.lexeme == closing => {
                    output.push(Syntax::new_null(token.span));
                    return Ok(Syntax::new_list(output, span));
                }
                token!(Lexeme::Period) => {
                    let peek1 = self.next_token().await?;
                    let peek2 = self.next_token().await?;
                    match (peek1, peek2) {
                        // Proper list with period:
                        (token!(Lexeme::LParen, end_span), token!(Lexeme::RParen))
                        | (token!(Lexeme::LBracket, end_span), token!(Lexeme::RBracket)) => {
                            output.push(Syntax::new_null(end_span));
                            return Ok(Syntax::new_list(output, span));
                        }
                        // Improper list:
                        (peek1, peek2) => {
                            self.return_token(peek2);
                            self.return_token(peek1);
                        }
                    }
                    output.push(self.require_expression().await?);
                    let last = self.next_token().await?;
                    if last.lexeme == closing {
                        return Ok(Syntax::new_list(output, span));
                    } else {
                        return Err(ParseSyntaxError::ExpectedClosingParen { span: last.span });
                    }
                }
                token => self.return_token(token),
            }
        }
    }

    async fn vector(&mut self, span: Span) -> Result<Syntax, ParseSyntaxError> {
        let mut output = Vec::new();
        loop {
            match self.next_token().await? {
                token!(Lexeme::RParen) => return Ok(Syntax::new_vector(output, span)),
                token => {
                    self.return_token(token);
                    if let Some(expr) = self.expression().await? {
                        output.push(expr);
                    }
                }
            }
        }
    }

    async fn byte_vector(&mut self) -> Result<Syntax, ParseSyntaxError> {
        todo!()
    }

    async fn alias(&mut self, alias: &str, span: Span) -> Result<Syntax, ParseSyntaxError> {
        let expr = self.require_expression().await?;
        let expr_span = expr.span().clone();
        Ok(Syntax::new_list(
            vec![
                Syntax::new_identifier(alias, span.clone()),
                expr,
                Syntax::new_null(expr_span),
            ],
            span,
        ))
    }
}

#[derive(Debug)]
pub enum ParseSyntaxError {
    EmptyInput,
    UnexpectedEndOfFile,
    ExpectedClosingParen { span: Span },
    UnexpectedClosingParen { span: Span },
    InvalidHexValue { value: String, span: Span },
    InvalidPeriodLocation { span: Span },
    NonByte { span: Span },
    UnclosedParen { span: Span },
    CharTryFrom(CharTryFromError),
    Lex(LexerError),
    TryFromInt(TryFromIntError),
    // TryFromNumber(TryFromNumberError),
    UnexpectedToken { token: Token },
}

impl fmt::Display for ParseSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyInput => write!(f, "cannot parse an empty list"),
            Self::UnexpectedEndOfFile => write!(f, "unexpected end of file"),
            Self::ExpectedClosingParen { span } => {
                write!(f, "closing parenthesis not found at `{span}`")
            }
            Self::UnexpectedClosingParen { span } => {
                write!(f, "unexpected closing parenthesis found at `{span}`")
            }
            Self::InvalidHexValue { value, span } => {
                write!(f, "invalid hex value `{value}` found at `{span}`")
            }
            Self::InvalidPeriodLocation { span } => {
                write!(f, "invalid period found at location `{span}`")
            }
            Self::NonByte { span } => write!(
                f,
                "non byte value found in byte vector at location `{span}`",
            ),
            Self::UnclosedParen { span } => {
                write!(f, "unclosed parenthesis at location `{span}`")
            }
            Self::CharTryFrom(e) => write!(f, "{e}"),
            Self::Lex(e) => write!(f, "{e:?}"),
            Self::TryFromInt(e) => write!(f, "{e}"),
            // Self::TryFromNumber(e) => write!(f, "{e}"),
            Self::UnexpectedToken { token } => {
                write!(
                    f,
                    "unexpected token {:?} at location `{}`",
                    token.lexeme, token.span
                )
            }
        }
    }
}
impl StdError for ParseSyntaxError {}

impl From<TryFromIntError> for ParseSyntaxError {
    fn from(e: TryFromIntError) -> Self {
        Self::TryFromInt(e)
    }
}

impl From<LexerError> for ParseSyntaxError {
    fn from(lex: LexerError) -> Self {
        Self::Lex(lex)
    }
}

impl From<CharTryFromError> for ParseSyntaxError {
    fn from(e: CharTryFromError) -> Self {
        Self::CharTryFrom(e)
    }
}

/*
impl From<TryFromNumberError> for ParseSyntaxError {
    fn from(e: TryFromNumberError) -> Self {
        Self::TryFromNumber(e)
    }
}
*/

impl ParseSyntaxError {
    fn try_parse_hex<S: AsRef<str> + ?Sized>(hex: &S, span: Span) -> Result<u32, Self> {
        u32::from_str_radix(hex.as_ref(), 16)
            .ok()
            .ok_or_else(|| Self::InvalidHexValue {
                value: hex.as_ref().to_string(),
                span,
            })
    }

    fn invalid_period(token: &Token) -> Self {
        Self::InvalidPeriodLocation {
            span: token.span.clone(),
        }
    }

    fn unclosed_paren(token: &Token) -> Self {
        Self::UnclosedParen {
            span: token.span.clone(),
        }
    }

    fn unexpected_closing_paren(token: &Token) -> Self {
        Self::UnexpectedClosingParen {
            span: token.span.clone(),
        }
    }
}

/*

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
        [
            Token {
                lexeme: Lexeme::Number(n),
                span,
            },
            tail @ ..,
        ] => Ok((tail, Syntax::new_literal(number(n)?, span.clone()))),
        [s @ token!(Lexeme::String(_)), tail @ ..] => {
            Ok((tail, Syntax::new_literal(string(s)?, s.span.clone())))
        }
        // Identifiers:
        [i @ token!(Lexeme::Identifier(_)), tail @ ..] => Ok((
            tail,
            Syntax::new_identifier(i.lexeme.to_ident(), i.span.clone()),
        )),
        // Lists:
        [
            n @ token!(Lexeme::LParen),
            token!(Lexeme::RParen),
            tail @ ..,
        ] => Ok((tail, Syntax::new_null(n.span.clone()))),
        [
            n @ token!(Lexeme::LBracket),
            token!(Lexeme::RBracket),
            tail @ ..,
        ] => Ok((tail, Syntax::new_null(n.span.clone()))),
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
        [q @ token!(Lexeme::Quote), tail @ ..] => alias("quote", tail, q.span.clone()),
        // Quasiquote:
        [qq @ token!(Lexeme::Backquote), tail @ ..] => alias("quasiquote", tail, qq.span.clone()),
        // Unquote:
        [c @ token!(Lexeme::Comma), tail @ ..] => alias("unquote", tail, c.span.clone()),
        // Unquote splicing:
        [ca @ token!(Lexeme::CommaAt), tail @ ..] => {
            alias("unquote-splicing", tail, ca.span.clone())
        }
        // Syntax:
        [s @ token!(Lexeme::HashTick), tail @ ..] => alias("syntax", tail, s.span.clone()),
        // Quasisyntax:
        [qs @ token!(Lexeme::HashBackquote), tail @ ..] => {
            alias("quasisyntax", tail, qs.span.clone())
        }
        // Unsyntax:
        [us @ token!(Lexeme::HashComma), tail @ ..] => alias("unsyntax", tail, us.span.clone()),
        [ca @ token!(Lexeme::HashCommaAt), tail @ ..] => {
            alias("unsyntax-splicing", tail, ca.span.clone())
        }
        [paren @ token!(Lexeme::RParen), ..] => {
            Err(ParseSyntaxError::unexpected_closing_paren(paren))
        }
        [paren @ token!(Lexeme::RBracket), ..] => {
            Err(ParseSyntaxError::unexpected_closing_paren(paren))
        }
        // Invalid locations:
        [d @ token!(Lexeme::Period), ..] => Err(ParseSyntaxError::invalid_period(d)),
        // Unexpected token (perhaps not supported?):
        [token, ..] => Err(ParseSyntaxError::UnexpectedToken {
            token: token.clone(),
        }),
    }
}

fn alias<'a, 'b>(
    alias: &str,
    tail: &'b [Token<'a>],
    span: InputSpan<'a>,
) -> Result<(&'b [Token<'a>], Syntax), ParseSyntaxError<'a>> {
    let (tail, expr) = expression(tail)?;
    let expr_span = expr.span().clone();
    Ok((
        tail,
        Syntax::new_list(
            vec![
                Syntax::new_identifier(alias, span.clone()),
                expr,
                Syntax::new_null(expr_span),
            ],
            span,
        ),
    ))
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
            [
                token!(Lexeme::Period),
                end @ token!(Lexeme::LParen),
                token!(Lexeme::RParen),
                token,
                tail @ ..,
            ]
            | [
                token!(Lexeme::Period),
                end @ token!(Lexeme::LBracket),
                token!(Lexeme::RBracket),
                token,
                tail @ ..,
            ] if token.lexeme == closing => {
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

*/
