use crate::{ast::Literal, num::Number, ports::PortInner, syntax::lex::ParseNumberError};

pub use super::lex::LexerError;
use super::{
    Span, Syntax,
    lex::{Character, Lexeme, Lexer, Token},
};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::{char::CharTryFromError, error::Error as StdError, fmt};

#[cfg(feature = "async")]
use futures::future::BoxFuture;

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

impl<'a> Parser<'a> {
    pub(crate) fn new(port: &'a mut PortInner, span: Span) -> Self {
        Parser {
            lookahead: Vec::new(),
            lexer: Lexer::new(port, span),
        }
    }
}

impl Parser<'_> {
    #[maybe_async]
    fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        if let Some(next) = self.lookahead.pop() {
            Ok(Some(next))
        } else {
            maybe_await!(self.lexer.next_token())
        }
    }

    pub(crate) fn curr_span(&self) -> Span {
        self.lexer.curr_span()
    }

    fn return_token(&mut self, token: Token) {
        self.lookahead.push(token)
    }

    #[cfg(feature = "async")]
    pub fn expression(&mut self) -> BoxFuture<'_, Result<Option<Syntax>, ParseSyntaxError>> {
        Box::pin(self.expression_inner())
    }

    #[cfg(not(feature = "async"))]
    pub fn expression(&mut self) -> Result<Option<Syntax>, ParseSyntaxError> {
        self.expression_inner()
    }

    #[maybe_async]
    fn expression_inner(&mut self) -> Result<Option<Syntax>, ParseSyntaxError> {
        match maybe_await!(self.next_token())?.ok_or(ParseSyntaxError::UnexpectedEof)? {
            // Literals:
            token!(Lexeme::Boolean(b), span) => {
                Ok(Some(Syntax::new_literal(Literal::Boolean(b), span)))
            }
            token!(Lexeme::Character(Character::Literal(c)), span) => {
                Ok(Some(Syntax::new_literal(Literal::Character(c), span)))
            }
            token!(Lexeme::Character(Character::Escaped(e)), span) => Ok(Some(
                Syntax::new_literal(Literal::Character(e.into()), span),
            )),
            token!(Lexeme::Character(Character::Unicode(u)), span) => {
                Ok(Some(Syntax::new_literal(
                    Literal::Character(char::try_from(u32::from_str_radix(&u, 16).unwrap())?),
                    span,
                )))
            }
            token!(Lexeme::String(s), span) => {
                Ok(Some(Syntax::new_literal(Literal::String(s), span)))
            }
            token!(Lexeme::Number(n), span) => Ok(Some(Syntax::new_literal(
                Literal::Number(Number::try_from(n)?),
                span,
            ))),

            // Identifiers:
            token!(Lexeme::Identifier(ident), span) => {
                Ok(Some(Syntax::new_identifier(&ident, span)))
            }

            // Lists:
            token!(Lexeme::LParen, span) => {
                Ok(Some(maybe_await!(self.list(span, Lexeme::RParen))?))
            }
            token!(Lexeme::LBracket, span) => {
                Ok(Some(maybe_await!(self.list(span, Lexeme::RBracket))?))
            }

            // Vectors:
            token!(Lexeme::HashParen, span) => Ok(Some(maybe_await!(self.vector(span))?)),
            token!(Lexeme::Vu8Paren, span) => Ok(Some(maybe_await!(self.byte_vector(span))?)),

            // Various aliases:
            token!(Lexeme::Quote, span) => Ok(Some(maybe_await!(self.alias("quote", span))?)),
            token!(Lexeme::Backquote, span) => {
                Ok(Some(maybe_await!(self.alias("quasiquote", span))?))
            }
            token!(Lexeme::Comma, span) => Ok(Some(maybe_await!(self.alias("unquote", span))?)),
            token!(Lexeme::CommaAt, span) => {
                Ok(Some(maybe_await!(self.alias("unquote-splicing", span))?))
            }
            token!(Lexeme::HashQuote, span) => Ok(Some(maybe_await!(self.alias("syntax", span))?)),
            token!(Lexeme::HashBackquote, span) => {
                Ok(Some(maybe_await!(self.alias("quasisyntax", span))?))
            }
            token!(Lexeme::HashComma, span) => {
                Ok(Some(maybe_await!(self.alias("unsyntax", span))?))
            }
            token!(Lexeme::HashCommaAt, span) => {
                Ok(Some(maybe_await!(self.alias("unsyntax-splicing", span))?))
            }

            // Datum comments:
            token!(Lexeme::DatumComment) => {
                // Discard next expression:
                let _ = maybe_await!(self.expression())?;
                Ok(None)
            }

            // Handle some erroneous situations:
            token!(Lexeme::RParen, span) | token!(Lexeme::RBracket, span) => {
                Err(ParseSyntaxError::UnexpectedClosingParen { span })
            }

            token!(Lexeme::Period, span) => Err(ParseSyntaxError::InvalidPeriodLocation { span }),
        }
    }

    #[maybe_async]
    pub fn get_sexpr(&mut self) -> Result<Syntax, ParseSyntaxError> {
        loop {
            if let Some(expr) = maybe_await!(self.expression())? {
                return Ok(expr);
            }
        }
    }

    #[maybe_async]
    pub fn get_sexpr_or_eof(&mut self) -> Result<Option<Syntax>, ParseSyntaxError> {
        loop {
            // Check for EOF
            match maybe_await!(self.next_token()) {
                Ok(None) => return Ok(None),
                Err(err) => return Err(ParseSyntaxError::Lex(err)),
                Ok(Some(token)) => self.return_token(token),
            }

            if let Some(expr) = maybe_await!(self.expression())? {
                return Ok(Some(expr));
            }
        }
    }

    #[maybe_async]
    pub fn all_sexprs(&mut self) -> Result<Vec<Syntax>, ParseSyntaxError> {
        let mut sexprs = Vec::new();
        loop {
            // Check for EOF
            match maybe_await!(self.next_token()) {
                Ok(None) => return Ok(sexprs),
                Err(err) => return Err(ParseSyntaxError::Lex(err)),
                Ok(Some(token)) => self.return_token(token),
            }

            sexprs.push(maybe_await!(self.get_sexpr())?);
        }
    }

    #[maybe_async]
    fn list(&mut self, span: Span, closing: Lexeme) -> Result<Syntax, ParseSyntaxError> {
        match maybe_await!(self.next_token())?.ok_or(ParseSyntaxError::UnexpectedEof)? {
            // We allow for (. expr) to resolve to expr, just because it's
            // easier. Maybe we'll disallow this eventualy
            token!(Lexeme::Period) => return maybe_await!(self.get_sexpr()),
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
            if let Some(expr) = maybe_await!(self.expression())? {
                output.push(expr);
            }
            match maybe_await!(self.next_token())?.ok_or(ParseSyntaxError::UnexpectedEof)? {
                token if token.lexeme == closing => {
                    output.push(Syntax::new_null(token.span));
                    return Ok(Syntax::new_list(output, span));
                }
                token!(Lexeme::Period) => {
                    let peek1 =
                        maybe_await!(self.next_token())?.ok_or(ParseSyntaxError::UnexpectedEof)?;
                    let peek2 =
                        maybe_await!(self.next_token())?.ok_or(ParseSyntaxError::UnexpectedEof)?;
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
                    output.push(maybe_await!(self.get_sexpr())?);
                    let last =
                        maybe_await!(self.next_token())?.ok_or(ParseSyntaxError::UnexpectedEof)?;
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

    #[maybe_async]
    fn vector(&mut self, span: Span) -> Result<Syntax, ParseSyntaxError> {
        let mut output = Vec::new();
        loop {
            match maybe_await!(self.next_token())?.ok_or(ParseSyntaxError::UnexpectedEof)? {
                token!(Lexeme::RParen) => return Ok(Syntax::new_vector(output, span)),
                token => {
                    self.return_token(token);
                    if let Some(expr) = maybe_await!(self.expression())? {
                        output.push(expr);
                    }
                }
            }
        }
    }

    #[maybe_async]
    fn byte_vector(&mut self, span: Span) -> Result<Syntax, ParseSyntaxError> {
        let mut output = Vec::new();
        loop {
            match maybe_await!(self.next_token())?.ok_or(ParseSyntaxError::UnexpectedEof)? {
                token!(Lexeme::Number(num), span) => {
                    if let Number::FixedInteger(i) = num.try_into()?
                        && let Ok(byte) = u8::try_from(i)
                    {
                        output.push(byte);
                        continue;
                    }
                    return Err(ParseSyntaxError::NonByte { span });
                }
                token!(Lexeme::RParen) => return Ok(Syntax::new_byte_vector(output, span)),
                token => {
                    return Err(ParseSyntaxError::NonByte { span: token.span });
                }
            }
        }
    }

    #[maybe_async]
    fn alias(&mut self, alias: &str, span: Span) -> Result<Syntax, ParseSyntaxError> {
        let expr = maybe_await!(self.get_sexpr())?;
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
    UnexpectedEof,
    ExpectedClosingParen { span: Span },
    UnexpectedClosingParen { span: Span },
    InvalidPeriodLocation { span: Span },
    NonByte { span: Span },
    UnclosedParen { span: Span },
    CharTryFrom(CharTryFromError),
    Lex(LexerError),
    ParseNumberError(ParseNumberError),
    UnexpectedToken { token: Box<Token> },
}

impl fmt::Display for ParseSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Self::EmptyInput => write!(f, "cannot parse an empty list"),
            Self::UnexpectedEof => write!(f, "unexpected end of file"),
            Self::ExpectedClosingParen { span } => {
                write!(f, "closing parenthesis not found at `{span}`")
            }
            Self::UnexpectedClosingParen { span } => {
                write!(f, "unexpected closing parenthesis found at `{span}`")
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
            Self::ParseNumberError(e) => write!(f, "{e:?}"),
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

impl From<ParseNumberError> for ParseSyntaxError {
    fn from(e: ParseNumberError) -> Self {
        Self::ParseNumberError(e)
    }
}
