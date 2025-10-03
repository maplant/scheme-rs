use crate::{
    ast::Literal,
    num::Number,
    ports::{Port, ReadError},
    syntax::lex::ParseNumberError,
};

pub use super::lex::LexerError;
use super::{
    Span, Syntax,
    lex::{Character, Lexeme, Lexer, Token},
};
use futures::future::BoxFuture;
use std::{char::CharTryFromError, error::Error as StdError, fmt};

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
    pub async fn new(input_port: &'a Port) -> Self {
        Parser {
            lookahead: Vec::new(),
            lexer: Lexer::new(input_port).await,
        }
    }
}

impl Parser<'_> {
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
                token!(Lexeme::LParen, span) => Ok(Some(self.list(span, Lexeme::RParen).await?)),
                token!(Lexeme::LBracket, span) => {
                    Ok(Some(self.list(span, Lexeme::RBracket).await?))
                }

                // Vectors:
                token!(Lexeme::HashParen, span) => Ok(Some(self.vector(span).await?)),
                token!(Lexeme::Vu8Paren, span) => Ok(Some(self.byte_vector(span).await?)),

                // Various aliases:
                token!(Lexeme::Quote, span) => Ok(Some(self.alias("quote", span).await?)),
                token!(Lexeme::Backquote, span) => Ok(Some(self.alias("quasiquote", span).await?)),
                token!(Lexeme::Comma, span) => Ok(Some(self.alias("unquote", span).await?)),
                token!(Lexeme::CommaAt, span) => {
                    Ok(Some(self.alias("unquote-splicing", span).await?))
                }
                token!(Lexeme::HashQuote, span) => Ok(Some(self.alias("syntax", span).await?)),
                token!(Lexeme::HashBackquote, span) => {
                    Ok(Some(self.alias("quasisyntax", span).await?))
                }
                token!(Lexeme::HashComma, span) => Ok(Some(self.alias("unsyntax", span).await?)),
                token!(Lexeme::HashCommaAt, span) => {
                    Ok(Some(self.alias("unsyntax-splicing", span).await?))
                }

                // Datum comments:
                token!(Lexeme::DatumComment) => {
                    // Discard next expression:
                    let _ = self.expression().await?;
                    Ok(None)
                }

                // Handle some erroneous situations:
                token!(Lexeme::RParen, span) | token!(Lexeme::RBracket, span) => {
                    return Err(ParseSyntaxError::UnexpectedClosingParen { span });
                }

                token!(Lexeme::Period, span) => {
                    return Err(ParseSyntaxError::InvalidPeriodLocation { span });
                }
            }
        })
    }

    pub async fn get_datum(&mut self) -> Result<Syntax, ParseSyntaxError> {
        loop {
            if let Some(expr) = self.expression().await? {
                return Ok(expr);
            }
        }
    }

    pub async fn all_datums(&mut self) -> Result<Vec<Syntax>, ParseSyntaxError> {
        let mut datums = Vec::new();
        loop {
            match self.get_datum().await {
                Ok(datum) => datums.push(datum),
                Err(ParseSyntaxError::Lex(LexerError::ReadError(ReadError::Eof))) => {
                    return Ok(datums);
                }
                Err(err) => return Err(err),
            }
        }
    }

    async fn list(&mut self, span: Span, closing: Lexeme) -> Result<Syntax, ParseSyntaxError> {
        match self.next_token().await? {
            // We allow for (. expr) to resolve to expr, just because it's
            // easier. Maybe we'll disallow this eventualy
            token!(Lexeme::Period) => return self.get_datum().await,
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
                    output.push(self.get_datum().await?);
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

    async fn byte_vector(&mut self, span: Span) -> Result<Syntax, ParseSyntaxError> {
        let mut output = Vec::new();
        loop {
            match self.next_token().await? {
                token!(Lexeme::Number(num), span) => {
                    if let Number::FixedInteger(i) = num.try_into()? {
                        if let Ok(byte) = u8::try_from(i) {
                            output.push(byte);
                            continue;
                        }
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

    async fn alias(&mut self, alias: &str, span: Span) -> Result<Syntax, ParseSyntaxError> {
        let expr = self.get_datum().await?;
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
    // UnexpectedEndOfFile,
    ExpectedClosingParen { span: Span },
    UnexpectedClosingParen { span: Span },
    InvalidPeriodLocation { span: Span },
    NonByte { span: Span },
    UnclosedParen { span: Span },
    CharTryFrom(CharTryFromError),
    Lex(LexerError),
    ParseNumberError(ParseNumberError),
    UnexpectedToken { token: Token },
}

impl fmt::Display for ParseSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Self::EmptyInput => write!(f, "cannot parse an empty list"),
            // Self::UnexpectedEndOfFile => write!(f, "unexpected end of file"),
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
