use crate::{
    ast::{Ident, Literal},
    lex::{Fragment, Lexeme, Span, Token},
    num::Number,
    sexpr::SExpr,
};
use nom::{
    branch::alt,
    combinator::{map, map_res},
    multi::many1,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use rug::Integer;

#[derive(Debug)]
pub enum ParseError<'a> {
    EmptyInput,
    UnexpectedEndOfFile,
    ExpectedClosingParen { span: Span<'a> },
    ParseNumberError { value: String, span: Span<'a> },
    InvalidHexValue { value: String, span: Span<'a> },
    InvalidDocCommentLocation { span: Span<'a> },
    InvalidPeriodLocation { span: Span<'a> },
    UnclosedParen { span: Span<'a> },
    DocCommentMustPrecedeDefine,
}

impl<'a> ParseError<'a> {
    fn invalid_period(token: &Token<'a>) -> Self {
        Self::InvalidPeriodLocation {
            span: token.span.clone(),
        }
    }

    fn invalid_doc_comment(token: &Token<'a>) -> Self {
        Self::InvalidDocCommentLocation {
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

pub fn expression<'a>(i: &'a [Token<'a>]) -> Result<(&'a [Token<'a>], SExpr), ParseError<'a>> {
    match i {
        // Calling expression with an empty list is an error
        [] => Err(ParseError::EmptyInput),
        // Literals:
        [b @ token!(Lexeme::Boolean(_)), tail @ ..] => {
            Ok((tail, SExpr::new_literal(boolean(b)?, b.span.clone())))
        }
        [n @ token!(Lexeme::Number(_)), tail @ ..] => {
            Ok((tail, SExpr::new_literal(number(n)?, n.span.clone())))
        }
        [s @ token!(Lexeme::String(_)), tail @ ..] => {
            Ok((tail, SExpr::new_literal(string(s)?, s.span.clone())))
        }
        // Identifiers:
        [i @ token!(Lexeme::Identifier(_)), tail @ ..] => Ok((
            tail,
            SExpr::new_identifier(Ident::new_free(i.lexeme.to_ident()), i.span.clone()),
        )),
        // Lists:
        [p @ token!(Lexeme::LParen), tail @ ..] => match list(tail, p.span.clone()) {
            Err(ParseListError::UnclosedParen) => Err(ParseError::unclosed_paren(p)),
            Err(ParseListError::ParseError(err)) => Err(err),
            Ok(ok) => Ok(ok),
        },
        // Invalid locations:
        [d @ token!(Lexeme::Period), ..] => Err(ParseError::invalid_period(d)),
        [d @ token!(Lexeme::DocComment(_)), ..] => Err(ParseError::invalid_doc_comment(d)),
        x => todo!("Not implemented: {x:#?}"),
    }
}

#[derive(Debug)]
enum ParseListError<'a> {
    UnclosedParen,
    ParseError(ParseError<'a>),
}

impl<'a> From<ParseError<'a>> for ParseListError<'a> {
    fn from(pe: ParseError<'a>) -> Self {
        Self::ParseError(pe)
    }
}

fn list<'a>(
    mut i: &'a [Token<'a>],
    span: Span<'a>,
) -> Result<(&'a [Token<'a>], SExpr<'a>), ParseListError<'a>> {
    let mut output = Vec::new();
    loop {
        if i.is_empty() {
            return Err(ParseListError::UnclosedParen);
        }

        let (remaining, expr) = expression(i)?;

        output.push(expr);

        match remaining {
            [end @ token!(Lexeme::RParen), tail @ ..]
            | [token!(Lexeme::Period), end @ token!(Lexeme::LParen), token!(Lexeme::RParen), token!(Lexeme::RParen), tail @ ..] =>
            {
                // Proper list
                output.push(SExpr::new_nil(end.span.clone()));
                return Ok((tail, SExpr::new_list(output, span)));
            }
            [token!(Lexeme::Period), tail @ ..] => {
                // Improper list
                let (remaining, expr) = expression(tail)?;
                output.push(expr);
                return match remaining {
                    [] => Err(ParseListError::ParseError(ParseError::UnexpectedEndOfFile)),
                    [token!(Lexeme::RParen), tail @ ..] => {
                        Ok((tail, SExpr::new_list(output, span)))
                    }
                    [unexpected, ..] => Err(ParseListError::ParseError(
                        ParseError::ExpectedClosingParen {
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

/*
pub fn list<'a>(i: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], SExpr> {
    enum ListEnding {
        Proper,
        Improper(SExpr),
    }

    map(
        tuple((
            many1(expression),
            alt((
                map(
                    tuple((
                        token!(Lexeme::Period),
                        token!(Lexeme::LParen),
                        token!(Lexeme::RParen),
                    )),
                    |_| ListEnding::Proper,
                ),
                map(
                    delimited(token!(Lexeme::Period), expression, token!(Lexeme::RParen)),
                    ListEnding::Improper,
                ),
                map(token!(Lexeme::RParen), |_| ListEnding::Proper),
            )),
        )),
        |(mut expressions, ending)| {
            match ending {
                ListEnding::Proper => expressions.push(SExpr::Nil),
                ListEnding::Improper(SExpr::List(tail)) => expressions.extend(tail),
                ListEnding::Improper(tail) => expressions.push(tail),
            }
            SExpr::List(expressions)
        },
    )(i)
}
*/

fn boolean<'a>(i: &Token<'a>) -> Result<Literal, ParseError<'a>> {
    Ok(Literal::Boolean(i.lexeme.to_boolean()))
}

fn number<'a>(i: &Token<'a>) -> Result<Literal, ParseError<'a>> {
    let number = i.lexeme.to_number();
    // TODO: Parse correctly
    let number: Integer = number.parse().unwrap();
    Ok(Literal::Number(Number::Integer(number)))
}

fn string<'a>(i: &Token<'a>) -> Result<Literal, ParseError<'a>> {
    let fragments = i.lexeme.to_string();
    let mut output = String::new();
    for fragment in fragments {
        match fragment {
            Fragment::Escaped(c) => output.push(*c),
            Fragment::Unescaped(s) => output.push_str(s),
            Fragment::HexValue(hex) => {
                let Ok(hex_value) = u32::from_str_radix(hex, 16) else {
                    return Err(ParseError::InvalidHexValue {
                        value: hex.to_string(),
                        span: i.span.clone(),
                    });
                };
                let Some(c) = char::from_u32(hex_value) else {
                    return Err(ParseError::InvalidHexValue {
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

/*
macro_rules! ident {
    ( $s:literal ) => {
        $crate::parse::satisfy(|lex| lex == &Lexeme::Identifier(std::borrow::Cow::Borrowed($s)))
    };
}

enum ParseError {
    ParseNumberError,
    InvalidHexValue(String),
}

pub fn expression<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], Expression> {
    println!("expression: {i:#?}");
    alt((
        map_res(token!(Lexeme::Number(_)), number),
        map_res(token!(Lexeme::Boolean(_)), boolean),
        map_res(token!(Lexeme::String(_)), string),
        map_res(token!(Lexeme::Identifier(_)), variable_ref),
        map(lambda, Expression::Lambda),
        map(let_expr, Expression::Let),
        map(define_var, Expression::DefVar),
        map(define_func, Expression::DefFunc),
        map(if_expr, Expression::If),
        map(body, Expression::Body),
        map(and, Expression::And),
        map(or, Expression::Or),
        map(call, Expression::Call),
    ))(i)
}

fn number(i: &Lexeme<'_>) -> Result<Expression, ParseError> {
    let number = i.to_number();
    // TODO: Parse correctly
    let number: Integer = number.parse().unwrap();
    Ok(Expression::Literal(Literal::Number(Number::Integer(
        number,
    ))))
}

fn boolean(i: &Lexeme<'_>) -> Result<Expression, ParseError> {
    Ok(Expression::Literal(Literal::Boolean(i.to_boolean())))
}

fn string(i: &Lexeme<'_>) -> Result<Expression, ParseError> {
    let fragments = i.to_string();
    let mut output = String::new();
    for fragment in fragments {
        match fragment {
            Fragment::Escaped(c) => output.push(*c),
            Fragment::Unescaped(s) => output.push_str(s),
            Fragment::HexValue(hex) => {
                let Ok(hex_value) = u32::from_str_radix(hex, 16) else {
                    return Err(ParseError::InvalidHexValue(hex.to_string()));
                };
                let Some(c) = char::from_u32(hex_value) else {
                    return Err(ParseError::InvalidHexValue(hex.to_string()));
                };
                output.push(c);
            }
        }
    }
    Ok(Expression::Literal(Literal::String(output)))
}

fn character(i: &Lexeme<'_>) -> Result<Expression, ParseError> {
    todo!()
}

fn variable_ref(i: &Lexeme<'_>) -> Result<Expression, ParseError> {
    Ok(Expression::VariableRef(Ident::from_lexeme(i)))
}

fn body<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], Body> {
    delimited(
        token!(Lexeme::LParen),
        preceded(ident!("begin"), map(many0(expression), Body::new)),
        token!(Lexeme::RParen),
    )(i)
}

pub fn define_func<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], DefineFunc> {
    map(
        delimited(
            token!(Lexeme::LParen),
            tuple((
                preceded(
                    ident!("define"),
                    delimited(
                        token!(Lexeme::LParen),
                        tuple((
                            map(token!(Lexeme::Identifier(_)), Ident::from_lexeme),
                            map(
                                many0(map(token!(Lexeme::Identifier(_)), Ident::from_lexeme)),
                                args_to_formals,
                            ),
                        )),
                        token!(Lexeme::RParen),
                    ),
                ),
                map(many0(expression), Body::new),
            )),
            token!(Lexeme::RParen),
        ),
        |((name, args), body)| DefineFunc { name, args, body },
    )(i)
}

pub fn define_var<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], Box<DefineVar>> {
    println!("define_var: {i:#?}");
    map(
        delimited(
            token!(Lexeme::LParen),
            preceded(
                ident!("define"),
                tuple((
                    map(token!(Lexeme::Identifier(_)), Ident::from_lexeme),
                    expression,
                )),
            ),
            token!(Lexeme::RParen),
        ),
        |(name, val)| Box::new(DefineVar { name, val }),
    )(i)
}

fn lambda<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], Lambda> {
    println!("lambda: {i:#?}");
    map(
        delimited(
            token!(Lexeme::LParen),
            preceded(
                alt((ident!("lambda"), ident!("λ"))),
                tuple((
                    alt((
                        map(
                            delimited(
                                token!(Lexeme::LParen),
                                many0(map(token!(Lexeme::Identifier(_)), Ident::from_lexeme)),
                                token!(Lexeme::RParen),
                            ),
                            args_to_formals,
                        ),
                        map(token!(Lexeme::Identifier(_)), |ident| {
                            Formals::VarArgs(Ident::from_lexeme(ident))
                        }),
                    )),
                    many0(expression),
                )),
            ),
            token!(Lexeme::RParen),
        ),
        |(args, exprs)| Lambda {
            args,
            body: Body::new(exprs),
        },
    )(i)
}

fn args_to_formals(args: Vec<Ident>) -> Formals {
    if args.len() > 2 && args[args.len() - 2] == "." {
        let fixed: Vec<_> = args[..args.len() - 2].iter().cloned().collect();
        let remaining = args.last().unwrap().clone();
        Formals::AtLeastN { fixed, remaining }
    } else {
        Formals::FixedArgs(args)
    }
}

fn let_expr<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], Let> {
    map(
        delimited(
            token!(Lexeme::LParen),
            preceded(
                ident!("let"),
                tuple((
                    delimited(
                        token!(Lexeme::LParen),
                        many0(delimited(
                            token!(Lexeme::LParen),
                            tuple((
                                map(token!(Lexeme::Identifier(_)), Ident::from_lexeme),
                                expression,
                            )),
                            token!(Lexeme::RParen),
                        )),
                        token!(Lexeme::RParen),
                    ),
                    many0(expression),
                )),
            ),
            token!(Lexeme::RParen),
        ),
        |(bindings, body)| Let {
            bindings,
            body: Body::new(body),
        },
    )(i)
}

fn if_expr<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], Box<If>> {
    map(
        delimited(
            token!(Lexeme::LParen),
            preceded(ident!("if"), tuple((expression, expression, expression))),
            token!(Lexeme::RParen),
        ),
        |(cond, success, failure)| {
            Box::new(If {
                cond,
                success,
                failure,
            })
        },
    )(i)
}

fn and<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], And> {
    delimited(
        token!(Lexeme::LParen),
        preceded(ident!("and"), map(many0(expression), And::new)),
        token!(Lexeme::RParen),
    )(i)
}

fn or<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], Or> {
    delimited(
        token!(Lexeme::LParen),
        preceded(ident!("or"), map(many0(expression), Or::new)),
        token!(Lexeme::RParen),
    )(i)
}

fn call<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], Box<Call>> {
    map(
        delimited(
            token!(Lexeme::LParen),
            tuple((expression, many0(expression))),
            token!(Lexeme::RParen),
        ),
        |(operator, args)| Box::new(Call { operator, args }),
    )(i)
}
*/
use nom::error::Error;
use nom::error::ErrorKind;
use nom::Err;

pub fn satisfy(
    cond: impl Fn(&Lexeme<'_>) -> bool,
) -> impl for<'a> Fn(&'a [Token<'a>]) -> IResult<&'a [Token<'a>], &'a Token<'a>> {
    move |i: &[Token<'_>]| match i.iter().next().map(|t| (cond(&t.lexeme), t)) {
        Some((true, t)) => Ok((&i[1..], t)),
        _ => Err(Err::Error(Error::new(i, ErrorKind::Satisfy))),
    }
}
