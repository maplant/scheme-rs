use crate::{
    ast::{Body, Call, DefineFunc, DefineVar, Expression, Formals, Ident, Lambda, Literal, Number},
    lex::{Fragment, Lexeme},
};
use nom::{
    branch::alt,
    combinator::{map, map_res},
    multi::many0,
    sequence::{delimited, preceded, tuple},
    IResult,
};

macro_rules! ident {
    ( $s:literal ) => {
        token!(Lexeme::Identifier(std::borrow::Cow::Borrowed($s)))
    };
}

macro_rules! token {
    ( $pattern:pat $(if $guard:expr)? $(,)? ) => {
        $crate::parse::satisfy(|lex| matches!(lex, $pattern $(if $guard)?))
    }
}

enum ParseError {
    ParseNumberError,
    InvalidHexValue(String),
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
                            many0(map(token!(Lexeme::Identifier(_)), Ident::from_lexeme)),
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

pub fn define_var<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], DefineVar> {
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
        |(name, val)| DefineVar { name, val },
    )(i)
}

pub fn expression<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], Expression> {
    alt((
        map_res(token!(Lexeme::Number(_)), number),
        map_res(token!(Lexeme::Boolean(_)), boolean),
        map_res(token!(Lexeme::String(_)), string),
        map_res(token!(Lexeme::Identifier(_)), variable_ref),
        map(lambda, Expression::Lambda),
        map(call, Expression::Call),
    ))(i)
}

fn number(i: &Lexeme<'_>) -> Result<Expression, ParseError> {
    let number = i.to_number();
    // TODO: Parse
    Ok(Expression::Literal(Literal::Number(todo!())))
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

fn lambda<'a>(i: &'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], Lambda> {
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
                            |args| {
                                //                        if args.len() > 2 && args[args.len() - 2] == "." {
                                //                            let fixed = args[..args.len() - 2].iter().cloned
                                Formals::FixedArgs(todo!())
                            },
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

pub fn satisfy(
    cond: impl Fn(&Lexeme<'_>) -> bool,
) -> impl for<'a> Fn(&'a [Lexeme<'a>]) -> IResult<&'a [Lexeme<'a>], &'a Lexeme<'a>> {
    move |i: &[Lexeme<'_>]| match i.iter().next().map(|t| (cond(t), t)) {
        Some((true, t)) => Ok((&i[1..], t)),
        _ => Err(todo!()),
    }
}
