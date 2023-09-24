use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, tag_no_case, take_until, take_while1},
    character::{
        complete::{char as match_char, hex_digit1, one_of, satisfy},
        streaming::anychar,
    },
    combinator::{map, opt, value, verify},
    multi::{fold_many0, fold_many1, many0},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use nom_locate::{position, LocatedSpan};
use std::{borrow::Cow, sync::Arc};
use unicode_categories::UnicodeCategories;

pub type InputSpan<'a> = LocatedSpan<&'a str, Arc<String>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lexeme<'a> {
    Identifier(Cow<'a, str>),
    Boolean(bool),
    Number(Cow<'a, str>),
    Character(Cow<'a, str>),
    String(Vec<Fragment<'a>>),
    LParen,
    RParen,
    LBracket,
    RBracket,
    HashParen,
    Vu8Paren,
    Quote,
    Tick,
    Comma,
    CommaAt,
    Period,
    HashQuote,
    HashTick,
    HashComma,
    HashCommaAt,
    DocComment(String),
}

impl Lexeme<'static> {
    fn identifier_owned(s: String) -> Self {
        Self::Identifier(Cow::Owned(s))
    }

    fn number_owned(s: String) -> Self {
        Self::Number(Cow::Owned(s))
    }

    fn string_owned(v: Vec<Fragment<'static>>) -> Self {
        Self::String(v)
    }
}

impl<'a> Lexeme<'a> {
    pub fn to_number(&self) -> &str {
        let Lexeme::Number(num) = self else {
            panic!("not a number");
        };
        num.as_ref()
    }

    pub fn to_boolean(&self) -> bool {
        let Lexeme::Boolean(b) = self else {
            panic!("not a boolean");
        };
        *b
    }

    pub fn to_ident(&self) -> &str {
        let Lexeme::Identifier(i) = self else {
            panic!("not an ident");
        };
        i.as_ref()
    }

    pub fn to_char(&self) -> &str {
        let Lexeme::Character(c) = self else {
            panic!("not a character");
        };
        c.as_ref()
    }

    pub fn to_string(&self) -> &[Fragment<'a>] {
        let Lexeme::String(s) = self else {
            panic!("not a string");
        };
        s.as_slice()
    }

    pub fn string(v: Vec<Fragment<'a>>) -> Self {
        Self::String(v)
    }
}

fn lexeme(i: InputSpan) -> IResult<InputSpan, Lexeme<'static>> {
    alt((
        map(identifier, Lexeme::identifier_owned),
        map(boolean, Lexeme::Boolean),
        map(string, Lexeme::string_owned),
        map(number, Lexeme::number_owned),
        map(doc_comment, Lexeme::DocComment),
        map(match_char('.'), |_| Lexeme::Period),
        map(match_char('\''), |_| Lexeme::Quote),
        map(match_char('('), |_| Lexeme::LParen),
        map(match_char(')'), |_| Lexeme::RParen),
        map(match_char('['), |_| Lexeme::LBracket),
        map(match_char(']'), |_| Lexeme::RBracket),
        map(tag("#("), |_| Lexeme::HashParen),
        map(tag("#'"), |_| Lexeme::HashTick),
    ))(i)
}

/*
fn comment(_i: InputSpan) -> IResult<InputSpan, ()> {
    todo!()
}
*/

fn whitespace(i: InputSpan) -> IResult<InputSpan, ()> {
    map(
        alt((satisfy(UnicodeCategories::is_separator), match_char('\n'))),
        |_| (),
    )(i)
}

fn atmosphere(i: InputSpan) -> IResult<InputSpan, ()> {
    map(tuple((whitespace,)), |_| ())(i)
}

fn interlexeme_space(i: InputSpan) -> IResult<InputSpan, ()> {
    fold_many0(atmosphere, || (), |_, _| ())(i)
}

fn identifier(i: InputSpan) -> IResult<InputSpan, String> {
    alt((
        map(tuple((initial, many0(subsequent))), |(i, s)| {
            format!("{i}{}", s.join(""))
        }),
        peculiar_identifier,
    ))(i)
}

fn boolean(i: InputSpan) -> IResult<InputSpan, bool> {
    alt((
        map(tag_no_case("#t"), |_| true),
        map(tag_no_case("#f"), |_| false),
    ))(i)
}

fn initial(i: InputSpan) -> IResult<InputSpan, String> {
    alt((
        map(satisfy(is_constituent), String::from),
        map(satisfy(is_special_initial), String::from),
        inline_hex_escape,
    ))(i)
}

fn subsequent(i: InputSpan) -> IResult<InputSpan, String> {
    alt((
        initial,
        map(satisfy(|c| c.is_ascii_digit()), String::from),
        map(
            satisfy(|c| {
                c.is_number_decimal_digit()
                    || c.is_mark_spacing_combining()
                    || c.is_mark_enclosing()
            }),
            String::from,
        ),
        map(special_subsequent, String::from),
    ))(i)
}

fn special_subsequent(i: InputSpan) -> IResult<InputSpan, char> {
    one_of("+-.@")(i)
}

fn peculiar_identifier(i: InputSpan) -> IResult<InputSpan, String> {
    alt((
        map(match_char('+'), |_| String::from("+")),
        map(match_char('-'), |_| String::from("-")),
        map(tag("..."), |_| String::from("...")),
        map(tuple((tag("->"), many0(subsequent))), |(_, subseq)| {
            format!("->{}", subseq.join(""))
        }),
    ))(i)
}

fn inline_hex_escape(i: InputSpan) -> IResult<InputSpan, String> {
    map(
        tuple((tag("\\x"), hex_scalar_value, match_char(';'))),
        |(_, value, _)| format!("\\x{value};"),
    )(i)
}

fn hex_scalar_value(i: InputSpan) -> IResult<InputSpan, InputSpan> {
    hex_digit1(i)
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

fn is_special_initial(c: char) -> bool {
    matches!(
        c,
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~'
    )
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Fragment<'a> {
    HexValue(Cow<'a, str>),
    Escaped(char),
    Unescaped(Cow<'a, str>),
}

fn string(i: InputSpan) -> IResult<InputSpan, Vec<Fragment<'static>>> {
    delimited(
        match_char('"'),
        many0(alt((
            preceded(
                match_char('\\'),
                alt((
                    map(
                        tuple((tag_no_case("x"), hex_scalar_value, match_char(';'))),
                        |(_, hex, _)| Fragment::HexValue(Cow::Owned(hex.to_string())),
                    ),
                    value(Fragment::Escaped('\u{07}'), match_char('a')),
                    value(Fragment::Escaped('\u{08}'), match_char('b')),
                    value(Fragment::Escaped('\t'), match_char('t')),
                    value(Fragment::Escaped('\n'), match_char('n')),
                    value(Fragment::Escaped('\u{0B}'), match_char('v')),
                    value(Fragment::Escaped('\u{0C}'), match_char('f')),
                    value(Fragment::Escaped('\r'), match_char('r')),
                    value(Fragment::Escaped('"'), match_char('"')),
                    value(Fragment::Escaped('\\'), match_char('\\')),
                    map(anychar, Fragment::Escaped),
                )),
            ),
            map(
                verify(is_not("\"\\"), |s: &InputSpan| !s.fragment().is_empty()),
                |s: InputSpan| Fragment::Unescaped(Cow::Owned(s.fragment().to_string())),
            ),
        ))),
        match_char('"'),
    )(i)
}

fn number(i: InputSpan) -> IResult<InputSpan, String> {
    map(
        tuple((
            opt(alt((
                tag_no_case("#b"),
                tag_no_case("#o"),
                tag_no_case("#d"),
                tag_no_case("#x"),
            ))),
            take_while1(|c: char| c.is_ascii_hexdigit()),
        )),
        |(radix, real): (Option<InputSpan>, InputSpan)| {
            format!("{}{real}", radix.map(|s| *s.fragment()).unwrap_or(""))
        },
    )(i)
}

fn doc_comment(i: InputSpan) -> IResult<InputSpan, String> {
    fold_many1(
        delimited(tag(";;"), take_until("\n"), many0(whitespace)),
        String::new,
        |mut comment, line| {
            comment.push_str(&line);
            comment.push('\n');
            comment
        },
    )(i)
}

#[derive(Debug)]
pub struct Token<'a> {
    pub lexeme: Lexeme<'static>,
    pub span: InputSpan<'a>,
}

pub type LexError<'a> = nom::Err<nom::error::Error<InputSpan<'a>>>;

impl<'a> Token<'a> {
    pub fn tokenize_file(s: &'a str, filename: &str) -> Result<Vec<Self>, LexError<'a>> {
        let mut span = InputSpan::new_extra(s, Arc::new(filename.to_string()));
        let mut output = Vec::new();
        while !span.is_empty() {
            let (remaining, ()) = interlexeme_space(span)?;
            if remaining.is_empty() {
                break;
            }
            let (remaining, curr_span) = position(remaining)?;
            let (remaining, lexeme) = lexeme(remaining)?;
            output.push(Token {
                lexeme,
                span: curr_span,
            });
            span = remaining;
        }
        Ok(output)
    }

    pub fn tokenize_str(s: &'a str) -> Result<Vec<Self>, LexError<'a>> {
        Self::tokenize_file(s, "<stdin>")
    }
}
