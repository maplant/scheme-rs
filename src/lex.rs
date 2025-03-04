use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, tag_no_case, take, take_while, take_while1},
    character::{
        complete::{char as match_char, hex_digit1, one_of, satisfy},
        streaming::anychar,
    },
    combinator::{map, not, opt, value, verify},
    multi::{fold_many0, many0},
    sequence::{delimited, preceded, tuple},
    IResult, Parser,
};
use nom_locate::{position, LocatedSpan};
use std::{borrow::{Borrow, Cow}, error::Error as StdError, fmt, sync::Arc};
use unicode_categories::UnicodeCategories;

pub type InputSpan<'a> = LocatedSpan<&'a str, Arc<String>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lexeme<'a> {
    Identifier(String),
    Boolean(bool),
    Number(NumberLexeme<'a>),
    Character(Character),
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
}

//impl Lexeme<'static> {
//    fn identifier_owned(s: String) -> Self {
//        Self::Identifier(Cow::Owned(s))
//    }
//
//    fn number_owned<'a>(num: NumberLexeme<'a>) -> Self {
//        todo!()
//        //Self::Number(NumberLexeme {
//        //    radix: num.radix,
//        //    negative: num.negative,
//        //    contents: num.contents,
//        //})
//    }
//
//    fn string_owned(v: Vec<Fragment<'static>>) -> Self {
//        Self::String(v)
//    }
//}

impl<'a> Lexeme<'a> {
    pub fn to_number(&self) -> &str {
        todo!()
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

    pub fn to_char(&self) -> &Character {
        let Lexeme::Character(c) = self else {
            panic!("not a character");
        };
        c
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

fn lexeme<'a>(i: InputSpan<'a>) -> IResult<InputSpan<'a>, Lexeme<'a>> {
    alt((
        map(character, Lexeme::Character),
        map(identifier, Lexeme::Identifier),
        map(boolean, Lexeme::Boolean),
        //map(string, Lexeme::String),
        map(number, Lexeme::Number),
        map(match_char('.'), |_| Lexeme::Period),
        map(match_char('\''), |_| Lexeme::Quote),
        map(match_char('('), |_| Lexeme::LParen),
        map(match_char(')'), |_| Lexeme::RParen),
        map(match_char('['), |_| Lexeme::LBracket),
        map(match_char(']'), |_| Lexeme::RBracket),
        map(tag("#("), |_| Lexeme::HashParen),
        map(tag("#u8("), |_| Lexeme::Vu8Paren),
        map(tag("#'"), |_| Lexeme::HashTick),
    ))(i)
}

fn character(i: InputSpan) -> IResult<InputSpan, Character> {
    preceded(
        tag("#\\"),
        alt((
            map(
                alt((
                    map(tag_no_case("alarm"), |_| EscapedCharacter::Alarm),
                    map(tag_no_case("backspace"), |_| EscapedCharacter::Backspace),
                    map(tag_no_case("delete"), |_| EscapedCharacter::Delete),
                    map(tag_no_case("escape"), |_| EscapedCharacter::Escape),
                    map(tag_no_case("newline"), |_| EscapedCharacter::Newline),
                    map(tag_no_case("null"), |_| EscapedCharacter::Null),
                    map(tag_no_case("return"), |_| EscapedCharacter::Return),
                    map(tag_no_case("space"), |_| EscapedCharacter::Space),
                    map(tag_no_case("tab"), |_| EscapedCharacter::Tab),
                )),
                Character::Escaped,
            ),
            preceded(
                tag_no_case("x"),
                map(hex_digit1, |s: InputSpan| Character::Unicode(s.to_string())),
            ),
            map(anychar, Character::Literal),
        )),
    )(i)
}

fn comment(i: InputSpan) -> IResult<InputSpan, ()> {
    map(
        delimited(tag(";"), take_while(|x| x != '\n'), many0(whitespace)),
        |_| (),
    )(i)
}

fn nested_comment(i: InputSpan) -> IResult<InputSpan, ()> {
    delimited(
        tag("#|"),
        fold_many0(
            alt((
                nested_comment,
                map(not(tag("|#")).and(take(1_usize)), |_| ()),
            )),
            || (),
            |_, _| (),
        ),
        tag("|#"),
    )(i)
}

fn whitespace(i: InputSpan) -> IResult<InputSpan, ()> {
    map(
        alt((
            satisfy(UnicodeCategories::is_separator),
            match_char('\t'),
            match_char('\n'),
        )),
        |_| (),
    )(i)
}

fn atmosphere(i: InputSpan) -> IResult<InputSpan, ()> {
    map(tuple((whitespace,)), |_| ())(i)
}

fn interlexeme_space(i: InputSpan) -> IResult<InputSpan, ()> {
    fold_many0(alt((atmosphere, comment, nested_comment)), || (), |_, _| ())(i)
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

fn number<'a>(i: InputSpan<'a>) -> IResult<InputSpan<'a>, NumberLexeme<'a>> {
    map::<InputSpan<'a>, (u32, bool, InputSpan<'a>), NumberLexeme<'a>, _, _, _>(
        tuple((
            map(
                opt(alt((
                    map(tag_no_case("#b"), |_| 2),
                    map(tag_no_case("#o"), |_| 8),
                    map(tag_no_case("#d"), |_| 10),
                    map(tag_no_case("#x"), |_| 16),
                ))),
                |radix: Option<u32>| radix.unwrap_or(10),
            ),
            map(opt(tag("-")), |negative| negative.is_some()),
            take_while1(|c: char| c.is_ascii_hexdigit())
        )),
        |(radix, negative, contents)| NumberLexeme::new(radix, negative, &contents),
    )(i)
}

/*
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
*/

#[derive(Debug)]
pub struct Token<'a> {
    pub lexeme: Lexeme<'a>,
    pub span: InputSpan<'a>,
}

pub type LexError<'a> = nom::Err<nom::error::Error<InputSpan<'a>>>;

impl<'a> Token<'a> {
    pub fn tokenize(s: &'a str, file_name: Option<&str>) -> Result<Vec<Self>, LexError<'a>> {
        let mut span =
            InputSpan::new_extra(s, Arc::new(file_name.unwrap_or("<stdin>").to_string()));
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumberLexeme<'a> {
    radix: u32,
    negative: bool,
    contents: &'a str,
}
impl<'a> NumberLexeme<'a> {
    pub const fn new(radix: u32, negative: bool, contents: &'a str) -> Self {
        Self {
            radix,
            negative,
            contents,
        }
    }
}
macro_rules! impl_try_into_number_lexeme_for {
    ($ty:ty) => {
        impl<'a> TryFrom<NumberLexeme<'a>> for $ty {
            type Error = TryFromNumberLexemeError<'a>;
            fn try_from(num: NumberLexeme<'a>) -> Result<Self, TryFromNumberLexemeError<'a>> {
                num.contents.chars()
                    .map(|digit_char| digit_char
                        .to_digit(num.radix)
                        .ok_or_else(|| TryFromNumberLexemeError::new(num.contents, TryFromNumberLexemeErrorKind::InvalidDigit(num.radix))))
                    .fold(Ok::<$ty, TryFromNumberLexemeError<'a>>(0), |number, new_digit| {
                        let overflow_err = || TryFromNumberLexemeError::new(&num.contents, TryFromNumberLexemeErrorKind::Overflow);

                        number?
                            .checked_mul(num.radix as $ty)
                            .ok_or_else(overflow_err)?
                            .checked_add(new_digit? as $ty)
                            .ok_or_else(overflow_err)
                    })
                    .map(|full_num| if num.negative {
                        -full_num
                    } else {
                        full_num
                    })
            }
        }
    };
    ($($ty:ty),* $(,)?) => {
        $(impl_try_into_number_lexeme_for!($ty);)*
    }
}
// We cannot use on `u.` types since the number may be negative.
impl_try_into_number_lexeme_for! [
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
];
#[derive(Debug, PartialEq)]
pub struct TryFromNumberLexemeError<'a> {
    num: &'a str,
    kind: TryFromNumberLexemeErrorKind,
}
impl<'a> TryFromNumberLexemeError<'a> {
    const fn new(num: &'a str, kind: TryFromNumberLexemeErrorKind) -> Self {
        Self {
            num,
            kind,
        }
    }
}
impl fmt::Display for TryFromNumberLexemeError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invalid number `{}`: {}", self.num, self.kind)
    }
}
impl StdError for TryFromNumberLexemeError<'_> {}
#[derive(Debug, PartialEq)]
enum TryFromNumberLexemeErrorKind {
    Overflow,
    /// Contains radix
    InvalidDigit(u32),
}
impl fmt::Display for TryFromNumberLexemeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidDigit(radix) => write!(f, "invalid digit of radix `{}`", radix),
            Self::Overflow => write!(f, "number too large"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number_lexeme_to_num() {
        assert_eq!(
            <NumberLexeme<'_> as TryInto<i32>>::try_into(NumberLexeme::new(10, false, "123")),
            Ok(123_i32),
        );
        assert_eq!(
            <NumberLexeme<'_> as TryInto<i64>>::try_into(NumberLexeme::new(16, false, "DEADBEEF")),
            Ok(0xDEADBEEF_i64),
        );
    }
    #[test]
    fn number_lexeme_errors() {
        assert_eq!(
            <NumberLexeme<'_> as TryInto<i8>>::try_into(NumberLexeme::new(10, false, "9001")),
            Err(TryFromNumberLexemeError::new("9001", TryFromNumberLexemeErrorKind::Overflow)),
        );
        assert_eq!(
            <NumberLexeme<'_> as TryInto<i8>>::try_into(NumberLexeme::new(10, false, "foo bar")),
            Err(TryFromNumberLexemeError::new("foo bar", TryFromNumberLexemeErrorKind::InvalidDigit(10))),
        );
    }
}
