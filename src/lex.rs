use malachite::{rational::Rational, Integer, Natural};
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
use std::{
    borrow::Cow,
    fmt,
    num::ParseFloatError,
    ops::{Add, Mul, Neg},
    sync::Arc,
};
use unicode_categories::UnicodeCategories;

pub type InputSpan<'a> = LocatedSpan<&'a str, Arc<String>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lexeme<'a> {
    Identifier(String),
    Boolean(bool),
    Number(Number<'a>),
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

impl<'a> Lexeme<'a> {
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

fn lexeme(i: InputSpan) -> IResult<InputSpan, Lexeme> {
    alt((
        map(character, Lexeme::Character),
        map(number, Lexeme::Number),
        map(identifier, Lexeme::Identifier),
        map(boolean, Lexeme::Boolean),
        map(string, Lexeme::String),
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

fn string(i: InputSpan) -> IResult<InputSpan, Vec<Fragment>> {
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

fn number(i: InputSpan<'_>) -> IResult<InputSpan<'_>, Number<'_>> {
    let (remaining, number) = number_inner(i.clone())?;
    if remaining
        .chars()
        .next()
        .map(is_valid_numeric_char)
        .unwrap_or(false)
    {
        return Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Alt,
        )));
    }
    Ok((remaining, number))
}

fn number_inner<'a>(i: InputSpan<'a>) -> IResult<InputSpan<'a>, Number<'a>> {
    macro_rules! gen_radix_parser {
        ($head:expr, $radix:expr) => {
            tuple((
                tag_no_case($head).map(|_| $radix),
                opt(match_char('-')).map(|neg| neg.is_some()),
                take_while1(|c: char| c.is_digit($radix)),
            ))
        };
    }

    // Special parsing for decimal followed by number, so for example: -.5
    let (i, frac) = opt(tuple((
        opt(match_char('-')).map(|neg| neg.is_some()),
        match_char('.'),
        take_while1(|c: char| c.is_ascii_digit()),
    )))(i)?;
    if let Some((negative, _, frac)) = frac {
        return Ok((
            i,
            Number {
                radix: 10,
                negative,
                integer_or_numerator: "0",
                fractional_or_denominator: Some((FractionalOrDenominator::Fractional, *frac)),
            },
        ));
    }

    let (remaining, mut number) =
        map::<InputSpan<'a>, (u32, bool, InputSpan<'a>), Number<'a>, _, _, _>(
            alt((
                gen_radix_parser!("#b", 2),
                gen_radix_parser!("#o", 8),
                gen_radix_parser!("#x", 16),
                tuple((
                    opt(tag_no_case("#d")).map(|_| 10),
                    opt(match_char('-')).map(|neg| neg.is_some()),
                    take_while1(|c: char| c.is_ascii_digit()),
                )),
            )),
            |(radix, neg, contents)| Number::new(radix, neg, &contents),
        )(i)?;

    if number.radix == 10 {
        let (remaining, fractional_or_denominator) = opt(tuple((
            alt((
                match_char('.').map(|_| FractionalOrDenominator::Fractional),
                match_char('/').map(|_| FractionalOrDenominator::Denominator),
            )),
            take_while(|c: char| c.is_ascii_digit()),
        )))(remaining)?;
        number.fractional_or_denominator = fractional_or_denominator.map(|(fd, cnts)| (fd, *cnts));
        Ok((remaining, number))
    } else {
        Ok((remaining, number))
    }
}

fn is_valid_numeric_char(ch: char) -> bool {
    ch.is_ascii_digit() || ch == '-' || ch == '/' || ch == '.' || is_constituent(ch)
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Number<'a> {
    radix: u32,
    negative: bool,
    integer_or_numerator: &'a str,
    fractional_or_denominator: Option<(FractionalOrDenominator, &'a str)>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FractionalOrDenominator {
    Fractional,
    Denominator,
}

impl<'a> Number<'a> {
    pub const fn new(radix: u32, negative: bool, integer_or_numerator: &'a str) -> Self {
        Self {
            radix,
            negative,
            integer_or_numerator,
            fractional_or_denominator: None,
        }
    }
}

macro_rules! impl_try_into_number_lexeme_for {
    ($ty:ty) => {
        impl<'a> TryFrom<Number<'a>> for $ty {
            type Error = TryFromNumberError;
            fn try_from(num: Number<'a>) -> Result<Self, TryFromNumberError> {
                if num.fractional_or_denominator.is_some() {
                    return Err(TryFromNumberError::InvalidDigit);
                }
                num.integer_or_numerator.chars()
                    .map(|digit_char| digit_char
                        .to_digit(num.radix)
                        .ok_or(TryFromNumberError::InvalidDigit))
                    .try_fold(0 as $ty, |number, new_digit| {
                        number
                            .checked_mul(num.radix as $ty)
                            .ok_or(TryFromNumberError::Overflow)?
                            .checked_add(new_digit? as $ty)
                            .ok_or(TryFromNumberError::Overflow)
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
impl_try_into_number_lexeme_for![i8, i16, i32, i64, i128, isize];

impl<'a> TryFrom<Number<'a>> for Integer {
    type Error = TryFromNumberError;
    fn try_from(num: Number<'a>) -> Result<Self, TryFromNumberError> {
        if num.fractional_or_denominator.is_some() {
            return Err(TryFromNumberError::InvalidDigit);
        }
        num.integer_or_numerator
            .chars()
            .map(|digit| {
                digit
                    .to_digit(num.radix)
                    .ok_or(TryFromNumberError::InvalidDigit)
            })
            .try_fold(Integer::default(), |number, digit| {
                Ok(number
                    .mul(Integer::from(num.radix))
                    .add(Integer::from(digit?)))
            })
            .map(|number| if num.negative { number.neg() } else { number })
    }
}

impl<'a> TryFrom<Number<'a>> for Rational {
    type Error = TryFromNumberError;

    fn try_from(num: Number<'a>) -> Result<Self, TryFromNumberError> {
        let Some((FractionalOrDenominator::Denominator, denominator)) =
            num.fractional_or_denominator
        else {
            return Err(TryFromNumberError::InvalidDigit);
        };

        if num.radix != 10 {
            return Err(TryFromNumberError::InvalidRadix);
        }

        if denominator.is_empty() {
            return Err(TryFromNumberError::EmptyDenominator);
        }

        let numerator: Natural = num
            .integer_or_numerator
            .parse()
            .map_err(|_| TryFromNumberError::Overflow)?;
        let denominator: Natural = denominator
            .parse()
            .map_err(|_| TryFromNumberError::Overflow)?;

        Ok(Rational::from_sign_and_naturals(
            // true is positive for malachite
            !num.negative,
            numerator,
            denominator,
        ))
    }
}

impl<'a> TryFrom<Number<'a>> for f64 {
    type Error = TryFromNumberError;

    fn try_from(num: Number<'a>) -> Result<Self, TryFromNumberError> {
        let Some((FractionalOrDenominator::Fractional, frac)) = num.fractional_or_denominator
        else {
            return Err(TryFromNumberError::InvalidDigit);
        };

        if num.radix != 10 {
            return Err(TryFromNumberError::InvalidRadix);
        }

        let negative = num.negative;
        let num = format!("{}.{frac}", num.integer_or_numerator);
        let num: f64 = num.parse()?;

        if negative {
            Ok(-num)
        } else {
            Ok(num)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TryFromNumberError {
    Overflow,
    InvalidDigit,
    InvalidRadix,
    ParseFloatError(ParseFloatError),
    EmptyDenominator,
}

impl fmt::Display for TryFromNumberError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidDigit => write!(f, "invalid digit"),
            Self::Overflow => write!(f, "number too large"),
            Self::InvalidRadix => write!(f, "invalid radix"),
            Self::ParseFloatError(_) => write!(f, "cannot parse float"),
            Self::EmptyDenominator => write!(f, "empty denominator"),
        }
    }
}

impl From<ParseFloatError> for TryFromNumberError {
    fn from(value: ParseFloatError) -> Self {
        Self::ParseFloatError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number_lexeme_to_num() {
        assert_eq!(
            <Number<'_> as TryInto<i32>>::try_into(Number::new(10, false, "123")),
            Ok(123_i32),
        );
        assert_eq!(
            <Number<'_> as TryInto<i64>>::try_into(Number::new(16, false, "DEADBEEF")),
            Ok(0xDEADBEEF_i64),
        );
    }
    #[test]
    fn number_lexeme_errors() {
        assert_eq!(
            <Number<'_> as TryInto<i8>>::try_into(Number::new(10, false, "9001")),
            Err(TryFromNumberError::Overflow)
        );
        assert_eq!(
            <Number<'_> as TryInto<i8>>::try_into(Number::new(10, false, "foo bar")),
            Err(TryFromNumberError::InvalidDigit),
        );
    }
}
