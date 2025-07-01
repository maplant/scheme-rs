use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{digit1, hex_digit1, line_ending, not_line_ending, space0},
    combinator::{map, opt},
    sequence::{delimited, preceded, terminated, tuple},
};

pub fn lex(src: &str) -> IResult<&str, Vec<Line<'_>>> {
    let mut lines = Vec::new();
    let mut src_p: &str = src;
    while !src_p.is_empty() {
        let out = Line::lex(src_p)?;
        src_p = out.0;
        lines.push(out.1);
    }

    Ok((src_p, lines))
}

#[derive(Debug)]
pub enum Line<'a> {
    Empty,
    CaseFolding(CaseFolding<'a>),
    NumericType(NumericType<'a>),
}
impl<'a> Line<'a> {
    pub fn lex(l: &'a str) -> IResult<&'a str, Self> {
        terminated(
            alt((
                map(CaseFolding::lex, Self::CaseFolding),
                map(NumericType::lex, Self::NumericType),
                map(
                    alt((
                        preceded(tag("#"), preceded(space0, not_line_ending)),
                        space0,
                    )),
                    |_| Self::Empty,
                ),
            )),
            line_ending,
        )(l)
    }
}

#[derive(Debug)]
pub struct CaseFolding<'a> {
    pub from: &'a str,
    pub _status: CaseFoldingStatus,
    pub into: &'a str,
    pub name: &'a str,
}
impl<'a> CaseFolding<'a> {
    pub fn lex(l: &'a str) -> IResult<&'a str, Self> {
        map(
            tuple((
                terminated(terminated(hex_digit1, tag(";")), space0),
                terminated(terminated(CaseFoldingStatus::lex, tag(";")), space0),
                terminated(
                    terminated(
                        take_while(|c: char| c.is_whitespace() || c.is_ascii_hexdigit()),
                        tag(";"),
                    ),
                    space0,
                ),
                preceded(tag("#"), preceded(space0, not_line_ending)),
            )),
            |(from, status, into, name)| Self {
                from,
                _status: status,
                into,
                name,
            },
        )(l)
    }
}

#[derive(Debug)]
pub enum CaseFoldingStatus {
    Common,
    Full,
    Simple,
    Turkic,
}
impl CaseFoldingStatus {
    pub fn lex(l: &str) -> IResult<&str, Self> {
        alt((
            map(tag("C"), |_| Self::Common),
            map(tag("F"), |_| Self::Full),
            map(tag("S"), |_| Self::Simple),
            map(tag("T"), |_| Self::Turkic),
        ))(l)
    }
}

#[derive(Debug)]
pub struct NumericType<'a> {
    pub lower_bound: &'a str,
    pub upper_bound: Option<&'a str>,
    pub num_group: NumberGroup,
    pub _char_group: CharGroup,
    pub len: Option<&'a str>,
    pub name: &'a str,
}
impl<'a> NumericType<'a> {
    pub fn lex(l: &'a str) -> IResult<&'a str, Self> {
        map(
            tuple((
                terminated(hex_digit1, space0),
                opt(terminated(preceded(tag(".."), hex_digit1), space0)),
                terminated(
                    preceded(preceded(tag(";"), space0), NumberGroup::lex),
                    space0,
                ),
                terminated(preceded(preceded(tag("#"), space0), CharGroup::lex), space0),
                terminated(opt(delimited(tag("["), digit1, tag("]"))), space0),
                not_line_ending,
            )),
            |(lower_bound, upper_bound, num_group, char_group, len, name)| Self {
                lower_bound,
                upper_bound,
                num_group,
                _char_group: char_group,
                len,
                name,
            },
        )(l)
    }
}

#[derive(Debug)]
pub enum NumberGroup {
    Decimal,
    Digit,
    Numeric,
}
impl NumberGroup {
    pub fn lex(l: &str) -> IResult<&str, Self> {
        alt((
            map(tag("Decimal"), |_| Self::Decimal),
            map(tag("Digit"), |_| Self::Digit),
            map(tag("Numeric"), |_| Self::Numeric),
        ))(l)
    }
}

#[derive(Debug)]
pub enum CharGroup {
    // number decimal
    Nd,
    /// number letter
    Nl,
    /// number other
    No,
    /// letter other
    Lo,
}
impl CharGroup {
    pub fn lex(l: &str) -> IResult<&str, Self> {
        alt((
            map(tag("Lo"), |_| Self::Lo),
            map(tag("Nd"), |_| Self::Nd),
            map(tag("Nl"), |_| Self::Nl),
            map(tag("No"), |_| Self::No),
        ))(l)
    }
}
