use crate::lexer::{Line, NumberGroup};
use std::{
    collections::{HashMap, hash_map},
    fmt::{Display, Formatter},
    num::ParseIntError,
    ops::Range,
    str::FromStr,
};

#[derive(Default)]
pub struct File<'a> {
    case_foldings: HashMap<u32, (Vec<u32>, &'a str)>,
    numeric_types: HashMap<Range<u32>, &'a str>,
}
impl<'a> TryFrom<Vec<Line<'a>>> for File<'a> {
    type Error = ParseIntError;
    fn try_from(lines: Vec<Line<'a>>) -> Result<Self, ParseIntError> {
        let mut out = Self::default();

        let (case_foldings, numeric_types): (Vec<_>, Vec<_>) = lines
            .into_iter()
            .filter(|line| matches!(line, Line::CaseFolding(_) | Line::NumericType(_)))
            .partition(|line| matches!(line, Line::CaseFolding(_)));

        case_foldings
            .into_iter()
            .map(|line| {
                let Line::CaseFolding(line) = line else {
                    unreachable!()
                };
                line
            })
            .map(|case_folding| {
                Ok((
                    u32::from_str_radix(case_folding.from, 16)?,
                    case_folding
                        .into
                        .split(' ')
                        .map(|c| u32::from_str_radix(c, 16))
                        .collect::<Result<Vec<_>, ParseIntError>>()?,
                    case_folding.name,
                ))
            })
            .collect::<Result<Vec<_>, ParseIntError>>()?
            .into_iter()
            // insert if the we have less characters in order to allow more conversions
            .for_each(|(from, into, name)| match out.case_foldings.entry(from) {
                hash_map::Entry::Occupied(mut e) => {
                    if e.get().0.len() <= into.len() {
                        return;
                    }
                    e.insert((into, name));
                }
                hash_map::Entry::Vacant(e) => {
                    e.insert((into, name));
                }
            });
        out.numeric_types.extend(
            numeric_types
                .into_iter()
                .map(|line| {
                    let Line::NumericType(line) = line else {
                        unreachable!()
                    };
                    line
                })
                .filter(|line| matches!(line.num_group, NumberGroup::Decimal))
                // all the decimals are complete
                .filter_map(|line| {
                    Some((line.lower_bound, line.upper_bound?, line.len?, line.name))
                })
                .map(|(lower_bound, upper_bound, len, name)| {
                    Ok((
                        u32::from_str_radix(lower_bound, 16)?,
                        u32::from_str_radix(upper_bound, 16)?,
                        usize::from_str(len)?,
                        name,
                    ))
                })
                .collect::<Result<Vec<_>, ParseIntError>>()?
                .into_iter()
                // split the decimals into ranges of 10
                .flat_map(|(mut lower_bound, mut upper_bound, mut len, name)| {
                    debug_assert!(len % 10 == 0, "all decimals should be a multiple of 10");

                    if len == 10 {
                        vec![(lower_bound..upper_bound, name)]
                    } else {
                        upper_bound = lower_bound + 10;
                        let mut ranges = Vec::with_capacity(len / 10);

                        while len >= 10 {
                            ranges.push((lower_bound..upper_bound, name));
                            lower_bound = upper_bound + 1;
                            upper_bound = lower_bound + 10;
                            len -= 10;
                        }

                        ranges
                    }
                }),
        );

        Ok(out)
    }
}
impl Display for File<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(
            f,
            "pub fn digit_to_num(ch: char) -> Option<u32> {{
    match ch {{"
        )?;
        self.numeric_types.iter().try_for_each(|(r, name)| {
            writeln!(
                f,
                "       '\\u{{{:x}}}'..'\\u{{{:x}}}' => Some(ch as u32 - {0}), // {}",
                r.start,
                r.end + 1,
                name
            )
        })?;
        writeln!(
            f,
            "       _ => None,
    }}
}}"
        )?;

        writeln!(
            f,
            "enum FoldcaseChars {{
    Unary(char),
    Variadic(&'static [char]),
}}
impl FoldcaseChars {{
    pub fn get(&self, index: usize) -> Option<char> {{
        match self {{
            Self::Unary(i) => if index == 0 {{ Some(*i) }} else {{ None }},
            Self::Variadic(i) => i.get(index).copied(),
        }}
    }}

    pub const fn len(&self) -> usize {{
        match self {{
            Self::Unary(_) => 1,
            Self::Variadic(i) => i.len(),
        }}
    }}
}}

pub struct ToFoldcase {{
    chars: FoldcaseChars,
    i: u8,
}}
impl Iterator for ToFoldcase {{
    type Item = char;
    fn next(&mut self) -> Option<char> {{
        let c = self.chars.get(self.i.into());
        self.i = self.i.saturating_add(1);
        c
    }}
}}
impl ExactSizeIterator for ToFoldcase {{
    fn len(&self) -> usize {{
        self.chars.len().saturating_sub(self.i.into())
    }}
}}
pub fn to_foldcase(ch: char) -> ToFoldcase {{
    let chars = match ch {{"
        )?;
        self.case_foldings
            .iter()
            .try_for_each(|(from, (to, name))| {
                debug_assert!(!to.is_empty());

                write!(f, "       '\\u{{{:x}}}' => ", from)?;
                if to.len() == 1 {
                    write!(
                        f,
                        "FoldcaseChars::Unary('\\u{{{:x}}}'),",
                        to.first().unwrap()
                    )?;
                } else {
                    write!(f, "FoldcaseChars::Variadic(&[ ")?;
                    to.iter()
                        .try_for_each(|t| write!(f, "'\\u{{{:x}}}', ", t))?;
                    write!(f, "]),")?;
                }
                writeln!(f, " // {}", name)?;

                Ok(())
            })?;
        writeln!(
            f,
            "       _ => FoldcaseChars::Unary(ch),
    }};

    ToFoldcase {{
        chars,
        i: 0,
    }}
}}"
        )?;

        Ok(())
    }
}
