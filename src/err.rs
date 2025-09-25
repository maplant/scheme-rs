use scheme_rs::{
    ast::ParseAstError, exceptions::Exception, lex::LexError, parse::ParseSyntaxError,
};

use crate::{lex::InputSpan, string_builder::Builder, syntax::Span};

trait ErrRender<'err> {
    fn render_into(
        &self,
        input_lines: &'err [String],
        builder: &mut Builder,
        source_name: &'err str,
    );
}

#[derive(derive_more::From, Debug)]
pub enum EvalError<'e> {
    LexError(LexError<'e>),
    ParseError(ParseSyntaxError<'e>),
    ParseAstError(ParseAstError),
    Exception(Exception),
}

impl<'err> EvalError<'err> {
    pub fn render(
        &self,
        input_lines: &'err [String],
        builder: &mut Builder,
        source_name: &'err str,
    ) {
        match self {
            EvalError::LexError(lex_err) => lex_err.render_into(input_lines, builder, source_name),
            EvalError::ParseError(parse_error) => {
                parse_error.render_into(input_lines, builder, source_name)
            }
            EvalError::ParseAstError(parse_ast_error) => {
                parse_ast_error.render_into(input_lines, builder, source_name)
            }
            EvalError::Exception(exception) => builder.write_string(format!("{exception:}")),
        }
    }
}

#[derive(Debug)]
struct ErrorContext<'s> {
    msg: &'s str,
    line: Option<u32>,
    column: Option<usize>,
    end: Option<usize>,
    // TODO: how do we get the source?
}

impl<'err> ErrRender<'err> for LexError<'err> {
    // damn these errors suck, guess matthew gotta handroll his lexer so we can get some better
    // errors
    fn render_into(
        &self,
        input_lines: &'err [String],
        builder: &mut Builder,
        source_name: &'err str,
    ) {
        // TODO: nom::error::Error<_> has some sort of undocumented cause for lexer errors, i just
        // found 'Tag' - idk what that means; So i guess we gotta find out what they mean and put
        // this into here
        let ctx: ErrorContext = <(&str, Option<&InputSpan<'err>>) as std::convert::Into<
            ErrorContext<'_>,
        >>::into(("Lexer error", None));
        ctx.render(input_lines, builder, source_name);
    }
}

fn write_line(builder: &mut Builder, idx: usize, line: &str) {
    builder.write_str("\n");
    builder.write_int(idx);
    builder.write_str(" | ");
    builder.write_str(line);
    builder.write_str("\n");
}

impl<'s> ErrorContext<'s> {
    pub fn render(&self, input_lines: &'s [String], builder: &mut Builder, source_name: &'s str) {
        dbg!(self);
        // Error: <msg / description of the failure>\n
        builder.write_str("error: ");
        builder.write_str(self.msg);
        builder.write_char('\n');

        // ~> <source (can be stdin or a filename)>:<line>:<column>
        builder.write_str("  --> ");
        builder.write_str(source_name);
        if let Some(l) = self.line {
            builder.write_char(':');
            builder.write_int(l);
            // we only print the column if we also have the line
            if let Some(c) = self.column {
                builder.write_char(':');
                builder.write_int(c);
            }
        }

        builder.write_str("\n");
        let (line, col, end) = (
            self.line.unwrap_or_default() as usize,
            self.column.unwrap_or_default() as usize,
            self.end.unwrap_or_default() as usize,
        );

        if !input_lines.is_empty() {
            if let Some(prev1) = line.checked_sub(1).and_then(|l| input_lines.get(l)) {
                write_line(builder, line - 1, prev1);
            }

            if let Some(offender) = input_lines.get(line) {
                write_line(builder, line, offender);
            }
        }
    }
}

impl<'s, 'a> From<(&'s str, Option<&InputSpan<'a>>)> for ErrorContext<'s> {
    fn from(value: (&'s str, Option<&InputSpan<'a>>)) -> Self {
        if let Some(span) = value.1 {
            Self {
                msg: value.0,
                line: Some(span.location_line()),
                column: Some(span.location_offset()),
                end: Some(span.location_offset() + span.fragment().len()),
            }
        } else {
            Self {
                msg: value.0,
                line: None,
                column: None,
                end: None,
            }
        }
    }
}

impl<'s> From<(&'s str, Option<&Span>)> for ErrorContext<'s> {
    fn from(value: (&'s str, Option<&Span>)) -> Self {
        if let Some(span) = value.1 {
            Self {
                msg: value.0,
                line: Some(span.line),
                column: Some(span.column),
                end: Some(span.column - span.offset),
            }
        } else {
            Self {
                msg: value.0,
                line: None,
                column: None,
                end: None,
            }
        }
    }
}

impl<'err> ErrRender<'err> for ParseSyntaxError<'err> {
    fn render_into(
        &self,
        input_lines: &'err [String],
        builder: &mut Builder,
        source_name: &'err str,
    ) {
        let ctx: ErrorContext = match self {
            ParseSyntaxError::EmptyInput => ("Empty Input", None),
            ParseSyntaxError::UnexpectedEndOfFile => ("Unexpected EOF", None),
            ParseSyntaxError::ExpectedClosingParen { span } => {
                ("Expected Closing Paren", Some(span))
            }
            ParseSyntaxError::UnexpectedClosingParen { span } => {
                ("Unexpected Closing Paren", Some(span))
            }
            ParseSyntaxError::InvalidHexValue { span, .. } => ("Invalid Hex value", Some(span)),
            ParseSyntaxError::InvalidPeriodLocation { span } => {
                ("Invalid Period Location", Some(span))
            }
            ParseSyntaxError::NonByte { span } => ("Invalid Non Byte Input found", Some(span)),
            ParseSyntaxError::UnclosedParen { span } => ("Missing Closing Paren", Some(span)),
            ParseSyntaxError::CharTryFrom(_) => ("Invalid Character", None),
            ParseSyntaxError::TryFromInt(_) => ("Invaild Integer", None),
            ParseSyntaxError::TryFromNumber(_) => ("Invalid Number", None),
            ParseSyntaxError::UnexpectedToken { token } => ("Unexpected Token", Some(&token.span)),
            // this is a wrapped lexer error, we just propagate to the LexError rendering
            ParseSyntaxError::Lex(lex_error) => {
                return lex_error.render_into(input_lines, builder, source_name);
            }
        }
        .into();

        ctx.render(input_lines, builder, source_name);
    }
}

impl<'err> ErrRender<'err> for ParseAstError {
    fn render_into(
        &self,
        input_lines: &'err [String],
        builder: &mut Builder,
        source_name: &'err str,
    ) {
        let ctx: ErrorContext = match self {
            ParseAstError::BadForm(span) => ("Bad Form", Some(span)),
            _ => todo!("{:?}", self),
        }
        .into();

        ctx.render(input_lines, builder, source_name);
    }
}
