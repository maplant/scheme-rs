use scheme_rs::{ast::ParseAstError, exception::Exception, lex::LexError, parse::ParseSyntaxError};

use crate::{lex::InputSpan, string_builder::Builder};

trait ErrRender<'err> {
    fn render_into(&self, builder: &mut Builder, source_name: &'err str);
}

#[derive(derive_more::From, Debug)]
pub enum EvalError<'e> {
    LexError(LexError<'e>),
    ParseError(ParseSyntaxError<'e>),
    ParseAstError(ParseAstError),
    Exception(Exception),
}

impl<'err> EvalError<'err> {
    pub fn render(&self, builder: &mut Builder, source_name: &'err str) {
        match self {
            EvalError::LexError(lex_err) => lex_err.render_into(builder, source_name),
            EvalError::ParseError(parse_error) => parse_error.render_into(builder, source_name),
            // TODO: impl the trait here too
            EvalError::ParseAstError(parse_ast_error) => {
                builder.write_string(format!("{parse_ast_error:?}"))
            }
            EvalError::Exception(exception) => builder.write_string(format!("{exception:}")),
        }
    }
}

impl<'err> ErrRender<'err> for LexError<'err> {
    // damn these errors suck, guess matthew gotta handroll his lexer so we can get some better
    // errors
    fn render_into(&self, builder: &mut Builder, source_name: &'err str) {
        builder.write_str("-> ");
        builder.write_str(source_name);
        // TODO: match on source.code and find out what these codes even mean
        builder.write_string(format!("{self:?}"))
    }
}

struct ErrorContext<'s> {
    msg: &'s str,
    line: Option<u32>,
    column: Option<usize>,
    end: Option<usize>,
    // TODO: how do we get the source?
}

impl<'s> ErrorContext<'s> {
    pub fn render(&self, builder: &mut Builder, source_name: &'s str) {
        // Error: <msg / description of the failure>\n
        builder.write_str("Error: ");
        builder.write_str(self.msg);
        builder.write_char('\n');

        // ~> <source (can be stdin or a filename)>:<line>:<column>
        builder.write_str("~> ");
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
        builder.write_char('\n')
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

impl<'err> ErrRender<'err> for ParseSyntaxError<'err> {
    fn render_into(&self, builder: &mut Builder, source_name: &'err str) {
        let ctx: ErrorContext = match self {
            ParseSyntaxError::EmptyInput => ("Empty Input", None),
            ParseSyntaxError::UnexpectedEndOfFile => ("Unexpected EOF", None),
            ParseSyntaxError::ExpectedClosingParen { span } => {
                ("Unexpected Closing Paren", Some(span))
            }
            ParseSyntaxError::UnexpectedClosingParen { span } => {
                ("Unexpected Closig Paren", Some(span))
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
            ParseSyntaxError::Lex(lex_error) => return lex_error.render_into(builder, source_name),
        }
        .into();

        ctx.render(builder, source_name);
    }
}
