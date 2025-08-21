use scheme_rs::{ast::ParseAstError, exception::Exception, lex::LexError, parse::ParseSyntaxError};

#[derive(derive_more::From, Debug)]
pub enum EvalError<'e> {
    LexError(LexError<'e>),
    ParseError(ParseSyntaxError<'e>),
    ParseAstError(ParseAstError),
    Exception(Exception),
}
