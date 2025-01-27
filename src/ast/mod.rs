//! Code for parsing and linking Scheme code into a format that is instantly
//! evaluatable. The AST format is largely equivalent to a post-expansion
//! process version Scheme without significant lowering. The biggest change
//! is all variables scopes have been resolved.

pub mod parse;

use either::Either;

use crate::{
    cps::PrimOp,
    env::{Environment, Local, Top, Var},
    expand::Transformer,
    gc::Trace,
    num::Number,
    records::DefineRecordType,
    syntax::{Span, Syntax},
    value::Value,
};
use std::sync::Arc;

#[derive(Debug, Clone, Trace)]
pub enum AstNode {
    Definition(Definition),
    Expression(Expression),
}

impl AstNode {
    pub async fn from_syntax(
        syn: Syntax,
        env: &Environment<impl Top>,
    ) -> Result<Option<Self>, parse::ParseAstError> {
        match syn.as_list() {
            /*
            Some(
                [Syntax::Identifier { ident, .. }, Syntax::Identifier { ident: name, .. }, expr, Syntax::Null { .. }],
            ) if ident.name == "define-syntax" => {
                define_syntax(name.clone(), expr.clone(), &env /* cont */).await?;
                Ok(None)
            }
            */

            /*
            Some([Syntax::Identifier { ident, span, .. }, body @ .., Syntax::Null { .. }])
                if ident == "define-record-type" =>
            {
                let record_type = DefineRecordType::parse(body, env, span)?;
                record_type.define(&env.lexical_contour);
                Ok(Some(AstNode::Definition(Definition::DefineRecordType(
                    record_type,
                ))))
            }
            */
            Some([Syntax::Identifier { ident, span, .. }, ..]) if ident == "define-syntax" => {
                Err(parse::ParseAstError::BadForm(span.clone()))
            }
            Some(syn @ [Syntax::Identifier { ident, span, .. }, ..]) if ident == "define" => {
                Ok(Some(Self::Definition(
                    Definition::parse(syn, env, span /* cont */).await?,
                )))
            }
            _ => Ok(Some(Self::Expression(
                Expression::parse(syn, env /* cont */).await?,
            ))),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub enum Definition {
    DefineVar(DefineVar),
    DefineFunc(DefineFunc),
    DefineRecordType(DefineRecordType),
}

#[derive(Debug, Clone, Trace)]
pub struct DefineVar {
    pub var: Var,
    pub val: Arc<Expression>,
}

#[derive(Debug, Clone, Trace)]
pub struct DefineFunc {
    pub var: Var,
    pub args: Formals,
    pub body: Body,
}

#[derive(Debug, Clone, Trace)]
pub enum Expression {
    Undefined,
    Literal(Literal),
    Quote(Quote),
    SyntaxQuote(SyntaxQuote),
    SyntaxCase(SyntaxCase),
    Apply(Apply),
    Let(Let),
    If(If),
    And(And),
    Or(Or),
    Lambda(Lambda),
    Set(Set),
    Var(Var),
    Vector(Vector),
    Begin(Body),
}

#[derive(Debug, Clone, PartialEq, Trace)]
// Vector should be in here too. Oh well.
pub enum Literal {
    Number(Number),
    Boolean(bool),
    Character(char),
    String(String),
    ByteVector(Vec<u8>),
}

#[derive(Debug, Clone, Trace)]
pub struct Quote {
    pub val: Value,
}

#[derive(Debug, Clone, Trace)]
pub struct SyntaxQuote {
    pub syn: Syntax,
}

#[derive(Debug, Clone, Trace)]
pub struct Apply {
    pub operator: Either<Box<Expression>, PrimOp>,
    pub args: Vec<Expression>,
    pub location: Span,
    pub proc_name: String,
}

#[derive(Debug, Clone, Trace)]
pub struct Lambda {
    pub args: Formals,
    pub body: Body,
}

#[derive(Debug, Clone, Trace)]
pub struct Let {
    pub bindings: Vec<(Local, Expression)>,
    pub body: Body,
}

impl Let {
    pub fn new(bindings: Vec<(Local, Expression)>, body: Body) -> Self {
        Self { bindings, body }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Set {
    pub var: Var,
    pub val: Arc<Expression>,
}

#[derive(Debug, Clone, Trace)]
pub struct If {
    pub cond: Arc<Expression>,
    pub success: Arc<Expression>,
    pub failure: Option<Arc<Expression>>,
}

#[derive(Debug, Clone, Trace)]
pub enum Formals {
    FixedArgs(Vec<Local>),
    VarArgs { fixed: Vec<Local>, remaining: Local },
}

impl Formals {
    pub fn iter(&self) -> impl Iterator<Item = &'_ Local> {
        let fixed_iter = match self {
            Self::FixedArgs(fixed) => fixed.iter(),
            Self::VarArgs { fixed, .. } => fixed.iter(),
        };
        let remaining = match self {
            Self::FixedArgs(_) => None,
            Self::VarArgs { ref remaining, .. } => Some(remaining),
        };
        fixed_iter.chain(remaining)
    }

    pub fn is_variadic(&self) -> bool {
        matches!(self, Self::VarArgs { .. })
    }
}

/*
impl Formals {
    pub fn to_args_and_remaining(&self) -> (Vec<Identifier>, Option<Identifier>) {
        match self {
            Self::VarArgs { fixed, remaining } => (fixed.clone(), Some(remaining.clone())),
            Self::FixedArgs(args) => (args.clone(), None),
        }
    }

    pub fn args_to_alloc(&self) -> usize {
        match self {
            Self::VarArgs { fixed, .. } => fixed.len() + 1,
            Self::FixedArgs(args) => args.len(),
        }
    }
}
*/

#[derive(Debug, Clone, Trace)]
pub struct Body {
    pub forms: Vec<AstNode>,
}

impl Body {
    pub fn new(defs: Vec<Definition>, exprs: Vec<Expression>) -> Self {
        Self {
            forms: Vec::from(
                defs.into_iter()
                    .map(AstNode::Definition)
                    .chain(exprs.into_iter().map(AstNode::Expression))
                    .collect::<Vec<_>>(),
            ),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct And {
    pub args: Vec<Expression>,
}

impl And {
    pub fn new(args: Vec<Expression>) -> Self {
        Self { args }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Or {
    pub args: Vec<Expression>,
}

impl Or {
    pub fn new(args: Vec<Expression>) -> Self {
        Self { args }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Vector {
    pub vals: Vec<Value>,
}

#[derive(Debug, Clone, Trace)]
pub struct SyntaxCase {
    pub arg: Arc<Expression>,
    pub transformer: Transformer,
}
