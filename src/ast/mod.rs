mod eval;
pub mod parse;

use nom::error::ParseError;

use crate::{
    continuation::Continuation,
    env::Env,
    error::RuntimeError,
    expand::Transformer,
    gc::{Gc, Trace},
    num::Number,
    proc::ValuesOrPreparedCall,
    syntax::{ExpansionEnv, Identifier, Mark, Span, Syntax},
    util::ArcSlice,
    value::Value,
};
use std::sync::Arc;

#[derive(Debug, Clone, Trace)]
pub enum AstNode {
    Definition(Definition),
    Expression(Expression),
}

impl AstNode {
    pub async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        match self {
            Self::Definition(def) => {
                def.eval(env, cont).await?;
                Ok(Vec::new())
            }
            Self::Expression(expr) => expr.eval(env, cont).await,
        }
    }

    pub async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        match self {
            Self::Definition(def) => {
                def.eval(env, cont).await?;
                Ok(ValuesOrPreparedCall::Values(Vec::new()))
            }
            Self::Expression(expr) => expr.tail_eval(env, cont).await
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub enum Definition {
    DefineVar(DefineVar),
    DefineFunc(DefineFunc),
}

impl Definition {
    pub async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<(), RuntimeError> {
        todo!()
    }

    pub async fn parse(
        syn: &Syntax,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, parse::ParseAstError> {
        todo!()
    }

    pub fn wrap(self, envs: Vec<ExpansionEnv>) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone, Trace)]
pub struct DefineVar {
    pub name: Identifier,
    pub val: Arc<Expression>,
}

#[derive(Debug, Clone, Trace)]
pub struct DefineFunc {
    pub name: Identifier,
    pub args: Formals,
    pub body: Body,
}

#[derive(Debug, Clone, Trace)]
pub enum Expression {
    Literal(Literal),
    Quote(Quote),
    SyntaxQuote(SyntaxQuote),
    Call(Call),
    Lambda(Lambda),
    Set(Set),
    MacroExpansionPoint(MacroExpansionPoint),
}

impl Expression {
    pub async fn eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Vec<Gc<Value>>, RuntimeError> {
        match self {
            _ => todo!(),
        }
    }

    pub async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        todo!()
    }

    pub async fn parse(
        syn: &Syntax,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, parse::ParseAstError> {
        todo!()
    }

    fn wrap(self, envs: Vec<ExpansionEnv>) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Trace)]
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
    pub env: Env,
}

#[derive(Debug, Clone, Trace)]
pub struct Call {
    pub args: ArcSlice<Arc<Expression>>,
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
    pub bindings: Arc<[(Identifier, Arc<Expression>)]>,
    pub body: Body,
}

impl Let {
    pub fn new(bindings: Vec<(Identifier, Arc<Expression>)>, body: Body) -> Self {
        Self {
            bindings: Arc::from(bindings),
            body,
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Set {
    pub var: Identifier,
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
    FixedArgs(Vec<Identifier>),
    VarArgs {
        fixed: Vec<Identifier>,
        remaining: Identifier,
    },
}

impl Formals {
    pub fn to_args_and_remaining(&self) -> (Vec<Identifier>, Option<Identifier>) {
        match self {
            Self::VarArgs { fixed, remaining } => (fixed.clone(), Some(remaining.clone())),
            Self::FixedArgs(args) => (args.clone(), None),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Body {
    pub forms: ArcSlice<AstNode>,
}

impl Body {
    pub fn new(defs: Vec<Definition>, exprs: Vec<Expression>) -> Self {
        Self {
            forms: ArcSlice::from(defs
                .into_iter()
                .map(AstNode::Definition)
                .chain(exprs.into_iter().map(AstNode::Expression))
                .collect::<Vec<_>>()),
        }
    }
}

#[derive(Clone, Trace)]
pub struct And {
    pub args: ArcSlice<Expression>,
}

impl And {
    pub fn new(args: Vec<Expression>) -> Self {
        Self {
            args: ArcSlice::from(args),
        }
    }
}

#[derive(Clone, Trace)]
pub struct Or {
    pub args: ArcSlice<Expression>,
}

impl Or {
    pub fn new(args: Vec<Expression>) -> Self {
        Self {
            args: ArcSlice::from(args),
        }
    }
}

#[derive(Clone, Trace)]
pub struct Vector {
    pub vals: Vec<Literal>,
}

#[derive(Clone, Trace)]
pub struct SyntaxCase {
    pub arg: Arc<Expression>,
    pub transformer: Transformer,
}

#[derive(Clone, Trace)]
pub struct Var {
    pub ident: Identifier,
}

impl Var {
    pub fn new(ident: Identifier) -> Self {
        Self { ident }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct MacroExpansionPoint {
    pub mark: Mark,
    pub macro_env: Env,
    pub expr: Arc<Expression>,
}

impl MacroExpansionPoint {
    pub fn new(mark: Mark, macro_env: Env, expr: Arc<Expression>) -> Self {
        Self {
            mark,
            macro_env,
            expr,
        }
    }
}
