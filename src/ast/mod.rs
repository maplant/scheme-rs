mod eval;
pub mod parse;

use parse::define_syntax;

use crate::{
    continuation::Continuation,
    env::{Env, ExpansionEnv, Ref},
    error::RuntimeError,
    expand::Transformer,
    gc::{Gc, Trace},
    num::Number,
    proc::ValuesOrPreparedCall,
    syntax::{FullyExpanded, Identifier, Span, Syntax},
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
        env: &Gc<Env>,
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
        env: &Gc<Env>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        match self {
            Self::Definition(def) => {
                def.eval(env, cont).await?;
                Ok(ValuesOrPreparedCall::Values(Vec::new()))
            }
            Self::Expression(expr) => expr.tail_eval(env, cont).await,
        }
    }

    pub async fn from_syntax(
        syn: Syntax,
        env: &Gc<Env>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Option<Self>, parse::ParseAstError> {
        let expansion_env = ExpansionEnv::from_env(env);
        let FullyExpanded {
            expanded,
            expansion_ctxs: expansion_envs,
        } = syn.expand(&expansion_env, cont).await?;
        let expansion_env = expansion_env.push_expansion_env(expansion_envs);
        Self::from_syntax_with_expansion_env(expanded, &expansion_env, cont).await
    }

    async fn from_syntax_with_expansion_env(
        syn: Syntax,
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Option<Self>, parse::ParseAstError> {
        match syn.as_list() {
            Some(
                [Syntax::Identifier { ident, .. }, Syntax::Identifier { ident: name, .. }, expr, Syntax::Null { .. }],
            ) if ident.name == "define-syntax" => {
                // println!("defining here {}", ident.name);
                // TODO: Error if define syntax isn't proper, error.
                // println!("mod.rs:80");
                define_syntax(name, expr.clone(), &env.lexical_contour, cont).await?;
                Ok(None)
            }
            Some([Syntax::Identifier { ident, span, .. }, ..]) if ident.name == "define-syntax" => {
                return Err(parse::ParseAstError::BadForm(span.clone()));
            }
            Some(syn @ [Syntax::Identifier { ident, span, .. }, ..]) if ident.name == "define" => {
                Ok(Some(Self::Definition(
                    Definition::parse(syn, env, cont, span).await?,
                )))
            }
            _ => Ok(Some(Self::Expression(
                Expression::parse(syn, env, cont).await?,
            ))),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub enum Definition {
    DefineVar(DefineVar),
    DefineFunc(DefineFunc),
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
    Undefined,
    Literal(Literal),
    Quote(Quote),
    SyntaxQuote(SyntaxQuote),
    SyntaxCase(SyntaxCase),
    Call(Call),
    Let(Let),
    If(If),
    And(And),
    Or(Or),
    Lambda(Lambda),
    Set(Set),
    Var(Ref),
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
pub struct Call {
    pub args: ArcSlice<Expression>,
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
    pub bindings: Arc<[(Identifier, Expression)]>,
    pub body: Body,
}

impl Let {
    pub fn new(bindings: Vec<(Identifier, Expression)>, body: Body) -> Self {
        Self {
            bindings: Arc::from(bindings),
            body,
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Set {
    pub var: Ref,
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
            forms: ArcSlice::from(
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
    pub args: ArcSlice<Expression>,
}

impl And {
    pub fn new(args: Vec<Expression>) -> Self {
        Self {
            args: ArcSlice::from(args),
        }
    }
}

#[derive(Debug, Clone, Trace)]
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

#[derive(Debug, Clone, Trace)]
pub struct Vector {
    pub vals: Vec<Value>,
}

#[derive(Debug, Clone, Trace)]
pub struct SyntaxCase {
    pub arg: Arc<Expression>,
    pub transformer: Transformer,
}
