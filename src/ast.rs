use crate::{
    env::Env,
    eval::Eval,
    expand::Transformer,
    gc::Trace,
    num::Number,
    syntax::{Identifier, Mark, Span, Syntax},
    util::ArcSlice,
    value::Value,
};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Trace)]
pub struct ByteVector(pub Vec<u8>);

#[derive(Debug, Clone, PartialEq, Trace)]
pub enum Literal {
    Number(Number),
    Boolean(bool),
    Character(char),
    String(String),
    ByteVector(ByteVector),
}

#[derive(Clone, Trace)]
pub struct Quote {
    pub val: Value,
}

#[derive(Clone, Trace)]
pub struct SyntaxQuote {
    pub syn: Syntax,
    pub env: Env,
}

#[derive(Clone, Trace)]
pub struct Call {
    pub args: ArcSlice<Arc<dyn Eval>>,
    pub location: Span,
    pub proc_name: String,
}

#[derive(Clone, Trace)]
pub struct DefineFunc {
    pub name: Identifier,
    pub args: Formals,
    pub body: Body,
}

#[derive(Clone, Trace)]
pub struct DefineVar {
    pub name: Identifier,
    pub val: Arc<dyn Eval>,
}

#[derive(Clone, Trace, derive_more::Debug)]
pub enum Define {
    DefineVar(#[debug(skip)] DefineVar),
    DefineFunc(#[debug(skip)] DefineFunc),
}

#[derive(Clone, Trace)]
pub struct DefineSyntax;

#[derive(Clone, Trace)]
pub struct Lambda {
    pub args: Formals,
    pub body: Body,
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

#[derive(Clone, Trace)]
pub struct Body {
    pub exprs: ArcSlice<Arc<dyn Eval>>,
}

impl Body {
    pub fn new(exprs: Vec<Arc<dyn Eval>>) -> Self {
        Self {
            exprs: ArcSlice::from(exprs),
        }
    }
}

#[derive(Clone, Trace)]
pub struct Let {
    pub bindings: Arc<[(Identifier, Arc<dyn Eval>)]>,
    pub body: Body,
}

#[derive(Clone, Trace)]
pub struct Set {
    pub var: Identifier,
    pub val: Arc<dyn Eval>,
}

#[derive(Clone, Trace)]
pub struct If {
    pub cond: Arc<dyn Eval>,
    pub success: Arc<dyn Eval>,
    pub failure: Option<Arc<dyn Eval>>,
}

#[derive(Clone, Trace)]
pub struct And {
    pub args: ArcSlice<Arc<dyn Eval>>,
}

impl And {
    pub fn new(args: Vec<Arc<dyn Eval>>) -> Self {
        Self {
            args: ArcSlice::from(args),
        }
    }
}

#[derive(Clone, Trace)]
pub struct Or {
    pub args: ArcSlice<Arc<dyn Eval>>,
}

impl Or {
    pub fn new(args: Vec<Arc<dyn Eval>>) -> Self {
        Self {
            args: ArcSlice::from(args),
        }
    }
}

#[derive(Clone, Trace)]
pub struct Vector {
    pub vals: Vec<Arc<dyn Eval>>,
}

#[derive(Clone, Trace)]
pub struct SyntaxCase {
    pub arg: Arc<dyn Eval>,
    pub transformer: Transformer,
}

#[derive(Clone, Trace)]
pub struct SyntaxRules {
    pub transformer: Transformer,
}

#[derive(Clone, Trace)]
pub struct FetchVar {
    pub ident: Identifier,
}

impl FetchVar {
    pub fn new(ident: Identifier) -> Self {
        Self { ident }
    }
}

#[derive(Clone, Trace)]
pub struct MacroExpansionPoint {
    pub mark: Mark,
    pub macro_env: Env,
    pub expr: Arc<dyn Eval>,
}

impl MacroExpansionPoint {
    pub fn new(mark: Mark, macro_env: Env, expr: Arc<dyn Eval>) -> Self {
        Self {
            mark,
            macro_env,
            expr,
        }
    }
}

/*
struct Export {}

struct Import {}

struct Library {
    imports: Vec<Import>,
    exports: Vec<Export>,
    library_body: Body,
}

struct TopLevelProgram {
    imports: Vec<Import>,
    program_body: Body,
}
*/
