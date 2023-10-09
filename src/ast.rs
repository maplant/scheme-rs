use crate::{
    env::{Env, LexicalContour},
    eval::Eval,
    expand::Transformer,
    gc::Gc,
    num::Number,
    syntax::{Identifier, Mark, Span, Syntax},
    util::ArcSlice,
    value::Value,
};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub struct ByteVector(pub Vec<u8>);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(Number),
    Boolean(bool),
    Character(char),
    String(String),
    ByteVector(ByteVector),
}

#[derive(Clone)]
pub struct Quote {
    pub val: Value,
}

#[derive(Clone)]
pub struct SyntaxQuote {
    pub syn: Syntax,
    pub env: Env,
}

#[derive(Clone)]
pub struct Call {
    //    pub operator: Arc<dyn Eval>,
    pub args: ArcSlice<Arc<dyn Eval>>,
    pub location: Span,
    pub proc_name: String,
}

#[derive(Clone)]
pub struct DefineFunc {
    pub name: Identifier,
    pub args: Formals,
    pub mark: Mark,
    pub body: Body,
}

#[derive(Clone)]
pub struct DefineVar {
    pub name: Identifier,
    pub val: Arc<dyn Eval>,
}

#[derive(Clone)]
pub enum Define {
    DefineVar(DefineVar),
    DefineFunc(DefineFunc),
}

#[derive(Clone)]
pub struct DefineSyntax {
    pub name: Identifier,
    pub transformer: Arc<dyn Eval>,
}

#[derive(Clone)]
pub struct Lambda {
    pub args: Formals,
    pub body: Body,
    pub mark: Mark,
}

#[derive(Debug, Clone)]
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

#[derive(Clone, Debug)]
pub struct Body {
    pub exprs: ArcSlice<Syntax>,
}

impl Body {
    pub fn new(exprs: Vec<Syntax>) -> Self {
        Self {
            exprs: ArcSlice::from(exprs),
        }
    }
}

#[derive(Clone)]
pub struct Let {
    pub scope: Gc<LexicalContour>,
    pub bindings: Arc<[(Identifier, Arc<dyn Eval>)]>,
    pub body: Body,
}

#[derive(Clone)]
pub struct Set {
    pub var: Identifier,
    pub val: Arc<dyn Eval>,
}

#[derive(Clone)]
pub struct If {
    pub cond: Arc<dyn Eval>,
    pub success: Arc<dyn Eval>,
    pub failure: Option<Arc<dyn Eval>>,
}

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
pub struct Vector {
    pub vals: Vec<Arc<dyn Eval>>,
}

#[derive(Clone)]
pub struct Nil;

#[derive(Clone)]
pub struct SyntaxCase {
    pub arg: Arc<dyn Eval>,
    pub transformer: Transformer,
}

#[derive(Clone)]
pub struct SyntaxRules {
    pub transformer: Transformer,
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
