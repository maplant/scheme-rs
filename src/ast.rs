use crate::{
    env::{Env, LexicalContour},
    eval::{Eval, Value},
    expand::SyntaxRule,
    gc::Gc,
    num::Number,
    syntax::{Identifier, Syntax},
};

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
    pub operator: Box<dyn Eval>,
    pub args: Vec<Box<dyn Eval>>,
}

#[derive(Clone)]
pub struct DefineFunc {
    pub name: Identifier,
    pub args: Formals,
    pub body: Body,
}

#[derive(Clone)]
pub struct DefineVar {
    pub name: Identifier,
    pub val: Box<dyn Eval>,
}

#[derive(Clone)]
pub enum Define {
    DefineVar(DefineVar),
    DefineFunc(DefineFunc),
}

#[derive(Clone)]
pub struct DefineSyntax {
    pub name: Identifier,
    pub rules: Vec<SyntaxRule>,
}

#[derive(Clone)]
pub struct Lambda {
    pub args: Formals,
    pub body: Body,
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

#[derive(Clone)]
pub struct Body {
    pub exprs: Vec<Syntax>,
}

impl Body {
    pub fn new(exprs: Vec<Syntax>) -> Self {
        Self { exprs }
    }
}

#[derive(Clone)]
pub struct Let {
    pub scope: Gc<LexicalContour>,
    pub bindings: Vec<(Identifier, Box<dyn Eval>)>,
    pub body: Body,
}

#[derive(Clone)]
pub struct Set {
    pub var: Identifier,
    pub val: Box<dyn Eval>,
}

#[derive(Clone)]
pub struct If {
    pub cond: Box<dyn Eval>,
    pub success: Box<dyn Eval>,
    pub failure: Option<Box<dyn Eval>>,
}

#[derive(Clone)]
pub struct And {
    pub args: Vec<Box<dyn Eval>>,
}

impl And {
    pub fn new(args: Vec<Box<dyn Eval>>) -> Self {
        Self { args }
    }
}

#[derive(Clone)]
pub struct Or {
    pub args: Vec<Box<dyn Eval>>,
}

impl Or {
    pub fn new(args: Vec<Box<dyn Eval>>) -> Self {
        Self { args }
    }
}

#[derive(Clone)]
pub struct Vector {
    pub vals: Vec<Box<dyn Eval>>,
}

#[derive(Clone)]
pub struct Nil;

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
