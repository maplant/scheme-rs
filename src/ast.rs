use crate::{
    eval::{Env, Eval, Value},
    expand::SyntaxRule,
    gc::Gc,
    num::Number,
};
use std::{
    fmt,
    hash::{Hash, Hasher},
};

#[derive(Clone)]
pub struct Ident {
    pub macro_env: Option<Gc<Env>>,
    pub sym: String,
}

impl Ident {
    pub fn new_free(sym: &str) -> Self {
        Self {
            macro_env: None,
            sym: sym.to_string(),
        }
    }

    pub fn new_macro(sym: &str, macro_env: &Gc<Env>) -> Self {
        Self {
            macro_env: Some(macro_env.clone()),
            sym: sym.to_string(),
        }
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.macro_env.is_some() {
            write!(f, "<macro>::{}", self.sym)
        } else {
            write!(f, "{}", self.sym)
        }
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.macro_env.as_ref().map(Gc::as_ptr).hash(state);
        self.sym.hash(state);
    }
}

impl PartialEq for Ident {
    fn eq(&self, rhs: &Ident) -> bool {
        self.macro_env.as_ref().map(Gc::as_ptr) == rhs.macro_env.as_ref().map(Gc::as_ptr)
            && self.sym == rhs.sym
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, rhs: &str) -> bool {
        self.sym == rhs
    }
}

impl Eq for Ident {}

#[derive(Clone)]
pub struct Ref {
    pub val: Gc<Value>,
}

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
pub struct Call {
    pub operator: Box<dyn Eval>,
    pub args: Vec<Box<dyn Eval>>,
}

#[derive(Clone)]
pub struct DefineFunc {
    pub name: Ident,
    pub args: Formals,
    pub body: Body,
}

#[derive(Clone)]
pub struct DefineVar {
    pub name: Ident,
    pub val: Box<dyn Eval>,
}

#[derive(Clone)]
pub enum Define {
    DefineVar(DefineVar),
    DefineFunc(DefineFunc),
}

#[derive(Clone)]
pub struct DefineSyntax {
    pub name: Ident,
    pub rules: Vec<SyntaxRule>,
}

#[derive(Clone)]
pub struct Lambda {
    pub args: Formals,
    pub body: Body,
}

#[derive(Debug, Clone)]
pub enum Formals {
    FixedArgs(Vec<Ident>),
    VarArgs { fixed: Vec<Ident>, remaining: Ident },
}

impl Formals {
    pub fn to_args_and_remaining(&self) -> (Vec<Ident>, Option<Ident>) {
        match self {
            Self::VarArgs { fixed, remaining } => (fixed.clone(), Some(remaining.clone())),
            Self::FixedArgs(args) => (args.clone(), None),
        }
    }
}

#[derive(Clone)]
pub struct Body {
    pub exprs: Vec<Box<dyn Eval>>,
}

impl Body {
    pub fn new(exprs: Vec<Box<dyn Eval>>) -> Self {
        Self { exprs }
    }
}

#[derive(Clone)]
pub struct Let {
    pub bindings: Vec<(Ident, Box<dyn Eval>)>,
    pub body: Body,
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
