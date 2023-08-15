use crate::{lex::Lexeme, num::Number};

#[derive(Debug, Clone)]
pub struct Ident(pub String);

impl Ident {
    pub fn from_lexeme(l: &Lexeme<'_>) -> Self {
        Self(l.to_ident().to_string())
    }
}

impl PartialEq<&'_ str> for Ident {
    fn eq(&self, rhs: &&str) -> bool {
        self.0 == *rhs
    }
}

#[derive(Debug, Clone)]
pub struct ByteVector(pub Vec<u8>);

#[derive(Debug, Clone)]
pub enum Literal {
    Number(Number),
    Boolean(bool),
    Character(char),
    String(String),
    ByteVector(ByteVector),
}

#[derive(Debug, Clone)]
pub struct Call {
    pub operator: Expression,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    VariableRef(Ident),
    Call(Box<Call>),
    DefVar(Box<DefineVar>),
    DefFunc(DefineFunc),
    And(And),
    Or(Or),
    If(Box<If>),
    Body(Body),
    Lambda(Lambda),
    Let(Let),
}

#[derive(Debug, Clone)]
pub struct DefineFunc {
    pub name: Ident,
    pub args: Formals,
    pub body: Body,
}

#[derive(Debug, Clone)]
pub struct DefineVar {
    pub name: Ident,
    pub val: Expression,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub args: Formals,
    pub body: Body,
}

#[derive(Debug, Clone)]
pub enum Formals {
    FixedArgs(Vec<Ident>),
    VarArgs(Ident),
    AtLeastN { fixed: Vec<Ident>, remaining: Ident },
}

impl Formals {
    pub fn to_args_and_remaining(&self) -> (Vec<Ident>, Option<Ident>) {
        match self {
            Self::AtLeastN { fixed, remaining } => (fixed.clone(), Some(remaining.clone())),
            Self::VarArgs(name) => (Vec::new(), Some(name.clone())),
            Self::FixedArgs(args) => (args.clone(), None),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Body {
    pub exprs: Vec<Expression>,
}

impl Body {
    pub fn new(exprs: Vec<Expression>) -> Self {
        Self { exprs }
    }
}

#[derive(Debug, Clone)]
pub struct Let {
    pub bindings: Vec<(Ident, Expression)>,
    pub body: Body,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Expression,
    pub success: Expression,
    pub failure: Expression,
}

#[derive(Debug, Clone)]
pub struct And {
    pub args: Vec<Expression>,
}

impl And {
    pub fn new(args: Vec<Expression>) -> Self {
        Self { args }
    }
}

#[derive(Debug, Clone)]
pub struct Or {
    pub args: Vec<Expression>,
}

impl Or {
    pub fn new(args: Vec<Expression>) -> Self {
        Self { args }
    }
}

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
