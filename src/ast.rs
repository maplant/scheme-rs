use crate::lex::Lexeme;

pub struct Ident(pub String);

impl Ident {
    pub fn from_lexeme(l: &Lexeme<'_>) -> Self {
        Self(l.to_ident().to_string())
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, rhs: &str) -> bool {
        self.0 == rhs
    }
}

pub enum Number {}

struct ByteVector(pub Vec<u8>);

pub enum Literal {
    Number(Number),
    Boolean(bool),
    Character(char),
    String(String),
    ByteVector(ByteVector),
}

pub struct Call {
    pub operator: Expression,
    pub args: Vec<Expression>,
}

pub enum Expression {
    Literal(Literal),
    VariableRef(Ident),
    Call(Box<Call>),
    Lambda(Lambda),
}

pub struct DefineFunc {
    pub name: Ident,
    pub args: Vec<Ident>,
    pub body: Body,
}

pub struct DefineVar {
    pub name: Ident,
    pub val: Expression,
}

pub struct Lambda {
    pub args: Formals,
    pub body: Body,
}

pub enum Formals {
    FixedArgs(Vec<Ident>),
    VarArgs(Ident),
    AtleastN {
        fixed: Vec<Ident>,
        remaining: Vec<Ident>,
    },
}

pub struct Body {
    exprs: Vec<Expression>,
}

impl Body {
    pub fn new(exprs: Vec<Expression>) -> Self {
        Self { exprs }
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
