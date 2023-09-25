use crate::{
    ast::{self, Literal},
    compile::{Compile, CompileError},
    env::Env,
    eval::{Eval, RuntimeError, Value},
    gc::Gc,
    lex::{InputSpan, Lexeme, Token},
    parse::ParseError,
};
use futures::future::BoxFuture;
use std::{collections::BTreeSet, fmt, sync::Arc};

#[derive(Debug, Clone)]
pub struct Span {
    pub line: u32,
    pub column: usize,
    pub offset: usize,
    pub file: Arc<String>,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

impl From<InputSpan<'_>> for Span {
    fn from(span: InputSpan<'_>) -> Self {
        Span {
            line: span.location_line(),
            column: span.get_column(),
            offset: span.location_offset(),
            file: span.extra.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Syntax {
    /// An empty list.
    Nil {
        span: Span,
    },
    /// A nested grouping of pairs. If the expression is a proper list, then the
    /// last element of expression will be Nil. This vector is guaranteed to contain
    /// at least two elements.
    List {
        list: Vec<Syntax>,
        span: Span,
    },
    Vector {
        vector: Vec<Syntax>,
        span: Span,
    },
    Literal {
        literal: Literal,
        span: Span,
    },
    Identifier {
        ident: Identifier,
        span: Span,
    },
}

impl Syntax {
    pub fn mark(&mut self, mark: Mark) {
        match self {
            Self::List { ref mut list, .. } => {
                for item in list {
                    item.mark(mark);
                }
            }
            Self::Vector { ref mut vector, .. } => {
                for item in vector {
                    item.mark(mark);
                }
            }
            Self::Identifier { ident, .. } => ident.mark(mark),
            _ => (),
        }
    }

    pub fn strip_unused_marks<'a>(&'a mut self, env: &'a Env) -> BoxFuture<'a, ()> {
        Box::pin(async move {
            match self {
                Self::List { ref mut list, .. } => {
                    for item in list {
                        item.strip_unused_marks(env).await;
                    }
                }
                Self::Vector { ref mut vector, .. } => {
                    for item in vector {
                        item.strip_unused_marks(env).await;
                    }
                }
                Self::Identifier { ref mut ident, .. } => env.strip_unused_marks(ident).await,
                _ => (),
            }
        })
    }

    async fn apply_transformer(
        &self,
        macro_env: Env,
        transformer: Gc<Value>,
    ) -> Result<Expansion<'static>, RuntimeError> {
        // Create a new mark for the expansion context
        let new_mark = Mark::new();
        // Apply the new mark to the input
        // TODO: Figure out a better way to do this without cloning so much
        let mut input = self.clone();
        input.mark(new_mark);
        // Convert the input to a value. We wrap this with the current environment, but in
        // theory it shouldn't matter too much.
        let input = Gc::new(Value::Syntax(input));
        // Call the transformer with the input:
        let output = transformer
            .read()
            .await
            .as_proc()
            .ok_or(RuntimeError::InvalidType)?
            .call(vec![input])
            .await?;
        // TODO: try_unwrap will make this more efficient
        let output = match &*output.read().await {
            Value::Syntax(syntax) => {
                let mut output = syntax.clone();
                // Apply the new mark to the output
                output.mark(new_mark);
                Ok(Expansion::Expanded {
                    mark: new_mark,
                    syntax: output,
                    macro_env,
                })
            }
            _ => todo!(),
        };
        output
    }

    fn expand<'a>(&'a self, env: &'a Env) -> BoxFuture<'a, Result<Expansion<'a>, RuntimeError>> {
        Box::pin(async move {
            match self {
                Self::List { list, .. } => {
                    // If the head is not an identifier, we leave the expression unexpanded
                    // for now. We will expand it later in the proc call
                    let ident = match list.get(0) {
                        Some(Self::Identifier { ident, .. }) => ident,
                        _ => return Ok(Expansion::Unexpanded(self)),
                    };
                    if let Some((macro_env, transformer)) = env.fetch_macro(ident).await {
                        return self.apply_transformer(macro_env, transformer).await;
                    }
                }
                Self::Identifier { ident, .. } => {
                    if let Some((macro_env, transformer)) = env.fetch_macro(ident).await {
                        return self.apply_transformer(macro_env, transformer).await;
                    }
                }
                _ => (),
            }
            Ok(Expansion::Unexpanded(self))
        })
    }

    pub async fn compile_expanded(&self, env: &Env) -> Result<Box<dyn Eval>, CompileError> {
        match self {
            Self::Nil { span } => Err(CompileError::UnexpectedEmptyList(span.clone())),
            Self::Identifier { ident, .. } => Ok(Box::new(
                env.fetch_var(ident)
                    .await
                    .ok_or_else(|| CompileError::UndefinedVariable(ident.clone()))?,
            ) as Box<dyn Eval>),
            Self::Literal { literal, .. } => Ok(Box::new(literal.clone()) as Box<dyn Eval>),
            Self::List { list: exprs, span } => match &exprs[..] {
                // Function call:
                [Self::Identifier { ident, .. }, ..] if env.is_bound(&ident).await => {
                    ast::Call::compile_to_expr(exprs, env, span).await
                }
                // Special forms:
                [Self::Identifier { ident, span }, tail @ ..] if ident == "quote" => {
                    ast::Quote::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "syntax" => {
                    ast::SyntaxQuote::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "begin" => {
                    ast::Body::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "let" => {
                    ast::Let::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "lambda" => {
                    ast::Lambda::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "if" => {
                    ast::If::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "and" => {
                    ast::And::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "or" => {
                    ast::Or::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "define" => {
                    ast::Define::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "define-syntax" => {
                    ast::DefineSyntax::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "syntax-case" => {
                    ast::SyntaxCase::compile_to_expr(tail, env, span).await
                }
                [Self::Identifier { ident, span }, tail @ ..] if ident == "set!" => {
                    ast::Set::compile_to_expr(tail, env, span).await
                }
                x => panic!("bad form: {:#?}", x),
            },
            Self::Vector { vector, .. } => {
                let mut vals = Vec::new();
                for item in vector {
                    match item {
                        Self::Nil { .. } => vals.push(Box::new(ast::Nil) as Box<dyn Eval>),
                        item => vals.push(item.compile(env).await?),
                    }
                }
                Ok(Box::new(ast::Vector { vals }) as Box<dyn Eval>)
            }
        }
    }

    pub async fn compile(&self, env: &Env) -> Result<Box<dyn Eval>, CompileError> {
        self.expand(env).await?.compile(env).await
    }
}

pub enum Expansion<'a> {
    /// Syntax remained unchanged after expansion
    Unexpanded(&'a Syntax),
    /// Syntax was expanded, producing a new expansion context
    Expanded {
        mark: Mark,
        macro_env: Env,
        syntax: Syntax,
    },
}

impl Expansion<'_> {
    pub fn is_expanded(&self) -> bool {
        matches!(self, Self::Expanded { .. })
    }

    pub fn is_unexpanded(&self) -> bool {
        matches!(self, Self::Unexpanded(_))
    }
}

impl<'a> Expansion<'a> {
    pub fn compile(self, env: &'a Env) -> BoxFuture<'a, Result<Box<dyn Eval>, CompileError>> {
        Box::pin(async move {
            match self {
                Self::Unexpanded(syntax) => syntax.compile_expanded(env).await,
                Self::Expanded {
                    mark,
                    syntax,
                    macro_env,
                } => {
                    // If the expression has been expanded, we may need to expand it again, but
                    // it must be done in a new expansion context.
                    let env = Env::Expansion(Gc::new(env.new_expansion_context(mark, macro_env)));
                    syntax.expand(&env).await?.compile_expanded(&env).await
                }
            }
        })
    }
}

impl std::ops::Deref for Expansion<'_> {
    type Target = Syntax;

    fn deref(&self) -> &Syntax {
        match self {
            Self::Unexpanded(syntax) => syntax,
            Self::Expanded { syntax, .. } => syntax,
        }
    }
}

#[derive(Debug)]
pub struct ParsedSyntax {
    pub doc_comment: Option<String>,
    syntax: Syntax,
}

impl ParsedSyntax {
    fn parse_fragment<'a>(i: &'a [Token<'a>]) -> Result<(&'a [Token<'a>], Self), ParseError<'a>> {
        let (doc_comment, remaining) = if let Token {
            lexeme: Lexeme::DocComment(ref doc_comment),
            ..
        } = i[0]
        {
            (Some(doc_comment.clone()), &i[1..])
        } else {
            (None, i)
        };
        let (remaining, syntax) = crate::parse::expression(remaining)?;
        Ok((
            remaining,
            Self {
                doc_comment,
                syntax,
            },
        ))
    }

    pub fn parse<'a>(mut i: &'a [Token<'a>]) -> Result<Vec<Self>, ParseError<'a>> {
        let mut output = Vec::new();
        while !i.is_empty() {
            let (remaining, expr) = Self::parse_fragment(i)?;
            output.push(expr);
            i = remaining
        }
        Ok(output)
    }

    pub async fn compile(&self, env: &Env) -> Result<Box<dyn Eval>, CompileError> {
        self.syntax.compile(env).await
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Mark(u64);

impl Mark {
    pub fn new() -> Self {
        Self(rand::random())
    }
}

impl Default for Mark {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub marks: BTreeSet<Mark>,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Self {
            name,
            marks: BTreeSet::default(),
        }
    }

    pub fn mark(&mut self, mark: Mark) {
        if self.marks.contains(&mark) {
            self.marks.remove(&mark);
        } else {
            self.marks.insert(mark);
        }
    }
}

impl PartialEq<str> for Identifier {
    fn eq(&self, rhs: &str) -> bool {
        self.name == rhs
    }
}

impl Syntax {
    pub fn span(&self) -> &Span {
        match self {
            Self::Nil { span } => span,
            Self::List { span, .. } => span,
            Self::Vector { span, .. } => span,
            Self::Literal { span, .. } => span,
            Self::Identifier { span, .. } => span,
        }
    }

    // There's got to be a better way:

    pub fn new_nil(span: impl Into<Span>) -> Self {
        Self::Nil { span: span.into() }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil { .. })
    }

    pub fn new_list(list: Vec<Syntax>, span: impl Into<Span>) -> Self {
        Self::List {
            list,
            span: span.into(),
        }
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Self::List { .. })
    }

    pub fn new_vector(vector: Vec<Syntax>, span: impl Into<Span>) -> Self {
        Self::Vector {
            vector,
            span: span.into(),
        }
    }

    pub fn is_vector(&self) -> bool {
        matches!(self, Self::Vector { .. })
    }

    pub fn new_literal(literal: Literal, span: impl Into<Span>) -> Self {
        Self::Literal {
            literal,
            span: span.into(),
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Self::Literal { .. })
    }

    pub fn new_identifier(name: &str, span: impl Into<Span>) -> Self {
        Self::Identifier {
            ident: Identifier::new(name.to_string()),
            span: span.into(),
        }
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier { .. })
    }
}
