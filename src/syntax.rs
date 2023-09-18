use crate::{
    ast::{self, Ident, Literal},
    compile::{Compile, CompileError},
    eval::{Env, Eval, Value},
    expand::Binds,
    gc::Gc,
    lex::{InputSpan, Lexeme, Token},
    parse::ParseError,
};
use futures::future::BoxFuture;
use std::{borrow::Cow, fmt, sync::Arc};

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
        ident: Ident,
        span: Span,
    },
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

    pub fn new_identifier(ident: Ident, span: impl Into<Span>) -> Self {
        Self::Identifier {
            ident,
            span: span.into(),
        }
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier { .. })
    }

    fn expand<'a>(&'a self, env: &'a Gc<Env>, binds: Arc<Binds>) -> BoxFuture<'a, Cow<'a, Syntax>> {
        Box::pin(async move {
            match self {
                Self::List { list, span } => {
                    let (head, tail) = list.split_first().unwrap();
                    let head = match head {
                        list @ Self::List { .. } => list.expand(env, binds.clone()).await,
                        x => Cow::Borrowed(x),
                    };
                    let ident = match head.as_ref() {
                        Self::Identifier { ident, .. } => ident,
                        _ => {
                            let mut borrowed = matches!(head, Cow::Borrowed(_));
                            let mut output = vec![head];
                            for item in tail {
                                let item = item.expand(env, binds.clone()).await;
                                borrowed &= matches!(item, Cow::Borrowed(_));
                                output.push(item);
                            }
                            // If every item is borrowed, nothing has changed, and we
                            // can return the expression as is.
                            if borrowed {
                                return Cow::Borrowed(self);
                            } else {
                                let output: Vec<_> =
                                    output.into_iter().map(Cow::into_owned).collect();
                                return Cow::Owned(Self::new_list(output, span.clone()));
                            }
                        }
                    };
                    if let Some(head_value) = env.read().await.fetch(ident).await {
                        if let Value::Transformer(transformer) = &*head_value.read().await {
                            let mut list = vec![head.into_owned()];
                            list.extend(tail.iter().cloned());
                            let mut expanded = transformer
                                .expand(&Syntax::new_list(list, span.clone()), binds.clone())
                                .unwrap();
                            loop {
                                if let ref expr @ Syntax::List { ref list, .. } = expanded {
                                    if let [Syntax::Identifier { ident, .. }, ..] = &list[..] {
                                        if let Some(head) = env.read().await.fetch(ident).await {
                                            if let Value::Transformer(transformer) =
                                                &*head.read().await
                                            {
                                                expanded = transformer
                                                    .expand(expr, binds.clone())
                                                    .unwrap();
                                                continue;
                                            }
                                        }
                                    }
                                }
                                return Cow::Owned(expanded);
                            }
                        }
                    }
                }
                expr @ Self::Identifier { ident, .. } => {
                    if let Some(val) = env.read().await.fetch(ident).await {
                        if let Value::Transformer(transformer) = &*val.read().await {
                            let expanded = transformer.expand(expr, binds.clone()).unwrap();
                            return Cow::Owned(expanded);
                        }
                    }
                }
                _ => (),
            }
            Cow::Borrowed(self)
        })
    }

    pub fn compile<'a>(
        &'a self,
        env: &'a Gc<Env>,
        binds: Arc<Binds>,
    ) -> BoxFuture<'a, Result<Box<dyn Eval>, CompileError>> {
        Box::pin(async move {
            let expr = self.expand(env, binds.clone()).await;
            match &*expr {
                Self::Nil { span } => Err(CompileError::UnexpectedEmptyList(span.clone())),
                Self::List { list: exprs, span } => match &exprs[..] {
                    // Special forms:
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "quote" => {
                        ast::Quote::compile_to_expr(tail, env, binds, span).await
                    }
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "and" => {
                        ast::And::compile_to_expr(tail, env, binds, span).await
                    }
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "or" => {
                        ast::Or::compile_to_expr(tail, env, binds, span).await
                    }
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "begin" => {
                        ast::Body::compile_to_expr(tail, env, binds, span).await
                    }
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "if" => {
                        ast::If::compile_to_expr(tail, env, binds, span).await
                    }
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "let" => {
                        ast::Let::compile_to_expr(tail, env, binds, span).await
                    }
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "lambda" => {
                        ast::Lambda::compile_to_expr(tail, env, binds, span).await
                    }
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "define" => {
                        ast::Define::compile_to_expr(tail, env, binds, span).await
                    }
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "define-syntax" => {
                        ast::DefineSyntax::compile_to_expr(tail, env, binds, span).await
                    }
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "syntax" => {
                        ast::Syntax::compile_to_expr(tail, env, binds, span).await
                    }
                    // Function call:
                    exprs => ast::Call::compile_to_expr(exprs, env, binds.clone(), span).await,
                },
                Self::Literal { literal, .. } => Ok(Box::new(literal.clone()) as Box<dyn Eval>),
                Self::Identifier { ident, .. } => {
                    // If the identifier has a macro environment and has not been bound we
                    // can attempt to look it up in order to properly scope it
                    match ident.hygiene {
                        Some(ref hygiene) if !binds.is_bound(&ident.sym) => {
                            if let Some(val) =
                                hygiene.read().await.fetch(&Ident::new(&ident.sym)).await
                            {
                                return Ok(Box::new(ast::Ref { val: val.clone() }) as Box<dyn Eval>);
                            }
                        }
                        _ => (),
                    }
                    Ok(Box::new(ident.clone()) as Box<dyn Eval>)
                }
                Self::Vector { vector, .. } => {
                    let mut vals = Vec::new();
                    for item in vector {
                        match item {
                            Self::Nil { .. } => vals.push(Box::new(ast::Nil) as Box<dyn Eval>),
                            item => vals.push(item.compile(env, binds.clone()).await?),
                        }
                    }
                    Ok(Box::new(ast::Vector { vals }) as Box<dyn Eval>)
                }
            }
        })
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

    pub async fn compile(&self, env: &Gc<Env>) -> Result<Box<dyn Eval>, CompileError> {
        let binds = Binds::from_global(env).await;
        self.syntax.compile(env, binds).await
    }
}
