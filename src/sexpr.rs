use crate::{
    ast::{self, Ident, Literal},
    compile::{Compile, CompileError},
    eval::{Env, Eval, Value},
    expand::Binds,
    gc::Gc,
    lex::{Lexeme, Span, Token},
    parse::ParseError,
};
use futures::future::BoxFuture;
use std::{borrow::Cow, sync::Arc};

#[derive(Debug, Clone, PartialEq)]
pub enum SExpr<'a> {
    /// An empty list.
    Nil {
        span: Span<'a>,
    },
    /// A nested grouping of pairs. If the expression is a proper list, then the
    /// last element of expression will be Nil. This vector is guaranteed to contain
    /// at least two elements.
    List {
        list: Vec<SExpr<'a>>,
        span: Span<'a>,
    },
    Vector {
        vector: Vec<SExpr<'a>>,
        span: Span<'a>,
    },
    Literal {
        literal: Literal,
        span: Span<'a>,
    },
    Identifier {
        ident: Ident,
        span: Span<'a>,
    },
}

impl<'a> SExpr<'a> {
    pub fn span(&self) -> &Span<'a> {
        match self {
            Self::Nil { span } => span,
            Self::List { span, .. } => span,
            Self::Vector { span, .. } => span,
            Self::Literal { span, .. } => span,
            Self::Identifier { span, .. } => span,
        }
    }

    // There's got to be a better way:

    pub fn new_nil(span: Span<'a>) -> Self {
        Self::Nil { span }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil { .. })
    }

    pub fn new_list(list: Vec<SExpr<'a>>, span: Span<'a>) -> Self {
        Self::List { list, span }
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Self::List { .. })
    }

    pub fn new_vector(vector: Vec<SExpr<'a>>, span: Span<'a>) -> Self {
        Self::Vector { vector, span }
    }

    pub fn is_vector(&self) -> bool {
        matches!(self, Self::Vector { .. })
    }

    pub fn new_literal(literal: Literal, span: Span<'a>) -> Self {
        Self::Literal { literal, span }
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Self::Literal { .. })
    }

    pub fn new_identifier(ident: Ident, span: Span<'a>) -> Self {
        Self::Identifier { ident, span }
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier { .. })
    }

    fn expand<'b>(
        &'b self,
        env: &'b Gc<Env>,
        binds: Arc<Binds>,
    ) -> BoxFuture<'b, Cow<'b, SExpr<'a>>> {
        Box::pin(async move {
            if let Self::List { list, span } = self {
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
                            let output: Vec<_> = output.into_iter().map(Cow::into_owned).collect();
                            return Cow::Owned(Self::new_list(output, span.clone()));
                        }
                    }
                };
                if let Some(head_value) = env.read().await.fetch(ident).await {
                    if let Value::Transformer(transformer) = &*head_value.read().await {
                        let mut list = vec![head.into_owned()];
                        list.extend(tail.iter().cloned());
                        let mut expanded = transformer
                            .expand(&SExpr::new_list(list, span.clone()), binds.clone())
                            .unwrap();
                        loop {
                            if let ref expr @ SExpr::List { ref list, .. } = expanded {
                                if let [SExpr::Identifier { ident, .. }, ..] = &list[..] {
                                    if let Some(head) = env.read().await.fetch(ident).await {
                                        if let Value::Transformer(transformer) = &*head.read().await
                                        {
                                            expanded =
                                                transformer.expand(expr, binds.clone()).unwrap();
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
            Cow::Borrowed(self)
        })
    }

    pub fn compile<'b>(
        &'b self,
        env: &'b Gc<Env>,
        binds: Arc<Binds>,
    ) -> BoxFuture<'b, Result<Box<dyn Eval>, CompileError<'a>>> {
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
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "define" => {
                        ast::Define::compile_to_expr(tail, env, binds, span).await
                    }
                    [Self::Identifier { ident, span }, tail @ ..] if ident == "define-syntax" => {
                        ast::DefineSyntax::compile_to_expr(tail, env, binds, span).await
                    }
                    // Function call:
                    exprs => ast::Call::compile_to_expr(exprs, env, binds.clone(), span).await,
                },
                Self::Literal { literal, .. } => Ok(Box::new(literal.clone()) as Box<dyn Eval>),
                Self::Identifier { ident, .. } => {
                    // If the identifier has a macro environment and has not been bound we
                    // can attempt to look it up in order to properly scope it
                    match ident.macro_env {
                        Some(ref macro_env) if !binds.is_bound(&ident.sym) => {
                            let ident = Ident {
                                sym: ident.sym.clone(),
                                macro_env: None,
                            };
                            if let Some(val) = macro_env.read().await.fetch(&ident).await {
                                return Ok(Box::new(ast::Ref { val: val.clone() }) as Box<dyn Eval>);
                            }
                        }
                        _ => (),
                    }
                    Ok(Box::new(ident.clone()) as Box<dyn Eval>)
                }
                x => todo!("expr: {x:#?}"),
            }
        })
    }
}

#[derive(Debug)]
pub struct ParsedSExpr<'a> {
    pub doc_comment: Option<String>,
    sexpr: SExpr<'a>,
}

impl<'a> ParsedSExpr<'a> {
    fn parse_fragment(i: &'a [Token<'a>]) -> Result<(&'a [Token<'a>], Self), ParseError<'a>> {
        let (doc_comment, remaining) = if let Token {
            lexeme: Lexeme::DocComment(ref doc_comment),
            ..
        } = i[0]
        {
            (Some(doc_comment.clone()), &i[1..])
        } else {
            (None, i)
        };
        let (remaining, sexpr) = crate::parse::expression(remaining)?;
        Ok((remaining, Self { doc_comment, sexpr }))
    }

    pub fn parse(mut i: &'a [Token<'a>]) -> Result<Vec<Self>, ParseError<'a>> {
        let mut output = Vec::new();
        while !i.is_empty() {
            let (remaining, expr) = Self::parse_fragment(i)?;
            output.push(expr);
            i = remaining
        }
        Ok(output)
    }

    pub async fn compile(&'a self, env: &'a Gc<Env>) -> Result<Box<dyn Eval>, CompileError<'a>> {
        let binds = Binds::from_global(env).await;
        self.sexpr.compile(env, binds).await
    }
}
