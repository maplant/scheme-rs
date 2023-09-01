use crate::{
    ast::{Ident, Literal},
    eval::{Env, Eval, Value},
    expand::Binds,
    gc::Gc,
    lex::{Lexeme, Span},
};
use futures::future::BoxFuture;
use std::borrow::Cow;

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

    pub fn new_literal(literal: Literal, span: Span<'a>) -> Self {
        Self::Literal { literal, span }
    }

    pub fn new_identifier(ident: Ident, span: Span<'a>) -> Self {
        Self::Identifier { ident, span }
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier { .. })
    }

    fn expand<'b: 'a>(
        &'b self,
        env: &'a Gc<Env>,
        binds: &'a Binds<'_>,
    ) -> BoxFuture<'a, Cow<'b, SExpr<'a>>> {
        Box::pin(async move {
            match self {
                Self::List { list, span } => {
                    let (head, tail) = list.split_first().unwrap();
                    let head = match head {
                        list @ Self::List { .. } => list.expand(env, binds).await,
                        x => Cow::Borrowed(x),
                    };
                    let ident = match head.as_ref() {
                        Self::Identifier { ident, .. } => ident,
                        _ => {
                            let mut borrowed = matches!(head, Cow::Borrowed(_));
                            let mut output = vec![head];
                            for item in tail {
                                let item = item.expand(env, binds).await;
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
                    if let Some(head_value) = env.read().await.fetch(&ident).await {
                        if let Value::Transformer(transformer) = &*head_value.read().await {
                            let mut list = vec![head.into_owned()];
                            list.extend(tail.iter().cloned());
                            let mut expanded = transformer
                                .expand(&SExpr::new_list(list, span.clone()), binds)
                                .unwrap();
                            loop {
                                match expanded {
                                    ref expr @ SExpr::List { ref list, .. } => match &list[..] {
                                        [SExpr::Identifier { ident, .. }, ..] => {
                                            if let Some(head) = env.read().await.fetch(&ident).await
                                            {
                                                if let Value::Transformer(transformer) =
                                                    &*head.read().await
                                                {
                                                    expanded =
                                                        transformer.expand(&expr, binds).unwrap();
                                                    continue;
                                                }
                                            }
                                        }
                                        _ => (),
                                    },
                                    _ => (),
                                }
                                return Cow::Owned(expanded);
                            }
                        }
                    }
                }
                _ => (),
            }
            Cow::Borrowed(self)
        })
    }

    pub fn compile<'b: 'a>(
        &'b self,
        env: &'a Gc<Env>,
        binds: &'a Binds<'_>,
    ) -> BoxFuture<'a, Box<dyn Eval>> {
        Box::pin(async move {
            let expr = self.expand(env, binds).await;
            todo!()
        })
    }
}

pub struct ParsedSExpr<'a> {
    doc_comment: Option<String>,
    sexpr: SExpr<'a>,
}

impl<'a> ParsedSExpr<'a> {
    fn parse_fragment(i: &'a [Lexeme<'a>]) -> Result<(&'a [Lexeme<'a>], Self), ()> {
        todo!()
        /*
        let (doc_comment, remaining) = match i[0] {
            Lexeme::DocComment(ref doc_comment) => (Some(doc_comment.clone()), &i[1..]),
            _ => (None, i),
        };
        let (remaining, sexpr) = crate::parse::expression(remaining).unwrap();
        Ok((remaining, Self { doc_comment, sexpr }))
        */
    }

    pub fn parse(mut i: &'a [Lexeme<'a>]) -> Result<Vec<Self>, ()> {
        let mut output = Vec::new();
        while !i.is_empty() {
            let (remaining, expr) = Self::parse_fragment(i)?;
            output.push(expr);
            i = remaining
        }
        Ok(output)
    }

    pub fn compile(self, env: &'a Gc<Env>) -> BoxFuture<'a, Box<dyn Eval>> {
        Box::pin(async move {
            let binds = Binds::from_global(env).await;
            self.sexpr.compile(env, &binds).await
        })
    }
}
