use crate::{
    ast::{self, Ident},
    eval::{Env, Eval, Value},
    expand::{Binds, Pattern, SyntaxRule, Template},
    gc::Gc,
    lex::Span,
    sexpr::SExpr,
};
use async_trait::async_trait;
use derive_more::From;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(From, Debug)]
pub enum CompileError<'a> {
    UnexpectedEmptyList(Span<'a>),
    CompileFuncCallError(CompileFuncCallError<'a>),
    CompileDefineError(CompileDefineError<'a>),
    CompileIfError(CompileIfError<'a>),
    CompileLetError(CompileLetError<'a>),
    CompileDefineSyntaxError(CompileDefineSyntaxError<'a>),
    CompileBodyError(CompileBodyError<'a>),
    CompileQuoteError(CompileQuoteError<'a>),
    CompileSetError(CompileSetError<'a>),
}

macro_rules! impl_from_compile_error {
    ( $error:ident ) => {
        impl<'a> From<CompileError<'a>> for $error<'a> {
            fn from(ce: CompileError<'a>) -> Self {
                Self::CompileError(Box::new(ce))
            }
        }
    };
}

#[async_trait]
pub trait Compile: Eval + Sized + 'static
where
    for<'a> CompileError<'a>: From<Self::Error<'a>>,
{
    type Error<'a>;

    async fn compile<'a>(
        exprs: &[SExpr<'a>],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span<'a>,
    ) -> Result<Self, Self::Error<'a>>;

    async fn compile_to_expr<'a>(
        exprs: &[SExpr<'a>],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span<'a>,
    ) -> Result<Box<dyn Eval>, CompileError<'a>> {
        Ok(Box::new(Self::compile(exprs, env, binds, span).await?))
    }
}

#[derive(From, Debug)]
pub enum CompileFuncCallError<'a> {
    EmptyFunctionCall(Span<'a>),
    CompileError(Box<CompileError<'a>>),
}

impl_from_compile_error!(CompileFuncCallError);

#[async_trait]
impl Compile for ast::Call {
    type Error<'a> = CompileFuncCallError<'a>;

    async fn compile<'a>(
        exprs: &[SExpr<'a>],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span<'a>,
    ) -> Result<ast::Call, CompileFuncCallError<'a>> {
        match exprs {
            [operator, args @ ..] => {
                let operator = operator.compile(env, binds.clone()).await?;
                let mut compiled_args = Vec::new();
                for arg in &args[..args.len() - 1] {
                    compiled_args.push(arg.compile(env, binds.clone()).await?);
                }
                // TODO: what if it's not a proper list?
                Ok(ast::Call {
                    operator,
                    args: compiled_args,
                })
            }
            [] => Err(CompileFuncCallError::EmptyFunctionCall(span.clone())),
        }
    }
}

#[derive(Debug)]
pub enum CompileDefineError<'a> {
    ParameterDefinedMultipleTimes {
        ident: Ident,
        first: Span<'a>,
        second: Span<'a>,
    },
    ExpectedIdentifier(Span<'a>),
    CompileBodyError(CompileBodyError<'a>),
    BadForm(Span<'a>),
    CompileError(Box<CompileError<'a>>),
}

impl_from_compile_error!(CompileDefineError);

#[async_trait]
impl Compile for ast::Define {
    type Error<'a> = CompileDefineError<'a>;

    async fn compile<'a>(
        exprs: &[SExpr<'a>],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span<'a>,
    ) -> Result<Self, Self::Error<'a>> {
        match exprs {
            [SExpr::Identifier { ident, .. }, expr] => Ok(ast::Define::DefineVar(ast::DefineVar {
                name: ident.clone(),
                val: expr.compile(env, binds).await?,
            })),
            [SExpr::List { list, span }, body @ ..] => {
                match &list[..] {
                    [] => Err(CompileDefineError::ExpectedIdentifier(span.clone())),
                    [SExpr::Identifier {
                        ident: func_name,
                        span: func_span,
                    }, args @ ..] => {
                        let mut scope = Binds::new_local(&binds);
                        let mut bound = HashMap::<Ident, Span<'_>>::new();
                        let mut fixed = Vec::new();
                        for arg in &args[..args.len() - 1] {
                            match arg {
                                SExpr::Identifier { ident, span } => {
                                    if let Some(prev_span) = bound.get(ident) {
                                        return Err(
                                            CompileDefineError::ParameterDefinedMultipleTimes {
                                                ident: ident.clone(),
                                                first: prev_span.clone(),
                                                second: span.clone(),
                                            },
                                        );
                                    }
                                    scope.bind(&ident.sym);
                                    bound.insert(ident.clone(), span.clone());
                                    fixed.push(ident.clone());
                                }
                                x => {
                                    return Err(CompileDefineError::ExpectedIdentifier(
                                        x.span().clone(),
                                    ))
                                }
                            }
                        }

                        let args = if let Some(last) = args.last() {
                            match last {
                                SExpr::Nil { .. } => {
                                    ast::Formals::FixedArgs(fixed.into_iter().collect())
                                }
                                SExpr::Identifier { ident, span } => {
                                    if let Some(prev_span) = bound.get(ident) {
                                        return Err(
                                            CompileDefineError::ParameterDefinedMultipleTimes {
                                                ident: ident.clone(),
                                                first: prev_span.clone(),
                                                second: span.clone(),
                                            },
                                        );
                                    }
                                    scope.bind(&ident.sym);
                                    ast::Formals::VarArgs {
                                        fixed: fixed.into_iter().collect(),
                                        remaining: ident.clone(),
                                    }
                                }
                                x => {
                                    return Err(CompileDefineError::ExpectedIdentifier(
                                        x.span().clone(),
                                    ))
                                }
                            }
                        } else {
                            // If there is no last argument, there are no arguments
                            ast::Formals::FixedArgs(Vec::new())
                        };

                        scope.bind(&func_name.sym);
                        let body = ast::Body::compile(body, env, Arc::new(scope), func_span)
                            .await
                            .map_err(CompileDefineError::CompileBodyError)?;
                        Ok(ast::Define::DefineFunc(ast::DefineFunc {
                            name: func_name.clone(),
                            args,
                            body,
                        }))
                    }
                    [x, ..] => Err(CompileDefineError::BadForm(x.span().clone())),
                }
            }
            _ => Err(CompileDefineError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug)]
pub enum CompileIfError<'a> {
    ExpectedConditional(Span<'a>),
    ExpectedArgumentAfterConditional(Span<'a>),
    UnexpectedArgument(Span<'a>),
    CompileError(Box<CompileError<'a>>),
}

impl_from_compile_error!(CompileIfError);

#[async_trait]
impl Compile for ast::If {
    type Error<'a> = CompileIfError<'a>;

    async fn compile<'a>(
        exprs: &[SExpr<'a>],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span<'a>,
    ) -> Result<Self, CompileIfError<'a>> {
        match exprs {
            [cond, success, SExpr::Nil { .. }] => Ok(ast::If {
                cond: cond.compile(env, binds.clone()).await?,
                success: success.compile(env, binds.clone()).await?,
                failure: None,
            }),
            [cond, success, failure, SExpr::Nil { .. }] => Ok(ast::If {
                cond: cond.compile(env, binds.clone()).await?,
                success: success.compile(env, binds.clone()).await?,
                failure: Some(failure.compile(env, binds.clone()).await?),
            }),
            [] => Err(CompileIfError::ExpectedConditional(span.clone())),
            [a1] => Err(CompileIfError::ExpectedArgumentAfterConditional(
                a1.span().clone(),
            )),
            [_, _, _, unexpected, ..] => Err(CompileIfError::UnexpectedArgument(
                unexpected.span().clone(),
            )),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum CompileBodyError<'a> {
    EmptyBody(Span<'a>),
    CompileError(Box<CompileError<'a>>),
}

impl_from_compile_error!(CompileBodyError);

#[async_trait]
impl Compile for ast::Body {
    type Error<'a> = CompileBodyError<'a>;

    async fn compile<'a>(
        exprs: &[SExpr<'a>],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span<'a>,
    ) -> Result<Self, CompileBodyError<'a>> {
        if exprs.is_empty() {
            return Err(CompileBodyError::EmptyBody(span.clone()));
        }
        let mut output = Vec::new();
        for expr in &exprs[..exprs.len() - 1] {
            let expr = expr.compile(env, binds.clone()).await?;
            output.push(expr);
        }
        // TODO: what if the body isn't a proper list?
        Ok(ast::Body::new(output))
    }
}

#[derive(Debug)]
pub enum CompileLetError<'a> {
    BadForm(Span<'a>),
    CompileBodyError(CompileBodyError<'a>),
    CompileLetBindingError(CompileLetBindingError<'a>),
}

#[async_trait]
impl Compile for ast::Let {
    type Error<'a> = CompileLetError<'a>;

    async fn compile<'a>(
        expr: &[SExpr<'a>],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span<'a>,
    ) -> Result<Self, CompileLetError<'a>> {
        match expr {
            [SExpr::List { list: bindings, .. }, body @ ..] => {
                let mut previously_bound = HashMap::new();
                let mut compiled_bindings = Vec::new();
                let mut new_scope = Binds::new_local(&binds);
                // TODO: Check that list is proper
                for binding in &bindings[..bindings.len() - 1] {
                    let binding =
                        LetBinding::compile(binding, env, binds.clone(), &previously_bound)
                            .await
                            .map_err(CompileLetError::CompileLetBindingError)?;
                    previously_bound.insert(binding.ident.clone(), binding.span.clone());
                    new_scope.bind(&binding.ident.sym);
                    compiled_bindings.push(binding);
                }
                let body = ast::Body::compile(body, env, binds.clone(), span)
                    .await
                    .map_err(CompileLetError::CompileBodyError)?;
                Ok(ast::Let {
                    bindings: compiled_bindings
                        .into_iter()
                        .map(|binding| (binding.ident, binding.expr))
                        .collect(),
                    body,
                })
            }
            _ => Err(CompileLetError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug)]
pub enum CompileLetBindingError<'a> {
    BadForm(Span<'a>),
    PreviouslyBound {
        ident: Ident,
        first: Span<'a>,
        second: Span<'a>,
    },
    NotAList(Span<'a>),
    CompileError(Box<CompileError<'a>>),
}

impl_from_compile_error!(CompileLetBindingError);

struct LetBinding<'a> {
    ident: Ident,
    span: Span<'a>,
    expr: Box<dyn Eval>,
}

impl<'a> LetBinding<'a> {
    async fn compile(
        expr: &SExpr<'a>,
        env: &Gc<Env>,
        binds: Arc<Binds>,
        previously_bound: &HashMap<Ident, Span<'a>>,
    ) -> Result<LetBinding<'a>, CompileLetBindingError<'a>> {
        match expr {
            SExpr::List { list, span } => match &list[..] {
                [SExpr::Identifier {
                    ident,
                    span: bind_span,
                }, expr, SExpr::Nil { .. }] => {
                    let ident = ident.clone();
                    if let Some(prev_bind) = previously_bound.get(&ident) {
                        return Err(CompileLetBindingError::PreviouslyBound {
                            ident,
                            first: prev_bind.clone(),
                            second: bind_span.clone(),
                        });
                    }

                    let expr = expr.compile(env, binds).await?;

                    Ok(LetBinding {
                        ident,
                        span: bind_span.clone(),
                        expr,
                    })
                }
                _ => Err(CompileLetBindingError::BadForm(span.clone())),
            },
            expr => Err(CompileLetBindingError::NotAList(expr.span().clone())),
        }
    }
}

#[derive(Debug)]
pub enum CompileDefineSyntaxError<'a> {
    BadForm(Span<'a>),
}

#[async_trait]
impl Compile for ast::DefineSyntax {
    type Error<'a> = CompileDefineSyntaxError<'a>;

    async fn compile<'a>(
        expr: &[SExpr<'a>],
        _env: &Gc<Env>,
        _binds: Arc<Binds>,
        span: &Span<'a>,
    ) -> Result<ast::DefineSyntax, CompileDefineSyntaxError<'a>> {
        match expr {
            [SExpr::Identifier {
                ident: macro_name, ..
            }, SExpr::List {
                list: syntax_rules, ..
            }, SExpr::Nil { .. }] => {
                let (mut keywords, mut rules) = match &syntax_rules[..] {
                    [SExpr::Identifier { ident, .. }, SExpr::List {
                        list: keywords_list,
                        ..
                    }, SExpr::List { list: rules, .. }, SExpr::Nil { .. }]
                        if ident.sym == "syntax-rules" =>
                    {
                        let mut keywords = HashSet::default();
                        // TODO: ensure keywords_list is proper
                        for keyword in &keywords_list[..keywords_list.len() - 1] {
                            if let SExpr::Identifier { ident, .. } = keyword {
                                keywords.insert(ident.sym.clone());
                            } else {
                                return Err(CompileDefineSyntaxError::BadForm(
                                    keyword.span().clone(),
                                ));
                            }
                        }
                        (keywords, &rules[..])
                    }
                    [SExpr::Identifier { ident, .. }, SExpr::Nil { .. }, SExpr::List { list: rules, .. }, SExpr::Nil { .. }]
                        if ident.sym == "syntax-rules" =>
                    {
                        (HashSet::default(), &rules[..])
                    }
                    _ => return Err(CompileDefineSyntaxError::BadForm(span.clone())),
                };
                keywords.insert(macro_name.sym.clone());
                let mut syntax_rules = Vec::new();
                loop {
                    match rules {
                        [SExpr::Nil { .. }] => break,
                        [pattern, template, tail @ ..] => {
                            syntax_rules.push(SyntaxRule {
                                pattern: Pattern::compile(pattern, &keywords),
                                template: Template::compile(template),
                            });
                            rules = tail;
                        }
                        _ => return Err(CompileDefineSyntaxError::BadForm(span.clone())),
                    }
                }
                Ok(ast::DefineSyntax {
                    name: macro_name.clone(),
                    rules: syntax_rules,
                })
            }
            _ => Err(CompileDefineSyntaxError::BadForm(span.clone())),
        }
    }
}

#[async_trait]
impl Compile for ast::And {
    type Error<'a> = CompileError<'a>;

    async fn compile<'a>(
        exprs: &[SExpr<'a>],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        _span: &Span<'a>,
    ) -> Result<Self, CompileError<'a>> {
        let mut output = Vec::new();
        // TODO: what if the arguments aren't a proper list?
        for expr in &exprs[..exprs.len() - 1] {
            let expr = expr.compile(env, binds.clone()).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}
#[async_trait]
impl Compile for ast::Or {
    type Error<'a> = CompileError<'a>;

    async fn compile<'a>(
        exprs: &[SExpr<'a>],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        _span: &Span<'a>,
    ) -> Result<Self, CompileError<'a>> {
        let mut output = Vec::new();
        // TODO: what if the arguments aren't a proper list?
        for expr in &exprs[..exprs.len() - 1] {
            let expr = expr.compile(env, binds.clone()).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

#[derive(Debug)]
pub enum CompileQuoteError<'a> {
    ExpectedArgument(Span<'a>),
    UnexpectedArgument(Span<'a>),
    BadForm(Span<'a>),
}

#[async_trait]
impl Compile for ast::Quote {
    type Error<'a> = CompileQuoteError<'a>;

    async fn compile<'a>(
        exprs: &[SExpr<'a>],
        _env: &Gc<Env>,
        _binds: Arc<Binds>,
        span: &Span<'a>,
    ) -> Result<Self, CompileQuoteError<'a>> {
        match exprs {
            [] => Err(CompileQuoteError::ExpectedArgument(span.clone())),
            [expr, SExpr::Nil { .. }] => Ok(ast::Quote {
                val: Value::from_sexpr(expr),
            }),
            [_, arg, ..] => Err(CompileQuoteError::UnexpectedArgument(arg.span().clone())),
            _ => Err(CompileQuoteError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug)]
pub enum CompileSetError<'a> {
    ExpectedArgument(Span<'a>),
    ExpectedIdent(Span<'a>),
    UnexpectedArgument(Span<'a>),
    BadForm(Span<'a>),
    CompileError(Box<CompileError<'a>>),
}

impl_from_compile_error!(CompileSetError);

#[async_trait]
impl Compile for ast::Set {
    type Error<'a> = CompileSetError<'a>;

    async fn compile<'a>(
        exprs: &[SExpr<'a>],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span<'a>,
    ) -> Result<Self, CompileSetError<'a>> {
        match exprs {
            [] => Err(CompileSetError::ExpectedArgument(span.clone())),
            [SExpr::Identifier { ident, .. }, expr, SExpr::Nil { .. }] => Ok(ast::Set {
                var: ident.clone(),
                val: expr.compile(env, binds.clone()).await?,
            }),
            [arg1, _, SExpr::Nil { .. }] => {
                Err(CompileSetError::ExpectedIdent(arg1.span().clone()))
            }
            [_, _, arg3, ..] => Err(CompileSetError::UnexpectedArgument(arg3.span().clone())),
            _ => Err(CompileSetError::BadForm(span.clone())),
        }
    }
}
