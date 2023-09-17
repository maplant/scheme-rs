use crate::{
    ast::{self, Ident},
    eval::{Env, Eval, Value},
    expand::{Binds, Pattern, SyntaxRule, Template},
    gc::Gc,
    syntax::{Syntax, Span},
};
use async_trait::async_trait;
use derive_more::From;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(From, Debug)]
pub enum CompileError {
    UnexpectedEmptyList(Span),
    CompileFuncCallError(CompileFuncCallError),
    CompileDefineError(CompileDefineError),
    CompileIfError(CompileIfError),
    CompileLetError(CompileLetError),
    CompileDefineSyntaxError(CompileDefineSyntaxError),
    CompileBodyError(CompileBodyError),
    CompileQuoteError(CompileQuoteError),
    CompileSetError(CompileSetError),
    CompileLambdaError(CompileLambdaError),
}

macro_rules! impl_from_compile_error {
    ( $error:ident ) => {
        impl From<CompileError> for $error {
            fn from(ce: CompileError) -> Self {
                Self::CompileError(Box::new(ce))
            }
        }
    };
}

#[async_trait]
pub trait Compile: Eval + Sized + 'static
where
    CompileError: From<Self::Error>,
{
    type Error;

    async fn compile(
        exprs: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span,
    ) -> Result<Self, Self::Error>;

    async fn compile_to_expr(
        exprs: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span,
    ) -> Result<Box<dyn Eval>, CompileError> {
        Ok(Box::new(Self::compile(exprs, env, binds, span).await?))
    }
}

#[derive(From, Debug)]
pub enum CompileFuncCallError {
    EmptyFunctionCall(Span),
    CompileError(Box<CompileError>),
}

impl_from_compile_error!(CompileFuncCallError);

#[async_trait]
impl Compile for ast::Call {
    type Error = CompileFuncCallError;

    async fn compile(
        exprs: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span,
    ) -> Result<ast::Call, CompileFuncCallError> {
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
pub enum CompileDefineError {
    ParameterDefinedMultipleTimes {
        ident: Ident,
        first: Span,
        second: Span,
    },
    ExpectedIdentifier(Span),
    CompileBodyError(CompileBodyError),
    BadForm(Span),
    CompileError(Box<CompileError>),
}

impl_from_compile_error!(CompileDefineError);

#[async_trait]
impl Compile for ast::Define {
    type Error = CompileDefineError;

    async fn compile(
        exprs: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span,
    ) -> Result<Self, Self::Error> {
        match exprs {
            [Syntax::Identifier { ident, .. }, expr] => Ok(ast::Define::DefineVar(ast::DefineVar {
                name: ident.clone(),
                val: expr.compile(env, binds).await?,
            })),
            [Syntax::List { list, span }, body @ ..] => {
                match &list[..] {
                    [] => Err(CompileDefineError::ExpectedIdentifier(span.clone())),
                    [Syntax::Identifier {
                        ident: func_name,
                        span: func_span,
                    }, args @ ..] => {
                        let mut scope = Binds::new_local(&binds);
                        let mut bound = HashMap::<Ident, Span>::new();
                        let mut fixed = Vec::new();
                        for arg in &args[..args.len() - 1] {
                            match arg {
                                Syntax::Identifier { ident, span } => {
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
                                Syntax::Nil { .. } => {
                                    ast::Formals::FixedArgs(fixed.into_iter().collect())
                                }
                                Syntax::Identifier { ident, span } => {
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
pub enum CompileIfError {
    ExpectedConditional(Span),
    ExpectedArgumentAfterConditional(Span),
    UnexpectedArgument(Span),
    CompileError(Box<CompileError>),
}

impl_from_compile_error!(CompileIfError);

#[async_trait]
impl Compile for ast::If {
    type Error = CompileIfError;

    async fn compile(
        exprs: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span,
    ) -> Result<Self, CompileIfError> {
        match exprs {
            [cond, success, Syntax::Nil { .. }] => Ok(ast::If {
                cond: cond.compile(env, binds.clone()).await?,
                success: success.compile(env, binds.clone()).await?,
                failure: None,
            }),
            [cond, success, failure, Syntax::Nil { .. }] => Ok(ast::If {
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
pub enum CompileBodyError {
    EmptyBody(Span),
    CompileError(Box<CompileError>),
}

impl_from_compile_error!(CompileBodyError);

#[async_trait]
impl Compile for ast::Body {
    type Error = CompileBodyError;

    async fn compile(
        exprs: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span,
    ) -> Result<Self, CompileBodyError> {
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
pub enum CompileLetError {
    BadForm(Span),
    CompileBodyError(CompileBodyError),
    CompileLetBindingError(CompileLetBindingError),
}

#[async_trait]
impl Compile for ast::Let {
    type Error = CompileLetError;

    async fn compile(
        expr: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span,
    ) -> Result<Self, CompileLetError> {
        match expr {
            [Syntax::List { list: bindings, .. }, body @ ..] => {
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
pub enum CompileLetBindingError {
    BadForm(Span),
    PreviouslyBound {
        ident: Ident,
        first: Span,
        second: Span,
    },
    NotAList(Span),
    CompileError(Box<CompileError>),
}

impl_from_compile_error!(CompileLetBindingError);

struct LetBinding {
    ident: Ident,
    span: Span,
    expr: Box<dyn Eval>,
}

impl LetBinding {
    async fn compile(
        expr: &Syntax,
        env: &Gc<Env>,
        binds: Arc<Binds>,
        previously_bound: &HashMap<Ident, Span>,
    ) -> Result<LetBinding, CompileLetBindingError> {
        match expr {
            Syntax::List { list, span } => match &list[..] {
                [Syntax::Identifier {
                    ident,
                    span: bind_span,
                }, expr, Syntax::Nil { .. }] => {
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
pub enum CompileDefineSyntaxError {
    BadForm(Span),
}

#[async_trait]
impl Compile for ast::DefineSyntax {
    type Error = CompileDefineSyntaxError;

    async fn compile(
        expr: &[Syntax],
        _env: &Gc<Env>,
        _binds: Arc<Binds>,
        span: &Span,
    ) -> Result<ast::DefineSyntax, CompileDefineSyntaxError> {
        match expr {
            [Syntax::Identifier {
                ident: macro_name, ..
            }, Syntax::List {
                list: syntax_rules, ..
            }, Syntax::Nil { .. }] => {
                let (mut keywords, mut rules) = match &syntax_rules[..] {
                    [Syntax::Identifier { ident, .. }, Syntax::List {
                        list: keywords_list,
                        ..
                    }, Syntax::List { list: rules, .. }, Syntax::Nil { .. }]
                        if ident.sym == "syntax-rules" =>
                    {
                        let mut keywords = HashSet::default();
                        // TODO: ensure keywords_list is proper
                        for keyword in &keywords_list[..keywords_list.len() - 1] {
                            if let Syntax::Identifier { ident, .. } = keyword {
                                keywords.insert(ident.sym.clone());
                            } else {
                                return Err(CompileDefineSyntaxError::BadForm(
                                    keyword.span().clone(),
                                ));
                            }
                        }
                        (keywords, &rules[..])
                    }
                    [Syntax::Identifier { ident, .. }, Syntax::Nil { .. }, Syntax::List { list: rules, .. }, Syntax::Nil { .. }]
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
                        [Syntax::Nil { .. }] => break,
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
    type Error = CompileError;

    async fn compile(
        exprs: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        _span: &Span,
    ) -> Result<Self, CompileError> {
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
    type Error = CompileError;

    async fn compile(
        exprs: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        _span: &Span,
    ) -> Result<Self, CompileError> {
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
pub enum CompileQuoteError {
    ExpectedArgument(Span),
    UnexpectedArgument(Span),
    BadForm(Span),
}

#[async_trait]
impl Compile for ast::Quote {
    type Error = CompileQuoteError;

    async fn compile(
        exprs: &[Syntax],
        _env: &Gc<Env>,
        _binds: Arc<Binds>,
        span: &Span,
    ) -> Result<Self, CompileQuoteError> {
        match exprs {
            [] => Err(CompileQuoteError::ExpectedArgument(span.clone())),
            [expr, Syntax::Nil { .. }] => Ok(ast::Quote {
                val: Value::from_syntax(expr),
            }),
            [_, arg, ..] => Err(CompileQuoteError::UnexpectedArgument(arg.span().clone())),
            _ => Err(CompileQuoteError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug)]
pub enum CompileSetError {
    ExpectedArgument(Span),
    ExpectedIdent(Span),
    UnexpectedArgument(Span),
    BadForm(Span),
    CompileError(Box<CompileError>),
}

impl_from_compile_error!(CompileSetError);

#[async_trait]
impl Compile for ast::Set {
    type Error = CompileSetError;

    async fn compile(
        exprs: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span,
    ) -> Result<Self, CompileSetError> {
        match exprs {
            [] => Err(CompileSetError::ExpectedArgument(span.clone())),
            [Syntax::Identifier { ident, .. }, expr, Syntax::Nil { .. }] => Ok(ast::Set {
                var: ident.clone(),
                val: expr.compile(env, binds.clone()).await?,
            }),
            [arg1, _, Syntax::Nil { .. }] => {
                Err(CompileSetError::ExpectedIdent(arg1.span().clone()))
            }
            [_, _, arg3, ..] => Err(CompileSetError::UnexpectedArgument(arg3.span().clone())),
            _ => Err(CompileSetError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug)]
pub enum CompileLambdaError {
    ExpectedList(Span),
    ExpectedIdentifier(Span),
    CompileBodyError(CompileBodyError),
    ParameterDefinedMultipleTimes {
        ident: Ident,
        first: Span,
        second: Span,
    },
}

#[async_trait]
impl Compile for ast::Lambda {
    type Error = CompileLambdaError;

    async fn compile(
        exprs: &[Syntax],
        env: &Gc<Env>,
        binds: Arc<Binds>,
        span: &Span,
    ) -> Result<Self, CompileLambdaError> {
        match exprs {
            [Syntax::List {
                list: args,
                ..
            }, body @ ..] => {
                let mut scope = Binds::new_local(&binds);
                let mut bound = HashMap::<Ident, Span>::new();
                let mut fixed = Vec::new();
                for arg in &args[..args.len() - 1] {
                    match arg {
                        Syntax::Identifier { ident, span } => {
                            if let Some(prev_span) = bound.get(ident) {
                                return Err(CompileLambdaError::ParameterDefinedMultipleTimes {
                                    ident: ident.clone(),
                                    first: prev_span.clone(),
                                    second: span.clone(),
                                });
                            }
                            scope.bind(&ident.sym);
                            bound.insert(ident.clone(), span.clone());
                            fixed.push(ident.clone());
                        }
                        x => return Err(CompileLambdaError::ExpectedIdentifier(x.span().clone())),
                    }
                }

                let args = if let Some(last) = args.last() {
                    match last {
                        Syntax::Nil { .. } => ast::Formals::FixedArgs(fixed.into_iter().collect()),
                        Syntax::Identifier { ident, span } => {
                            if let Some(prev_span) = bound.get(ident) {
                                return Err(CompileLambdaError::ParameterDefinedMultipleTimes {
                                    ident: ident.clone(),
                                    first: prev_span.clone(),
                                    second: span.clone(),
                                });
                            }
                            scope.bind(&ident.sym);
                            ast::Formals::VarArgs {
                                fixed: fixed.into_iter().collect(),
                                remaining: ident.clone(),
                            }
                        }
                        x => return Err(CompileLambdaError::ExpectedIdentifier(x.span().clone())),
                    }
                } else {
                    // If there is no last argument, there are no arguments
                    ast::Formals::FixedArgs(Vec::new())
                };

                let body = ast::Body::compile(body, env, Arc::new(scope), span)
                    .await
                    .map_err(CompileLambdaError::CompileBodyError)?;
                Ok(ast::Lambda { args, body })
            }
            _ => todo!(),
        }
    }
}
