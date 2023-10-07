use crate::{
    ast,
    env::Env,
    error::RuntimeError,
    eval::Eval,
    expand::{Pattern, SyntaxRule, Template, Transformer},
    gc::Gc,
    syntax::{Identifier, Mark, Span, Syntax},
    value::Value,
};
use async_trait::async_trait;
use derive_more::From;
use std::{sync::Arc, collections::{HashMap, HashSet}};

#[derive(From, Debug)]
pub enum CompileError {
    UnexpectedEmptyList(Span),
    UndefinedVariable(Identifier),
    RuntimeError(Box<RuntimeError>),
    NotVariableTransformer,
    CompileBodyError(CompileBodyError),
    CompileLetError(CompileLetError),
    CompileFuncCallError(CompileFuncCallError),
    CompileIfError(CompileIfError),
    CompileDefineError(CompileDefineError),
    CompileDefineSyntaxError(CompileDefineSyntaxError),
    CompileQuoteError(CompileQuoteError),
    CompileSetError(CompileSetError),
    CompileLambdaError(CompileLambdaError),
    CompileSyntaxError(CompileSyntaxError),
    CompileSyntaxCaseError(CompileSyntaxCaseError),
    CompileSyntaxRulesError(CompileSyntaxRulesError),
}

impl From<RuntimeError> for CompileError {
    fn from(re: RuntimeError) -> Self {
        Self::RuntimeError(Box::new(re))
    }
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

    async fn compile(exprs: &[Syntax], env: &Env, span: &Span) -> Result<Self, Self::Error>;

    async fn compile_to_expr(
        exprs: &[Syntax],
        env: &Env,
        span: &Span,
    ) -> Result<Arc<dyn Eval>, CompileError> {
        Ok(Arc::new(Self::compile(exprs, env, span).await?))
    }
}

#[derive(Debug)]
pub enum CompileBodyError {
    EmptyBody(Span),
}

#[async_trait]
impl Compile for ast::Body {
    type Error = CompileBodyError;

    async fn compile(exprs: &[Syntax], _env: &Env, span: &Span) -> Result<Self, CompileBodyError> {
        if exprs.is_empty() {
            return Err(CompileBodyError::EmptyBody(span.clone()));
        }
        // TODO: what if the body isn't a proper list?
        Ok(ast::Body::new(exprs[..exprs.len() - 1].to_vec()))
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

    async fn compile(expr: &[Syntax], env: &Env, span: &Span) -> Result<Self, CompileLetError> {
        match expr {
            [Syntax::List { list: bindings, .. }, body @ ..] => {
                let mut previously_bound = HashMap::new();
                let mut new_contour = env.new_lexical_contour(Mark::new());
                let mut compiled_bindings = Vec::new();
                // TODO: Check that the list of bindings is proper
                for binding in &bindings[..bindings.len() - 1] {
                    let binding =
                        LetBinding::compile(new_contour.mark, binding, env, &previously_bound)
                            .await
                            .map_err(CompileLetError::CompileLetBindingError)?;
                    previously_bound.insert(binding.ident.clone(), binding.span.clone());
                    new_contour.def_var(&binding.ident, Gc::new(Value::Nil));
                    compiled_bindings.push(binding);
                }
                let mut body = body.to_vec();
                for item in &mut body {
                    item.mark(new_contour.mark);
                }
                let env = Gc::new(new_contour);
                let body = ast::Body::compile(&body, &Env::from(env.clone()), span)
                    .await
                    .map_err(CompileLetError::CompileBodyError)?;
                Ok(ast::Let {
                    scope: env,
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
        ident: Identifier,
        first: Span,
        second: Span,
    },
    NotAList(Span),
    CompileError(Box<CompileError>),
}

impl_from_compile_error!(CompileLetBindingError);

struct LetBinding {
    ident: Identifier,
    span: Span,
    expr: Arc<dyn Eval>,
}

impl LetBinding {
    async fn compile(
        mark: Mark,
        expr: &Syntax,
        env: &Env,
        previously_bound: &HashMap<Identifier, Span>,
    ) -> Result<LetBinding, CompileLetBindingError> {
        match expr {
            Syntax::List { list, span } => match &list[..] {
                [Syntax::Identifier {
                    ident,
                    span: bind_span,
                    ..
                }, expr, Syntax::Nil { .. }] => {
                    let mut ident = ident.clone();
                    ident.mark(mark);
                    if let Some(prev_bind) = previously_bound.get(&ident) {
                        return Err(CompileLetBindingError::PreviouslyBound {
                            ident,
                            first: prev_bind.clone(),
                            second: bind_span.clone(),
                        });
                    }

                    let expr = expr.compile(env).await?;

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
        env: &Env,
        span: &Span,
    ) -> Result<ast::Call, CompileFuncCallError> {
        match exprs {
            [operator, args @ ..] => {
                // TODO: Support macro expansions in the call position that eventually
                // resolve into an identifier, that is a macro (or function)
                let proc_name = match operator {
                    Syntax::Identifier { ident, .. } => ident.name.clone(),
                    _ => String::from("<lambda>"),
                };
                let operator = operator.compile(env).await?;
                let mut compiled_args = Vec::new();
                for arg in &args[..args.len() - 1] {
                    compiled_args.push(arg.compile(env).await?);
                }
                // TODO: what if it's not a proper list?
                Ok(ast::Call {
                    operator,
                    args: compiled_args,
                    location: span.clone(),
                    proc_name,
                })
            }
            [] => Err(CompileFuncCallError::EmptyFunctionCall(span.clone())),
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

    async fn compile(exprs: &[Syntax], env: &Env, span: &Span) -> Result<Self, CompileIfError> {
        match exprs {
            [cond, success, Syntax::Nil { .. }] => Ok(ast::If {
                cond: cond.compile(env).await?,
                success: success.compile(env).await?,
                failure: None,
            }),
            [cond, success, failure, Syntax::Nil { .. }] => Ok(ast::If {
                cond: cond.compile(env).await?,
                success: success.compile(env).await?,
                failure: Some(failure.compile(env).await?),
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
pub enum CompileDefineError {
    ParameterDefinedMultipleTimes {
        ident: Identifier,
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

    async fn compile(exprs: &[Syntax], env: &Env, span: &Span) -> Result<Self, Self::Error> {
        match exprs {
            [Syntax::Identifier { ident, .. }, expr, Syntax::Nil { .. }] => {
                Ok(ast::Define::DefineVar(ast::DefineVar {
                    name: ident.clone(),
                    val: expr.compile(env).await?,
                }))
            }
            [Syntax::List { list, span }, body @ ..] => {
                match &list[..] {
                    [] => Err(CompileDefineError::ExpectedIdentifier(span.clone())),
                    [Syntax::Identifier {
                        ident: func_name,
                        span: func_span,
                        ..
                    }, args @ ..] => {
                        let mut bound = HashMap::<Identifier, Span>::new();
                        let mut fixed = Vec::new();
                        let new_mark = Mark::new();
                        for arg in &args[..args.len() - 1] {
                            match arg {
                                Syntax::Identifier { ident, span, .. } => {
                                    if let Some(prev_span) = bound.get(ident) {
                                        return Err(
                                            CompileDefineError::ParameterDefinedMultipleTimes {
                                                ident: ident.clone(),
                                                first: prev_span.clone(),
                                                second: span.clone(),
                                            },
                                        );
                                    }
                                    bound.insert(ident.clone(), span.clone());
                                    let mut ident = ident.clone();
                                    ident.mark(new_mark);
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
                                Syntax::Identifier { ident, span, .. } => {
                                    if let Some(prev_span) = bound.get(ident) {
                                        return Err(
                                            CompileDefineError::ParameterDefinedMultipleTimes {
                                                ident: ident.clone(),
                                                first: prev_span.clone(),
                                                second: span.clone(),
                                            },
                                        );
                                    }
                                    let mut remaining = ident.clone();
                                    remaining.mark(new_mark);
                                    ast::Formals::VarArgs {
                                        fixed: fixed.into_iter().collect(),
                                        remaining,
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
                        let mut body = body.to_vec();
                        for item in &mut body {
                            item.mark(new_mark);
                        }
                        let body = ast::Body::compile(&body, env, func_span)
                            .await
                            .map_err(CompileDefineError::CompileBodyError)?;
                        Ok(ast::Define::DefineFunc(ast::DefineFunc {
                            name: func_name.clone(),
                            args,
                            body,
                            mark: new_mark,
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
pub enum CompileDefineSyntaxError {
    BadForm(Span),
    CompileError(Box<CompileError>),
}

impl_from_compile_error!(CompileDefineSyntaxError);

#[async_trait]
impl Compile for ast::DefineSyntax {
    type Error = CompileDefineSyntaxError;

    async fn compile(
        expr: &[Syntax],
        env: &Env,
        span: &Span,
    ) -> Result<ast::DefineSyntax, CompileDefineSyntaxError> {
        match expr {
            [Syntax::Identifier { ident, .. }, expr, Syntax::Nil { .. }] => Ok(ast::DefineSyntax {
                name: ident.clone(),
                transformer: expr.compile(env).await?,
            }),
            _ => Err(CompileDefineSyntaxError::BadForm(span.clone())),
        }
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

    async fn compile(exprs: &[Syntax], _env: &Env, span: &Span) -> Result<Self, CompileQuoteError> {
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

#[async_trait]
impl Compile for ast::And {
    type Error = CompileError;

    async fn compile(exprs: &[Syntax], env: &Env, _span: &Span) -> Result<Self, CompileError> {
        let mut output = Vec::new();
        // TODO: what if the arguments aren't a proper list?
        for expr in &exprs[..exprs.len() - 1] {
            let expr = expr.compile(env).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

#[async_trait]
impl Compile for ast::Or {
    type Error = CompileError;

    async fn compile(exprs: &[Syntax], env: &Env, _span: &Span) -> Result<Self, CompileError> {
        let mut output = Vec::new();
        // TODO: what if the arguments aren't a proper list?
        for expr in &exprs[..exprs.len() - 1] {
            let expr = expr.compile(env).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
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

    async fn compile(exprs: &[Syntax], env: &Env, span: &Span) -> Result<Self, CompileSetError> {
        // TODO: We need to check if the identifier is defined as a variable transformer
        match exprs {
            [] => Err(CompileSetError::ExpectedArgument(span.clone())),
            [Syntax::Identifier { ident, .. }, expr, Syntax::Nil { .. }] => Ok(ast::Set {
                var: ident.clone(),
                val: expr.compile(env).await?,
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
pub enum CompileSyntaxError {
    ExpectedArgument(Span),
    UnexpectedArgument(Span),
    BadForm(Span),
}

#[async_trait]
impl Compile for ast::SyntaxQuote {
    type Error = CompileSyntaxError;

    async fn compile(exprs: &[Syntax], env: &Env, span: &Span) -> Result<Self, CompileSyntaxError> {
        match exprs {
            [] => Err(CompileSyntaxError::ExpectedArgument(span.clone())),
            [expr, Syntax::Nil { .. }] => Ok(ast::SyntaxQuote {
                syn: expr.clone(),
                env: env.clone(),
            }),
            [_, arg, ..] => Err(CompileSyntaxError::UnexpectedArgument(arg.span().clone())),
            _ => Err(CompileSyntaxError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug)]
pub enum CompileLambdaError {
    ExpectedList(Span),
    ExpectedIdentifier(Span),
    CompileBodyError(CompileBodyError),
    ParameterDefinedMultipleTimes {
        ident: Identifier,
        first: Span,
        second: Span,
    },
}

#[async_trait]
impl Compile for ast::Lambda {
    type Error = CompileLambdaError;

    async fn compile(exprs: &[Syntax], env: &Env, span: &Span) -> Result<Self, CompileLambdaError> {
        match exprs {
            [Syntax::List { list: args, .. }, body @ ..] => {
                let mut bound = HashMap::<Identifier, Span>::new();
                let mut fixed = Vec::new();
                let new_mark = Mark::new();
                for arg in &args[..args.len() - 1] {
                    match arg {
                        Syntax::Identifier { ident, span, .. } => {
                            if let Some(prev_span) = bound.get(ident) {
                                return Err(CompileLambdaError::ParameterDefinedMultipleTimes {
                                    ident: ident.clone(),
                                    first: prev_span.clone(),
                                    second: span.clone(),
                                });
                            }
                            bound.insert(ident.clone(), span.clone());
                            let mut ident = ident.clone();
                            ident.mark(new_mark);
                            fixed.push(ident);
                        }
                        x => return Err(CompileLambdaError::ExpectedIdentifier(x.span().clone())),
                    }
                }

                let args = if let Some(last) = args.last() {
                    match last {
                        Syntax::Nil { .. } => ast::Formals::FixedArgs(fixed.into_iter().collect()),
                        Syntax::Identifier { ident, span, .. } => {
                            if let Some(prev_span) = bound.get(ident) {
                                return Err(CompileLambdaError::ParameterDefinedMultipleTimes {
                                    ident: ident.clone(),
                                    first: prev_span.clone(),
                                    second: span.clone(),
                                });
                            }
                            let mut remaining = ident.clone();
                            remaining.mark(new_mark);
                            ast::Formals::VarArgs {
                                fixed: fixed.into_iter().collect(),
                                remaining,
                            }
                        }
                        x => return Err(CompileLambdaError::ExpectedIdentifier(x.span().clone())),
                    }
                } else {
                    // If there is no last argument, there are no arguments
                    ast::Formals::FixedArgs(Vec::new())
                };

                // This heavily relies on the fact that body defers compilation until it is run,
                // as lambda does not create a lexical contour until it is run.
                let mut body = body.to_vec();
                for item in &mut body {
                    item.mark(new_mark);
                }
                let body = ast::Body::compile(&body, env, span)
                    .await
                    .map_err(CompileLambdaError::CompileBodyError)?;
                Ok(ast::Lambda {
                    args,
                    body,
                    mark: new_mark,
                })
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum CompileSyntaxCaseError {
    CompileError(Box<CompileError>),
    BadForm(Span),
}

impl_from_compile_error!(CompileSyntaxCaseError);

#[async_trait]
impl Compile for ast::SyntaxCase {
    type Error = CompileSyntaxCaseError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        span: &Span,
    ) -> Result<Self, CompileSyntaxCaseError> {
        let (arg, keywords, mut rules) = match exprs {
            [arg, Syntax::List { list, .. }, rules @ ..] => {
                let mut keywords = HashSet::default();
                // TODO: ensure keywords_list is proper
                for keyword in &list[..list.len() - 1] {
                    if let Syntax::Identifier { ident, .. } = keyword {
                        keywords.insert(ident.name.clone());
                    } else {
                        return Err(CompileSyntaxCaseError::BadForm(keyword.span().clone()));
                    }
                }
                (arg, keywords, rules)
            }
            [arg, Syntax::Nil { .. }, rules @ ..] => (arg, HashSet::default(), rules),
            _ => return Err(CompileSyntaxCaseError::BadForm(span.clone())),
        };
        let mut syntax_rules = Vec::new();
        loop {
            match rules {
                [Syntax::Nil { .. }] => break,
                [Syntax::List { list, .. }, tail @ ..] => match &list[..] {
                    [pattern, template, Syntax::Nil { .. }] => {
                        syntax_rules.push(SyntaxRule {
                            pattern: Pattern::compile(pattern, &keywords),
                            template: Template::compile(template),
                        });
                        rules = tail;
                    }
                    _ => return Err(CompileSyntaxCaseError::BadForm(span.clone())),
                },
                _ => return Err(CompileSyntaxCaseError::BadForm(span.clone())),
            }
        }
        Ok(ast::SyntaxCase {
            arg: arg.compile(env).await?,
            transformer: Transformer {
                rules: syntax_rules,
                is_variable_transformer: false,
            },
        })
    }
}

#[derive(Debug)]
pub enum CompileSyntaxRulesError {
    BadForm(Span),
}

#[async_trait]
impl Compile for ast::SyntaxRules {
    type Error = CompileSyntaxRulesError;

    async fn compile(
        exprs: &[Syntax],
        _env: &Env,
        span: &Span,
    ) -> Result<Self, CompileSyntaxRulesError> {
        let (keywords, mut rules) = match exprs {
            [Syntax::List { list, .. }, rules @ ..] => {
                let mut keywords = HashSet::default();
                // TODO: ensure keywords_list is proper
                for keyword in &list[..list.len() - 1] {
                    if let Syntax::Identifier { ident, .. } = keyword {
                        keywords.insert(ident.name.clone());
                    } else {
                        return Err(CompileSyntaxRulesError::BadForm(keyword.span().clone()));
                    }
                }
                (keywords, rules)
            }
            [Syntax::Nil { .. }, rules @ ..] => (HashSet::default(), rules),
            _ => return Err(CompileSyntaxRulesError::BadForm(span.clone())),
        };
        let mut syntax_rules = Vec::new();
        loop {
            match rules {
                [Syntax::Nil { .. }] => break,
                [Syntax::List { list, .. }, tail @ ..] => match &list[..] {
                    [pattern, template, Syntax::Nil { .. }] => {
                        syntax_rules.push(SyntaxRule {
                            pattern: Pattern::compile(pattern, &keywords),
                            template: Template::compile(template),
                        });
                        rules = tail;
                    }
                    _ => return Err(CompileSyntaxRulesError::BadForm(span.clone())),
                },
                _ => return Err(CompileSyntaxRulesError::BadForm(span.clone())),
            }
        }

        Ok(Self {
            transformer: Transformer {
                rules: syntax_rules,
                is_variable_transformer: false,
            },
        })
    }
}
