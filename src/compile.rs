use crate::{
    ast,
    continuation::Continuation,
    env::Env,
    error::RuntimeError,
    eval::Eval,
    expand::{SyntaxRule, Transformer},
    gc::Gc,
    syntax::{Identifier, Span, Syntax},
    util::{ArcSlice, RequireOne},
    value::Value,
};
use async_trait::async_trait;
use derive_more::From;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(From, Debug, Clone)]
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
    CompileDefineRecordTypeError(crate::records::CompileDefineRecordTypeError),
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

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, Self::Error>;

    async fn compile_to_expr(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Arc<dyn Eval>, CompileError> {
        Ok(Arc::new(Self::compile(exprs, env, cont, span).await?))
    }
}

#[derive(Debug, Clone)]
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
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, CompileBodyError> {
        if exprs.is_empty() {
            return Err(CompileBodyError::EmptyBody(span.clone()));
        }
        let mut output = Vec::new();
        for expr in &exprs[..exprs.len() - 1] {
            output.push(expr.compile(env, cont).await?);
        }
        // TODO: what if the body isn't a proper list?
        Ok(ast::Body::new(output))
    }
}

#[derive(Debug, Clone)]
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
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, CompileLetError> {
        match expr {
            [Syntax::Null { .. }, body @ ..] => compile_let(None, &[], body, env, cont, span).await,
            [Syntax::List { list: bindings, .. }, body @ ..] => {
                compile_let(None, bindings, body, env, cont, span).await
            }
            // Named let:
            [Syntax::Identifier { ident, .. }, Syntax::List { list: bindings, .. }, body @ ..] => {
                compile_let(Some(ident), bindings, body, env, cont, span).await
            }
            [Syntax::Identifier { ident, .. }, Syntax::Null { .. }, body @ ..] => {
                compile_let(Some(ident), &[], body, env, cont, span).await
            }
            _ => Err(CompileLetError::BadForm(span.clone())),
        }
    }
}

async fn compile_let(
    name: Option<&Identifier>,
    bindings: &[Syntax],
    body: &[Syntax],
    env: &Env,
    cont: &Option<Arc<Continuation>>,
    span: &Span,
) -> Result<ast::Let, CompileLetError> {
    let mut previously_bound = HashMap::new();
    let mut new_contour = env.new_lexical_contour();
    let mut compiled_bindings = Vec::new();
    // TODO: Check that the list of bindings is proper
    if !bindings.is_empty() {
        for binding in &bindings[..bindings.len() - 1] {
            let binding = LetBinding::compile(binding, env, cont, &previously_bound)
                .await
                .map_err(CompileLetError::CompileLetBindingError)?;
            previously_bound.insert(binding.ident.clone(), binding.span.clone());
            new_contour.def_var(&binding.ident, Gc::new(Value::Undefined));
            compiled_bindings.push(binding);
        }
    }

    let env = Gc::new(new_contour);
    let body = ast::Body::compile(body, &Env::from(env.clone()), cont, span)
        .await
        .map_err(CompileLetError::CompileBodyError)?;

    let mut bindings: Vec<_> = compiled_bindings
        .into_iter()
        .map(|binding| (binding.ident, binding.expr))
        .collect();

    // If this is a named let, add a binding for a procedure with the same
    // body and args of the formals:
    if let Some(name) = name {
        let lambda = ast::Lambda {
            args: ast::Formals::FixedArgs(
                bindings.iter().map(|(ident, _)| ident.clone()).collect(),
            ),
            body: body.clone(),
        };
        bindings.push((name.clone(), Arc::new(lambda)));
    }

    Ok(ast::Let {
        bindings: Arc::from(bindings),
        body,
    })
}

#[derive(Debug, Clone)]
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
        expr: &Syntax,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        previously_bound: &HashMap<Identifier, Span>,
    ) -> Result<LetBinding, CompileLetBindingError> {
        match expr {
            Syntax::List { list, span } => match &list[..] {
                [Syntax::Identifier {
                    ident,
                    span: bind_span,
                    ..
                }, expr, Syntax::Null { .. }] => {
                    if let Some(prev_bind) = previously_bound.get(ident) {
                        return Err(CompileLetBindingError::PreviouslyBound {
                            ident: ident.clone(),
                            first: prev_bind.clone(),
                            second: bind_span.clone(),
                        });
                    }

                    let expr = expr.compile(env, cont).await?;

                    Ok(LetBinding {
                        ident: ident.clone(),
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

#[derive(From, Debug, Clone)]
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
        cont: &Option<Arc<Continuation>>,
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
                let operator = operator.compile(env, cont).await?;
                let mut compiled_args = vec![operator];
                for arg in &args[..args.len() - 1] {
                    compiled_args.push(arg.compile(env, cont).await?);
                }
                // TODO: what if it's not a proper list?
                Ok(ast::Call {
                    // operator,
                    args: ArcSlice::from(compiled_args),
                    location: span.clone(),
                    proc_name,
                })
            }
            [] => Err(CompileFuncCallError::EmptyFunctionCall(span.clone())),
        }
    }
}

#[derive(Debug, Clone)]
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
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, CompileIfError> {
        match exprs {
            [cond, success, Syntax::Null { .. }] => Ok(ast::If {
                cond: cond.compile(env, cont).await?,
                success: success.compile(env, cont).await?,
                failure: None,
            }),
            [cond, success, failure, Syntax::Null { .. }] => Ok(ast::If {
                cond: cond.compile(env, cont).await?,
                success: success.compile(env, cont).await?,
                failure: Some(failure.compile(env, cont).await?),
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

#[derive(Debug, Clone)]
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

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, Self::Error> {
        match exprs {
            [Syntax::Identifier { ident, .. }, expr, Syntax::Null { .. }] => {
                Ok(ast::Define::DefineVar(ast::DefineVar {
                    name: ident.clone(),
                    val: expr.compile(env, cont).await?,
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
                                Syntax::Null { .. } => {
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
                                    let remaining = ident.clone();
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
                        let body = ast::Body::compile(body, env, cont, func_span)
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

#[derive(Debug, Clone)]
pub enum CompileDefineSyntaxError {
    BadForm(Span),
    CompileError(Box<CompileError>),
    RuntimeError(Box<RuntimeError>),
}

impl_from_compile_error!(CompileDefineSyntaxError);

impl From<RuntimeError> for CompileDefineSyntaxError {
    fn from(value: RuntimeError) -> Self {
        Self::RuntimeError(Box::new(value))
    }
}

#[async_trait]
impl Compile for ast::DefineSyntax {
    type Error = CompileDefineSyntaxError;

    async fn compile(
        expr: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<ast::DefineSyntax, CompileDefineSyntaxError> {
        match expr {
            [Syntax::Identifier { ident, .. }, expr, Syntax::Null { .. }] => {
                env.def_macro(
                    ident,
                    expr.compile(env, cont)
                        .await?
                        .eval(env, cont)
                        .await?
                        .require_one()?,
                );
                Ok(ast::DefineSyntax)
            }
            _ => Err(CompileDefineSyntaxError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug, Clone)]
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
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, CompileQuoteError> {
        match exprs {
            [] => Err(CompileQuoteError::ExpectedArgument(span.clone())),
            [Syntax::Null { .. }] => Ok(ast::Quote { val: Value::Null }),
            [expr, Syntax::Null { .. }] => Ok(ast::Quote {
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

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        _span: &Span,
    ) -> Result<Self, CompileError> {
        let mut output = Vec::new();
        // TODO: what if the arguments aren't a proper list?
        for expr in &exprs[..exprs.len() - 1] {
            let expr = expr.compile(env, cont).await?;
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
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        _span: &Span,
    ) -> Result<Self, CompileError> {
        let mut output = Vec::new();
        // TODO: what if the arguments aren't a proper list?
        for expr in &exprs[..exprs.len() - 1] {
            let expr = expr.compile(env, cont).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

#[derive(Debug, Clone)]
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
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, CompileSetError> {
        // TODO: We need to check if the identifier is defined as a variable transformer
        match exprs {
            [] => Err(CompileSetError::ExpectedArgument(span.clone())),
            [Syntax::Identifier { ident, .. }, expr, Syntax::Null { .. }] => Ok(ast::Set {
                var: ident.clone(),
                val: expr.compile(env, cont).await?,
            }),
            [arg1, _, Syntax::Null { .. }] => {
                Err(CompileSetError::ExpectedIdent(arg1.span().clone()))
            }
            [_, _, arg3, ..] => Err(CompileSetError::UnexpectedArgument(arg3.span().clone())),
            _ => Err(CompileSetError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompileSyntaxError {
    ExpectedArgument(Span),
    UnexpectedArgument(Span),
    BadForm(Span),
}

#[async_trait]
impl Compile for ast::SyntaxQuote {
    type Error = CompileSyntaxError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, CompileSyntaxError> {
        match exprs {
            [] => Err(CompileSyntaxError::ExpectedArgument(span.clone())),
            [expr, Syntax::Null { .. }] => Ok(ast::SyntaxQuote {
                syn: expr.clone(),
                env: env.clone(),
            }),
            [_, arg, ..] => Err(CompileSyntaxError::UnexpectedArgument(arg.span().clone())),
            _ => Err(CompileSyntaxError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug, Clone)]
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

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, CompileLambdaError> {
        match exprs {
            [Syntax::Null { .. }, body @ ..] => compile_lambda(&[], body, env, cont, span).await,
            [Syntax::List { list: args, .. }, body @ ..] => {
                compile_lambda(args, body, env, cont, span).await
            }
            [Syntax::Identifier { ident, .. }, body @ ..] => {
                let mut env = env.new_lexical_contour();
                env.def_var(ident, Gc::new(Value::Undefined));

                let body = ast::Body::compile(body, &Env::from(Gc::new(env)), cont, span)
                    .await
                    .map_err(CompileLambdaError::CompileBodyError)?;

                Ok(ast::Lambda {
                    args: ast::Formals::VarArgs {
                        fixed: Vec::new(),
                        remaining: ident.clone(),
                    },
                    body,
                })
            }
            _ => todo!(),
        }
    }
}

async fn compile_lambda(
    args: &[Syntax],
    body: &[Syntax],
    env: &Env,
    cont: &Option<Arc<Continuation>>,
    span: &Span,
) -> Result<ast::Lambda, CompileLambdaError> {
    let mut bound = HashMap::<Identifier, Span>::new();
    let mut fixed = Vec::new();
    if !args.is_empty() {
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
                    fixed.push(ident.clone());
                }
                x => return Err(CompileLambdaError::ExpectedIdentifier(x.span().clone())),
            }
        }
    }
    let args = if let Some(last) = args.last() {
        match last {
            Syntax::Null { .. } => ast::Formals::FixedArgs(fixed.into_iter().collect()),
            Syntax::Identifier { ident, span, .. } => {
                if let Some(prev_span) = bound.get(ident) {
                    return Err(CompileLambdaError::ParameterDefinedMultipleTimes {
                        ident: ident.clone(),
                        first: prev_span.clone(),
                        second: span.clone(),
                    });
                }
                let remaining = ident.clone();
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

    let mut env = env.new_lexical_contour();
    for bound in bound.into_keys() {
        env.def_var(&bound, Gc::new(Value::Undefined));
    }

    let body = ast::Body::compile(body, &Env::from(Gc::new(env)), cont, span)
        .await
        .map_err(CompileLambdaError::CompileBodyError)?;
    Ok(ast::Lambda { args, body })
}

#[derive(Debug, Clone)]
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
        cont: &Option<Arc<Continuation>>,
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
            [arg, Syntax::Null { .. }, rules @ ..] => (arg, HashSet::default(), rules),
            _ => return Err(CompileSyntaxCaseError::BadForm(span.clone())),
        };
        let mut syntax_rules = Vec::new();
        loop {
            match rules {
                [Syntax::Null { .. }] => break,
                [Syntax::List { list, .. }, tail @ ..] => match &list[..] {
                    [pattern, template, Syntax::Null { .. }] => {
                        syntax_rules.push(SyntaxRule::compile(&keywords, pattern, template));
                        rules = tail;
                    }
                    _ => return Err(CompileSyntaxCaseError::BadForm(span.clone())),
                },
                _ => return Err(CompileSyntaxCaseError::BadForm(span.clone())),
            }
        }
        Ok(ast::SyntaxCase {
            arg: arg.compile(env, cont).await?,
            transformer: Transformer {
                rules: syntax_rules,
                is_variable_transformer: false,
            },
        })
    }
}
