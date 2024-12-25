//! todo

use crate::{
    continuation::Continuation,
    env::Env,
    error::RuntimeError,
    expand::{SyntaxRule, Transformer},
    gc::Gc,
    syntax::{Expansion, FullyExpanded, Identifier, Span, Syntax},
    util::{ArcSlice, RequireOne},
    value::Value,
};

use super::*;

use async_trait::async_trait;
use derive_more::From;
use futures::future::BoxFuture;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Debug, Clone)]
pub enum ParseAstError {
    BadForm(Span),
    UnexpectedEmptyList(Span),
    UndefinedVariable(Identifier),
    RuntimeError(Box<RuntimeError>),
    NotAVariableTransformer,
    EmptyBody(Span),
    DefInExprContext(Span),
    /*
    ParseLetError(ParseLetError),
    ParseFuncCallError(ParseFuncCallError),
    ParseIfError(ParseIfError),
    ParseDefineError(ParseDefineError),
    ParseDefineSyntaxError(ParseDefineSyntaxError),
    ParseQuoteError(ParseQuoteError),
    ParseSetError(ParseSetError),
    ParseLambdaError(ParseLambdaError),
    ParseSyntaxError(ParseSyntaxError),
    ParseSyntaxCaseError(ParseSyntaxCaseError),
    */
    // ParseDefineRecordTypeError(crate::records::ParseDefineRecordTypeError),
}

impl From<RuntimeError> for ParseAstError {
    fn from(re: RuntimeError) -> Self {
        Self::RuntimeError(Box::new(re))
    }
}

impl Body {
    async fn parse(
        body: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        let mut defs = Vec::new();
        let mut exprs = Vec::new();

        splice_in(&mut defs, &mut exprs, body, env, cont, span).await?;

        let mut defs_parsed = Vec::new();
        let mut exprs_parsed = Vec::new();

        for def in defs.into_iter() {
            defs_parsed.push(
                Definition::parse(&def.expanded, env, cont)
                    .await?
                    .wrap(def.expansion_envs),
            );
        }

        for expr in exprs.into_iter() {
            exprs_parsed.push(
                Expression::parse(&expr.expanded, env, cont)
                    .await?
                    .wrap(expr.expansion_envs),
            );
        }

        Ok(Self::new(defs_parsed, exprs_parsed))
    }
}


fn splice_in<'a>(
    defs: &'a mut Vec<FullyExpanded<'static>>,
    exprs: &'a mut Vec<FullyExpanded<'static>>,
    body: &'a [Syntax],
    env: &'a Env,
    cont: &'a Option<Arc<Continuation>>,
    span: &'a Span,
) -> BoxFuture<'a, Result<(), ParseAstError>>
{
    Box::pin(async move {
        if body.len() == 0 {
            return Err(ParseAstError::EmptyBody(span.clone()));
        }

        let mut expanded = Vec::new();
        for unexpanded in body {
            expanded.push(unexpanded.expand(env, cont).await?);
        }

        for expanded in expanded.into_iter() {
            /*
            let mut is_def;
            (defs, exprs, is_def) = body1(defs, exprs, expanded
            */
            let is_def = {
                match expanded.as_ref().as_list() {
                    Some([Syntax::Identifier { ident, .. }, body @ .., Syntax::Null { .. }])
                        if ident == "begin" =>
                    {
                        splice_in(defs, exprs, body, env, cont, span).await?;
                        continue;
                    }
                    Some([Syntax::Identifier { ident, .. }, expr, Syntax::Null { .. }])
                        if ident == "define-syntax" =>
                    {
                        define_syntax(ident, expr, env, cont).await?;
                        continue;
                    }
                    Some(
                        [Syntax::Identifier { ident, span, .. }, def, .., Syntax::Null { .. }],
                    ) if ident == "define" => {
                        if !exprs.is_empty() {
                            return Err(ParseAstError::DefInExprContext(span.clone()));
                        }

                        let ident = match def.as_list() {
                            Some([Syntax::Identifier { ident, .. }, ..]) => ident,
                            _ => def
                                .as_ident()
                                .ok_or(ParseAstError::BadForm(def.span().clone()))?,
                        };
                        env.def_var(ident, Gc::new(Value::Undefined));
                        true
                    }
                    _ => false,
                }
            };

            if is_def {
                defs.push(expanded.to_owned());
            } else {
                exprs.push(expanded.to_owned());
            }
        }

        Ok(())
    })
}

async fn define_syntax(
    ident: &Identifier,
    expr: &Syntax,
    env: &Env,
    cont: &Option<Arc<Continuation>>,
) -> Result<(), ParseAstError> {
    env.def_macro(
        ident,
        Expression::parse(expr, env, cont)
            .await?
            .eval(cont)
            .await?
            .require_one()?,
    );
    Ok(())
}

/*
#[async_trait]
impl Parse for ast::Body {
    type Error = ParseBodyError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseBodyError> {
        if exprs.is_empty() {
            return Err(ParseBodyError::EmptyBody(span.clone()));
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
pub enum ParseLetError {
    BadForm(Span),
    ParseBodyError(ParseBodyError),
    ParseLetBindingError(ParseLetBindingError),
}

#[async_trait]
impl Parse for ast::Let {
    type Error = ParseLetError;

    async fn compile(
        expr: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseLetError> {
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
            _ => Err(ParseLetError::BadForm(span.clone())),
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
) -> Result<ast::Let, ParseLetError> {
    let mut previously_bound = HashMap::new();
    let mut new_contour = env.new_lexical_contour();
    let mut compiled_bindings = Vec::new();
    // TODO: Check that the list of bindings is proper
    if !bindings.is_empty() {
        for binding in &bindings[..bindings.len() - 1] {
            let binding = LetBinding::compile(binding, env, cont, &previously_bound)
                .await
                .map_err(ParseLetError::ParseLetBindingError)?;
            previously_bound.insert(binding.ident.clone(), binding.span.clone());
            new_contour.def_var(&binding.ident, Gc::new(Value::Undefined));
            compiled_bindings.push(binding);
        }
    }

    let env = Gc::new(new_contour);
    let body = ast::Body::compile(body, &Env::from(env.clone()), cont, span)
        .await
        .map_err(ParseLetError::ParseBodyError)?;

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
pub enum ParseLetBindingError {
    BadForm(Span),
    PreviouslyBound {
        ident: Identifier,
        first: Span,
        second: Span,
    },
    NotAList(Span),
    ParseError(Box<ParseError>),
}

impl_from_parse_error!(ParseLetBindingError);

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
    ) -> Result<LetBinding, ParseLetBindingError> {
        match expr {
            Syntax::List { list, span } => match &list[..] {
                [Syntax::Identifier {
                    ident,
                    span: bind_span,
                    ..
                }, expr, Syntax::Null { .. }] => {
                    if let Some(prev_bind) = previously_bound.get(ident) {
                        return Err(ParseLetBindingError::PreviouslyBound {
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
                _ => Err(ParseLetBindingError::BadForm(span.clone())),
            },
            expr => Err(ParseLetBindingError::NotAList(expr.span().clone())),
        }
    }
}

#[derive(From, Debug, Clone)]
pub enum ParseFuncCallError {
    EmptyFunctionCall(Span),
    ParseError(Box<ParseError>),
}

impl_from_parse_error!(ParseFuncCallError);

#[async_trait]
impl Parse for ast::Call {
    type Error = ParseFuncCallError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<ast::Call, ParseFuncCallError> {
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
            [] => Err(ParseFuncCallError::EmptyFunctionCall(span.clone())),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseIfError {
    ExpectedConditional(Span),
    ExpectedArgumentAfterConditional(Span),
    UnexpectedArgument(Span),
    ParseError(Box<ParseError>),
}

impl_from_parse_error!(ParseIfError);

#[async_trait]
impl Parse for ast::If {
    type Error = ParseIfError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseIfError> {
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
            [] => Err(ParseIfError::ExpectedConditional(span.clone())),
            [a1] => Err(ParseIfError::ExpectedArgumentAfterConditional(
                a1.span().clone(),
            )),
            [_, _, _, unexpected, ..] => Err(ParseIfError::UnexpectedArgument(
                unexpected.span().clone(),
            )),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseDefineError {
    ParameterDefinedMultipleTimes {
        ident: Identifier,
        first: Span,
        second: Span,
    },
    ExpectedIdentifier(Span),
    ParseBodyError(ParseBodyError),
    BadForm(Span),
    ParseError(Box<ParseError>),
}

impl_from_parse_error!(ParseDefineError);

#[async_trait]
impl Parse for ast::Define {
    type Error = ParseDefineError;

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
                    [] => Err(ParseDefineError::ExpectedIdentifier(span.clone())),
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
                                            ParseDefineError::ParameterDefinedMultipleTimes {
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
                                    return Err(ParseDefineError::ExpectedIdentifier(
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
                                            ParseDefineError::ParameterDefinedMultipleTimes {
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
                                    return Err(ParseDefineError::ExpectedIdentifier(
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
                            .map_err(ParseDefineError::ParseBodyError)?;
                        Ok(ast::Define::DefineFunc(ast::DefineFunc {
                            name: func_name.clone(),
                            args,
                            body,
                        }))
                    }
                    [x, ..] => Err(ParseDefineError::BadForm(x.span().clone())),
                }
            }
            _ => Err(ParseDefineError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseDefineSyntaxError {
    BadForm(Span),
    ParseError(Box<ParseError>),
    RuntimeError(Box<RuntimeError>),
}

impl_from_parse_error!(ParseDefineSyntaxError);

impl From<RuntimeError> for ParseDefineSyntaxError {
    fn from(value: RuntimeError) -> Self {
        Self::RuntimeError(Box::new(value))
    }
}

#[async_trait]
impl Parse for ast::DefineSyntax {
    type Error = ParseDefineSyntaxError;

    async fn compile(
        expr: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<ast::DefineSyntax, ParseDefineSyntaxError> {
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
            _ => Err(ParseDefineSyntaxError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseQuoteError {
    ExpectedArgument(Span),
    UnexpectedArgument(Span),
    BadForm(Span),
}

#[async_trait]
impl Parse for ast::Quote {
    type Error = ParseQuoteError;

    async fn compile(
        exprs: &[Syntax],
        _env: &Env,
        _cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseQuoteError> {
        match exprs {
            [] => Err(ParseQuoteError::ExpectedArgument(span.clone())),
            [Syntax::Null { .. }] => Ok(ast::Quote { val: Value::Null }),
            [expr, Syntax::Null { .. }] => Ok(ast::Quote {
                val: Value::from_syntax(expr),
            }),
            [_, arg, ..] => Err(ParseQuoteError::UnexpectedArgument(arg.span().clone())),
            _ => Err(ParseQuoteError::BadForm(span.clone())),
        }
    }
}

#[async_trait]
impl Parse for ast::And {
    type Error = ParseError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        _span: &Span,
    ) -> Result<Self, ParseError> {
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
impl Parse for ast::Or {
    type Error = ParseError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        _span: &Span,
    ) -> Result<Self, ParseError> {
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
pub enum ParseSetError {
    ExpectedArgument(Span),
    ExpectedIdent(Span),
    UnexpectedArgument(Span),
    BadForm(Span),
    ParseError(Box<ParseError>),
}

impl_from_parse_error!(ParseSetError);

#[async_trait]
impl Parse for ast::Set {
    type Error = ParseSetError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseSetError> {
        // TODO: We need to check if the identifier is defined as a variable transformer
        match exprs {
            [] => Err(ParseSetError::ExpectedArgument(span.clone())),
            [Syntax::Identifier { ident, .. }, expr, Syntax::Null { .. }] => Ok(ast::Set {
                var: ident.clone(),
                val: expr.compile(env, cont).await?,
            }),
            [arg1, _, Syntax::Null { .. }] => {
                Err(ParseSetError::ExpectedIdent(arg1.span().clone()))
            }
            [_, _, arg3, ..] => Err(ParseSetError::UnexpectedArgument(arg3.span().clone())),
            _ => Err(ParseSetError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseSyntaxError {
    ExpectedArgument(Span),
    UnexpectedArgument(Span),
    BadForm(Span),
}

#[async_trait]
impl Parse for ast::SyntaxQuote {
    type Error = ParseSyntaxError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        _cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseSyntaxError> {
        match exprs {
            [] => Err(ParseSyntaxError::ExpectedArgument(span.clone())),
            [expr, Syntax::Null { .. }] => Ok(ast::SyntaxQuote {
                syn: expr.clone(),
                env: env.clone(),
            }),
            [_, arg, ..] => Err(ParseSyntaxError::UnexpectedArgument(arg.span().clone())),
            _ => Err(ParseSyntaxError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseLambdaError {
    ExpectedList(Span),
    ExpectedIdentifier(Span),
    ParseBodyError(ParseBodyError),
    ParameterDefinedMultipleTimes {
        ident: Identifier,
        first: Span,
        second: Span,
    },
}

#[async_trait]
impl Parse for ast::Lambda {
    type Error = ParseLambdaError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseLambdaError> {
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
                    .map_err(ParseLambdaError::ParseBodyError)?;

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
) -> Result<ast::Lambda, ParseLambdaError> {
    let mut bound = HashMap::<Identifier, Span>::new();
    let mut fixed = Vec::new();
    if !args.is_empty() {
        for arg in &args[..args.len() - 1] {
            match arg {
                Syntax::Identifier { ident, span, .. } => {
                    if let Some(prev_span) = bound.get(ident) {
                        return Err(ParseLambdaError::ParameterDefinedMultipleTimes {
                            ident: ident.clone(),
                            first: prev_span.clone(),
                            second: span.clone(),
                        });
                    }
                    bound.insert(ident.clone(), span.clone());
                    fixed.push(ident.clone());
                }
                x => return Err(ParseLambdaError::ExpectedIdentifier(x.span().clone())),
            }
        }
    }
    let args = if let Some(last) = args.last() {
        match last {
            Syntax::Null { .. } => ast::Formals::FixedArgs(fixed.into_iter().collect()),
            Syntax::Identifier { ident, span, .. } => {
                if let Some(prev_span) = bound.get(ident) {
                    return Err(ParseLambdaError::ParameterDefinedMultipleTimes {
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
            x => return Err(ParseLambdaError::ExpectedIdentifier(x.span().clone())),
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
        .map_err(ParseLambdaError::ParseBodyError)?;
    Ok(ast::Lambda { args, body })
}

#[derive(Debug, Clone)]
pub enum ParseSyntaxCaseError {
    ParseError(Box<ParseError>),
    BadForm(Span),
}

impl_from_parse_error!(ParseSyntaxCaseError);

#[async_trait]
impl Parse for ast::SyntaxCase {
    type Error = ParseSyntaxCaseError;

    async fn compile(
        exprs: &[Syntax],
        env: &Env,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseSyntaxCaseError> {
        let (arg, keywords, mut rules) = match exprs {
            [arg, Syntax::List { list, .. }, rules @ ..] => {
                let mut keywords = HashSet::default();
                // TODO: ensure keywords_list is proper
                for keyword in &list[..list.len() - 1] {
                    if let Syntax::Identifier { ident, .. } = keyword {
                        keywords.insert(ident.name.clone());
                    } else {
                        return Err(ParseSyntaxCaseError::BadForm(keyword.span().clone()));
                    }
                }
                (arg, keywords, rules)
            }
            [arg, Syntax::Null { .. }, rules @ ..] => (arg, HashSet::default(), rules),
            _ => return Err(ParseSyntaxCaseError::BadForm(span.clone())),
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
                    _ => return Err(ParseSyntaxCaseError::BadForm(span.clone())),
                },
                _ => return Err(ParseSyntaxCaseError::BadForm(span.clone())),
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

*/
