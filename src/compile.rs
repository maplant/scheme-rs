use crate::{
    ast::{self, Ident},
    eval::{Env, Eval},
    expand::{Binds, Pattern, SyntaxRule, Template},
    gc::Gc,
    lex::Span,
    sexpr::SExpr,
};
use std::collections::{HashMap, HashSet};

// TODO: Put all of these functions in a trait? I kind of like them as free
// standing functions.

#[derive(Debug)]
pub enum CompileFuncCallError<'a> {
    EmptyFunctionCall(Span<'a>),
}

pub async fn compile_func_call<'a>(
    exprs: &[SExpr<'a>],
    env: &Gc<Env>,
    binds: &Binds<'_>,
    span: &Span<'a>,
) -> Result<ast::Call, CompileFuncCallError<'a>> {
    match exprs {
        [operator, args @ ..] => {
            let operator = operator.compile(env, binds).await;
            let mut compiled_args = Vec::new();
            for arg in &args[..args.len() - 1] {
                compiled_args.push(arg.compile(env, binds).await);
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
}

pub async fn compile_define<'a>(
    exprs: &[SExpr<'a>],
    env: &Gc<Env>,
    binds: &Binds<'_>,
    span: &Span<'a>,
) -> Result<ast::Define, CompileDefineError<'a>> {
    match exprs {
        [SExpr::Identifier { ident, .. }, expr] => Ok(ast::Define::DefineVar(ast::DefineVar {
            name: ident.clone(),
            val: expr.compile(env, binds).await,
        })),
        [SExpr::List { list, span }, body @ ..] => {
            match &list[..] {
                [] => Err(CompileDefineError::ExpectedIdentifier(span.clone())),
                [SExpr::Identifier {
                    ident: func_name,
                    span: func_span,
                }, args @ ..] => {
                    let mut scope = Binds::new_local(binds);
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
                    let body = compile_body(body, env, &scope, func_span)
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

#[derive(Debug)]
pub enum CompileIfError<'a> {
    ExpectedConditional(Span<'a>),
    ExpectedArgumentAfterConditional(Span<'a>),
    UnexpectedArgument(Span<'a>),
}

pub async fn compile_if<'a>(
    exprs: &[SExpr<'a>],
    env: &Gc<Env>,
    binds: &Binds<'_>,
    span: &Span<'a>,
) -> Result<ast::If, CompileIfError<'a>> {
    match exprs {
        [cond, success, SExpr::Nil { .. }] => Ok(ast::If {
            cond: cond.compile(env, binds).await,
            success: success.compile(env, binds).await,
            failure: None,
        }),
        [cond, success, failure, SExpr::Nil { .. }] => Ok(ast::If {
            cond: cond.compile(env, binds).await,
            success: success.compile(env, binds).await,
            failure: Some(failure.compile(env, binds).await),
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

#[derive(Debug)]
pub enum CompileBodyError<'a> {
    EmptyBody(Span<'a>),
}

pub async fn compile_body<'a>(
    exprs: &[SExpr<'a>],
    env: &Gc<Env>,
    binds: &Binds<'_>,
    span: &Span<'a>,
) -> Result<ast::Body, CompileBodyError<'a>> {
    if exprs.is_empty() {
        return Err(CompileBodyError::EmptyBody(span.clone()));
    }
    let mut output = Vec::new();
    for expr in &exprs[..exprs.len() - 1] {
        let expr = expr.compile(env, binds).await;
        output.push(expr);
    }
    // TODO: what if the body isn't a proper list?
    Ok(ast::Body::new(output))
}

pub enum CompileLetError<'a> {
    BadForm(Span<'a>),
    CompileBodyError(CompileBodyError<'a>),
    CompileLetBindingError(CompileLetBindingError<'a>),
}

pub async fn compile_let<'a>(
    expr: &[SExpr<'a>],
    env: &Gc<Env>,
    binds: &Binds<'_>,
    span: &Span<'a>,
) -> Result<ast::Let, CompileLetError<'a>> {
    match expr {
        [SExpr::List { list: bindings, .. }, body @ ..] => {
            let mut previously_bound = HashMap::new();
            let mut compiled_bindings = Vec::new();
            let mut new_scope = Binds::new_local(binds);
            for binding in bindings {
                let binding = LetBinding::compile(binding, env, binds, &previously_bound)
                    .await
                    .map_err(CompileLetError::CompileLetBindingError)?;
                previously_bound.insert(binding.ident.clone(), binding.span.clone());
                new_scope.bind(&binding.ident.sym);
                compiled_bindings.push(binding);
            }
            let body = compile_body(body, env, binds, span)
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

pub enum CompileLetBindingError<'a> {
    BadForm(Span<'a>),
    PreviouslyBound {
        ident: Ident,
        first: Span<'a>,
        second: Span<'a>,
    },
}

struct LetBinding<'a> {
    ident: Ident,
    span: Span<'a>,
    expr: Box<dyn Eval>,
}

impl<'a> LetBinding<'a> {
    async fn compile(
        expr: &SExpr<'a>,
        env: &Gc<Env>,
        binds: &Binds<'_>,
        previously_bound: &HashMap<Ident, Span<'a>>,
    ) -> Result<LetBinding<'a>, CompileLetBindingError<'a>> {
        match expr {
            SExpr::List { list, span } => match &list[..] {
                [SExpr::Identifier {
                    ident,
                    span: bind_span,
                }, expr] => {
                    let ident = ident.clone();
                    if let Some(prev_bind) = previously_bound.get(&ident) {
                        return Err(CompileLetBindingError::PreviouslyBound {
                            ident,
                            first: prev_bind.clone(),
                            second: bind_span.clone(),
                        });
                    }

                    let expr = expr.clone().compile(env, binds).await;

                    Ok(LetBinding {
                        ident,
                        span: bind_span.clone(),
                        expr,
                    })
                }
                _ => Err(CompileLetBindingError::BadForm(span.clone())),
            },
            expr => Err(CompileLetBindingError::BadForm(expr.span().clone())),
        }
    }
}

#[derive(Debug)]
pub enum CompileDefineSyntaxError<'a> {
    BadForm(Span<'a>),
}

pub async fn compile_define_syntax<'a>(
    expr: &[SExpr<'a>],
    _env: &Gc<Env>,
    _binds: &Binds<'_>,
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
                            return Err(CompileDefineSyntaxError::BadForm(keyword.span().clone()));
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
