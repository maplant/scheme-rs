use crate::{
    continuation::Continuation,
    env::{Env, VarRef},
    error::RuntimeError,
    expand::{SyntaxRule, Transformer},
    gc::Gc,
    syntax::{FullyExpanded, Identifier, Span, Syntax},
    util::{ArcSlice, RequireOne},
    value::Value,
};

use super::*;

use derive_more::From;
use futures::future::BoxFuture;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Debug, Clone, Trace)]
pub enum ParseAstError {
    BadForm(Span),
    UnexpectedEmptyList(Span),
    UndefinedVariable(Identifier),
    RuntimeError(Box<RuntimeError>),
    NotAVariableTransformer,
    EmptyBody(Span),
    DefInExprContext(Span),
    ExpectedIdentifier(Span),
    ParentSpecifiedMultipleTimes {
        first: Span,
        second: Span,
    },
    MultipleFieldsClauses {
        first: Span,
        second: Span,
    },
    NameBoundMultipleTimes {
        ident: Identifier,
        first: Span,
        second: Span,
    },
    ExpectedArgument(Span),
    UnexpectedArgument(Span),
}

impl From<RuntimeError> for ParseAstError {
    fn from(re: RuntimeError) -> Self {
        Self::RuntimeError(Box::new(re))
    }
}

impl Definition {
    pub(super) async fn parse(
        syn: &[Syntax],
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match syn {
            [_, Syntax::Identifier { ident, .. }, expr, Syntax::Null { .. }] => {
                Ok(Definition::DefineVar(DefineVar {
                    name: ident.clone(),
                    val: Arc::new(Expression::parse(expr.clone(), env, cont).await?),
                }))
            }
            [_, Syntax::List { list, span }, body @ .., Syntax::Null { .. }] => {
                if body.is_empty() {
                    return Err(ParseAstError::EmptyBody(span.clone()));
                }
                match list.as_slice() {
                    [Syntax::Identifier {
                        ident: func_name,
                        span: func_span,
                        ..
                    }, args @ ..] => {
                        let mut bound = HashMap::<Identifier, Span>::new();
                        let mut new_env = env.lexical_contour.new_lexical_contour();
                        let mut fixed = Vec::new();

                        // Bind the arguments to a new environment:
                        for arg in &args[..args.len() - 1] {
                            match arg {
                                Syntax::Identifier { ident, span, .. } => {
                                    if let Some(prev_span) = bound.get(ident) {
                                        return Err(ParseAstError::NameBoundMultipleTimes {
                                            ident: ident.clone(),
                                            first: prev_span.clone(),
                                            second: span.clone(),
                                        });
                                    }
                                    new_env.def_local_var(ident, Gc::new(Value::Undefined));
                                    bound.insert(ident.clone(), span.clone());
                                    fixed.push(ident.clone());
                                }
                                x => {
                                    return Err(ParseAstError::ExpectedIdentifier(x.span().clone()))
                                }
                            }
                        }

                        let args = if let Some(last) = args.last() {
                            match last {
                                Syntax::Null { .. } => {
                                    Formals::FixedArgs(fixed.into_iter().collect())
                                }
                                Syntax::Identifier { ident, span, .. } => {
                                    if let Some(prev_span) = bound.get(ident) {
                                        return Err(ParseAstError::NameBoundMultipleTimes {
                                            ident: ident.clone(),
                                            first: prev_span.clone(),
                                            second: span.clone(),
                                        });
                                    }
                                    let remaining = ident.clone();
                                    new_env.def_local_var(ident, Gc::new(Value::Undefined));
                                    bound.insert(remaining.clone(), span.clone());
                                    Formals::VarArgs {
                                        fixed: fixed.into_iter().collect(),
                                        remaining,
                                    }
                                }
                                x => {
                                    return Err(ParseAstError::ExpectedIdentifier(x.span().clone()))
                                }
                            }
                        } else {
                            // If there is no last argument, there are no arguments
                            Formals::FixedArgs(Vec::new())
                        };

                        let new_env = env.push_lexical_contour(Gc::new(new_env));

                        // Parse the body:
                        let body = Body::parse(body, &new_env, cont, func_span).await?;

                        Ok(Self::DefineFunc(DefineFunc {
                            name: func_name.clone(),
                            args,
                            body,
                        }))
                    }
                    _ => Err(ParseAstError::BadForm(span.clone())),
                }
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

impl Body {
    /// Parse the body. body is expected to be a list of valid syntax objects, and should not include
    /// _any_ nulls, including one at the end.
    pub(super) fn parse<'a, 'b>(
        body: &'a [Syntax],
        env: &'a ExpansionEnv<'b>,
        cont: &'a Option<Arc<Continuation>>,
        span: &'a Span,
    ) -> BoxFuture<'a, Result<Self, ParseAstError>>
    where
        'a: 'b,
    {
        Box::pin(async move {
            let mut defs = Vec::new();
            let mut exprs = Vec::new();

            splice_in(&mut defs, &mut exprs, body, env, cont, span).await?;

            let mut defs_parsed = Vec::new();
            let mut exprs_parsed = Vec::new();

            for def in defs.into_iter() {
                let def = match def {
                    Ok(def) => {
                        let new_expansion_env = env.push_expansion_env(def.expansion_ctxs);
                        Definition::parse(
                            def.expanded.as_list().unwrap(),
                            &new_expansion_env,
                            cont,
                            def.expanded.span(),
                        )
                        .await?
                    }
                    Err(def_record) => Definition::DefineRecordType(def_record),
                };
                defs_parsed.push(def);
            }

            for expr in exprs.into_iter() {
                let new_expansion_env = env.push_expansion_env(expr.expansion_ctxs);
                exprs_parsed.push(
                    Expression::parse_expanded(expr.expanded, &new_expansion_env, cont).await?,
                );
            }

            Ok(Self::new(defs_parsed, exprs_parsed))
        })
    }

    /// Differs from Body by being purely expression based. No definitions allowed.
    pub(super) async fn parse_in_expr_context(
        body: &[Syntax],
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, ParseAstError> {
        let mut forms = Vec::new();
        for sexpr in body {
            let parsed = AstNode::Expression(Expression::parse(sexpr.clone(), env, cont).await?);
            forms.push(parsed);
        }
        let forms = ArcSlice::from(forms);
        Ok(Self { forms })
    }
}

fn splice_in<'a, 'b>(
    defs: &'a mut Vec<Result<FullyExpanded, DefineRecordType>>,
    exprs: &'a mut Vec<FullyExpanded>,
    body: &'a [Syntax],
    env: &'a ExpansionEnv<'b>,
    cont: &'a Option<Arc<Continuation>>,
    span: &'a Span,
) -> BoxFuture<'a, Result<(), ParseAstError>>
where
    'a: 'b,
{
    Box::pin(async move {
        if body.is_empty() {
            return Err(ParseAstError::EmptyBody(span.clone()));
        }

        for unexpanded in body {
            let expanded = unexpanded.clone().expand(env, cont).await?;
            let is_def = {
                match expanded.as_ref().as_list() {
                    Some([Syntax::Identifier { ident, .. }, body @ .., Syntax::Null { .. }])
                        if ident == "begin" =>
                    {
                        splice_in(defs, exprs, body, env, cont, span).await?;
                        continue;
                    }
                    Some(
                        [Syntax::Identifier { ident, .. }, Syntax::Identifier { ident: name, .. }, expr, Syntax::Null { .. }],
                    ) if ident == "define-syntax" => {
                        define_syntax(name, expr.clone(), &env.lexical_contour, cont).await?;
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
                        env.lexical_contour
                            .write()
                            .def_local_var(ident, Gc::new(Value::Undefined));
                        true
                    }
                    Some(
                        [Syntax::Identifier { ident, span, .. }, body @ .., Syntax::Null { .. }],
                    ) if ident == "define-record-type" => {
                        let record_type = DefineRecordType::parse(body, env, span)?;
                        record_type.define(&env.lexical_contour);
                        defs.push(Err(record_type));
                        continue;
                    }
                    Some([Syntax::Identifier { ident, span, .. }, ..])
                        if ident == "define-syntax" =>
                    {
                        return Err(ParseAstError::BadForm(span.clone()));
                    }
                    _ => false,
                }
            };

            if is_def {
                defs.push(Ok(expanded));
            } else {
                exprs.push(expanded);
            }
        }

        Ok(())
    })
}

pub(super) async fn define_syntax(
    ident: &Identifier,
    expr: Syntax,
    env: &Gc<Env>,
    cont: &Option<Arc<Continuation>>,
) -> Result<(), ParseAstError> {
    let expansion_env = ExpansionEnv::from_env(env);
    let FullyExpanded {
        expanded,
        expansion_ctxs: expansion_envs,
    } = expr.expand(&expansion_env, cont).await?;
    let expansion_env = expansion_env.push_expansion_env(expansion_envs);
    let value = Expression::parse(expanded, &expansion_env, cont)
        .await?
        .eval(env, cont)
        .await?
        .require_one()?;
    env.write().def_local_macro(ident, value);
    Ok(())
}

impl Expression {
    pub(crate) async fn parse(
        syn: Syntax,
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, ParseAstError> {
        let FullyExpanded {
            expanded,
            expansion_ctxs: expansion_envs,
        } = syn.expand(env, cont).await?;
        let expansion_env = env.push_expansion_env(expansion_envs);
        Self::parse_expanded(expanded, &expansion_env, cont).await
    }

    fn parse_expanded<'a, 'b>(
        syn: Syntax,
        env: &'a ExpansionEnv<'b>,
        cont: &'a Option<Arc<Continuation>>,
    ) -> BoxFuture<'a, Result<Self, ParseAstError>>
    where
        'a: 'b,
    {
        Box::pin(async move {
            match syn {
                Syntax::Null { span } => Err(ParseAstError::UnexpectedEmptyList(span)),

                // Special Identifiers:
                Syntax::Identifier { ident, .. } if ident.name == "<undefined>" => {
                    Ok(Self::Undefined)
                }

                // Regular identifiers:
                Syntax::Identifier { ident, .. } => Ok(Self::Var(env.fetch_var_ref(&ident))),

                // Literals:
                Syntax::Literal { literal, .. } => Ok(Self::Literal(literal)),

                // Vector literals:
                Syntax::Vector { vector, .. } => Ok(Self::Vector(Vector::parse(&vector))),

                // Functional forms:
                Syntax::List {
                    list: exprs, span, ..
                } => match exprs.as_slice() {
                    // Special forms:
                    [Syntax::Identifier { ident, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "begin" =>
                    {
                        Body::parse_in_expr_context(tail, env, cont)
                            .await
                            .map(Expression::Begin)
                    }
                    [Syntax::Identifier { ident, span, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "lambda" =>
                    {
                        Lambda::parse(tail, env, cont, span)
                            .await
                            .map(Expression::Lambda)
                    }
                    [Syntax::Identifier { ident, span, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "let" =>
                    {
                        Let::parse(tail, env, cont, span).await.map(Expression::Let)
                    }
                    [Syntax::Identifier { ident, span, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "if" =>
                    {
                        If::parse(tail, env, cont, span).await.map(Expression::If)
                    }
                    [Syntax::Identifier { ident, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "and" =>
                    {
                        And::parse(tail, env, cont).await.map(Expression::And)
                    }
                    [Syntax::Identifier { ident, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "or" =>
                    {
                        Or::parse(tail, env, cont).await.map(Expression::Or)
                    }
                    [Syntax::Identifier { ident, span, .. }, tail @ ..] if ident == "quote" => {
                        Quote::parse(tail, span).await.map(Expression::Quote)
                    }
                    [Syntax::Identifier { ident, span, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "syntax" =>
                    {
                        SyntaxQuote::parse(tail, span)
                            .await
                            .map(Expression::SyntaxQuote)
                    }
                    [Syntax::Identifier { ident, span, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "syntax-case" =>
                    {
                        SyntaxCase::parse(tail, env, cont, span)
                            .await
                            .map(Expression::SyntaxCase)
                    }

                    // Extra special form (set!):
                    [Syntax::Identifier { ident, span, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "set!" =>
                    {
                        Set::parse(tail, env, cont, span).await.map(Expression::Set)
                    }

                    // Definition in expression context is illegal:
                    [Syntax::Identifier { ident, span, .. }, .., Syntax::Null { .. }]
                        if ident == "define" || ident == "define-record-type" =>
                    {
                        Err(ParseAstError::DefInExprContext(span.clone()))
                    }

                    // Regular old function call:
                    [operator, args @ .., Syntax::Null { .. }] => {
                        Call::parse(operator.clone(), args, env, cont)
                            .await
                            .map(Expression::Call)
                    }

                    _ => Err(ParseAstError::BadForm(span.clone())),
                },
            }
        })
    }
}

impl Call {
    async fn parse(
        operator: Syntax,
        args: &[Syntax],
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, ParseAstError> {
        let location = operator.span().clone();
        let proc_name = match operator {
            Syntax::Identifier { ref ident, .. } => ident.name.clone(),
            _ => String::from("<lambda>"),
        };
        let operator = Expression::parse(operator, env, cont).await?;
        let mut compiled_args = vec![operator];
        for arg in args {
            compiled_args.push(Expression::parse(arg.clone(), env, cont).await?);
        }
        Ok(Call {
            args: ArcSlice::from(compiled_args),
            location,
            proc_name,
        })
    }
}

impl Lambda {
    async fn parse(
        sexprs: &[Syntax],
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match sexprs {
            [Syntax::Null { .. }, body @ ..] => parse_lambda(&[], body, env, cont, span).await,
            [Syntax::List { list: args, .. }, body @ ..] => {
                parse_lambda(args, body, env, cont, span).await
            }
            [ident @ Syntax::Identifier { .. }, body @ ..] => {
                let args = &[ident.clone()];
                parse_lambda(args, body, env, cont, span).await
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

async fn parse_lambda(
    args: &[Syntax],
    body: &[Syntax],
    env: &ExpansionEnv<'_>,
    cont: &Option<Arc<Continuation>>,
    span: &Span,
) -> Result<Lambda, ParseAstError> {
    let mut bound = HashMap::<Identifier, Span>::new();
    let mut fixed = Vec::new();
    let mut new_contour = env.lexical_contour.new_lexical_contour();

    if !args.is_empty() {
        for arg in &args[..args.len() - 1] {
            match arg {
                Syntax::Identifier { ident, span, .. } => {
                    if let Some(prev_span) = bound.get(ident) {
                        return Err(ParseAstError::NameBoundMultipleTimes {
                            ident: ident.clone(),
                            first: prev_span.clone(),
                            second: span.clone(),
                        });
                    }
                    new_contour.def_local_var(ident, Gc::new(Value::Undefined));
                    bound.insert(ident.clone(), span.clone());
                    fixed.push(ident.clone());
                }
                x => return Err(ParseAstError::ExpectedIdentifier(x.span().clone())),
            }
        }
    }

    let args = if let Some(last) = args.last() {
        match last {
            Syntax::Null { .. } => Formals::FixedArgs(fixed.into_iter().collect()),
            Syntax::Identifier { ident, span, .. } => {
                if let Some(prev_span) = bound.get(ident) {
                    return Err(ParseAstError::NameBoundMultipleTimes {
                        ident: ident.clone(),
                        first: prev_span.clone(),
                        second: span.clone(),
                    });
                }
                new_contour.def_local_var(ident, Gc::new(Value::Undefined));
                let remaining = ident.clone();
                Formals::VarArgs {
                    fixed: fixed.into_iter().collect(),
                    remaining,
                }
            }
            x => return Err(ParseAstError::ExpectedIdentifier(x.span().clone())),
        }
    } else {
        // If there is no last argument, there are no arguments
        Formals::FixedArgs(Vec::new())
    };

    let new_env = env.push_lexical_contour(Gc::new(new_contour));
    let body = Body::parse(body, &new_env, cont, span).await?;

    Ok(Lambda { args, body })
}

impl Let {
    async fn parse(
        syn: &[Syntax],
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match syn {
            [Syntax::Null { .. }, body @ ..] => parse_let(None, &[], body, env, cont, span).await,
            [Syntax::List { list: bindings, .. }, body @ ..] => {
                parse_let(None, bindings, body, env, cont, span).await
            }
            // Named let:
            [Syntax::Identifier { ident, .. }, Syntax::List { list: bindings, .. }, body @ ..] => {
                parse_let(Some(ident), bindings, body, env, cont, span).await
            }
            [Syntax::Identifier { ident, .. }, Syntax::Null { .. }, body @ ..] => {
                parse_let(Some(ident), &[], body, env, cont, span).await
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

async fn parse_let(
    name: Option<&Identifier>,
    bindings: &[Syntax],
    body: &[Syntax],
    env: &ExpansionEnv<'_>,
    cont: &Option<Arc<Continuation>>,
    span: &Span,
) -> Result<Let, ParseAstError> {
    let mut previously_bound = HashMap::new();
    let mut new_contour = env.lexical_contour.new_lexical_contour();
    let mut compiled_bindings = Vec::new();

    match bindings {
        [] | [Syntax::Null { .. }] => (),
        [bindings @ .., Syntax::Null { .. }] => {
            for binding in bindings {
                let binding = LetBinding::parse(binding, env, cont, &previously_bound).await?;
                previously_bound.insert(binding.ident.clone(), binding.span.clone());
                new_contour.def_local_var(&binding.ident, Gc::new(Value::Undefined));
                compiled_bindings.push(binding);
            }
        }
        _ => {
            return Err(ParseAstError::BadForm(span.clone()));
        }
    }

    let new_env = Gc::new(new_contour);
    let new_exp_env = env.push_lexical_contour(new_env.clone());

    if let Some(name) = name {
        new_env
            .write()
            .def_local_var(name, Gc::new(Value::Undefined));
    }

    let mut ast_body = Body::parse(body, &new_exp_env, cont, span).await?;

    let mut bindings: Vec<_> = compiled_bindings
        .into_iter()
        .map(|binding| (binding.ident, binding.expr))
        .collect();

    // If this is a named let, add a binding for a procedure with the same
    // body and args of the formals.
    // This code is really bad, but I wanted to get this working
    if let Some(name) = name {
        let mut new_new_env = new_env.new_lexical_contour();
        for (binding, _) in &bindings {
            new_new_env.def_local_var(binding, Gc::new(Value::Undefined));
        }
        let new_new_exp_env = new_exp_env.push_lexical_contour(Gc::new(new_new_env));
        let lambda = Lambda {
            args: Formals::FixedArgs(bindings.iter().map(|(ident, _)| ident.clone()).collect()),
            body: Body::parse(body, &new_new_exp_env, cont, span).await?,
        };
        ast_body = Body {
            forms: ArcSlice::from(
                vec![AstNode::Expression(Expression::Set(Set {
                    var: Ref::Regular(VarRef::default().offset(bindings.len())),
                    val: Arc::new(Expression::Lambda(lambda)),
                }))]
                .into_iter()
                .chain(ast_body.forms.iter().map(|(x, _)| x).cloned())
                .collect::<Vec<_>>(),
            ),
        };
        bindings.push((name.clone(), Expression::Undefined));
    }

    Ok(Let {
        bindings: Arc::from(bindings),
        body: ast_body,
    })
}

struct LetBinding {
    ident: Identifier,
    span: Span,
    expr: Expression,
}

impl LetBinding {
    async fn parse(
        form: &Syntax,
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
        previously_bound: &HashMap<Identifier, Span>,
    ) -> Result<LetBinding, ParseAstError> {
        if let Some(
            [Syntax::Identifier {
                ident,
                span: bind_span,
                ..
            }, expr, Syntax::Null { .. }],
        ) = form.as_list()
        {
            if let Some(prev_bind) = previously_bound.get(ident) {
                return Err(ParseAstError::NameBoundMultipleTimes {
                    ident: ident.clone(),
                    first: prev_bind.clone(),
                    second: bind_span.clone(),
                });
            }

            let expr = Expression::parse(expr.clone(), env, cont).await?;

            Ok(LetBinding {
                ident: ident.clone(),
                span: bind_span.clone(),
                expr,
            })
        } else {
            Err(ParseAstError::BadForm(form.span().clone()))
        }
    }
}

impl If {
    async fn parse(
        exprs: &[Syntax],
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match exprs {
            [cond, success] => Ok(If {
                cond: Arc::new(Expression::parse(cond.clone(), env, cont).await?),
                success: Arc::new(Expression::parse(success.clone(), env, cont).await?),
                failure: None,
            }),
            [cond, success, failure] => Ok(If {
                cond: Arc::new(Expression::parse(cond.clone(), env, cont).await?),
                success: Arc::new(Expression::parse(success.clone(), env, cont).await?),
                failure: Some(Arc::new(
                    Expression::parse(failure.clone(), env, cont).await?,
                )),
            }),
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [a1] => Err(ParseAstError::ExpectedArgument(a1.span().clone())),
            [_, _, _, unexpected, ..] => {
                Err(ParseAstError::UnexpectedArgument(unexpected.span().clone()))
            }
        }
    }
}

impl And {
    async fn parse(
        exprs: &[Syntax],
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, ParseAstError> {
        let mut output = Vec::new();
        for expr in exprs {
            let expr = Expression::parse(expr.clone(), env, cont).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

impl Or {
    async fn parse(
        exprs: &[Syntax],
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, ParseAstError> {
        let mut output = Vec::new();
        for expr in exprs {
            let expr = Expression::parse(expr.clone(), env, cont).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

impl Set {
    async fn parse(
        exprs: &[Syntax],
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        match exprs {
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [Syntax::Identifier { ident, .. }, expr] => Ok(Set {
                var: env.fetch_var_ref(ident),
                val: Arc::new(Expression::parse(expr.clone(), env, cont).await?),
            }),
            [arg1, _] => Err(ParseAstError::ExpectedIdentifier(arg1.span().clone())),
            [_, _, arg3, ..] => Err(ParseAstError::UnexpectedArgument(arg3.span().clone())),
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

impl Quote {
    async fn parse(exprs: &[Syntax], span: &Span) -> Result<Self, ParseAstError> {
        match exprs {
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [Syntax::Null { .. }] => Ok(Quote { val: Value::Null }),
            [expr, Syntax::Null { .. }] => Ok(Quote {
                val: Value::from_syntax(expr),
            }),
            [_, arg, ..] => Err(ParseAstError::UnexpectedArgument(arg.span().clone())),
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

impl Vector {
    fn parse(exprs: &[Syntax]) -> Self {
        let mut vals = Vec::new();
        for expr in exprs {
            vals.push(Value::from_syntax(expr));
        }
        Self { vals }
    }
}

impl SyntaxQuote {
    async fn parse(exprs: &[Syntax], span: &Span) -> Result<Self, ParseAstError> {
        match exprs {
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [expr] => Ok(SyntaxQuote { syn: expr.clone() }),
            [_, arg, ..] => Err(ParseAstError::UnexpectedArgument(arg.span().clone())),
        }
    }
}

impl SyntaxCase {
    async fn parse(
        exprs: &[Syntax],
        env: &ExpansionEnv<'_>,
        cont: &Option<Arc<Continuation>>,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        let (arg, keywords, mut rules) = match exprs {
            [arg, Syntax::List { list, .. }, rules @ ..] => {
                let mut keywords = HashSet::default();
                // TODO: ensure keywords_list is proper
                for keyword in &list[..list.len() - 1] {
                    if let Syntax::Identifier { ident, .. } = keyword {
                        keywords.insert(ident.name.clone());
                    } else {
                        return Err(ParseAstError::BadForm(keyword.span().clone()));
                    }
                }
                (arg, keywords, rules)
            }
            [arg, Syntax::Null { .. }, rules @ ..] => (arg, HashSet::default(), rules),
            _ => return Err(ParseAstError::BadForm(span.clone())),
        };
        let mut syntax_rules = Vec::new();
        loop {
            match rules {
                [] => break,
                [Syntax::List { list, .. }, tail @ ..] => match &list[..] {
                    [pattern, template, Syntax::Null { .. }] => {
                        syntax_rules.push(SyntaxRule::compile(&keywords, pattern, template));
                        rules = tail;
                    }
                    _ => return Err(ParseAstError::BadForm(span.clone())),
                },
                _ => return Err(ParseAstError::BadForm(span.clone())),
            }
        }
        Ok(SyntaxCase {
            arg: Arc::new(Expression::parse(arg.clone(), env, cont).await?),
            transformer: Transformer {
                rules: syntax_rules,
                is_variable_transformer: false,
            },
        })
    }
}
