use crate::{
    env::{Environment, Top},
    error::RuntimeError,
    expand::{SyntaxRule, Transformer},
    syntax::{FullyExpanded, Identifier, Span, Syntax},
    value::Value,
};

use super::*;

use derive_more::From;
use either::Either;
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
        env: &Environment<impl Top>,
        span: &Span,
        // cont: &Closure
    ) -> Result<Self, ParseAstError> {
        match syn {
            [_, Syntax::Identifier { ident, .. }, expr, Syntax::Null { .. }] => {
                Ok(Definition::DefineVar(DefineVar {
                    var: env.def_var(ident.clone()),

                    val: Arc::new(Expression::parse(expr.clone(), env /* cont */).await?),
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
                        // Define the variable, just in case.
                        let var = env.def_var(func_name.clone());

                        let mut bound = HashMap::<Identifier, Span>::new();
                        let mut fixed = Vec::new();
                        let new_env = env.new_lexical_contour();

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
                                    let Var::Local(sym) = new_env.def_var(ident.clone()) else {
                                        unreachable!()
                                    };
                                    bound.insert(ident.clone(), span.clone());
                                    fixed.push(sym);
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
                                    let Var::Local(remaining) = new_env.def_var(ident.clone())
                                    else {
                                        unreachable!()
                                    };
                                    bound.insert(ident.clone(), span.clone());
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

                        // Parse the body:
                        let body = Body::parse(body, &new_env, func_span /* cont */).await?;

                        Ok(Self::DefineFunc(DefineFunc { var, args, body }))
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
    pub(super) fn parse<'a>(
        body: &'a [Syntax],
        env: &'a Environment<impl Top>,
        span: &'a Span,
        // cont: &'a Closure
    ) -> BoxFuture<'a, Result<Self, ParseAstError>> {
        Box::pin(async move {
            let mut defs = Vec::new();
            let mut exprs = Vec::new();

            splice_in(&mut defs, &mut exprs, body, env, span /* cont */).await?;

            let mut defs_parsed = Vec::new();
            let mut exprs_parsed = Vec::new();

            for def in defs.into_iter() {
                let def = match def {
                    Either::Left(def) => {
                        Definition::parse(
                            def.expanded.as_list().unwrap(),
                            &def.expansion_env,
                            def.expanded.span(),
                            /* cont */
                        )
                        .await?
                    }
                    Either::Right(def_record) => Definition::DefineRecordType(def_record),
                };
                defs_parsed.push(def);
            }

            for expr in exprs.into_iter() {
                // let new_expansion_env = env.push_expansion_env(expr.expansion_ctxs);
                exprs_parsed.push(
                    Expression::parse_expanded(expr.expanded, &expr.expansion_env /* cont */)
                        .await?,
                );
            }

            Ok(Self::new(defs_parsed, exprs_parsed))
        })
    }

    /// Differs from Body by being purely expression based. No definitions allowed.
    pub(super) async fn parse_in_expr_context(
        body: &[Syntax],
        env: &Environment<impl Top>,
        // cont: &Closure
    ) -> Result<Self, ParseAstError> {
        let mut forms = Vec::new();
        for sexpr in body {
            let parsed =
                AstNode::Expression(Expression::parse(sexpr.clone(), env /* cont */).await?);
            forms.push(parsed);
        }
        Ok(Self { forms })
    }
}

fn splice_in<'a, T: Top>(
    defs: &'a mut Vec<Either<FullyExpanded<T>, DefineRecordType>>,
    exprs: &'a mut Vec<FullyExpanded<T>>,
    body: &'a [Syntax],
    env: &'a Environment<T>,
    span: &'a Span,
    // cont: &Closure
) -> BoxFuture<'a, Result<(), ParseAstError>> {
    Box::pin(async move {
        if body.is_empty() {
            return Err(ParseAstError::EmptyBody(span.clone()));
        }

        for unexpanded in body {
            let FullyExpanded {
                expansion_env,
                expanded,
            } = unexpanded.clone().expand(env /* cont */).await?;
            let is_def = {
                match expanded.as_list() {
                    Some([Syntax::Identifier { ident, .. }, body @ .., Syntax::Null { .. }])
                        if ident == "begin" =>
                    {
                        splice_in(defs, exprs, body, &expansion_env, span /* cont */).await?;
                        continue;
                    }
                    /*
                    Some(
                        [Syntax::Identifier { ident, .. }, Syntax::Identifier { ident: name, .. }, expr, Syntax::Null { .. }],
                    ) if ident == "define-syntax" => {
                        define_syntax(name.clone(), expr.clone(), &expansion_env /* cont */)
                            .await?;
                        continue;
                    }
                    */
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
                        expansion_env.def_var(ident.clone());
                        true
                    }
                    /*
                    Some(
                        [Syntax::Identifier { ident, span, .. }, body @ .., Syntax::Null { .. }],
                    ) if ident == "define-record-type" => {
                        let record_type = DefineRecordType::parse(body, env, span)?;
                        record_type.define(&env.lexical_contour);
                        defs.push(Err(record_type));
                        continue;
                    }
                    */
                    Some([Syntax::Identifier { ident, span, .. }, ..])
                        if ident == "define-syntax" =>
                    {
                        return Err(ParseAstError::BadForm(span.clone()));
                    }
                    _ => false,
                }
            };

            let expanded = FullyExpanded::new(expansion_env, expanded);
            if is_def {
                defs.push(Either::Left(expanded));
            } else {
                exprs.push(expanded);
            }
        }

        Ok(())
    })
}

/*
pub(super) async fn define_syntax(
    ident: Identifier,
    expr: Syntax,
    env: &Environment<impl Top>,
    // cont: &Closure
) -> Result<(), ParseAstError> {
    let FullyExpanded {
        expanded,
        expansion_env,
    } = expr.expand(&env /* cont */).await?;
    /*
    let value = Expression::parse(expanded, &expansion_env, /* cont */)
        .await?
        .eval(env, cont)
        .await?
    .require_one()?;
     */

    env.def_macro(ident, todo!());
    Ok(())
}
*/

impl Expression {
    pub(crate) async fn parse(
        syn: Syntax,
        env: &Environment<impl Top>,
        // cont: &Closure
    ) -> Result<Self, ParseAstError> {
        let FullyExpanded {
            expansion_env,
            expanded,
        } = syn.expand(env /* cont */).await?;
        Self::parse_expanded(expanded, &expansion_env /* cont*/).await
    }

    fn parse_expanded(
        syn: Syntax,
        env: &Environment<impl Top>,
        // cont: &Closure,
    ) -> BoxFuture<'_, Result<Self, ParseAstError>> {
        Box::pin(async move {
            match syn {
                Syntax::Null { span } => Err(ParseAstError::UnexpectedEmptyList(span)),

                // Special Identifiers:
                Syntax::Identifier { ident, .. } if ident.name == "<undefined>" => {
                    Ok(Self::Undefined)
                }

                // Regular identifiers:
                Syntax::Identifier { ident, .. } => {
                    Ok(Self::Var(env.fetch_var(&ident).ok_or_else(|| {
                        ParseAstError::UndefinedVariable(ident.clone())
                    })?))
                }

                // Literals:
                Syntax::Literal { literal, .. } => Ok(Self::Literal(literal)),

                // Vector literals:
                Syntax::Vector { /* vector, */ .. } => todo!(), // Ok(Self::Vector(Vector::parse(&vector))),

                // Functional forms:
                Syntax::List {
                    list: exprs, span, ..
                } => match exprs.as_slice() {
                    // Special forms:
                    [Syntax::Identifier { ident, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "begin" =>
                    {
                        Body::parse_in_expr_context(tail, env /* cont */)
                            .await
                            .map(Expression::Begin)
                    }
                    [Syntax::Identifier { ident, span, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "lambda" =>
                    {
                        Lambda::parse(tail, env, span /* cont */)
                            .await
                            .map(Expression::Lambda)
                    }
                    [Syntax::Identifier { ident, span, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "let" =>
                    {
                        Let::parse(tail, env, span /* cont */)
                            .await
                            .map(Expression::Let)
                    }
                    [Syntax::Identifier { ident, span, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "if" =>
                    {
                        If::parse(tail, env, span /* cont */)
                            .await
                            .map(Expression::If)
                    }
                    [Syntax::Identifier { ident, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "and" =>
                    {
                        And::parse(tail, env /* cont */).await.map(Expression::And)
                    }
                    [Syntax::Identifier { ident, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "or" =>
                    {
                        Or::parse(tail, env /* cont */).await.map(Expression::Or)
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
                        SyntaxCase::parse(tail, env, span /* cont */)
                            .await
                            .map(Expression::SyntaxCase)
                    }

                    // Extra special form (set!):
                    [Syntax::Identifier { ident, span, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "set!" =>
                    {
                        Set::parse(tail, env, span /* cont */)
                            .await
                            .map(Expression::Set)
                    }

                    // Definition in expression context is illegal:
                    [Syntax::Identifier { ident, span, .. }, .., Syntax::Null { .. }]
                        if ident == "define" || ident == "define-record-type" =>
                    {
                        Err(ParseAstError::DefInExprContext(span.clone()))
                    }

                    // Regular old function call:
                    [operator, args @ .., Syntax::Null { .. }] => {
                        Apply::parse(operator.clone(), args, env /* cont */)
                            .await
                            .map(Expression::Apply)
                    }

                    _ => Err(ParseAstError::BadForm(span.clone())),
                },
            }
        })
    }
}

impl Apply {
    async fn parse(
        operator: Syntax,
        args: &[Syntax],
        env: &Environment<impl Top>,
        // cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, ParseAstError> {
        let location = operator.span().clone();
        let proc_name = match operator {
            Syntax::Identifier { ref ident, .. } => ident.name.clone(),
            _ => String::from(""),
        };
        let operator = match proc_name.parse::<PrimOp>() {
            Ok(prim_op) => Either::Right(prim_op),
            _ => Either::Left(Box::new(Expression::parse(operator, env /* cont */).await?)),
        };
        let mut parsed_args = Vec::new();
        for arg in args {
            parsed_args.push(Expression::parse(arg.clone(), env /* cont */).await?);
        }
        Ok(Apply {
            operator,
            args: parsed_args,
            location,
            proc_name,
        })
    }
}

impl Lambda {
    async fn parse(
        sexprs: &[Syntax],
        env: &Environment<impl Top>,
        span: &Span,
        // cont: &Closure,
    ) -> Result<Self, ParseAstError> {
        match sexprs {
            [Syntax::Null { .. }, body @ ..] => parse_lambda(&[], body, env, span /* cont */).await,
            [Syntax::List { list: args, .. }, body @ ..] => {
                parse_lambda(args, body, env, span /* cont */).await
            }
            [ident @ Syntax::Identifier { .. }, body @ ..] => {
                let args = &[ident.clone()];
                parse_lambda(args, body, env, span /* cont */).await
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

async fn parse_lambda(
    args: &[Syntax],
    body: &[Syntax],
    env: &Environment<impl Top>,
    span: &Span,
    // cont: &Closure
) -> Result<Lambda, ParseAstError> {
    let mut bound = HashMap::<Identifier, Span>::new();
    let mut fixed = Vec::new();
    let new_contour = env.new_lexical_contour();

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
                    let Var::Local(arg) = new_contour.def_var(ident.clone()) else {
                        unreachable!()
                    };
                    fixed.push(arg);
                    bound.insert(ident.clone(), span.clone());
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
                let Var::Local(remaining) = new_contour.def_var(ident.clone()) else {
                    unreachable!()
                };
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

    let body = Body::parse(body, &new_contour, span /* cont */).await?;

    Ok(Lambda { args, body })
}

impl Let {
    async fn parse(
        syn: &[Syntax],
        env: &Environment<impl Top>,
        span: &Span,
        // cont: &Closure,
    ) -> Result<Self, ParseAstError> {
        match syn {
            [Syntax::Null { .. }, body @ ..] => {
                parse_let(None, &[], body, env, span /* cont */).await
            }
            [Syntax::List { list: bindings, .. }, body @ ..] => {
                parse_let(None, bindings, body, env, span /* cont */).await
            }
            // Named let:
            [Syntax::Identifier { ident, .. }, Syntax::List { list: bindings, .. }, body @ ..] => {
                parse_let(Some(ident), bindings, body, env, span /* cont */).await
            }
            [Syntax::Identifier { ident, .. }, Syntax::Null { .. }, body @ ..] => {
                parse_let(Some(ident), &[], body, env, span /* cont */).await
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

async fn parse_let(
    name: Option<&Identifier>,
    bindings: &[Syntax],
    body: &[Syntax],
    env: &Environment<impl Top>,
    span: &Span,
    // cont: &Closure
) -> Result<Let, ParseAstError> {
    let mut previously_bound = HashMap::new();
    let mut parsed_bindings = Vec::new();
    let new_contour = env.new_lexical_contour();

    match bindings {
        [] | [Syntax::Null { .. }] => (),
        [bindings @ .., Syntax::Null { .. }] => {
            for binding in bindings {
                let binding = LetBinding::parse(binding, env, &previously_bound /* cont */).await?;
                previously_bound.insert(binding.ident.clone(), binding.span.clone());
                let Var::Local(var) = new_contour.def_var(binding.ident.clone()) else {
                    unreachable!()
                };
                parsed_bindings.push((var, binding));
            }
        }
        _ => {
            return Err(ParseAstError::BadForm(span.clone()));
        }
    }

    let lambda_var = name.map(|name| new_contour.def_var(name.clone()));

    let ast_body = Body::parse(body, &new_contour, span /* cont */).await?;

    // TODO: Lot of unnecessary cloning here, fix that.
    let mut bindings: Vec<_> = parsed_bindings
        .iter()
        .map(|(var, binding)| (var.clone(), binding.expr.clone()))
        .collect();

    // If this is a named let, add a binding for a procedure with the same body
    // and args of the formals.
    if let Some(Var::Local(lambda_var)) = lambda_var {
        let new_new_contour = new_contour.new_lexical_contour();
        let args = parsed_bindings
            .iter()
            .map(|(_, binding)| {
                let Var::Local(var) = new_new_contour.def_var(binding.ident.clone()) else {
                    unreachable!()
                };
                var
            })
            .collect();
        let lambda = Lambda {
            args: Formals::FixedArgs(args),
            body: Body::parse(body, &new_new_contour, span /* cont */).await?,
        };
        bindings.push((lambda_var, Expression::Lambda(lambda)));
    }

    Ok(Let {
        bindings,
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
        env: &Environment<impl Top>,
        previously_bound: &HashMap<Identifier, Span>,
        // cont: &Closure
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

            let expr = Expression::parse(expr.clone(), env /* cont */).await?;

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
        env: &Environment<impl Top>,
        span: &Span,
        // cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, ParseAstError> {
        match exprs {
            [cond, success] => Ok(If {
                cond: Arc::new(Expression::parse(cond.clone(), env /* cont */).await?),
                success: Arc::new(Expression::parse(success.clone(), env /* cont */).await?),
                failure: None,
            }),
            [cond, success, failure] => Ok(If {
                cond: Arc::new(Expression::parse(cond.clone(), env /* cont */).await?),
                success: Arc::new(Expression::parse(success.clone(), env /* cont */).await?),
                failure: Some(Arc::new(
                    Expression::parse(failure.clone(), env /* cont */).await?,
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
        env: &Environment<impl Top>,
        // cont: &Closure
    ) -> Result<Self, ParseAstError> {
        let mut output = Vec::new();
        for expr in exprs {
            let expr = Expression::parse(expr.clone(), env /* cont */).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

impl Or {
    async fn parse(
        exprs: &[Syntax],
        env: &Environment<impl Top>,
        // cont: &Closure
    ) -> Result<Self, ParseAstError> {
        let mut output = Vec::new();
        for expr in exprs {
            let expr = Expression::parse(expr.clone(), env /* cont */).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

impl Set {
    async fn parse(
        exprs: &[Syntax],
        env: &Environment<impl Top>,
        span: &Span,
        // cont: &Closure,
    ) -> Result<Self, ParseAstError> {
        match exprs {
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [Syntax::Identifier { ident, .. }, expr] => Ok(Set {
                var: env
                    .fetch_var(ident)
                    .ok_or_else(|| ParseAstError::UndefinedVariable(ident.clone()))?,
                val: Arc::new(Expression::parse(expr.clone(), env /* cont */).await?),
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
    #[allow(dead_code)]
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
        env: &Environment<impl Top>,
        span: &Span,
        // cont: &Closure,
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
            arg: Arc::new(Expression::parse(arg.clone(), env /* cont */).await?),
            transformer: Transformer {
                rules: syntax_rules,
                is_variable_transformer: false,
            },
        })
    }
}
