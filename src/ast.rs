//! Data structures for expanding and representing Scheme code.

use crate::{
    cps::{Compile, PrimOp},
    env::{CapturedEnv, Environment, Local, Var},
    expand::SyntaxRule,
    expand::Transformer,
    gc::{Gc, Trace},
    num::{Number, NumberToUsizeError},
    proc::{Closure, FunctionDebugInfo},
    records::DefineRecordType,
    runtime::{CallSiteId, FunctionDebugInfoId, Runtime},
    syntax::{FullyExpanded, Identifier},
    syntax::{Span, Syntax},
    value::Value,
};
use either::Either;

use derive_more::From;
use futures::future::BoxFuture;
use inkwell::builder::BuilderError;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Debug)]
pub enum ParseAstError {
    /// The most general error. Something just looks bad.
    ///
    /// This error type should be avoided, and instead one should use a more specific error type,
    /// or create one.
    BadForm(Span),

    ExpectedArgument(Span),
    ExpectedBody(Span),
    ExpectedIdentifier(Span),
    ExpectedNumber(Span),
    ExpectedVariableTransformer,
    ExpectedInteger(NumberToUsizeError),

    UnexpectedArgument(Span),
    UnexpectedDefinition(Span),
    UnexpectedEmptyList(Span),

    UndefinedVariable(Identifier),

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

    BuilderError(BuilderError),

    RaisedValue(Value),
}

impl From<BuilderError> for ParseAstError {
    fn from(be: BuilderError) -> Self {
        Self::BuilderError(be)
    }
}

impl From<Value> for ParseAstError {
    fn from(raised: Value) -> Self {
        Self::RaisedValue(raised)
    }
}

#[derive(Debug, Clone, Trace)]
pub enum Definition {
    DefineVar(DefineVar),
    DefineFunc(DefineFunc),
    DefineRecordType(DefineRecordType),
}

#[derive(Debug, Clone, Trace)]
pub struct DefineVar {
    pub var: Var,
    pub val: Arc<Expression>,
    pub next: Option<Either<Box<Definition>, ExprBody>>,
}

#[derive(Debug, Clone, Trace)]
pub struct DefineFunc {
    pub var: Var,
    pub args: Formals,
    pub body: Box<DefinitionBody>,
    pub next: Option<Either<Box<Definition>, ExprBody>>,
    pub debug_info_id: FunctionDebugInfoId,
}

impl Definition {
    fn set_next(self, next: Either<Box<Definition>, ExprBody>) -> Self {
        match self {
            Self::DefineVar(mut def_var) => {
                def_var.next = Some(next);
                Self::DefineVar(def_var)
            }
            Self::DefineFunc(mut def_func) => {
                def_func.next = Some(next);
                Self::DefineFunc(def_func)
            }
            _ => todo!(),
        }
    }

    async fn parse(
        runtime: &Gc<Runtime>,
        syn: &[Syntax],
        env: &Environment,
        span: &Span,
        // cont: &Closure
    ) -> Result<Self, ParseAstError> {
        match syn {
            [_, Syntax::Identifier { ident, .. }, expr, Syntax::Null { .. }] => {
                Ok(Definition::DefineVar(DefineVar {
                    var: env.fetch_var(ident).unwrap(),
                    val: Arc::new(Expression::parse(runtime, expr.clone(), env /* cont */).await?),
                    next: None,
                }))
            }
            [_, Syntax::List { list, span }, body @ .., Syntax::Null { .. }] => {
                if body.is_empty() {
                    return Err(ParseAstError::ExpectedBody(span.clone()));
                }
                match list.as_slice() {
                    [Syntax::Identifier {
                        ident: func_name,
                        span: func_span,
                        ..
                    }, args @ ..] => {
                        let var = env.fetch_var(func_name).unwrap();

                        let mut bound = HashMap::<Identifier, Span>::new();
                        let mut fixed = Vec::new();
                        let new_env = env.new_lexical_contour();
                        let mut arg_names = Vec::new();

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
                                    arg_names.push(ident.name.clone());
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
                                    arg_names.push(ident.name.clone());
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
                        let body = DefinitionBody::parse(
                            runtime, body, &new_env, func_span, /* cont */
                        )
                        .await?;

                        // Create the new debug info:
                        let debug_info_id = runtime.write().debug_info.new_function_debug_info(
                            FunctionDebugInfo::new(
                                Some(func_name.name.clone()),
                                arg_names,
                                span.clone(),
                            ),
                        );

                        Ok(Self::DefineFunc(DefineFunc {
                            var,
                            args,
                            body: Box::new(body),
                            next: None,
                            debug_info_id,
                        }))
                    }
                    _ => Err(ParseAstError::BadForm(span.clone())),
                }
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

pub(super) async fn define_syntax(
    runtime: &Gc<Runtime>,
    ident: Identifier,
    expr: Syntax,
    env: &Environment,
    // cont: &Closure
) -> Result<(), ParseAstError> {
    let FullyExpanded {
        expanded,
        expansion_env,
    } = expr.expand(env /* cont */).await?;

    let expr = Expression::parse(runtime, expanded, &expansion_env /* cont */).await?;
    let cps_expr = expr.compile_top_level();
    let mac = runtime
        .compile_expr(cps_expr)
        .await?
        .call(&[])
        .await
        .map_err(|err| ParseAstError::RaisedValue(err.into()))?;
    let transformer: Gc<Closure> = mac[0].clone().try_into().unwrap();
    env.def_macro(ident, transformer);

    Ok(())
}

#[derive(Debug, Clone, Trace)]
pub enum Expression {
    Undefined,
    Literal(Literal),
    Quote(Quote),
    SyntaxQuote(SyntaxQuote),
    SyntaxCase(SyntaxCase),
    Apply(Apply),
    Let(Let),
    If(If),
    And(And),
    Or(Or),
    Lambda(Lambda),
    Set(Set),
    Var(Var),
    Vector(Vector),
    ByteVector(Vec<u8>),
    Begin(ExprBody),
}

impl Expression {
    pub async fn parse(
        runtime: &Gc<Runtime>,
        syn: Syntax,
        env: &Environment,
        // cont: &Closure
    ) -> Result<Self, ParseAstError> {
        let FullyExpanded {
            expansion_env,
            expanded,
        } = syn.expand(env /* cont */).await?;
        Self::parse_expanded(runtime, expanded, &expansion_env /* cont*/).await
    }

    fn parse_expanded<'a>(
        runtime: &'a Gc<Runtime>,
        syn: Syntax,
        env: &'a Environment,
        // cont: &Closure,
    ) -> BoxFuture<'a, Result<Self, ParseAstError>> {
        Box::pin(async move {
            match syn {
                Syntax::Null { span } => Err(ParseAstError::UnexpectedEmptyList(span)),

                // Special Identifiers:
                Syntax::Identifier { ident, .. } if ident.name == "<undefined>" => {
                    Ok(Self::Undefined)
                }

                // Regular identifiers:
                Syntax::Identifier { ident, .. } => Ok(Self::Var(
                    env.fetch_var(&ident)
                        .or_else(|| {
                            let top = env.fetch_top();
                            let is_repl = { top.read().is_repl() };
                            is_repl.then(|| {
                                Var::Global(top.write().def_var(ident.clone(), Value::undefined()))
                            })
                        })
                        .ok_or_else(|| ParseAstError::UndefinedVariable(ident.clone()))?,
                )),

                // Literals:
                Syntax::Literal { literal, .. } => Ok(Self::Literal(literal)),

                // Vector literals:
                Syntax::Vector { vector, .. } => Ok(Self::Vector(Vector::parse(&vector))),
                Syntax::ByteVector { vector, .. } => Ok(Self::ByteVector(vector)),

                // Functional forms:
                Syntax::List {
                    list: exprs, span, ..
                } => match exprs.as_slice() {
                    // Special forms:
                    [Syntax::Identifier { ident, bound, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "begin" && !bound =>
                    {
                        ExprBody::parse(runtime, tail, env /* cont */)
                            .await
                            .map(Expression::Begin)
                    }
                    [Syntax::Identifier { ident, span, bound }, tail @ .., Syntax::Null { .. }]
                        if ident == "lambda" && !bound =>
                    {
                        Lambda::parse(runtime, tail, env, span /* cont */)
                            .await
                            .map(Expression::Lambda)
                    }
                    [Syntax::Identifier { ident, span, bound }, tail @ .., Syntax::Null { .. }]
                        if ident == "let" && !bound =>
                    {
                        Let::parse(runtime, tail, env, span /* cont */)
                            .await
                            .map(Expression::Let)
                    }
                    [Syntax::Identifier { ident, bound, .. }, bindings, tail @ .., Syntax::Null { .. }]
                        if ident == "let-syntax" && !bound =>
                    {
                        let new_env = parse_let_syntax(runtime, false, bindings, env).await?;
                        ExprBody::parse(runtime, tail, &new_env)
                            .await
                            .map(Expression::Begin)
                    }
                    [Syntax::Identifier { ident, bound, .. }, bindings, tail @ .., Syntax::Null { .. }]
                        if ident == "letrec-syntax" && !bound =>
                    {
                        let new_env = parse_let_syntax(runtime, true, bindings, env).await?;
                        ExprBody::parse(runtime, tail, &new_env)
                            .await
                            .map(Expression::Begin)
                    }
                    [Syntax::Identifier { ident, span, bound }, tail @ .., Syntax::Null { .. }]
                        if ident == "if" && !bound =>
                    {
                        If::parse(runtime, tail, env, span /* cont */)
                            .await
                            .map(Expression::If)
                    }
                    [Syntax::Identifier { ident, bound, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "and" && !bound =>
                    {
                        And::parse(runtime, tail, env /* cont */)
                            .await
                            .map(Expression::And)
                    }
                    [Syntax::Identifier { ident, bound, .. }, tail @ .., Syntax::Null { .. }]
                        if ident == "or" && !bound =>
                    {
                        Or::parse(runtime, tail, env /* cont */)
                            .await
                            .map(Expression::Or)
                    }
                    [Syntax::Identifier { ident, span, bound }, tail @ ..]
                        if ident == "quote" && !bound =>
                    {
                        Quote::parse(tail, span).await.map(Expression::Quote)
                    }
                    [Syntax::Identifier { ident, span, bound }, tail @ .., Syntax::Null { .. }]
                        if ident == "syntax" && !bound =>
                    {
                        SyntaxQuote::parse(tail, span)
                            .await
                            .map(Expression::SyntaxQuote)
                    }
                    [Syntax::Identifier { ident, span, bound }, tail @ .., Syntax::Null { .. }]
                        if ident == "syntax-case" && !bound =>
                    {
                        SyntaxCase::parse(runtime, tail, env, span /* cont */)
                            .await
                            .map(Expression::SyntaxCase)
                    }

                    // Extra special form (set!):
                    [Syntax::Identifier { ident, span, bound }, tail @ .., Syntax::Null { .. }]
                        if ident == "set!" && !bound =>
                    {
                        Set::parse(runtime, tail, env, span /* cont */)
                            .await
                            .map(Expression::Set)
                    }

                    // Definition in expression context is illegal:
                    [Syntax::Identifier { ident, span, bound }, .., Syntax::Null { .. }]
                        if !bound && (ident == "define" || ident == "define-record-type") =>
                    {
                        Err(ParseAstError::UnexpectedDefinition(span.clone()))
                    }

                    // Regular old function call:
                    [operator, args @ .., Syntax::Null { .. }] => {
                        Apply::parse(runtime, operator.clone(), args, env /* cont */)
                            .await
                            .map(Expression::Apply)
                    }

                    _ => Err(ParseAstError::BadForm(span.clone())),
                },
            }
        })
    }

    // These function pointer comparisons are guaranteed to be meaningful since
    // they are returned from a store.
    #[allow(unpredictable_function_pointer_comparisons)]
    pub fn to_primop(&self) -> Option<PrimOp> {
        use crate::{
            num::{
                add_builtin, div_builtin, equal_builtin, greater_builtin, greater_equal_builtin,
                lesser_builtin, lesser_equal_builtin, mul_builtin, sub_builtin,
            },
            proc::{Closure, FuncPtr::Bridge},
        };

        if let Expression::Var(Var::Global(global)) = self {
            let val = global.value_ref().read().clone();
            let val: Gc<Closure> = val.try_into().ok()?;
            let val_read = val.read();
            match val_read.func {
                Bridge(ptr) if ptr == add_builtin => Some(PrimOp::Add),
                Bridge(ptr) if ptr == sub_builtin => Some(PrimOp::Sub),
                Bridge(ptr) if ptr == mul_builtin => Some(PrimOp::Mul),
                Bridge(ptr) if ptr == div_builtin => Some(PrimOp::Div),
                Bridge(ptr) if ptr == equal_builtin => Some(PrimOp::Equal),
                Bridge(ptr) if ptr == greater_builtin => Some(PrimOp::Greater),
                Bridge(ptr) if ptr == greater_equal_builtin => Some(PrimOp::GreaterEqual),
                Bridge(ptr) if ptr == lesser_builtin => Some(PrimOp::Lesser),
                Bridge(ptr) if ptr == lesser_equal_builtin => Some(PrimOp::LesserEqual),
                _ => None,
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Trace)]
// Vector should be in here too. Oh well.
pub enum Literal {
    Number(Number),
    Boolean(bool),
    Character(char),
    String(String),
    ByteVector(Vec<u8>),
}

#[derive(Debug, Clone, Trace)]
pub struct Quote {
    pub val: Value,
}

impl Quote {
    async fn parse(exprs: &[Syntax], span: &Span) -> Result<Self, ParseAstError> {
        match exprs {
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [Syntax::Null { .. }] => Ok(Quote { val: Value::null() }),
            [expr, Syntax::Null { .. }] => Ok(Quote {
                val: Value::datum_from_syntax(expr),
            }),
            [_, arg, ..] => Err(ParseAstError::UnexpectedArgument(arg.span().clone())),
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct SyntaxQuote {
    pub syn: Syntax,
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

#[derive(Debug, Clone, Trace)]
pub struct Apply {
    pub operator: Either<Box<Expression>, PrimOp>,
    pub args: Vec<Expression>,
    pub call_site_id: CallSiteId,
}

impl Apply {
    async fn parse(
        runtime: &Gc<Runtime>,
        operator: Syntax,
        args: &[Syntax],
        env: &Environment,
        // cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, ParseAstError> {
        let location = operator.span().clone();
        let call_site_id = runtime.write().debug_info.new_call_site(location);
        let proc_name = match operator {
            Syntax::Identifier { ref ident, .. } => ident.name.clone(),
            _ => String::from(""),
        };
        let operator = match proc_name.parse::<PrimOp>() {
            Ok(prim_op) => Either::Right(prim_op),
            _ => Either::Left(Box::new(
                Expression::parse(runtime, operator, env /* cont */).await?,
            )),
        };
        let mut parsed_args = Vec::new();
        for arg in args {
            parsed_args.push(Expression::parse(runtime, arg.clone(), env /* cont */).await?);
        }
        Ok(Apply {
            operator,
            args: parsed_args,
            call_site_id,
        })
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Lambda {
    pub args: Formals,
    pub body: DefinitionBody,
    pub debug_info_id: FunctionDebugInfoId,
}

impl Lambda {
    async fn parse(
        runtime: &Gc<Runtime>,
        sexprs: &[Syntax],
        env: &Environment,
        span: &Span,
        // cont: &Closure,
    ) -> Result<Self, ParseAstError> {
        match sexprs {
            [Syntax::Null { .. }, body @ ..] => {
                parse_lambda(runtime, &[], body, env, span /* cont */).await
            }
            [Syntax::List { list: args, .. }, body @ ..] => {
                parse_lambda(runtime, args, body, env, span /* cont */).await
            }
            [ident @ Syntax::Identifier { .. }, body @ ..] => {
                let args = &[ident.clone()];
                parse_lambda(runtime, args, body, env, span /* cont */).await
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

async fn parse_lambda(
    runtime: &Gc<Runtime>,
    args: &[Syntax],
    body: &[Syntax],
    env: &Environment,
    span: &Span,
    // cont: &Closure
) -> Result<Lambda, ParseAstError> {
    let mut bound = HashMap::<Identifier, Span>::new();
    let mut fixed = Vec::new();
    let new_contour = env.new_lexical_contour();
    let mut arg_names = Vec::new();

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
                    arg_names.push(ident.name.clone());
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
                arg_names.push(ident.name.clone());
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

    let body = DefinitionBody::parse(runtime, body, &new_contour, span /* cont */).await?;

    // Create the new debug info:
    let debug_info_id = runtime
        .write()
        .debug_info
        .new_function_debug_info(FunctionDebugInfo::new(None, arg_names, span.clone()));

    Ok(Lambda {
        args,
        body,
        debug_info_id,
    })
}

#[derive(Debug, Clone, Trace)]
pub struct Let {
    pub bindings: Vec<(Local, Expression)>,
    pub body: DefinitionBody,
}

impl Let {
    pub fn new(bindings: Vec<(Local, Expression)>, body: DefinitionBody) -> Self {
        Self { bindings, body }
    }

    async fn parse(
        runtime: &Gc<Runtime>,
        syn: &[Syntax],
        env: &Environment,
        span: &Span,
        // cont: &Closure,
    ) -> Result<Self, ParseAstError> {
        match syn {
            [Syntax::Null { .. }, body @ ..] => {
                parse_let(runtime, None, &[], body, env, span /* cont */).await
            }
            [Syntax::List { list: bindings, .. }, body @ ..] => {
                parse_let(runtime, None, bindings, body, env, span /* cont */).await
            }
            // Named let:
            [Syntax::Identifier { ident, .. }, Syntax::List { list: bindings, .. }, body @ ..] => {
                parse_let(
                    runtime,
                    Some(ident),
                    bindings,
                    body,
                    env,
                    span, /* cont */
                )
                .await
            }
            [Syntax::Identifier { ident, .. }, Syntax::Null { .. }, body @ ..] => {
                parse_let(runtime, Some(ident), &[], body, env, span /* cont */).await
            }
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

async fn parse_let(
    runtime: &Gc<Runtime>,
    name: Option<&Identifier>,
    bindings: &[Syntax],
    body: &[Syntax],
    env: &Environment,
    span: &Span,
    // cont: &Closure
) -> Result<Let, ParseAstError> {
    let mut previously_bound = HashMap::new();
    let mut parsed_bindings = Vec::new();
    let mut binding_names = Vec::new();
    let new_contour = env.new_lexical_contour();

    match bindings {
        [] | [Syntax::Null { .. }] => (),
        [bindings @ .., Syntax::Null { .. }] => {
            for binding in bindings {
                let binding =
                    LetBinding::parse(runtime, binding, env, &previously_bound /* cont */).await?;
                previously_bound.insert(binding.ident.clone(), binding.span.clone());
                let Var::Local(var) = new_contour.def_var(binding.ident.clone()) else {
                    unreachable!()
                };
                binding_names.push(binding.ident.name.clone());
                parsed_bindings.push((var, binding));
            }
        }
        _ => {
            return Err(ParseAstError::BadForm(span.clone()));
        }
    }

    let lambda_var = name.map(|name| new_contour.def_var(name.clone()));

    let ast_body = DefinitionBody::parse(runtime, body, &new_contour, span /* cont */).await?;

    // TODO: Lot of unnecessary cloning here, fix that.
    let mut bindings: Vec<_> = parsed_bindings
        .iter()
        .map(|(var, binding)| (*var, binding.expr.clone()))
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

        let debug_info_id =
            runtime
                .write()
                .debug_info
                .new_function_debug_info(FunctionDebugInfo::new(
                    Some(name.as_ref().unwrap().name.clone()),
                    binding_names,
                    span.clone(),
                ));

        let lambda = Lambda {
            args: Formals::FixedArgs(args),
            body: DefinitionBody::parse(runtime, body, &new_new_contour, span /* cont */).await?,
            debug_info_id,
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
        runtime: &Gc<Runtime>,
        form: &Syntax,
        env: &Environment,
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

            let expr = Expression::parse(runtime, expr.clone(), env /* cont */).await?;

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

#[derive(Debug, Clone, Trace)]
pub struct Set {
    pub var: Var,
    pub val: Arc<Expression>,
}

impl Set {
    async fn parse(
        runtime: &Gc<Runtime>,
        exprs: &[Syntax],
        env: &Environment,
        span: &Span,
        // cont: &Closure,
    ) -> Result<Self, ParseAstError> {
        match exprs {
            [] => Err(ParseAstError::ExpectedArgument(span.clone())),
            [Syntax::Identifier { ident, .. }, expr] => Ok(Set {
                var: env
                    .fetch_var(ident)
                    .ok_or_else(|| ParseAstError::UndefinedVariable(ident.clone()))?,
                val: Arc::new(Expression::parse(runtime, expr.clone(), env /* cont */).await?),
            }),
            [arg1, _] => Err(ParseAstError::ExpectedIdentifier(arg1.span().clone())),
            [_, _, arg3, ..] => Err(ParseAstError::UnexpectedArgument(arg3.span().clone())),
            _ => Err(ParseAstError::BadForm(span.clone())),
        }
    }
}

#[derive(Debug, Clone, Trace)]
pub struct If {
    pub cond: Arc<Expression>,
    pub success: Arc<Expression>,
    pub failure: Option<Arc<Expression>>,
}

impl If {
    async fn parse(
        runtime: &Gc<Runtime>,
        exprs: &[Syntax],
        env: &Environment,
        span: &Span,
        // cont: &Option<Arc<Continuation>>,
    ) -> Result<Self, ParseAstError> {
        match exprs {
            [cond, success] => Ok(If {
                cond: Arc::new(Expression::parse(runtime, cond.clone(), env /* cont */).await?),
                success: Arc::new(
                    Expression::parse(runtime, success.clone(), env /* cont */).await?,
                ),
                failure: None,
            }),
            [cond, success, failure] => Ok(If {
                cond: Arc::new(Expression::parse(runtime, cond.clone(), env /* cont */).await?),
                success: Arc::new(
                    Expression::parse(runtime, success.clone(), env /* cont */).await?,
                ),
                failure: Some(Arc::new(
                    Expression::parse(runtime, failure.clone(), env /* cont */).await?,
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

#[derive(Debug, Clone, Trace)]
pub enum Formals {
    FixedArgs(Vec<Local>),
    VarArgs { fixed: Vec<Local>, remaining: Local },
}

impl Formals {
    pub fn iter(&self) -> impl Iterator<Item = &'_ Local> {
        let fixed_iter = match self {
            Self::FixedArgs(fixed) => fixed.iter(),
            Self::VarArgs { fixed, .. } => fixed.iter(),
        };
        let remaining = match self {
            Self::FixedArgs(_) => None,
            Self::VarArgs { ref remaining, .. } => Some(remaining),
        };
        fixed_iter.chain(remaining)
    }

    pub fn is_variadic(&self) -> bool {
        matches!(self, Self::VarArgs { .. })
    }
}

/*
impl Formals {
    pub fn to_args_and_remaining(&self) -> (Vec<Identifier>, Option<Identifier>) {
        match self {
            Self::VarArgs { fixed, remaining } => (fixed.clone(), Some(remaining.clone())),
            Self::FixedArgs(args) => (args.clone(), None),
        }
    }

    pub fn args_to_alloc(&self) -> usize {
        match self {
            Self::VarArgs { fixed, .. } => fixed.len() + 1,
            Self::FixedArgs(args) => args.len(),
        }
    }
}
 */

#[derive(Debug, Clone, Trace)]
pub struct DefinitionBody {
    pub first: Either<Definition, ExprBody>,
}

impl DefinitionBody {
    pub fn new(first: Either<Definition, ExprBody>) -> Self {
        Self { first }
    }

    pub async fn parse_program_body(
        runtime: &Gc<Runtime>,
        body: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        Self::parse_helper(runtime, body, true, env, span).await
    }

    pub async fn parse(
        runtime: &Gc<Runtime>,
        body: &[Syntax],
        env: &Environment,
        span: &Span,
    ) -> Result<Self, ParseAstError> {
        Self::parse_helper(runtime, body, false, env, span).await
    }

    /// Parse the body. body is expected to be a list of valid syntax objects, and should not include
    /// _any_ nulls, including one at the end.
    fn parse_helper<'a>(
        runtime: &'a Gc<Runtime>,
        body: &'a [Syntax],
        permissive: bool,
        env: &'a Environment,
        span: &'a Span,
        // cont: &'a Closure
    ) -> BoxFuture<'a, Result<Self, ParseAstError>> {
        Box::pin(async move {
            let mut defs = Vec::new();
            let mut exprs = Vec::new();

            splice_in(
                runtime, permissive, body, env, span, &mut defs, &mut exprs, /* cont */
            )
            .await?;

            let mut defs_parsed = Vec::new();
            let mut exprs_parsed = Vec::new();

            // Mark all of the defs as defined:
            for def in defs.iter() {
                if let Some([_, def, ..]) = def.as_ref().left().and_then(|d| d.expanded.as_list()) {
                    let ident = match def.as_list() {
                        Some([Syntax::Identifier { ident, .. }, ..]) => ident,
                        _ => def
                            .as_ident()
                            .ok_or(ParseAstError::BadForm(def.span().clone()))?,
                    };
                    env.def_var(ident.clone());
                }
            }

            for def in defs.into_iter() {
                let def = match def {
                    Either::Left(def) => {
                        Definition::parse(
                            runtime,
                            def.expanded.as_list().unwrap(),
                            &def.expansion_env,
                            def.expanded.span(),
                            /* cont */
                        )
                        .await?
                        // expansion_env.def_var(ident.clone());
                    }
                    Either::Right(def_record) => Definition::DefineRecordType(def_record),
                };
                defs_parsed.push(def);
            }

            for expr in exprs.into_iter() {
                // let new_expansion_env = env.push_expansion_env(expr.expansion_ctxs);
                exprs_parsed.push(
                    Expression::parse_expanded(
                        runtime,
                        expr.expanded,
                        &expr.expansion_env, /* cont */
                    )
                    .await?,
                );
            }

            let expr_body = ExprBody::new(exprs_parsed);
            if let Some(last_def) = defs_parsed.pop() {
                let mut last_def = last_def.set_next(Either::Right(expr_body));
                for next_def in defs_parsed.into_iter().rev() {
                    last_def = next_def.set_next(Either::Left(Box::new(last_def)));
                }
                Ok(Self::new(Either::Left(last_def)))
            } else {
                Ok(Self::new(Either::Right(expr_body)))
            }
        })
    }
}

#[derive(Debug, Clone, Trace)]
pub struct ExprBody {
    pub exprs: Vec<Expression>,
}

impl ExprBody {
    pub fn new(exprs: Vec<Expression>) -> Self {
        Self { exprs }
    }

    /// Differs from Body by being purely expression based. No definitions allowed.
    async fn parse(
        runtime: &Gc<Runtime>,
        body: &[Syntax],
        env: &Environment,
        // cont: &Closure
    ) -> Result<Self, ParseAstError> {
        let mut exprs = Vec::new();
        for sexpr in body {
            let parsed = Expression::parse(runtime, sexpr.clone(), env /* cont */).await?;
            exprs.push(parsed);
        }
        Ok(Self { exprs })
    }
}

fn splice_in<'a>(
    runtime: &'a Gc<Runtime>,
    permissive: bool,
    body: &'a [Syntax],
    env: &'a Environment,
    span: &'a Span,
    defs: &'a mut Vec<Either<FullyExpanded, DefineRecordType>>,
    exprs: &'a mut Vec<FullyExpanded>,
    // cont: &Closure
) -> BoxFuture<'a, Result<(), ParseAstError>> {
    Box::pin(async move {
        if body.is_empty() {
            return Err(ParseAstError::ExpectedBody(span.clone()));
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
                        splice_in(
                            runtime,
                            permissive,
                            body,
                            &expansion_env,
                            span, /* cont */
                            defs,
                            exprs,
                        )
                        .await?;
                        continue;
                    }

                    Some(
                        [Syntax::Identifier { ident, .. }, Syntax::Identifier { ident: name, .. }, expr, Syntax::Null { .. }],
                    ) if ident == "define-syntax" => {
                        define_syntax(
                            runtime,
                            name.clone(),
                            expr.clone(),
                            &expansion_env, /* cont */
                        )
                        .await?;
                        continue;
                    }

                    Some(
                        [Syntax::Identifier { ident, span, .. }, bindings, form @ .., Syntax::Null { .. }],
                    ) if ident == "let-syntax" => {
                        let new_env =
                            parse_let_syntax(runtime, false, bindings, &expansion_env).await?;
                        splice_in(runtime, permissive, form, &new_env, span, defs, exprs).await?;
                        continue;
                    }

                    Some(
                        [Syntax::Identifier { ident, span, .. }, bindings, form @ .., Syntax::Null { .. }],
                    ) if ident == "letrec-syntax" => {
                        let new_env =
                            parse_let_syntax(runtime, true, bindings, &expansion_env).await?;
                        splice_in(runtime, permissive, form, &new_env, span, defs, exprs).await?;
                        continue;
                    }

                    Some([Syntax::Identifier { ident, span, .. }, _, .., Syntax::Null { .. }])
                        if ident == "define" =>
                    {
                        if !permissive && !exprs.is_empty() {
                            return Err(ParseAstError::UnexpectedDefinition(span.clone()));
                        }
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

async fn parse_let_syntax(
    runtime: &Gc<Runtime>,
    recursive: bool,
    bindings: &Syntax,
    env: &Environment,
) -> Result<Environment, ParseAstError> {
    let Some([keyword_bindings @ .., Syntax::Null { .. }]) = bindings.as_list() else {
        return Err(ParseAstError::BadForm(bindings.span().clone()));
    };

    let new_env = env.new_let_syntax_contour(recursive);

    for binding in keyword_bindings {
        if let Some([Syntax::Identifier { ident: keyword, .. }, expr, Syntax::Null { .. }]) =
            binding.as_list()
        {
            define_syntax(runtime, keyword.clone(), expr.clone(), &new_env).await?;
        } else {
            return Err(ParseAstError::BadForm(binding.span().clone()));
        }
    }

    Ok(new_env)
}

#[derive(Debug, Clone, Trace)]
pub struct And {
    pub args: Vec<Expression>,
}

impl And {
    pub fn new(args: Vec<Expression>) -> Self {
        Self { args }
    }
}

impl And {
    async fn parse(
        runtime: &Gc<Runtime>,
        exprs: &[Syntax],
        env: &Environment,
        // cont: &Closure
    ) -> Result<Self, ParseAstError> {
        let mut output = Vec::new();
        for expr in exprs {
            let expr = Expression::parse(runtime, expr.clone(), env /* cont */).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Or {
    pub args: Vec<Expression>,
}

impl Or {
    pub fn new(args: Vec<Expression>) -> Self {
        Self { args }
    }

    async fn parse(
        runtime: &Gc<Runtime>,
        exprs: &[Syntax],
        env: &Environment,
        // cont: &Closure
    ) -> Result<Self, ParseAstError> {
        let mut output = Vec::new();
        for expr in exprs {
            let expr = Expression::parse(runtime, expr.clone(), env /* cont */).await?;
            output.push(expr);
        }
        Ok(Self::new(output))
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Vector {
    pub vals: Vec<Value>,
}

impl Vector {
    #[allow(dead_code)]
    fn parse(exprs: &[Syntax]) -> Self {
        let mut vals = Vec::new();
        for expr in exprs {
            vals.push(Value::datum_from_syntax(expr));
        }
        Self { vals }
    }
}

#[derive(derive_more::Debug, Clone, Trace)]
pub struct SyntaxCase {
    pub arg: Arc<Expression>,
    pub transformer: Transformer,
    #[debug(skip)]
    pub captured_env: CapturedEnv,
}

impl SyntaxCase {
    async fn parse(
        runtime: &Gc<Runtime>,
        exprs: &[Syntax],
        env: &Environment,
        span: &Span,
        // cont: &Closure,
    ) -> Result<Self, ParseAstError> {
        // Get every possible variable referenced by the transformer:
        let captured_locals = fetch_all_identifiers(exprs)
            .into_iter()
            .flat_map(|ident| env.fetch_local(&ident))
            .collect();
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
            arg: Arc::new(Expression::parse(runtime, arg.clone(), env /* cont */).await?),
            transformer: Transformer {
                rules: syntax_rules,
                is_variable_transformer: false,
            },
            captured_env: CapturedEnv::new(env.clone(), captured_locals),
        })
    }
}

fn fetch_all_identifiers(syn: &[Syntax]) -> HashSet<Identifier> {
    let mut idents = HashSet::new();
    for syn in syn {
        syn.fetch_all_identifiers(&mut idents);
    }
    idents
}
