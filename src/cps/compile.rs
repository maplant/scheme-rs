use super::*;
use crate::{
    ast::*,
    expand::{ExpansionCombiner, SyntaxRule},
    records::Record,
    syntax::{Identifier, Syntax},
    value::Value as RuntimeValue,
};
use either::Either;
use indexmap::IndexSet;

/// There's not too much reason that this is a trait, other than I wanted to
/// see all of the Compile implementations in one place.
pub trait Compile: std::fmt::Debug {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps;

    /// The top level function takes no arguments.
    fn compile_top_level(&self) -> Cps {
        let k = Local::gensym();
        let result = Local::gensym();
        let compiled = Cps::Lambda {
            args: LambdaArgs::new(vec![result], true, None),
            body: Box::new(Cps::Halt(Value::from(result))),
            val: k,
            cexp: Box::new(self.compile(&mut |value| Cps::App(value, vec![Value::from(k)]))),
            span: None,
        };
        let mutable_vars = compiled.mutable_vars();
        compiled
            .vals_to_cells(&mutable_vars)
            .reduce()
            .add_pop_call_stack_insts(&mut HashSet::default())
    }
}

impl Compile for Lambda {
    /// Generates the maximally-correct implementation of a lambda, i.e. a closure that
    /// tail-calls a closure.
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        compile_lambda(
            self.args.iter().cloned().collect(),
            self.args.is_variadic(),
            &self.body,
            self.span.clone(),
            None,
            meta_cont,
        )
    }
}

fn compile_lambda(
    args: Vec<Local>,
    is_variadic: bool,
    body: &DefinitionBody,
    span: Span,
    name: Option<Symbol>,
    mut meta_cont: impl FnMut(Value) -> Cps,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let mut k4 = Local::gensym();
    k4.name = name;

    Cps::Lambda {
        args: LambdaArgs::new(vec![k2], false, None),
        body: Box::new(Cps::Lambda {
            args: LambdaArgs::new(args, is_variadic, Some(k3)),
            body: Box::new(body.compile(&mut |result| Cps::App(result, vec![Value::from(k3)]))),
            val: k4,
            cexp: Box::new(Cps::App(Value::from(k2), vec![Value::from(k4)])),
            span: Some(span.clone()),
        }),
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
        span: Some(span),
    }
}

impl Compile for Let {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        compile_let(&self.bindings, &self.body, meta_cont)
    }
}

fn compile_let(
    binds: &[(Local, Expression)],
    body: &DefinitionBody,
    meta_cont: &mut dyn FnMut(Value) -> Cps,
) -> Cps {
    if let Some(((curr_bind, curr_expr), tail)) = binds.split_first() {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Lambda {
                args: LambdaArgs::new(vec![*curr_bind], false, None),
                body: Box::new(compile_let(tail, body, &mut move |result| {
                    Cps::App(result, vec![Value::from(k2)])
                })),
                val: k3,
                cexp: Box::new(
                    curr_expr.compile(&mut move |result| Cps::App(result, vec![Value::from(k3)])),
                ),
                span: None,
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    } else {
        body.compile(meta_cont)
    }
}

impl Compile for Expression {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        match self {
            Self::Literal(l) => l.compile(meta_cont),
            Self::Apply(e) => e.compile(meta_cont),
            Self::Let(e) => e.compile(meta_cont),
            Self::If(e) => e.compile(meta_cont),
            Self::Lambda(e) => e.compile(meta_cont),
            Self::Var(v) => v.compile(meta_cont),
            Self::Begin(e) => e.compile(meta_cont),
            Self::And(e) => e.compile(meta_cont),
            Self::Or(e) => e.compile(meta_cont),
            Self::Quote(q) => q.compile(meta_cont),
            Self::SyntaxQuote(sq) => sq.compile(meta_cont),
            Self::SyntaxCase(sc) => sc.compile(meta_cont),
            Self::Set(set) => set.compile(meta_cont),
            Self::Undefined => compile_undefined(meta_cont),
            Self::Vector(vec) => vec.compile(meta_cont),
            Self::ByteVector(vec) => vec.compile(meta_cont),
        }
    }
}

impl Compile for Var {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(Value::from(k2), vec![Value::from(self.clone())])),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for &[Expression] {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        match self {
            [] => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Lambda {
                    args: LambdaArgs::new(vec![k2], false, None),
                    body: Box::new(Cps::App(Value::from(k2), Vec::new())),
                    val: k1,
                    cexp: Box::new(meta_cont(Value::from(k1))),
                    span: None,
                }
            }
            [last_expr] => last_expr.compile(meta_cont),
            [head, tail @ ..] => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Lambda {
                    args: LambdaArgs::new(vec![k1], true, None),
                    body: Box::new(tail.compile(meta_cont)),
                    val: k2,
                    cexp: Box::new(
                        head.compile(&mut move |result| Cps::App(result, vec![Value::from(k2)])),
                    ),
                    span: None,
                }
            }
        }
    }
}

fn compile_undefined(meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: LambdaArgs::new(vec![k2], false, None),
        body: Box::new(Cps::App(
            Value::from(k2),
            vec![Value::from(RuntimeValue::undefined())],
        )),
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
        span: None,
    }
}

impl Compile for Literal {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(RuntimeValue::from(self.clone()))],
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for ExprBody {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        self.exprs.as_slice().compile(meta_cont)
    }
}

impl Compile for Apply {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        compile_apply(&self.operator, &self.args, self.span.clone(), meta_cont)
    }
}

fn compile_apply(
    operator: &Expression,
    args: &[Expression],
    span: Span,
    meta_cont: &mut dyn FnMut(Value) -> Cps,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let k4 = Local::gensym();
    Cps::Lambda {
        args: LambdaArgs::new(vec![k2], false, None),
        body: Box::new(if let Some(primop) = operator.to_primop() {
            compile_primop(Value::from(k2), primop, Vec::new(), args)
        } else {
            let frame = if let Expression::Var(var) = operator
                && let Some(sym) = var.symbol()
            {
                Syntax::Identifier {
                    ident: Identifier::from_symbol(sym),
                    binding_env: None,
                    span,
                }
            } else {
                Syntax::Identifier {
                    ident: Identifier::new("<unknown>"),
                    binding_env: None,
                    span,
                }
            };
            operator.compile(&mut move |op_result| Cps::Lambda {
                args: LambdaArgs::new(vec![k3], false, None),
                body: Box::new(compile_apply_args(
                    Value::from(k2),
                    Value::from(k3),
                    Vec::new(),
                    args,
                    frame.clone(),
                )),
                val: k4,
                cexp: Box::new(Cps::App(op_result, vec![Value::from(k4)])),
                span: None,
            })
        }),
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
        span: None,
    }
}

fn compile_apply_args(
    cont: Value,
    op: Value,
    mut collected_args: Vec<Value>,
    remaining_args: &[Expression],
    frame: Syntax,
) -> Cps {
    let (arg, tail) = match remaining_args {
        [] => {
            collected_args.push(cont);
            return Cps::PrimOp(
                PrimOp::PushCallStack,
                vec![Value::from(RuntimeValue::from(frame))],
                Local::gensym(),
                Box::new(Cps::App(op, collected_args)),
            );
        }
        [arg, tail @ ..] => (arg, tail),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: LambdaArgs::new(vec![k2], false, None),
        body: Box::new({
            collected_args.push(Value::from(k2));
            compile_apply_args(cont, op, collected_args, tail, frame)
        }),
        val: k1,
        cexp: Box::new(arg.compile(&mut |result| Cps::App(result, vec![Value::from(k1)]))),
        span: None,
    }
}

fn compile_primop(
    cont: Value,
    primop: PrimOp,
    mut collected_args: Vec<Value>,
    remaining_args: &[Expression],
) -> Cps {
    let (arg, tail) = match remaining_args {
        [] => {
            let val = Local::gensym();
            return Cps::PrimOp(
                primop,
                collected_args,
                val,
                Box::new(Cps::App(cont, vec![Value::from(val)])),
            );
        }
        [arg, tail @ ..] => (arg, tail),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: LambdaArgs::new(vec![k2], false, None),
        body: Box::new({
            collected_args.push(Value::from(k2));
            compile_primop(cont, primop, collected_args, tail)
        }),
        val: k1,
        cexp: Box::new(arg.compile(&mut |result| Cps::App(result, vec![Value::from(k1)]))),
        span: None,
    }
}

impl Compile for If {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Lambda {
            args: LambdaArgs::new(vec![k1], false, None),
            body: Box::new(self.cond.compile(&mut |cond_result| {
                let k3 = Local::gensym();
                let cond_arg = Local::gensym();
                Cps::Lambda {
                    args: LambdaArgs::new(vec![cond_arg], false, None),
                    body: Box::new(Cps::If(
                        Value::from(cond_arg),
                        Box::new(
                            self.success
                                .compile(&mut |success| Cps::App(success, vec![Value::from(k1)])),
                        ),
                        Box::new(if let Some(ref failure) = self.failure {
                            failure.compile(&mut |failure| Cps::App(failure, vec![Value::from(k1)]))
                        } else {
                            Cps::App(Value::from(k1), Vec::new())
                        }),
                    )),
                    val: k3,
                    cexp: Box::new(Cps::App(cond_result, vec![Value::from(k3)])),
                    span: None,
                }
            })),
            val: k2,
            cexp: Box::new(meta_cont(Value::from(k2))),
            span: None,
        }
    }
}

impl Compile for And {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        compile_and(&self.args, meta_cont)
    }
}

fn compile_and(exprs: &[Expression], meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
    let (expr, tail) = match exprs {
        [] => return meta_cont(Value::from(RuntimeValue::from(true))),
        [expr] => (expr, None),
        [expr, tail @ ..] => (expr, Some(tail)),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: LambdaArgs::new(vec![k1], false, None),
        body: Box::new(expr.compile(&mut |expr_result| {
            let k3 = Local::gensym();
            let cond_arg = Local::gensym();
            Cps::Lambda {
                args: LambdaArgs::new(vec![cond_arg], false, None),
                body: Box::new(Cps::If(
                    Value::from(cond_arg),
                    Box::new(if let Some(tail) = tail {
                        compile_and(tail, &mut |expr| Cps::App(expr, vec![Value::from(k1)]))
                    } else {
                        Cps::App(Value::from(k1), vec![Value::from(RuntimeValue::from(true))])
                    }),
                    Box::new(Cps::App(
                        Value::from(k1),
                        vec![Value::from(RuntimeValue::from(false))],
                    )),
                )),
                val: k3,
                cexp: Box::new(Cps::App(expr_result, vec![Value::from(k3)])),
                span: None,
            }
        })),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
        span: None,
    }
}

impl Compile for Or {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        compile_or(&self.args, meta_cont)
    }
}

fn compile_or(exprs: &[Expression], meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
    let (expr, tail) = match exprs {
        [] => return meta_cont(Value::from(RuntimeValue::from(false))),
        [expr] => (expr, None),
        [expr, tail @ ..] => (expr, Some(tail)),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: LambdaArgs::new(vec![k1], false, None),
        body: Box::new(expr.compile(&mut |expr_result| {
            let k3 = Local::gensym();
            let cond_arg = Local::gensym();
            Cps::Lambda {
                args: LambdaArgs::new(vec![cond_arg], false, None),
                body: Box::new(Cps::If(
                    Value::from(cond_arg),
                    Box::new(Cps::App(
                        Value::from(k1),
                        vec![Value::from(RuntimeValue::from(true))],
                    )),
                    Box::new(if let Some(tail) = tail {
                        compile_or(tail, &mut |expr| Cps::App(expr, vec![Value::from(k1)]))
                    } else {
                        Cps::App(
                            Value::from(k1),
                            vec![Value::from(RuntimeValue::from(false))],
                        )
                    }),
                )),
                val: k3,
                cexp: Box::new(Cps::App(expr_result, vec![Value::from(k3)])),
                span: None,
            }
        })),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
        span: None,
    }
}

impl Compile for Definition {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        match self {
            Self::DefineVar(var) => var.compile(meta_cont),
            Self::DefineFunc(func) => func.compile(meta_cont),
        }
    }
}

impl Definition {
    fn alloc_cells(&self, wrap: Cps) -> Cps {
        match self {
            Self::DefineVar(def) => def.alloc_cells(wrap),
            Self::DefineFunc(func) => func.alloc_cells(wrap),
        }
    }
}

impl DefineVar {
    fn alloc_cells(&self, wrap: Cps) -> Cps {
        let cps = match self.var {
            Var::Global(_) => wrap,
            Var::Local(local) => Cps::PrimOp(PrimOp::AllocCell, Vec::new(), local, Box::new(wrap)),
        };
        next_or_wrap(&self.next, cps)
    }
}

impl DefineFunc {
    fn alloc_cells(&self, wrap: Cps) -> Cps {
        let cps = match self.var {
            Var::Global(_) => wrap,
            Var::Local(local) => Cps::PrimOp(PrimOp::AllocCell, Vec::new(), local, Box::new(wrap)),
        };
        next_or_wrap(&self.next, cps)
    }
}

impl Compile for DefinitionBody {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        match self.first {
            Either::Left(ref def) => def.alloc_cells(def.compile(meta_cont)),
            Either::Right(ref exprs) => exprs.compile(meta_cont),
        }
    }
}

fn next_or_wrap(next: &Option<Either<Box<Definition>, ExprBody>>, wrap: Cps) -> Cps {
    match next {
        Some(Either::Left(def)) => def.alloc_cells(wrap),
        _ => wrap,
    }
}

impl Compile for Option<Either<Box<Definition>, ExprBody>> {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        match self {
            Some(Either::Left(def)) => def.compile(meta_cont),
            Some(Either::Right(exprs)) => exprs.compile(meta_cont),
            _ => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Lambda {
                    args: LambdaArgs::new(vec![k1], false, None),
                    body: Box::new(Cps::App(Value::from(k1), Vec::new())),
                    val: k2,
                    cexp: Box::new(meta_cont(Value::from(k2))),
                    span: None,
                }
            }
        }
    }
}

impl Compile for Set {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Lambda {
                args: LambdaArgs::new(vec![expr_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var.clone()),
                        Value::Var(Var::Local(expr_result)),
                    ],
                    Local::gensym(),
                    Box::new(Cps::App(Value::from(k2), Vec::new())),
                )),
                val: k3,
                cexp: Box::new(
                    self.val
                        .compile(&mut move |result| Cps::App(result, vec![Value::from(k3)])),
                ),
                span: None,
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for DefineVar {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Lambda {
                args: LambdaArgs::new(vec![expr_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var.clone()),
                        Value::Var(Var::Local(expr_result)),
                    ],
                    Local::gensym(),
                    Box::new(
                        self.next
                            .compile(&mut move |result| Cps::App(result, vec![Value::from(k2)])),
                    ),
                )),
                val: k3,
                cexp: Box::new(
                    self.val
                        .compile(&mut move |result| Cps::App(result, vec![Value::from(k3)])),
                ),
                span: None,
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for DefineFunc {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let lambda_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Lambda {
                args: LambdaArgs::new(vec![lambda_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var.clone()),
                        Value::Var(Var::Local(lambda_result)),
                    ],
                    Local::gensym(),
                    Box::new(
                        self.next
                            .compile(&mut move |result| Cps::App(result, vec![Value::from(k2)])),
                    ),
                )),
                val: k3,
                cexp: Box::new(compile_lambda(
                    self.args.iter().cloned().collect(),
                    self.args.is_variadic(),
                    &self.body,
                    self.span.clone(),
                    self.var.symbol(),
                    |lambda_result| Cps::App(lambda_result, vec![Value::from(k3)]),
                )),
                span: None,
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for Quote {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(self.val.clone())],
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for SyntaxQuote {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let expanded = Local::gensym();

        let mut expansions_seen = IndexSet::new();
        let mut uses = HashMap::new();

        for (ident, expansion) in self.expansions.iter() {
            let (idx, _) = expansions_seen.insert_full(expansion);
            uses.insert(ident.clone(), idx);
        }

        let mut args = vec![
            Value::from(RuntimeValue::from(Record::from_rust_type(
                self.template.clone(),
            ))),
            Value::from(RuntimeValue::from(Record::from_rust_type(
                ExpansionCombiner { uses },
            ))),
        ];

        for expansion in expansions_seen.iter() {
            args.push(Value::from(**expansion));
        }

        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::PrimOp(
                PrimOp::ExpandTemplate,
                args,
                expanded,
                Box::new(Cps::App(Value::from(k2), vec![Value::from(expanded)])),
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for SyntaxCase {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(self.arg.compile(&mut |expr_result| {
                let k3 = Local::gensym();
                let arg = Local::gensym();
                Cps::Lambda {
                    args: LambdaArgs::new(vec![arg], false, None),
                    body: Box::new(compile_syntax_rules(&self.rules, arg, &mut |expanded| {
                        Cps::App(expanded, vec![Value::from(k2)])
                    })),
                    val: k3,
                    cexp: Box::new(Cps::App(expr_result, vec![Value::from(k3)])),
                    span: None,
                }
            })),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}
fn compile_syntax_rules(
    rules: &[SyntaxRule],
    arg: Local,
    meta_cont: &mut dyn FnMut(Value) -> Cps,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: LambdaArgs::new(vec![k2], false, None),
        body: match rules {
            [] => Box::new(Cps::PrimOp(
                PrimOp::ErrorNoPatternsMatch,
                Vec::new(),
                Local::gensym(),
                Box::new(Cps::App(Value::from(k2), Vec::new())),
            )),
            [rule, tail @ ..] => {
                let pattern = Record::from_rust_type(rule.pattern.clone());
                Box::new(Cps::PrimOp(
                    PrimOp::Matches,
                    vec![Value::from(RuntimeValue::from(pattern)), Value::from(arg)],
                    rule.binds,
                    {
                        let match_result = if rule.fender.is_some() {
                            Local::gensym()
                        } else {
                            rule.binds
                        };
                        let inner =
                            Box::new(Cps::If(
                                Value::from(match_result),
                                Box::new(rule.output_expression.compile(&mut |matches| {
                                    Cps::App(matches, vec![Value::from(k2)])
                                })),
                                Box::new(compile_syntax_rules(tail, arg, &mut |next_pattern| {
                                    Cps::App(next_pattern, vec![Value::from(k2)])
                                })),
                            ));
                        if let Some(fender) = &rule.fender {
                            let k3 = Local::gensym();
                            Box::new(Cps::Lambda {
                                args: LambdaArgs::new(vec![match_result], false, None),
                                body: inner,
                                val: k3,
                                cexp: Box::new(Cps::If(
                                    Value::from(rule.binds),
                                    // Check the fender
                                    Box::new(fender.compile(&mut |fender| {
                                        Cps::App(fender, vec![Value::from(k3)])
                                    })),
                                    Box::new(Cps::App(
                                        Value::from(k3),
                                        vec![Value::from(RuntimeValue::from(false))],
                                    )),
                                )),
                                span: None,
                            })
                        } else {
                            inner
                        }
                    },
                ))
            }
        },
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
        span: None,
    }
}

impl Compile for Vector {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();

        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(RuntimeValue::from(self.vals.clone()))],
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}
impl Compile for Vec<u8> {
    fn compile(&self, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();

        Cps::Lambda {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(RuntimeValue::from(self.clone()))],
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Cps {
    /// Convert arguments for closures into cells if they are written to
    fn vals_to_cells(self, mutable_vars: &HashSet<Local>) -> Self {
        match self {
            Self::PrimOp(PrimOp::AllocCell, vals, local, cexpr) => Self::PrimOp(
                PrimOp::AllocCell,
                vals,
                local,
                Box::new(cexpr.vals_to_cells(mutable_vars)),
            ),
            Self::PrimOp(op, vals, local, cexpr) => {
                let cexpr = Box::new(cexpr.vals_to_cells(mutable_vars));
                Self::PrimOp(op, vals, local, cexpr)
            }
            Self::If(val, succ, fail) => Self::If(
                val,
                Box::new(succ.vals_to_cells(mutable_vars)),
                Box::new(fail.vals_to_cells(mutable_vars)),
            ),
            Self::Lambda {
                mut args,
                body,
                val,
                cexp,
                span,
            } => {
                let mut body = Box::new(body.vals_to_cells(mutable_vars));
                let cexp = Box::new(cexp.vals_to_cells(mutable_vars));

                for arg in args.iter_mut() {
                    if mutable_vars.contains(arg) {
                        body = val_to_cell(arg, body);
                    }
                }

                Self::Lambda {
                    args,
                    body,
                    val,
                    cexp,
                    span,
                }
            }
            done => done,
        }
    }
}

fn val_to_cell(arg: &mut Local, body: Box<Cps>) -> Box<Cps> {
    let mut val_arg = Local::gensym();
    val_arg.name = arg.name;
    let cell_arg = std::mem::replace(arg, val_arg);
    Box::new(Cps::PrimOp(
        PrimOp::AllocCell,
        Vec::new(),
        cell_arg,
        Box::new(Cps::PrimOp(
            PrimOp::Set,
            vec![Value::from(cell_arg), Value::from(val_arg)],
            Local::gensym(),
            body,
        )),
    ))
}

impl Cps {
    /// Add PopCallStack instructions to the Cps IR programatically
    fn add_pop_call_stack_insts(self, apply_k: &mut HashSet<Local>) -> Cps {
        match self {
            Cps::PrimOp(PrimOp::PushCallStack, args, val, cexp) => {
                match cexp.as_ref() {
                    Self::App(_, args) => {
                        apply_k.insert(args.last().unwrap().to_local().unwrap());
                    }
                    _ => unreachable!(),
                }
                Cps::PrimOp(PrimOp::PushCallStack, args, val, cexp)
            }
            Cps::PrimOp(op, args, val, cexp) => {
                let cexp = Box::new(cexp.add_pop_call_stack_insts(apply_k));
                Cps::PrimOp(op, args, val, cexp)
            }
            Cps::If(val, succ, fail) => Cps::If(
                val,
                Box::new(succ.add_pop_call_stack_insts(apply_k)),
                Box::new(fail.add_pop_call_stack_insts(apply_k)),
            ),
            Cps::Lambda {
                args,
                body,
                val,
                cexp,
                span,
            } => {
                let mut body = Box::new(body.add_pop_call_stack_insts(apply_k));
                let cexp = Box::new(cexp.add_pop_call_stack_insts(apply_k));
                if apply_k.remove(&val) {
                    body = Box::new(Cps::PrimOp(
                        PrimOp::PopCallStack,
                        Vec::new(),
                        Local::gensym(),
                        body,
                    ));
                }
                Cps::Lambda {
                    args,
                    body,
                    val,
                    cexp,
                    span,
                }
            }
            app => app,
        }
    }
}
