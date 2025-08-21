use super::*;
use crate::{
    ast::*,
    expand::{ExpansionCombiner, SyntaxRule},
    gc::Gc,
    value::Value as RuntimeValue,
};
use either::Either;
use indexmap::IndexSet;

/// There's not too much reason that this is a trait, other than I wanted to
/// see all of the Compile implementations in one place.
pub trait Compile: std::fmt::Debug {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps;

    /// The top level function takes no arguments.
    fn compile_top_level(&self) -> Cps {
        let k = Local::gensym();
        let result = Local::gensym();
        Cps::Lambda {
            args: ClosureArgs::new(vec![result], true, None),
            body: Box::new(Cps::Halt(Value::from(result))),
            val: k,
            cexp: Box::new(self.compile(Box::new(|value| {
                Cps::App(value, vec![Value::from(k)], None)
            }))),
            span: None,
        }
        .reduce()
        .args_to_cells(&mut AHashMap::default())
    }
}

impl Compile for Lambda {
    /// Generates the maximally-correct implementation of a lambda, i.e. a closure that
    /// tail-calls a closure.
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
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
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new(Cps::Lambda {
            args: ClosureArgs::new(args, is_variadic, Some(k3)),
            body: Box::new(body.compile(Box::new(|result| {
                Cps::App(result, vec![Value::from(k3)], None)
            }))),
            val: k4,
            cexp: Box::new(Cps::App(Value::from(k2), vec![Value::from(k4)], None)),
            span: Some(span),
        }),
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
        span: None,
    }
}

impl Compile for Let {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        compile_let(&self.bindings, &self.body, meta_cont)
    }
}

fn compile_let(
    binds: &[(Local, Expression)],
    body: &DefinitionBody,
    mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>,
) -> Cps {
    if let Some(((curr_bind, curr_expr), tail)) = binds.split_first() {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::PrimOp(
                PrimOp::AllocCell,
                Vec::new(),
                *curr_bind,
                Box::new(Cps::Lambda {
                    args: ClosureArgs::new(vec![expr_result], false, None),
                    body: Box::new(Cps::PrimOp(
                        PrimOp::Set,
                        vec![
                            Value::Var(Var::Local(*curr_bind)),
                            Value::Var(Var::Local(expr_result)),
                        ],
                        Local::gensym(),
                        Box::new(compile_let(
                            tail,
                            body,
                            Box::new(move |result| Cps::App(result, vec![Value::from(k2)], None)),
                        )),
                    )),
                    val: k3,
                    cexp: Box::new(curr_expr.compile(Box::new(move |result| {
                        Cps::App(result, vec![Value::from(k3)], None)
                    }))),
                    span: None,
                }),
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    } else {
        body.compile(meta_cont)
    }
}

impl Compile for Expression {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
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
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(self.clone())],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for &[Expression] {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self {
            [] => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Lambda {
                    args: ClosureArgs::new(vec![k2], false, None),
                    body: Box::new(Cps::App(Value::from(k2), Vec::new(), None)),
                    val: k1,
                    cexp: Box::new(meta_cont(Value::from(k1))),
                    span: None,
                }
            }
            [last_expr] => last_expr.compile(Box::new(meta_cont) as Box<dyn FnMut(Value) -> Cps>),
            [head, tail @ ..] => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Lambda {
                    args: ClosureArgs::new(vec![k1], true, None),
                    body: Box::new(tail.compile(meta_cont)),
                    val: k2,
                    cexp: Box::new(head.compile(Box::new(move |result| {
                        Cps::App(result, vec![Value::from(k2)], None)
                    }))),
                    span: None,
                }
            }
        }
    }
}

fn compile_undefined(mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new(Cps::App(
            Value::from(k2),
            vec![Value::from(RuntimeValue::undefined())],
            None,
        )),
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
        span: None,
    }
}

impl Compile for Literal {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(RuntimeValue::from(self.clone()))],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for ExprBody {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        self.exprs.as_slice().compile(meta_cont)
    }
}

impl Compile for Apply {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self.operator {
            Either::Left(ref op) => compile_apply(op, &self.args, self.span.clone(), meta_cont),
            Either::Right(PrimOp::CallWithCurrentContinuation) => {
                compile_call_with_cc(&self.args[0], meta_cont)
            }
            _ => todo!(),
        }
    }
}

fn compile_apply(
    operator: &Expression,
    args: &[Expression],
    span: Span,
    mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let k4 = Local::gensym();
    Cps::Lambda {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new(if let Some(primop) = operator.to_primop() {
            compile_primop(Value::from(k2), primop, Vec::new(), args)
        } else {
            operator.compile(Box::new(move |op_result| Cps::Lambda {
                args: ClosureArgs::new(vec![k3], false, None),
                body: Box::new(compile_apply_args(
                    Value::from(k2),
                    Value::from(k3),
                    Vec::new(),
                    args,
                    span.clone(),
                )),
                val: k4,
                cexp: Box::new(Cps::App(op_result, vec![Value::from(k4)], None)),
                span: None,
            }))
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
    span: Span,
) -> Cps {
    let (arg, tail) = match remaining_args {
        [] => {
            collected_args.push(cont);
            return Cps::App(op, collected_args, Some(span));
        }
        [arg, tail @ ..] => (arg, tail),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new({
            collected_args.push(Value::from(k2));
            compile_apply_args(cont, op, collected_args, tail, span)
        }),
        val: k1,
        cexp: Box::new(arg.compile(Box::new(|result| {
            Cps::App(result, vec![Value::from(k1)], None)
        }))),
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
                Box::new(Cps::App(cont, vec![Value::from(val)], None)),
            );
        }
        [arg, tail @ ..] => (arg, tail),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new({
            collected_args.push(Value::from(k2));
            compile_primop(cont, primop, collected_args, tail)
        }),
        val: k1,
        cexp: Box::new(arg.compile(Box::new(|result| {
            Cps::App(result, vec![Value::from(k1)], None)
        }))),
        span: None,
    }
}

fn compile_call_with_cc(
    thunk: &Expression,
    mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let k4 = Local::gensym();
    let arg = Local::gensym();
    let winders = Local::gensym();
    let escape_procedure = Local::gensym();
    let prepared_cont = Local::gensym();

    Cps::Lambda {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(Cps::PrimOp(
            PrimOp::ExtractWinders,
            Vec::new(),
            winders,
            Box::new(Cps::Lambda {
                args: ClosureArgs::new(vec![arg], true, Some(Local::gensym())),
                body: Box::new(Cps::PrimOp(
                    PrimOp::PrepareContinuation,
                    vec![Value::from(k1), Value::from(winders)],
                    prepared_cont,
                    Box::new(Cps::Forward(Value::from(prepared_cont), Value::from(arg))),
                )),
                val: escape_procedure,
                cexp: Box::new(Cps::Lambda {
                    args: ClosureArgs::new(vec![k3], false, None),
                    body: Box::new(Cps::App(
                        Value::from(k3),
                        vec![Value::from(escape_procedure), Value::from(k1)],
                        None,
                    )),
                    val: k4,
                    cexp: Box::new(thunk.compile(Box::new(|thunk_result| {
                        Cps::App(thunk_result, vec![Value::from(k4)], None)
                    }))),
                    span: None,
                }),
                span: None,
            }),
        )),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
        span: None,
    }
}

impl Compile for If {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Lambda {
            args: ClosureArgs::new(vec![k1], false, None),
            body: Box::new(self.cond.compile(Box::new(|cond_result| {
                let k3 = Local::gensym();
                let cond_arg = Local::gensym();
                Cps::Lambda {
                    args: ClosureArgs::new(vec![cond_arg], false, None),
                    body: Box::new(Cps::If(
                        Value::from(cond_arg),
                        Box::new(self.success.compile(Box::new(|success| {
                            Cps::App(success, vec![Value::from(k1)], None)
                        }))),
                        Box::new(if let Some(ref failure) = self.failure {
                            failure.compile(Box::new(|failure| {
                                Cps::App(failure, vec![Value::from(k1)], None)
                            }))
                        } else {
                            Cps::App(Value::from(k1), Vec::new(), None)
                        }),
                    )),
                    val: k3,
                    cexp: Box::new(Cps::App(cond_result, vec![Value::from(k3)], None)),
                    span: None,
                }
            }))),
            val: k2,
            cexp: Box::new(meta_cont(Value::from(k2))),
            span: None,
        }
    }
}

impl Compile for And {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        compile_and(&self.args, meta_cont)
    }
}

fn compile_and(exprs: &[Expression], mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
    let (expr, tail) = match exprs {
        [] => return meta_cont(Value::from(RuntimeValue::from(true))),
        [expr] => (expr, None),
        [expr, tail @ ..] => (expr, Some(tail)),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(expr.compile(Box::new(|expr_result| {
            let k3 = Local::gensym();
            let cond_arg = Local::gensym();
            Cps::Lambda {
                args: ClosureArgs::new(vec![cond_arg], false, None),
                body: Box::new(Cps::If(
                    Value::from(cond_arg),
                    Box::new(if let Some(tail) = tail {
                        compile_and(
                            tail,
                            Box::new(|expr| Cps::App(expr, vec![Value::from(k1)], None)),
                        )
                    } else {
                        Cps::App(
                            Value::from(k1),
                            vec![Value::from(RuntimeValue::from(true))],
                            None,
                        )
                    }),
                    Box::new(Cps::App(
                        Value::from(k1),
                        vec![Value::from(RuntimeValue::from(false))],
                        None,
                    )),
                )),
                val: k3,
                cexp: Box::new(Cps::App(expr_result, vec![Value::from(k3)], None)),
                span: None,
            }
        }))),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
        span: None,
    }
}

impl Compile for Or {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        compile_or(&self.args, meta_cont)
    }
}

fn compile_or(exprs: &[Expression], mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
    let (expr, tail) = match exprs {
        [] => return meta_cont(Value::from(RuntimeValue::from(false))),
        [expr] => (expr, None),
        [expr, tail @ ..] => (expr, Some(tail)),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(expr.compile(Box::new(|expr_result| {
            let k3 = Local::gensym();
            let cond_arg = Local::gensym();
            Cps::Lambda {
                args: ClosureArgs::new(vec![cond_arg], false, None),
                body: Box::new(Cps::If(
                    Value::from(cond_arg),
                    Box::new(Cps::App(
                        Value::from(k1),
                        vec![Value::from(RuntimeValue::from(true))],
                        None,
                    )),
                    Box::new(if let Some(tail) = tail {
                        compile_or(
                            tail,
                            Box::new(|expr| Cps::App(expr, vec![Value::from(k1)], None)),
                        )
                    } else {
                        Cps::App(
                            Value::from(k1),
                            vec![Value::from(RuntimeValue::from(false))],
                            None,
                        )
                    }),
                )),
                val: k3,
                cexp: Box::new(Cps::App(expr_result, vec![Value::from(k3)], None)),
                span: None,
            }
        }))),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
        span: None,
    }
}

impl Compile for Definition {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
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
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
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
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self {
            Some(Either::Left(def)) => def.compile(meta_cont),
            Some(Either::Right(exprs)) => exprs.compile(meta_cont),
            _ => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Lambda {
                    args: ClosureArgs::new(vec![k1], false, None),
                    body: Box::new(Cps::App(Value::from(k1), Vec::new(), None)),
                    val: k2,
                    cexp: Box::new(meta_cont(Value::from(k2))),
                    span: None,
                }
            }
        }
    }
}

impl Compile for Set {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        // let k4 = Local::gensym();
        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Lambda {
                args: ClosureArgs::new(vec![expr_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var.clone()),
                        Value::Var(Var::Local(expr_result)),
                    ],
                    Local::gensym(),
                    Box::new(Cps::App(Value::from(k2), Vec::new(), None)),
                )),
                val: k3,
                cexp: Box::new(self.val.compile(Box::new(move |result| {
                    Cps::App(result, vec![Value::from(k3)], None)
                }))),
                span: None,
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for DefineVar {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        // let k4 = Local::gensym();
        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Lambda {
                args: ClosureArgs::new(vec![expr_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var.clone()),
                        Value::Var(Var::Local(expr_result)),
                    ],
                    Local::gensym(),
                    Box::new(self.next.compile(Box::new(move |result| {
                        Cps::App(result, vec![Value::from(k2)], None)
                    }))),
                )),
                val: k3,
                cexp: Box::new(self.val.compile(Box::new(move |result| {
                    Cps::App(result, vec![Value::from(k3)], None)
                }))),
                span: None,
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for DefineFunc {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let lambda_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Lambda {
                args: ClosureArgs::new(vec![lambda_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var.clone()),
                        Value::Var(Var::Local(lambda_result)),
                    ],
                    Local::gensym(),
                    Box::new(self.next.compile(Box::new(move |result| {
                        Cps::App(result, vec![Value::from(k2)], None)
                    }))),
                )),
                val: k3,
                cexp: Box::new(compile_lambda(
                    self.args.iter().cloned().collect(),
                    self.args.is_variadic(),
                    &self.body,
                    self.span.clone(),
                    self.var.symbol(),
                    |lambda_result| Cps::App(lambda_result, vec![Value::from(k3)], None),
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
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(self.val.clone())],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for SyntaxQuote {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let expanded = Local::gensym();

        let mut expansions_seen = IndexSet::new();
        let mut uses = AHashMap::new();

        for (ident, expansion) in self.expansions.iter() {
            let (idx, _) = expansions_seen.insert_full(expansion);
            uses.insert(ident.clone(), idx);
        }

        let mut args = vec![
            Value::from(RuntimeValue::from(Gc::new(Gc::into_any(Gc::new(
                self.template.clone(),
            ))))),
            Value::from(RuntimeValue::from(Gc::new(Gc::into_any(Gc::new(
                ExpansionCombiner { uses },
            ))))),
        ];

        for expansion in expansions_seen.iter() {
            args.push(Value::from(**expansion));
        }

        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::PrimOp(
                PrimOp::ExpandTemplate,
                args,
                expanded,
                Box::new(Cps::App(Value::from(k2), vec![Value::from(expanded)], None)),
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Compile for SyntaxCase {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(self.arg.compile(Box::new(|expr_result| {
                let k3 = Local::gensym();
                let arg = Local::gensym();
                Cps::Lambda {
                    args: ClosureArgs::new(vec![arg], false, None),
                    body: Box::new(compile_syntax_rules(
                        &self.rules,
                        arg,
                        Box::new(|expanded| Cps::App(expanded, vec![Value::from(k2)], None)),
                    )),
                    val: k3,
                    cexp: Box::new(Cps::App(expr_result, vec![Value::from(k3)], None)),
                    span: None,
                }
            }))),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}
fn compile_syntax_rules(
    rules: &[SyntaxRule],
    arg: Local,
    mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Lambda {
        args: ClosureArgs::new(vec![k2], false, None),
        body: match rules {
            [] => Box::new(Cps::PrimOp(
                PrimOp::ErrorNoPatternsMatch,
                Vec::new(),
                Local::gensym(),
                Box::new(Cps::App(Value::from(k2), Vec::new(), None)),
            )),
            [rule, tail @ ..] => {
                let pattern = Gc::new(Gc::into_any(Gc::new(rule.pattern.clone())));
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
                        let inner = Box::new(Cps::If(
                            Value::from(match_result),
                            Box::new(rule.output_expression.compile(Box::new(|matches| {
                                Cps::App(matches, vec![Value::from(k2)], None)
                            }))),
                            Box::new(compile_syntax_rules(
                                tail,
                                arg,
                                Box::new(|next_pattern| {
                                    Cps::App(next_pattern, vec![Value::from(k2)], None)
                                }),
                            )),
                        ));
                        if let Some(fender) = &rule.fender {
                            let k3 = Local::gensym();
                            Box::new(Cps::Lambda {
                                args: ClosureArgs::new(vec![match_result], false, None),
                                body: inner,
                                val: k3,
                                cexp: Box::new(Cps::If(
                                    Value::from(rule.binds),
                                    // Check the fender
                                    Box::new(fender.compile(Box::new(|fender| {
                                        Cps::App(fender, vec![Value::from(k3)], None)
                                    }))),
                                    Box::new(Cps::App(
                                        Value::from(k3),
                                        vec![Value::from(RuntimeValue::from(false))],
                                        None,
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
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();

        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(RuntimeValue::from(self.vals.clone()))],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}
impl Compile for Vec<u8> {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();

        Cps::Lambda {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(RuntimeValue::from(self.clone()))],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            span: None,
        }
    }
}

impl Cps {
    /// Convert arguments for closures into cells if they are written to or escape.
    fn args_to_cells(self, needs_cell_cache: &mut AHashMap<Local, AHashSet<Local>>) -> Self {
        match self {
            Self::PrimOp(PrimOp::AllocCell, vals, local, cexpr) => Self::PrimOp(
                PrimOp::AllocCell,
                vals,
                local,
                Box::new(cexpr.args_to_cells(needs_cell_cache)),
            ),
            Self::PrimOp(op, vals, mut local, cexpr) => {
                let cexpr = Box::new(cexpr.args_to_cells(needs_cell_cache));
                let escaping_args =
                    cexpr.need_cells(&[local].into_iter().collect(), needs_cell_cache);
                let cexpr = if escaping_args.contains(&local) {
                    arg_to_cell(&mut local, cexpr)
                } else {
                    cexpr
                };
                Self::PrimOp(op, vals, local, cexpr)
            }
            Self::If(val, succ, fail) => Self::If(
                val,
                Box::new(succ.args_to_cells(needs_cell_cache)),
                Box::new(fail.args_to_cells(needs_cell_cache)),
            ),
            Self::Lambda {
                mut args,
                body,
                val,
                cexp,
                span: loc,
            } => {
                let local_args: AHashSet<_> = args.iter().copied().collect();
                let escaping_args = body.need_cells(&local_args, needs_cell_cache);

                let mut body = Box::new(body.args_to_cells(needs_cell_cache));
                let cexp = Box::new(cexp.args_to_cells(needs_cell_cache));

                for arg in args.iter_mut() {
                    if escaping_args.contains(arg) {
                        body = arg_to_cell(arg, body);
                    }
                }

                Self::Lambda {
                    args,
                    body,
                    val,
                    cexp,
                    span: loc,
                }
            }
            done => done,
        }
    }
}

fn arg_to_cell(arg: &mut Local, body: Box<Cps>) -> Box<Cps> {
    let val_arg = Local::gensym();
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
