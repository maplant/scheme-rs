use std::{collections::VecDeque, iter::once, mem::take};

use super::*;
use crate::{ast::*, gc::Gc, syntax::Identifier, value::Value as SchemeValue};
use either::Either;

/// There's not too much reason that this is a trait, other than I wanted to
/// see all of the Compile implementations in one place.
pub trait Compile: Sized {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps;

    /// The top level function takes no arguments.
    fn compile_top_level(self) -> Cps {
        let k = Local::gensym();
        let result = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![result], true, None),
            body: Box::new(Cps::Halt(Value::from(result))),
            val: k,
            cexp: Box::new(self.compile(&mut |value| {
                Cps::App(value, vec![Value::from(k)], None)
            })),
            debug: None,
        }
        .reduce()
    }
}

impl Compile for Lambda {
    /// Generates the maximally-correct implementation of a lambda, i.e. a closure that
    /// tail-calls a closure.
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        compile_lambda(
            self.args.is_variadic(),
            self.args.into(),
            self.body,
            self.debug_info_id,
            meta_cont,
        )
    }
}

fn compile_lambda(
    is_variadic: bool,
    args: Vec<Local>,
    body: DefinitionBody,
    debug_info_id: FunctionDebugInfoId,
    meta_cont: &mut dyn FnMut(Value) -> Cps,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let k4 = Local::gensym();

    Cps::Closure {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new(Cps::Closure {
            args: ClosureArgs::new(args, is_variadic, Some(k3)),
            body: Box::new(body.compile(&mut |result| {
                Cps::App(result, vec![Value::from(k3)], None)
            })),
            val: k4,
            cexp: Box::new(Cps::App(Value::from(k2), vec![Value::from(k4)], None)),
            debug: Some(debug_info_id),
        }),
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
        debug: None,
    }
}

impl Compile for Let {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        compile_let(self.bindings, self.body, meta_cont)
    }
}

fn compile_let(
    mut binds: VecDeque<(Local, Expression)>,
    body: DefinitionBody,
    meta_cont: &mut (dyn FnMut(Value) -> Cps + '_),
) -> Cps {
    if let Some((curr_bind, curr_expr)) = binds.pop_front() {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::AllocCell(
                curr_bind,
                Box::new(Cps::Closure {
                    args: ClosureArgs::new(vec![expr_result], false, None),
                    body: Box::new(Cps::PrimOp(
                        PrimOp::Set,
                        vec![
                            Value::Var(Var::Local(curr_bind)),
                            Value::Var(Var::Local(expr_result)),
                        ],
                        Local::gensym(),
                        Box::new(compile_let(
                            binds,
                            body,
                            &mut move |result| Cps::App(result, vec![Value::from(k2)], None),
                        )),
                    )),
                    val: k3,
                    cexp: Box::new(curr_expr.compile(&mut move |result| {
                        Cps::App(result, vec![Value::from(k3)], None)
                    })),
                    debug: None,
                }),
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            debug: None,
        }
    } else {
        body.compile(meta_cont)
    }
}

impl Compile for Expression {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
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
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(self)],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            debug: None,
        }
    }
}

impl Compile for &mut [Expression] {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        match self {
            [] => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![k2], false, None),
                    body: Box::new(Cps::App(Value::from(k2), vec![], None)),
                    val: k1,
                    cexp: Box::new(meta_cont(Value::from(k1))),
                    debug: None,
                }
            }
            [last_expr] => take(last_expr).compile(meta_cont),
            [head, tail @ ..] => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![k1], true, None),
                    body: Box::new(tail.compile(meta_cont)),
                    val: k2,
                    cexp: Box::new(take(head).compile(&mut move |result| {
                        Cps::App(result, vec![Value::from(k2)], None)
                    })),
                    debug: None,
                }
            }
        }
    }
}

/// Create a constant value from any Scheme value
/// TODO: The way we do this is obviously quite bad, we're boxing every single
/// constant that comes our way and making them global variables. This is a
/// hack, but one that _is technically correct_.
fn constant(constant: SchemeValue) -> Value {
    Value::from(Global::new(
        Identifier::new(format!("{constant:?}")),
        Gc::new(constant),
    ))
}

fn compile_undefined(meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new(Cps::App(
            Value::from(k2),
            vec![constant(SchemeValue::Undefined)],
            None,
        )),
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
        debug: None,
    }
}

impl Compile for Literal {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![constant(SchemeValue::from_literal(self))],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            debug: None,
        }
    }
}

impl Compile for ExprBody {
    fn compile(mut self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        self.exprs.as_mut_slice().compile(meta_cont)
    }
}

impl Compile for Apply {
    fn compile(mut self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        match self.operator {
            Either::Left(op) => compile_apply(*op, self.args, self.call_site_id, meta_cont),
            Either::Right(PrimOp::CallWithCurrentContinuation) => {
                compile_call_with_cc(self.args.pop_front().unwrap(), meta_cont)
            }
            _ => todo!(),
        }
    }
}

fn compile_apply(
    operator: Expression,
    mut args: VecDeque<Expression>,
    call_site_id: CallSiteId,
    meta_cont: &mut (dyn FnMut(Value) -> Cps + '_),
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let k4 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new(if let Some(primop) = operator.to_primop() {
            compile_primop(Value::from(k2), primop, Vec::new(), args)
        } else {
            operator.compile(&mut move |op_result| Cps::Closure {
                args: ClosureArgs::new(vec![k3], false, None),
                body: Box::new(compile_apply_args(
                    Value::from(k2),
                    Value::from(k3),
                    Vec::new(),
                    take(&mut args),
                    call_site_id,
                )),
                val: k4,
                cexp: Box::new(Cps::App(op_result, vec![Value::from(k4)], None)),
                debug: None,
            })
        }),
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
        debug: None,
    }
}

fn compile_apply_args(
    cont: Value,
    op: Value,
    mut collected_args: Vec<Value>,
    mut remaining_args: VecDeque<Expression>,
    call_site_id: CallSiteId,
) -> Cps {
    let Some(arg) = remaining_args.pop_front() else {
        collected_args.push(cont);
        return Cps::App(op, collected_args, Some(call_site_id))
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new({
            collected_args.push(Value::from(k2));
            compile_apply_args(cont, op, collected_args, remaining_args, call_site_id)
        }),
        val: k1,
        cexp: Box::new(arg.compile(&mut |result| {
            Cps::App(result, vec![Value::from(k1)], None)
        })),
        debug: None,
    }
}

fn compile_primop(
    cont: Value,
    primop: PrimOp,
    mut collected_args: Vec<Value>,
    mut remaining_args: VecDeque<Expression>,
) -> Cps {
    let Some(arg) = remaining_args.pop_front() else {
        let val = Local::gensym();
        return Cps::PrimOp(
            primop,
            collected_args,
            val,
            Box::new(Cps::App(cont, vec![Value::from(val)], None)),
        )
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new({
            collected_args.push(Value::from(k2));
            compile_primop(cont, primop, collected_args, remaining_args)
        }),
        val: k1,
        cexp: Box::new(arg.compile(&mut |result| {
            Cps::App(result, vec![Value::from(k1)], None)
        })),
        debug: None,
    }
}

fn compile_call_with_cc(thunk: Expression, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let k4 = Local::gensym();
    let arg = Local::gensym();
    let winders = Local::gensym();
    let escape_procedure = Local::gensym();
    let prepared_cont = Local::gensym();

    Cps::Closure {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(Cps::PrimOp(
            PrimOp::ExtractWinders,
            Vec::new(),
            winders,
            Box::new(Cps::Closure {
                args: ClosureArgs::new(vec![arg], true, Some(Local::gensym())),
                body: Box::new(Cps::PrimOp(
                    PrimOp::PrepareContinuation,
                    vec![Value::from(k1), Value::from(winders)],
                    prepared_cont,
                    Box::new(Cps::Forward(Value::from(prepared_cont), Value::from(arg))),
                )),
                val: escape_procedure,
                cexp: Box::new(Cps::Closure {
                    args: ClosureArgs::new(vec![k3], false, None),
                    body: Box::new(Cps::App(
                        Value::from(k3),
                        vec![Value::from(escape_procedure), Value::from(k1)],
                        None,
                    )),
                    val: k4,
                    cexp: Box::new(thunk.compile(&mut |thunk_result| {
                        Cps::App(thunk_result, vec![Value::from(k4)], None)
                    })),
                    debug: None,
                }),
                debug: None,
            }),
        )),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
        debug: None,
    }
}

impl Compile for If {
    fn compile(mut self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();

        Cps::Closure {
            args: ClosureArgs::new(vec![k1], false, None),
            body: Box::new(self.cond.compile(&mut move |cond_result| {
                let k3 = Local::gensym();
                let cond_arg = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![cond_arg], false, None),
                    body: Box::new(Cps::If(
                        Value::from(cond_arg),
                        Box::new(
                            take::<Expression>(&mut self.success).compile(&mut |success| {
                                Cps::App(success, vec![Value::from(k1)], None)
                            }),
                        ),
                        Box::new(if let Some(failure) = &mut self.failure {
                            take::<Expression>(failure).compile(&mut |failure| {
                                Cps::App(failure, vec![Value::from(k1)], None)
                            })
                        } else {
                            Cps::App(Value::from(k1), Vec::new(), None)
                        }),
                    )),
                    val: k3,
                    cexp: Box::new(Cps::App(cond_result, vec![Value::from(k3)], None)),
                    debug: None,
                }
            })),
            val: k2,
            cexp: Box::new(meta_cont(Value::from(k2))),
            debug: None,
        }
    }
}

impl Compile for And {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        compile_and(self.args, meta_cont)
    }
}

fn compile_and(mut exprs: VecDeque<Expression>, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
    let Some(expr) = exprs.pop_front() else {
        return meta_cont(constant(SchemeValue::from(true)))
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(expr.compile(&mut |expr_result| {
            let k3 = Local::gensym();
            let cond_arg = Local::gensym();
            Cps::Closure {
                args: ClosureArgs::new(vec![cond_arg], false, None),
                body: Box::new(Cps::If(
                    Value::from(cond_arg),
                    Box::new(if exprs.is_empty() {
                        Cps::App(
                            Value::from(k1),
                            vec![constant(SchemeValue::from(true))],
                            None,
                        )
                    } else {
                        compile_and(take(&mut exprs), &mut |expr| Cps::App(expr, vec![Value::from(k1)], None))
                    }),
                    Box::new(Cps::App(
                        Value::from(k1),
                        vec![constant(SchemeValue::from(false))],
                        None,
                    )),
                )),
                val: k3,
                cexp: Box::new(Cps::App(expr_result, vec![Value::from(k3)], None)),
                debug: None,
            }
        })),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
        debug: None,
    }
}

impl Compile for Or {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        compile_or(self.args, meta_cont)
    }
}

fn compile_or(mut exprs: VecDeque<Expression>, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
    let Some(expr) = exprs.pop_front() else {
        return meta_cont(constant(SchemeValue::from(false)))
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(expr.compile(&mut |expr_result| {
            let k3 = Local::gensym();
            let cond_arg = Local::gensym();
            Cps::Closure {
                args: ClosureArgs::new(vec![cond_arg], false, None),
                body: Box::new(Cps::If(
                    Value::from(cond_arg),
                    Box::new(Cps::App(
                        Value::from(k1),
                        vec![constant(SchemeValue::from(true))],
                        None,
                    )),
                    Box::new(if exprs.is_empty() {
                        Cps::App(
                            Value::from(k1),
                            vec![constant(SchemeValue::from(false))],
                            None,
                        )
                    } else {
                        compile_or(take(&mut exprs), &mut |expr| Cps::App(expr, vec![Value::from(k1)], None))
                    }),
                )),
                val: k3,
                cexp: Box::new(Cps::App(expr_result, vec![Value::from(k3)], None)),
                debug: None,
            }
        })),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
        debug: None,
    }
}

impl Compile for Definition {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        match self {
            Self::DefineVar(var) => var.compile(meta_cont),
            Self::DefineFunc(func) => func.compile(meta_cont),
            _ => todo!(),
        }
    }
}

impl Definition {
    fn alloc_cells(&self, wrap: Cps) -> Cps {
        match self {
            Self::DefineVar(ref def) => def.alloc_cells(wrap),
            Self::DefineFunc(ref func) => func.alloc_cells(wrap),
            _ => todo!(),
        }
    }
}

impl DefineVar {
    fn alloc_cells(&self, wrap: Cps) -> Cps {
        let cps = match self.var {
            Var::Global(_) => wrap,
            Var::Local(local) => Cps::AllocCell(local, Box::new(wrap)),
        };
        next_or_wrap(&self.next, cps)
    }
}

impl DefineFunc {
    fn alloc_cells(&self, wrap: Cps) -> Cps {
        let cps = match self.var {
            Var::Global(_) => wrap,
            Var::Local(local) => Cps::AllocCell(local, Box::new(wrap)),
        };
        next_or_wrap(&self.next, cps)
    }
}

impl Compile for DefinitionBody {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        match self.first {
            Either::Left(def) => def.clone().alloc_cells(def.compile(meta_cont)),
            Either::Right(exprs) => exprs.compile(meta_cont),
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
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        match self {
            Some(Either::Left(def)) => def.compile(meta_cont),
            Some(Either::Right(exprs)) => exprs.compile(meta_cont),
            _ => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![k1], false, None),
                    body: Box::new(Cps::App(Value::from(k1), Vec::new(), None)),
                    val: k2,
                    cexp: Box::new(meta_cont(Value::from(k2))),
                    debug: None,
                }
            }
        }
    }
}

impl Compile for Set {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        // let k4 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Closure {
                args: ClosureArgs::new(vec![expr_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var),
                        Value::Var(Var::Local(expr_result)),
                    ],
                    Local::gensym(),
                    Box::new(Cps::App(Value::from(k2), Vec::new(), None)),
                    /*
                        // TODO: We should not return the value of the variable.
                        Box::new(self.var.compile(Box::new(move |result| {
                            Cps::App(result, vec![Value::from(k2)], None)
                    }))),
                        */
                )),
                val: k3,
                cexp: Box::new(self.val.compile(
                    &mut move |result| {
                        Cps::App(result, vec![Value::from(k3)], None) // Value::from(k3), vec![result, Value::from(k2)])
                    },
                )),
                debug: None,
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            debug: None,
        }
    }
}

impl Compile for DefineVar {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        // let k4 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Closure {
                args: ClosureArgs::new(vec![expr_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var),
                        Value::Var(Var::Local(expr_result)),
                    ],
                    Local::gensym(),
                    Box::new(self.next.compile(&mut move |result| {
                        Cps::App(result, vec![Value::from(k2)], None)
                    })),
                )),
                val: k3,
                cexp: Box::new(self.val.compile(&mut move |result| {
                    Cps::App(result, vec![Value::from(k3)], None)
                })),
                debug: None,
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            debug: None,
        }
    }
}

impl Compile for DefineFunc {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let lambda_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Closure {
                args: ClosureArgs::new(vec![lambda_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var),
                        Value::Var(Var::Local(lambda_result)),
                    ],
                    Local::gensym(),
                    Box::new(self.next.compile(&mut move |result| {
                        Cps::App(result, vec![Value::from(k2)], None)
                    })),
                )),
                val: k3,
                cexp: Box::new(compile_lambda(
                    self.args.is_variadic(),
                    self.args.into(),
                    *self.body,
                    self.debug_info_id,
                    &mut |lambda_result| Cps::App(lambda_result, vec![Value::from(k3)], None),
                )),
                debug: None,
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            debug: None,
        }
    }
}

impl Compile for Quote {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![constant(self.val)],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            debug: None,
        }
    }
}

impl Compile for SyntaxQuote {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![constant(SchemeValue::Syntax(self.syn))],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            debug: None,
        }
    }
}

impl Compile for SyntaxCase {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k1], false, None),
            body: Box::new(self.arg.compile(&mut |arg_result| {
                let k3 = Local::gensym();
                let to_expand = Local::gensym();
                let call_transformer = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![to_expand], false, None),
                    body: Box::new(Cps::PrimOp(
                        PrimOp::GetCallTransformerFn,
                        vec![],
                        call_transformer,
                        Box::new(Cps::App(
                            Value::from(call_transformer),
                            vec![
                                constant(SchemeValue::CapturedEnv(self.captured_env.clone())),
                                constant(SchemeValue::Transformer(self.transformer.clone())),
                                Value::from(to_expand),
                            ]
                            .into_iter()
                            .chain(self.captured_env.captured.iter().copied().map(Value::from))
                            .chain(once(Value::from(k1)))
                            .collect(),
                            None,
                        )),
                    )),
                    val: k3,
                    cexp: Box::new(Cps::App(arg_result, vec![Value::from(k3)], None)),
                    debug: None,
                }
            })),
            val: k2,
            cexp: Box::new(meta_cont(Value::from(k2))),
            debug: None,
        }
    }
}

impl Compile for Vector {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();

        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![constant(SchemeValue::Vector(self.vals))],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            debug: None,
        }
    }
}
impl Compile for Vec<u8> {
    fn compile(self, meta_cont: &mut (dyn FnMut(Value) -> Cps + '_)) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();

        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![constant(SchemeValue::ByteVector(self))],
                None,
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
            debug: None,
        }
    }
}
