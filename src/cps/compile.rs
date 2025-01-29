use super::*;
use crate::{ast::*, gc::Gc, value::Value as SchemeValue};
use either::Either;

/// There's not too much reason that this is a trait, other than I wanted to
/// see all of the Compile implementations in one place.
pub trait Compile {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps;

    /// The top level function takes no arguments.
    fn compile_top_level(&self) -> TopLevelExpr {
        let k = Local::gensym();
        let result = Local::gensym();
        TopLevelExpr {
            body: Cps::Closure {
                args: ClosureArgs::new(vec![result], true, None),
                body: Box::new(Cps::ReturnValues(Value::from(result))),
                val: k,
                cexp: Box::new(
                    self.compile(Box::new(|value| Cps::App(value, vec![Value::from(k)]))),
                ),
            },
        }
    }
}

#[derive(Debug)]
pub struct TopLevelExpr {
    pub body: Cps,
}

impl Compile for Lambda {
    /// Generates the maximally-correct implementation of a lambda, i.e. a closure that
    /// tail-calls a closure.
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        compile_lambda(
            self.args.iter().cloned().collect(),
            self.args.is_variadic(),
            &self.body,
            meta_cont,
        )
    }
}

fn compile_lambda(
    args: Vec<Local>,
    is_variadic: bool,
    body: &DefinitionBody,
    mut meta_cont: impl FnMut(Value) -> Cps,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let k4 = Local::gensym();

    Cps::Closure {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new(Cps::Closure {
            args: ClosureArgs::new(args, is_variadic, Some(k3)),
            body: Box::new(
                body.compile(Box::new(|result| Cps::App(result, vec![Value::from(k3)]))),
            ),
            val: k4,
            cexp: Box::new(Cps::App(Value::from(k2), vec![Value::from(k4)])),
        }),
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
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
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::AllocCell(
                *curr_bind,
                Box::new(Cps::Closure {
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
                            Box::new(move |result| Cps::App(result, vec![Value::from(k2)])),
                        )),
                    )),
                    val: k3,
                    cexp: Box::new(curr_expr.compile(Box::new(move |result| {
                        Cps::App(Value::from(result), vec![Value::from(k3)])
                    }))),
                }),
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
        }
    } else {
        body.compile(meta_cont)
    }
}

impl Compile for AstNode {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self {
            Self::Definition(def) => def.compile(meta_cont),
            Self::Expression(exp) => exp.compile(meta_cont),
        }
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
            x => panic!("not yet implemented: {x:#?}"),
        }
    }
}

impl Compile for Var {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(Value::from(k2), vec![Value::from(self.clone())])),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
        }
    }
}

impl Compile for &[Expression] {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self {
            [] => panic!("Empty body"),
            [last_expr] => last_expr.compile(Box::new(meta_cont) as Box<dyn FnMut(Value) -> Cps>),
            [head, tail @ ..] => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![k1], false, None),
                    body: Box::new(tail.compile(meta_cont)),
                    val: k2,
                    cexp: Box::new(head.compile(Box::new(move |result| {
                        Cps::App(result, vec![Value::from(k2)])
                    }))),
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
    Value::from(Global::new(format!("{constant:?}"), Gc::new(constant)))
}

impl Compile for Literal {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![constant(SchemeValue::from_literal(self))],
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
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
            Either::Left(ref op) => compile_apply(op, &self.args, meta_cont),
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
    mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let k4 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new(operator.compile(Box::new(move |op_result| Cps::Closure {
            args: ClosureArgs::new(vec![k3], false, None),
            body: Box::new(compile_apply_args(
                Value::from(k2),
                Value::from(k3),
                Vec::new(),
                args,
            )),
            val: k4,
            cexp: Box::new(Cps::App(Value::from(op_result), vec![Value::from(k4)])),
        }))),
        val: k1,
        cexp: Box::new(meta_cont(Value::from(k1))),
    }
}

fn compile_apply_args(
    cont: Value,
    op: Value,
    mut collected_args: Vec<Value>,
    remaining_args: &[Expression],
) -> Cps {
    let (arg, tail) = match remaining_args {
        [] => {
            collected_args.push(cont);
            return Cps::App(op, collected_args);
        }
        [arg, tail @ ..] => (arg, tail),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k2], false, None),
        body: Box::new({
            collected_args.push(Value::from(k2));
            compile_apply_args(cont, op, collected_args, tail)
        }),
        val: k1,
        cexp: Box::new(arg.compile(Box::new(|result| Cps::App(result, vec![Value::from(k1)])))),
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
    let escape_procedure = Local::gensym();
    let arg = Local::gensym();

    Cps::Closure {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(Cps::Closure {
            args: ClosureArgs::new(vec![arg], false, Some(Local::gensym())),
            body: Box::new(Cps::App(Value::from(k1), vec![Value::from(arg)])),
            val: escape_procedure,
            cexp: Box::new(Cps::Closure {
                args: ClosureArgs::new(vec![k3], false, None),
                body: Box::new(Cps::App(
                    Value::from(k3),
                    vec![Value::from(escape_procedure), Value::from(k1)],
                )),
                val: k4,
                cexp: Box::new(thunk.compile(Box::new(|thunk_result| {
                    Cps::App(thunk_result, vec![Value::from(k4)])
                }))),
            }),
        }),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
    }
}

impl Compile for If {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k1], false, None),
            body: Box::new(self.cond.compile(Box::new(|cond_result| {
                let k3 = Local::gensym();
                let cond_arg = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![cond_arg], false, None),
                    body: Box::new(Cps::If(
                        Value::from(cond_arg),
                        Box::new(
                            self.success.compile(Box::new(|success| {
                                Cps::App(success, vec![Value::from(k1)])
                            })),
                        ),
                        Box::new(if let Some(ref failure) = self.failure {
                            failure.compile(Box::new(|failure| {
                                Cps::App(failure, vec![Value::from(k1)])
                            }))
                        } else {
                            Cps::App(Value::from(k1), Vec::new())
                        }),
                    )),
                    val: k3,
                    cexp: Box::new(Cps::App(cond_result, vec![Value::from(k3)])),
                }
            }))),
            val: k2,
            cexp: Box::new(meta_cont(Value::from(k2))),
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
        [] => return meta_cont(Value::from(true)),
        [expr] => (expr, None),
        [expr, tail @ ..] => (expr, Some(tail)),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(expr.compile(Box::new(|expr_result| {
            let k3 = Local::gensym();
            let cond_arg = Local::gensym();
            Cps::Closure {
                args: ClosureArgs::new(vec![cond_arg], false, None),
                body: Box::new(Cps::If(
                    Value::from(cond_arg),
                    Box::new(if let Some(tail) = tail {
                        compile_and(tail, Box::new(|expr| Cps::App(expr, vec![Value::from(k1)])))
                    } else {
                        Cps::App(Value::from(k1), vec![constant(SchemeValue::from(true))])
                    }),
                    Box::new(Cps::App(
                        Value::from(k1),
                        vec![constant(SchemeValue::from(false))],
                    )),
                )),
                val: k3,
                cexp: Box::new(Cps::App(expr_result, vec![Value::from(k3)])),
            }
        }))),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
    }
}

impl Compile for Or {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        compile_or(&self.args, meta_cont)
    }
}

fn compile_or(exprs: &[Expression], mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
    let (expr, tail) = match exprs {
        [] => return meta_cont(Value::from(false)),
        [expr] => (expr, None),
        [expr, tail @ ..] => (expr, Some(tail)),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(expr.compile(Box::new(|expr_result| {
            let k3 = Local::gensym();
            let cond_arg = Local::gensym();
            Cps::Closure {
                args: ClosureArgs::new(vec![cond_arg], false, None),
                body: Box::new(Cps::If(
                    Value::from(cond_arg),
                    Box::new(Cps::App(
                        Value::from(k1),
                        vec![constant(SchemeValue::from(true))],
                    )),
                    Box::new(if let Some(tail) = tail {
                        compile_or(tail, Box::new(|expr| Cps::App(expr, vec![Value::from(k1)])))
                    } else {
                        Cps::App(Value::from(k1), vec![constant(SchemeValue::from(false))])
                    }),
                )),
                val: k3,
                cexp: Box::new(Cps::App(expr_result, vec![Value::from(k3)])),
            }
        }))),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
    }
}

impl Compile for Definition {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self {
            Self::DefineVar(var) => var.compile(meta_cont),
            Self::DefineFunc(func) => func.compile(meta_cont),
            _ => todo!(),
        }
    }
}

impl Compile for DefinitionBody {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self.first {
            Either::Left(ref def) => def.compile(meta_cont),
            Either::Right(ref exprs) => exprs.compile(meta_cont),
        }
    }
}

impl Compile for Option<Either<Box<Definition>, ExprBody>> {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self {
            Some(Either::Left(ref def)) => def.compile(meta_cont),
            Some(Either::Right(ref exprs)) => exprs.compile(meta_cont),
            _ => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![k1], false, None),
                    body: Box::new(Cps::App(Value::from(k1), Vec::new())),
                    val: k2,
                    cexp: Box::new(meta_cont(Value::from(k2))),
                }
            }
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
        let set_cont = Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Closure {
                args: ClosureArgs::new(vec![expr_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var.clone()),
                        Value::Var(Var::Local(expr_result)),
                    ],
                    Local::gensym(),
                    Box::new(self.next.compile(Box::new(move |result| {
                        Cps::App(result, vec![Value::from(k2)])
                    }))),
                )),
                val: k3,
                cexp: Box::new(self.val.compile(Box::new(move |result| {
                    Cps::App(result, vec![Value::from(k3)]) // Value::from(k3), vec![result, Value::from(k2)])
                }))),
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
        };

        // If we need to allocate a cell because the value is not a global,
        // do so.
        match self.var {
            Var::Global(_) => set_cont,
            Var::Local(local) => Cps::AllocCell(local, Box::new(set_cont)),
        }
    }
}

impl Compile for DefineFunc {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let lambda_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        let set_cont = Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Closure {
                args: ClosureArgs::new(vec![lambda_result], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::Set,
                    vec![
                        Value::from(self.var.clone()),
                        Value::Var(Var::Local(lambda_result)),
                    ],
                    Local::gensym(),
                    Box::new(self.next.compile(Box::new(move |result| {
                        Cps::App(result, vec![Value::from(k2)])
                    }))),
                )),
                val: k3,
                cexp: Box::new(compile_lambda(
                    self.args.iter().cloned().collect(),
                    self.args.is_variadic(),
                    &self.body,
                    |lambda_result| Cps::App(lambda_result, vec![Value::from(k3)]),
                )),
            }),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
        };
        match self.var {
            Var::Global(_) => set_cont,
            Var::Local(local) => Cps::AllocCell(local, Box::new(set_cont)),
        }
    }
}
