use either::Either;

use super::*;
use crate::ast::*;

/// There's not too much reason that this is a trait, other than I wanted to
/// see all of the Compile implementations in one place.
pub trait Compile {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps;

    fn compile_top_level(&self) -> Cps {
        let k = Local::gensym();
        let arg = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![arg], true, None),
            body: Box::new(Cps::ReturnValues(arg)),
            val: k,
            cexp: Box::new(self.compile(Box::new(|expr| Cps::App(expr, vec![Value::from(k)])))),
        }
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
            meta_cont,
        )
    }
}

fn compile_lambda(
    args: Vec<Local>,
    is_variadic: bool,
    body: &Body,
    mut meta_cont: impl FnMut(Value) -> Cps,
) -> Cps {
    let closure_arg = Local::gensym();
    let k = Local::gensym();
    let args = ClosureArgs::new(args, is_variadic, Some(closure_arg));
    Cps::Closure {
        args,
        body: Box::new(body.compile(Box::new(|result| {
            Cps::App(result, vec![Value::from(closure_arg)])
        }))),
        val: k,
        cexp: Box::new(meta_cont(Value::Var(Var::Local(k)))),
    }
}

impl Compile for Let {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        compile_let(&self.bindings, &self.body, meta_cont)
    }
}

fn compile_let(
    binds: &[(Local, Expression)],
    body: &Body,
    mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>,
) -> Cps {
    if let Some(((curr_bind, curr_expr), tail)) = binds.split_first() {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        let k4 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::AllocCell(
                *curr_bind,
                Box::new(Cps::Closure {
                    args: ClosureArgs::new(vec![expr_result], false, Some(k4)),
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
                            Box::new(move |result| Cps::App(Value::from(k4), vec![result])),
                        )),
                    )),
                    val: k3,
                    cexp: Box::new(curr_expr.compile(Box::new(move |result| {
                        Cps::App(Value::from(k3), vec![result, Value::from(k2)])
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
        /*
        let k1 = Local::genxsym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(Value::from(k2), vec![Value::from(self.clone())])),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
        }
         */
        meta_cont(Value::from(self.clone()))
    }
}

impl Compile for &[AstNode] {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self {
            [] => panic!("Empty body"),
            [last_expr] => last_expr.compile(Box::new(meta_cont) as Box<dyn FnMut(Value) -> Cps>),
            [head, tail @ ..] => {
                let k = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![Local::gensym()], false, None),
                    body: Box::new(tail.compile(meta_cont)),
                    val: k,
                    cexp: Box::new(head.compile(Box::new(move |result| {
                        Cps::App(Value::from(k), vec![result])
                    }))),
                }
            }
        }
    }
}

impl Compile for Literal {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: ClosureArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::Literal(self.clone())],
            )),
            val: k1,
            cexp: Box::new(meta_cont(Value::from(k1))),
        }
    }
}

impl Compile for Body {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        self.forms.as_slice().compile(meta_cont)
    }
}

impl Compile for Apply {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self.operator {
            Either::Left(ref app) => app.compile(Box::new(move |op_result| {
                compile_apply(&op_result, &self.args, &[], Box::new(&mut meta_cont))
            })),
            Either::Right(PrimOp::CallWithCurrentContinuation) => {
                compile_call_with_cc(&self.args[0], meta_cont)
            }
            _ => todo!(),
        }
    }
}

fn compile_apply(
    operator: &Value,
    args: &[Expression],
    collected_args: &[Value],
    mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>,
) -> Cps {
    let (arg, tail) = match args {
        [] => return meta_cont(operator.clone()),
        [arg] => (arg, None),
        [arg, tail @ ..] => (arg, Some(tail)),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(arg.compile(Box::new(move |arg_result| {
            let k3 = Local::gensym();
            let k4 = Local::gensym();
            let new_arg = Local::gensym();
            let collected_args: Vec<_> = collected_args
                .iter()
                .cloned()
                .chain(vec![Value::from(new_arg), Value::from(k3)])
                .collect();
            Cps::Closure {
                args: ClosureArgs::new(vec![new_arg], false, Some(k3)),
                body: Box::new(if let Some(tail) = tail {
                    compile_apply(
                        operator,
                        tail,
                        collected_args.as_slice(),
                        Box::new(move |k| Cps::App(Value::from(k), vec![Value::from(k3)])),
                    )
                } else {
                    Cps::App(operator.clone(), collected_args)
                }),
                val: k4,
                cexp: Box::new(Cps::App(Value::from(k4), vec![arg_result, Value::from(k1)])),
            }
        }))),
        val: k2,
        cexp: Box::new(meta_cont(Value::from(k2))),
    }
}

fn compile_call_with_cc(
    thunk: &Expression,
    mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Closure {
        args: ClosureArgs::new(vec![k1], false, None),
        body: Box::new(thunk.compile(Box::new(|thunk| {
            let k1 = Value::from(k1);
            Cps::App(thunk, vec![k1.clone(), k1])
        }))),
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
                let k4 = Local::gensym();
                let cond_arg = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![cond_arg], false, Some(k3)),
                    body: Box::new(Cps::If(
                        Value::from(cond_arg),
                        Box::new(
                            self.success.compile(Box::new(|success| {
                                Cps::App(success, vec![Value::from(k3)])
                            })),
                        ),
                        Box::new(if let Some(ref failure) = self.failure {
                            failure.compile(Box::new(|failure| {
                                Cps::App(failure, vec![Value::from(k3)])
                            }))
                        } else {
                            Cps::App(Value::from(k3), Vec::new())
                        }),
                    )),
                    val: k4,
                    cexp: Box::new(Cps::App(
                        Value::from(k4),
                        vec![cond_result, Value::from(k1)],
                    )),
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
            let k4 = Local::gensym();
            let cond_arg = Local::gensym();
            Cps::Closure {
                args: ClosureArgs::new(vec![cond_arg], false, Some(k3)),
                body: Box::new(Cps::If(
                    Value::from(cond_arg),
                    Box::new(if let Some(tail) = tail {
                        compile_and(tail, Box::new(|expr| Cps::App(expr, vec![Value::from(k3)])))
                    } else {
                        Cps::App(Value::from(k3), vec![Value::from(true)])
                    }),
                    Box::new(Cps::App(Value::from(k3), vec![Value::from(false)])),
                )),
                val: k4,
                cexp: Box::new(Cps::App(
                    Value::from(k4),
                    vec![expr_result, Value::from(k1)],
                )),
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
            let k4 = Local::gensym();
            let cond_arg = Local::gensym();
            Cps::Closure {
                args: ClosureArgs::new(vec![cond_arg], false, Some(k3)),
                body: Box::new(Cps::If(
                    Value::from(cond_arg),
                    Box::new(Cps::App(Value::from(k3), vec![Value::from(true)])),
                    Box::new(if let Some(tail) = tail {
                        compile_or(tail, Box::new(|expr| Cps::App(expr, vec![Value::from(k3)])))
                    } else {
                        Cps::App(Value::from(k3), vec![Value::from(false)])
                    }),
                )),
                val: k4,
                cexp: Box::new(Cps::App(
                    Value::from(k4),
                    vec![expr_result, Value::from(k1)],
                )),
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

impl Compile for DefineVar {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let set_cont = Cps::Closure {
            args: ClosureArgs::new(vec![k1], false, None),
            body: Box::new(self.val.compile(Box::new(|val_result| {
                let k3 = Local::gensym();
                let k4 = Local::gensym();
                let define_arg = Local::gensym();
                Cps::Closure {
                    args: ClosureArgs::new(vec![define_arg], false, Some(k3)),
                    body: Box::new(Cps::PrimOp(
                        PrimOp::Set,
                        vec![Value::from(self.var.clone()), Value::from(define_arg)],
                        Local::gensym(),
                        Box::new(Cps::App(Value::from(k3), Vec::new())),
                    )),
                    val: k4,
                    cexp: Box::new(Cps::App(Value::from(k4), vec![val_result, Value::from(k1)])),
                }
            }))),
            val: k2,
            cexp: Box::new(meta_cont(Value::from(k2))),
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
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let set_cont = Cps::Closure {
            args: ClosureArgs::new(vec![k1], false, None),
            body: Box::new(compile_lambda(
                self.args.iter().cloned().collect(),
                self.args.is_variadic(),
                &self.body,
                |lambda_result| {
                    Cps::PrimOp(
                        PrimOp::Set,
                        vec![Value::from(self.var.clone()), Value::from(lambda_result)],
                        Local::gensym(),
                        Box::new(Cps::App(Value::from(k1), Vec::new())),
                    )
                },
            )),
            val: k2,
            cexp: Box::new(meta_cont(Value::from(k2))),
        };

        match self.var {
            Var::Global(_) => set_cont,
            Var::Local(local) => Cps::AllocCell(local, Box::new(set_cont)),
        }
    }
}
