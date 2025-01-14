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
            args: vec![arg],
            body: Box::new(Cps::PrintLocal(arg)),
            val: k,
            cexp: Box::new(self.compile(Box::new(|expr| Cps::App(expr, vec![Value::from(k)])))),
        }
    }
}

impl Compile for Lambda {
    /// Generates the maximally-correct implementation of a lambda, i.e. a closure that
    /// tail-calls a closure.
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let args: Vec<_> = self.args.iter().cloned().collect();
        compile_lambda(args, &self.body, meta_cont)
    }
}

fn compile_lambda(
    mut args: Vec<Local>,
    body: &Body,
    mut meta_cont: impl FnMut(Value) -> Cps,
) -> Cps {
    let closure_arg = Local::gensym();
    let k = Local::gensym();
    args.push(closure_arg);
    Cps::Closure {
        args,
        body: Box::new(body.compile(Box::new(|result| {
            Cps::App(Value::from(closure_arg), vec![result])
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
            args: vec![k2],
            body: Box::new(Cps::AllocCell(
                *curr_bind,
                Box::new(Cps::Closure {
                    args: vec![expr_result, k4],
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
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        match self {
            Self::Literal(l) => l.compile(meta_cont),
            Self::Apply(e) => e.compile(meta_cont),
            Self::Let(e) => e.compile(meta_cont),
            Self::If(e) => e.compile(meta_cont),
            Self::Lambda(e) => e.compile(meta_cont),
            Self::Var(v) => meta_cont(Value::from(v.clone())),
            x => panic!("not yet implemented: {x:#?}"),
        }
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
                    args: vec![],
                    body: Box::new(tail.compile(meta_cont)),
                    val: k,
                    cexp: Box::new(head.compile(
                        // Discard the result
                        Box::new(move |_result| Cps::App(Value::from(k), vec![])),
                    )),
                }
            }
        }
    }
}

impl Compile for Literal {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        meta_cont(Value::Literal(self.clone()))
    }
}

impl Compile for Body {
    fn compile(&self, meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        self.forms.as_slice().compile(meta_cont)
    }
}

impl Compile for Apply {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        self.operator
            .as_ref()
            .left()
            .unwrap()
            .compile(Box::new(move |op_result| {
                compile_apply(&op_result, &self.args, &[], Box::new(&mut meta_cont))
            }))
    }
}

fn compile_apply(
    operator: &Value,
    args: &[Expression],
    collected_args: &[Value],
    mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>,
) -> Cps {
    // TODO: Move match into closure.
    match args {
        [arg] => {
            let k1 = Local::gensym();
            let k2 = Local::gensym();
            Cps::Closure {
                args: vec![k1],
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
                        args: vec![new_arg, k3],
                        body: Box::new(Cps::App(operator.clone(), collected_args)),
                        val: k4,
                        cexp: Box::new(Cps::App(
                            Value::from(k4),
                            vec![arg_result, Value::from(k1)],
                        )),
                    }
                }))),
                val: k2,
                cexp: Box::new(meta_cont(Value::from(k2))),
            }
        }
        [arg, tail @ ..] => {
            let k1 = Local::gensym();
            let k2 = Local::gensym();
            Cps::Closure {
                args: vec![k1],
                body: Box::new(arg.compile(Box::new(move |arg_result| {
                    let k3 = Local::gensym();
                    let k4 = Local::gensym();
                    let new_arg = Local::gensym();
                    let collected_args: Vec<_> = collected_args
                        .iter()
                        .cloned()
                        .chain(vec![Value::from(new_arg)])
                        .collect();
                    Cps::Closure {
                        args: vec![new_arg, k3],
                        body: Box::new(compile_apply(
                            operator,
                            tail,
                            collected_args.as_slice(),
                            Box::new(move |k| Cps::App(Value::from(k), vec![Value::from(k3)])),
                        )),
                        val: k4,
                        cexp: Box::new(Cps::App(
                            Value::from(k4),
                            vec![arg_result, Value::from(k1)],
                        )),
                    }
                }))),
                val: k2,
                cexp: Box::new(meta_cont(Value::from(k2))),
            }
        }
        [] => todo!(),
    }
}

impl Compile for If {
    fn compile(&self, mut meta_cont: Box<dyn FnMut(Value) -> Cps + '_>) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Closure {
            args: vec![k1],
            body: Box::new(self.cond.compile(Box::new(|cond_result| {
                let k3 = Local::gensym();
                let k4 = Local::gensym();
                let cond_arg = Local::gensym();
                Cps::Closure {
                    args: vec![cond_arg, k3],
                    body: Box::new(Cps::If(
                        Value::Var(Var::Local(cond_arg)),
                        Box::new(
                            self.success.compile(Box::new(|success| {
                                Cps::App(Value::from(k3), vec![success])
                            })),
                        ),
                        Box::new(
                            self.failure.as_ref().unwrap().compile(Box::new(|failure| {
                                Cps::App(Value::from(k3), vec![failure])
                            })),
                        ),
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

/*
impl Compile for And {
    fn compile(&self, mut meta_cont: impl FnMut(Value) -> Cps) -> Cps {
        // This has to be a series of nested Ifs.
        todo!()
    }
}
*/

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
            args: vec![k1],
            body: Box::new(self.val.compile(Box::new(|val_result| {
                let k3 = Local::gensym();
                let k4 = Local::gensym();
                let define_arg = Local::gensym();
                Cps::Closure {
                    args: vec![define_arg, k3],
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
        let args: Vec<_> = self.args.iter().cloned().collect();
        let set_cont = Cps::Closure {
            args: vec![k1],
            body: Box::new(compile_lambda(args, &self.body, |lambda_result| {
                Cps::PrimOp(
                    PrimOp::Set,
                    vec![Value::from(self.var.clone()), Value::from(lambda_result)],
                    Local::gensym(),
                    Box::new(Cps::App(Value::from(k1), Vec::new())),
                )
            })),
            val: k2,
            cexp: Box::new(meta_cont(Value::from(k2))),
        };

        match self.var {
            Var::Global(_) => set_cont,
            Var::Local(local) => Cps::AllocCell(local, Box::new(set_cont)),
        }
    }
}
