use std::collections::BTreeSet;

use super::*;
use crate::{
    Either,
    ast::*,
    cps::analysis::FreeVariablesCache,
    exceptions::Exception,
    expand::{ExpansionCombiner, SyntaxRule},
    proc::Procedure,
    records::Record,
    runtime::Runtime,
    syntax::{Identifier, Syntax},
    value::Value as RuntimeValue,
};
use indexmap::IndexSet;
use scheme_rs_macros::{maybe_async, maybe_await};

pub(crate) struct Compiler {
    mutable_vars: HashSet<Local>,
    free_vars_cache: FreeVariablesCache,
}

impl Compiler {
    pub fn new(mutable_vars: HashSet<Local>) -> Self {
        Self {
            mutable_vars,
            free_vars_cache: FreeVariablesCache::default(),
        }
    }

    #[maybe_async]
    pub fn compile(
        mut self,
        runtime: &Runtime,
        expr: &impl Compile,
    ) -> Result<Procedure, Exception> {
        let k = Local::gensym();
        let result = Local::gensym();
        let cps = Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![result], true, None),
                body: Box::new(Cps::Halt(Value::from(result))),
                val: k,
                span: None,
            }],
            Box::new(expr.compile(&self, &mut |value| Cps::App(value, vec![Value::from(k)]))),
        );
        let reduced = cps.vals_to_cells(&self.mutable_vars).reduce();
        // Check if there are any remaining free variables, signaling a phase error
        if !self.free_vars_cache.free_variables(&reduced).is_empty() {
            // TODO: More detailed error message with name of variables
            return Err(Exception::error("reference to out of phase identifiers"));
        }
        Ok(maybe_await!(
            runtime.compile_expr(reduced, self.free_vars_cache)
        ))
    }
}

pub trait Compile {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps;
}

impl Compile for Lambda {
    /// Generates the maximally-correct implementation of a lambda, i.e. a closure that
    /// tail-calls a closure.
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        compile_lambda(
            ctxt,
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
    ctxt: &Compiler,
    args: Vec<Local>,
    is_variadic: bool,
    body: &Definitions,
    span: Span,
    name: Option<Symbol>,
    mut meta_cont: impl FnMut(Value) -> Cps,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let mut k4 = Local::gensym();
    k4.name = name;

    Cps::Fix(
        vec![LambdaBinding {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::Fix(
                vec![LambdaBinding {
                    args: LambdaArgs::new(args, is_variadic, Some(k3)),
                    body: Box::new(
                        body.compile(ctxt, &mut |result| Cps::App(result, vec![Value::from(k3)])),
                    ),
                    val: k4,
                    span: Some(span.clone()),
                }],
                Box::new(Cps::App(Value::from(k2), vec![Value::from(k4)])),
            )),
            val: k1,
            span: Some(span),
        }],
        Box::new(meta_cont(Value::from(k1))),
    )
}

impl Compile for Let {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        compile_let(ctxt, &self.bindings, &self.body, meta_cont)
    }
}

fn compile_let(
    ctxt: &Compiler,
    binds: &[(Local, Expression)],
    body: &Definitions,
    meta_cont: &mut dyn FnMut(Value) -> Cps,
) -> Cps {
    if let Some(((curr_bind, curr_expr), tail)) = binds.split_first() {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k2], false, None),
                body: Box::new(Cps::Fix(
                    vec![LambdaBinding {
                        args: LambdaArgs::new(vec![*curr_bind], false, None),
                        body: Box::new(compile_let(ctxt, tail, body, &mut move |result| {
                            Cps::App(result, vec![Value::from(k2)])
                        })),
                        val: k3,
                        span: None,
                    }],
                    Box::new(curr_expr.compile(ctxt, &mut move |result| {
                        Cps::App(result, vec![Value::from(k3)])
                    })),
                )),
                val: k1,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k1))),
        )
    } else {
        body.compile(ctxt, meta_cont)
    }
}

impl Compile for LetRec {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        // Implementation of Robust and Effective Transformation of Letrec by
        // Oscar Waddell, Dipanwita Sarkar, and R. Kent Dybvig.
        //
        // The most important aspect of this is to transform a series of
        // unassigned, mutually recursive lambda bindings into a FIX operator.
        //
        // This allows us to avoid an allocation per binding, but also allows
        // to more rigorously optimize the lambda in the FIX binding.
        //
        // The paper defines one partition that we ignore for now -
        // unreferenced variables. It would be a little annoying to include
        // that analysis at this point, so we defer that to a later time.
        // Unreferenced variables are simply considered simple.

        // Binding partitions:
        // let mut simple = Vec::new();
        let mut lambda = Vec::new();
        let mut complex = Vec::new();

        for (local, expr) in &self.bindings {
            if let Expression::Lambda(l) = expr
                && !ctxt.mutable_vars.contains(local)
            {
                lambda.push((local, l));
            } else {
                complex.push((local, expr));
            }
        }

        // Convert lambda bindings into a fix:
        let mut inner =
            Cps::Fix(
                lambda
                    .into_iter()
                    .map(|(local, lambda)| {
                        let k3 = Local::gensym();
                        LambdaBinding {
                            args: LambdaArgs::new(
                                lambda.args.iter().cloned().collect(),
                                lambda.args.is_variadic(),
                                Some(k3),
                            ),
                            body: Box::new(lambda.body.compile(ctxt, &mut |result| {
                                Cps::App(result, vec![Value::from(k3)])
                            })),
                            val: *local,
                            span: Some(lambda.span.clone()),
                        }
                    })
                    .collect(),
                // Compile the complex bindings
                Box::new(compile_letrec_complex_bindings(
                    ctxt, &complex, &self.body, meta_cont,
                )),
            );

        // Allocate the cells. Do it in reverse just show it shows up better in
        // debug output
        for binding in complex.iter().rev() {
            inner = Cps::PrimOp(PrimOp::AllocCell, Vec::new(), *binding.0, Box::new(inner))
        }

        inner
    }
}

fn compile_letrec_complex_bindings(
    ctxt: &Compiler,
    binds: &[(&Local, &Expression)],
    body: &Definitions,
    meta_cont: &mut dyn FnMut(Value) -> Cps,
) -> Cps {
    if let Some((&(curr_bind, curr_expr), tail)) = binds.split_first() {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k2], false, None),
                body: Box::new(Cps::Fix(
                    vec![LambdaBinding {
                        args: LambdaArgs::new(vec![expr_result], false, None),
                        body: Box::new(Cps::PrimOp(
                            PrimOp::Set,
                            vec![Value::from(*curr_bind), Value::Var(Var::Local(expr_result))],
                            Local::gensym(),
                            Box::new(compile_letrec_complex_bindings(
                                ctxt,
                                tail,
                                body,
                                &mut move |result| Cps::App(result, vec![Value::from(k2)]),
                            )),
                        )),
                        val: k3,
                        span: None,
                    }],
                    Box::new(curr_expr.compile(ctxt, &mut move |result| {
                        Cps::App(result, vec![Value::from(k3)])
                    })),
                )),
                val: k1,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k1))),
        )
    } else {
        body.compile(ctxt, meta_cont)
    }
}

impl Compile for Expression {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        match self {
            Self::Literal(l) => l.compile(ctxt, meta_cont),
            Self::Apply(e) => e.compile(ctxt, meta_cont),
            Self::Let(e) => e.compile(ctxt, meta_cont),
            Self::LetRec(e) => e.compile(ctxt, meta_cont),
            Self::If(e) => e.compile(ctxt, meta_cont),
            Self::Lambda(e) => e.compile(ctxt, meta_cont),
            Self::Var(v) => v.compile(ctxt, meta_cont),
            Self::Begin(e) => e.compile(ctxt, meta_cont),
            Self::And(e) => e.compile(ctxt, meta_cont),
            Self::Or(e) => e.compile(ctxt, meta_cont),
            Self::Quote(q) => q.compile(ctxt, meta_cont),
            Self::SyntaxQuote(sq) => sq.compile(ctxt, meta_cont),
            Self::SyntaxCase(sc) => sc.compile(ctxt, meta_cont),
            Self::Set(set) => set.compile(ctxt, meta_cont),
            Self::Undefined => compile_undefined(meta_cont),
            Self::Vector(vec) => vec.compile(ctxt, meta_cont),
        }
    }
}

impl Compile for Var {
    fn compile(&self, _ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k2], false, None),
                body: Box::new(Cps::App(Value::from(k2), vec![Value::from(self.clone())])),
                val: k1,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k1))),
        )
    }
}

impl Compile for [Expression] {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        match self {
            [] => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Fix(
                    vec![LambdaBinding {
                        args: LambdaArgs::new(vec![k2], false, None),
                        body: Box::new(Cps::App(Value::from(k2), Vec::new())),
                        val: k1,
                        span: None,
                    }],
                    Box::new(meta_cont(Value::from(k1))),
                )
            }
            [last_expr] => last_expr.compile(ctxt, meta_cont),
            [head, tail @ ..] => {
                let k1 = Local::gensym();
                let k2 = Local::gensym();
                Cps::Fix(
                    vec![LambdaBinding {
                        args: LambdaArgs::new(vec![k1], true, None),
                        body: Box::new(tail.compile(ctxt, meta_cont)),
                        val: k2,
                        span: None,
                    }],
                    Box::new(head.compile(ctxt, &mut move |result| {
                        Cps::App(result, vec![Value::from(k2)])
                    })),
                )
            }
        }
    }
}

fn compile_undefined(meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Fix(
        vec![LambdaBinding {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(Cps::App(
                Value::from(k2),
                vec![Value::from(RuntimeValue::undefined())],
            )),
            val: k1,
            span: None,
        }],
        Box::new(meta_cont(Value::from(k1))),
    )
}

impl Compile for RuntimeValue {
    fn compile(&self, _ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k2], false, None),
                body: Box::new(Cps::App(Value::from(k2), vec![Value::from(self.clone())])),
                val: k1,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k1))),
        )
    }
}

impl Compile for Body {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        self.exprs.as_slice().compile(ctxt, meta_cont)
    }
}

impl Compile for Apply {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        compile_apply(
            ctxt,
            &self.operator,
            &self.args,
            self.span.clone(),
            meta_cont,
        )
    }
}

fn compile_apply(
    ctxt: &Compiler,
    operator: &Expression,
    args: &[Expression],
    span: Span,
    meta_cont: &mut dyn FnMut(Value) -> Cps,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    let k3 = Local::gensym();
    let k4 = Local::gensym();
    Cps::Fix(
        vec![LambdaBinding {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new(
                if let Some(primop) = operator.to_primop()
                    && primop.info().matches_args(args.len())
                {
                    compile_primop(ctxt, Value::from(k2), primop, Vec::new(), args)
                } else {
                    let frame = if let Expression::Var(var) = operator
                        && let Some(sym) = var.symbol()
                    {
                        Some(Syntax::Identifier {
                            ident: Identifier {
                                sym,
                                scopes: BTreeSet::new(),
                            },
                            span: span.clone(),
                        })
                    } else {
                        None
                    };
                    operator.compile(ctxt, &mut move |op_result| {
                        Cps::Fix(
                            vec![LambdaBinding {
                                args: LambdaArgs::new(vec![k3], false, None),
                                body: Box::new(compile_apply_args(
                                    ctxt,
                                    Value::from(k2),
                                    Value::from(k3),
                                    Vec::new(),
                                    args,
                                    frame.clone(),
                                    span.clone(),
                                )),
                                val: k4,
                                span: None,
                            }],
                            Box::new(Cps::App(op_result, vec![Value::from(k4)])),
                        )
                    })
                },
            ),
            val: k1,
            span: None,
        }],
        Box::new(meta_cont(Value::from(k1))),
    )
}

fn compile_apply_args(
    ctxt: &Compiler,
    cont: Value,
    op: Value,
    mut collected_args: Vec<Value>,
    remaining_args: &[Expression],
    frame: Option<Syntax>,
    span: Span,
) -> Cps {
    let (arg, tail) = match remaining_args {
        [] => {
            collected_args.push(cont);
            let frame = frame.map_or_else(
                || Value::from(Local::gensym()),
                |frame| Value::from(RuntimeValue::from(frame)),
            );
            let app = Cps::PrimOp(
                PrimOp::SetContinuationMark,
                vec![
                    Value::from(RuntimeValue::from(Symbol::intern("trace"))),
                    frame.clone(),
                ],
                Local::gensym(),
                Box::new(Cps::App(op.clone(), collected_args)),
            );
            return if let Value::Var(Var::Local(frame)) = frame {
                Cps::PrimOp(
                    PrimOp::GetFrame,
                    vec![op, Value::from(RuntimeValue::from_rust_type(span))],
                    frame,
                    Box::new(app),
                )
            } else {
                app
            };
        }
        [arg, tail @ ..] => (arg, tail),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Fix(
        vec![LambdaBinding {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new({
                collected_args.push(Value::from(k2));
                compile_apply_args(ctxt, cont, op, collected_args, tail, frame, span)
            }),
            val: k1,
            span: None,
        }],
        Box::new(arg.compile(ctxt, &mut |result| Cps::App(result, vec![Value::from(k1)]))),
    )
}

fn compile_primop(
    ctxt: &Compiler,
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
    Cps::Fix(
        vec![LambdaBinding {
            args: LambdaArgs::new(vec![k2], false, None),
            body: Box::new({
                collected_args.push(Value::from(k2));
                compile_primop(ctxt, cont, primop, collected_args, tail)
            }),
            val: k1,
            span: None,
        }],
        Box::new(arg.compile(ctxt, &mut |result| Cps::App(result, vec![Value::from(k1)]))),
    )
}

impl Compile for If {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k1], false, None),
                body: Box::new(self.cond.compile(ctxt, &mut |cond_result| {
                    let k3 = Local::gensym();
                    let cond_arg = Local::gensym();
                    Cps::Fix(
                        vec![LambdaBinding {
                            args: LambdaArgs::new(vec![cond_arg], false, None),
                            body: Box::new(Cps::If(
                                Value::from(cond_arg),
                                Box::new(self.success.compile(ctxt, &mut |success| {
                                    Cps::App(success, vec![Value::from(k1)])
                                })),
                                Box::new(if let Some(ref failure) = self.failure {
                                    failure.compile(ctxt, &mut |failure| {
                                        Cps::App(failure, vec![Value::from(k1)])
                                    })
                                } else {
                                    Cps::App(Value::from(k1), Vec::new())
                                }),
                            )),
                            val: k3,
                            span: None,
                        }],
                        Box::new(Cps::App(cond_result, vec![Value::from(k3)])),
                    )
                })),
                val: k2,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k2))),
        )
    }
}

impl Compile for And {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        if self.args.is_empty() {
            let k1 = Local::gensym();
            let k2 = Local::gensym();
            Cps::Fix(
                vec![LambdaBinding {
                    args: LambdaArgs::new(vec![k1], false, None),
                    body: Box::new(Cps::App(
                        Value::from(k1),
                        vec![Value::from(RuntimeValue::from(true))],
                    )),
                    val: k2,
                    span: None,
                }],
                Box::new(meta_cont(Value::from(k2))),
            )
        } else {
            compile_and(ctxt, &self.args, meta_cont)
        }
    }
}

fn compile_and(
    ctxt: &Compiler,
    exprs: &[Expression],
    meta_cont: &mut dyn FnMut(Value) -> Cps,
) -> Cps {
    let (expr, tail) = match exprs {
        [] => return meta_cont(Value::from(RuntimeValue::from(true))),
        [expr] => (expr, None),
        [expr, tail @ ..] => (expr, Some(tail)),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Fix(
        vec![LambdaBinding {
            args: LambdaArgs::new(vec![k1], false, None),
            body: Box::new(expr.compile(ctxt, &mut |expr_result| {
                let k3 = Local::gensym();
                let cond_arg = Local::gensym();
                Cps::Fix(
                    vec![LambdaBinding {
                        args: LambdaArgs::new(vec![cond_arg], false, None),
                        body: if let Some(tail) = tail {
                            Box::new(Cps::If(
                                Value::from(cond_arg),
                                Box::new(compile_and(ctxt, tail, &mut |expr| {
                                    Cps::App(expr, vec![Value::from(k1)])
                                })),
                                Box::new(Cps::App(
                                    Value::from(k1),
                                    vec![Value::from(RuntimeValue::from(false))],
                                )),
                            ))
                        } else {
                            Box::new(Cps::App(Value::from(k1), vec![Value::from(cond_arg)]))
                        },
                        val: k3,
                        span: None,
                    }],
                    Box::new(Cps::App(expr_result, vec![Value::from(k3)])),
                )
            })),
            val: k2,
            span: None,
        }],
        Box::new(meta_cont(Value::from(k2))),
    )
}

impl Compile for Or {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        if self.args.is_empty() {
            let k1 = Local::gensym();
            let k2 = Local::gensym();
            Cps::Fix(
                vec![LambdaBinding {
                    args: LambdaArgs::new(vec![k1], false, None),
                    body: Box::new(Cps::App(
                        Value::from(k1),
                        vec![Value::from(RuntimeValue::from(false))],
                    )),
                    val: k2,
                    span: None,
                }],
                Box::new(meta_cont(Value::from(k2))),
            )
        } else {
            compile_or(ctxt, &self.args, meta_cont)
        }
    }
}

fn compile_or(
    ctxt: &Compiler,
    exprs: &[Expression],
    meta_cont: &mut dyn FnMut(Value) -> Cps,
) -> Cps {
    let (expr, tail) = match exprs {
        [] => return meta_cont(Value::from(RuntimeValue::from(false))),
        [expr] => (expr, None),
        [expr, tail @ ..] => (expr, Some(tail)),
    };

    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Fix(
        vec![LambdaBinding {
            args: LambdaArgs::new(vec![k1], false, None),
            body: Box::new(expr.compile(ctxt, &mut |expr_result| {
                let k3 = Local::gensym();
                let cond_arg = Local::gensym();
                Cps::Fix(
                    vec![LambdaBinding {
                        args: LambdaArgs::new(vec![cond_arg], false, None),
                        body: Box::new(Cps::If(
                            Value::from(cond_arg),
                            Box::new(Cps::App(Value::from(k1), vec![Value::from(cond_arg)])),
                            Box::new(if let Some(tail) = tail {
                                compile_or(ctxt, tail, &mut |expr| {
                                    Cps::App(expr, vec![Value::from(k1)])
                                })
                            } else {
                                Cps::App(
                                    Value::from(k1),
                                    vec![Value::from(RuntimeValue::from(false))],
                                )
                            }),
                        )),
                        val: k3,
                        span: None,
                    }],
                    Box::new(Cps::App(expr_result, vec![Value::from(k3)])),
                )
            })),
            val: k2,
            span: None,
        }],
        Box::new(meta_cont(Value::from(k2))),
    )
}

impl Compile for Definitions {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        match self.inner {
            Either::Left(ref let_rec) => let_rec.compile(ctxt, meta_cont),
            Either::Right(ref exprs) => exprs.compile(ctxt, meta_cont),
        }
    }
}

impl Compile for Set {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let expr_result = Local::gensym();
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let k3 = Local::gensym();
        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k2], false, None),
                body: Box::new(Cps::Fix(
                    vec![LambdaBinding {
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
                        span: None,
                    }],
                    Box::new(self.val.compile(ctxt, &mut move |result| {
                        Cps::App(result, vec![Value::from(k3)])
                    })),
                )),
                val: k1,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k1))),
        )
    }
}

impl Compile for Quote {
    fn compile(&self, _ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k2], false, None),
                body: Box::new(Cps::App(
                    Value::from(k2),
                    vec![Value::from(self.val.clone())],
                )),
                val: k1,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k1))),
        )
    }
}

impl Compile for SyntaxQuote {
    fn compile(&self, _ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        let expanded = Local::gensym();

        let mut expansions_seen = IndexSet::new();
        let mut uses = HashMap::default();

        for (binding, expansion) in self.expansions.iter() {
            let (idx, _) = expansions_seen.insert_full(expansion);
            uses.insert(*binding, idx);
        }

        let mut args = vec![
            Value::from(RuntimeValue::from_rust_type(self.template.clone())),
            Value::from(RuntimeValue::from_rust_type(ExpansionCombiner { uses })),
        ];

        for expansion in expansions_seen.iter() {
            args.push(Value::from(**expansion));
        }

        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k2], false, None),
                body: Box::new(Cps::PrimOp(
                    PrimOp::ExpandTemplate,
                    args,
                    expanded,
                    Box::new(Cps::App(Value::from(k2), vec![Value::from(expanded)])),
                )),
                val: k1,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k1))),
        )
    }
}

impl Compile for SyntaxCase {
    fn compile(&self, ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();
        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k2], false, None),
                body: Box::new(self.arg.compile(ctxt, &mut |expr_result| {
                    let k3 = Local::gensym();
                    let arg = Local::gensym();
                    Cps::Fix(
                        vec![LambdaBinding {
                            args: LambdaArgs::new(vec![arg], false, None),
                            body: Box::new(compile_syntax_rules(
                                ctxt,
                                &self.rules,
                                arg,
                                &mut |expanded| Cps::App(expanded, vec![Value::from(k2)]),
                            )),
                            val: k3,
                            span: None,
                        }],
                        Box::new(Cps::App(expr_result, vec![Value::from(k3)])),
                    )
                })),
                val: k1,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k1))),
        )
    }
}

fn compile_syntax_rules(
    ctxt: &Compiler,
    rules: &[SyntaxRule],
    arg: Local,
    meta_cont: &mut dyn FnMut(Value) -> Cps,
) -> Cps {
    let k1 = Local::gensym();
    let k2 = Local::gensym();
    Cps::Fix(
        vec![LambdaBinding {
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
                            let inner = Box::new(Cps::If(
                                Value::from(match_result),
                                Box::new(rule.output_expression.compile(ctxt, &mut |matches| {
                                    Cps::App(matches, vec![Value::from(k2)])
                                })),
                                Box::new(compile_syntax_rules(
                                    ctxt,
                                    tail,
                                    arg,
                                    &mut |next_pattern| {
                                        Cps::App(next_pattern, vec![Value::from(k2)])
                                    },
                                )),
                            ));
                            if let Some(fender) = &rule.fender {
                                let k3 = Local::gensym();
                                Box::new(Cps::Fix(
                                    vec![LambdaBinding {
                                        args: LambdaArgs::new(vec![match_result], false, None),
                                        body: inner,
                                        val: k3,
                                        span: None,
                                    }],
                                    Box::new(Cps::If(
                                        Value::from(rule.binds),
                                        // Check the fender
                                        Box::new(fender.compile(ctxt, &mut |fender| {
                                            Cps::App(fender, vec![Value::from(k3)])
                                        })),
                                        Box::new(Cps::App(
                                            Value::from(k3),
                                            vec![Value::from(RuntimeValue::from(false))],
                                        )),
                                    )),
                                ))
                            } else {
                                inner
                            }
                        },
                    ))
                }
            },
            val: k1,
            span: None,
        }],
        Box::new(meta_cont(Value::from(k1))),
    )
}

impl Compile for Vector {
    fn compile(&self, _ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();

        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k2], false, None),
                body: Box::new(Cps::App(
                    Value::from(k2),
                    vec![Value::from(RuntimeValue::from(self.vals.clone()))],
                )),
                val: k1,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k1))),
        )
    }
}
impl Compile for Vec<u8> {
    fn compile(&self, _ctxt: &Compiler, meta_cont: &mut dyn FnMut(Value) -> Cps) -> Cps {
        let k1 = Local::gensym();
        let k2 = Local::gensym();

        Cps::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![k2], false, None),
                body: Box::new(Cps::App(
                    Value::from(k2),
                    vec![Value::from(RuntimeValue::from(self.clone()))],
                )),
                val: k1,
                span: None,
            }],
            Box::new(meta_cont(Value::from(k1))),
        )
    }
}

impl Cps {
    /// Convert arguments for closures into cells if they are written to
    fn vals_to_cells(self, mutable_vars: &HashSet<Local>) -> Self {
        match self {
            Cps::PrimOp(PrimOp::AllocCell, vals, local, cexpr) => Cps::PrimOp(
                PrimOp::AllocCell,
                vals,
                local,
                Box::new(cexpr.vals_to_cells(mutable_vars)),
            ),
            Cps::PrimOp(op, vals, local, cexpr) => {
                let cexpr = Box::new(cexpr.vals_to_cells(mutable_vars));
                Cps::PrimOp(op, vals, local, cexpr)
            }
            Cps::If(val, succ, fail) => Cps::If(
                val,
                Box::new(succ.vals_to_cells(mutable_vars)),
                Box::new(fail.vals_to_cells(mutable_vars)),
            ),
            Cps::Fix(bindings, cexpr) => {
                let bindings = bindings
                    .into_iter()
                    .map(|mut binding| {
                        let mut body = Box::new(binding.body.vals_to_cells(mutable_vars));

                        for arg in binding.args.iter_mut() {
                            if mutable_vars.contains(arg) {
                                body = val_to_cell(arg, body);
                            }
                        }

                        LambdaBinding {
                            args: binding.args,
                            body,
                            val: binding.val,
                            span: binding.span,
                        }
                    })
                    .collect();
                Cps::Fix(bindings, Box::new(cexpr.vals_to_cells(mutable_vars)))
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
