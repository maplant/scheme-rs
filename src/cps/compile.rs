use super::*;
use crate::ast::*;

/// There's not too much reason that this is a trait, other than I wanted to
/// see all of the Compile implementations in one place.
pub trait Compile {
    fn compile(&self, meta_cont: impl FnMut(Value) -> Cps) -> Cps;
}

impl Compile for Lambda {
    /// Generates the maximally-correct implementation of a lambda, i.e. a closure that
    /// tail-calls a closure.
    fn compile(&self, mut meta_cont: impl FnMut(Value) -> Cps) -> Cps {
        /*
            let f = Var::gensym();
            let env = Var::gensym();
            let k = Var::gensym();

            // TODO: This does not capture any of the parameters into an environment, that
            // needs to be done next.
            let body = self.body.compile(env, |z| {
                // In order to call the continuation, we must extract the function pointer
                // and the parent environment.
                let func_ptr = Var::gensym();
                let parent_env = Var::gensym();
                Cps::Select(
                    0,
                    k,
                    func_ptr,
                    Box::new(Cps::Select(
                        1,
                        k,
                        parent_env,
                        // Now that we have the parent environment and the function pointer,
                        // we can call the continuation closure.
                        Box::new(Cps::App(
                            Value::Var(func_ptr),
                            vec![Value::Var(parent_env), z],
                        )),
                    )),
                )
            });

            // Generates a closure for the lambda function.
            Cps::Fix(f, vec![k, env], Box::new(body), {
                let closure = Var::gensym();
                let s1 = Var::gensym();
                let s2 = Var::gensym();
                Box::new(Cps::Record(
                    2,
                    closure,
                    Box::new(Cps::Select(
                        0,
                        closure,
                        s1,
                        Box::new(Cps::PrimOp(
                            PrimOp::Set,
                            Value::Var(f),
                            s1,
                            Box::new(Cps::Select(
                                1,
                                closure,
                                s2,
                                Box::new(Cps::PrimOp(
                                    PrimOp::Set,
                                    Value::Var(parent_env),
                                    s2,
                                    Box::new(meta_cont(Value::Var(closure))),
                                )),
                            )),
                        )),
                    )),
                ))
        })
             */
        todo!()
    }
}

/*
impl Compile for Let {
    fn compile(&self, parent_env: Var, meta_cont: impl FnMut(Value) -> Cps) -> Cps {
        let env = Var::gensym();
        let body = self.body.compile(env, meta_cont);
        Cps::Record(self.bindings.len(), parent_env, env, Box::new(body))
    }
}
 */

impl Compile for Expression {
    fn compile(&self, meta_cont: impl FnMut(Value) -> Cps) -> Cps {
        todo!()
    }
}

impl Compile for &[Expression] {
    fn compile(&self, mut meta_cont: impl FnMut(Value) -> Cps) -> Cps {
        match self {
            [] => panic!("Empty body"),
            [last_expr] => last_expr.compile(meta_cont),
            [head, tail @ ..] => compile_lambda(
                &[],
                tail,
                |val| meta_cont(val),
                |closure| head.compile(move |_discard| Cps::App(closure.clone(), Vec::new())),
            ),
        }
    }
}

fn compile_lambda(
    args: &[Var],
    body: &[Expression],
    inner_meta_cont: impl FnMut(Value) -> Cps,
    outer_meta_cont: impl FnMut(Value) -> Cps,
) -> Cps {
    todo!()
}

impl Compile for If {
    fn compile(&self, mut meta_cont: impl FnMut(Value) -> Cps) -> Cps {
        self.cond.compile(|result| {
            Cps::If(
                result,
                Box::new(self.success.compile(&mut meta_cont)),
                // TODO: Deal with empty failure
                Box::new(self.failure.as_ref().unwrap().compile(&mut meta_cont)),
            )
        })
    }
}

impl Compile for And {
    fn compile(&self, mut meta_cont: impl FnMut(Value) -> Cps) -> Cps {
        // This has to be a series of nested Ifs.
        todo!()
    }
}
