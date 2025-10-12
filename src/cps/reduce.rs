//! Optimization passes that reduce the amount of CPS code, therefore reducing
//! the amount of LLVM code that needs to be optimized.

use super::*;

impl Cps {
    pub(super) fn reduce(self) -> Self {
        // Perform beta reduction twice. This seems like the sweet spot for now
        let mut uses_cache = HashMap::default();
        self.beta_reduction(&mut uses_cache)
            .beta_reduction(&mut uses_cache)
            .dead_code_elimination(&mut uses_cache)
    }

    /// Beta-reduction optimization step. This function replaces applications to
    /// functions with the body of the function with arguments substituted.
    ///
    /// Our initial heuristic is rather simple: if a function is non-recursive and
    /// is applied to exactly once in its continuation expression, its body is
    /// substituted for the application.
    ///
    /// The uses analysis cache is absolutely demolished and dangerous to use by
    /// the end of this function.
    fn beta_reduction(self, uses_cache: &mut HashMap<Local, HashMap<Local, usize>>) -> Self {
        match self {
            Cps::PrimOp(prim_op, values, result, cexp) => Cps::PrimOp(
                prim_op,
                values,
                result,
                Box::new(cexp.beta_reduction(uses_cache)),
            ),
            Cps::If(cond, success, failure) => Cps::If(
                cond,
                Box::new(success.beta_reduction(uses_cache)),
                Box::new(failure.beta_reduction(uses_cache)),
            ),
            Cps::Lambda {
                args,
                body,
                val,
                cexp,
                span,
            } => {
                let body = body.beta_reduction(uses_cache);
                let mut cexp = cexp.beta_reduction(uses_cache);

                let is_recursive = body.uses(uses_cache).contains_key(&val);
                let uses = cexp.uses(uses_cache).get(&val).copied().unwrap_or(0);

                // TODO: When we get more list primops, allow for variadic substitutions
                if
                /* !args.variadic && */
                !is_recursive && uses == 1 {
                    let reduced = cexp.reduce_function(val, &args, &body, uses_cache);
                    if reduced {
                        // We can probably do better than just destroying the
                        // whole cache, but this works and doesn't hurt perf
                        // too much.
                        uses_cache.clear();
                        return cexp;
                    }
                }

                Cps::Lambda {
                    args,
                    body: Box::new(body),
                    val,
                    cexp: Box::new(cexp),
                    span,
                }
            }
            cexp => cexp,
        }
    }

    fn reduce_function(
        &mut self,
        func: Local,
        args: &LambdaArgs,
        func_body: &Cps,
        uses_cache: &mut HashMap<Local, HashMap<Local, usize>>,
    ) -> bool {
        let new = match self {
            Cps::PrimOp(_, _, _, cexp) => {
                return cexp.reduce_function(func, args, func_body, uses_cache);
            }
            Cps::If(_, succ, fail) => {
                return succ.reduce_function(func, args, func_body, uses_cache)
                    || fail.reduce_function(func, args, func_body, uses_cache);
            }
            Cps::Lambda {
                val, body, cexp, ..
            } => {
                let reduced = body.reduce_function(func, args, func_body, uses_cache)
                    || cexp.reduce_function(func, args, func_body, uses_cache);
                if reduced {
                    uses_cache.remove(val);
                }
                return reduced;
            }
            Cps::App(Value::Var(Var::Local(operator)), applied, _) if *operator == func => {
                if args.variadic {
                    let (req, var) = applied.split_at(args.num_required());
                    let var_args = Local::gensym();
                    Cps::PrimOp(
                        PrimOp::List,
                        var.to_vec(),
                        var_args,
                        Box::new(substitute(
                            func_body.clone(),
                            args,
                            req.iter().cloned().chain(Some(Value::from(var_args))),
                        )),
                    )
                } else {
                    substitute(func_body.clone(), args, applied.iter().cloned())
                }
            }
            Cps::App(_, _, _) | Cps::Forward(_, _) | Cps::Halt(_) => return false,
        };
        *self = new;
        true
    }

    /// Removes any closures and allocated cells that are left unused.
    #[allow(dead_code)]
    fn dead_code_elimination(self, uses_cache: &mut HashMap<Local, HashMap<Local, usize>>) -> Self {
        match self {
            Cps::Lambda { val, cexp, .. } if !cexp.uses(uses_cache).contains_key(&val) => {
                // Unused closure can be eliminated
                cexp.dead_code_elimination(uses_cache)
            }
            Cps::PrimOp(PrimOp::AllocCell, _, result, cexp)
                if !cexp.uses(uses_cache).contains_key(&result) =>
            {
                cexp.dead_code_elimination(uses_cache)
            }
            Cps::PrimOp(prim_op, values, result, cexp) => Cps::PrimOp(
                prim_op,
                values,
                result,
                Box::new(cexp.dead_code_elimination(uses_cache)),
            ),
            Cps::If(cond, success, failure) => Cps::If(
                cond,
                Box::new(success.dead_code_elimination(uses_cache)),
                Box::new(failure.dead_code_elimination(uses_cache)),
            ),
            Cps::Lambda {
                args,
                body,
                val,
                cexp,
                span,
            } => Cps::Lambda {
                args,
                body: Box::new(body.dead_code_elimination(uses_cache)),
                val,
                cexp: Box::new(cexp.dead_code_elimination(uses_cache)),
                span,
            },
            cexp => cexp,
        }
    }
}

fn substitute(mut body: Cps, args: &LambdaArgs, applied: impl Iterator<Item = Value>) -> Cps {
    let substitutions = args.iter().copied().zip(applied).collect::<HashMap<_, _>>();
    body.substitute(&substitutions);
    body
}
