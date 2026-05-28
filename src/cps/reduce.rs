//! Optimization passes that reduce the amount of CPS code, therefore reducing
//! the amount of LLVM code that needs to be optimized.

use crate::cps::analysis::UsesCache;

use super::*;

impl Cps {
    pub(super) fn reduce(mut self) -> Self {
        let mut uses_cache = UsesCache::default();

        // Perform beta and eta reduction until reaching a fixed point:
        loop {
            let mut modified = false;
            self = self
                .beta_reduction(&mut uses_cache, &mut modified)
                .eta_reduction(&mut uses_cache, &mut modified);
            if !modified {
                break;
            }
        }

        // Lastly, perform dead code elimination:
        self.dead_code_elimination(&mut uses_cache)
    }

    /// Beta-reduction optimization step. This function replaces applications to
    /// functions with the body of the function with arguments substituted.
    ///
    /// Our initial heuristic is rather simple: if a function is non-recursive and
    /// is applied to exactly once in its continuation expression, its body is
    /// substituted for the application.
    fn beta_reduction(self, uses_cache: &mut UsesCache, modified: &mut bool) -> Self {
        match self {
            Cps::PrimOp(prim_op, values, result, cexp) => Cps::PrimOp(
                prim_op,
                values,
                result,
                Box::new(cexp.beta_reduction(uses_cache, modified)),
            ),
            Cps::If(cond, success, failure) => Cps::If(
                cond,
                Box::new(success.beta_reduction(uses_cache, modified)),
                Box::new(failure.beta_reduction(uses_cache, modified)),
            ),
            Cps::Fix(mut bindings, cexpr) if bindings.len() == 1 => {
                let binding = bindings.pop().unwrap();

                let body = binding.body.beta_reduction(uses_cache, modified);
                let mut cexpr = cexpr.beta_reduction(uses_cache, modified);

                let is_recursive = uses_cache.uses(&body).contains_key(&binding.val);
                let uses = uses_cache
                    .uses(&cexpr)
                    .get(&binding.val)
                    .copied()
                    .unwrap_or(0);

                if !is_recursive
                    && uses == 1
                    && cexpr.beta_reduce_function(binding.val, &binding.args, &body, uses_cache)
                {
                    *modified = true;
                    return cexpr;
                }

                Cps::Fix(
                    vec![LambdaBinding {
                        args: binding.args,
                        body: Box::new(body),
                        val: binding.val,
                        span: binding.span,
                    }],
                    Box::new(cexpr),
                )
            }
            Cps::Fix(mut bindings, cexpr) => {
                for binding in &mut bindings {
                    let body = std::mem::replace(
                        binding.body.as_mut(),
                        Cps::Halt(Value::Const(RuntimeValue::undefined())),
                    );
                    *binding.body = body.beta_reduction(uses_cache, modified);
                }
                Cps::Fix(
                    bindings,
                    Box::new(cexpr.beta_reduction(uses_cache, modified)),
                )
            }
            cexpr => cexpr,
        }
    }

    fn beta_reduce_function(
        &mut self,
        func: Local,
        args: &LambdaArgs,
        func_body: &Cps,
        uses_cache: &mut UsesCache,
    ) -> bool {
        let new = match self {
            Cps::PrimOp(_, _, val, cexp) => {
                let reduced = cexp.beta_reduce_function(func, args, func_body, uses_cache);
                if reduced {
                    uses_cache.remove(val);
                }
                return reduced;
            }
            Cps::If(_, succ, fail) => {
                return succ.beta_reduce_function(func, args, func_body, uses_cache)
                    || fail.beta_reduce_function(func, args, func_body, uses_cache);
            }
            Cps::Fix(bindings, cexpr) => {
                for binding in bindings {
                    if binding
                        .body
                        .beta_reduce_function(func, args, func_body, uses_cache)
                    {
                        uses_cache.remove(&binding.val);
                        return true;
                    }
                }
                return cexpr.beta_reduce_function(func, args, func_body, uses_cache);
            }
            Cps::App(Value::Var(Var::Local(operator)), applied) if *operator == func => {
                let (k, applied) = if args.continuation.is_some() {
                    if let Some((k, applied)) = applied.split_first() {
                        (Some(k.clone()), applied)
                    } else {
                        return false;
                    }
                } else {
                    (None, applied.as_slice())
                };

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
                            k.into_iter()
                                .chain(req.iter().cloned())
                                .chain(Some(Value::from(var_args))),
                            uses_cache,
                        )),
                    )
                } else if args.num_required() == applied.len() {
                    substitute(
                        func_body.clone(),
                        args,
                        k.into_iter().chain(applied.iter().cloned()),
                        uses_cache,
                    )
                } else {
                    // It's an error if the number of arguments don't match but
                    // defer until evaluation to raise it.
                    return false;
                }
            }
            Cps::App(_, _) | Cps::Halt(_) => return false,
        };
        *self = new;
        true
    }

    /// Eta-reduction optimization steps. Replaces lambdas that forward their
    /// arguments to another lambda with the body lambda.
    fn eta_reduction(self, uses_cache: &mut UsesCache, modified: &mut bool) -> Self {
        match self {
            Cps::PrimOp(prim_op, values, result, cexp) => Cps::PrimOp(
                prim_op,
                values,
                result,
                Box::new(cexp.eta_reduction(uses_cache, modified)),
            ),
            Cps::If(cond, success, failure) => Cps::If(
                cond,
                Box::new(success.eta_reduction(uses_cache, modified)),
                Box::new(failure.eta_reduction(uses_cache, modified)),
            ),
            Cps::Fix(mut bindings, cexpr) if bindings.len() == 1 => {
                let binding = bindings.pop().unwrap();
                let body = binding.body.eta_reduction(uses_cache, modified);
                let mut cexpr = cexpr.eta_reduction(uses_cache, modified);

                if !binding.args.variadic
                    && binding.args.continuation.is_none()
                    && let Cps::App(k, app_args) = &body
                    && *k != Value::from(binding.val)
                    && binding
                        .args
                        .args
                        .iter()
                        .zip(app_args.iter())
                        .all(|(arg, app_arg)| *app_arg == Value::from(*arg))
                {
                    *modified = true;
                    cexpr.substitute(
                        &[(binding.val, k.clone())].into_iter().collect(),
                        uses_cache,
                    );
                    cexpr
                } else {
                    Cps::Fix(
                        vec![LambdaBinding {
                            args: binding.args,
                            body: Box::new(body),
                            val: binding.val,
                            span: binding.span,
                        }],
                        Box::new(cexpr),
                    )
                }
            }
            Cps::Fix(mut bindings, cexpr) => {
                for binding in &mut bindings {
                    let body = std::mem::replace(
                        binding.body.as_mut(),
                        Cps::Halt(Value::Const(RuntimeValue::undefined())),
                    );
                    *binding.body = body.eta_reduction(uses_cache, modified);
                }
                Cps::Fix(
                    bindings,
                    Box::new(cexpr.beta_reduction(uses_cache, modified)),
                )
            }
            cexpr => cexpr,
        }
    }

    /// Removes any closures and allocated cells that are left unused.
    fn dead_code_elimination(self, uses_cache: &mut UsesCache) -> Self {
        match self {
            Cps::PrimOp(PrimOp::AllocCell, _, result, cexpr)
                if !uses_cache.uses(&cexpr).contains_key(&result) =>
            {
                cexpr.dead_code_elimination(uses_cache)
            }
            Cps::PrimOp(prim_op, values, result, cexpr) => Cps::PrimOp(
                prim_op,
                values,
                result,
                Box::new(cexpr.dead_code_elimination(uses_cache)),
            ),
            Cps::If(cond, success, failure) => Cps::If(
                cond,
                Box::new(success.dead_code_elimination(uses_cache)),
                Box::new(failure.dead_code_elimination(uses_cache)),
            ),
            Cps::Fix(mut bindings, cexpr) => {
                let cexpr = cexpr.dead_code_elimination(uses_cache);

                // Compute the live set of the Fix operator. A procedure is live
                // if it used in the continuation expression or used in the body
                // of another live procedure.
                let mut live: HashSet<Local> = uses_cache.uses(&cexpr).keys().copied().collect();
                let mut prev_num_live = 0;
                while live.len() > prev_num_live {
                    prev_num_live = live.len();
                    for binding in &bindings {
                        if live.contains(&binding.val) {
                            live.extend(uses_cache.uses(&binding.body).keys().copied());
                        }
                    }
                }

                bindings = bindings
                    .into_iter()
                    .filter_map(|binding| {
                        live.contains(&binding.val).then(|| LambdaBinding {
                            args: binding.args,
                            body: Box::new(binding.body.dead_code_elimination(uses_cache)),
                            val: binding.val,
                            span: binding.span,
                        })
                    })
                    .collect::<Vec<_>>();

                if bindings.is_empty() {
                    cexpr
                } else {
                    Cps::Fix(bindings, Box::new(cexpr))
                }
            }
            cexpr => cexpr,
        }
    }
}

fn substitute(
    mut body: Cps,
    args: &LambdaArgs,
    applied: impl Iterator<Item = Value>,
    uses_cache: &mut UsesCache,
) -> Cps {
    let substitutions = args.iter().copied().zip(applied).collect::<HashMap<_, _>>();
    body.substitute(&substitutions, uses_cache);
    body
}
