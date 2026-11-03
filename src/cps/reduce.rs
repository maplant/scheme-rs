//! Optimization passes that reduce the amount of CPS code, therefore reducing
//! the amount of Cranelift code that needs to be generated and optimized.

use crate::cps::analysis::Uses;

use super::*;

impl Cps {
    pub(super) fn reduce(mut self) -> Self {
        let mut uses = Uses::default();
        let mut constants = HashMap::default();
        let mut global_aliases = HashMap::default();

        // Perform beta and eta reduction and propagate constants until reaching
        // a fixed point:
        loop {
            let mut reduced = false;
            let mut modified = false;
            self = self
                .beta_reduction(&mut uses, &mut reduced)
                .eta_reduction(&mut uses, &mut reduced)
                .propagate_constants(
                    &Scope::default(),
                    &mut constants,
                    &mut global_aliases,
                    &mut uses,
                    &mut modified,
                );
            if !reduced && !modified {
                break;
            }
        }

        // Lastly, perform dead code elimination:
        self.dead_code_elimination(&mut uses)
    }

    /// Beta-reduction optimization step. This function replaces applications to
    /// functions with the body of the function with arguments substituted.
    ///
    /// Our initial heuristic is rather simple: if a function is non-recursive and
    /// is applied to exactly once in its continuation expression, its body is
    /// substituted for the application.
    fn beta_reduction(self, uses: &mut Uses, modified: &mut bool) -> Self {
        let Cps { inst, dest: local } = self;
        match inst {
            Inst::PrimOp(prim_op, values, cexp) => Cps::with_dest(
                Inst::PrimOp(
                    prim_op,
                    values,
                    Box::new(cexp.beta_reduction(uses, modified)),
                ),
                local,
            ),
            Inst::If(cond, success, failure) => Cps::with_dest(
                Inst::If(
                    cond,
                    Box::new(success.beta_reduction(uses, modified)),
                    Box::new(failure.beta_reduction(uses, modified)),
                ),
                local,
            ),
            Inst::Fix(mut bindings, cexpr) if bindings.len() == 1 => {
                let binding = bindings.pop().unwrap();

                let body = binding.body.beta_reduction(uses, modified);
                let mut cexpr = cexpr.beta_reduction(uses, modified);

                let is_recursive = uses.find_uses(&body).contains_key(&binding.val);
                let num_uses = uses
                    .find_uses(&cexpr)
                    .get(&binding.val)
                    .copied()
                    .unwrap_or(0);

                if !is_recursive
                    && num_uses == 1
                    && cexpr.beta_reduce_function(binding.val, &binding.args, &body, uses)
                {
                    *modified = true;
                    return cexpr;
                }

                Cps::with_dest(
                    Inst::Fix(
                        vec![LambdaBinding {
                            args: binding.args,
                            val: binding.val,
                            span: binding.span,
                            body,
                        }],
                        Box::new(cexpr),
                    ),
                    local,
                )
            }
            Inst::Fix(mut bindings, cexpr) => {
                for LambdaBinding { body, .. } in &mut bindings {
                    body.update_term(|body| body.beta_reduction(uses, modified));
                }
                Cps::with_dest(
                    Inst::Fix(bindings, Box::new(cexpr.beta_reduction(uses, modified))),
                    local,
                )
            }
            inst => Cps::with_dest(inst, local),
        }
    }

    fn beta_reduce_function(
        &mut self,
        func: Local,
        args: &LambdaArgs,
        func_body: &Cps,
        uses: &mut Uses,
    ) -> bool {
        let new = match &mut self.inst {
            Inst::PrimOp(_, _, cexp) => {
                let reduced = cexp.beta_reduce_function(func, args, func_body, uses);
                if reduced {
                    uses.remove(&self.dest);
                }
                return reduced;
            }
            Inst::If(_, succ, fail) => {
                return succ.beta_reduce_function(func, args, func_body, uses)
                    || fail.beta_reduce_function(func, args, func_body, uses);
            }
            Inst::Fix(bindings, cexpr) => {
                for binding in bindings {
                    if binding
                        .body
                        .beta_reduce_function(func, args, func_body, uses)
                    {
                        uses.remove(&binding.val);
                        return true;
                    }
                }
                return cexpr.beta_reduce_function(func, args, func_body, uses);
            }
            Inst::App(Value::Var(Var::Local(operator)), applied) if *operator == func => {
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
                    Cps::with_dest(
                        Inst::PrimOp(
                            PrimOp::List,
                            var.to_vec(),
                            Box::new(substitute(
                                func_body.clone(),
                                args,
                                k.into_iter()
                                    .chain(req.iter().cloned())
                                    .chain(Some(Value::from(var_args))),
                                uses,
                            )),
                        ),
                        var_args,
                    )
                } else if args.num_required() == applied.len() {
                    substitute(
                        func_body.clone(),
                        args,
                        k.into_iter().chain(applied.iter().cloned()),
                        uses,
                    )
                } else {
                    // It's an error if the number of arguments don't match but
                    // defer until evaluation to raise it.
                    return false;
                }
            }
            Inst::App(_, _) | Inst::Halt(_) => return false,
        };
        *self = new;
        true
    }

    /// Eta-reduction optimization steps. Replaces lambdas that forward their
    /// arguments to another lambda with the body lambda.
    #[stacksafe::stacksafe]
    fn eta_reduction(self, uses: &mut Uses, modified: &mut bool) -> Self {
        let Cps { inst, dest } = self;
        match inst {
            Inst::PrimOp(prim_op, values, cexp) => Cps::with_dest(
                Inst::PrimOp(
                    prim_op,
                    values,
                    Box::new(cexp.eta_reduction(uses, modified)),
                ),
                dest,
            ),
            Inst::If(cond, success, failure) => Cps::with_dest(
                Inst::If(
                    cond,
                    Box::new(success.eta_reduction(uses, modified)),
                    Box::new(failure.eta_reduction(uses, modified)),
                ),
                dest,
            ),
            Inst::Fix(mut bindings, cexpr) if bindings.len() == 1 => {
                let binding = bindings.pop().unwrap();
                let body = binding.body.eta_reduction(uses, modified);
                let mut cexpr = cexpr.eta_reduction(uses, modified);

                if !binding.args.variadic
                    && binding.args.continuation.is_none()
                    && let Inst::App(k, app_args) = &body.inst
                    && *k != Value::from(binding.val)
                    && binding.args.args.len() == app_args.len()
                    && binding
                        .args
                        .args
                        .iter()
                        .zip(app_args.iter())
                        .all(|(arg, app_arg)| *app_arg == Value::from(*arg))
                {
                    *modified = true;
                    cexpr.substitute(&[(binding.val, k.clone())].into_iter().collect(), uses);
                    cexpr
                } else {
                    Cps::with_dest(
                        Inst::Fix(
                            vec![LambdaBinding {
                                args: binding.args,
                                body,
                                val: binding.val,
                                span: binding.span,
                            }],
                            Box::new(cexpr),
                        ),
                        dest,
                    )
                }
            }
            Inst::Fix(mut bindings, cexpr) => {
                for LambdaBinding { body, .. } in &mut bindings {
                    body.update_term(|body| body.eta_reduction(uses, modified));
                }
                Cps::with_dest(
                    Inst::Fix(bindings, Box::new(cexpr.eta_reduction(uses, modified))),
                    dest,
                )
            }
            inst => Cps::with_dest(inst, dest),
        }
    }

    /// Removes any closures and allocated cells that are left unused.
    fn dead_code_elimination(self, uses: &mut Uses) -> Self {
        let Cps { inst, dest: local } = self;
        match inst {
            Inst::PrimOp(PrimOp::AllocCell, _, cexpr)
                if !uses.find_uses(&cexpr).contains_key(&local) =>
            {
                cexpr.dead_code_elimination(uses)
            }
            Inst::PrimOp(prim_op, values, cexpr) => Cps::with_dest(
                Inst::PrimOp(prim_op, values, Box::new(cexpr.dead_code_elimination(uses))),
                local,
            ),
            Inst::If(cond, success, failure) => Cps::with_dest(
                Inst::If(
                    cond,
                    Box::new(success.dead_code_elimination(uses)),
                    Box::new(failure.dead_code_elimination(uses)),
                ),
                local,
            ),
            Inst::Fix(mut bindings, cexpr) => {
                let cexpr = cexpr.dead_code_elimination(uses);

                // Compute the live set of the Fix operator. A procedure is live
                // if it used in the continuation expression or used in the body
                // of another live procedure.
                let mut live: HashSet<Local> = uses.find_uses(&cexpr).keys().copied().collect();
                let mut prev_num_live = 0;
                while live.len() > prev_num_live {
                    prev_num_live = live.len();
                    for binding in &bindings {
                        if live.contains(&binding.val) {
                            live.extend(uses.find_uses(&binding.body).keys().copied());
                        }
                    }
                }

                bindings = bindings
                    .into_iter()
                    .filter_map(|binding| {
                        live.contains(&binding.val).then(|| LambdaBinding {
                            args: binding.args,
                            body: binding.body.dead_code_elimination(uses),
                            val: binding.val,
                            span: binding.span,
                        })
                    })
                    .collect::<Vec<_>>();

                if bindings.is_empty() {
                    cexpr
                } else {
                    Cps::with_dest(Inst::Fix(bindings, Box::new(cexpr)), local)
                }
            }
            inst => Cps::with_dest(inst, local),
        }
    }

    /// Propagates constants and fold constant expressions.
    ///
    /// Globals also count as constants if the are immutable and already defined.
    #[stacksafe::stacksafe]
    fn propagate_constants(
        self,
        scope: &Scope<'_>,
        constants: &mut HashMap<Local, Value>,
        global_aliases: &mut HashMap<Global, Value>,
        uses: &mut Uses,
        modified: &mut bool,
    ) -> Self {
        let Cps { inst, dest } = self;

        let scope = scope.new([dest]);

        let result = match inst {
            Inst::PrimOp(PrimOp::ReadCell, vals, cexpr)
                if let [val] = &*vals
                    && let Some(global) = val.to_global()
                    && !global.is_mutable()
                    && let read_val = global.read()
                    && !read_val.is_undefined() =>
            {
                // Reading from a global immutable variable can be
                // propagated if it has already been set. Remove the
                // read.
                *modified = true;
                constants.insert(dest, Value::from(read_val));
                cexpr.propagate_constants(&scope, constants, global_aliases, uses, modified)
            }
            Inst::PrimOp(PrimOp::ReadCell, vals, cexpr)
                if let [val] = &*vals
                    && let Some(global) = val.to_global()
                    && !global.is_mutable()
                    && let Some(alias) = global_aliases.get(&global)
                    && alias
                        .to_local()
                        .map_or(true, |local| scope.contains(&local)) =>
            {
                let alias = if let Some(local) = alias.to_local()
                    && let Some(constant) = constants.get(&local)
                {
                    Value::from(constant.clone())
                } else {
                    alias.clone()
                };
                *modified = true;
                // An immutable global variable has an alias that is in scope,
                // replace the read with the alias
                constants.insert(dest, alias.clone());
                cexpr.propagate_constants(&scope, constants, global_aliases, uses, modified)
            }
            Inst::PrimOp(PrimOp::SetCell, vals, cexpr)
                if let [global, from] = vals.as_slice()
                    && let Some(global) = global.to_global()
                    && let from = from.clone()
                    && !global.is_mutable()
                    && !global_aliases.contains_key(&global) =>
            {
                if matches!(from, Value::Const(_))
                    || from.to_local().is_some_and(|local| scope.contains(&local))
                {
                    // If we set a global immutable variable to a constant, that
                    // constant can become an alias
                    *modified = true;
                    global_aliases.insert(global, from.clone());
                }
                // We still need the set cell
                Cps::with_dest(
                    Inst::PrimOp(
                        PrimOp::SetCell,
                        vals,
                        Box::new(cexpr.propagate_constants(
                            &scope,
                            constants,
                            global_aliases,
                            uses,
                            modified,
                        )),
                    ),
                    dest,
                )
            }
            Inst::PrimOp(prim_op, mut vals, cexpr) => {
                substitute_values(&mut vals, constants);
                // Attempt to fold the primitive operator
                if let Some(folded) = prim_op.fold(&vals) {
                    constants.insert(dest, folded);
                    cexpr.propagate_constants(&scope, constants, global_aliases, uses, modified)
                } else {
                    let cexpr =
                        cexpr.propagate_constants(&scope, constants, global_aliases, uses, modified);
                    Cps::with_dest(Inst::PrimOp(prim_op, vals, Box::new(cexpr)), dest)
                }
            }
            Inst::If(mut cond, succ, fail) => {
                // Propagate constants across branches:
                let mut modified_in_succ = false;
                let succ = succ.propagate_constants(
                    &scope,
                    constants,
                    global_aliases,
                    uses,
                    &mut modified_in_succ,
                );
                let mut modified_in_fail = false;
                let fail = fail.propagate_constants(
                    &scope,
                    constants,
                    global_aliases,
                    uses,
                    &mut modified_in_fail,
                );
                *modified |= modified_in_fail | modified_in_succ;

                // Eliminate a branch if the conditional is known:
                substitute_value(&mut cond, constants);
                if let Value::Const(cond) = &cond
                    && !cond.is_undefined()
                {
                    if cond.is_true() {
                        *modified = true;
                        return succ;
                    } else {
                        *modified = true;
                        return fail;
                    }
                }

                Cps::with_dest(Inst::If(cond, Box::new(succ), Box::new(fail)), dest)
            }
            Inst::Fix(mut bindings, cexpr) => {
                let scope = scope.new(bindings.iter().map(|binding| binding.val));

                for LambdaBinding { body, val, .. } in &mut bindings {
                    let mut modified_body_branch = false;
                    body.update_term(|body| {
                        body.propagate_constants(
                            &scope,
                            constants,
                            global_aliases,
                            uses,
                            &mut modified_body_branch,
                        )
                    });
                    if modified_body_branch {
                        uses.remove(&val);
                    }
                    *modified |= modified_body_branch;
                }

                let cexpr =
                    cexpr.propagate_constants(&scope, constants, global_aliases, uses, modified);

                Cps::with_dest(Inst::Fix(bindings, Box::new(cexpr)), dest)
            }
            Inst::App(mut op, mut args) => {
                substitute_value(&mut op, constants);
                substitute_values(&mut args, constants);
                Cps::with_dest(Inst::App(op, args), dest)
            }
            Inst::Halt(mut val) => {
                substitute_value(&mut val, constants);
                Cps::with_dest(Inst::Halt(val), dest)
            }
        };

        if *modified {
            uses.remove(&dest);
        }

        result
    }
}

impl PrimOp {
    /// Attempt to fold a primitive operator into a constant value.
    fn fold(self, vals: &[Value]) -> Option<Value> {
        use crate::{
            // lists::{car, cdr, list},
            num::{add, div, equal, greater, greater_equal, lesser, lesser_equal, mul, sub},
            proc::{ContBarrier, Procedure},
            value::{not, null_pred, pair_pred},
        };

        let func_ptr = match self {
            // TODO: Need to check if the pair is immutable first
            // Self::Car => car,
            // Self::Cdr => cdr,
            // Self::List => list,
            Self::IsNull => null_pred,
            Self::IsPair => pair_pred,
            Self::Add => add,
            Self::Mul => mul,
            Self::Sub => sub,
            Self::Div => div,
            Self::Equal => equal,
            Self::Greater => greater,
            Self::GreaterEqual => greater_equal,
            Self::Lesser => lesser,
            Self::LesserEqual => lesser_equal,
            Self::Not => not,
            _ => return None,
        };

        let args = vals
            .iter()
            .map(|val| val.to_const())
            .collect::<Option<Vec<_>>>()?;

        let proc = Procedure::new(Vec::new(), func_ptr, 0, true);

        #[cfg(not(feature = "async"))]
        let result = proc.call(&args, &mut ContBarrier::new());
        #[cfg(feature = "async")]
        let result = proc.call_sync(&args, &mut ContBarrier::new());

        result.ok().and_then(|results| {
            let [result] = results.try_into().ok()?;
            Some(Value::Const(result))
        })
    }
}

fn substitute(
    mut body: Cps,
    args: &LambdaArgs,
    applied: impl Iterator<Item = Value>,
    uses: &mut Uses,
) -> Cps {
    let substitutions = args.iter().copied().zip(applied).collect::<HashMap<_, _>>();
    body.substitute(&substitutions, uses);
    body
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Local;

    #[test]
    fn eta_reduction_preserves_lambdas_that_drop_args() {
        // Lambda(a, b): App(f, a) must NOT be reduced to f,
        // because that would change the arity and silently drop b.
        let a = Local::gensym();
        let b = Local::gensym();
        let f = Local::gensym();
        let lambda_val = Local::gensym();

        let cps = Cps::new(Inst::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![a, b], false, None),
                body: Cps::new(Inst::App(Value::from(f), vec![Value::from(a)])),
                val: lambda_val,
                span: None,
            }],
            Box::new(Cps::new(Inst::Halt(Value::from(lambda_val)))),
        ));

        let reduced = cps.reduce();

        // The lambda must survive — it should NOT be eta-reduced.
        match &reduced.inst {
            Inst::Fix(bindings, _) if bindings.len() == 1 => {
                let binding = &bindings[0];
                assert_eq!(binding.args.args.len(), 2, "lambda should keep both args");
                assert!(
                    matches!(&binding.body.inst, Inst::App(_, app_args) if app_args.len() == 1),
                    "body should still forward only one arg"
                );
            }
            other => panic!("expected Fix, got {other:?}"),
        }
    }

    #[test]
    fn eta_reduction_still_works_for_exact_forwarding() {
        // Lambda(a, b): App(f, a, b) SHOULD be reduced to f.
        let a = Local::gensym();
        let b = Local::gensym();
        let f = Local::gensym();
        let lambda_val = Local::gensym();

        let cps = Cps::new(Inst::Fix(
            vec![LambdaBinding {
                args: LambdaArgs::new(vec![a, b], false, None),
                body: Cps::new(Inst::App(
                    Value::from(f),
                    vec![Value::from(a), Value::from(b)],
                )),
                val: lambda_val,
                span: None,
            }],
            Box::new(Cps::new(Inst::Halt(Value::from(lambda_val)))),
        ));

        let reduced = cps.reduce();

        // The lambda should be eta-reduced: Halt(f)
        match &reduced.inst {
            Inst::Halt(val) => {
                assert_eq!(*val, Value::from(f), "should eta-reduce to f");
            }
            other => panic!("expected Halt(f) after eta-reduction, got {other:?}"),
        }
    }
}
