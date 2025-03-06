//! Optimization passes that reduce the amount of CPS code, therefore reducing
//! the amount of LLVM code that needs to be optimized.

use super::*;

impl Cps {
    pub(super) fn reduce(self) -> Self {
        self.beta_reduction(&mut HashMap::default(), &mut HashMap::default())
        // .dead_code_elimination(&mut HashMap::default())
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
    fn beta_reduction(
        self,
        single_use_functions: &mut HashMap<Local, (ClosureArgs, Cps)>,
        uses_cache: &mut HashMap<Local, HashMap<Local, usize>>,
    ) -> Self {
        match self {
            Cps::AllocCell(cell, cexp) => Cps::AllocCell(
                cell,
                Box::new(cexp.beta_reduction(single_use_functions, uses_cache)),
            ),
            Cps::PrimOp(prim_op, values, result, cexp) => Cps::PrimOp(
                prim_op,
                values,
                result,
                Box::new(cexp.beta_reduction(single_use_functions, uses_cache)),
            ),
            Cps::If(cond, success, failure) => Cps::If(
                cond,
                Box::new(success.beta_reduction(single_use_functions, uses_cache)),
                Box::new(failure.beta_reduction(single_use_functions, uses_cache)),
            ),
            Cps::Closure {
                args,
                body,
                val,
                cexp,
            } => {
                let body = body.beta_reduction(single_use_functions, uses_cache);
                let cexp = cexp.beta_reduction(single_use_functions, uses_cache);

                let is_recursive = body.uses(uses_cache).contains_key(&val);
                let uses = cexp.uses(uses_cache).get(&val).copied().unwrap_or(0);

                if !is_recursive && uses == 1 {
                    single_use_functions.insert(val, (args, body));
                    let cexp = cexp.beta_reduction(single_use_functions, uses_cache);
                    if let Some((args, body)) = single_use_functions.remove(&val) {
                        uses_cache.remove(&val);
                        Cps::Closure {
                            args,
                            body: Box::new(body),
                            val,
                            cexp: Box::new(cexp),
                        }
                    } else {
                        cexp
                    }
                } else {
                    uses_cache.remove(&val);
                    Cps::Closure {
                        args,
                        body: Box::new(body),
                        val,
                        cexp: Box::new(cexp),
                    }
                }
            }
            Cps::App(Value::Var(Var::Local(operator)), applied)
                if single_use_functions.contains_key(&operator) =>
            {
                let (args, mut body) = single_use_functions.remove(&operator).unwrap();

                if args.args.len() != applied.len() {
                    // Not really sure what to do about variadic args right now
                    single_use_functions.insert(operator, (args, body));
                    return Cps::App(Value::Var(Var::Local(operator)), applied);
                }

                // Get the substitutions:
                let substitutions: HashMap<_, _> = args
                    .to_vec()
                    .into_iter()
                    .zip(applied.iter().cloned())
                    .collect();

                // Perform the beta reduction:
                body.substitute(&substitutions);

                body
            }
            cexp => cexp,
        }
    }

    /*
    /// Removes any closures and allocated cells that are left unused.
    #[allow(dead_code)]
    fn dead_code_elimination(
        self,
        uses_cache: &mut HashMap<Local, HashMap<Local, usize>>,
    ) -> Self {
        match self {
            Cps::Closure { val, cexp, .. } if !cexp.uses(uses_cache).contains_key(&val) => {
                // Unused closure can be eliminated
                cexp.dead_code_elimination(uses_cache)
            }
            Cps::AllocCell(cell, cexp) if !cexp.uses(uses_cache).contains_key(&cell) => {
                // Unused cell can be eliminated
                cexp.dead_code_elimination(uses_cache)
            }
            Cps::AllocCell(cell, cexp) => {
                Cps::AllocCell(cell, Box::new(cexp.dead_code_elimination(uses_cache)))
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
            Cps::Closure {
                args,
                body,
                val,
                cexp,
                ..
            } => Cps::Closure {
                args,
                body: Box::new(body.dead_code_elimination(uses_cache)),
                val,
                cexp: Box::new(cexp.dead_code_elimination(uses_cache)),
            },
            cexp => cexp,
        }
    }
    */
}
