//! Basic analysis stuff that we need.
//!
//! ## Free Variables:
//!
//! The free variables of a function are essentially the variables that we need
//! to store in the environment for the closure we create for that function.
//! Functions with no free variables do not escape and thus do not need a
//! closure.
//!
//! To begin, we are converting all functions to closures, regardless of whether
//! or not they escape. In this case, the free variables of a function f is
//! simply F(f) =  V(f) - B(f), where V(f) is the variables in the body of f and
//! B(f) are the variables introduced in a binding in f.
//!
//! The function name itself does not count as a bound variable, and thus is a
//! free variable in the context of the function's body. Also, _globals_ do not
//! count as free variables, because we already have a different way for
//! accessing those.

use super::*;

impl Cps {
    fn free_variables(&self) -> HashSet<Local> {
        match self {
            Self::AllocCell(ref bind, cexpr) => {
                let mut free = cexpr.free_variables();
                free.remove(bind);
                free
            }
            Self::PrimOp(_, args, bind, cexpr) => {
                let mut free = cexpr.free_variables();
                free.remove(bind);
                free.union(&values_to_free_variables(&args))
                    .copied()
                    .collect()
            }
            Self::If(cond, success, failure) => {
                let mut free: HashSet<_> = success
                    .free_variables()
                    .union(&failure.free_variables())
                    .copied()
                    .collect();
                free.extend(cond.to_local());
                free
            }
            Self::App(op, vals) => {
                let mut free = values_to_free_variables(&vals);
                free.extend(op.to_local());
                free
            }
            Self::Closure {
                args,
                body,
                val,
                cexp,
            } => {
                let mut free_body = body.free_variables();
                for arg in args {
                    free_body.remove(arg);
                }
                let mut free: HashSet<_> =
                    free_body.union(&cexp.free_variables()).copied().collect();
                free.remove(val);
                free
            }
            Self::PrintLocal(_) => HashSet::new(),
        }
    }

    // I could merge these into one function, but I"m lazy.
    fn globals(&self) -> HashSet<Global> {
        match self {
            Self::AllocCell(_, cexpr) => cexpr.globals(),
            Self::PrimOp(_, args, _, cexpr) => cexpr
                .globals()
                .union(&values_to_globals(&args))
                .cloned()
                .collect(),
            Self::If(cond, success, failure) => {
                let mut globals: HashSet<_> = success
                    .globals()
                    .union(&failure.globals())
                    .cloned()
                    .collect();
                globals.extend(cond.to_global());
                globals
            }
            Self::App(op, vals) => {
                let mut globals = values_to_globals(&vals);
                globals.extend(op.to_global());
                globals
            }
            Self::Closure { body, cexp, .. } => {
                body.globals().union(&cexp.globals()).cloned().collect()
            }
            Self::PrintLocal(_) => HashSet::new(),
        }
    }
}

fn values_to_free_variables(vals: &[Value]) -> HashSet<Local> {
    vals.iter().flat_map(|val| val.to_local()).collect()
}

fn values_to_globals(vals: &[Value]) -> HashSet<Global> {
    vals.iter().flat_map(|val| val.to_global()).collect()
}
