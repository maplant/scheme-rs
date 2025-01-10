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

use crate::ast::*;

use super::*;

trait FreeVariables {
    fn free_variables(&self) -> HashSet<Local>;
}

impl FreeVariables for DefineFunc {
    fn free_variables(&self) -> HashSet<Local> {
        let bound: HashSet<Local> = self.args.iter().copied().collect();
        let free_vars = self.body.free_variables();
        free_vars.difference(&bound).copied().collect()
    }
}

impl FreeVariables for Lambda {
    fn free_variables(&self) -> HashSet<Local> {
        let bound: HashSet<Local> = self.args.iter().copied().collect();
        let free_vars = self.body.free_variables();
        free_vars.difference(&bound).copied().collect()
    }
}

impl FreeVariables for Let {
    fn free_variables(&self) -> HashSet<Local> {
        // We do not need to do anything special with regards to ordering here,
        // this is because all locals are globally unique.
        let (bound, free_vars): (HashSet<_>, Vec<_>) = self
            .bindings
            .iter()
            .map(|(bound, expr)| (bound, expr.free_variables()))
            .unzip();
        let free_vars: HashSet<_> = free_vars.into_iter().flatten().collect();
        free_vars.difference(&bound).copied().collect()
    }
}

impl FreeVariables for Body {
    fn free_variables(&self) -> HashSet<Local> {
        todo!()
    }
}

impl FreeVariables for Expression {
    fn free_variables(&self) -> HashSet<Local> {
        todo!()
    }
}
