//! Basic analysis stuff that we need.
//!
//!

use super::*;

/// To begin, we are converting all functions to closures, regardless of whether
/// or not they escape. In this case, the free variables of a function f is
/// simply F(f) =  V(f) - B(f), where V(f) is the variables in the body of f and
/// B(f) are the variables introduced in a binding in f.
trait FreeVariables {
    fn free_variables(&self) -> HashSet<Var>;
}
