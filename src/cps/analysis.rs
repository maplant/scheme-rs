//! Basic analysis stuff that we need.
//!
//!

use super::*;
use crate::{env::DeBruijnIndex, ast::Body};

impl CpsCtx<'_> {
    /// Returns the variables that are present in the CpsCtx but not local to
    /// the function body.
    pub fn free_variables(&self, body: &Body) -> HashSet<CpsVar> {
        let mut vars = HashSet::new();
        body.free_variables(self, 0, &mut vars);
        vars
    }

    pub fn lexical_to_cps(&self, lexical: DeBruijnIndex) -> CpsVar {
        todo!()
    }
}

trait FreeVariables {
    fn free_variables(&self, ctx: &CpsCtx, curr_depth: usize, free_vars: &mut HashSet<CpsVar>);
}

impl FreeVariables for Body {
    fn free_variables(&self, ctx: &CpsCtx, curr_depth: usize, free_vars: &mut HashSet<CpsVar>) {
        todo!()
    }
}
