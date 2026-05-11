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

#[derive(Default)]
pub(crate) struct FreeVariablesCache {
    cache: HashMap<Local, HashSet<Local>>,
}

impl FreeVariablesCache {
    #[stacksafe::stacksafe]
    pub fn free_variables(&mut self, cps: &Cps) -> HashSet<Local> {
        match cps {
            Cps::PrimOp(PrimOp::AllocCell, _, bind, cexpr) => {
                let mut free = self.free_variables(cexpr);
                free.remove(bind);
                free
            }
            Cps::PrimOp(_, args, bind, cexpr) => {
                let mut free = self.free_variables(cexpr);
                free.remove(bind);
                free.union(&values_to_locals(args)).copied().collect()
            }
            Cps::If(cond, success, failure) => {
                let mut free: HashSet<_> = self
                    .free_variables(success)
                    .union(&self.free_variables(failure))
                    .copied()
                    .collect();
                free.extend(cond.to_local());
                free
            }
            Cps::App(op, vals) => {
                let mut free = values_to_locals(vals);
                free.extend(op.to_local());
                free
            }
            Cps::Fix(bindings, cexpr) => {
                let mut free_variables = HashSet::default();
                for binding in bindings {
                    if !self.cache.contains_key(&binding.val) {
                        let mut free_body = self.free_variables(&binding.body);
                        for arg in binding.args.iter() {
                            free_body.remove(arg);
                        }
                        self.cache.insert(binding.val, free_body);
                    }
                    free_variables = if free_variables.is_empty() {
                        self.cache[&binding.val].clone()
                    } else {
                        self.cache[&binding.val]
                            .union(&free_variables)
                            .copied()
                            .collect()
                    }
                }
                free_variables = self
                    .free_variables(cexpr)
                    .union(&free_variables)
                    .copied()
                    .collect();
                for binding in bindings {
                    free_variables.remove(&binding.val);
                }
                free_variables
            }
            Cps::Halt(val) => val.to_local().into_iter().collect(),
        }
    }
}

/// Tracks the number of times a local is used.
#[derive(Default)]
pub(crate) struct UsesCache {
    cache: HashMap<Local, HashMap<Local, usize>>,
}

impl UsesCache {
    pub fn remove(&mut self, local: &Local) {
        self.cache.remove(local);
    }

    pub fn uses(&mut self, cps: &Cps) -> HashMap<Local, usize> {
        match cps {
            Cps::PrimOp(_, args, val, cexpr) => {
                if !self.cache.contains_key(val) {
                    let uses = merge_uses(values_to_uses(args), self.uses(cexpr));
                    self.cache.insert(*val, uses);
                }
                self.cache[val].clone()
            }
            Cps::If(cond, success, failure) => {
                let uses = merge_uses(self.uses(success), self.uses(failure));
                add_value_use(uses, cond)
            }
            Cps::App(op, vals) => {
                let uses = values_to_uses(vals);
                add_value_use(uses, op)
            }

            Cps::Fix(bindings, cexpr) => {
                let mut uses = HashMap::default();
                for binding in bindings {
                    if !self.cache.contains_key(&binding.val) {
                        let uses = self.uses(&binding.body);
                        self.cache.insert(binding.val, uses);
                    }
                    uses = if uses.is_empty() {
                        self.cache[&binding.val].clone()
                    } else {
                        merge_uses(self.cache[&binding.val].clone(), uses)
                    };
                }
                merge_uses(uses, self.uses(cexpr))
            }
            Cps::Halt(value) => add_value_use(HashMap::default(), value),
        }
    }
}

impl Cps {
    pub(super) fn max_drops(&self) -> usize {
        match self {
            Cps::PrimOp(primop, _, _, cexpr) => {
                cexpr.max_drops() + primop.info().needs_drop as usize
            }
            Cps::Fix(_, cexpr) => cexpr.max_drops() + 1,
            Cps::If(_, success, failure) => success.max_drops().max(failure.max_drops()),
            _ => 0,
        }
    }

    /// Returns a all of the variables that set within the cexpr
    pub(super) fn mutable_vars(&self, out: &mut HashSet<Local>) {
        match self {
            Cps::PrimOp(PrimOp::Set, args, _, cexp) => {
                let [to, _] = args.as_slice() else {
                    unreachable!()
                };
                cexp.mutable_vars(out);
                out.extend(to.to_local());
            }
            Cps::PrimOp(_, _, _, cexp) => {
                cexp.mutable_vars(out);
            }
            Cps::If(_, succ, fail) => {
                succ.mutable_vars(out);
                fail.mutable_vars(out);
            }
            Cps::Fix(bindings, cexp) => {
                for binding in bindings {
                    binding.body.mutable_vars(out);
                }
                cexp.mutable_vars(out);
            }
            _ => (),
        }
    }

    pub(super) fn cells(&self, out: &mut HashSet<Local>) {
        match self {
            Cps::PrimOp(PrimOp::AllocCell, _, val, cexp) => {
                cexp.cells(out);
                out.insert(*val);
            }
            Cps::PrimOp(_, _, _, cexp) => {
                cexp.cells(out);
            }
            Cps::If(_, succ, fail) => {
                succ.cells(out);
                fail.cells(out);
            }
            Cps::Fix(bindings, cexp) => {
                for binding in bindings {
                    binding.body.cells(out);
                }
                cexp.cells(out);
            }
            _ => (),
        }
    }
}

fn values_to_locals(vals: &[Value]) -> HashSet<Local> {
    vals.iter().flat_map(|val| val.to_local()).collect()
}

fn values_to_uses(vals: &[Value]) -> HashMap<Local, usize> {
    let mut uses = HashMap::default();
    for local in vals.iter().flat_map(|val| val.to_local()) {
        *uses.entry(local).or_default() += 1;
    }
    uses
}

fn merge_uses(mut l: HashMap<Local, usize>, mut r: HashMap<Local, usize>) -> HashMap<Local, usize> {
    if r.len() > l.len() {
        for (local, uses) in l.into_iter() {
            *r.entry(local).or_default() += uses;
        }
        r
    } else {
        for (local, uses) in r.into_iter() {
            *l.entry(local).or_default() += uses;
        }
        l
    }
}

fn add_value_use(mut uses: HashMap<Local, usize>, value: &Value) -> HashMap<Local, usize> {
    if let Some(local) = value.to_local() {
        *uses.entry(local).or_default() += 1;
    }
    uses
}
