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
use std::{collections::VecDeque, slice};

#[derive(Default)]
pub(crate) struct FreeVariables {
    free_vars: HashMap<Local, HashSet<Local>>,
}

impl FreeVariables {
    #[stacksafe::stacksafe]
    pub fn find_free_vars(&mut self, cps: &Cps) -> HashSet<Local> {
        match cps {
            Cps::PrimOp(PrimOp::AllocCell, _, bind, cexpr) => {
                let mut free = self.find_free_vars(cexpr);
                free.remove(bind);
                free
            }
            Cps::PrimOp(_, args, bind, cexpr) => {
                let mut free = self.find_free_vars(cexpr);
                free.remove(bind);
                free.union(&values_to_locals(args)).copied().collect()
            }
            Cps::If(cond, success, failure) => {
                let mut free: HashSet<_> = self
                    .find_free_vars(success)
                    .union(&self.find_free_vars(failure))
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
                    if !self.free_vars.contains_key(&binding.val) {
                        let mut free_body = self.find_free_vars(&binding.body);
                        for arg in binding.args.iter() {
                            free_body.remove(arg);
                        }
                        self.free_vars.insert(binding.val, free_body);
                    }
                    free_variables = if free_variables.is_empty() {
                        self.free_vars[&binding.val].clone()
                    } else {
                        self.free_vars[&binding.val]
                            .union(&free_variables)
                            .copied()
                            .collect()
                    }
                }
                free_variables = self
                    .find_free_vars(cexpr)
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
pub(crate) struct Uses {
    uses: HashMap<Local, HashMap<Local, usize>>,
}

impl Uses {
    pub fn remove(&mut self, local: &Local) {
        self.uses.remove(local);
    }

    pub fn find_uses(&mut self, cps: &Cps) -> HashMap<Local, usize> {
        match cps {
            Cps::PrimOp(_, args, val, cexpr) => {
                if !self.uses.contains_key(val) {
                    let uses = merge_uses(values_to_uses(args), self.find_uses(cexpr));
                    self.uses.insert(*val, uses);
                }
                self.uses[val].clone()
            }
            Cps::If(cond, success, failure) => {
                let uses = merge_uses(self.find_uses(success), self.find_uses(failure));
                add_value_use(uses, cond)
            }
            Cps::App(op, vals) => {
                let uses = values_to_uses(vals);
                add_value_use(uses, op)
            }
            Cps::Fix(bindings, cexpr) => {
                let mut uses = HashMap::default();
                for binding in bindings {
                    if !self.uses.contains_key(&binding.val) {
                        let uses = self.find_uses(&binding.body);
                        self.uses.insert(binding.val, uses);
                    }
                    uses = if uses.is_empty() {
                        self.uses[&binding.val].clone()
                    } else {
                        merge_uses(self.uses[&binding.val].clone(), uses)
                    };
                }
                merge_uses(uses, self.find_uses(cexpr))
            }
            Cps::Halt(value) => add_value_use(HashMap::default(), value),
        }
    }
}

/// Extremely simple escape analysis. A function escapes if it appears in a
/// non-operator position or if it is among the free variables of a funcction
/// that escapes.
///
/// At the moment, because this analysis is used for the express purpose of
/// contification, we add a few more criteria to mark a function as escaping
/// to make our lives easier:
///
/// - A function is escaping if it is applied with the wrong number of
///   arguments. This is because we have no error path at compilation time
///   for wrong number of arguments.
///
/// - A function is escaping if it is variadic. This should be pretty easy to
///   deal with, but for now we ignore such functions to reduce the code in our
///   initial MVP.
///
#[derive(Default)]
pub struct Escaping {
    escaping: HashSet<Local>,
}

impl Escaping {
    pub fn find_escaping(
        cexpr: &Cps,
        procs: &HashMap<Local, &LambdaBinding>,
        free_variables: &FreeVariables,
    ) -> Self {
        let mut escaping = Self::default();
        escaping.scan(cexpr, procs);
        escaping.find_transitive_closure(procs, free_variables);
        escaping
    }

    pub fn contains(&self, local: Local) -> bool {
        self.escaping.contains(&local)
    }

    fn scan(&mut self, cexpr: &Cps, procs: &HashMap<Local, &LambdaBinding>) {
        match cexpr {
            Cps::App(op, args) => {
                self.scan_vals(args, procs);
                // Functions applied with the wrong number of arguments escape:
                if let Some(local) = op.to_local()
                    && let Some(proc) = procs.get(&local)
                    && !proc.args.matches_args(args.len())
                {
                    self.escaping.insert(local);
                }
            }
            Cps::PrimOp(_, args, _, cexpr) => {
                self.scan_vals(args, procs);
                self.scan(cexpr, procs);
            }
            Cps::If(cond, succ, fail) => {
                self.scan_vals(slice::from_ref(cond), procs);
                self.scan(succ, procs);
                self.scan(fail, procs);
            }
            Cps::Fix(bindings, cexpr) => {
                for binding in bindings {
                    self.scan(&binding.body, procs);
                    // Variadic functions escape (for now):
                    if binding.args.variadic {
                        self.escaping.insert(binding.val);
                    }
                }
                self.scan(cexpr, procs);
            }
            Cps::Halt(val) => self.scan_vals(slice::from_ref(val), procs),
        }
    }

    fn scan_vals<T>(&mut self, vals: &[Value], procs: &HashMap<Local, T>) {
        for val in vals {
            if let Some(proc) = val.to_local()
                && procs.contains_key(&proc)
            {
                self.escaping.insert(proc);
            }
        }
    }

    fn find_transitive_closure<T>(
        &mut self,
        procs: &HashMap<Local, T>,
        free_variables: &FreeVariables,
    ) {
        let mut work_queue = self.escaping.iter().copied().collect::<VecDeque<_>>();
        while let Some(proc) = work_queue.pop_front() {
            for p in &free_variables.free_vars[&proc] {
                if procs.contains_key(p) && !self.escaping.contains(p) {
                    self.escaping.insert(*p);
                    work_queue.push_back(*p);
                }
            }
        }
    }
}

impl Cps {
    pub(super) fn max_drops(&self) -> usize {
        match self {
            Cps::PrimOp(primop, _, _, cexpr) => {
                cexpr.max_drops() + primop.info().needs_drop as usize
            }
            Cps::Fix(bindings, cexpr) => cexpr.max_drops() + bindings.len(),
            Cps::If(_, success, failure) => success.max_drops().max(failure.max_drops()),
            _ => 0,
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
