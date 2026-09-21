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

pub(crate) struct FreeVariables {
    free_vars: HashMap<Local, HashSet<Local>>,
}

impl FreeVariables {
    pub(crate) fn analyze(cps: &Cps) -> Self {
        let mut free_vars = Self {
            free_vars: HashMap::default(),
        };
        free_vars.visit(cps);
        free_vars
    }

    pub(crate) fn free_in(&self, local: Local) -> &HashSet<Local> {
        &self.free_vars[&local]
    }

    #[stacksafe::stacksafe]
    fn visit(&mut self, cps: &Cps) -> HashSet<Local> {
        let free = match &cps.inst {
            Inst::PrimOp(PrimOp::AllocCell, _, bind, cexpr) => {
                let mut free = self.visit(cexpr);
                free.remove(bind);
                free
            }
            Inst::PrimOp(_, args, bind, cexpr) => {
                let mut free = self.visit(cexpr);
                free.remove(bind);
                free.extend(values_to_locals(args));
                free
            }
            Inst::If(cond, success, failure) => {
                let mut free = self.visit(success);
                free.extend(self.visit(failure));
                free.extend(cond.to_local());
                free
            }
            Inst::App(op, vals) => {
                let mut free = values_to_locals(vals);
                free.extend(op.to_local());
                free
            }
            Inst::Fix(bindings, cexpr) => {
                let mut free = self.visit(cexpr);
                for binding in bindings {
                    let mut free_body = self.visit(&binding.body);
                    for arg in binding.args.iter() {
                        free_body.remove(arg);
                    }
                    free.extend(free_body);
                }
                for binding in bindings {
                    free.remove(&binding.val);
                }
                free
            }
            Inst::Halt(val) => val.to_local().into_iter().collect(),
        };
        self.free_vars.insert(cps.local, free.clone());
        free
    }
}

pub(crate) struct Liveness {
    live: HashMap<Local, HashSet<Local>>,
    cont_live: HashMap<Local, HashSet<Local>>,
}

impl Liveness {
    pub(crate) fn analyze(cps: &Cps, free_vars: &FreeVariables, escaping: &Escaping) -> Self {
        let mut liveness = Self {
            live: HashMap::default(),
            cont_live: HashMap::default(),
        };
        liveness.visit(cps, free_vars, escaping);
        liveness
    }

    pub(crate) fn live_in(&self, local: Local) -> &HashSet<Local> {
        &self.live[&local]
    }

    pub(crate) fn live_after_jump(&self, cont: Local) -> &HashSet<Local> {
        &self.cont_live[&cont]
    }

    fn expand(&self, mut live: HashSet<Local>) -> HashSet<Local> {
        let conts: Vec<_> = live
            .iter()
            .filter(|local| self.cont_live.contains_key(local))
            .copied()
            .collect();
        for cont in conts {
            live.extend(self.cont_live[&cont].iter().copied());
        }
        live
    }

    #[stacksafe::stacksafe]
    fn visit(&mut self, cps: &Cps, free_vars: &FreeVariables, escaping: &Escaping) {
        let live = self.expand(free_vars.free_in(cps.local).clone());
        self.live.insert(cps.local, live);
        match &cps.inst {
            Inst::PrimOp(_, _, _, cexpr) => self.visit(cexpr, free_vars, escaping),
            Inst::If(_, succ, fail) => {
                self.visit(succ, free_vars, escaping);
                self.visit(fail, free_vars, escaping);
            }
            Inst::Fix(bindings, cexpr) => {
                let local_conts: Vec<_> = bindings
                    .iter()
                    .filter(|binding| binding.is_continuation() && !escaping.contains(binding.val))
                    .collect();
                for binding in &local_conts {
                    self.cont_live.insert(binding.val, HashSet::default());
                }
                loop {
                    let mut changed = false;
                    for binding in &local_conts {
                        let mut body = self.expand(free_vars.free_in(binding.body.local).clone());
                        for arg in binding.args.iter() {
                            body.remove(arg);
                        }
                        if self.cont_live[&binding.val] != body {
                            self.cont_live.insert(binding.val, body);
                            changed = true;
                        }
                    }
                    if !changed {
                        break;
                    }
                }
                for binding in bindings {
                    self.visit(&binding.body, free_vars, escaping);
                }
                self.visit(cexpr, free_vars, escaping);
            }
            Inst::App(_, _) | Inst::Halt(_) => (),
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
        match &cps.inst {
            Inst::PrimOp(_, args, val, cexpr) => {
                if !self.uses.contains_key(val) {
                    let uses = merge_uses(values_to_uses(args), self.find_uses(cexpr));
                    self.uses.insert(*val, uses);
                }
                self.uses[val].clone()
            }
            Inst::If(cond, success, failure) => {
                let uses = merge_uses(self.find_uses(success), self.find_uses(failure));
                add_value_use(uses, cond)
            }
            Inst::App(op, vals) => {
                let uses = values_to_uses(vals);
                add_value_use(uses, op)
            }
            Inst::Fix(bindings, cexpr) => {
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
            Inst::Halt(value) => add_value_use(HashMap::default(), value),
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
        match &cexpr.inst {
            Inst::App(op, args) => {
                self.scan_vals(args, procs);
                // Functions applied with the wrong number of arguments escape:
                if let Some(local) = op.to_local()
                    && let Some(proc) = procs.get(&local)
                    && !proc.args.matches_args(args.len())
                {
                    self.escaping.insert(local);
                }
            }
            Inst::PrimOp(_, args, _, cexpr) => {
                self.scan_vals(args, procs);
                self.scan(cexpr, procs);
            }
            Inst::If(cond, succ, fail) => {
                self.scan_vals(slice::from_ref(cond), procs);
                self.scan(succ, procs);
                self.scan(fail, procs);
            }
            Inst::Fix(bindings, cexpr) => {
                for binding in bindings {
                    self.scan(&binding.body, procs);
                    // Variadic functions escape (for now):
                    if binding.args.variadic {
                        self.escaping.insert(binding.val);
                    }
                }
                self.scan(cexpr, procs);
            }
            Inst::Halt(val) => self.scan_vals(slice::from_ref(val), procs),
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

    fn find_transitive_closure(
        &mut self,
        procs: &HashMap<Local, &LambdaBinding>,
        free_variables: &FreeVariables,
    ) {
        let mut work_queue = self.escaping.iter().copied().collect::<VecDeque<_>>();
        while let Some(proc) = work_queue.pop_front() {
            for p in free_variables.free_in(procs[&proc].body.local) {
                if procs.contains_key(p) && !self.escaping.contains(p) {
                    self.escaping.insert(*p);
                    work_queue.push_back(*p);
                }
            }
        }
    }
}

impl Cps {
    pub(super) fn cells(&self, out: &mut HashSet<Local>) {
        match &self.inst {
            Inst::PrimOp(PrimOp::AllocCell, _, val, cexp) => {
                cexp.cells(out);
                out.insert(*val);
            }
            Inst::PrimOp(_, _, _, cexp) => {
                cexp.cells(out);
            }
            Inst::If(_, succ, fail) => {
                succ.cells(out);
                fail.cells(out);
            }
            Inst::Fix(bindings, cexp) => {
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
