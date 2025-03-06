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

use std::cell::{Ref, RefCell};

use super::*;

#[derive(Debug, Default, Clone)]
pub struct Analysis {
    uses: HashMap<Local, usize>,
}

pub(super) type AnalysisCache = RefCell<Option<Analysis>>;

impl Analysis {
    fn analyze_fix(body: &Cps, cexp: &Cps) -> Self {
        let uses = merge_uses(body.uses(), cexp.uses());
        Self { uses }
    }

    fn uses(&self) -> HashMap<Local, usize> {
        self.uses.clone()
    }

    fn fetch<'a>(cache: &'a AnalysisCache, body: &Cps, cexp: &Cps) -> Ref<'a, Analysis> {
        {
            let mut cached = cache.borrow_mut();
            if cached.is_none() {
                *cached = Some(Analysis::analyze_fix(body, cexp));
            }
        }
        Ref::map(cache.borrow(), |cache| cache.as_ref().unwrap())
    }
}

impl Cps {
    // TODO: Have this function return a Cow<'_, HashSet<Local>>
    pub(super) fn free_variables(&self) -> HashSet<Local> {
        match self {
            Cps::AllocCell(ref bind, cexpr) => {
                let mut free = cexpr.free_variables();
                free.remove(bind);
                free
            }
            Cps::PrimOp(_, args, bind, cexpr) => {
                let mut free = cexpr.free_variables();
                free.remove(bind);
                free.union(&values_to_locals(args)).copied().collect()
            }
            Cps::If(cond, success, failure) => {
                let mut free: HashSet<_> = success
                    .free_variables()
                    .union(&failure.free_variables())
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
            Cps::Forward(op, arg) => vec![op.to_local(), arg.to_local()]
                .into_iter()
                .flatten()
                .collect(),
            Cps::Closure {
                args,
                body,
                val,
                cexp,
                ..
            } => {
                let mut free_body = body.free_variables();
                for arg in args.to_vec() {
                    free_body.remove(&arg);
                }
                let mut free_variables: HashSet<_> =
                    free_body.union(&cexp.free_variables()).copied().collect();
                free_variables.remove(val);
                free_variables
            }
            Cps::Halt(val) => val.to_local().into_iter().collect(),
        }
    }

    // TODO: Have this function return a Cow<'_, HashSet<Local>>
    pub(super) fn globals(&self) -> HashSet<Global> {
        match self {
            Cps::AllocCell(_, cexpr) => cexpr.globals(),
            Cps::PrimOp(_, args, _, cexpr) => cexpr
                .globals()
                .union(&values_to_globals(args))
                .cloned()
                .collect(),
            Cps::If(cond, success, failure) => {
                let mut globals: HashSet<_> = success
                    .globals()
                    .union(&failure.globals())
                    .cloned()
                    .collect();
                globals.extend(cond.to_global());
                globals
            }
            Cps::App(op, vals) => {
                let mut globals = values_to_globals(vals);
                globals.extend(op.to_global());
                globals
            }
            Cps::Forward(op, arg) => vec![op.to_global(), arg.to_global()]
                .into_iter()
                .flatten()
                .collect(),
            Cps::Closure { body, cexp, .. } => {
                body.globals().union(&cexp.globals()).cloned().collect()
            }
            Cps::Halt(val) => val.to_global().into_iter().collect(),
        }
    }

    // TODO: Have this function return a Cow<'_, HashSet<Local>>
    pub(super) fn uses(&self) -> HashMap<Local, usize> {
        match self {
            Cps::AllocCell(_, cexpr) => cexpr.uses(),
            Cps::PrimOp(_, args, _, cexpr) => merge_uses(cexpr.uses(), values_to_uses(args)),
            Cps::If(cond, success, failure) => {
                let uses = merge_uses(success.uses(), failure.uses());
                add_value_use(uses, cond)
            }
            Cps::App(op, vals) => {
                let uses = values_to_uses(vals);
                add_value_use(uses, op)
            }
            Cps::Forward(op, arg) => add_value_use(add_value_use(HashMap::new(), op), arg),
            Cps::Closure {
                body,
                cexp,
                analysis,
                ..
            } => Analysis::fetch(analysis, body, cexp).uses(),
            Cps::Halt(value) => add_value_use(HashMap::new(), value),
        }
    }

    pub fn reset_uses(self) -> Self {
        match self {
            Cps::AllocCell(cell, cexpr) => Cps::AllocCell(cell, Box::new(cexpr.reset_uses())),
            Cps::PrimOp(op, args, result, cexpr) => {
                Cps::PrimOp(op, args, result, Box::new(cexpr.reset_uses()))
            }
            Cps::If(cond, success, failure) => Cps::If(
                cond,
                Box::new(success.reset_uses()),
                Box::new(failure.reset_uses()),
            ),
            Cps::Closure {
                args,
                body,
                val,
                cexp,
                ..
            } => Cps::Closure {
                args,
                body: Box::new(body.reset_uses()),
                val,
                cexp: Box::new(cexp.reset_uses()),
                analysis: AnalysisCache::default(),
            },
            cexpr => cexpr,
        }
    }

    /*
    pub(super) fn clear_uses(&self) -> HashMap<Local, usize> {
        match self {
            Cps::AllocCell(_, cexpr) => cexpr.uses(),
            Cps::PrimOp(_, args, _, cexpr) => merge_uses(cexpr.uses(), values_to_uses(args)),
            Cps::If(cond, success, failure) => {
                let uses = merge_uses(success.uses(), failure.uses());
                add_value_use(uses, cond)
            }
            Cps::App(op, vals) => {
                let uses = values_to_uses(vals);
                add_value_use(uses, op)
            }
            Cps::Forward(op, arg) => add_value_use(add_value_use(HashMap::new(), op), arg),
            Cps::Closure {
                body,
                cexp,
                analysis,
                ..
            } => Analysis::fetch(analysis, body, cexp).uses(),
            Cps::Halt(value) => add_value_use(HashMap::new(), value),
        }
    }
    */
}

fn values_to_locals(vals: &[Value]) -> HashSet<Local> {
    vals.iter().flat_map(|val| val.to_local()).collect()
}

fn values_to_uses(vals: &[Value]) -> HashMap<Local, usize> {
    let mut uses = HashMap::new();
    for local in vals.iter().flat_map(|val| val.to_local()) {
        *uses.entry(local).or_default() += 1;
    }
    uses
}

fn values_to_globals(vals: &[Value]) -> HashSet<Global> {
    vals.iter().flat_map(|val| val.to_global()).collect()
}

fn merge_uses(mut l: HashMap<Local, usize>, r: HashMap<Local, usize>) -> HashMap<Local, usize> {
    for (local, uses) in r.into_iter() {
        *l.entry(local).or_default() += uses;
    }
    l
}
fn add_value_use(mut uses: HashMap<Local, usize>, value: &Value) -> HashMap<Local, usize> {
    if let Some(local) = value.to_local() {
        *uses.entry(local).or_default() += 1;
    }
    uses
}
