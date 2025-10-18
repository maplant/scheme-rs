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
    pub(super) fn free_variables(&self) -> HashSet<Local> {
        match self {
            Cps::PrimOp(PrimOp::AllocCell, _, bind, cexpr) => {
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
            Cps::App(op, vals, _) => {
                let mut free = values_to_locals(vals);
                free.extend(op.to_local());
                free
            }
            Cps::Forward(op, arg) => vec![op.to_local(), arg.to_local()]
                .into_iter()
                .flatten()
                .collect(),
            Cps::Lambda {
                args,
                body,
                val,
                cexp,
                ..
            } => {
                let mut free_body = body.free_variables();
                for arg in args.iter() {
                    free_body.remove(arg);
                }
                let mut free_variables: HashSet<_> =
                    free_body.union(&cexp.free_variables()).copied().collect();
                free_variables.remove(val);
                free_variables
            }
            Cps::Halt(val) => val.to_local().into_iter().collect(),
        }
    }

    pub(super) fn uses(
        &self,
        uses_cache: &mut HashMap<Local, HashMap<Local, usize>>,
    ) -> HashMap<Local, usize> {
        match self {
            // Cps::AllocCell(_, cexpr) => cexpr.uses(uses_cache).clone(),
            Cps::PrimOp(_, args, _, cexpr) => {
                merge_uses(values_to_uses(args), cexpr.uses(uses_cache))
            }
            Cps::If(cond, success, failure) => {
                let uses = merge_uses(success.uses(uses_cache).clone(), failure.uses(uses_cache));
                add_value_use(uses, cond)
            }
            Cps::App(op, vals, _) => {
                let uses = values_to_uses(vals);
                add_value_use(uses, op)
            }
            Cps::Forward(op, arg) => add_value_use(add_value_use(HashMap::new(), op), arg),
            Cps::Lambda {
                body, val, cexp, ..
            } => {
                if !uses_cache.contains_key(val) {
                    let uses = merge_uses(body.uses(uses_cache).clone(), cexp.uses(uses_cache));
                    uses_cache.insert(*val, uses);
                }
                uses_cache.get(val).unwrap().clone()
            }
            Cps::Halt(value) => add_value_use(HashMap::new(), value),
        }
    }

    pub(super) fn max_drops(&self) -> usize {
        match self {
            Cps::PrimOp(PrimOp::AllocCell, _, _, cexpr) => cexpr.max_drops() + 1,
            Cps::PrimOp(
                PrimOp::Cons
                | PrimOp::List
                | PrimOp::Add
                | PrimOp::Sub
                | PrimOp::Mul
                | PrimOp::Div
                | PrimOp::Equal
                | PrimOp::Greater
                | PrimOp::GreaterEqual
                | PrimOp::Lesser
                | PrimOp::LesserEqual
                | PrimOp::Matches
                | PrimOp::ExpandTemplate,
                _,
                _,
                cexpr,
            ) => cexpr.max_drops() + 1,
            Cps::Lambda { cexp, .. } => cexp.max_drops() + 1,
            Cps::PrimOp(_, _, _, cexpr) => cexpr.max_drops(),
            Cps::If(_, success, failure) => success.max_drops().max(failure.max_drops()),
            _ => 0,
        }
    }

    /// Returns a all of the variables that set within the cexpr
    pub(super) fn mutable_vars(&self) -> HashSet<Local> {
        match self {
            Cps::PrimOp(PrimOp::Set, args, _, cexp) => {
                let [to, _] = args.as_slice() else {
                    unreachable!()
                };
                let mut mutables = cexp.mutable_vars();
                mutables.extend(to.to_local());
                mutables
            }
            Cps::PrimOp(_, _, _, cexp) => cexp.mutable_vars(),
            Cps::If(_, succ, fail) => succ
                .mutable_vars()
                .union(&fail.mutable_vars())
                .copied()
                .collect(),
            Cps::Lambda { body, cexp, .. } => body
                .mutable_vars()
                .union(&cexp.mutable_vars())
                .copied()
                .collect(),
            _ => HashSet::new(),
        }
    }

    pub(super) fn cells(&self) -> HashSet<Local> {
        match self {
            Cps::PrimOp(PrimOp::AllocCell, _, val, cexp) => {
                let mut cells = cexp.cells();
                cells.insert(*val);
                cells
            }
            Cps::PrimOp(_, _, _, cexp) => cexp.cells(),
            Cps::If(_, succ, fail) => succ.cells().union(&fail.cells()).copied().collect(),
            Cps::Lambda { body, cexp, .. } => body.cells().union(&cexp.cells()).copied().collect(),
            _ => HashSet::new(),
        }
    }

    /*
    // TODO: Clean up this function!
    pub(super) fn need_cells(
        &self,
        local_args: &HashSet<Local>,
        escaping_arg_cache: &mut HashMap<Local, HashSet<Local>>,
    ) -> HashSet<Local> {
        /*
        match self {
            Cps::PrimOp(PrimOp::Set, args, _, cexp) => {
                let [to, from] = args.as_slice() else {
                    unreachable!()
                };
                // From should always escape so that it can be set. This is
                // really stretching the definition of "escaping", but it's easy
                // to put in here for now.
                let mut escaping_args = cexp.need_cells(local_args, escaping_arg_cache);
                if let Some(local) = from.to_local()
                    && !local_args.contains(&local)
                {
                    escaping_args.insert(local);
                }
                escaping_args.extend(to.to_local());
                escaping_args
            }
            Cps::PrimOp(_, args, _, cexp) => values_to_locals(args)
                .difference(local_args)
                .copied()
                .collect::<HashSet<_>>()
                .union(&cexp.need_cells(local_args, escaping_arg_cache))
                .copied()
                .collect::<HashSet<_>>(),
            Cps::If(cond, success, failure) => {
                let mut escaping_args: HashSet<_> = success
                    .need_cells(local_args, escaping_arg_cache)
                    .union(&failure.need_cells(local_args, escaping_arg_cache))
                    .copied()
                    .collect();
                if let Some(local) = cond.to_local()
                    && !local_args.contains(&local)
                {
                    escaping_args.insert(local);
                }
                escaping_args
            }
            Cps::App(op, vals, _) => {
                let mut escaping_args: HashSet<_> = values_to_locals(vals)
                    .difference(local_args)
                    .copied()
                    .collect();
                match op.to_local() {
                    Some(local) if !local_args.contains(&local) => {
                        escaping_args.insert(local);
                    }
                    _ => (),
                }
                escaping_args
            }
            Cps::Forward(op, val) => {
                let mut escaping_args = HashSet::new();
                if let Some(local) = val.to_local()
                    && !local_args.contains(&local)
                {
                    escaping_args.insert(local);
                }
                if let Some(local) = op.to_local()
                    && !local_args.contains(&local)
                {
                    escaping_args.insert(local);
                }
                escaping_args
            }
            Cps::Halt(val) => {
                let mut escaping_args = HashSet::new();
                if let Some(local) = val.to_local()
                    && !local_args.contains(&local)
                {
                    escaping_args.insert(local);
                }
                escaping_args
            }
            Cps::Lambda {
                args,
                body,
                val,
                cexp,
                ..
            } => {
                if !escaping_arg_cache.contains_key(val) {
                    let new_local_args: HashSet<_> = args.iter().copied().collect();
                    let escaping_args = body
                        .need_cells(&new_local_args, escaping_arg_cache)
                        .union(&cexp.need_cells(local_args, escaping_arg_cache))
                        .copied()
                        .collect();

                    escaping_arg_cache.insert(*val, escaping_args);
                }
                escaping_arg_cache.get(val).unwrap().clone()
            }
        }
         */
        todo!()
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
