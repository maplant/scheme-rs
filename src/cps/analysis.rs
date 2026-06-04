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

use std::{hash::Hash, slice};

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

#[derive(Debug)]
pub(crate) struct Dominators<'a> {
    bindings_map: HashMap<Local, &'a LambdaBinding>,
    idoms: HashMap<Local, ReturnNode>,
}

impl<'a> Dominators<'a> {
    /// Implementation of Contification Using Dominators A_Dom analysis
    pub(crate) fn find_dominators(cps: &'a Cps) -> Self {
        // First, we need a map of all Fix bindings in the program:
        let mut bindings_map = HashMap::default();
        cps.collect_bindings(&mut bindings_map);

        // Collect a map of all the continuation arguments to their respective
        // functions:
        let cont_owners = bindings_map
            .values()
            .filter_map(|func| func.args.continuation.map(|k| (k, func.val)))
            .collect();

        // Compute the return graph G:
        let mut g = ReturnGraph::default();

        // { (Root, k) | k ∈ Cont } ⊆ Edge
        for binding in bindings_map.values() {
            if binding.is_continuation() {
                g.add_edge(ReturnNode::Root, binding.val);
            }
        }

        g.collect_returns(cps, &bindings_map, &cont_owners);

        // Compute dominators using A Simple, Fast Dominance Algorithm
        let mut postorder = Vec::new();
        g.postorder(ReturnNode::Root, &mut HashSet::default(), &mut postorder);

        let node_to_idom_idx = postorder
            .iter()
            .enumerate()
            .map(|(idx, node)| (*node, idx))
            .collect::<HashMap<ReturnNode, _>>();

        let n = postorder.len();
        let root = n - 1;
        let mut idoms = vec![None; n];
        idoms[root] = Some(root);

        let mut changed = true;
        while changed {
            changed = false;
            for (ib, b) in postorder.iter().enumerate().rev() {
                let ReturnNode::Lambda(b) = b else {
                    continue;
                };

                let mut new_idom = None;
                for pred in g.preds[&b].iter() {
                    let Some(&p) = node_to_idom_idx.get(pred) else {
                        continue;
                    };
                    if idoms[p].is_some() {
                        new_idom = Some(match new_idom {
                            None => p,
                            Some(new_idom) => intersect(p, new_idom, &idoms),
                        });
                    }
                }
                if new_idom.is_some() && idoms[ib] != new_idom {
                    idoms[ib] = new_idom;
                    changed = true;
                }
            }
        }

        Self {
            bindings_map,
            idoms: idoms
                .iter()
                .enumerate()
                .filter_map(|(idx, idom)| match postorder[idx] {
                    ReturnNode::Root => None,
                    ReturnNode::Lambda(local) => Some((local, postorder[idom.unwrap()])),
                })
                .collect(),
        }
    }
}

fn intersect(mut finger1: usize, mut finger2: usize, idoms: &[Option<usize>]) -> usize {
    while finger1 != finger2 {
        while finger1 < finger2 {
            finger1 = idoms[finger1].unwrap();
        }
        while finger2 < finger1 {
            finger2 = idoms[finger2].unwrap();
        }
    }
    finger1
}

#[derive(Default)]
pub(crate) struct ReturnGraph {
    edges: HashMap<ReturnNode, HashSet<Local>>,
    preds: HashMap<Local, HashSet<ReturnNode>>,
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum ReturnNode {
    Root,
    Lambda(Local),
}

impl ReturnNode {
    fn to_local(self) -> Option<Local> {
        match self {
            Self::Root => None,
            Self::Lambda(binding) => Some(binding),
        }
    }
}

impl Hash for ReturnNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_local().hash(state)
    }
}

impl PartialEq for ReturnNode {
    fn eq(&self, other: &Self) -> bool {
        self.to_local() == other.to_local()
    }
}

impl Eq for ReturnNode {}

impl ReturnGraph {
    fn postorder(
        &self,
        curr: ReturnNode,
        visited: &mut HashSet<ReturnNode>,
        out: &mut Vec<ReturnNode>,
    ) {
        if !visited.insert(curr) {
            return;
        }
        for edge in self.edges.get(&curr).into_iter().flatten() {
            self.postorder(ReturnNode::Lambda(*edge), visited, out);
        }
        out.push(curr);
    }

    fn collect_returns(
        &mut self,
        cps: &Cps,
        bindings: &HashMap<Local, &LambdaBinding>,
        cont_owners: &HashMap<Local, Local>,
    ) {
        match cps {
            Cps::App(operator, args) => {
                if let Some(func) = lookup_value(operator, bindings)
                    && func.is_func()
                {
                    self.add_return_edge(func.val, args.first(), bindings, cont_owners);
                }
                self.collect_escaping(args, bindings);
            }
            Cps::PrimOp(_, args, _, cexpr) => {
                self.collect_escaping(args, bindings);
                self.collect_returns(cexpr, bindings, cont_owners);
            }
            Cps::If(cond, succ, fail) => {
                self.collect_escaping(slice::from_ref(cond), bindings);
                self.collect_returns(succ, bindings, cont_owners);
                self.collect_returns(fail, bindings, cont_owners);
            }
            Cps::Fix(fix_bindings, cexpr) => {
                for binding in fix_bindings {
                    self.collect_returns(&binding.body, bindings, cont_owners);
                }
                self.collect_returns(cexpr, bindings, cont_owners);
            }
            Cps::Halt(val) => self.collect_escaping(slice::from_ref(val), bindings),
        }
    }

    fn collect_escaping(&mut self, values: &[Value], bindings: &HashMap<Local, &LambdaBinding>) {
        for func in values
            .iter()
            .flat_map(|v| lookup_value(v, bindings))
            .filter(|l| l.is_func())
        {
            self.add_edge(ReturnNode::Root, func.val);
        }
    }

    fn add_return_edge(
        &mut self,
        func: Local,
        return_cont: Option<&Value>,
        bindings: &HashMap<Local, &LambdaBinding>,
        cont_owners: &HashMap<Local, Local>,
    ) {
        match return_cont.and_then(|v| v.to_local()) {
            Some(k) if bindings.get(&k).is_some_and(|k| k.is_continuation()) => {
                self.add_edge(ReturnNode::Lambda(k), func)
            }
            Some(k) if let Some(owner) = cont_owners.get(&k) => {
                self.add_edge(ReturnNode::Lambda(*owner), func)
            }
            _ => self.add_edge(ReturnNode::Root, func),
        }
    }

    fn add_edge(&mut self, from: ReturnNode, to: Local) {
        self.edges.entry(from).or_default().insert(to);
        self.preds.entry(to).or_default().insert(from);
    }
}

fn lookup_value<'a>(
    value: &Value,
    bindings: &HashMap<Local, &'a LambdaBinding>,
) -> Option<&'a LambdaBinding> {
    value
        .to_local()
        .and_then(|local| bindings.get(&local).copied())
}

impl Cps {
    fn collect_bindings<'a>(&'a self, bindings: &mut HashMap<Local, &'a LambdaBinding>) {
        match self {
            Cps::PrimOp(_, _, _, cexpr) => cexpr.collect_bindings(bindings),
            Cps::If(_, succ, fail) => {
                succ.collect_bindings(bindings);
                fail.collect_bindings(bindings);
            }
            Cps::Fix(fix_bindings, cexpr) => {
                for binding in fix_bindings {
                    bindings.insert(binding.val, binding);
                    binding.body.collect_bindings(bindings);
                }
                cexpr.collect_bindings(bindings);
            }
            Cps::Halt(_) => (),
            Cps::App(_, _) => (),
        }
    }
}
