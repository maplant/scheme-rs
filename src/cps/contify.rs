//! Contification - convert functions into continuations.
//!
//!

use std::{collections::HashSet, hash::Hash, slice};

use super::*;

#[derive(Default)]
struct Scope<'a> {
    up: Option<&'a Scope<'a>>,
    curr: HashSet<Local>,
}

impl Scope<'_> {
    fn contains(&self, local: &Local) -> bool {
        self.curr.contains(local) || self.up.is_some_and(|up| up.contains(local))
    }
}

impl Cps {
    pub(super) fn contify(mut self) -> Self {
        let dominators = Dominators::find_dominators(&self);

        self.flip_in_place_contifiables(&dominators, &Scope::default());

        let mut places = HashMap::default();
        let mut cexpr = self.collect_relocatable_contifiables(&dominators, &mut places);
        cexpr.relocate_and_contify(&dominators, &mut places);

        cexpr
    }

    fn flip_in_place_contifiables(&mut self, dominators: &Dominators, scope: &Scope<'_>) {
        match self {
            Cps::Fix(bindings, cexpr) => {
                let fix_scope = Scope {
                    up: Some(scope),
                    curr: bindings.iter().map(|binding| binding.val).collect(),
                };

                for binding in bindings.iter_mut() {
                    let body_scope = Scope {
                        up: Some(&fix_scope),
                        curr: binding.args.continuation.iter().copied().collect(),
                    };

                    binding
                        .body
                        .flip_in_place_contifiables(dominators, &body_scope);

                    if binding.is_func()
                        && let Some(new_k) = dominators.target_cont(binding.val)
                        && fix_scope.contains(&new_k)
                    {
                        let old_k = binding.args.continuation.take().unwrap();
                        binding.body.substitute(
                            &[(old_k, Value::from(new_k))].into_iter().collect(),
                            &mut UsesCache::default(),
                        );
                    }
                }

                cexpr.flip_in_place_contifiables(dominators, &fix_scope);
            }
            Cps::If(_, succ, fail) => {
                succ.flip_in_place_contifiables(dominators, scope);
                fail.flip_in_place_contifiables(dominators, scope);
            }
            Cps::PrimOp(_, _, _, cexpr) => cexpr.flip_in_place_contifiables(dominators, scope),
            Cps::App(op, args)
                if let Some(local) = op.to_local()
                    && matches!(dominators.idoms.get(&local), Some(ReturnNode::Lambda(_))) =>
            {
                args.remove(0);
            }
            Cps::App(_, _) => (),
            Cps::Halt(_) => (),
        }
    }

    fn collect_relocatable_contifiables(
        self,
        dominators: &Dominators,
        places: &mut HashMap<Local, Vec<LambdaBinding>>,
    ) -> Cps {
        match self {
            Cps::Fix(bindings, cexpr) => {
                let bindings = bindings
                    .into_iter()
                    .filter_map(|mut binding| {
                        binding.body.update_term(|binding| {
                            binding.collect_relocatable_contifiables(dominators, places)
                        });
                        if binding.is_func()
                            && let Some(ReturnNode::Lambda(val)) =
                                dominators.idoms.get(&binding.val)
                        {
                            places.entry(*val).or_default().push(binding);
                            None
                        } else {
                            Some(binding)
                        }
                    })
                    .collect::<Vec<_>>();
                let cexpr = cexpr.collect_relocatable_contifiables(dominators, places);
                if bindings.is_empty() {
                    cexpr
                } else {
                    Cps::Fix(bindings, Box::new(cexpr))
                }
            }
            Cps::If(cond, succ, fail) => Cps::If(
                cond,
                Box::new(succ.collect_relocatable_contifiables(dominators, places)),
                Box::new(fail.collect_relocatable_contifiables(dominators, places)),
            ),
            Cps::PrimOp(op, args, res, cexpr) => Cps::PrimOp(
                op,
                args,
                res,
                Box::new(cexpr.collect_relocatable_contifiables(dominators, places)),
            ),
            cexpr => cexpr,
        }
    }

    fn relocate_and_contify(
        &mut self,
        dominators: &Dominators,
        places: &mut HashMap<Local, Vec<LambdaBinding>>,
    ) {
        match self {
            Cps::Fix(bindings, cexpr) => {
                let mut new_bindings = vec![];
                for binding in bindings.iter_mut() {
                    binding.body.relocate_and_contify(dominators, places);
                    if let Some(mut funcs) = places.remove(&binding.val) {
                        for func in funcs.iter_mut() {
                            func.contify(dominators, places);
                        }
                        if binding.is_func() {
                            binding
                                .body
                                .update_term(|body| Cps::Fix(funcs, Box::new(body)));
                        } else {
                            new_bindings.extend(funcs.into_iter());
                        }
                    }
                }
                bindings.extend(new_bindings);
                cexpr.relocate_and_contify(dominators, places);
            }
            Cps::If(_, succ, fail) => {
                succ.relocate_and_contify(dominators, places);
                fail.relocate_and_contify(dominators, places);
            }
            Cps::PrimOp(_, _, _, cexpr) => cexpr.relocate_and_contify(dominators, places),
            Cps::App(_, _) => (),
            Cps::Halt(_) => (),
        }
    }
}

impl LambdaBinding {
    fn contify(
        &mut self,
        dominators: &Dominators,
        to_place: &mut HashMap<Local, Vec<LambdaBinding>>,
    ) {
        let new_k = dominators.target_cont(self.val).unwrap();
        let old_k = self.args.continuation.take().unwrap();
        let subs = [(old_k, Value::from(new_k))]
            .into_iter()
            .collect::<HashMap<_, _>>();
        self.body.relocate_and_contify(dominators, to_place);
        self.body.substitute(&subs, &mut UsesCache::default());

        // Place any continuations that move into this function:
        if let Some(mut funcs) = to_place.remove(&self.val) {
            for func in funcs.iter_mut() {
                func.contify(dominators, to_place);
            }
            self.body
                .update_term(|body| Cps::Fix(funcs, Box::new(body)));
        }
    }
}

#[derive(Debug)]
pub(crate) struct Dominators {
    idoms: HashMap<Local, ReturnNode>,
    cont_params: HashMap<Local, Local>,
}

impl Dominators {
    /// Return the target continuation that the contifiable function always
    /// returns to, if it is contifiable.
    fn target_cont(&self, f: Local) -> Option<Local> {
        match self.idoms[&f] {
            ReturnNode::Root => None,
            ReturnNode::Lambda(d) => {
                if matches!(self.idoms.get(&d), Some(ReturnNode::Lambda(_))) {
                    self.target_cont(d)
                } else {
                    Some(self.cont_params.get(&d).copied().unwrap_or(d))
                }
            }
        }
    }

    /// Implementation of Contification Using Dominators A_Dom analysis
    pub(crate) fn find_dominators(cps: &Cps) -> Self {
        // First, we need a map of all Fix bindings in the program:
        let mut bindings_map = HashMap::default();
        cps.collect_bindings(&mut bindings_map);

        // Collect a map of all the continuation arguments to their respective
        // functions and a map of all functions to continuation arguments.
        let (cont_owners, cont_params) = bindings_map
            .values()
            .filter_map(|func| {
                let k = func.args.continuation?;
                Some(((k, func.val), (func.val, k)))
            })
            .unzip();

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
            cont_params,
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
