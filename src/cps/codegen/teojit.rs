//! Specialized compilation for τJIT, see [`crate::teojit`].
//!
//! A specialized lambda is compiled from the same CPS as its baseline, with:
//! - an entry guard that checks that the arguments that were only ever fixnums
//!   still are, and tail calls the baseline otherwise,
//! - binary arithmetic and comparisons on those arguments without tag checks
//!   or slow paths,
//! - direct calls to itself when it calls itself through a global, e.g. `fib`,
//!   handing over its own reference to the closure when it can, so that the
//!   call needs no reference counting.

use super::*;
use crate::{env::Global, gc::Gc, proc::FuncPtr, teojit::teojit_trace, value::UnpackedValue};

/// Everything needed to compile a user lambda again.
pub(crate) struct RetainedLambda {
    unit: Arc<Unit>,
    /// Points into `unit`.
    binding: *const LambdaBinding,
    env: Vec<Local>,
}

// SAFETY: `binding` points into `unit`, which is never mutated and is kept
// alive by `unit`.
unsafe impl Send for RetainedLambda {}
unsafe impl Sync for RetainedLambda {}

impl RetainedLambda {
    pub(super) fn new(unit: Arc<Unit>, binding: &LambdaBinding, env: Vec<Local>) -> Self {
        Self { unit, binding, env }
    }

    fn binding(&self) -> &LambdaBinding {
        unsafe { &*self.binding }
    }
}

/// A specialized compilation of a user lambda.
pub(super) struct Specialization {
    /// Locals known to hold fixnums.
    fixnums: HashSet<Local>,
    /// Arguments guarded to be fixnums on entry.
    guarded_args: Vec<usize>,
    /// Entry of the baseline, tail called when a guard fails.
    baseline: UserPtr,
    /// The specialized body itself.
    func_id: FuncId,
    /// A global the lambda calls itself through, if any.
    self_ref: Option<SelfRef>,
    /// IR of each compiled function, labelled by the part of the lambda.
    pub(super) ir: RefCell<Vec<(String, String)>>,
}

/// A global holding a closure of the lambda being specialized. Calls through it
/// go straight to the specialized code while it still holds that closure.
struct SelfRef {
    /// Address of the global's cell, to recognize reads of it.
    cell: usize,
    /// The global's cell as a raw value, to read it normally.
    cell_value: i64,
    /// Address of the value stored in the cell, read without locking. It is
    /// only compared against `proc`, never dereferenced.
    slot: usize,
    /// The raw closure the global held at specialization time. It is kept
    /// alive by the lambda's LambdaCode, so it is never freed.
    proc: i64,
}

/// Compile a specialized version of a hot user lambda.
pub(crate) fn specialize(
    code: &'static LambdaCode,
    baseline: UserPtr,
    runtime_funcs: &RuntimeFunctions,
    module: &mut JITModule,
    debug_info: &mut DebugInfo,
) -> Result<UserPtr, &'static str> {
    let retained = code.retained().ok_or("lambda was not retained")?;
    let binding = retained.binding();
    let args = &binding.args;
    if args.variadic {
        return Err("variadic");
    }
    if args.args.len() > MAX_DIRECT_ARGS {
        return Err("too many arguments");
    }
    if binding.body.contains_user_lambda() {
        return Err("contains nested lambdas");
    }

    let arg_types = code.arg_types();
    let guarded_args = (0..args.args.len())
        .filter(|i| {
            arg_types[*i].is_only(Tag::FixNum) && !retained.unit.cells.contains(&args.args[*i])
        })
        .collect::<Vec<_>>();
    if guarded_args.is_empty() {
        return Err("no fixnum arguments");
    }

    let func_id = module
        .declare_anonymous_function(&user_sig())
        .expect("Could not declare function");
    let spec = Rc::new(Specialization {
        fixnums: guarded_args.iter().map(|i| args.args[*i]).collect(),
        guarded_args: guarded_args.clone(),
        baseline,
        func_id,
        self_ref: find_self_ref(code, &binding.body),
        ir: RefCell::default(),
    });
    let bundle = ProcedureBundle {
        func_id,
        owner: Some(ReturnOwner {
            code,
            cont: args.continuation.unwrap(),
        }),
        spec: Some(spec.clone()),
        val: binding.val,
        env: retained.env.clone(),
        binding,
    };

    let mut continuations = HashSet::default();
    let mut deferred_procs = vec![bundle];
    while let Some(next) = deferred_procs.pop() {
        next.codegen(
            runtime_funcs,
            &retained.unit,
            &mut continuations,
            module,
            debug_info,
            &mut deferred_procs,
        );
    }
    module.finalize_definitions().unwrap();

    teojit_trace!(
        "{code}: specializing -> specialized (arguments {guarded_args:?} guarded as FixNum)"
    );
    code.dump_ir(&spec.ir.take());
    let entry = module.get_finalized_function(func_id) as *mut u8;
    Ok(UserPtr(NonNull::new(entry).unwrap()))
}

/// Find a global that `body` reads and that holds a closure of the lambda
/// being specialized, e.g. `fib` in the body of `fib`.
fn find_self_ref(code: &'static LambdaCode, body: &Cps) -> Option<SelfRef> {
    let mut globals = Vec::new();
    body.read_globals(&mut globals);
    globals.into_iter().find_map(|global| {
        let value = global.read();
        let is_self = matches!(
            &*value.unpacked_ref(),
            UnpackedValue::Procedure(proc)
                if matches!(proc.0.func, FuncPtr::User(_, c) if std::ptr::eq(c, code))
        );
        if !is_self {
            return None;
        }
        teojit_trace!(
            "{code}: calls through global {} go directly to the specialized version",
            global.name
        );
        let cell = SchemeValue::from(global.val.clone());
        let self_ref = SelfRef {
            cell: Gc::as_ptr(&global.val.0) as usize,
            cell_value: SchemeValue::as_raw(&cell) as i64,
            slot: global.val.0.data_ptr() as usize,
            proc: SchemeValue::as_raw(&value) as i64,
        };
        code.keep_alive(value);
        code.keep_alive(cell);
        Some(self_ref)
    })
}

impl Cps {
    /// Collect the globals read in this expression, outside of nested user
    /// lambdas.
    fn read_globals(&self, out: &mut Vec<Global>) {
        match &self.inst {
            Inst::PrimOp(PrimOp::ReadCell, args, _, cexpr) => {
                if let [CpsValue::Var(Var::Global(global))] = args.as_slice() {
                    out.push(global.clone());
                }
                cexpr.read_globals(out);
            }
            Inst::PrimOp(_, _, _, cexpr) => cexpr.read_globals(out),
            Inst::If(_, success, failure) => {
                success.read_globals(out);
                failure.read_globals(out);
            }
            Inst::Fix(bindings, cexpr) => {
                for binding in bindings.iter().filter(|binding| !binding.is_func()) {
                    binding.body.read_globals(out);
                }
                cexpr.read_globals(out);
            }
            Inst::App(_, _) | Inst::Halt(_) => (),
        }
    }

    /// Whether a user lambda is defined anywhere in this expression.
    fn contains_user_lambda(&self) -> bool {
        match &self.inst {
            Inst::PrimOp(_, _, _, cexpr) => cexpr.contains_user_lambda(),
            Inst::If(_, success, failure) => {
                success.contains_user_lambda() || failure.contains_user_lambda()
            }
            Inst::Fix(bindings, cexpr) => {
                bindings
                    .iter()
                    .any(|binding| binding.is_func() || binding.body.contains_user_lambda())
                    || cexpr.contains_user_lambda()
            }
            Inst::App(_, _) | Inst::Halt(_) => false,
        }
    }
}

/// Check the guarded arguments of a specialized lambda on entry, and tail call
/// the baseline instead if any of them is not a fixnum.
pub(super) fn guard_codegen(
    builder: &mut FunctionBuilder,
    module: &mut JITModule,
    runtime_funcs: &RuntimeFunctions,
    entry_block: Block,
    spec: &Specialization,
    code: &LambdaCode,
) {
    let params = builder.block_params(entry_block).to_vec();
    let mut anded = params[USER_ARG1_PARAM + spec.guarded_args[0]];
    for i in &spec.guarded_args[1..] {
        anded = builder.ins().band(anded, params[USER_ARG1_PARAM + i]);
    }
    let low_bit = builder.ins().band_imm_s(anded, 1);
    let guarded_block = builder.create_block();
    let deopt_block = builder.create_block();
    builder
        .ins()
        .brif(low_bit, guarded_block, &[], deopt_block, &[]);

    builder.switch_to_block(deopt_block);
    builder.seal_block(deopt_block);
    builder.set_cold_block(deopt_block);
    let teojit_deopt = module.declare_func_in_func(runtime_funcs.teojit_deopt, builder.func);
    let code = builder
        .ins()
        .iconst(types::I64, code as *const LambdaCode as i64);
    builder.ins().call(teojit_deopt, &[code]);
    let baseline = builder
        .ins()
        .iconst(types::I64, spec.baseline.0.as_ptr() as i64);
    let sig = builder.import_signature(user_sig());
    builder.ins().return_call_indirect(sig, baseline, &params);

    builder.switch_to_block(guarded_block);
    builder.seal_block(guarded_block);
}

impl<'c> CompilationUnit<'_, '_> {
    /// Whether a value is known to be a fixnum: a fixnum constant, or a local
    /// guarded to be one in a specialized lambda.
    pub(super) fn is_known_fixnum(&self, val: &CpsValue) -> bool {
        match val {
            CpsValue::Const(val) => val.tag() == Tag::FixNum,
            CpsValue::Var(Var::Local(local)) => self
                .spec
                .as_ref()
                .is_some_and(|spec| spec.fixnums.contains(local)),
            _ => false,
        }
    }

    fn self_ref(&self) -> Option<&SelfRef> {
        self.spec.as_ref()?.self_ref.as_ref()
    }

    /// Fold a read of the global the specialized lambda calls itself through
    /// into the call that follows it, see [`Self::self_call_codegen`]. Only
    /// done if `result` is only used as the operator of that call and nothing
    /// in between can change the global.
    pub(super) fn fold_self_ref_read(
        &mut self,
        global: &Global,
        result: Local,
        cexpr: &Cps,
    ) -> bool {
        let Some(self_ref) = self.self_ref() else {
            return false;
        };
        if Gc::as_ptr(&global.val.0) as usize != self_ref.cell || !only_called(result, cexpr) {
            return false;
        }
        // The value is only read at the call.
        let proc = self_ref.proc;
        let proc = self.builder.ins().iconst(types::I64, proc);
        self.live.bind(result, IrValue::Ref(proc));
        self.self_procs.insert(result);
        true
    }

    /// Call through a folded read of the global the lambda calls itself
    /// through. If the global still holds the closure we specialized for, tail
    /// call the specialized code directly. If we are running that closure,
    /// hand over our own reference to it instead of cloning it. Otherwise, read
    /// the global and return it, to be called normally.
    pub(super) fn self_call_codegen(
        &mut self,
        operator: &CpsValue,
        op: Value,
        args: [Value; 7],
    ) -> Value {
        let Some(local) = operator
            .to_local()
            .filter(|local| self.self_procs.contains(local))
        else {
            return op;
        };
        let spec = self.spec.clone().unwrap();
        let self_ref = spec.self_ref.as_ref().unwrap();

        let slot = self.builder.ins().iconst(types::I64, self_ref.slot as i64);
        let current = self
            .builder
            .ins()
            .load(types::I64, MemFlagsData::trusted(), slot, 0);
        let is_self = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::Equal, current, self_ref.proc);
        let self_call_block = self.builder.create_block();
        let read_block = self.builder.create_block();
        self.builder
            .ins()
            .brif(is_self, self_call_block, &[], read_block, &[]);

        self.builder.switch_to_block(self_call_block);
        self.builder.seal_block(self_call_block);
        let live = self.live.clone();
        if let Some(proc_local) = self.proc_local
            && let IrValue::Owned(ours) = *self.live.fetch(&proc_local)
        {
            let is_ours = self
                .builder
                .ins()
                .icmp_imm_s(IntCC::Equal, ours, self_ref.proc);
            let hand_over_block = self.builder.create_block();
            let clone_block = self.builder.create_block();
            self.builder
                .ins()
                .brif(is_ours, hand_over_block, &[], clone_block, &[]);

            self.builder.switch_to_block(hand_over_block);
            self.builder.seal_block(hand_over_block);
            self.live.kill(proc_local);
            self.self_tail_call(spec.func_id, ours, args);
            self.live = live.clone();

            self.builder.switch_to_block(clone_block);
            self.builder.seal_block(clone_block);
        }
        let cloned = self.clone_value_codegen(current);
        self.self_tail_call(spec.func_id, cloned, args);
        self.live = live;

        // The global was redefined, read it and call it normally. Once defined,
        // a global cannot become undefined again.
        self.builder.switch_to_block(read_block);
        self.builder.seal_block(read_block);
        let cell = self.builder.ins().iconst(types::I64, self_ref.cell_value);
        let read_cell = self
            .module
            .declare_func_in_func(self.runtime_funcs.read_cell, self.builder.func);
        let call = self.builder.ins().call(read_cell, &[cell]);
        let read = self.builder.inst_results(call)[0];
        self.live.bind(local, IrValue::Owned(read));
        read
    }

    /// Tail call the specialized body, passing it the owned closure `proc`.
    fn self_tail_call(
        &mut self,
        func_id: FuncId,
        proc: Value,
        [arg1, arg2, arg3, arg4, argn, barrier, out]: [Value; 7],
    ) {
        let proc = self.builder.ins().band_imm_s(proc, !(TAG as i64));
        self.drop_all_codegen();
        let callee = self.module.declare_func_in_func(func_id, self.builder.func);
        self.builder
            .ins()
            .return_call(callee, &[proc, arg1, arg2, arg3, arg4, argn, barrier, out]);
    }
}

/// Whether `local` is only used as the operator of the call that ends
/// `cexpr`, with nothing in between that could change a global.
fn only_called(local: Local, mut cexpr: &Cps) -> bool {
    let uses = |vals: &[CpsValue]| vals.iter().any(|val| val.to_local() == Some(local));
    loop {
        match &cexpr.inst {
            Inst::PrimOp(PrimOp::SetCell | PrimOp::CallKnown0 | PrimOp::CallKnown1, ..) => {
                return false;
            }
            Inst::PrimOp(_, args, _, next) if !uses(args) => cexpr = next,
            Inst::App(op, args) => return op.to_local() == Some(local) && !uses(args),
            _ => return false,
        }
    }
}
