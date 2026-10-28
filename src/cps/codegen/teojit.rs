//! Specialized compilation for τJIT, see [`crate::teojit`].
//!
//! A specialized lambda is compiled from the same CPS as its baseline, with:
//! - an entry guard that checks that the arguments that were only ever fixnums
//!   still are, and tail calls the baseline otherwise,
//! - binary arithmetic and comparisons on those arguments without tag checks
//!   or slow paths.

use super::*;
use crate::teojit::teojit_trace;

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
    /// IR of each compiled function, labelled by the part of the lambda.
    pub(super) ir: RefCell<Vec<(String, String)>>,
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

impl Cps {
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
}
