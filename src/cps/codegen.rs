//! Cranelift Codegen from CPS.

use cranelift::{
    codegen::{
        ir::{BlockArg, MemFlagsData, StackSlot, entities::Value},
        isa::CallConv,
    },
    prelude::*,
};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use indexmap::IndexMap;
use std::sync::Arc;

use crate::{
    cps::{
        Value as CpsValue,
        analysis::{Escaping, FreeVariables, Liveness},
    },
    proc::{ContinuationPtr, ProcDebugInfo, Procedure},
    runtime::{DebugInfo, Runtime},
    value::{
        FALSE_VALUE, FIXNUM_MAX, FIXNUM_MIN, NULL_VALUE, TAG, TRUE_VALUE, Tag, UNDEFINED_VALUE,
        Value as SchemeValue,
    },
};

use super::*;

#[derive(Copy, Clone, Debug)]
enum IrValue {
    Cell(Value),
    Ref(Value),
    Owned(Value),
    Dead,
}

#[derive(Clone)]
struct LiveValues {
    frames: Vec<IndexMap<Local, IrValue>>,
}

impl LiveValues {
    fn new() -> Self {
        Self {
            frames: vec![IndexMap::default()],
        }
    }

    fn bind(&mut self, var: Local, value: IrValue) {
        self.frames.last_mut().unwrap().insert(var, value);
    }

    fn kill(&mut self, var: Local) {
        self.bind(var, IrValue::Dead);
    }

    fn fetch(&self, var: &Local) -> &IrValue {
        self.frames
            .iter()
            .rev()
            .find_map(|frame| frame.get(var))
            .unwrap_or_else(|| panic!("could not find {var:?}"))
    }

    fn push_frame(&mut self) {
        self.frames.push(IndexMap::default());
    }

    fn pop_frame(&mut self) {
        self.frames.pop().unwrap();
    }

    fn owned(&self) -> impl Iterator<Item = (Local, Value)> + use<> {
        self.frames
            .iter()
            .flat_map(|frame| frame.iter().map(|(&var, &value)| (var, value)))
            .collect::<IndexMap<_, _>>()
            .into_iter()
            .filter_map(|(var, value)| match value {
                IrValue::Owned(val) | IrValue::Cell(val) => Some((var, val)),
                IrValue::Ref(_) | IrValue::Dead => None,
            })
    }
}

#[derive(derive_builder::Builder)]
pub(crate) struct RuntimeFunctions {
    apply: FuncId,
    halt: FuncId,
    make_user: FuncId,
    raise_wrong_num_args: FuncId,
    tail_callable: FuncId,
    proc_env: FuncId,
    pop_env: FuncId,
    push_continuation: FuncId,
    call_continuation: FuncId,
    pop_jit_continuation: FuncId,
    patch_env_slot: FuncId,
    unroot_proc: FuncId,
    alloc_cell: FuncId,
    read_cell: FuncId,
    store: FuncId,
    error_unbound_variable: FuncId,
    clonev: FuncId,
    dropv: FuncId,
    raise_rt: FuncId,

    // Syntax primops:
    matches: FuncId,
    expand_template: FuncId,
    error_no_patterns_match: FuncId,

    // List primops:
    cons: FuncId,
    list: FuncId,
    append: FuncId,
    car: FuncId,
    cdr: FuncId,

    // Frame primops:
    #[cfg(feature = "continuation-marks")]
    get_frame: FuncId,

    // Continuation mark primops:
    #[cfg(feature = "continuation-marks")]
    set_continuation_mark: FuncId,

    // Math primops:
    add: FuncId,
    sub: FuncId,
    i64_to_number: FuncId,
    i128_to_number: FuncId,
    mul: FuncId,
    div: FuncId,
    equal: FuncId,
    greater: FuncId,
    greater_equal: FuncId,
    lesser: FuncId,
    lesser_equal: FuncId,
}

fn rust_entry_codegen(module: &mut JITModule, body: FuncId, entry: FuncId) {
    let mut ctx = module.make_context();
    ctx.func.signature = module
        .declarations()
        .get_function_decl(entry)
        .signature
        .clone();
    let mut builder_context = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
    let block = builder.create_block();
    builder.append_block_params_for_function_params(block);
    builder.switch_to_block(block);
    builder.seal_block(block);

    let params = builder.block_params(block).to_vec();
    let body = module.declare_func_in_func(body, builder.func);
    builder.ins().call(body, &params);
    builder.ins().return_(&[]);
    builder.finalize(module.target_config());

    module.define_function(entry, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
}

impl Cps {
    pub(crate) fn compile(
        self,
        runtime_funcs: &RuntimeFunctions,
        module: &mut JITModule,
        debug_info: &mut DebugInfo,
    ) -> ContinuationPtr {
        if std::env::var("SCHEME_RS_DEBUG").is_ok() {
            eprintln!(
                "- Compiling: -------------------------------------------------------------------"
            );
            self.pretty_print(0);
            eprintln!(
                "--------------------------------------------------------------------------------"
            );
        }

        // Collect free variables
        let free_vars = FreeVariables::analyze(&self);

        // Collect escaping functions
        let mut lambda_bindings = HashMap::default();
        self.collect_bindings(&mut lambda_bindings);
        let escaping = Escaping::find_escaping(&self, &lambda_bindings, &free_vars);
        let liveness = Liveness::analyze(&self, &free_vars, &escaping);

        let mut cells = HashSet::default();
        self.cells(&mut cells);
        let mut builder_context = FunctionBuilderContext::new();
        let mut ctx = module.make_context();
        ctx.func.signature = cont_sig();

        let val = Local::gensym();
        let name = val.get_func_name();
        let entry_func = module
            .declare_function(&name, Linkage::Export, &ctx.func.signature)
            .unwrap();
        let native_entry = module
            .declare_function(
                &format!("{name}_entry"),
                Linkage::Export,
                &rust_entry_sig(module, &ctx.func.signature),
            )
            .unwrap();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let params = {
            let block_params = builder.block_params(entry_block);
            [
                block_params[CONT_BARRIER_PARAM],
                block_params[CONT_OUT_PARAM],
            ]
        };

        let mut continuations = HashSet::default();

        let mut cu = CompilationUnit {
            builder,
            live: LiveValues::new(),
            runtime_funcs,
            params,
            continuations: &mut continuations,
            module,
            local_cont_scopes: HashMap::default(),
            local_cont_blocks: HashMap::default(),
            free_vars: &free_vars,
            escaping: &escaping,
            liveness: &liveness,
            proc_local: None,
            debug_info,
        };

        let mut deferred_procs = Vec::new();
        let mut deferred_local_conts = Vec::new();
        cu.cps_codegen(self, &mut deferred_procs, &mut deferred_local_conts);

        while let Some(local_cont) = deferred_local_conts.pop() {
            cu.local_cont_codegen(local_cont, &mut deferred_procs, &mut deferred_local_conts);
        }

        // Seal all of the local continuations
        for block in cu.local_cont_blocks.values() {
            cu.builder.seal_block(*block);
        }

        cu.builder.finalize(module.target_config());

        module.define_function(entry_func, &mut ctx).unwrap();
        module.clear_context(&mut ctx);
        rust_entry_codegen(module, entry_func, native_entry);

        while let Some(next) = deferred_procs.pop() {
            next.codegen(
                runtime_funcs,
                &cells,
                &escaping,
                &liveness,
                &mut continuations,
                &free_vars,
                module,
                debug_info,
                &mut deferred_procs,
            );
        }

        module.finalize_definitions().unwrap();

        unsafe {
            std::mem::transmute::<*const u8, ContinuationPtr>(
                module.get_finalized_function(native_entry),
            )
        }
    }
}

struct CompilationUnit<'m, 'a> {
    builder: FunctionBuilder<'m>,
    live: LiveValues,
    local_cont_scopes: HashMap<Local, LiveValues>,
    liveness: &'a Liveness,
    proc_local: Option<Local>,
    local_cont_blocks: HashMap<Local, Block>,
    runtime_funcs: &'a RuntimeFunctions,
    params: [Value; 2],
    continuations: &'a mut HashSet<Local>,
    free_vars: &'a FreeVariables,
    escaping: &'a Escaping,
    module: &'a mut JITModule,
    debug_info: &'a mut DebugInfo,
}

impl CompilationUnit<'_, '_> {
    fn get_barrier(&self) -> Value {
        self.params[0]
    }

    fn get_out(&self) -> Value {
        self.params[1]
    }

    fn cps_codegen(
        &mut self,
        cps: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let liveness = self.liveness;
        self.drop_dead_codegen(liveness.live_in(cps.local));
        match cps.inst {
            Inst::If(cond, success, failure) => {
                self.if_codegen(
                    &cond,
                    *success,
                    *failure,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            Inst::App(operator, args) => self.app_codegen(&operator, &args),
            Inst::PrimOp(PrimOp::Set, args, _, cexpr) => {
                self.store_codegen(
                    &args[1],
                    &args[0],
                    *cexpr,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            Inst::PrimOp(PrimOp::AllocCell, _, into, cexpr) => {
                self.alloc_cell_codegen(into, *cexpr, deferred_procs, deferred_local_conts);
            }
            Inst::PrimOp(PrimOp::Read, args, result, cexpr) => {
                self.read_codegen(
                    &args[0],
                    result,
                    *cexpr,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            Inst::PrimOp(PrimOp::Matches, args, bind_to, cexpr) => {
                let [pattern, expr] = args.as_slice() else {
                    unreachable!()
                };
                self.matches_codegen(
                    pattern,
                    expr,
                    bind_to,
                    *cexpr,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            Inst::PrimOp(PrimOp::ExpandTemplate, args, expand_to, cexpr) => {
                let [template, expansion_combiner, expansions @ ..] = args.as_slice() else {
                    unreachable!()
                };
                self.expand_template_codegen(
                    template,
                    expansion_combiner,
                    expansions,
                    expand_to,
                    *cexpr,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            Inst::PrimOp(PrimOp::ErrorNoPatternsMatch, _, _, _) => {
                self.error_no_patterns_match_codegen();
            }
            #[cfg(feature = "continuation-marks")]
            Inst::PrimOp(PrimOp::GetFrame, args, dest, cexpr) => {
                let [op, span] = args.as_slice() else {
                    unreachable!()
                };
                self.get_frame_codegen(
                    op,
                    span,
                    dest,
                    *cexpr,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            #[cfg(feature = "continuation-marks")]
            Inst::PrimOp(PrimOp::SetContinuationMark, args, _, cexpr) => {
                let [tag, val] = args.as_slice() else {
                    unreachable!()
                };
                self.set_continuation_mark_codegen(tag, val);
                self.cps_codegen(*cexpr, deferred_procs, deferred_local_conts);
            }
            Inst::PrimOp(
                primop @ (PrimOp::Not | PrimOp::IsNull | PrimOp::IsPair),
                args,
                result,
                cexpr,
            ) => {
                let [arg] = args.as_slice() else {
                    unreachable!()
                };
                self.bool_primop_codegen(
                    primop,
                    arg,
                    result,
                    *cexpr,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            Inst::PrimOp(primop, vals, result, cexpr) => {
                self.value_primop_codegen(
                    primop,
                    &vals,
                    result,
                    *cexpr,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            Inst::Fix(bindings, cexp) => {
                self.fix_codegen(bindings, *cexp, deferred_procs, deferred_local_conts);
            }
            Inst::Halt(value) => self.halt_codegen(&value),
        }
    }

    fn value_codegen(&mut self, value: &CpsValue) -> Value {
        let (cell, symbol) = match value {
            CpsValue::Var(Var::Local(var)) => {
                let cell = match *self.live.fetch(var) {
                    IrValue::Cell(cell) => cell,
                    IrValue::Ref(val) | IrValue::Owned(val) => return val,
                    IrValue::Dead => unreachable!("{var:?} is dead"),
                };
                let symbol = if let Some(sym) = var.name {
                    sym.0
                } else {
                    Symbol::intern(&format!("{}:{cell}", self.builder.func.name,)).0
                };
                (cell, symbol)
            }
            CpsValue::Var(Var::Global(global)) => {
                let mut globals_pool = Runtime::handle().0.globals_pool.lock();
                globals_pool.insert(global.clone());
                let cell = self.builder.ins().iconst(
                    types::I64,
                    SchemeValue::as_raw(&SchemeValue::from(global.val.clone())) as i64,
                );
                (cell, global.name.0)
            }
            CpsValue::Const(val)
                if let Some(proc) = val.cast::<Procedure>()
                    && let Some(known) = proc.to_known() =>
            {
                // Known functions get converted to i64 constants
                return self
                    .builder
                    .ins()
                    .iconst(types::I64, known.cast_to_usize() as i64);
            }
            CpsValue::Const(val) => {
                let mut constants_pool = Runtime::handle().0.constants_pool.lock();
                constants_pool.insert(val.clone());
                let raw = SchemeValue::as_raw(constants_pool.get(val));
                return self.builder.ins().iconst(types::I64, raw as i64);
            }
        };

        let read_cell = self
            .module
            .declare_func_in_func(self.runtime_funcs.read_cell, self.builder.func);
        let call = self.builder.ins().call(read_cell, &[cell]);
        let cell_value = self.builder.inst_results(call)[0];

        // Check if the cell is undefined:
        let cond = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::Equal, cell_value, UNDEFINED_VALUE as i64);

        let undefined_block = self.builder.create_block();
        let defined_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(cond, undefined_block, &[], defined_block, &[]);

        // Throw undefined variable error:
        self.builder.switch_to_block(undefined_block);
        self.builder.seal_block(undefined_block);

        self.drop_all_codegen();
        let symbol = self.builder.ins().iconst(types::I32, symbol as i64);
        let error_unbound_variable = self
            .module
            .declare_func_in_func(self.runtime_funcs.error_unbound_variable, self.builder.func);
        let call = self.builder.ins().call(error_unbound_variable, &[symbol]);
        let unbound_variable_error = self.builder.inst_results(call)[0];
        self.raise_codegen(unbound_variable_error);

        self.builder.switch_to_block(defined_block);
        self.builder.seal_block(defined_block);

        cell_value
    }

    fn drop_value_codegen(&mut self, val: Value) {
        let dropv = self
            .module
            .declare_func_in_func(self.runtime_funcs.dropv, self.builder.func);
        self.builder.ins().call(dropv, &[val]);
    }

    fn matches_codegen(
        &mut self,
        pattern: &CpsValue,
        expr: &CpsValue,
        binds: Local,
        cexpr: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let pattern = self.value_codegen(pattern);
        let expr = self.value_codegen(expr);
        let matches = self
            .module
            .declare_func_in_func(self.runtime_funcs.matches, self.builder.func);
        let call = self.builder.ins().call(matches, &[pattern, expr]);
        let match_result = self.builder.inst_results(call)[0];
        self.live.bind(binds, IrValue::Owned(match_result));
        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
    }

    #[allow(clippy::too_many_arguments)]
    fn expand_template_codegen(
        &mut self,
        template: &CpsValue,
        expansion_combiner: &CpsValue,
        expansions: &[CpsValue],
        dest: Local,
        cexpr: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let template = self.value_codegen(template);
        let expansion_combiner = self.value_codegen(expansion_combiner);

        // Put the expansions into an array:
        let expansions_slot = self.alloc_array(expansions.len());
        for (i, expansion) in expansions.iter().enumerate() {
            let expansion = self.value_codegen(expansion);
            self.array_store(expansions_slot, i, expansion);
        }

        let expansions_addr = self
            .builder
            .ins()
            .stack_addr(types::I64, expansions_slot, 0);
        let expansions_len = self
            .builder
            .ins()
            .iconst(types::I32, expansions.len() as i64);

        let error_slot = self.alloc_array(1);
        let error_addr = self.builder.ins().stack_addr(types::I64, error_slot, 0);

        let expand_template = self
            .module
            .declare_func_in_func(self.runtime_funcs.expand_template, self.builder.func);
        let call = self.builder.ins().call(
            expand_template,
            &[
                template,
                expansion_combiner,
                expansions_addr,
                expansions_len,
                error_addr,
            ],
        );
        let expanded = self.builder.inst_results(call)[0];

        let cond = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::Equal, expanded, UNDEFINED_VALUE as i64);
        let failure_block = self.builder.create_block();
        let success_block = self.builder.create_block();
        self.builder
            .ins()
            .brif(cond, failure_block, &[], success_block, &[]);

        self.builder.switch_to_block(failure_block);
        self.builder.seal_block(failure_block);
        let error_val = self.array_load(error_slot, 0);
        self.drop_all_codegen();
        self.raise_codegen(error_val);

        self.builder.switch_to_block(success_block);
        self.builder.seal_block(success_block);
        self.live.bind(dest, IrValue::Owned(expanded));
        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
    }

    fn bool_primop_codegen(
        &mut self,
        primop: PrimOp,
        arg: &CpsValue,
        dest: Local,
        cexpr: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let arg = self.value_codegen(arg);
        let cond = match primop {
            PrimOp::Not => self
                .builder
                .ins()
                .icmp_imm_s(IntCC::Equal, arg, FALSE_VALUE as i64),
            PrimOp::IsNull => self
                .builder
                .ins()
                .icmp_imm_s(IntCC::Equal, arg, NULL_VALUE as i64),
            PrimOp::IsPair => {
                let tag = self.builder.ins().band_imm_s(arg, TAG as i64);
                let is_pair_tag =
                    self.builder
                        .ins()
                        .icmp_imm_s(IntCC::Equal, tag, Tag::Pair as i64);
                let is_not_null =
                    self.builder
                        .ins()
                        .icmp_imm_s(IntCC::NotEqual, arg, NULL_VALUE as i64);
                self.builder.ins().band(is_pair_tag, is_not_null)
            }
            _ => unreachable!(),
        };
        let true_val = self.builder.ins().iconst(types::I64, TRUE_VALUE as i64);
        let false_val = self.builder.ins().iconst(types::I64, FALSE_VALUE as i64);
        let result = self.builder.ins().select(cond, true_val, false_val);
        self.live.bind(dest, IrValue::Ref(result));
        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
    }

    fn value_primop_codegen(
        &mut self,
        primop: PrimOp,
        vals: &[CpsValue],
        dest: Local,
        cexpr: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let arg_vals: Vec<Value> = vals.iter().map(|val| self.value_codegen(val)).collect();

        // If we have two arguments and the operator is a numeric primop, we can
        // generate faster code.
        if let [lhs, rhs] = arg_vals[..]
            && matches!(
                primop,
                PrimOp::Add
                    | PrimOp::Sub
                    | PrimOp::Mul
                    | PrimOp::Equal
                    | PrimOp::Lesser
                    | PrimOp::Greater
                    | PrimOp::LesserEqual
                    | PrimOp::GreaterEqual
            )
        {
            self.fixnum_binop_codegen(
                primop,
                lhs,
                rhs,
                dest,
                cexpr,
                deferred_procs,
                deferred_local_conts,
            );
        } else {
            let arg_vals = if matches!(primop, PrimOp::CallKnown0 | PrimOp::CallKnown1) {
                let liveness = self.liveness;
                let live_after = liveness.live_in(cexpr.local);
                let mut owned = vec![arg_vals[0]];
                owned.extend(self.take_args(&vals[1..], &arg_vals[1..], live_after));
                owned
            } else {
                arg_vals
            };
            let result = self.slow_value_primop_codegen(primop, &arg_vals);

            let bind = if primop.info().needs_drop {
                IrValue::Owned(result)
            } else {
                IrValue::Ref(result)
            };
            self.live.bind(dest, bind);

            self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
        }
    }

    fn slow_value_primop_codegen(&mut self, primop: PrimOp, arg_vals: &[Value]) -> Value {
        let primop_info = primop.info();

        let is_known = matches!(primop, PrimOp::CallKnown0 | PrimOp::CallKnown1);

        let mut args = if is_known {
            arg_vals[1..].to_vec()
        } else if primop_info.variadic {
            // Put the values into an array:
            let array = self.alloc_array(arg_vals.len());

            for (i, val) in arg_vals.iter().enumerate() {
                self.array_store(array, i, *val);
            }

            let vals_addr = self.builder.ins().stack_addr(types::I64, array, 0);
            let num_vals = self.builder.ins().iconst(types::I32, arg_vals.len() as i64);
            vec![vals_addr, num_vals]
        } else {
            arg_vals.to_vec()
        };

        // The runtime function to call, unless this is a known call:
        let runtime_func = match primop {
            PrimOp::Add => Some(self.runtime_funcs.add),
            PrimOp::Sub => Some(self.runtime_funcs.sub),
            PrimOp::Mul => Some(self.runtime_funcs.mul),
            PrimOp::Div => Some(self.runtime_funcs.div),
            PrimOp::Equal => Some(self.runtime_funcs.equal),
            PrimOp::Greater => Some(self.runtime_funcs.greater),
            PrimOp::GreaterEqual => Some(self.runtime_funcs.greater_equal),
            PrimOp::Lesser => Some(self.runtime_funcs.lesser),
            PrimOp::LesserEqual => Some(self.runtime_funcs.lesser_equal),
            PrimOp::Cons => Some(self.runtime_funcs.cons),
            PrimOp::List => Some(self.runtime_funcs.list),
            PrimOp::Append => Some(self.runtime_funcs.append),
            PrimOp::Car => Some(self.runtime_funcs.car),
            PrimOp::Cdr => Some(self.runtime_funcs.cdr),
            PrimOp::CallKnown0 | PrimOp::CallKnown1 => None,
            _ => unreachable!(),
        };

        // TODO: Having multiple of these is redundant.
        // Add a slot for the error if this function can error:
        let error_slot = primop_info.can_error.then(|| {
            let error_slot = self.alloc_array(1);
            if is_known {
                let undefined = self
                    .builder
                    .ins()
                    .iconst(types::I64, UNDEFINED_VALUE as i64);
                self.array_store(error_slot, 0, undefined);
            }
            let error_addr = self.builder.ins().stack_addr(types::I64, error_slot, 0);
            args.push(error_addr);
            error_slot
        });

        // Call the function:
        let primop_call = if let Some(runtime_func) = runtime_func {
            let runtime_func = self
                .module
                .declare_func_in_func(runtime_func, self.builder.func);
            self.builder.ins().call(runtime_func, args.as_slice())
        } else {
            let known_sig = {
                let mut sig = self.module.make_signature();
                for _ in 0..args.len() {
                    sig.params.push(AbiParam::new(types::I64));
                }
                sig.returns.push(AbiParam::new(types::I64));
                self.builder.import_signature(sig)
            };
            self.builder
                .ins()
                .call_indirect(known_sig, arg_vals[0], &args)
        };
        let result = self.builder.inst_results(primop_call)[0];

        // Check for error if we need to:
        if let Some(error_slot) = error_slot {
            // Check if the result is undefined:
            let cond = self
                .builder
                .ins()
                .icmp_imm_s(IntCC::Equal, result, UNDEFINED_VALUE as i64);

            let failure_block = self.builder.create_block();
            let success_block = self.builder.create_block();

            self.builder
                .ins()
                .brif(cond, failure_block, &[], success_block, &[]);

            // Throw the error:
            self.builder.switch_to_block(failure_block);
            self.builder.seal_block(failure_block);

            let error_val = self.array_load(error_slot, 0);
            self.drop_all_codegen();
            self.raise_codegen(error_val);

            // Otherwise continue with the correct value
            self.builder.switch_to_block(success_block);
            self.builder.seal_block(success_block);
        }

        result
    }

    #[allow(clippy::too_many_arguments)]
    fn fixnum_binop_codegen(
        &mut self,
        primop: PrimOp,
        lhs: Value,
        rhs: Value,
        dest: Local,
        cexpr: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        // Both operands are fixnums iff the low bit of their bitwise-and is set.
        let anded = self.builder.ins().band(lhs, rhs);
        let low_bit = self.builder.ins().band_imm_s(anded, 1);
        let both_fixnums = self.builder.ins().icmp_imm_s(IntCC::Equal, low_bit, 1);

        let fast_block = self.builder.create_block();
        let slow_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        self.builder
            .ins()
            .brif(both_fixnums, fast_block, &[], slow_block, &[]);

        // Fast path: both operands are fixnums.
        self.builder.switch_to_block(fast_block);
        self.builder.seal_block(fast_block);
        match primop {
            PrimOp::Equal
            | PrimOp::Lesser
            | PrimOp::Greater
            | PrimOp::LesserEqual
            | PrimOp::GreaterEqual => {
                // Since both operands have their lowest bit set, we can
                // compare them as i64s.
                let cc = match primop {
                    PrimOp::Equal => IntCC::Equal,
                    PrimOp::Lesser => IntCC::SignedLessThan,
                    PrimOp::Greater => IntCC::SignedGreaterThan,
                    PrimOp::LesserEqual => IntCC::SignedLessThanOrEqual,
                    PrimOp::GreaterEqual => IntCC::SignedGreaterThanOrEqual,
                    _ => unreachable!(),
                };
                let cmp = self.builder.ins().icmp(cc, lhs, rhs);
                let true_val = self.builder.ins().iconst(types::I64, TRUE_VALUE as i64);
                let false_val = self.builder.ins().iconst(types::I64, FALSE_VALUE as i64);
                let result = self.builder.ins().select(cmp, true_val, false_val);
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(result)]);
            }
            PrimOp::Add | PrimOp::Sub => {
                let lhs = self.builder.ins().sshr_imm_s(lhs, 1);
                let rhs = self.builder.ins().sshr_imm_s(rhs, 1);
                let value = match primop {
                    PrimOp::Add => self.builder.ins().iadd(lhs, rhs),
                    PrimOp::Sub => self.builder.ins().isub(lhs, rhs),
                    _ => unreachable!(),
                };

                // Check if we're in range of an i64
                let ge_min = self.builder.ins().icmp_imm_s(
                    IntCC::SignedGreaterThanOrEqual,
                    value,
                    FIXNUM_MIN,
                );
                let le_max =
                    self.builder
                        .ins()
                        .icmp_imm_s(IntCC::SignedLessThanOrEqual, value, FIXNUM_MAX);
                let in_range = self.builder.ins().band(ge_min, le_max);

                let encode_block = self.builder.create_block();
                let overflow_block = self.builder.create_block();
                self.builder
                    .ins()
                    .brif(in_range, encode_block, &[], overflow_block, &[]);

                // Convert back to a Value
                self.builder.switch_to_block(encode_block);
                self.builder.seal_block(encode_block);
                let shifted = self.builder.ins().ishl_imm_s(value, 1);
                let result = self.builder.ins().bor_imm_s(shifted, 1);
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(result)]);

                // The fixnum overflows 63 bits, allocate
                self.builder.switch_to_block(overflow_block);
                self.builder.seal_block(overflow_block);
                let i64_to_number = self
                    .module
                    .declare_func_in_func(self.runtime_funcs.i64_to_number, self.builder.func);
                let call = self.builder.ins().call(i64_to_number, &[value]);
                let result = self.builder.inst_results(call)[0];
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(result)]);
            }
            PrimOp::Mul => {
                let lhs = self.builder.ins().sshr_imm_s(lhs, 1);
                let rhs = self.builder.ins().sshr_imm_s(rhs, 1);

                let value = self.builder.ins().imul(lhs, rhs);
                let hi = self.builder.ins().smulhi(lhs, rhs);
                let sign = self.builder.ins().sshr_imm_s(value, 63);
                let fits_i64 = self.builder.ins().icmp(IntCC::Equal, hi, sign);

                let ge_min = self.builder.ins().icmp_imm_s(
                    IntCC::SignedGreaterThanOrEqual,
                    value,
                    FIXNUM_MIN,
                );
                let le_max =
                    self.builder
                        .ins()
                        .icmp_imm_s(IntCC::SignedLessThanOrEqual, value, FIXNUM_MAX);
                let in_fixnum = self.builder.ins().band(ge_min, le_max);
                let in_range = self.builder.ins().band(fits_i64, in_fixnum);

                let encode_block = self.builder.create_block();
                let overflow_block = self.builder.create_block();
                self.builder
                    .ins()
                    .brif(in_range, encode_block, &[], overflow_block, &[]);

                self.builder.switch_to_block(encode_block);
                self.builder.seal_block(encode_block);
                let shifted = self.builder.ins().ishl_imm_s(value, 1);
                let result = self.builder.ins().bor_imm_s(shifted, 1);
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(result)]);

                // Overflow: convert to a bignum
                self.builder.switch_to_block(overflow_block);
                self.builder.seal_block(overflow_block);
                let i128_to_number = self
                    .module
                    .declare_func_in_func(self.runtime_funcs.i128_to_number, self.builder.func);
                let call = self.builder.ins().call(i128_to_number, &[value, hi]);
                let result = self.builder.inst_results(call)[0];
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(result)]);
            }
            _ => unreachable!(),
        }

        self.builder.switch_to_block(slow_block);
        self.builder.seal_block(slow_block);
        let result = self.slow_value_primop_codegen(primop, &[lhs, rhs]);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(result)]);

        // Merge the two paths.
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        let result = self.builder.block_params(merge_block)[0];

        let bind = if primop.info().needs_drop {
            IrValue::Owned(result)
        } else {
            IrValue::Ref(result)
        };
        self.live.bind(dest, bind);

        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
    }

    #[cfg(feature = "continuation-marks")]
    fn get_frame_codegen(
        &mut self,
        op: &CpsValue,
        span: &CpsValue,
        dest: Local,
        cexpr: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let op = self.value_codegen(op);
        let span = self.value_codegen(span);
        let get_frame_func = self
            .module
            .declare_func_in_func(self.runtime_funcs.get_frame, self.builder.func);
        let get_frame_call = self.builder.ins().call(get_frame_func, &[op, span]);
        let result = self.builder.inst_results(get_frame_call)[0];
        self.live.bind(dest, IrValue::Owned(result));
        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
    }

    #[cfg(feature = "continuation-marks")]
    fn set_continuation_mark_codegen(&mut self, tag: &CpsValue, val: &CpsValue) {
        let tag = self.value_codegen(tag);
        let val = self.value_codegen(val);
        let barrier = self.get_barrier();
        let set_continuation_mark = self
            .module
            .declare_func_in_func(self.runtime_funcs.set_continuation_mark, self.builder.func);
        self.builder
            .ins()
            .call(set_continuation_mark, &[tag, val, barrier]);
    }

    fn error_no_patterns_match_codegen(&mut self) {
        self.drop_all_codegen();
        let error_no_patterns_match = self.module.declare_func_in_func(
            self.runtime_funcs.error_no_patterns_match,
            self.builder.func,
        );
        let call = self.builder.ins().call(error_no_patterns_match, &[]);
        let error = self.builder.inst_results(call)[0];
        self.raise_codegen(error);
    }

    fn alloc_cell_codegen(
        &mut self,
        var: Local,
        cexpr: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let alloc_cell = self
            .module
            .declare_func_in_func(self.runtime_funcs.alloc_cell, self.builder.func);
        let call = self.builder.ins().call(alloc_cell, &[]);
        let cell = self.builder.inst_results(call)[0];
        self.live.bind(var, IrValue::Cell(cell));
        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
    }

    fn read_codegen(
        &mut self,
        from: &CpsValue,
        result: Local,
        cexpr: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let value = self.value_codegen(from);
        let clonev = self
            .module
            .declare_func_in_func(self.runtime_funcs.clonev, self.builder.func);
        let call = self.builder.ins().call(clonev, &[value]);
        let value = self.builder.inst_results(call)[0];
        self.live.bind(result, IrValue::Owned(value));
        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
    }

    fn drop_all_codegen(&mut self) {
        for (_, val) in self.live.owned() {
            self.drop_value_codegen(val);
        }
    }

    fn drop_dead_codegen(&mut self, live: &HashSet<Local>) {
        for (var, val) in self.live.owned() {
            if !live.contains(&var) && Some(var) != self.proc_local {
                self.drop_value_codegen(val);
                self.live.kill(var);
            }
        }
    }

    fn clone_value_codegen(&mut self, val: Value) -> Value {
        let clonev = self
            .module
            .declare_func_in_func(self.runtime_funcs.clonev, self.builder.func);
        let call = self.builder.ins().call(clonev, &[val]);
        self.builder.inst_results(call)[0]
    }

    fn take_args(
        &mut self,
        args: &[CpsValue],
        vals: &[Value],
        live_after: &HashSet<Local>,
    ) -> Vec<Value> {
        let mut seen = HashSet::default();
        let mut moves = vec![false; args.len()];
        for (i, arg) in args.iter().enumerate().rev() {
            if let Some(local) = arg.to_local()
                && seen.insert(local)
                && !live_after.contains(&local)
                && matches!(self.live.fetch(&local), IrValue::Owned(_))
            {
                moves[i] = true;
            }
        }
        args.iter()
            .zip(vals)
            .zip(moves)
            .map(|((arg, val), moved)| {
                if moved {
                    self.live.kill(arg.to_local().unwrap());
                    *val
                } else {
                    self.clone_value_codegen(*val)
                }
            })
            .collect()
    }

    fn arg_slots_codegen(
        &mut self,
        args: &[CpsValue],
        live_after: &HashSet<Local>,
    ) -> [Value; NUM_ARG_SLOTS] {
        // Evaluate the arguments:
        let direct = args
            .iter()
            .take(NUM_ARG_SLOTS - 1)
            .map(|arg| self.value_codegen(arg))
            .collect::<Vec<_>>();

        // Store any remaining argument values in an array; they will be made
        // into a list.
        let extra_args = &args[args.len().min(NUM_ARG_SLOTS - 1)..];
        let extras_slot = if !extra_args.is_empty() {
            let extras_slot = self.alloc_array(extra_args.len());
            for (i, arg) in extra_args.iter().enumerate() {
                let val = self.value_codegen(arg);
                self.array_store(extras_slot, i, val);
            }
            Some(extras_slot)
        } else {
            None
        };

        let undefined = self
            .builder
            .ins()
            .iconst(types::I64, UNDEFINED_VALUE as i64);
        let null = self.builder.ins().iconst(types::I64, NULL_VALUE as i64);
        let mut slots = [undefined, undefined, undefined, undefined, null];
        let direct_args = &args[..direct.len()];
        let taken = self.take_args(direct_args, &direct, live_after);
        for (slot, val) in slots.iter_mut().zip(taken) {
            *slot = val;
        }

        if let Some(extras_slot) = extras_slot {
            let extras_addr = self.builder.ins().stack_addr(types::I64, extras_slot, 0);
            let extras_len = self
                .builder
                .ins()
                .iconst(types::I32, extra_args.len() as i64);
            let list = self
                .module
                .declare_func_in_func(self.runtime_funcs.list, self.builder.func);
            let call = self.builder.ins().call(list, &[extras_addr, extras_len]);
            slots[NUM_ARG_SLOTS - 1] = self.builder.inst_results(call)[0];
        }

        slots
    }

    fn app_codegen(&mut self, operator: &CpsValue, args: &[CpsValue]) {
        if let Some(local) = operator.to_local()
            && let Some(&block) = self.local_cont_blocks.get(&local)
        {
            // Operator is a local continuation
            self.jump_codegen(block, local, args);
        } else if let Some(local) = operator.to_local()
            && self.continuations.contains(&local)
        {
            // Operator is a regular continuation
            let mut args = self.arg_slots_codegen(args, &HashSet::default()).to_vec();
            self.drop_all_codegen();

            // Check if this is a JIT continuation, and if it is, tail call it.
            let ret_to_tramp_block = self.builder.create_block();
            let tail_call_block = self.builder.create_block();

            let out = self.get_out();
            let barrier = self.get_barrier();
            args.push(barrier);
            args.push(out);
            let pop_jit_cont = self
                .module
                .declare_func_in_func(self.runtime_funcs.pop_jit_continuation, self.builder.func);
            let call = self.builder.ins().call(pop_jit_cont, &[barrier]);
            let jit_cont = self.builder.inst_results(call)[0];

            // If jit_cont is null, we need to return to the trampoline
            self.builder
                .ins()
                .brif(jit_cont, tail_call_block, &[], ret_to_tramp_block, &[]);

            self.builder.switch_to_block(tail_call_block);
            self.builder.seal_block(tail_call_block);
            let sig = self.builder.import_signature(cont_sig());
            self.builder
                .ins()
                .return_call_indirect(sig, jit_cont, &args);

            self.builder.switch_to_block(ret_to_tramp_block);
            self.builder.seal_block(ret_to_tramp_block);

            let call_cont = self
                .module
                .declare_func_in_func(self.runtime_funcs.call_continuation, self.builder.func);
            self.builder.ins().call(call_cont, &args);
            self.builder.ins().return_(&[]);
        } else {
            // Operator is a function
            let barrier = self.get_barrier();
            let op = self.value_codegen(operator);

            let args = if let Some(first) = args.first()
                && let Some(local) = first.to_local()
                && self.continuations.contains(&local)
            {
                &args[1..]
            } else {
                args
            };

            let [arg1, arg2, arg3, arg4, argn] = self.arg_slots_codegen(args, &HashSet::default());
            let out = self.get_out();

            // Check if the operator can be tail called:
            let tail_callable = self
                .module
                .declare_func_in_func(self.runtime_funcs.tail_callable, self.builder.func);
            let call = self.builder.ins().call(tail_callable, &[op]);
            let func_ptr = self.builder.inst_results(call)[0];

            let tail_call_block = self.builder.create_block();
            let return_to_tramp_block = self.builder.create_block();
            self.builder
                .ins()
                .brif(func_ptr, tail_call_block, &[], return_to_tramp_block, &[]);

            self.builder.switch_to_block(return_to_tramp_block);
            self.builder.seal_block(return_to_tramp_block);
            let apply = self
                .module
                .declare_func_in_func(self.runtime_funcs.apply, self.builder.func);
            self.builder
                .ins()
                .call(apply, &[op, arg1, arg2, arg3, arg4, argn, barrier, out]);
            self.drop_all_codegen();
            self.builder.ins().return_(&[]);

            self.builder.switch_to_block(tail_call_block);
            self.builder.seal_block(tail_call_block);
            let op_owned = if let Some(local) = operator.to_local()
                && matches!(self.live.fetch(&local), IrValue::Owned(_))
            {
                self.live.kill(local);
                op
            } else {
                self.clone_value_codegen(op)
            };
            let op_owned = self.builder.ins().band_imm_s(op_owned, !(TAG as i64));
            self.drop_all_codegen();
            let sig = self.builder.import_signature(user_sig());
            self.builder.ins().return_call_indirect(
                sig,
                func_ptr,
                &[op_owned, arg1, arg2, arg3, arg4, argn, barrier, out],
            );
        }
    }

    fn jump_codegen(&mut self, to: Block, local: Local, args: &[CpsValue]) {
        let live_after = self.liveness.live_after_jump(local);

        let vals = args
            .iter()
            .map(|arg| self.value_codegen(arg))
            .collect::<Vec<_>>();

        // Transfer or clone the arguments.
        let block_args = self
            .take_args(args, &vals, live_after)
            .into_iter()
            .map(BlockArg::Value)
            .collect::<Vec<_>>();

        // Drop the values that do not survive the jump.
        self.drop_dead_codegen(live_after);
        self.builder.ins().jump(to, &block_args);
    }

    fn halt_codegen(&mut self, args: &CpsValue) {
        let val = self.value_codegen(args);
        let out = self.get_out();
        let halt = self
            .module
            .declare_func_in_func(self.runtime_funcs.halt, self.builder.func);
        self.builder.ins().call(halt, &[val, out]);
        self.drop_all_codegen();
        self.builder.ins().return_(&[]);
    }

    fn if_codegen(
        &mut self,
        cond: &CpsValue,
        success: Cps,
        failure: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let cond = self.value_codegen(cond);
        let cond = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::NotEqual, cond, FALSE_VALUE as i64);

        // Because our compiler is not particularly sophisticated right now, we
        // can guarantee that both branches terminate. Thus, no merge basic
        // block.
        let success_block = self.builder.create_block();
        let failure_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(cond, success_block, &[], failure_block, &[]);

        self.live.push_frame();
        self.builder.switch_to_block(success_block);
        self.builder.seal_block(success_block);
        self.cps_codegen(success, deferred_procs, deferred_local_conts);
        self.live.pop_frame();

        self.live.push_frame();
        self.builder.switch_to_block(failure_block);
        self.builder.seal_block(failure_block);
        self.cps_codegen(failure, deferred_procs, deferred_local_conts);
        self.live.pop_frame();
    }

    fn raise_codegen(&mut self, val: Value) {
        let barrier = self.get_barrier();
        let out = self.get_out();
        let raise = self
            .module
            .declare_func_in_func(self.runtime_funcs.raise_rt, self.builder.func);
        self.builder.ins().call(raise, &[val, barrier, out]);
        self.builder.ins().return_(&[]);
    }

    fn store_codegen(
        &mut self,
        from: &CpsValue,
        to: &CpsValue,
        cexpr: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let from = self.value_codegen(from);
        let to = match to {
            CpsValue::Var(Var::Global(global)) => {
                let mut globals_pool = Runtime::handle().0.globals_pool.lock();
                globals_pool.insert(global.clone());
                self.builder.ins().iconst(
                    types::I64,
                    SchemeValue::as_raw(&SchemeValue::from(global.val.clone())) as i64,
                )
            }
            CpsValue::Var(Var::Local(local)) => match self.live.fetch(local) {
                IrValue::Cell(to) => *to,
                _ => panic!("{to:?} is not a pointer"),
            },
            _ => panic!("{to:?} is not a pointer"),
        };
        let store = self
            .module
            .declare_func_in_func(self.runtime_funcs.store, self.builder.func);
        self.builder.ins().call(store, &[from, to]);
        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts)
    }

    fn alloc_array(&mut self, len: usize) -> StackSlot {
        self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            len as u32 * 8,
            0,
        ))
    }

    fn array_store(&mut self, slot: StackSlot, i: usize, val: Value) {
        self.builder
            .ins()
            .stack_store(types::I64, val, slot, i as i32 * 8);
    }

    fn array_load(&mut self, slot: StackSlot, i: usize) -> Value {
        self.builder
            .ins()
            .stack_load(types::I64, types::I64, slot, i as i32 * 8)
    }

    fn fix_codegen(
        &mut self,
        bindings: Vec<LambdaBinding>,
        cexp: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        // Collect the function, escaping continuation and local continuation
        // bundles.
        let mut proc_bundles = Vec::new();
        let mut escaping_cont_bundles = Vec::new();
        let mut local_cont_bundles = Vec::new();
        for binding in bindings.into_iter() {
            let is_func = binding.is_func();
            let bundle = ProcedureBundle::new(
                binding.val,
                binding.args,
                *binding.body,
                binding.span,
                self.continuations,
                self.free_vars,
                self.module,
            );
            if is_func {
                proc_bundles.push(bundle);
            } else if self.escaping.contains(binding.val) {
                self.continuations.insert(binding.val);
                escaping_cont_bundles.push(bundle);
            } else {
                local_cont_bundles.push(bundle);
            }
        }

        // The set of functions bound in this Fix. A binding's body may
        // reference any of these, including itself, so we cannot resolve them
        // until after all of the functions have been allocated.
        let fix_vals = proc_bundles.iter().map(|b| b.val).collect::<HashSet<_>>();

        // Allocate all of the functions. The functions are rooted and thus we
        // have exclusive mutable access to them.
        for bundle in &proc_bundles {
            self.alloc_procedure_codegen(bundle, &fix_vals);
        }

        // Patch any functions that were created by the fix primitive into the
        // environment of the functions.
        for bundle in &proc_bundles {
            self.patch_env_codegen(bundle, &fix_vals);
        }

        // Now that we no longer need mutable access, unroot the functions.
        for bundle in &proc_bundles {
            self.unroot_proc_codegen(bundle);
        }

        // Alloc escaping continuations after the procedures because the
        // former can reference the latter.
        for bundle in &escaping_cont_bundles {
            self.alloc_procedure_codegen(bundle, &HashSet::default());
        }

        for bundle in &local_cont_bundles {
            let cont_block = self.builder.create_block();
            self.local_cont_blocks.insert(bundle.val, cont_block);
            self.local_cont_scopes.insert(bundle.val, self.live.clone());
        }

        deferred_procs.extend(proc_bundles);
        deferred_procs.extend(escaping_cont_bundles);
        deferred_local_conts.extend(local_cont_bundles);

        self.cps_codegen(cexp, deferred_procs, deferred_local_conts);
    }

    fn local_cont_codegen(
        &mut self,
        bundle: ProcedureBundle,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        let cont_block = self.local_cont_blocks[&bundle.val];
        self.builder.switch_to_block(cont_block);

        // Restore the live values to their state at the continuation's
        // definition:
        self.live = self.local_cont_scopes[&bundle.val].clone();
        let liveness = self.liveness;
        let live_in = liveness.live_in(bundle.body.local);
        for (var, _) in self.live.owned() {
            if !live_in.contains(&var) && Some(var) != self.proc_local {
                self.live.kill(var);
            }
        }

        let mut param_vals = Vec::new();
        for arg in &bundle.args.args {
            let value = self.builder.append_block_param(cont_block, types::I64);
            param_vals.push((*arg, value));
        }

        for (arg, param_val) in param_vals {
            self.live.bind(arg, IrValue::Owned(param_val));
        }

        // No need to rebind env variables, they are already present
        self.cps_codegen(bundle.body, deferred_procs, deferred_local_conts);
    }

    fn alloc_procedure_codegen(&mut self, bundle: &ProcedureBundle, fix_vals: &HashSet<Local>) {
        // Construct the env array. Recursive references between functions get
        // a placeholder that will be overwritten once every function in the
        // group has been allocated.
        let env = self.alloc_array(bundle.env.len());
        for (i, env_var) in bundle.env.iter().enumerate() {
            let val = if bundle.args.continuation.is_some() && fix_vals.contains(env_var) {
                // Undefined
                self.builder.ins().iconst(types::I64, Tag::Record as i64)
            } else {
                match *self.live.fetch(env_var) {
                    IrValue::Cell(ptr) => ptr,
                    IrValue::Ref(val) | IrValue::Owned(val) => val,
                    IrValue::Dead => unreachable!("{env_var:?} is dead"),
                }
            };
            self.array_store(env, i, val);
        }

        let func_ref = self
            .module
            .declare_func_in_func(bundle.func_id, self.builder.func);
        let func_ptr = self.builder.ins().func_addr(types::I64, func_ref);
        let entry_ref = self
            .module
            .declare_func_in_func(bundle.rust_entry_id, self.builder.func);
        let entry_ptr = self.builder.ins().func_addr(types::I64, entry_ref);
        let env_addr = self.builder.ins().stack_addr(types::I64, env, 0);
        let env_len = self
            .builder
            .ins()
            .iconst(types::I32, bundle.env.len() as i64);
        let num_required = self
            .builder
            .ins()
            .iconst(types::I32, bundle.args.num_required() as i64);
        let is_variadic = self
            .builder
            .ins()
            .iconst(types::I8, bundle.args.variadic as i64);
        assert_eq!(std::mem::size_of::<bool>(), 1);

        let mut args = vec![
            func_ptr,
            entry_ptr,
            env_addr,
            env_len,
            num_required,
            is_variadic,
        ];

        if bundle.args.continuation.is_some() {
            args.push(if let Some(ref loc) = bundle.loc {
                let debug_info = Arc::new(ProcDebugInfo::new(
                    bundle.val.name,
                    bundle.args.args.clone(),
                    loc.clone(),
                ));
                let debug_info_ptr = Arc::as_ptr(&debug_info);
                self.debug_info.store_func_info(debug_info);
                self.builder.ins().iconst(types::I64, debug_info_ptr as i64)
            } else {
                self.builder.ins().iconst(types::I64, 0)
            });
            let make_user = self
                .module
                .declare_func_in_func(self.runtime_funcs.make_user, self.builder.func);
            let call = self.builder.ins().call(make_user, &args);
            let proc = self.builder.inst_results(call)[0];
            self.live.bind(bundle.val, IrValue::Owned(proc));
        } else {
            args.push(self.get_barrier());
            let push_cont = self
                .module
                .declare_func_in_func(self.runtime_funcs.push_continuation, self.builder.func);
            self.builder.ins().call(push_cont, &args);
        }
    }

    fn patch_env_codegen(&mut self, bundle: &ProcedureBundle, fix_vals: &HashSet<Local>) {
        // Continuations do not need to be patched.
        if bundle.args.continuation.is_none() {
            return;
        }

        let (IrValue::Ref(proc) | IrValue::Owned(proc)) = self.live.fetch(&bundle.val) else {
            unreachable!();
        };

        let patch_fn = self
            .module
            .declare_func_in_func(self.runtime_funcs.patch_env_slot, self.builder.func);

        for (i, env_var) in bundle.env.iter().enumerate() {
            if !fix_vals.contains(env_var) {
                continue;
            }
            let (IrValue::Ref(target) | IrValue::Owned(target)) = self.live.fetch(env_var) else {
                unreachable!();
            };
            let target = *target;
            let slot_idx = self.builder.ins().iconst(types::I32, i as i64);
            self.builder
                .ins()
                .call(patch_fn, &[*proc, slot_idx, target]);
        }
    }

    fn unroot_proc_codegen(&mut self, bundle: &ProcedureBundle) {
        if bundle.args.continuation.is_none() {
            return;
        }

        let (IrValue::Ref(proc) | IrValue::Owned(proc)) = self.live.fetch(&bundle.val) else {
            unreachable!();
        };

        let unroot_proc = self
            .module
            .declare_func_in_func(self.runtime_funcs.unroot_proc, self.builder.func);

        self.builder.ins().call(unroot_proc, &[*proc]);
    }
}

pub struct ProcedureBundle {
    func_id: FuncId,
    rust_entry_id: FuncId,
    val: Local,
    env: Vec<Local>,
    args: LambdaArgs,
    body: Cps,
    loc: Option<Span>,
}

const NUM_ARG_SLOTS: usize = crate::proc::MAX_DIRECT_ARGS + 1;

const PROC_PARAM: usize = 0;
const USER_ARG1_PARAM: usize = PROC_PARAM + 1;
const USER_BARRIER_PARAM: usize = USER_ARG1_PARAM + NUM_ARG_SLOTS;
const USER_OUT_PARAM: usize = USER_BARRIER_PARAM + 1;

fn user_sig() -> Signature {
    let mut sig = Signature::new(CallConv::Tail);
    sig.params.push(AbiParam::new(types::I64)); // Owned procedure value
    sig.params
        .extend((0..NUM_ARG_SLOTS).map(|_| AbiParam::new(types::I64))); // Arg1..Arg4, Argn
    sig.params.push(AbiParam::new(types::I64)); // ContBarrier
    sig.params.push(AbiParam::new(types::I64)); // Application out-pointer
    sig
}

const CONT_ARG1_PARAM: usize = 0;
const CONT_BARRIER_PARAM: usize = CONT_ARG1_PARAM + NUM_ARG_SLOTS;
const CONT_OUT_PARAM: usize = CONT_BARRIER_PARAM + 1;

fn cont_sig() -> Signature {
    let mut sig = Signature::new(CallConv::Tail);
    sig.params
        .extend((0..NUM_ARG_SLOTS).map(|_| AbiParam::new(types::I64))); // Arg1..Arg4, Argn
    sig.params.push(AbiParam::new(types::I64)); // ContBarrier
    sig.params.push(AbiParam::new(types::I64)); // Application out-pointer
    sig
}

fn rust_entry_sig(module: &JITModule, body: &Signature) -> Signature {
    let mut sig = module.make_signature();
    sig.params = body.params.clone();
    sig
}

impl ProcedureBundle {
    #[allow(clippy::too_many_arguments)]
    fn new(
        val: Local,
        args: LambdaArgs,
        body: Cps,
        loc: Option<Span>,
        continuations: &HashSet<Local>,
        free_vars: &FreeVariables,
        module: &mut JITModule,
    ) -> Self {
        let sig = Self::sig(&args);
        let func_id = module
            .declare_anonymous_function(&sig)
            .expect("Could not declare function");
        let rust_entry_id = module
            .declare_anonymous_function(&rust_entry_sig(module, &sig))
            .expect("Could not declare function");

        let env = free_vars
            .free_in(body.local)
            .difference(&args.iter().cloned().collect::<HashSet<_>>())
            .cloned()
            .filter(|var| !continuations.contains(var))
            .collect::<Vec<_>>();

        Self {
            func_id,
            rust_entry_id,
            val,
            env,
            args,
            body,
            loc,
        }
    }

    /// A lambda with a continuation parameter is a user function; one
    /// without is a continuation.
    fn is_user(args: &LambdaArgs) -> bool {
        args.continuation.is_some()
    }

    /// The signature of the generated function.
    fn sig(args: &LambdaArgs) -> Signature {
        if Self::is_user(args) {
            user_sig()
        } else {
            cont_sig()
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn codegen(
        self,
        runtime_funcs: &RuntimeFunctions,
        cells: &HashSet<Local>,
        escaping: &Escaping,
        liveness: &Liveness,
        continuations: &mut HashSet<Local>,
        free_vars: &FreeVariables,
        module: &mut JITModule,
        debug_info: &mut DebugInfo,
        deferred_procs: &mut Vec<Self>,
    ) {
        let mut builder_context = FunctionBuilderContext::new();
        let mut ctx = module.make_context();
        let is_user = Self::is_user(&self.args);
        ctx.func.signature = Self::sig(&self.args);

        let (arg1_param, barrier_param, out_param) = if is_user {
            (USER_ARG1_PARAM, USER_BARRIER_PARAM, USER_OUT_PARAM)
        } else {
            (CONT_ARG1_PARAM, CONT_BARRIER_PARAM, CONT_OUT_PARAM)
        };

        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
        let mut live = LiveValues::new();

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let params = {
            let block_params = builder.block_params(entry_block);
            [block_params[barrier_param], block_params[out_param]]
        };

        // Load environment:
        let proc_local = if is_user {
            // Load the environment from the self (proc) parameter:
            let proc = builder.block_params(entry_block)[PROC_PARAM];
            let proc = builder.ins().bor_imm_s(proc, Tag::Procedure as i64);
            let proc_local = Local::gensym();
            live.bind(proc_local, IrValue::Owned(proc));
            if !self.env.is_empty() {
                let proc_env = module.declare_func_in_func(runtime_funcs.proc_env, builder.func);
                let call = builder.ins().call(proc_env, &[proc]);
                let env_ptr = builder.inst_results(call)[0];
                for (i, env_var) in self.env.iter().enumerate() {
                    let var = builder.ins().load(
                        types::I64,
                        MemFlagsData::new(),
                        env_ptr,
                        (i * 8) as i32,
                    );
                    let var = if cells.contains(env_var) {
                        IrValue::Cell(var)
                    } else {
                        IrValue::Ref(var)
                    };
                    live.bind(*env_var, var);
                }
            }

            Some(proc_local)
        } else {
            // Load the environment by repeatedly calling pop_env:
            let pop_env = module.declare_func_in_func(runtime_funcs.pop_env, builder.func);
            let mut vals = Vec::with_capacity(self.env.len());
            for _ in 0..self.env.len() {
                let call = builder.ins().call(pop_env, &[params[0]]);
                vals.push(builder.inst_results(call)[0]);
            }
            for (env_var, var) in self.env.iter().zip(vals.into_iter().rev()) {
                let var = if cells.contains(env_var) {
                    IrValue::Cell(var)
                } else {
                    IrValue::Owned(var)
                };
                live.bind(*env_var, var);
            }

            None
        };

        // Function prologue: check and unpack the arguments
        let arg_slots: [Value; NUM_ARG_SLOTS] = {
            let block_params = builder.block_params(entry_block);
            std::array::from_fn(|i| block_params[arg1_param + i])
        };

        let num_required = self.args.num_required();
        let variadic = self.args.variadic;

        if num_required <= crate::proc::MAX_DIRECT_ARGS {
            // Fast path: no argument touches the argn list.
            let mut err_block: Option<Block> = None;
            let mut check = |cond: Value, builder: &mut FunctionBuilder| {
                let err = *err_block.get_or_insert_with(|| builder.create_block());
                let ok = builder.create_block();
                builder.ins().brif(cond, ok, &[], err, &[]);
                builder.switch_to_block(ok);
                builder.seal_block(ok);
            };

            for slot in arg_slots.iter().take(num_required) {
                let defined =
                    builder
                        .ins()
                        .icmp_imm_s(IntCC::NotEqual, *slot, UNDEFINED_VALUE as i64);
                check(defined, &mut builder);
            }

            if !variadic {
                let no_extra = if num_required < crate::proc::MAX_DIRECT_ARGS {
                    builder.ins().icmp_imm_s(
                        IntCC::Equal,
                        arg_slots[num_required],
                        UNDEFINED_VALUE as i64,
                    )
                } else {
                    builder.ins().icmp_imm_s(
                        IntCC::Equal,
                        arg_slots[crate::proc::MAX_DIRECT_ARGS],
                        NULL_VALUE as i64,
                    )
                };
                check(no_extra, &mut builder);
            }

            if let Some(err_block) = err_block {
                let ok_block = builder.current_block().unwrap();
                builder.switch_to_block(err_block);
                builder.seal_block(err_block);
                builder.set_cold_block(err_block);
                let dropv = module.declare_func_in_func(runtime_funcs.dropv, builder.func);
                for (_, val) in live.owned() {
                    builder.ins().call(dropv, &[val]);
                }
                let num_required_v = builder.ins().iconst(types::I32, num_required as i64);
                let raise_wrong_num_args =
                    module.declare_func_in_func(runtime_funcs.raise_wrong_num_args, builder.func);
                builder.ins().call(
                    raise_wrong_num_args,
                    &[
                        arg_slots[0],
                        arg_slots[1],
                        arg_slots[2],
                        arg_slots[3],
                        arg_slots[4],
                        num_required_v,
                        params[0],
                        params[1],
                    ],
                );
                builder.ins().return_(&[]);

                builder.switch_to_block(ok_block);
            }

            for (i, arg) in self.args.args.iter().take(num_required).enumerate() {
                live.bind(*arg, IrValue::Owned(arg_slots[i]));
            }

            if variadic {
                // Collect the rest args into a list:
                let cons = module.declare_func_in_func(runtime_funcs.cons, builder.func);
                let dropv = module.declare_func_in_func(runtime_funcs.dropv, builder.func);

                let mut rest = arg_slots[crate::proc::MAX_DIRECT_ARGS];
                for i in (num_required..crate::proc::MAX_DIRECT_ARGS).rev() {
                    let slot = arg_slots[i];
                    let is_undef =
                        builder
                            .ins()
                            .icmp_imm_s(IntCC::Equal, slot, UNDEFINED_VALUE as i64);
                    let cons_block = builder.create_block();
                    let cont_block = builder.create_block();
                    builder.append_block_param(cont_block, types::I64);
                    builder.ins().brif(
                        is_undef,
                        cont_block,
                        &[BlockArg::Value(rest)],
                        cons_block,
                        &[],
                    );

                    builder.switch_to_block(cons_block);
                    builder.seal_block(cons_block);
                    let call = builder.ins().call(cons, &[slot, rest]);
                    let consed = builder.inst_results(call)[0];
                    builder.ins().call(dropv, &[slot]);
                    builder.ins().call(dropv, &[rest]);
                    builder.ins().jump(cont_block, &[BlockArg::Value(consed)]);

                    builder.switch_to_block(cont_block);
                    builder.seal_block(cont_block);
                    rest = builder.block_params(cont_block)[0];
                }

                live.bind(self.args.args[num_required], IrValue::Owned(rest));
            }
        } else {
            // Slow path: some required arguments live in the argn list.
            // TODO: car/cdr functions that do not error.
            let car = module.declare_func_in_func(runtime_funcs.car, builder.func);
            let cdr = module.declare_func_in_func(runtime_funcs.cdr, builder.func);
            let clonev = module.declare_func_in_func(runtime_funcs.clonev, builder.func);
            let dropv = module.declare_func_in_func(runtime_funcs.dropv, builder.func);

            let scratch_slot = builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                8,
                0,
            ));
            let scratch_addr = builder.ins().stack_addr(types::I64, scratch_slot, 0);

            let raise_block = builder.create_block();

            // Check every direct slot for undefined:
            for slot in arg_slots.iter().take(crate::proc::MAX_DIRECT_ARGS) {
                let defined =
                    builder
                        .ins()
                        .icmp_imm_s(IntCC::NotEqual, *slot, UNDEFINED_VALUE as i64);
                let ok_block = builder.create_block();
                builder.ins().brif(defined, ok_block, &[], raise_block, &[]);
                builder.switch_to_block(ok_block);
                builder.seal_block(ok_block);
            }

            // Extract the required arguments from the argn list
            let argn = arg_slots[crate::proc::MAX_DIRECT_ARGS];
            let mut extracted = Vec::new();
            let clone_call = builder.ins().call(clonev, &[argn]);
            let mut cur = builder.inst_results(clone_call)[0];
            for _ in crate::proc::MAX_DIRECT_ARGS..num_required {
                let is_null = builder
                    .ins()
                    .icmp_imm_s(IntCC::Equal, cur, NULL_VALUE as i64);
                let err_block = builder.create_block();
                let cont_block = builder.create_block();
                builder.ins().brif(is_null, err_block, &[], cont_block, &[]);

                // Too few arguments: drop the values extracted so far.
                builder.switch_to_block(err_block);
                builder.seal_block(err_block);
                builder.set_cold_block(err_block);
                for val in &extracted {
                    builder.ins().call(dropv, &[*val]);
                }
                builder.ins().jump(raise_block, &[]);

                builder.switch_to_block(cont_block);
                builder.seal_block(cont_block);
                let call = builder.ins().call(car, &[cur, scratch_addr]);
                extracted.push(builder.inst_results(call)[0]);
                let call = builder.ins().call(cdr, &[cur, scratch_addr]);
                let next = builder.inst_results(call)[0];
                builder.ins().call(dropv, &[cur]);
                cur = next;
            }

            let rest = if variadic {
                // The remainder of the list is the rest argument.
                Some(cur)
            } else {
                // The list must be exhausted if the function is not variadic
                let is_null = builder
                    .ins()
                    .icmp_imm_s(IntCC::Equal, cur, NULL_VALUE as i64);
                let err_block = builder.create_block();
                let done_block = builder.create_block();
                builder.ins().brif(is_null, done_block, &[], err_block, &[]);

                // Too many arguments: drop the rest of the list and the
                // extracted values, and raise.
                builder.switch_to_block(err_block);
                builder.seal_block(err_block);
                builder.set_cold_block(err_block);
                builder.ins().call(dropv, &[cur]);
                for val in &extracted {
                    builder.ins().call(dropv, &[*val]);
                }
                builder.ins().jump(raise_block, &[]);

                builder.switch_to_block(done_block);
                builder.seal_block(done_block);
                None
            };

            let body_block = builder.create_block();
            builder.ins().jump(body_block, &[]);

            builder.switch_to_block(raise_block);
            builder.seal_block(raise_block);
            builder.set_cold_block(raise_block);
            for (_, val) in live.owned() {
                builder.ins().call(dropv, &[val]);
            }
            let num_required_v = builder.ins().iconst(types::I32, num_required as i64);
            let raise_wrong_num_args =
                module.declare_func_in_func(runtime_funcs.raise_wrong_num_args, builder.func);
            builder.ins().call(
                raise_wrong_num_args,
                &[
                    arg_slots[0],
                    arg_slots[1],
                    arg_slots[2],
                    arg_slots[3],
                    arg_slots[4],
                    num_required_v,
                    params[0],
                    params[1],
                ],
            );
            builder.ins().return_(&[]);

            builder.switch_to_block(body_block);
            builder.seal_block(body_block);

            // Everything extracted is a clone; release the original list.
            builder.ins().call(dropv, &[argn]);

            for (i, arg) in self.args.args.iter().enumerate() {
                let var = if i < crate::proc::MAX_DIRECT_ARGS {
                    arg_slots[i]
                } else if i < num_required {
                    extracted[i - crate::proc::MAX_DIRECT_ARGS]
                } else {
                    rest.unwrap()
                };
                live.bind(*arg, IrValue::Owned(var));
            }
        }

        continuations.extend(self.args.continuation);

        let mut cu = CompilationUnit {
            builder,
            live,
            continuations,
            local_cont_scopes: HashMap::default(),
            local_cont_blocks: HashMap::default(),
            escaping,
            liveness,
            proc_local,
            runtime_funcs,
            params,
            module,
            free_vars,
            debug_info,
        };

        let mut deferred_local_conts = Vec::new();
        cu.cps_codegen(self.body, deferred_procs, &mut deferred_local_conts);

        while let Some(local_cont) = deferred_local_conts.pop() {
            cu.local_cont_codegen(local_cont, deferred_procs, &mut deferred_local_conts);
        }

        // Seal all of the local continuations
        for block in cu.local_cont_blocks.values() {
            cu.builder.seal_block(*block);
        }

        cu.builder.finalize(module.target_config());

        module.define_function(self.func_id, &mut ctx).unwrap();
        module.clear_context(&mut ctx);
        rust_entry_codegen(module, self.func_id, self.rust_entry_id);
    }
}
