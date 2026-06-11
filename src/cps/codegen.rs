//! Cranelift Codegen from CPS.

use cranelift::{
    codegen::ir::{BlockArg, StackSlot, entities::Value},
    prelude::*,
};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use std::sync::Arc;

use crate::{
    cps::{
        Value as CpsValue,
        analysis::{Escaping, FreeVariables},
    },
    proc::{ContinuationPtr, FuncPtr, ProcDebugInfo, Procedure},
    runtime::{DebugInfo, Runtime},
    value::{FALSE_VALUE, NULL_VALUE, TAG, TRUE_VALUE, Tag, Value as SchemeValue},
};

use super::*;

#[derive(Copy, Clone, Debug)]
enum IrValue {
    Cell(Value),
    Value(Value),
}

struct Rebinds {
    rebinds: HashMap<Local, IrValue>,
}

impl Rebinds {
    fn rebind(&mut self, old_var: Local, new_var: IrValue) {
        self.rebinds.insert(old_var, new_var);
    }

    fn fetch_bind(&self, var: &Local) -> &IrValue {
        self.rebinds
            .get(var)
            .unwrap_or_else(|| panic!("could not find {var:?}"))
    }

    fn new() -> Self {
        Self {
            rebinds: HashMap::default(),
        }
    }
}

#[derive(derive_builder::Builder)]
pub(crate) struct RuntimeFunctions {
    apply: FuncId,
    halt: FuncId,
    make_user: FuncId,
    make_continuation: FuncId,
    patch_env_slot: FuncId,
    unroot_proc: FuncId,
    alloc_cell: FuncId,
    read_cell: FuncId,
    store: FuncId,
    error_unbound_variable: FuncId,
    clonev: FuncId,
    dropv: FuncId,
    raise_rt: FuncId,

    // Known function operations:
    call_known_0x1: FuncId,
    call_known_1x0: FuncId,
    call_known_1x1: FuncId,
    call_known_2x0: FuncId,
    call_known_2x1: FuncId,
    call_known_3x0: FuncId,
    call_known_3x1: FuncId,

    // Syntax primops:
    matches: FuncId,
    expand_template: FuncId,
    error_no_patterns_match: FuncId,

    // List primops:
    cons: FuncId,
    list: FuncId,
    car: FuncId,
    cdr: FuncId,

    // Frame primops:
    get_frame: FuncId,

    // Continuation mark primops:
    set_continuation_mark: FuncId,

    // Math primops:
    add: FuncId,
    sub: FuncId,
    mul: FuncId,
    div: FuncId,
    equal: FuncId,
    greater: FuncId,
    greater_equal: FuncId,
    lesser: FuncId,
    lesser_equal: FuncId,
}

impl Cps {
    pub(crate) fn into_procedure(
        self,
        runtime: Runtime,
        escaping: Escaping,
        runtime_funcs: &RuntimeFunctions,
        module: &mut JITModule,
        debug_info: &mut DebugInfo,
    ) -> Procedure {
        if std::env::var("SCHEME_RS_DEBUG").is_ok() {
            eprintln!("Compiling:");
            self.pretty_print(0);
            eprintln!();
        }

        let mut free_vars = FreeVariables::default();
        free_vars.find_free_vars(&self);

        let mut cells = HashSet::default();
        self.cells(&mut cells);
        let mut builder_context = FunctionBuilderContext::new();
        let mut ctx = module.make_context();

        make_sig(&mut ctx.func.signature, false);

        let val = Local::gensym();
        let name = val.get_func_name();
        let entry_func = module
            .declare_function(&name, Linkage::Export, &ctx.func.signature)
            .unwrap();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let mut allocs_at_local_conts = HashMap::default();
        let max_allocs = self.max_allocs(0, &escaping, &mut allocs_at_local_conts);

        let vals = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            max_allocs as u32 * 8,
            0,
        ));

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let params = {
            let block_params = builder.block_params(entry_block);
            [
                block_params[RUNTIME_PARAM],
                block_params[CONT_BARRIER_PARAM],
            ]
        };

        let mut cu = CompilationUnit {
            runtime: runtime.clone(),
            builder,
            // Top level cannot inherit environmental variables, by defintion.
            rebinds: Rebinds::new(),
            allocs: vals,
            curr_allocs: 0,
            runtime_funcs,
            params,
            module,
            allocs_at_local_cont: &allocs_at_local_conts,
            local_cont_blocks: HashMap::default(),
            free_vars: &mut free_vars,
            escaping: &escaping,
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

        cu.builder.finalize();

        module.define_function(entry_func, &mut ctx).unwrap();
        module.clear_context(&mut ctx);

        while let Some(next) = deferred_procs.pop() {
            next.codegen(
                runtime_funcs,
                &cells,
                &escaping,
                &mut free_vars,
                module,
                debug_info,
                &mut deferred_procs,
            );
        }

        module.finalize_definitions().unwrap();

        let func = unsafe {
            std::mem::transmute::<*const u8, ContinuationPtr>(
                module.get_finalized_function(entry_func),
            )
        };

        Procedure::new(runtime, Vec::new(), FuncPtr::Continuation(func), 0, true)
    }
}

struct CompilationUnit<'m, 'a> {
    runtime: Runtime,
    builder: FunctionBuilder<'m>,
    rebinds: Rebinds,
    allocs: StackSlot,
    curr_allocs: usize,
    allocs_at_local_cont: &'a HashMap<Local, usize>,
    local_cont_blocks: HashMap<Local, Block>,
    runtime_funcs: &'a RuntimeFunctions,
    params: [Value; 2],
    free_vars: &'a mut FreeVariables,
    escaping: &'a Escaping,
    module: &'a mut JITModule,
    debug_info: &'a mut DebugInfo,
}

impl CompilationUnit<'_, '_> {
    fn push_alloc(&mut self, val: Value) {
        self.builder
            .ins()
            .stack_store(val, self.allocs, self.curr_allocs as i32 * 8);
        self.curr_allocs += 1;
    }

    fn get_runtime(&self) -> Value {
        self.params[0]
    }

    fn get_barrier(&self) -> Value {
        self.params[1]
    }

    fn cps_codegen(
        &mut self,
        cps: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        match cps {
            Cps::If(cond, success, failure) => {
                self.if_codegen(
                    &cond,
                    *success,
                    *failure,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            Cps::App(operator, args) => self.app_codegen(&operator, &args),
            Cps::PrimOp(PrimOp::Set, args, _, cexpr) => {
                self.store_codegen(
                    &args[1],
                    &args[0],
                    *cexpr,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            Cps::PrimOp(PrimOp::AllocCell, _, into, cexpr) => {
                self.alloc_cell_codegen(into, *cexpr, deferred_procs, deferred_local_conts);
            }
            Cps::PrimOp(PrimOp::Matches, args, bind_to, cexpr) => {
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
            Cps::PrimOp(PrimOp::ExpandTemplate, args, expand_to, cexpr) => {
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
            Cps::PrimOp(PrimOp::ErrorNoPatternsMatch, _, _, _) => {
                self.error_no_patterns_match_codegen();
            }
            Cps::PrimOp(PrimOp::GetFrame, args, dest, cexpr) => {
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
            Cps::PrimOp(PrimOp::SetContinuationMark, args, _, cexpr) => {
                let [tag, val] = args.as_slice() else {
                    unreachable!()
                };
                self.set_continuation_mark_codegen(tag, val);
                self.cps_codegen(*cexpr, deferred_procs, deferred_local_conts);
            }
            Cps::PrimOp(
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
            Cps::PrimOp(primop, vals, result, cexpr) => {
                self.value_primop_codegen(
                    primop,
                    &vals,
                    result,
                    *cexpr,
                    deferred_procs,
                    deferred_local_conts,
                );
            }
            Cps::Fix(bindings, cexp) => {
                self.fix_codegen(bindings, *cexp, deferred_procs, deferred_local_conts);
            }
            Cps::Halt(value) => self.halt_codegen(&value),
        }
    }

    fn value_codegen(&mut self, value: &CpsValue) -> Value {
        let (cell, symbol) = match value {
            CpsValue::Var(Var::Local(var)) => {
                let cell = match *self.rebinds.fetch_bind(var) {
                    IrValue::Cell(cell) => cell,
                    IrValue::Value(int) => return int,
                };
                let symbol = if let Some(sym) = var.name {
                    sym.0
                } else {
                    Symbol::intern(&format!("{}:{cell}", self.builder.func.name,)).0
                };
                (cell, symbol)
            }
            CpsValue::Var(Var::Global(global)) => {
                let mut runtime_write = self.runtime.0.write();
                runtime_write.globals_pool.insert(global.clone());
                let cell = self.builder.ins().iconst(
                    types::I64,
                    SchemeValue::as_raw(&SchemeValue::from(global.val.clone())) as i64,
                );
                (cell, global.name.0)
            }
            CpsValue::Const(val)
                if let Some(proc) = val.cast_to_scheme_type::<Procedure>()
                    && let Some(known) = proc.to_known() =>
            {
                // Known functions get converted to i64 constants
                return self
                    .builder
                    .ins()
                    .iconst(types::I64, known.cast_to_usize() as i64);
            }
            CpsValue::Const(val) => {
                let mut runtime_write = self.runtime.0.write();
                runtime_write.constants_pool.insert(val.clone());
                let raw = SchemeValue::as_raw(runtime_write.constants_pool.get(val));
                return self.builder.ins().iconst(types::I64, raw as i64);
            }
        };

        let read_cell = self
            .module
            .declare_func_in_func(self.runtime_funcs.read_cell, self.builder.func);
        let call = self.builder.ins().call(read_cell, &[cell]);
        let cell_value = self.builder.inst_results(call)[0];

        // Check if the cell is undefined:
        let cond = self.builder.ins().icmp_imm(IntCC::Equal, cell_value, 0);

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
        self.rebinds.rebind(binds, IrValue::Value(match_result));
        self.push_alloc(match_result);
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
            ],
        );
        let expanded = self.builder.inst_results(call)[0];
        self.rebinds.rebind(dest, IrValue::Value(expanded));
        self.push_alloc(expanded);
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
                .icmp_imm(IntCC::Equal, arg, FALSE_VALUE as i64),
            PrimOp::IsNull => self
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, arg, NULL_VALUE as i64),
            PrimOp::IsPair => {
                let tag = self.builder.ins().band_imm(arg, TAG as i64);
                let is_pair_tag = self
                    .builder
                    .ins()
                    .icmp_imm(IntCC::Equal, tag, Tag::Pair as i64);
                let is_not_null =
                    self.builder
                        .ins()
                        .icmp_imm(IntCC::NotEqual, arg, NULL_VALUE as i64);
                self.builder.ins().band(is_pair_tag, is_not_null)
            }
            _ => unreachable!(),
        };
        let true_val = self.builder.ins().iconst(types::I64, TRUE_VALUE as i64);
        let false_val = self.builder.ins().iconst(types::I64, FALSE_VALUE as i64);
        let result = self.builder.ins().select(cond, true_val, false_val);
        self.rebinds.rebind(dest, IrValue::Value(result));
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
        let primop_info = primop.info();

        let mut args = if primop_info.variadic {
            // Put the values into an array:
            let args = self.alloc_array(vals.len());

            for (i, val) in vals.iter().enumerate() {
                let val = self.value_codegen(val);
                self.array_store(args, i, val);
            }

            let vals_addr = self.builder.ins().stack_addr(types::I64, args, 0);
            let num_vals = self.builder.ins().iconst(types::I32, vals.len() as i64);
            vec![vals_addr, num_vals]
        } else {
            vals.iter().map(|val| self.value_codegen(val)).collect()
        };

        // Call the respective runtime function:
        let runtime_func = match primop {
            PrimOp::Add => self.runtime_funcs.add,
            PrimOp::Sub => self.runtime_funcs.sub,
            PrimOp::Mul => self.runtime_funcs.mul,
            PrimOp::Div => self.runtime_funcs.div,
            PrimOp::Equal => self.runtime_funcs.equal,
            PrimOp::Greater => self.runtime_funcs.greater,
            PrimOp::GreaterEqual => self.runtime_funcs.greater_equal,
            PrimOp::Lesser => self.runtime_funcs.lesser,
            PrimOp::LesserEqual => self.runtime_funcs.lesser_equal,
            PrimOp::Cons => self.runtime_funcs.cons,
            PrimOp::List => self.runtime_funcs.list,
            PrimOp::Car => self.runtime_funcs.car,
            PrimOp::Cdr => self.runtime_funcs.cdr,
            PrimOp::CallKnown0 => match vals.len() {
                2 => self.runtime_funcs.call_known_1x0,
                3 => self.runtime_funcs.call_known_2x0,
                4 => self.runtime_funcs.call_known_3x0,
                _ => unreachable!(),
            },
            PrimOp::CallKnown1 => match vals.len() {
                1 => self.runtime_funcs.call_known_0x1,
                2 => self.runtime_funcs.call_known_1x1,
                3 => self.runtime_funcs.call_known_2x1,
                4 => self.runtime_funcs.call_known_3x1,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        let runtime_func = self
            .module
            .declare_func_in_func(runtime_func, self.builder.func);

        // TODO: Having multiple of these is redundant.
        // Add a slot for the error if this function can error:
        let error_slot = primop_info.can_error.then(|| {
            let error_slot = self.alloc_array(1);
            let error_addr = self.builder.ins().stack_addr(types::I64, error_slot, 0);
            args.push(error_addr);
            error_slot
        });

        // Call the function:
        let primop_call = self.builder.ins().call(runtime_func, args.as_slice());
        let result = self.builder.inst_results(primop_call)[0];

        // Check for error if we need to:
        if let Some(error_slot) = error_slot {
            // Check if the result is undefined:
            let cond = self.builder.ins().icmp_imm(IntCC::Equal, result, 0);

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

        self.rebinds.rebind(dest, IrValue::Value(result));

        if primop_info.needs_drop {
            self.push_alloc(result);
        }

        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
    }

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
        self.rebinds.rebind(dest, IrValue::Value(result));
        self.push_alloc(result);
        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
    }

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
        self.rebinds.rebind(var, IrValue::Cell(cell));
        self.push_alloc(cell);
        self.cps_codegen(cexpr, deferred_procs, deferred_local_conts);
    }

    fn drop_all_codegen(&mut self) {
        if self.curr_allocs > 0 {
            let vals = self.builder.ins().stack_addr(types::I64, self.allocs, 0);
            let num_vals = self
                .builder
                .ins()
                .iconst(types::I32, self.curr_allocs as i64);
            let dropv = self
                .module
                .declare_func_in_func(self.runtime_funcs.dropv, self.builder.func);
            self.builder.ins().call(dropv, &[vals, num_vals]);
        }
    }

    fn drop_n_codegen(&mut self, n: usize) {
        if n > 0 {
            let vals = self.builder.ins().stack_addr(
                types::I64,
                self.allocs,
                (self.curr_allocs - n) as i32 * 8,
            );
            let num_vals = self.builder.ins().iconst(types::I32, n as i64);
            let dropv = self
                .module
                .declare_func_in_func(self.runtime_funcs.dropv, self.builder.func);
            self.builder.ins().call(dropv, &[vals, num_vals]);
        }
    }

    fn app_codegen(&mut self, operator: &CpsValue, args: &[CpsValue]) {
        if let Some(local) = operator.to_local()
            && let Some(num_allocs_at_dest) = self.allocs_at_local_cont.get(&local)
        {
            self.jump_codegen(self.local_cont_blocks[&local], *num_allocs_at_dest, args);
            return;
        }

        let runtime = self.get_runtime();
        let barrier = self.get_barrier();
        let operator = self.value_codegen(operator);

        // Allocate space for the args to be passed to make_application
        let args_slot = self.alloc_array(args.len());
        for (i, arg) in args.iter().enumerate() {
            let val = self.value_codegen(arg);
            self.array_store(args_slot, i, val);
        }

        let args_addr = self.builder.ins().stack_addr(types::I64, args_slot, 0);
        let args_len = self.builder.ins().iconst(types::I32, args.len() as i64);
        let apply = self
            .module
            .declare_func_in_func(self.runtime_funcs.apply, self.builder.func);
        let call = self
            .builder
            .ins()
            .call(apply, &[runtime, operator, args_addr, args_len, barrier]);
        let app = self.builder.inst_results(call)[0];
        self.drop_all_codegen();
        self.builder.ins().return_(&[app]);
    }

    fn jump_codegen(&mut self, to: Block, num_allocs_at_dest: usize, args: &[CpsValue]) {
        assert!(
            self.curr_allocs >= num_allocs_at_dest,
            "cannot jump to a continuation with more allocations"
        );
        let clone = self
            .module
            .declare_func_in_func(self.runtime_funcs.clonev, self.builder.func);
        let mut cloned_args = Vec::new();
        for arg in args {
            let arg_val = self.value_codegen(arg);
            let clone_call = self.builder.ins().call(clone, &[arg_val]);
            cloned_args.push(BlockArg::Value(self.builder.inst_results(clone_call)[0]));
        }
        // Drop any allocations that are not present in the continuation we're
        // jumping to
        self.drop_n_codegen(self.curr_allocs - num_allocs_at_dest);
        self.builder.ins().jump(to, &cloned_args);
    }

    fn halt_codegen(&mut self, args: &CpsValue) {
        let val = self.value_codegen(args);
        let halt = self
            .module
            .declare_func_in_func(self.runtime_funcs.halt, self.builder.func);
        let call = self.builder.ins().call(halt, &[val]);
        let result = self.builder.inst_results(call)[0];
        self.drop_all_codegen();
        self.builder.ins().return_(&[result]);
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
            .icmp_imm(IntCC::NotEqual, cond, FALSE_VALUE as i64);

        // Because our compiler is not particularly sophisticated right now, we
        // can guarantee that both branches terminate. Thus, no merge basic
        // block.
        let success_block = self.builder.create_block();
        let failure_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(cond, success_block, &[], failure_block, &[]);

        // Generate success block:
        let num_allocs = self.curr_allocs;
        self.builder.switch_to_block(success_block);
        self.builder.seal_block(success_block);
        self.cps_codegen(success, deferred_procs, deferred_local_conts);

        // Generate failure block:
        self.curr_allocs = num_allocs;
        self.builder.switch_to_block(failure_block);
        self.builder.seal_block(failure_block);
        self.cps_codegen(failure, deferred_procs, deferred_local_conts);
    }

    fn raise_codegen(&mut self, val: Value) {
        let runtime = self.get_runtime();
        let barrier = self.get_barrier();
        let raise = self
            .module
            .declare_func_in_func(self.runtime_funcs.raise_rt, self.builder.func);
        let call = self.builder.ins().call(raise, &[runtime, val, barrier]);
        let result = self.builder.inst_results(call)[0];
        self.builder.ins().return_(&[result]);
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
                let mut runtime_write = self.runtime.0.write();
                runtime_write.globals_pool.insert(global.clone());
                self.builder.ins().iconst(
                    types::I64,
                    SchemeValue::as_raw(&SchemeValue::from(global.val.clone())) as i64,
                )
            }
            CpsValue::Var(Var::Local(local)) => match self.rebinds.fetch_bind(local) {
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
        self.builder.ins().stack_store(val, slot, i as i32 * 8);
    }

    fn array_load(&mut self, slot: StackSlot, i: usize) -> Value {
        self.builder
            .ins()
            .stack_load(types::I64, slot, i as i32 * 8)
    }

    fn fix_codegen(
        &mut self,
        bindings: Vec<LambdaBinding>,
        cexp: Cps,
        deferred_procs: &mut Vec<ProcedureBundle>,
        deferred_local_conts: &mut Vec<ProcedureBundle>,
    ) {
        // Collect local_cont and proc bundles
        let mut proc_bundles = Vec::new();
        let mut local_cont_bundles = Vec::new();
        for binding in bindings.into_iter() {
            let is_proc = binding.is_func() || self.escaping.contains(binding.val);
            let bundle = ProcedureBundle::new(
                self.runtime.clone(),
                binding.val,
                binding.args,
                *binding.body,
                binding.span,
                self.free_vars,
                self.module,
            );
            if is_proc {
                proc_bundles.push(bundle);
            } else {
                let cont_block = self.builder.create_block();
                self.local_cont_blocks.insert(bundle.val, cont_block);
                local_cont_bundles.push(bundle);
            }
        }

        // The set of vals bound in this Fix (that are not local continuations).
        // A binding's body may reference any of these, including itself, so we
        // cannot resolve them until after all of the procedures have been
        // allocated.
        let fix_vals = proc_bundles.iter().map(|b| b.val).collect::<HashSet<_>>();

        // Allocate all of the procedures. The procedures are rooted and thus we
        // have exclusive mutable access to them.
        for bundle in &proc_bundles {
            self.alloc_procedure_codegen(bundle, &fix_vals);
        }

        // Patch any procedures that were created by the fix primitive into the
        // environment of the procedures.
        for bundle in &proc_bundles {
            self.patch_env_codegen(bundle, &fix_vals);
        }

        // Now that we no longer need mutable access, unroot the procedures.
        for bundle in &proc_bundles {
            self.unroot_proc_codegen(bundle);
        }

        deferred_procs.extend(proc_bundles);
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

        // Reset curr_allocs to whatever
        self.curr_allocs = self.allocs_at_local_cont[&bundle.val];

        let mut param_vals = Vec::new();
        for arg in &bundle.args.args {
            let value = self.builder.append_block_param(cont_block, types::I64);
            param_vals.push(value);
            self.rebinds.rebind(*arg, IrValue::Value(value));
        }

        for param_val in param_vals {
            self.push_alloc(param_val);
        }

        // No need to rebind env variables, they are already present
        self.cps_codegen(bundle.body, deferred_procs, deferred_local_conts);
    }

    fn alloc_procedure_codegen(&mut self, bundle: &ProcedureBundle, fix_vals: &HashSet<Local>) {
        // Construct the env array. Recursive references get a placeholder that
        // will be overwritten once every procedure in the group has been
        // allocated.
        let env = self.alloc_array(bundle.env.len());
        for (i, env_var) in bundle.env.iter().enumerate() {
            let val = if fix_vals.contains(env_var) {
                // Undefined
                self.builder.ins().iconst(types::I64, 0)
            } else {
                match *self.rebinds.fetch_bind(env_var) {
                    IrValue::Cell(ptr) => ptr,
                    IrValue::Value(val) => val,
                }
            };
            self.array_store(env, i, val);
        }

        let runtime_param = self.get_runtime();
        let func_ref = self
            .module
            .declare_func_in_func(bundle.func_id, self.builder.func);
        let func_ptr = self.builder.ins().func_addr(types::I64, func_ref);
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
            runtime_param,
            func_ptr,
            env_addr,
            env_len,
            num_required,
            is_variadic,
        ];

        let make_proc = if bundle.args.continuation.is_some() {
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
            self.runtime_funcs.make_user
        } else {
            args.push(self.get_barrier());
            self.runtime_funcs.make_continuation
        };

        let make_proc = self
            .module
            .declare_func_in_func(make_proc, self.builder.func);
        let call = self.builder.ins().call(make_proc, &args);
        let proc = self.builder.inst_results(call)[0];
        self.rebinds.rebind(bundle.val, IrValue::Value(proc));
        self.push_alloc(proc);
    }

    fn patch_env_codegen(&mut self, bundle: &ProcedureBundle, fix_vals: &HashSet<Local>) {
        let IrValue::Value(proc) = self.rebinds.fetch_bind(&bundle.val) else {
            unreachable!();
        };

        let patch_fn = self
            .module
            .declare_func_in_func(self.runtime_funcs.patch_env_slot, self.builder.func);

        for (i, env_var) in bundle.env.iter().enumerate() {
            if !fix_vals.contains(env_var) {
                continue;
            }
            let IrValue::Value(target) = self.rebinds.fetch_bind(env_var) else {
                unreachable!();
            };
            let slot_idx = self.builder.ins().iconst(types::I32, i as i64);
            self.builder
                .ins()
                .call(patch_fn, &[*proc, slot_idx, *target]);
        }
    }

    fn unroot_proc_codegen(&mut self, bundle: &ProcedureBundle) {
        let IrValue::Value(proc) = self.rebinds.fetch_bind(&bundle.val) else {
            unreachable!();
        };

        let unroot_proc = self
            .module
            .declare_func_in_func(self.runtime_funcs.unroot_proc, self.builder.func);

        self.builder.ins().call(unroot_proc, &[*proc]);
    }
}

pub struct ProcedureBundle {
    runtime: Runtime,
    func_id: FuncId,
    val: Local,
    env: Vec<Local>,
    args: LambdaArgs,
    body: Cps,
    loc: Option<Span>,
}

const RUNTIME_PARAM: usize = 0;
const ENV_PARAM: usize = 1;
const ARGS_PARAM: usize = 2;
const CONT_BARRIER_PARAM: usize = 3;
const CONTINUATION_PARAM: usize = 4;

fn make_sig(sig: &mut Signature, has_continuation: bool) {
    sig.params.push(AbiParam::new(types::I64)); // Runtime
    sig.params.push(AbiParam::new(types::I64)); // Env
    sig.params.push(AbiParam::new(types::I64)); // Args
    sig.params.push(AbiParam::new(types::I64)); // DynStack

    if has_continuation {
        sig.params.push(AbiParam::new(types::I64)); // Continuation
    }

    sig.returns.push(AbiParam::new(types::I64)); // Application
}

impl ProcedureBundle {
    fn new(
        runtime: Runtime,
        val: Local,
        args: LambdaArgs,
        body: Cps,
        loc: Option<Span>,
        free_vars_cache: &mut FreeVariables,
        module: &mut JITModule,
    ) -> Self {
        let mut sig = module.make_signature();
        make_sig(&mut sig, args.continuation.is_some());
        // let name = val.to_func_name();
        let func_id = module
            .declare_anonymous_function(&sig)
            .expect("Could not declare function");

        let env = free_vars_cache
            .find_free_vars(&body)
            .difference(&args.iter().cloned().collect::<HashSet<_>>())
            .cloned()
            .collect::<Vec<_>>();

        Self {
            runtime,
            func_id,
            val,
            env,
            args,
            body,
            loc,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn codegen(
        self,
        runtime_funcs: &RuntimeFunctions,
        cells: &HashSet<Local>,
        escaping: &Escaping,
        free_vars: &mut FreeVariables,
        module: &mut JITModule,
        debug_info: &mut DebugInfo,
        deferred_procs: &mut Vec<Self>,
    ) {
        let mut builder_context = FunctionBuilderContext::new();
        let mut ctx = module.make_context();
        make_sig(&mut ctx.func.signature, self.args.continuation.is_some());
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let mut allocs_at_local_conts = HashMap::default();
        let max_allocs = self
            .body
            .max_allocs(0, escaping, &mut allocs_at_local_conts);

        let allocs = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            max_allocs as u32 * 8,
            0,
        ));

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let params = {
            let block_params = builder.block_params(entry_block);
            [
                block_params[RUNTIME_PARAM],
                block_params[CONT_BARRIER_PARAM],
            ]
        };

        let mut rebinds = Rebinds::new();

        // Load environment:
        let env_param = builder.block_params(entry_block)[ENV_PARAM];
        for (i, env_var) in self.env.into_iter().enumerate() {
            let var = builder
                .ins()
                .load(types::I64, MemFlags::new(), env_param, (i * 8) as i32);
            let var = if cells.contains(&env_var) {
                IrValue::Cell(var)
            } else {
                IrValue::Value(var)
            };
            rebinds.rebind(env_var, var);
        }

        // Load args. The continuation is passed as a separate parameter (see
        // below), so only the regular arguments live in the args array.
        let args_param = builder.block_params(entry_block)[ARGS_PARAM];
        for (i, arg) in self.args.args.iter().enumerate() {
            let var = builder
                .ins()
                .load(types::I64, MemFlags::new(), args_param, (i * 8) as i32);
            rebinds.rebind(*arg, IrValue::Value(var));
        }

        // Load continuation:
        if let Some(cont) = self.args.continuation {
            let cont_param = builder.block_params(entry_block)[CONTINUATION_PARAM];
            let val = builder.ins().bor_imm(cont_param, Tag::Procedure as i64);
            rebinds.rebind(cont, IrValue::Value(val));
        }

        let mut cu = CompilationUnit {
            runtime: self.runtime,
            builder,
            rebinds,
            allocs,
            curr_allocs: 0,
            allocs_at_local_cont: &allocs_at_local_conts,
            local_cont_blocks: HashMap::default(),
            escaping,
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

        cu.builder.finalize();

        module.define_function(self.func_id, &mut ctx).unwrap();
        module.clear_context(&mut ctx);
    }
}
