//! Cranelift Codegen from CPS.

use cranelift::{
    codegen::ir::{StackSlot, entities::Value},
    prelude::*,
};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use std::{collections::HashSet, sync::Arc};

use crate::{
    cps::Value as CpsValue,
    proc::{ContinuationPtr, FuncPtr, ProcDebugInfo, Procedure},
    runtime::{DebugInfo, Runtime},
    value::Value as SchemeValue,
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
    truthy: FuncId,
    alloc_cell: FuncId,
    read_cell: FuncId,
    store: FuncId,
    error_unbound_variable: FuncId,
    matches: FuncId,
    expand_template: FuncId,
    error_no_patterns_match: FuncId,
    dropv: FuncId,
    raise_rt: FuncId,

    // List primops:
    cons: FuncId,
    list: FuncId,

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
        runtime_funcs: &RuntimeFunctions,
        module: &mut JITModule,
        debug_info: &mut DebugInfo,
    ) -> Procedure {
        if std::env::var("SCHEME_RS_DEBUG").is_ok() {
            eprintln!("compiling: {self:#?}");
        }

        let cells = self.cells();
        let mut builder_context = FunctionBuilderContext::new();
        let mut ctx = module.make_context();

        make_sig(&mut ctx.func.signature, false);

        let val = Local::gensym();
        let name = val.get_func_name();
        let entry_func = module
            .declare_function(&name, Linkage::Export, &ctx.func.signature)
            .unwrap();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let num_drops = self.max_drops();

        let vals = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            num_drops as u32 * 8,
            0,
        ));

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let params = {
            let block_params = builder.block_params(entry_block);
            [block_params[RUNTIME_PARAM], block_params[DYN_STATE_PARAM]]
        };

        let mut deferred = Vec::new();

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
            debug_info,
        };

        cu.cps_codegen(self, &mut deferred);

        if std::env::var("SCHEME_RS_DEBUG").is_ok() {
            eprintln!("compiled: {}", cu.builder.func.display());
        }

        cu.builder.finalize();

        module.define_function(entry_func, &mut ctx).unwrap();
        module.clear_context(&mut ctx);

        while let Some(next) = deferred.pop() {
            next.codegen(runtime_funcs, &cells, module, debug_info, &mut deferred);
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

struct CompilationUnit<'m, 'f, 'd> {
    runtime: Runtime,
    builder: FunctionBuilder<'m>,
    rebinds: Rebinds,
    allocs: StackSlot,
    curr_allocs: usize,
    runtime_funcs: &'f RuntimeFunctions,
    params: [Value; 2],
    module: &'m mut JITModule,
    debug_info: &'d mut DebugInfo,
}

impl<'m, 'f, 'd> CompilationUnit<'m, 'f, 'd> {
    fn push_alloc(&mut self, val: Value) {
        self.builder
            .ins()
            .stack_store(val, self.allocs, self.curr_allocs as i32 * 8);
        self.curr_allocs += 1;
    }

    fn get_runtime(&self) -> Value {
        self.params[0]
    }

    fn get_dyn_state(&self) -> Value {
        self.params[1]
    }

    fn cps_codegen(&mut self, cps: Cps, deferred: &mut Vec<ProcedureBundle>) {
        match cps {
            Cps::If(cond, success, failure) => {
                self.if_codegen(&cond, *success, *failure, deferred);
            }
            Cps::App(operator, args) => self.app_codegen(&operator, &args),
            Cps::PrimOp(PrimOp::Set, args, _, cexpr) => {
                self.store_codegen(&args[1], &args[0], *cexpr, deferred);
            }
            Cps::PrimOp(PrimOp::AllocCell, _, into, cexpr) => {
                self.alloc_cell_codegen(into, *cexpr, deferred);
            }
            Cps::PrimOp(PrimOp::Matches, args, bind_to, cexpr) => {
                let [pattern, expr] = args.as_slice() else {
                    unreachable!()
                };
                self.matches_codegen(pattern, expr, bind_to, *cexpr, deferred);
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
                    deferred,
                );
            }
            Cps::PrimOp(PrimOp::ErrorNoPatternsMatch, _, _, _) => {
                self.error_no_patterns_match_codegen();
            }
            Cps::PrimOp(PrimOp::GetFrame, args, dest, cexpr) => {
                let [op, span] = args.as_slice() else {
                    unreachable!()
                };
                self.get_frame_codegen(op, span, dest, *cexpr, deferred);
            }
            Cps::PrimOp(PrimOp::SetContinuationMark, args, _, cexpr) => {
                let [tag, val] = args.as_slice() else {
                    unreachable!()
                };
                self.set_continuation_mark_codegen(tag, val);
                self.cps_codegen(*cexpr, deferred);
            }
            Cps::PrimOp(primop, vals, result, cexpr) => {
                self.simple_primop_codegen(primop, &vals, result, *cexpr, deferred);
            }
            Cps::Lambda {
                args,
                body,
                val,
                cexp,
                span: loc,
            } => {
                let bundle = ProcedureBundle::new(
                    self.runtime.clone(),
                    val,
                    args.clone(),
                    body.as_ref().clone(),
                    loc,
                    self.module,
                );
                self.make_procedure_codegen(&bundle, *cexp, deferred);
                deferred.push(bundle);
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

        self.drops_codegen();
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
        deferred: &mut Vec<ProcedureBundle>,
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
        self.cps_codegen(cexpr, deferred);
    }

    fn expand_template_codegen(
        &mut self,
        template: &CpsValue,
        expansion_combiner: &CpsValue,
        expansions: &[CpsValue],
        dest: Local,
        cexpr: Cps,
        deferred: &mut Vec<ProcedureBundle>,
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
        self.cps_codegen(cexpr, deferred);
    }

    fn simple_primop_codegen(
        &mut self,
        primop: PrimOp,
        vals: &[CpsValue],
        dest: Local,
        cexpr: Cps,
        deferred: &mut Vec<ProcedureBundle>,
    ) {
        // Put the values into an array:
        let args = self.alloc_array(vals.len());

        for (i, val) in vals.iter().enumerate() {
            let val = self.value_codegen(val);
            self.array_store(args, i, val);
        }

        let vals_addr = self.builder.ins().stack_addr(types::I64, args, 0);
        let num_vals = self.builder.ins().iconst(types::I32, vals.len() as i64);

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
            _ => unreachable!(),
        };

        let runtime_func = self
            .module
            .declare_func_in_func(runtime_func, self.builder.func);

        let error_slot = self.alloc_array(1);
        let error_addr = self.builder.ins().stack_addr(types::I64, error_slot, 0);
        let primop_call = self
            .builder
            .ins()
            .call(runtime_func, &[vals_addr, num_vals, error_addr]);
        let result = self.builder.inst_results(primop_call)[0];

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
        self.drops_codegen();
        self.raise_codegen(error_val);

        // Otherwise continue with the correct value
        self.builder.switch_to_block(success_block);
        self.builder.seal_block(success_block);
        self.rebinds.rebind(dest, IrValue::Value(result));
        self.push_alloc(result);
        self.cps_codegen(cexpr, deferred);
    }

    fn get_frame_codegen(
        &mut self,
        op: &CpsValue,
        span: &CpsValue,
        dest: Local,
        cexpr: Cps,
        deferred: &mut Vec<ProcedureBundle>,
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
        self.cps_codegen(cexpr, deferred);
    }

    fn set_continuation_mark_codegen(&mut self, tag: &CpsValue, val: &CpsValue) {
        let tag = self.value_codegen(tag);
        let val = self.value_codegen(val);
        let dyn_state = self.get_dyn_state();
        let set_continuation_mark = self
            .module
            .declare_func_in_func(self.runtime_funcs.set_continuation_mark, self.builder.func);
        self.builder
            .ins()
            .call(set_continuation_mark, &[tag, val, dyn_state]);
    }

    fn error_no_patterns_match_codegen(&mut self) {
        self.drops_codegen();
        let error_no_patterns_match = self.module.declare_func_in_func(
            self.runtime_funcs.error_no_patterns_match,
            self.builder.func,
        );
        let call = self.builder.ins().call(error_no_patterns_match, &[]);
        let error = self.builder.inst_results(call)[0];
        self.raise_codegen(error);
    }

    fn alloc_cell_codegen(&mut self, var: Local, cexpr: Cps, deferred: &mut Vec<ProcedureBundle>) {
        let alloc_cell = self
            .module
            .declare_func_in_func(self.runtime_funcs.alloc_cell, self.builder.func);
        let call = self.builder.ins().call(alloc_cell, &[]);
        let cell = self.builder.inst_results(call)[0];
        self.rebinds.rebind(var, IrValue::Cell(cell));
        self.push_alloc(cell);
        self.cps_codegen(cexpr, deferred);
    }

    fn drops_codegen(&mut self) {
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

    fn app_codegen(&mut self, operator: &CpsValue, args: &[CpsValue]) {
        let runtime = self.get_runtime();
        let dyn_state = self.get_dyn_state();
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
            .call(apply, &[runtime, operator, args_addr, args_len, dyn_state]);
        let app = self.builder.inst_results(call)[0];
        self.drops_codegen();
        self.builder.ins().return_(&[app]);
    }

    /*
    fn forward_codegen(&mut self, operator: &CpsValue, arg: &CpsValue) {
        let runtime = self.get_runtime();
        let dyn_state = self.get_dyn_state();
        let operator = self.value_codegen(operator);
        let arg = self.value_codegen(arg);

        let forward = self
            .module
            .declare_func_in_func(self.runtime_funcs.forward, self.builder.func);
        let call = self
            .builder
            .ins()
            .call(forward, &[runtime, operator, arg, dyn_state]);
        let result = self.builder.inst_results(call)[0];
        self.drops_codegen();
        self.builder.ins().return_(&[result]);
    }
    */

    fn halt_codegen(&mut self, args: &CpsValue) {
        let val = self.value_codegen(args);
        let halt = self
            .module
            .declare_func_in_func(self.runtime_funcs.halt, self.builder.func);
        let call = self.builder.ins().call(halt, &[val]);
        let result = self.builder.inst_results(call)[0];
        self.drops_codegen();
        self.builder.ins().return_(&[result]);
    }

    fn if_codegen(
        &mut self,
        cond: &CpsValue,
        success: Cps,
        failure: Cps,
        deferred: &mut Vec<ProcedureBundle>,
    ) {
        let cond = self.value_codegen(cond);
        let truthy = self
            .module
            .declare_func_in_func(self.runtime_funcs.truthy, self.builder.func);
        let truthy_call = self.builder.ins().call(truthy, &[cond]);
        let cond = self.builder.inst_results(truthy_call)[0];

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
        self.cps_codegen(success, deferred);

        // Generate failure block:
        self.curr_allocs = num_allocs;
        self.builder.switch_to_block(failure_block);
        self.builder.seal_block(failure_block);
        self.cps_codegen(failure, deferred);
    }

    fn raise_codegen(&mut self, val: Value) {
        let runtime = self.get_runtime();
        let dyn_state = self.get_dyn_state();
        let raise = self
            .module
            .declare_func_in_func(self.runtime_funcs.raise_rt, self.builder.func);
        let call = self.builder.ins().call(raise, &[runtime, val, dyn_state]);
        let result = self.builder.inst_results(call)[0];
        self.builder.ins().return_(&[result]);
    }

    fn store_codegen(
        &mut self,
        from: &CpsValue,
        to: &CpsValue,
        cexpr: Cps,
        deferred: &mut Vec<ProcedureBundle>,
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
        self.cps_codegen(cexpr, deferred)
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

    fn make_procedure_codegen(
        &mut self,
        bundle: &ProcedureBundle,
        cexp: Cps,
        deferred: &mut Vec<ProcedureBundle>,
    ) {
        // Construct the envs array:
        let env = self.alloc_array(bundle.env.len());
        for (i, env_var) in bundle.env.iter().enumerate() {
            let val = match *self.rebinds.fetch_bind(env_var) {
                IrValue::Cell(ptr) => ptr,
                IrValue::Value(val) => val,
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
            args.push(self.get_dyn_state());
            self.runtime_funcs.make_continuation
        };

        let make_proc = self
            .module
            .declare_func_in_func(make_proc, self.builder.func);
        let call = self.builder.ins().call(make_proc, &args);
        let proc = self.builder.inst_results(call)[0];
        self.rebinds.rebind(bundle.val, IrValue::Value(proc));
        self.push_alloc(proc);
        self.cps_codegen(cexp, deferred);
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
const DYN_STATE_PARAM: usize = 3;
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
        module: &mut JITModule,
    ) -> Self {
        let mut sig = module.make_signature();
        make_sig(&mut sig, args.continuation.is_some());
        // let name = val.to_func_name();
        let func_id = module
            .declare_anonymous_function(&sig)
            .expect("Could not declare function");

        let env = body
            .free_variables()
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

    fn codegen(
        self,
        runtime_funcs: &RuntimeFunctions,
        cells: &HashSet<Local>,
        module: &mut JITModule,
        debug_info: &mut DebugInfo,
        deferred: &mut Vec<Self>,
    ) {
        let mut builder_context = FunctionBuilderContext::new();
        let mut ctx = module.make_context();
        make_sig(&mut ctx.func.signature, self.args.continuation.is_some());
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let max_drops = self.body.max_drops();

        let allocs = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            max_drops as u32 * 8,
            0,
        ));

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let params = {
            let block_params = builder.block_params(entry_block);
            [block_params[RUNTIME_PARAM], block_params[DYN_STATE_PARAM]]
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

        // Load args:
        let args_param = builder.block_params(entry_block)[ARGS_PARAM];
        for (i, arg) in self.args.iter().enumerate() {
            let var = builder
                .ins()
                .load(types::I64, MemFlags::new(), args_param, (i * 8) as i32);
            rebinds.rebind(*arg, IrValue::Value(var));
        }

        // Load continuation:
        if let Some(cont) = self.args.continuation {
            let cont_param = builder.block_params(entry_block)[CONTINUATION_PARAM];
            rebinds.rebind(cont, IrValue::Value(cont_param));
        }

        let mut cu = CompilationUnit {
            runtime: self.runtime,
            builder,
            rebinds,
            allocs,
            curr_allocs: 0,
            runtime_funcs,
            params,
            module,
            debug_info,
        };

        cu.cps_codegen(self.body, deferred);

        if std::env::var("GOUKI_DEBUG").is_ok() {
            eprintln!("compiled: {}", cu.builder.func.display());
        }

        cu.builder.finalize();

        module.define_function(self.func_id, &mut ctx).unwrap();
        module.clear_context(&mut ctx);
    }
}
