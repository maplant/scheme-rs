//! Cranelift Codegen from CPS.

use cranelift::{
    codegen::ir::{StackSlot, entities::Value},
    prelude::*,
};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use std::{collections::HashSet, sync::Arc};

use crate::{
    cps::{Value as CpsValue, analysis::MaxDrops},
    proc::{ClosureInner, ContinuationPtr, FuncDebugInfo, FuncPtr},
    runtime::{DebugInfo, Runtime},
    value::{ReflexiveValue, Value as SchemeValue},
};

use super::*;

#[derive(Copy, Clone, Debug)]
enum IrValue {
    Cell(Value),
    Value(Value),
}

struct Rebinds {
    rebinds: HashMap<Var, IrValue>,
}

impl Rebinds {
    fn rebind(&mut self, old_var: Var, new_var: IrValue) {
        self.rebinds.insert(old_var, new_var);
    }

    fn fetch_bind(&self, var: &Var) -> &IrValue {
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
    forward: FuncId,
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
    dropc: FuncId,
    raise_rt: FuncId,

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
    pub(crate) fn into_closure(
        self,
        runtime: Runtime,
        runtime_funcs: &RuntimeFunctions,
        module: &mut JITModule,
        debug_info: &mut DebugInfo,
    ) -> ClosureInner {
        if std::env::var("GOUKI_DEBUG").is_ok() {
            eprintln!("compiling: {self:#?}");
        }

        let mut builder_context = FunctionBuilderContext::new();
        let mut ctx = module.make_context();

        make_sig(&mut ctx.func.signature, false);

        let val = Local::gensym();
        let name = val.to_func_name();
        let entry_func = module
            .declare_function(&name, Linkage::Export, &ctx.func.signature)
            .unwrap();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let MaxDrops {
            value_drops,
            cell_drops,
        } = self.max_drops();

        let vals = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            value_drops as u32 * 8,
            0,
        ));
        let cells = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            cell_drops as u32 * 8,
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
                block_params[EXCEPTION_HANDLER_PARAM],
                block_params[DYNAMIC_WIND_PARAM],
            ]
        };

        let mut rebinds = Rebinds::new();

        // Top level cannot inherit environmental variables, by defintion.

        // Load globals:
        let globals = self.globals().into_iter().collect::<Vec<_>>();
        let globals_param = builder.block_params(entry_block)[GLOBALS_PARAM];
        for (i, global) in globals.iter().enumerate() {
            let var =
                builder
                    .ins()
                    .load(types::I64, MemFlags::new(), globals_param, (i * 8) as i32);
            rebinds.rebind(Var::Global(global.clone()), IrValue::Cell(var));
        }

        let mut deferred = Vec::new();

        {
            let mut cu = CompilationUnit {
                runtime: runtime.clone(),
                builder,
                rebinds,
                vals,
                curr_val: 0,
                cells,
                curr_cell: 0,
                runtime_funcs,
                params,
                module,
                debug_info,
            };

            cu.cps_codegen(self, &mut deferred);

            if std::env::var("GOUKI_DEBUG").is_ok() {
                eprintln!("compiled: {}", cu.builder.func.display());
            }

            cu.builder.finalize();
        }

        module.define_function(entry_func, &mut ctx).unwrap();
        module.clear_context(&mut ctx);

        while let Some(next) = deferred.pop() {
            next.codegen(runtime_funcs, module, debug_info, &mut deferred);
        }

        module.finalize_definitions().unwrap();

        let func = unsafe {
            std::mem::transmute::<*const u8, ContinuationPtr>(
                module.get_finalized_function(entry_func),
            )
        };

        ClosureInner::new(
            runtime,
            Vec::new(),
            globals.into_iter().map(Global::value).collect::<Vec<_>>(),
            FuncPtr::Continuation(func),
            0,
            true,
            None,
        )
    }
}

struct CompilationUnit<'m, 'f, 'd> {
    runtime: Runtime,
    builder: FunctionBuilder<'m>,
    rebinds: Rebinds,
    vals: StackSlot,
    curr_val: usize,
    cells: StackSlot,
    curr_cell: usize,
    runtime_funcs: &'f RuntimeFunctions,
    params: [Value; 3],
    module: &'m mut JITModule,
    debug_info: &'d mut DebugInfo,
}

impl<'m, 'f, 'd> CompilationUnit<'m, 'f, 'd> {
    fn push_val_alloc(&mut self, val: Value) {
        self.builder
            .ins()
            .stack_store(val, self.vals, self.curr_val as i32 * 8);
        self.curr_val += 1;
    }

    fn push_cell_alloc(&mut self, val: Value) {
        self.builder
            .ins()
            .stack_store(val, self.cells, self.curr_cell as i32 * 8);
        self.curr_cell += 1;
    }

    fn get_runtime(&self) -> Value {
        self.params[0]
    }

    fn get_exception_handler(&self) -> Value {
        self.params[1]
    }

    fn get_dynamic_wind(&self) -> Value {
        self.params[2]
    }

    fn cps_codegen(&mut self, cps: Cps, deferred: &mut Vec<ClosureBundle>) {
        match cps {
            Cps::If(cond, success, failure) => {
                self.if_codegen(&cond, *success, *failure, deferred);
            }
            Cps::App(operator, args, loc) => self.app_codegen(&operator, &args, loc),
            Cps::Forward(operator, arg) => self.forward_codegen(&operator, &arg),
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
                let bundle = ClosureBundle::new(
                    self.runtime.clone(),
                    val,
                    args.clone(),
                    body.as_ref().clone(),
                    loc,
                    self.module,
                );
                self.make_closure_codegen(&bundle, *cexp, deferred);
                deferred.push(bundle);
            }
            Cps::Halt(value) => self.halt_codegen(&value),
        }
    }

    fn value_codegen(&mut self, value: &CpsValue) -> Value {
        match value {
            CpsValue::Var(var) => {
                let cell = match *self.rebinds.fetch_bind(var) {
                    IrValue::Cell(cell) => cell,
                    IrValue::Value(int) => return int,
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
                let symbol = match var {
                    Var::Global(glob) => glob.name.sym.0,
                    Var::Local(Local {
                        name: Some(sym), ..
                    }) => sym.0,
                    _ => Symbol::intern(&format!("{}:{cell}", self.builder.func.name,)).0,
                } as i64;
                let symbol = self.builder.ins().iconst(types::I32, symbol);
                let error_unbound_variable = self.module.declare_func_in_func(
                    self.runtime_funcs.error_unbound_variable,
                    self.builder.func,
                );
                let call = self.builder.ins().call(error_unbound_variable, &[symbol]);
                let unbound_variable_error = self.builder.inst_results(call)[0];
                self.raise_codegen(unbound_variable_error);

                self.builder.switch_to_block(defined_block);
                self.builder.seal_block(defined_block);

                cell_value
            }
            CpsValue::Const(val) => {
                let mut runtime_write = self.runtime.0.write();
                let reflexive_val = ReflexiveValue(val.clone());
                if !runtime_write.constants_pool.contains(&reflexive_val) {
                    runtime_write.constants_pool.insert(reflexive_val.clone());
                }
                let raw = SchemeValue::as_raw(
                    runtime_write
                        .constants_pool
                        .get(&reflexive_val)
                        .unwrap()
                        .as_ref(),
                );
                self.builder.ins().iconst(types::I64, raw as i64)
            }
        }
    }

    fn matches_codegen(
        &mut self,
        pattern: &CpsValue,
        expr: &CpsValue,
        binds: Local,
        cexpr: Cps,
        deferred: &mut Vec<ClosureBundle>,
    ) {
        let pattern = self.value_codegen(pattern);
        let expr = self.value_codegen(expr);
        let matches = self
            .module
            .declare_func_in_func(self.runtime_funcs.matches, self.builder.func);
        let call = self.builder.ins().call(matches, &[pattern, expr]);
        let match_result = self.builder.inst_results(call)[0];
        self.rebinds
            .rebind(Var::Local(binds), IrValue::Value(match_result));
        self.push_val_alloc(match_result);
        self.cps_codegen(cexpr, deferred);
    }

    fn expand_template_codegen(
        &mut self,
        template: &CpsValue,
        expansion_combiner: &CpsValue,
        expansions: &[CpsValue],
        dest: Local,
        cexpr: Cps,
        deferred: &mut Vec<ClosureBundle>,
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
        self.rebinds
            .rebind(Var::Local(dest), IrValue::Value(expanded));
        self.push_val_alloc(expanded);
        self.cps_codegen(cexpr, deferred);
    }

    fn simple_primop_codegen(
        &mut self,
        primop: PrimOp,
        vals: &[CpsValue],
        dest: Local,
        cexpr: Cps,
        deferred: &mut Vec<ClosureBundle>,
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
        self.rebinds
            .rebind(Var::Local(dest), IrValue::Value(result));
        self.push_val_alloc(result);
        self.cps_codegen(cexpr, deferred);
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

    fn alloc_cell_codegen(&mut self, var: Local, cexpr: Cps, deferred: &mut Vec<ClosureBundle>) {
        let alloc_cell = self
            .module
            .declare_func_in_func(self.runtime_funcs.alloc_cell, self.builder.func);
        let call = self.builder.ins().call(alloc_cell, &[]);
        let cell = self.builder.inst_results(call)[0];
        self.rebinds.rebind(Var::Local(var), IrValue::Cell(cell));
        self.push_cell_alloc(cell);
        self.cps_codegen(cexpr, deferred);
    }

    fn drops_codegen(&mut self) {
        self.drop_values_codegen();
        self.drop_cells_codegen();
    }

    fn drop_values_codegen(&mut self) {
        if self.curr_val > 0 {
            let vals = self.builder.ins().stack_addr(types::I64, self.vals, 0);
            let num_vals = self.builder.ins().iconst(types::I32, self.curr_val as i64);
            let dropv = self
                .module
                .declare_func_in_func(self.runtime_funcs.dropv, self.builder.func);
            self.builder.ins().call(dropv, &[vals, num_vals]);
        }
    }

    fn drop_cells_codegen(&mut self) {
        if self.curr_cell > 0 {
            let cells = self.builder.ins().stack_addr(types::I64, self.cells, 0);
            let num_cells = self.builder.ins().iconst(types::I32, self.curr_cell as i64);
            let dropc = self
                .module
                .declare_func_in_func(self.runtime_funcs.dropc, self.builder.func);
            self.builder.ins().call(dropc, &[cells, num_cells]);
        }
    }

    fn app_codegen(&mut self, operator: &CpsValue, args: &[CpsValue], loc: Option<Span>) {
        let runtime = self.get_runtime();
        let operator = self.value_codegen(operator);

        // Allocate space for the args to be passed to make_application
        let args_slot = self.alloc_array(args.len());
        for (i, arg) in args.iter().enumerate() {
            let val = self.value_codegen(arg);
            self.array_store(args_slot, i, val);
        }

        let args_addr = self.builder.ins().stack_addr(types::I64, args_slot, 0);
        let args_len = self.builder.ins().iconst(types::I32, args.len() as i64);
        let exception_handler = self.get_exception_handler();
        let dynamic_wind = self.get_dynamic_wind();
        let span = if let Some(loc) = loc {
            let span = Arc::new(loc);
            let span_ptr = Arc::as_ptr(&span);
            self.debug_info.store_span(span);
            self.builder.ins().iconst(types::I64, span_ptr as i64)
        } else {
            self.builder.ins().iconst(types::I64, 0)
        };
        let apply = self
            .module
            .declare_func_in_func(self.runtime_funcs.apply, self.builder.func);
        let call = self.builder.ins().call(
            apply,
            &[
                runtime,
                operator,
                args_addr,
                args_len,
                exception_handler,
                dynamic_wind,
                span,
            ],
        );
        let app = self.builder.inst_results(call)[0];
        self.drops_codegen();
        self.builder.ins().return_(&[app]);
    }

    fn forward_codegen(&mut self, operator: &CpsValue, arg: &CpsValue) {
        let runtime = self.get_runtime();
        let operator = self.value_codegen(operator);
        let arg = self.value_codegen(arg);
        let exception_handler = self.get_exception_handler();
        let dynamic_wind = self.get_dynamic_wind();

        let forward = self
            .module
            .declare_func_in_func(self.runtime_funcs.forward, self.builder.func);
        let call = self.builder.ins().call(
            forward,
            &[runtime, operator, arg, exception_handler, dynamic_wind],
        );
        let result = self.builder.inst_results(call)[0];
        self.drops_codegen();
        self.builder.ins().return_(&[result]);
    }

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
        deferred: &mut Vec<ClosureBundle>,
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
        let num_vals = self.curr_val;
        let num_cells = self.curr_cell;
        self.builder.switch_to_block(success_block);
        self.builder.seal_block(success_block);
        self.cps_codegen(success, deferred);

        // Generate failure block:
        self.curr_val = num_vals;
        self.curr_cell = num_cells;
        self.builder.switch_to_block(failure_block);
        self.builder.seal_block(failure_block);
        self.cps_codegen(failure, deferred);
    }

    fn raise_codegen(&mut self, val: Value) {
        let runtime = self.get_runtime();
        let exception_handler = self.get_exception_handler();
        let dynamic_wind = self.get_dynamic_wind();
        let raise = self
            .module
            .declare_func_in_func(self.runtime_funcs.raise_rt, self.builder.func);
        let call = self
            .builder
            .ins()
            .call(raise, &[runtime, val, exception_handler, dynamic_wind]);
        let result = self.builder.inst_results(call)[0];
        self.builder.ins().return_(&[result]);
    }

    fn store_codegen(
        &mut self,
        from: &CpsValue,
        to: &CpsValue,
        cexpr: Cps,
        deferred: &mut Vec<ClosureBundle>,
    ) {
        let from = self.value_codegen(from);
        let CpsValue::Var(to) = to else {
            unreachable!()
        };
        let IrValue::Cell(to) = self.rebinds.fetch_bind(to) else {
            panic!("{to:?} is not a pointer");
        };
        let store = self
            .module
            .declare_func_in_func(self.runtime_funcs.store, self.builder.func);
        self.builder.ins().call(store, &[from, *to]);
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

    fn make_closure_codegen(
        &mut self,
        bundle: &ClosureBundle,
        cexp: Cps,
        deferred: &mut Vec<ClosureBundle>,
    ) {
        // Construct the envs array:
        let env = self.alloc_array(bundle.env.len());
        for (i, env_var) in bundle.env.iter().enumerate() {
            let val = match *self.rebinds.fetch_bind(&Var::Local(*env_var)) {
                IrValue::Cell(ptr) => ptr,
                IrValue::Value(val) => panic!("{val:?} is not a pointer"),
            };
            self.array_store(env, i, val);
        }

        // Construct the globals array:
        let globals = self.alloc_array(bundle.globals.len());
        for (i, global_var) in bundle.globals.iter().enumerate() {
            let val = match *self.rebinds.fetch_bind(&Var::Global(global_var.clone())) {
                IrValue::Cell(ptr) => ptr,
                IrValue::Value(val) => panic!("{val:?} is not a pointer"),
            };
            self.array_store(globals, i, val);
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
        let globals_addr = self.builder.ins().stack_addr(types::I64, globals, 0);
        let globals_len = self
            .builder
            .ins()
            .iconst(types::I32, bundle.globals.len() as i64);
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
            globals_addr,
            globals_len,
            num_required,
            is_variadic,
        ];

        let make_closure = if bundle.args.continuation.is_some() {
            args.push(if let Some(ref loc) = bundle.loc {
                let debug_info = Arc::new(FuncDebugInfo::new(
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
            self.runtime_funcs.make_continuation
        };

        let make_closure = self
            .module
            .declare_func_in_func(make_closure, self.builder.func);
        let call = self.builder.ins().call(make_closure, &args);
        let closure = self.builder.inst_results(call)[0];
        self.rebinds
            .rebind(Var::Local(bundle.val), IrValue::Cell(closure));
        self.push_cell_alloc(closure);
        self.cps_codegen(cexp, deferred);
    }
}

pub struct ClosureBundle {
    runtime: Runtime,
    func_id: FuncId,
    val: Local,
    env: Vec<Local>,
    globals: Vec<Global>,
    args: ClosureArgs,
    body: Cps,
    loc: Option<Span>,
}

const RUNTIME_PARAM: usize = 0;
const ENV_PARAM: usize = 1;
const GLOBALS_PARAM: usize = 2;
const ARGS_PARAM: usize = 3;
const EXCEPTION_HANDLER_PARAM: usize = 4;
const DYNAMIC_WIND_PARAM: usize = 5;
const CONTINUATION_PARAM: usize = 6;

fn make_sig(sig: &mut Signature, has_continuation: bool) {
    sig.params.push(AbiParam::new(types::I64)); // Runtime
    sig.params.push(AbiParam::new(types::I64)); // Env
    sig.params.push(AbiParam::new(types::I64)); // Globals
    sig.params.push(AbiParam::new(types::I64)); // Args
    sig.params.push(AbiParam::new(types::I64)); // Exception handler
    sig.params.push(AbiParam::new(types::I64)); // Dynamic wind

    if has_continuation {
        sig.params.push(AbiParam::new(types::I64)); // Continuation
    }

    sig.returns.push(AbiParam::new(types::I64)); // Application    
}

impl ClosureBundle {
    fn new(
        runtime: Runtime,
        val: Local,
        args: ClosureArgs,
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
        let globals = body.globals().into_iter().collect::<Vec<_>>();

        Self {
            runtime,
            func_id,
            val,
            env,
            globals,
            args,
            body,
            loc,
        }
    }

    fn codegen(
        self,
        runtime_funcs: &RuntimeFunctions,
        module: &mut JITModule,
        debug_info: &mut DebugInfo,
        deferred: &mut Vec<Self>,
    ) {
        let mut builder_context = FunctionBuilderContext::new();
        let mut ctx = module.make_context();
        make_sig(&mut ctx.func.signature, self.args.continuation.is_some());
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let MaxDrops {
            value_drops,
            cell_drops,
        } = self.body.max_drops();

        let vals = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            value_drops as u32 * 8,
            0,
        ));
        let cells = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            cell_drops as u32 * 8,
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
                block_params[EXCEPTION_HANDLER_PARAM],
                block_params[DYNAMIC_WIND_PARAM],
            ]
        };

        let mut rebinds = Rebinds::new();

        // Load environment:
        let env_param = builder.block_params(entry_block)[ENV_PARAM];
        for (i, env_var) in self.env.into_iter().enumerate() {
            let var = builder
                .ins()
                .load(types::I64, MemFlags::new(), env_param, (i * 8) as i32);
            rebinds.rebind(Var::Local(env_var), IrValue::Cell(var));
        }

        // Load globals:
        let globals_param = builder.block_params(entry_block)[GLOBALS_PARAM];
        for (i, global) in self.globals.into_iter().enumerate() {
            let var =
                builder
                    .ins()
                    .load(types::I64, MemFlags::new(), globals_param, (i * 8) as i32);
            rebinds.rebind(Var::Global(global), IrValue::Cell(var));
        }

        // Load args:
        let args_param = builder.block_params(entry_block)[ARGS_PARAM];
        for (i, arg) in self.args.iter().enumerate() {
            let var = builder
                .ins()
                .load(types::I64, MemFlags::new(), args_param, (i * 8) as i32);
            rebinds.rebind(Var::Local(*arg), IrValue::Value(var));
        }

        // Load continuation:
        if let Some(cont) = self.args.continuation {
            let cont_param = builder.block_params(entry_block)[CONTINUATION_PARAM];
            rebinds.rebind(Var::Local(cont), IrValue::Cell(cont_param));
        }

        {
            let mut cu = CompilationUnit {
                runtime: self.runtime,
                builder,
                rebinds,
                vals,
                curr_val: 0,
                cells,
                curr_cell: 0,
                runtime_funcs,
                params,
                module,
                debug_info,
            };

            cu.cps_codegen(self.body, deferred);

            if std::env::var("GOUKI_DEBUG").is_ok() {
                eprintln!("(bundle) compiled: {}", cu.builder.func.display());
            }

            cu.builder.finalize();
        }

        module.define_function(self.func_id, &mut ctx).unwrap();
        module.clear_context(&mut ctx);
    }
}
