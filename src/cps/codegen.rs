//! LLVM SSA Codegen from CPS.

use indexmap::IndexMap;
use inkwell::{
    AddressSpace, IntPredicate,
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
};
use std::{collections::HashMap, rc::Rc};

use crate::{
    gc::Gc,
    proc::{Closure, ContinuationPtr, FuncPtr},
    runtime::{IGNORE_CALL_SITE, IGNORE_FUNCTION, Runtime},
    value::{ReflexiveValue, Value as SchemeValue},
};

use super::*;

struct Rebinds<'ctx> {
    rebinds: HashMap<Var, BasicValueEnum<'ctx>>,
}

impl<'ctx> Rebinds<'ctx> {
    fn rebind(&mut self, old_var: Var, new_var: BasicValueEnum<'ctx>) {
        self.rebinds.insert(old_var, new_var);
    }

    fn fetch_bind(&self, var: &Var) -> &BasicValueEnum<'ctx> {
        self.rebinds
            .get(var)
            .unwrap_or_else(|| panic!("could not find {var:?}"))
    }

    fn fetch_bind_opt(&self, var: &Var) -> Option<&BasicValueEnum<'ctx>> {
        self.rebinds.get(var)
    }

    fn new() -> Self {
        Self {
            rebinds: HashMap::default(),
        }
    }
}

struct Allocs<'ctx> {
    prev_alloc: Option<Rc<Allocs<'ctx>>>,
    value: BasicValueEnum<'ctx>, // PointerValue<'ctx>,
}

impl<'ctx> Allocs<'ctx> {
    fn new(
        prev_alloc: Option<Rc<Allocs<'ctx>>>,
        value: BasicValueEnum<'ctx>,
    ) -> Option<Rc<Allocs<'ctx>>> {
        Some(Rc::new(Allocs { prev_alloc, value }))
    }

    fn to_cells(&self) -> Vec<PointerValue<'ctx>> {
        let mut allocs = match self.value {
            BasicValueEnum::PointerValue(value) => vec![value],
            _ => Vec::new(),
        };

        if let Some(ref prev_alloc) = self.prev_alloc {
            allocs.extend(prev_alloc.to_cells());
        }

        allocs
    }

    fn to_values(&self) -> Vec<IntValue<'ctx>> {
        let mut allocs = match self.value {
            BasicValueEnum::IntValue(value) => vec![value],
            _ => Vec::new(),
        };

        if let Some(ref prev_alloc) = self.prev_alloc {
            allocs.extend(prev_alloc.to_values());
        }

        allocs
    }
}

impl Cps {
    pub fn into_closure<'ctx, 'b>(
        self,
        runtime: Gc<Runtime>,
        env: IndexMap<Local, Gc<SchemeValue>>,
        ctx: &'ctx Context,
        module: &'b Module<'ctx>,
        ee: &ExecutionEngine<'ctx>,
        builder: &'b Builder<'ctx>,
    ) -> Result<Closure, BuilderError>
    where
        'ctx: 'b,
    {
        if std::env::var("SCHEME_RS_DEBUG").is_ok() {
            eprintln!("compiling: {self:#?}");
        }

        let ptr_type = ctx.ptr_type(AddressSpace::default());
        let fn_type = ptr_type.fn_type(
            &[
                ptr_type.into(), // Runtime
                ptr_type.into(), // Env
                ptr_type.into(), // Globals
                ptr_type.into(), // Args
                ptr_type.into(), // Exception handler
                ptr_type.into(), // Dyanmic wind
            ],
            false,
        );
        let fn_value = Local::gensym();
        let fn_name = fn_value.to_func_name();
        let function = module.add_function(&fn_name, fn_type, None);

        let mut cu = CompilationUnit::new(runtime.clone(), ctx, module, builder, function);
        let entry = ctx.append_basic_block(function, "entry");
        builder.position_at_end(entry);

        // Collect the provided environment:

        let env_param = function
            .get_nth_param(ENV_PARAM)
            .unwrap()
            .into_pointer_value();
        let array_type = ptr_type.array_type(env.len() as u32);
        let env_load = builder
            .build_load(array_type, env_param, "env_load")?
            .into_array_value();

        let mut collected_env = Vec::new();
        for (i, (local, val)) in env.into_iter().enumerate() {
            collected_env.push(val);
            let res = builder
                .build_extract_value(env_load, i as u32, "extract_env")
                .unwrap();
            cu.rebinds.rebind(Var::Local(local), res);
        }

        // Collect the provided globals:

        let globals = self.globals().into_iter().collect::<Vec<_>>();

        let globals_param = function
            .get_nth_param(GLOBALS_PARAM)
            .unwrap()
            .into_pointer_value();
        let array_type = ptr_type.array_type(globals.len() as u32);
        let globals_load = builder
            .build_load(array_type, globals_param, "globals_load")?
            .into_array_value();

        for (i, global) in globals.iter().enumerate() {
            let res = builder
                .build_extract_value(globals_load, i as u32, "extract_global")
                .unwrap();
            cu.rebinds.rebind(Var::Global(global.clone()), res);
        }

        let mut deferred = Vec::new();
        cu.cps_codegen(self, None, &mut deferred)?;

        while let Some(next) = deferred.pop() {
            next.codegen(ctx, module, builder, &mut deferred)?;
        }

        assert!(function.verify(true));

        if std::env::var("SCHEME_RS_DEBUG").is_ok() {
            function.print_to_stderr();
        }

        let func = unsafe {
            ee.get_function::<ContinuationPtr>(&fn_name)
                .unwrap()
                .into_raw()
        };

        Ok(Closure::new(
            runtime,
            collected_env,
            globals.into_iter().map(Global::value).collect::<Vec<_>>(),
            FuncPtr::Continuation(func),
            0,
            true,
            None,
        ))
    }
}

struct CompilationUnit<'ctx, 'b> {
    runtime: Gc<Runtime>,
    ctx: &'ctx Context,
    module: &'b Module<'ctx>,
    builder: &'b Builder<'ctx>,
    function: FunctionValue<'ctx>,
    rebinds: Rebinds<'ctx>,
}

// Everything returns a pointer allocated from a Gc, so everything is a Gc<Value>.
//
// If we do this, then we only need to do two things: dec the ref count of every gc allocated
// in the function at return, and inc the ref count when we create a Gc from a raw pointer
// on the Rust side
//
// List of included runtime functions can be found in runtime.rs

impl<'ctx, 'b> CompilationUnit<'ctx, 'b> {
    fn new(
        runtime: Gc<Runtime>,
        ctx: &'ctx Context,
        module: &'b Module<'ctx>,
        builder: &'b Builder<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> Self {
        Self {
            runtime,
            ctx,
            module,
            builder,
            function,
            rebinds: Rebinds::new(),
        }
    }

    fn cps_codegen(
        &mut self,
        cps: Cps,
        allocs: Option<Rc<Allocs<'ctx>>>,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        match cps {
            Cps::If(cond, success, failure) => {
                self.if_codegen(&cond, *success, *failure, allocs, deferred)?
            }
            Cps::App(operator, args, call_site_id) => {
                self.app_codegen(&operator, &args, call_site_id, allocs)?
            }
            Cps::Forward(operator, arg) => self.forward_codegen(&operator, &arg, allocs)?,
            Cps::PrimOp(PrimOp::Set, args, _, cexpr) => {
                self.store_codegen(&args[1], &args[0])?;
                self.cps_codegen(*cexpr, allocs, deferred)?;
            }
            Cps::PrimOp(PrimOp::AllocCell, _, into, cexpr) => {
                self.alloc_cell_codegen(into, *cexpr, allocs, deferred)?;
            }
            Cps::PrimOp(PrimOp::ExtractWinders, _, extract_to, cexpr) => {
                self.extract_winders_codegen(extract_to, *cexpr, allocs, deferred)?;
            }
            Cps::PrimOp(PrimOp::PrepareContinuation, args, prepare_to, cexpr) => {
                let [cont, winders] = args.as_slice() else {
                    unreachable!()
                };
                self.prepare_continuation_codegen(
                    cont, winders, prepare_to, *cexpr, allocs, deferred,
                )?;
            }
            Cps::PrimOp(PrimOp::GetCallTransformerFn, env, res, cexpr) => {
                self.get_call_transformer_codegen(&env, res)?;
                self.cps_codegen(*cexpr, allocs, deferred)?;
            }
            Cps::PrimOp(primop, vals, result, cexpr) => {
                self.simple_primop_codegen(primop, &vals, result, *cexpr, allocs, deferred)?
            }
            Cps::Closure {
                args,
                body,
                val,
                cexp,
                debug: debug_info_id,
            } => {
                let bundle = ClosureBundle::new(
                    self.runtime.clone(),
                    self.ctx,
                    self.module,
                    val,
                    args.clone(),
                    body.as_ref().clone(),
                );
                self.make_closure_codegen(&bundle, *cexp, debug_info_id, allocs, deferred)?;
                deferred.push(bundle);
            }
            Cps::Halt(value) => self.halt_codegen(&value, allocs)?,
        }
        Ok(())
    }

    fn value_codegen(&self, value: &Value) -> BasicValueEnum<'ctx> {
        match value {
            Value::Var(var) => {
                let cell = *self.rebinds.fetch_bind(var);
                if cell.is_int_value() {
                    return cell;
                }
                let read_cell = self.module.get_function("read_cell").unwrap();
                self.builder
                    .build_call(read_cell, &[cell.into()], "read_value")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            }
            Value::Const(val) => {
                let mut runtime_write = self.runtime.write();
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
                let i64_type = self.ctx.i64_type();
                i64_type.const_int(raw, false).into()
            }
        }
    }

    fn extract_winders_codegen(
        &mut self,
        extract_to: Local,
        cexpr: Cps,
        allocs: Option<Rc<Allocs<'ctx>>>,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        let extract_winders = self.module.get_function("extract_winders").unwrap();
        let winders = self
            .builder
            .build_call(
                extract_winders,
                &[self
                    .function
                    .get_nth_param(DYNAMIC_WIND_PARAM)
                    .unwrap()
                    .into_pointer_value()
                    .into()],
                "winders",
            )?
            .try_as_basic_value()
            .left()
            .unwrap();

        self.rebinds.rebind(Var::Local(extract_to), winders);
        let new_alloc = Allocs::new(allocs, winders);
        self.cps_codegen(cexpr, new_alloc, deferred)?;

        Ok(())
    }

    fn prepare_continuation_codegen(
        &mut self,
        cont: &Value,
        winders: &Value,
        prepare_to: Local,
        cexpr: Cps,
        allocs: Option<Rc<Allocs<'ctx>>>,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        let cont = self.value_codegen(cont);
        let winders = self.value_codegen(winders);
        let prepare_continuation = self.module.get_function("prepare_continuation").unwrap();
        let prepared = self
            .builder
            .build_call(
                prepare_continuation,
                &[
                    cont.into(),
                    winders.into(),
                    self.function
                        .get_nth_param(DYNAMIC_WIND_PARAM)
                        .unwrap()
                        .into_pointer_value()
                        .into(),
                ],
                "prepared_continuation",
            )?
            .try_as_basic_value()
            .left()
            .unwrap();

        self.rebinds.rebind(Var::Local(prepare_to), prepared);
        let new_alloc = Allocs::new(allocs, prepared);
        self.cps_codegen(cexpr, new_alloc, deferred)?;

        Ok(())
    }
    fn simple_primop_codegen(
        &mut self,
        primop: PrimOp,
        vals: &[Value],
        result: Local,
        cexpr: Cps,
        allocs: Option<Rc<Allocs<'ctx>>>,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        // Put the values into an array:
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());
        let i32_type = self.ctx.i32_type();
        let num_vals = vals.len();
        let array_type = ptr_type.array_type(num_vals as u32);
        let vals_alloca = self.builder.build_alloca(array_type, "vals")?;
        for (i, val) in vals.iter().enumerate() {
            let ep = unsafe {
                self.builder.build_gep(
                    ptr_type,
                    vals_alloca,
                    &[i32_type.const_int(i as u64, false)],
                    "vals_elem",
                )?
            };
            let val = self.value_codegen(val);
            self.builder.build_store(ep, val)?;
        }

        // Call the respective runtime function:
        let runtime_fn_name = match primop {
            PrimOp::Add => "add",
            PrimOp::Sub => "sub",
            PrimOp::Mul => "mul",
            PrimOp::Div => "div",
            PrimOp::Equal => "equal",
            PrimOp::Greater => "greater",
            PrimOp::GreaterEqual => "greater_equal",
            PrimOp::Lesser => "lesser",
            PrimOp::LesserEqual => "lesser_equal",
            _ => unreachable!(),
        };

        let error_val = self.builder.build_alloca(ptr_type, "error")?;

        let runtime_fn = self.module.get_function(runtime_fn_name).unwrap();
        let result_val = self
            .builder
            .build_call(
                runtime_fn,
                &[
                    vals_alloca.into(),
                    i32_type.const_int(num_vals as u64, false).into(),
                    error_val.into(),
                ],
                runtime_fn_name,
            )?
            .try_as_basic_value()
            .left()
            .unwrap();

        let is_undef_bb = self.ctx.append_basic_block(self.function, "is_undef");
        let success_bb = self.ctx.append_basic_block(self.function, "success");

        let i64_type = self.ctx.i64_type();
        let undef = i64_type.const_int(0, false);
        let is_undef = self.builder.build_int_compare(
            IntPredicate::EQ,
            result_val.into_int_value(),
            undef,
            "is_undef",
        )?;
        // let is_undef = self.builder.build_is_null(result_val, "is_undef")?;
        self.builder
            .build_conditional_branch(is_undef, is_undef_bb, success_bb)?;

        self.builder.position_at_end(is_undef_bb);
        self.drops_codegen(allocs.clone())?;
        let error_val = self
            .builder
            .build_load(ptr_type, error_val, "error_val_load")?;
        self.builder.build_return(Some(&error_val))?;

        self.builder.position_at_end(success_bb);

        self.rebinds.rebind(Var::Local(result), result_val);
        let new_alloc = Allocs::new(allocs, result_val);

        self.cps_codegen(cexpr, new_alloc, deferred)?;

        Ok(())
    }

    fn alloc_cell_codegen(
        &mut self,
        var: Local,
        cexpr: Cps,
        allocs: Option<Rc<Allocs<'ctx>>>,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        // Get a newly allocated undefined value
        let alloc_cell = self.module.get_function("alloc_cell").unwrap();
        let cell = self
            .builder
            .build_call(alloc_cell, &[], "cell")?
            .try_as_basic_value()
            .left()
            .unwrap();

        // Rebind the variable to it
        self.rebinds.rebind(Var::Local(var), cell);
        let new_alloc = Allocs::new(allocs, cell);

        // Compile the continuation with the newly allocated value
        self.cps_codegen(cexpr, new_alloc, deferred)?;

        Ok(())
    }

    fn drops_codegen(&self, drops: Option<Rc<Allocs<'ctx>>>) -> Result<(), BuilderError> {
        self.drop_values_codgen(drops.clone())?;
        self.drop_cells_codegen(drops.clone())?;
        Ok(())
    }

    fn drop_values_codgen(&self, drops: Option<Rc<Allocs<'ctx>>>) -> Result<(), BuilderError> {
        let vals = drops.as_ref().map_or_else(Vec::new, |x| x.to_values());

        let dropv = self.module.get_function("dropv").unwrap();
        for val in vals.into_iter() {
            self.builder.build_call(dropv, &[val.into()], "_")?;
        }

        Ok(())
    }

    fn drop_cells_codegen(&self, drops: Option<Rc<Allocs<'ctx>>>) -> Result<(), BuilderError> {
        let cells = drops.as_ref().map_or_else(Vec::new, |x| x.to_cells());

        let dropc = self.module.get_function("dropc").unwrap();
        for cell in cells.into_iter() {
            self.builder.build_call(dropc, &[cell.into()], "_")?;
        }

        Ok(())
    }

    fn app_codegen(
        &self,
        operator: &Value,
        args: &[Value],
        call_site_id: Option<CallSiteId>,
        allocs: Option<Rc<Allocs<'ctx>>>,
    ) -> Result<(), BuilderError> {
        let operator = self.value_codegen(operator);

        // Allocate space for the args to be passed to make_application
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());
        let i32_type = self.ctx.i32_type();
        let array_type = ptr_type.array_type(args.len() as u32);
        let args_alloca = self.builder.build_alloca(array_type, "args")?;
        for (i, arg) in args.iter().enumerate() {
            let ep = unsafe {
                self.builder.build_gep(
                    ptr_type,
                    args_alloca,
                    &[i32_type.const_int(i as u64, false)],
                    "alloca_elem",
                )?
            };
            let val = self.value_codegen(arg);
            self.builder.build_store(ep, val)?;
        }

        let make_app = self.module.get_function("apply").unwrap();
        let app = self
            .builder
            .build_call(
                make_app,
                &[
                    self.function
                        .get_nth_param(RUNTIME_PARAM)
                        .unwrap()
                        .into_pointer_value()
                        .into(),
                    operator.into(),
                    args_alloca.into(),
                    i32_type.const_int(args.len() as u64, false).into(),
                    self.function
                        .get_nth_param(EXCEPTION_HANDLER_PARAM)
                        .unwrap()
                        .into_pointer_value()
                        .into(),
                    self.function
                        .get_nth_param(DYNAMIC_WIND_PARAM)
                        .unwrap()
                        .into_pointer_value()
                        .into(),
                    i32_type
                        .const_int(call_site_id.unwrap_or(IGNORE_CALL_SITE) as u64, false)
                        .into(),
                ],
                "apply",
            )?
            .try_as_basic_value()
            .left()
            .unwrap();

        // Now that we have created an application, we can reduce the ref counts of
        // all of the Gcs we have allocated in this function:
        self.drops_codegen(allocs)?;

        let _ = self.builder.build_return(Some(&app))?;

        Ok(())
    }

    fn forward_codegen(
        &self,
        operator: &Value,
        arg: &Value,
        allocs: Option<Rc<Allocs<'ctx>>>,
    ) -> Result<(), BuilderError> {
        let operator = self.value_codegen(operator);
        let arg = self.value_codegen(arg);

        let make_forward = self.module.get_function("forward").unwrap();
        let app = self
            .builder
            .build_call(
                make_forward,
                &[
                    operator.into(),
                    arg.into(),
                    self.function
                        .get_nth_param(EXCEPTION_HANDLER_PARAM)
                        .unwrap()
                        .into_pointer_value()
                        .into(),
                    self.function
                        .get_nth_param(DYNAMIC_WIND_PARAM)
                        .unwrap()
                        .into_pointer_value()
                        .into(),
                ],
                "forward",
            )?
            .try_as_basic_value()
            .left()
            .unwrap();

        // Now that we have created an application, we can reduce the ref counts of
        // all of the Gcs we have allocated in this function:
        self.drops_codegen(allocs)?;

        let _ = self.builder.build_return(Some(&app))?;

        Ok(())
    }

    fn halt_codegen(
        &self,
        args: &Value,
        allocs: Option<Rc<Allocs<'ctx>>>,
    ) -> Result<(), BuilderError> {
        let val = self.value_codegen(args);
        let make_app = self.module.get_function("halt").unwrap();
        let app = self
            .builder
            .build_call(make_app, &[val.into()], "halt")?
            .try_as_basic_value()
            .left()
            .unwrap();

        self.drops_codegen(allocs)?;

        let _ = self.builder.build_return(Some(&app))?;

        Ok(())
    }

    fn if_codegen(
        &mut self,
        cond: &Value,
        success: Cps,
        failure: Cps,
        allocs: Option<Rc<Allocs<'ctx>>>,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        let cond = self.value_codegen(cond);
        let truthy = self.module.get_function("truthy").unwrap();
        let cond = self
            .builder
            .build_call(truthy, &[cond.into()], "truthy")?
            .try_as_basic_value()
            .left()
            .unwrap();

        // Because our compiler is not particularly sophisticated right now, we can guarantee
        // that both branches terminate. Thus, no continuation basic block.
        let success_bb = self.ctx.append_basic_block(self.function, "success");
        let failure_bb = self.ctx.append_basic_block(self.function, "failure");

        self.builder
            .build_conditional_branch(cond.into_int_value(), success_bb, failure_bb)?;

        self.builder.position_at_end(success_bb);
        self.cps_codegen(success, allocs.clone(), deferred)?;

        self.builder.position_at_end(failure_bb);
        self.cps_codegen(failure, allocs, deferred)?;

        Ok(())
    }

    fn store_codegen(&self, from: &Value, to: &Value) -> Result<(), BuilderError> {
        let from = self.value_codegen(from).into();
        let Value::Var(to) = to else { unreachable!() };
        let to = self.rebinds.fetch_bind(to).into_pointer_value().into();
        let store = self.module.get_function("store").unwrap();
        let _ = self.builder.build_call(store, &[from, to], "")?;
        Ok(())
    }

    fn get_call_transformer_codegen(
        &mut self,
        env: &[Value],
        result: Local,
    ) -> Result<(), BuilderError> {
        let i32_type = self.ctx.i32_type();
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());

        let num_envs = i32_type.const_int(env.len() as u64, false);
        let env_type = ptr_type.array_type(env.len() as u32);
        let env_alloca = self.builder.build_alloca(env_type, "env_alloca")?;

        for (i, env_var) in env.iter().enumerate() {
            let ep = unsafe {
                self.builder.build_gep(
                    ptr_type,
                    env_alloca,
                    &[i32_type.const_int(i as u64, false)],
                    "alloca_elem",
                )?
            };
            let Value::Var(var) = env_var else {
                unreachable!()
            };
            let val = self.fetch_bind_or_undefined(var)?;
            if !val.is_pointer_value() {
                panic!("{env_var:?} is not a pointer");
            }
            self.builder.build_store(ep, val)?;
        }

        let get_call_transformer_fn = self.module.get_function("get_call_transformer_fn").unwrap();
        let expanded = self
            .builder
            .build_call(
                get_call_transformer_fn,
                &[
                    self.function
                        .get_nth_param(RUNTIME_PARAM)
                        .unwrap()
                        .into_pointer_value()
                        .into(),
                    env_alloca.into(),
                    num_envs.into(),
                ],
                "call_transformer",
            )?
            .try_as_basic_value()
            .left()
            .unwrap();
        self.rebinds.rebind(Var::Local(result), expanded);
        Ok(())
    }

    fn make_closure_codegen(
        &mut self,
        bundle: &ClosureBundle<'ctx>,
        cexp: Cps,
        debug_info_id: Option<FunctionDebugInfoId>,
        allocs: Option<Rc<Allocs<'ctx>>>,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        let i32_type = self.ctx.i32_type();
        let bool_type = self.ctx.bool_type();
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());

        // Construct the envs array:
        let num_envs = i32_type.const_int(bundle.env.len() as u64, false);
        let env_type = ptr_type.array_type(bundle.env.len() as u32);
        let env_alloca = self.builder.build_alloca(env_type, "env_alloca")?;

        for (i, env_var) in bundle.env.iter().enumerate() {
            let ep = unsafe {
                self.builder.build_gep(
                    ptr_type,
                    env_alloca,
                    &[i32_type.const_int(i as u64, false)],
                    "alloca_elem",
                )?
            };
            let val = self.fetch_bind_or_undefined(&Var::Local(*env_var))?;
            if !val.is_pointer_value() {
                panic!("{env_var:?} is not a pointer");
            }
            // assert!(val.is_pointer_value());
            self.builder.build_store(ep, val)?;
        }

        // Construct the globals array:
        let num_globals = i32_type.const_int(bundle.globals.len() as u64, false);
        let globals_type = ptr_type.array_type(bundle.globals.len() as u32);
        let globals_alloca = self.builder.build_alloca(globals_type, "globals_alloca")?;

        for (i, var) in bundle.globals.iter().enumerate() {
            let ep = unsafe {
                self.builder.build_gep(
                    ptr_type,
                    globals_alloca,
                    &[i32_type.const_int(i as u64, false)],
                    "alloca_elem",
                )?
            };
            let val = *self.rebinds.fetch_bind(&Var::Global(var.clone()));
            self.builder.build_store(ep, val)?;
        }

        let mut args = vec![
            self.function
                .get_nth_param(RUNTIME_PARAM)
                .unwrap()
                .into_pointer_value()
                .into(),
            bundle.function.as_global_value().as_pointer_value().into(),
            env_alloca.into(),
            num_envs.into(),
            globals_alloca.into(),
            num_globals.into(),
            i32_type
                .const_int(bundle.args.num_required() as u64, false)
                .into(),
            bool_type
                .const_int(bundle.args.variadic as u64, false)
                .into(),
        ];

        let make_closure = if bundle.args.continuation.is_some() {
            args.push(
                i32_type
                    .const_int(debug_info_id.unwrap_or(IGNORE_FUNCTION) as u64, false)
                    .into(),
            );
            self.module.get_function("make_closure").unwrap()
        } else {
            self.module.get_function("make_continuation").unwrap()
        };

        let closure = self
            .builder
            .build_call(make_closure, &args, "make_closure")?
            .try_as_basic_value()
            .left()
            .unwrap();

        self.rebinds.rebind(Var::Local(bundle.val), closure);

        let new_alloc = Allocs::new(allocs, closure);

        self.cps_codegen(cexp, new_alloc, deferred)?;

        Ok(())
    }

    fn fetch_bind_or_undefined(&self, var: &Var) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        if let Some(val) = self.rebinds.fetch_bind_opt(var) {
            Ok(*val)
        } else {
            // This variable is not available to the macro transformer (or
            // whatever else, but most likely a macro transformer)
            let alloc_cell = self.module.get_function("alloc_cell").unwrap();
            Ok(self
                .builder
                .build_call(alloc_cell, &[], "cell")?
                .try_as_basic_value()
                .left()
                .unwrap())
        }
    }
}

#[derive(Debug)]
pub struct ClosureBundle<'ctx> {
    runtime: Gc<Runtime>,
    val: Local,
    env: Vec<Local>,
    globals: Vec<Global>,
    args: ClosureArgs,
    body: Cps,
    function: FunctionValue<'ctx>,
}

const RUNTIME_PARAM: u32 = 0;
const ENV_PARAM: u32 = 1;
const GLOBALS_PARAM: u32 = 2;
const ARGS_PARAM: u32 = 3;
const EXCEPTION_HANDLER_PARAM: u32 = 4;
const DYNAMIC_WIND_PARAM: u32 = 5;
const CONTINUATION_PARAM: u32 = 6;

impl<'ctx> ClosureBundle<'ctx> {
    fn new(
        runtime: Gc<Runtime>,
        ctx: &'ctx Context,
        module: &Module<'ctx>,
        val: Local,
        args: ClosureArgs,
        body: Cps,
    ) -> Self {
        // TODO: These calls need to be cached and also calculated at the same time.
        let env = body
            .free_variables()
            .difference(&args.to_vec().into_iter().collect::<HashSet<_>>())
            .cloned()
            .collect::<Vec<_>>();
        let globals = body.globals().into_iter().collect::<Vec<_>>();

        let ptr_type = ctx.ptr_type(AddressSpace::default());

        let fn_type = if args.continuation.is_some() {
            ptr_type.fn_type(
                &[
                    ptr_type.into(), // Runtime
                    ptr_type.into(), // Env
                    ptr_type.into(), // Globals
                    ptr_type.into(), // Args
                    ptr_type.into(), // Exception handler
                    ptr_type.into(), // Dynamic wind
                    ptr_type.into(), // Continuation
                ],
                false,
            )
        } else {
            ptr_type.fn_type(
                &[
                    ptr_type.into(), // Runtime
                    ptr_type.into(), // Env
                    ptr_type.into(), // Globals
                    ptr_type.into(), // Args
                    ptr_type.into(), // Exception handler
                    ptr_type.into(), // Dynamic wind
                ],
                false,
            )
        };
        let name = val.to_func_name();
        let function = module.add_function(&name, fn_type, None);
        Self {
            runtime,
            val,
            env,
            globals,
            args,
            body,
            function,
        }
    }

    fn codegen<'b>(
        self,
        ctx: &'ctx Context,
        module: &'b Module<'ctx>,
        builder: &'b Builder<'ctx>,
        deferred: &mut Vec<Self>,
    ) -> Result<(), BuilderError> {
        let i64_type = ctx.i64_type();
        let ptr_type = ctx.ptr_type(AddressSpace::default());
        let entry = ctx.append_basic_block(self.function, "entry");

        builder.position_at_end(entry);

        let mut cu =
            CompilationUnit::new(self.runtime.clone(), ctx, module, builder, self.function);

        let env_param = self
            .function
            .get_nth_param(ENV_PARAM)
            .unwrap()
            .into_pointer_value();
        let array_type = ptr_type.array_type(self.env.len() as u32);
        let env_load = builder
            .build_load(array_type, env_param, "env_load")?
            .into_array_value();

        for (i, env_var) in self.env.iter().enumerate() {
            let res = builder
                .build_extract_value(env_load, i as u32, "extract_env")
                .unwrap();
            cu.rebinds.rebind(Var::Local(*env_var), res);
        }

        let globals_param = self
            .function
            .get_nth_param(GLOBALS_PARAM)
            .unwrap()
            .into_pointer_value();
        let array_type = ptr_type.array_type(self.globals.len() as u32);
        let globals_load = builder
            .build_load(array_type, globals_param, "globals_load")?
            .into_array_value();

        for (i, global) in self.globals.iter().enumerate() {
            let res = builder
                .build_extract_value(globals_load, i as u32, "extract_global")
                .unwrap();
            cu.rebinds.rebind(Var::Global(global.clone()), res);
        }

        let args_param = self
            .function
            .get_nth_param(ARGS_PARAM)
            .unwrap()
            .into_pointer_value();
        let array_type = i64_type.array_type(self.args.args.len() as u32);
        let args_load = builder
            .build_load(array_type, args_param, "args_load")?
            .into_array_value();

        for (i, arg_var) in self.args.args.iter().enumerate() {
            let res = builder
                .build_extract_value(args_load, i as u32, "extract_arg")
                .unwrap();
            cu.rebinds.rebind(Var::Local(*arg_var), res);
        }

        if let Some(cont) = self.args.continuation {
            let cont_param = self
                .function
                .get_nth_param(CONTINUATION_PARAM)
                .unwrap()
                .into_pointer_value();
            let cont_load = builder.build_load(i64_type, cont_param, "continuation")?;
            cu.rebinds.rebind(Var::Local(cont), cont_load);
        }

        cu.cps_codegen(self.body, None, deferred)?;

        if std::env::var("SCHEME_RS_DEBUG").is_ok() {
            self.function.print_to_stderr();
        }

        if !self.function.verify(true) {
            panic!("Invalid function");
        }

        Ok(())
    }
}
