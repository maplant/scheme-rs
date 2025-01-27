//! LLVM SSA Codegen from CPS.

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use std::collections::HashMap;

use crate::{
    proc::{Closure, FuncPtr, SyncFuncPtr},
    runtime,
};

use super::*;

struct Rebinds<'ctx> {
    // up: Option<&'ctx Rebinds<'ctx>>,
    rebinds: HashMap<Var, PointerValue<'ctx>>,
}

impl<'ctx> Rebinds<'ctx> {
    fn rebind(&mut self, old_var: Var, new_var: PointerValue<'ctx>) {
        self.rebinds.insert(old_var, new_var);
    }

    fn fetch_bind(&self, var: &Var) -> &PointerValue<'ctx> {
        self.rebinds.get(var).expect(&format!(
            "Couldn't find var: {var:?}, rebinds: {:#?}",
            self.rebinds
        ))
        // .unwrap_or_else(|| self.up.as_ref().unwrap().fetch_bind(var))
    }

    fn new() -> Self {
        Self {
            rebinds: HashMap::default(),
        }
    }
}

impl Cps {
    pub async fn compile(self) -> Result<Closure, BuilderError> {
        runtime::compile_cps(self).await
    }

    pub fn into_closure<'ctx, 'b>(
        self,
        ctx: &'ctx Context,
        module: &'b Module<'ctx>,
        ee: &ExecutionEngine<'ctx>,
        builder: &'b Builder<'ctx>,
    ) -> Result<Closure, BuilderError>
    where
        'ctx: 'b,
    {
        // let i32_type = ctx.i32_type();
        let ptr_type = ctx.ptr_type(AddressSpace::default());
        let fn_type = ptr_type.fn_type(
            &[
                ptr_type.into(), // Env
                ptr_type.into(), // Globals
                ptr_type.into(), // Args
            ],
            false,
        );
        let name = Local::gensym().to_func_name();
        let function = module.add_function(&name, fn_type, None);
        // There should be _no_ env variables for the CPS we call
        // into_closure with
        let globals = self.globals().into_iter().collect::<Vec<_>>();

        let entry = ctx.append_basic_block(function, "entry");
        builder.position_at_end(entry);

        /*
        builder.build_call(
            module.get_function("dbg_args").unwrap(),
            &[ function.get_nth_param(0).unwrap().into(),
               function.get_nth_param(1).unwrap().into(),
               function.get_nth_param(2).unwrap().into(), ], "dbg")?;
         */

        let mut cu = CompilationUnit::new(ctx, module, builder, function);

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
                .unwrap()
                .into_pointer_value();
            cu.rebinds.rebind(Var::Global(global.clone()), res);
        }

        let mut deferred = Vec::new();
        cu.cps_codegen(self, &mut deferred)?;

        while let Some(next) = deferred.pop() {
            next.codegen(ctx, module, builder, &mut deferred)?;
        }

        assert!(function.verify(true));

        function.print_to_stderr();

        let func = unsafe { ee.get_function::<SyncFuncPtr>(&name).unwrap().into_raw() };

        Ok(Closure::new(
            Vec::new(),
            globals.into_iter().map(Global::value).collect::<Vec<_>>(),
            FuncPtr::SyncFunc(func),
            0,
            true,
        ))
    }
}

struct CompilationUnit<'ctx, 'b> {
    ctx: &'ctx Context,
    module: &'b Module<'ctx>,
    builder: &'b Builder<'ctx>,
    function: FunctionValue<'ctx>,
    rebinds: Rebinds<'ctx>,
}

//
// Update: everything returns a pointer allocated from a Gc, so everything is a Gc<Value>
// If we do this, then we only need to do two things: dec the ref count of every gc allocated
// in the function at return, and inc the ref count when we create a Gc from a raw pointer
// on the Rust side
//
// List of included runtime functions can be found in runtime.rs

impl<'ctx, 'b> CompilationUnit<'ctx, 'b> {
    fn new(
        ctx: &'ctx Context,
        module: &'b Module<'ctx>,
        builder: &'b Builder<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> Self {
        Self {
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
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        match cps {
            Cps::AllocCell(into, cexpr) => {
                self.alloc_cell_codegen(into)?;
                self.cps_codegen(*cexpr, deferred)?;
            }
            Cps::If(cond, success, failure) => {
                self.if_codegen(&cond, *success, *failure, deferred)?
            }
            Cps::App(operator, args) => self.app_codegen(&operator, &args)?,
            Cps::PrimOp(PrimOp::Set, args, _, cexpr) => {
                self.store_codegen(&args[1], &args[0])?;
                self.cps_codegen(*cexpr, deferred)?;
            }
            Cps::Closure {
                args,
                body,
                val,
                cexp,
            } => {
                let bundle = ClosureBundle::new(
                    self.ctx,
                    &self.module,
                    val,
                    args.clone(),
                    body.as_ref().clone(),
                );
                self.make_closure_codegen(&bundle, *cexp, deferred)?;
                deferred.push(bundle);
            }
            Cps::ReturnValues(value) => self.return_values_codegen(&[Value::from(value)])?,
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn value_codegen(&self, value: &Value) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match value {
            Value::Var(var) => Ok((*self.rebinds.fetch_bind(var)).into()),
            Value::Literal(Literal::Number(num)) => {
                // FIXME: Number has to be a u64
                let num = num.to_u64();
                let i64_to_number = self.module.get_function("i64_to_number").unwrap();
                let constant = self.ctx.i64_type().const_int(num, false);
                Ok(self
                    .builder
                    .build_call(i64_to_number, &[constant.into()], "i64_to_number")?
                    .try_as_basic_value()
                    .left()
                    .unwrap())
            }
            _ => todo!(),
        }
    }

    fn alloc_cell_codegen(&mut self, var: Local) -> Result<(), BuilderError> {
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());
        let val = self.builder.build_alloca(ptr_type, &var.to_string())?;
        let gc_alloc_undef_val = self.module.get_function("alloc_undef_val").unwrap();
        let undef_val = self
            .builder
            .build_call(gc_alloc_undef_val, &[], "")?
            .try_as_basic_value()
            .left()
            .unwrap();
        let _store = self.builder.build_store(val, undef_val);
        self.rebinds.rebind(Var::Local(var), val);
        Ok(())
    }

    #[allow(dead_code)]
    fn drop_values_codegen(&self, drops: &[Value]) -> Result<(), BuilderError> {
        // Put the drops in an array
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());
        let i32_type = self.ctx.i32_type();
        let array_type = ptr_type.array_type(drops.len() as u32);
        let drops_alloca = self.builder.build_alloca(array_type, "drops")?;
        let drops_load = self
            .builder
            .build_load(array_type, drops_alloca, "drops_load")?
            .into_array_value();
        for (i, drp) in drops.iter().enumerate() {
            let drp = self.value_codegen(drp)?;
            self.builder
                .build_insert_value(drops_load, drp, i as u32, "drops_insert")?;
        }

        // Call drop_values
        let drop_values = self.module.get_function("drop_values").unwrap();
        self.builder.build_call(
            drop_values,
            &[
                drops_alloca.into(),
                i32_type.const_int(drops.len() as u64, false).into(),
            ],
            "drop_values",
        )?;

        Ok(())
    }

    fn app_codegen(&self, operator: &Value, args: &[Value]) -> Result<(), BuilderError> {
        let operator = self.value_codegen(operator)?;

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
            let val = self.value_codegen(arg)?;
            self.builder.build_store(ep, val)?;
        }

        // Call make_application
        let make_app = self.module.get_function("make_application").unwrap();
        let app = self
            .builder
            .build_call(
                make_app,
                &[
                    operator.into(),
                    args_alloca.into(),
                    i32_type.const_int(args.len() as u64, false).into(),
                ],
                "make_app",
            )?
            .try_as_basic_value()
            .left()
            .unwrap();
        let _ = self.builder.build_return(Some(&app))?;

        Ok(())
    }

    fn return_values_codegen(&self, args: &[Value]) -> Result<(), BuilderError> {
        // Allocate space for the args to be passed to make_application
        let i32_type = self.ctx.i32_type();
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());
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
            let val = self.value_codegen(arg)?;
            self.builder.build_store(ep, val)?;
        }

        // Call make_application
        let make_app = self.module.get_function("make_return_values").unwrap();
        let app = self
            .builder
            .build_call(
                make_app,
                &[
                    args_alloca.into(),
                    i32_type.const_int(args.len() as u64, false).into(),
                ],
                "make_return_values",
            )?
            .try_as_basic_value()
            .left()
            .unwrap();
        let _ = self.builder.build_return(Some(&app))?;

        Ok(())
    }

    fn if_codegen(
        &mut self,
        cond: &Value,
        success: Cps,
        failure: Cps,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        let cond = self.value_codegen(cond)?;
        let truthy = self.module.get_function("truthy").unwrap();
        let cond = self
            .builder
            .build_call(truthy, &[cond.into()], "truthy")?
            .try_as_basic_value()
            .left()
            .unwrap();

        // Because our compiler is not particularly sophisticated right now, we can guarantee
        // that both branches terminate. Thus, noc ontinuation basic block.
        let success_bb = self.ctx.append_basic_block(self.function, "success");
        let failure_bb = self.ctx.append_basic_block(self.function, "failure");

        self.builder
            .build_conditional_branch(cond.into_int_value(), success_bb, failure_bb)?;

        self.builder.position_at_end(success_bb);
        self.cps_codegen(success, deferred)?;

        self.builder.position_at_end(failure_bb);
        self.cps_codegen(failure, deferred)?;

        Ok(())
    }

    fn store_codegen(&self, from: &Value, to: &Value) -> Result<(), BuilderError> {
        let from = self.value_codegen(from)?.into();
        let to = self.value_codegen(to)?.into();
        let store = self.module.get_function("store").unwrap();
        let _ = self.builder.build_call(store, &[from, to], "")?;
        Ok(())
    }

    fn make_closure_codegen(
        &mut self,
        bundle: &ClosureBundle<'ctx>,
        cexp: Cps,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        let i32_type = self.ctx.i32_type();
        let bool_type = self.ctx.bool_type();
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());

        // Construct the envs array:
        let num_envs = i32_type.const_int(bundle.env.len() as u64, false);
        let env_type = ptr_type.array_type(bundle.env.len() as u32);
        let env_alloca = self.builder.build_alloca(env_type, "env_alloca")?;

        for (i, var) in bundle.env.iter().enumerate() {
            let ep = unsafe {
                self.builder.build_gep(
                    ptr_type,
                    env_alloca,
                    &[i32_type.const_int(i as u64, false)],
                    "alloca_elem",
                )?
            };
            let val: BasicValueEnum = (*self.rebinds.fetch_bind(&Var::Local(*var))).into();
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
            let val: BasicValueEnum = (*self.rebinds.fetch_bind(&Var::Global(var.clone()))).into();
            self.builder.build_store(ep, val)?;
        }

        let make_closure = if bundle.args.continuation.is_some() {
            self.module
                .get_function("make_closure_with_continuation")
                .unwrap()
        } else {
            self.module.get_function("make_closure").unwrap()
        };
        let closure = self
            .builder
            .build_call(
                make_closure,
                &[
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
                ],
                "make_closure",
            )?
            .try_as_basic_value()
            .left()
            .unwrap();

        self.rebinds
            .rebind(Var::Local(bundle.val), closure.into_pointer_value());

        self.cps_codegen(cexp, deferred)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct ClosureBundle<'ctx> {
    val: Local,
    env: Vec<Local>,
    globals: Vec<Global>,
    args: ClosureArgs,
    body: Cps,
    function: FunctionValue<'ctx>,
}

const ENV_PARAM: u32 = 0;
const GLOBALS_PARAM: u32 = 1;
const ARGS_PARAM: u32 = 2;
const CONTINUATION_PARAM: u32 = 3;

impl<'ctx> ClosureBundle<'ctx> {
    fn new(
        ctx: &'ctx Context,
        module: &Module<'ctx>,
        val: Local,
        // name: String,
        args: ClosureArgs,
        body: Cps,
    ) -> Self {
        // TODO: These calls need to be cached and also calculated at the same time.
        let env = body
            .free_variables()
            .difference(&args.into_vec().into_iter().collect::<HashSet<_>>())
            .cloned()
            .collect::<Vec<_>>();
        let globals = body.globals().into_iter().collect::<Vec<_>>();

        let ptr_type = ctx.ptr_type(AddressSpace::default());

        let fn_type = if args.continuation.is_some() {
            ptr_type.fn_type(
                &[
                    ptr_type.into(), // Env
                    ptr_type.into(), // Globals
                    ptr_type.into(), // Args
                    ptr_type.into(), // Continuation
                ],
                false,
            )
        } else {
            ptr_type.fn_type(
                &[
                    ptr_type.into(), // Env
                    ptr_type.into(), // Globals
                    ptr_type.into(), // Args
                ],
                false,
            )
        };
        let name = val.to_func_name();
        let function = module.add_function(&name, fn_type, None);
        Self {
            val,
            // name,
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
        let ptr_type = ctx.ptr_type(AddressSpace::default());
        let entry = ctx.append_basic_block(self.function, "entry");

        builder.position_at_end(entry);

        let mut cu = CompilationUnit::new(ctx, module, builder, self.function);

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
                .build_extract_value(env_load, i as u32, "extract_global")
                .unwrap()
                .into_pointer_value();
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
                .unwrap()
                .into_pointer_value();
            cu.rebinds.rebind(Var::Global(global.clone()), res);
        }

        let args_param = self
            .function
            .get_nth_param(ARGS_PARAM)
            .unwrap()
            .into_pointer_value();
        let array_type = ptr_type.array_type(self.args.args.len() as u32);
        let args_load = builder
            .build_load(array_type, args_param, "args_load")?
            .into_array_value();

        for (i, arg_var) in self.args.args.iter().enumerate() {
            let res = builder
                .build_extract_value(args_load, i as u32, "extract_arg")
                .unwrap()
                .into_pointer_value();
            cu.rebinds.rebind(Var::Local(*arg_var), res);
        }

        if let Some(cont) = self.args.continuation {
            let cont_param = self
                .function
                .get_nth_param(CONTINUATION_PARAM)
                .unwrap()
                .into_pointer_value();
            cu.rebinds.rebind(Var::Local(cont), cont_param);
        }

        cu.cps_codegen(self.body, deferred)?;

        self.function.print_to_stderr();
        eprintln!();

        if !self.function.verify(true) {
            panic!("Invalid function");
        }

        Ok(())
    }
}
