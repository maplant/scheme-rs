//! LLVM SSA Codegen from CPS.

use either::Either;
use indexmap::IndexSet;
use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use std::collections::HashMap;

use crate::proc::{Closure, SyncFuncPtr};

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
        self.rebinds.get(var).unwrap()
        // .unwrap_or_else(|| self.up.as_ref().unwrap().fetch_bind(var))
    }

    /*
    fn new_scope(&'ctx self) -> Rebinds<'ctx> {
        Self {
            up: Some(self),
            rebinds: HashMap::new(),
        }
    }
     */
    fn new() -> Self {
        Self {
            rebinds: HashMap::default(),
        }
    }
}

impl Cps {
    pub fn into_closure<'ctx, 'b>(
        self,
        ctx: &'ctx Context,
        module: &Module<'ctx>,
        ee: &ExecutionEngine<'ctx>,
        builder: &'b Builder<'ctx>,
    ) -> Result<Closure, BuilderError>
    where
        'ctx: 'b,
    {
        let i32_type = ctx.i32_type();
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
        let mut cu = CompilationUnit::new(ctx, module.clone(), builder, function);

        for (i, global) in globals.iter().enumerate() {
            let globals = function
                .get_nth_param(GLOBALS_PARAM)
                .unwrap()
                .into_pointer_value();
            let res = unsafe {
                cu.builder.build_gep(
                    ptr_type,
                    globals,
                    &[i32_type.const_int(i as u64, false)],
                    "global",
                )?
            };
            cu.rebinds.rebind(Var::Global(global.clone()), res);
        }

        let mut deferred = Vec::new();
        cu.cps_codegen(self, &mut deferred)?;

        while let Some(next) = deferred.pop() {
            next.codegen(ctx, module.clone(), builder, &mut deferred)?;
        }

        let func = unsafe { ee.get_function::<SyncFuncPtr>(&name).unwrap().into_raw() };

        Ok(Closure::new(
            Vec::new(),
            globals.into_iter().map(Global::value).collect(),
            Either::Left(func),
        ))
    }
}

struct CompilationUnit<'ctx, 'b> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
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
// List of necessary runtime functions:
//  - fn gc_alloc_undef_val() -> *const Value (allocated from a Gc)
//  - fn make_closure(env: *const Value, num_env: u32, globals: *const Value, num_globals: u32, f_ptr: *const fn()) -> *const Closure (allocated from a Gc)
//  - fn make_application(op: *const Value, args: *const *const Value, num_args: usize) -> *const Application
//  - fn (dec/inc)_ref_count(*const ())
//  - fn store(Gc<Value>, Box<Value>)
//  - fn truthy(*const Value) -> bool
//  - fn i64_to_number(i32) -> *const Value

impl<'ctx, 'b> CompilationUnit<'ctx, 'b> {
    fn new(
        ctx: &'ctx Context,
        module: Module<'ctx>,
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
        let gc_alloc_undef_val = self.module.get_function("gc_alloc_undef_val").unwrap();
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

    fn app_codegen(&self, operator: &Value, args: &[Value]) -> Result<(), BuilderError> {
        let operator = self.value_codegen(operator)?;

        // Allocate space for the args to be passed to make_application
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());
        let array_type = ptr_type.array_type(args.len() as u32);
        let args_alloca = self.builder.build_alloca(array_type, "args")?;
        let args_load = self
            .builder
            .build_load(ptr_type, args_alloca, "args_load")?
            .into_array_value();
        for (i, arg) in args.iter().enumerate() {
            let arg = self.value_codegen(arg)?;
            self.builder
                .build_insert_value(args_load, arg, i as u32, "args_insert")?;
        }

        // Call make_application
        let make_app = self.module.get_function("make_application").unwrap();
        let app = self
            .builder
            .build_call(make_app, &[operator.into(), args_alloca.into()], "make_app")?
            .try_as_basic_value()
            .left()
            .unwrap();
        let _ = self.builder.build_return(Some(&app))?;

        Ok(())
    }

    fn return_values_codegen(&self, args: &[Value]) -> Result<(), BuilderError> {
        // Allocate space for the args to be passed to make_application
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());
        let array_type = ptr_type.array_type(args.len() as u32);
        let args_alloca = self.builder.build_alloca(array_type, "args")?;
        let args_load = self
            .builder
            .build_load(ptr_type, args_alloca, "args_load")?
            .into_array_value();
        for (i, arg) in args.iter().enumerate() {
            let arg = self.value_codegen(arg)?;
            self.builder
                .build_insert_value(args_load, arg, i as u32, "args_insert")?;
        }

        // Call make_application
        let make_app = self.module.get_function("make_return_values").unwrap();
        let app = self
            .builder
            .build_call(make_app, &[args_alloca.into()], "make_return_values")?
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
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());

        // Construct the envs array:
        let num_envs = i32_type.const_int(bundle.env.len() as u64, false);
        let env_type = ptr_type.array_type(bundle.env.len() as u32);
        let env_alloca = self.builder.build_alloca(env_type, "env_alloca")?;
        let env = self
            .builder
            .build_load(ptr_type, env_alloca, "env_load")?
            .into_array_value();

        for (i, var) in bundle.env.iter().enumerate() {
            let val: BasicValueEnum = (*self.rebinds.fetch_bind(&Var::Local(*var))).into();
            self.builder
                .build_insert_value(env, val, i as u32, "insert_env")?;
        }

        // Construct the globals array:
        let num_globals = i32_type.const_int(bundle.globals.len() as u64, false);
        let globals_type = ptr_type.array_type(bundle.globals.len() as u32);
        let globals_alloca = self.builder.build_alloca(globals_type, "globals_alloca")?;
        let globals = self
            .builder
            .build_load(ptr_type, globals_alloca, "globals_load")?
            .into_array_value();

        for (i, var) in bundle.globals.iter().enumerate() {
            let val: BasicValueEnum = (*self.rebinds.fetch_bind(&Var::Global(var.clone()))).into();
            self.builder
                .build_insert_value(globals, val, i as u32, "insert_global")?;
        }

        let make_closure = self.module.get_function("make_closure").unwrap();
        let closure = self
            .builder
            .build_call(
                make_closure,
                &[
                    env_alloca.into(),
                    num_envs.into(),
                    globals_alloca.into(),
                    num_globals.into(),
                    bundle.function.as_global_value().as_pointer_value().into(),
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

pub struct ClosureBundle<'ctx> {
    val: Local,
    // name: String,
    env: Vec<Local>,
    globals: Vec<Global>,
    args: Vec<Local>,
    body: Cps,
    function: FunctionValue<'ctx>,
}

const ENV_PARAM: u32 = 0;
const GLOBALS_PARAM: u32 = 1;
const ARGS_PARAM: u32 = 2;

impl<'ctx> ClosureBundle<'ctx> {
    fn new(
        ctx: &'ctx Context,
        module: &Module<'ctx>,
        val: Local,
        // name: String,
        args: Vec<Local>,
        body: Cps,
    ) -> Self {
        // TODO: These calls need to be cached and also done at the same time.
        let env = body.free_variables().into_iter().collect::<Vec<_>>();
        let globals = body.globals().into_iter().collect::<Vec<_>>();

        let ptr_type = ctx.ptr_type(AddressSpace::default());

        let fn_type = ptr_type.fn_type(
            &[
                ptr_type.into(), // Env
                ptr_type.into(), // Globals
                ptr_type.into(), // Args
            ],
            false,
        );
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
        module: Module<'ctx>,
        builder: &'b Builder<'ctx>,
        deferred: &mut Vec<Self>,
    ) -> Result<(), BuilderError> {
        let i32_type = ctx.i32_type();
        let ptr_type = ctx.ptr_type(AddressSpace::default());
        let entry = ctx.append_basic_block(self.function, "entry");

        builder.position_at_end(entry);

        let mut cu = CompilationUnit::new(ctx, module, builder, self.function);

        for (i, env_var) in self.env.iter().enumerate() {
            let envs = self
                .function
                .get_nth_param(ENV_PARAM)
                .unwrap()
                .into_pointer_value();
            let res = unsafe {
                cu.builder.build_gep(
                    ptr_type,
                    envs,
                    &[i32_type.const_int(i as u64, false)],
                    "env",
                )?
            };
            cu.rebinds.rebind(Var::Local(*env_var), res);
        }

        for (i, global_var) in self.globals.iter().enumerate() {
            let globals = self
                .function
                .get_nth_param(GLOBALS_PARAM)
                .unwrap()
                .into_pointer_value();
            let res = unsafe {
                cu.builder.build_gep(
                    ptr_type,
                    globals,
                    &[i32_type.const_int(i as u64, false)],
                    "global",
                )?
            };
            cu.rebinds.rebind(Var::Global(global_var.clone()), res);
        }

        for (i, arg_var) in self.args.iter().enumerate() {
            let args = self
                .function
                .get_nth_param(ARGS_PARAM)
                .unwrap()
                .into_pointer_value();
            let res = unsafe {
                cu.builder.build_gep(
                    ptr_type,
                    args,
                    &[i32_type.const_int(i as u64, false)],
                    "arg",
                )?
            };
            cu.rebinds.rebind(Var::Local(*arg_var), res);
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
