//! LLVM SSA Codegen from CPS.

use indexmap::IndexMap;
use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use std::{collections::HashMap, rc::Rc};

use crate::{
    gc::Gc,
    proc::{Closure, ContinuationPtr, FuncPtr},
    num::Number,
    runtime::Runtime,
    value::Value as SchemeValue,
};

use super::*;

struct Rebinds<'ctx> {
    rebinds: HashMap<Var, PointerValue<'ctx>>,
}

impl<'ctx> Rebinds<'ctx> {
    fn rebind(&mut self, old_var: Var, new_var: PointerValue<'ctx>) {
        self.rebinds.insert(old_var, new_var);
    }

    fn fetch_bind(&self, var: &Var) -> &PointerValue<'ctx> {
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

struct Allocs<'ctx> {
    prev_alloc: Option<Rc<Allocs<'ctx>>>,
    value: PointerValue<'ctx>,
}

impl<'ctx> Allocs<'ctx> {
    fn new(
        prev_alloc: Option<Rc<Allocs<'ctx>>>,
        value: PointerValue<'ctx>,
    ) -> Option<Rc<Allocs<'ctx>>> {
        Some(Rc::new(Allocs { prev_alloc, value }))
    }

    fn to_values(&self) -> Vec<PointerValue<'ctx>> {
        let mut allocs = vec![self.value];

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
                ptr_type.into(), // ExceptionHandler
            ],
            false,
        );
        let fn_value = Local::gensym();
        let fn_name = fn_value.to_func_name();
        let function = module.add_function(&fn_name, fn_type, None);

        let mut cu = CompilationUnit::new(ctx, module, builder, function);
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
                .unwrap()
                .into_pointer_value();
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
                .unwrap()
                .into_pointer_value();
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

// Everything returns a pointer allocated from a Gc, so everything is a Gc<Value>.
//
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
        allocs: Option<Rc<Allocs<'ctx>>>,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        match cps {
            Cps::AllocCell(into, cexpr) => {
                self.alloc_cell_codegen(into, *cexpr, allocs, deferred)?;
            }
            Cps::If(cond, success, failure) => {
                self.if_codegen(&cond, *success, *failure, allocs, deferred)?
            }
            Cps::App(operator, args) => self.app_codegen(&operator, &args, allocs)?,
            Cps::Forward(operator, arg) => self.forward_codegen(&operator, &arg, allocs)?,
            Cps::PrimOp(PrimOp::Set, args, _, cexpr) => {
                self.store_codegen(&args[1], &args[0])?;
                self.cps_codegen(*cexpr, allocs, deferred)?;
            }
            Cps::PrimOp(PrimOp::CloneClosure, proc, val, cexpr) => {
                let [proc] = proc.as_slice() else {
                    unreachable!()
                };
                self.clone_closure_codegen(proc, val, *cexpr, allocs, deferred)?;
            }
            Cps::PrimOp(PrimOp::GetCallTransformerFn, _, res, cexpr) => {
                self.get_call_transformer_codegen(res)?;
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
                ..
            } => {
                let bundle = ClosureBundle::new(
                    self.ctx,
                    self.module,
                    val,
                    args.clone(),
                    body.as_ref().clone(),
                );
                self.make_closure_codegen(&bundle, *cexp, allocs, deferred)?;
                deferred.push(bundle);
            }
            Cps::Halt(value) => self.halt_codegen(&value, allocs)?,
        }
        Ok(())
    }

    fn value_codegen(&self, value: &Value) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match value {
            Value::Var(var) => Ok((*self.rebinds.fetch_bind(var)).into()),
            Value::Literal(Literal::Number(num)) => {
                // FIXME: Number has to be a u64
                let num: u64 = u64::try_from(usize::try_from(num).unwrap()).unwrap();
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

    fn clone_closure_codegen(
        &mut self,
        proc: &Value,
        new_var: Local,
        cexpr: Cps,
        allocs: Option<Rc<Allocs<'ctx>>>,
        deferred: &mut Vec<ClosureBundle<'ctx>>,
    ) -> Result<(), BuilderError> {
        let proc = self.value_codegen(proc)?.into_pointer_value();
        let clone_env = self.module.get_function("clone_closure").unwrap();
        let cloned = self
            .builder
            .build_call(clone_env, &[proc.into()], "clone_closure")?
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();
        self.rebinds.rebind(Var::Local(new_var), cloned);
        let new_alloc = Allocs::new(allocs, cloned);
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
            let val = self.value_codegen(val)?;
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

        let runtime_fn = self.module.get_function(runtime_fn_name).unwrap();
        let result_val = self
            .builder
            .build_call(
                runtime_fn,
                &[
                    vals_alloca.into(),
                    i32_type.const_int(num_vals as u64, false).into(),
                ],
                runtime_fn_name,
            )?
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

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
        let gc_alloc_undef_val = self.module.get_function("alloc_undef_val").unwrap();
        let undef_val = self
            .builder
            .build_call(gc_alloc_undef_val, &[], "undefined")?
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        // Rebind the variable to it
        self.rebinds.rebind(Var::Local(var), undef_val);

        let new_alloc = Allocs::new(allocs, undef_val);

        // Compile the continuation with the newly allocated value
        self.cps_codegen(cexpr, new_alloc, deferred)?;

        Ok(())
    }

    fn drop_values_codegen(&self, drops: Option<Rc<Allocs<'ctx>>>) -> Result<(), BuilderError> {
        let drops = drops.as_ref().map_or_else(Vec::new, |x| x.to_values());
        let num_drops = drops.len();

        if num_drops == 0 {
            return Ok(());
        }

        // Put the drops in an array
        let ptr_type = self.ctx.ptr_type(AddressSpace::default());
        let i32_type = self.ctx.i32_type();
        let array_type = ptr_type.array_type(drops.len() as u32);
        let drops_alloca = self.builder.build_alloca(array_type, "drops")?;
        for (i, drp) in drops.into_iter().enumerate() {
            let ep = unsafe {
                self.builder.build_gep(
                    ptr_type,
                    drops_alloca,
                    &[i32_type.const_int(i as u64, false)],
                    "alloca_elem",
                )?
            };
            self.builder.build_store(ep, drp)?;
        }

        // Call drop_values
        let drop_values = self.module.get_function("drop_values").unwrap();
        self.builder.build_call(
            drop_values,
            &[
                drops_alloca.into(),
                i32_type.const_int(num_drops as u64, false).into(),
            ],
            "drop_values",
        )?;

        Ok(())
    }

    fn app_codegen(
        &self,
        operator: &Value,
        args: &[Value],
        allocs: Option<Rc<Allocs<'ctx>>>,
    ) -> Result<(), BuilderError> {
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
                    self.function
                        .get_nth_param(EXCEPTION_HANDLER_PARAM)
                        .unwrap()
                        .into_pointer_value()
                        .into(),
                ],
                "make_application",
            )?
            .try_as_basic_value()
            .left()
            .unwrap();

        // Now that we have created an application, we can reduce the ref counts of
        // all of the Gcs we have allocated in this function:
        self.drop_values_codegen(allocs)?;

        let _ = self.builder.build_return(Some(&app))?;

        Ok(())
    }

    fn forward_codegen(
        &self,
        operator: &Value,
        arg: &Value,
        allocs: Option<Rc<Allocs<'ctx>>>,
    ) -> Result<(), BuilderError> {
        let operator = self.value_codegen(operator)?;
        let arg = self.value_codegen(arg)?;

        // Call make_forward
        let make_forward = self.module.get_function("make_forward").unwrap();
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
                ],
                "make_forward",
            )?
            .try_as_basic_value()
            .left()
            .unwrap();

        // Now that we have created an application, we can reduce the ref counts of
        // all of the Gcs we have allocated in this function:
        self.drop_values_codegen(allocs)?;

        let _ = self.builder.build_return(Some(&app))?;

        Ok(())
    }

    fn halt_codegen(
        &self,
        args: &Value,
        allocs: Option<Rc<Allocs<'ctx>>>,
    ) -> Result<(), BuilderError> {
        let val = self.value_codegen(args)?;
        // Call make_return_values
        let make_app = self.module.get_function("make_return_values").unwrap();
        let app = self
            .builder
            .build_call(make_app, &[val.into()], "make_return_values")?
            .try_as_basic_value()
            .left()
            .unwrap();

        self.drop_values_codegen(allocs)?;

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
        self.cps_codegen(success, allocs.clone(), deferred)?;

        self.builder.position_at_end(failure_bb);
        self.cps_codegen(failure, allocs, deferred)?;

        Ok(())
    }

    fn store_codegen(&self, from: &Value, to: &Value) -> Result<(), BuilderError> {
        let from = self.value_codegen(from)?.into();
        let to = self.value_codegen(to)?.into();
        let store = self.module.get_function("store").unwrap();
        let _ = self.builder.build_call(store, &[from, to], "")?;
        Ok(())
    }

    fn get_call_transformer_codegen(&mut self, result: Local) -> Result<(), BuilderError> {
        let get_call_transformer_fn = self.module.get_function("get_call_transformer_fn").unwrap();
        let expanded = self
            .builder
            .build_call(
                get_call_transformer_fn,
                &[self
                    .function
                    .get_nth_param(RUNTIME_PARAM)
                    .unwrap()
                    .into_pointer_value()
                    .into()],
                "call_transformer",
            )?
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();
        self.rebinds.rebind(Var::Local(result), expanded);
        Ok(())
    }

    fn make_closure_codegen(
        &mut self,
        bundle: &ClosureBundle<'ctx>,
        cexp: Cps,
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
            self.module.get_function("make_closure").unwrap()
        } else {
            self.module.get_function("make_continuation").unwrap()
        };
        let closure = self
            .builder
            .build_call(
                make_closure,
                &[
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
                ],
                "make_closure",
            )?
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        self.rebinds.rebind(Var::Local(bundle.val), closure);

        let new_alloc = Allocs::new(allocs, closure);

        self.cps_codegen(cexp, new_alloc, deferred)?;

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

const RUNTIME_PARAM: u32 = 0;
const ENV_PARAM: u32 = 1;
const GLOBALS_PARAM: u32 = 2;
const ARGS_PARAM: u32 = 3;
const EXCEPTION_HANDLER_PARAM: u32 = 4;
const CONTINUATION_PARAM: u32 = 5;
// const DYNAMIC_WIND_PARAM: u32 = 5;

impl<'ctx> ClosureBundle<'ctx> {
    fn new(
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
                ],
                false,
            )
        };
        let name = val.to_func_name();
        let function = module.add_function(&name, fn_type, None);
        Self {
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
                .build_extract_value(env_load, i as u32, "extract_env")
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
