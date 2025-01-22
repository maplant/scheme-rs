//! LLVM SSA Codegen from CPS.

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use std::collections::HashMap;

use crate::proc::Closure;

use super::*;

struct Rebinds<'ctx> {
    up: Option<&'ctx Rebinds<'ctx>>,
    rebinds: HashMap<Var, PointerValue<'ctx>>,
}

impl<'ctx> Rebinds<'ctx> {
    fn rebind(&mut self, old_var: Var, new_var: PointerValue<'ctx>) {
        self.rebinds.insert(old_var, new_var);
    }

    fn fetch_bind(&self, var: &Var) -> &PointerValue<'ctx> {
        self.rebinds
            .get(var)
            .unwrap_or_else(|| self.up.as_ref().unwrap().fetch_bind(var))
    }
}

impl Cps {
    pub fn into_closure(
        self,
        ctx: &Context,
        module: &Module<'_>,
        builder: &Builder<'_>,
    ) -> Closure {
        todo!()
    }
}

struct CompilationUnit<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    function: FunctionValue<'ctx>,
    rebinds: Rebinds<'ctx>,
}

//
// List of necessary runtime functions:
//  - fn gc_alloc_undef_val() -> *const Value (allocated from a Gc)
//  - fn make_closure(env: *const Value, num_env: usize, f_ptr: *const fn()) -> *const Closure (allocated from a Gc)
//  - fn make_application(op: *const Value, args: *const *const Value, num_args: usize) -> *const Application
//  - fn (dec/inc)_ref_count(*const ())
//  - fn store(Gc<Value>, Box<Value>)
//  - fn truthy(*const Value) -> bool
//  - fn i64_to_number(i32) -> *const Value

impl<'ctx> CompilationUnit<'ctx> {
    fn cps_codegen(&mut self, cps: &Cps) -> Result<(), BuilderError> {
        match cps {
            Cps::AllocCell(into, cexpr) => {
                self.alloc_cell_codegen(*into)?;
                self.cps_codegen(cexpr)?;
            }
            Cps::If(cond, succ, fail) => self.if_codegen(cond, succ, fail)?,
            Cps::App(operator, args) => self.app_codegen(operator, args)?,
            Cps::PrimOp(PrimOp::Set, args, _, cexpr) => {
                self.store_codegen(&args[1], &args[0])?;
                self.cps_codegen(cexpr)?;
            }
            // TODO: The big kahuna: Closure
            _ => todo!(),
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

    fn if_codegen(&mut self, cond: &Value, succ: &Cps, fail: &Cps) -> Result<(), BuilderError> {
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
        let succ_bb = self.ctx.append_basic_block(self.function, "succ");
        let fail_bb = self.ctx.append_basic_block(self.function, "fail");

        self.builder
            .build_conditional_branch(cond.into_int_value(), succ_bb, fail_bb)?;

        self.builder.position_at_end(succ_bb);
        self.cps_codegen(succ)?;

        self.builder.position_at_end(fail_bb);
        self.cps_codegen(fail)?;

        Ok(())
    }

    fn store_codegen(&self, from: &Value, to: &Value) -> Result<(), BuilderError> {
        let from = self.value_codegen(from)?.into();
        let to = self.value_codegen(to)?.into();
        let store = self.module.get_function("store").unwrap();
        let _ = self.builder.build_call(store, &[from, to], "")?;
        Ok(())
    }
}
