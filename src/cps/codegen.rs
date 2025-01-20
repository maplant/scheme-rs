//! LLVM SSA Codegen from CPS.

use std::collections::HashMap;
use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    AddressSpace,
};

use super::*;

struct Rebinds<'a> {
    up: Option<&'a Rebinds<'a>>,
    rebinds: HashMap<Local, Local>,
}

impl Rebinds<'_> {
    fn rebind(&mut self, old_var: Local, new_var: Local) {
        todo!()
    }

    fn fetch_bind(&self, var: Local) -> Local {
        todo!()
    }
}

impl Cps {
    pub fn codegen(&self, ctx: &Context, module: &Module<'_>, builder: &Builder<'_>) -> Result<(), BuilderError> {
        todo!()
    }

    pub fn func_name(&self) -> Option<String> {
        todo!()
    }
}

// 
// List of necessary runtime functions:
//  - fn gc_alloc_undef_val() -> *const Value (allocated from a Gc)
//  - fn make_closure(env: *cont Value, num_env: usize, f_ptr: *const fn()) -> *const Closure (allocated from a Gc)
//  - fn (dec/inc)_ref_count(*const ())

/*
pub fn record_codegen(ctx: &Context, builder: &Builder<'_>, size: usize, var: Local, cont: &Cps) {

    let ptr_type = ctx.ptr_type(AddressSpace::default());
    let _ = builder.build_array_alloca(
        ptr_type,
        ctx.i32_type().const_int(size as u64, false),
        &var.to_string(),
    );
    cont.codegen(ctx, builder);
}
*/

fn create_closure_codegen(
    ctx: &Context,
    module: &Module<'_>,
    builder: &Builder<'_>,
    body: &Cps,
    val: Local,
    rebinds: &Rebinds<'_>,
) -> Result<(), BuilderError> {
    todo!()
}

fn define_closure_func_codegen(
    ctx: &Context,
    module: &Module<'_>,
    builder: &Builder<'_>,
    name: Local,
    args: &[Local],
    body: &Cps,
) -> Result<(), BuilderError> {
    todo!()
}

fn alloc_cell_codegen(
    ctx: &Context,
    module: &Module<'_>,
    builder: &Builder<'_>,
    var: Local,
    cont: &Cps,
) -> Result<(), BuilderError> {
    let ptr_type = ctx.ptr_type(AddressSpace::default());
    let val = builder.build_alloca(ptr_type, &var.to_string())?;
    let gc_alloc_undef_val = module.get_function("gc_alloc_undef_val").unwrap();
    let undef_val = builder
        .build_call(gc_alloc_undef_val, &[], "")?
        .try_as_basic_value()
        .left()
        .unwrap();
    let _store = builder.build_store(val, undef_val);
    Ok(())
}

fn app_codegen(
    ctx: &Context,
    module: &Module<'_>,
    builder: &Builder<'_>,
    var: Local,
    cont: &Cps,
) -> Result<(), BuilderError> {
    // Return a *const Application
    todo!()
}
