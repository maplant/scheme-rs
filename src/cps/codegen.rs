//! LLVM SSA Codegen from CPS.

// Example of how codegen will work (most likely)

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    AddressSpace,
};

use super::*;

impl Cps {
    fn codegen(&self, ctx: &Context, builder: &Builder<'_>) {
        todo!()
    }
}

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

pub fn alloc_cell_codegen(
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

pub fn app_codegen(
    ctx: &Context,
    module: &Module<'_>,
    builder: &Builder<'_>,
    var: Local,
    cont: &Cps,
) -> Result<(), BuilderError> {
    // Return a *const Application
    todo!()
}
