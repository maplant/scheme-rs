//! LLVM SSA Codegen from CPS.

// Example of how codegen will work (most likely)

use inkwell::{builder::Builder, context::Context, AddressSpace};

use super::*;

impl Cps {
    fn codegen(&self, ctx: &Context, builder: &Builder<'_>) {
        todo!()
    }
}

pub fn record_codegen(ctx: &Context, builder: &Builder<'_>, size: usize, var: CpsVar, cont: &Cps) {
    let ptr_type = ctx.ptr_type(AddressSpace::default());
    let _ = builder.build_array_alloca(
        ptr_type,
        ctx.i32_type().const_int(size as u64, false),
        &var.to_string(),
    );
    cont.codegen(ctx, builder);
}
