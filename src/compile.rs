use crate::{
    eval::{Env, Eval},
    expand::Binds,
    gc::Gc,
    sexpr::SExpr,
};
use futures::future::BoxFuture;

pub enum CompileLetError {}

pub fn compile_let<'a>(
    expr: SExpr,
    gc: &Gc<Env>,
    binds: &'a Binds<'_>,
) -> BoxFuture<'a, Result<Box<dyn Eval>, CompileLetError>> {
    todo!()
}
