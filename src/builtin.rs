use futures::future::BoxFuture;

use crate::{
    ast::Ident,
    eval::{Env, ExternalFn, RuntimeError, Value},
    gc::Gc,
};

type ExprFuture = BoxFuture<'static, Result<Gc<Value>, RuntimeError>>;

pub struct Builtin {
    pub name: &'static str,
    num_args: usize,
    variadic: bool,
    wrapper: fn(Gc<Env>, Vec<Gc<Value>>) -> ExprFuture,
}

impl Builtin {
    pub const fn new(
        name: &'static str,
        num_args: usize,
        variadic: bool,
        wrapper: fn(Gc<Env>, Vec<Gc<Value>>) -> ExprFuture,
    ) -> Self {
        Self {
            name,
            num_args,
            variadic,
            wrapper,
        }
    }

    pub fn install(&self, into: &mut Env) {
        into.define(
            &Ident::new(self.name),
            Gc::new(Value::from(ExternalFn {
                num_args: self.num_args,
                variadic: self.variadic,
                func: self.wrapper,
            })),
        );
    }
}

inventory::collect!(Builtin);
