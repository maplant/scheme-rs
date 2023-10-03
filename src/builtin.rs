use futures::future::BoxFuture;

use crate::{
    env::{Env, LexicalContour},
    error::RuntimeError,
    gc::Gc,
    proc::ExternalFn,
    syntax::Identifier,
    value::Value,
};

type ExprFuture = BoxFuture<'static, Result<Gc<Value>, RuntimeError>>;

pub struct Builtin {
    pub name: &'static str,
    num_args: usize,
    variadic: bool,
    wrapper: fn(Env, Vec<Gc<Value>>) -> ExprFuture,
}

impl Builtin {
    pub const fn new(
        name: &'static str,
        num_args: usize,
        variadic: bool,
        wrapper: fn(Env, Vec<Gc<Value>>) -> ExprFuture,
    ) -> Self {
        Self {
            name,
            num_args,
            variadic,
            wrapper,
        }
    }

    pub fn install(&self, into: &mut LexicalContour) {
        into.def_var(
            &Identifier::new(self.name.to_string()),
            Gc::new(Value::from(ExternalFn {
                num_args: self.num_args,
                variadic: self.variadic,
                func: self.wrapper,
            })),
        );
    }
}

inventory::collect!(Builtin);
