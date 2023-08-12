use futures::future::BoxFuture;

use crate::{
    eval::{Env, ExternalFn, RuntimeError, Value},
    gc::Gc,
};

pub struct Builtin {
    name: &'static str,
    num_args: usize,
    variadic: bool,
    wrapper: fn(&Gc<Env>, &[Gc<Value>]) -> BoxFuture<'static, Result<Gc<Value>, RuntimeError>>,
}

impl Builtin {
    pub const fn new(
        name: &'static str,
        num_args: usize,
        variadic: bool,
        wrapper: fn(&Gc<Env>, &[Gc<Value>]) -> BoxFuture<'static, Result<Gc<Value>, RuntimeError>>,
    ) -> Self {
        Self {
            name,
            num_args,
            variadic,
            wrapper,
        }
    }

    pub async fn install(&self, into: &Gc<Env>) {
        into.write().await.define(
            self.name,
            Gc::new(Value::from(ExternalFn {
                num_args: self.num_args,
                variadic: self.variadic,
                func: self.wrapper,
            }))
        )
    }
}

inventory::collect!(Builtin);
