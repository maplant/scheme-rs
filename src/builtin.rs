use crate::{
    continuation::Continuation, env::LexicalContour, error::RuntimeError,
    proc::ValuesOrPreparedCall, gc::Gc, proc::ExternalFn, syntax::Identifier, value::Value,
};
use futures::future::BoxFuture;
use std::sync::Arc;

type ExprFuture = BoxFuture<'static, Result<ValuesOrPreparedCall, RuntimeError>>;

pub struct Builtin {
    pub name: &'static str,
    num_args: usize,
    variadic: bool,
    wrapper: fn(Option<Arc<Continuation>>, Vec<Gc<Value>>) -> ExprFuture,
}

impl Builtin {
    pub const fn new(
        name: &'static str,
        num_args: usize,
        variadic: bool,
        wrapper: fn(Option<Arc<Continuation>>, Vec<Gc<Value>>) -> ExprFuture,
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
                name: self.name,
                num_args: self.num_args,
                variadic: self.variadic,
                func: self.wrapper,
            })),
        );
    }
}

inventory::collect!(Builtin);
