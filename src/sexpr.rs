use crate::{
    ast::Literal,
    eval::{Env, Eval, Value},
    expand::Binds,
    gc::Gc,
    lex::Lexeme,
};
use futures::future::BoxFuture;
use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
};

#[derive(Clone, PartialEq)]
pub enum SExpr {
    Nil,
    List(Vec<SExpr>),
    Vector(Vec<SExpr>),
    Literal(Literal),
    Identifier(Ident),
}

impl SExpr {
    fn expand<'a>(self, env: &'a Gc<Env>, binds: &'a Binds<'_>) -> BoxFuture<'a, SExpr> {
        Box::pin(async move {
            match self {
                Self::List(mut list) => {
                    let (head, tail) = list.split_first().unwrap();
                    let head = match head.clone() {
                        list @ Self::List(_) => list.expand(env, binds).await,
                        x => x,
                    };
                    if !matches!(head, Self::Identifier(_)) {
                        let mut output = vec![head];
                        for item in tail {
                            output.push(item.clone().expand(env, binds).await);
                        }
                        return SExpr::List(output);
                    };
                    let mut list = vec![head];
                    list.extend(tail.iter().cloned());
                    let mut list = SExpr::List(list);
                    // Check if current ident is a macro
                    while let Value::Transformer(transformer) = todo!() {
                        list = transformer.expand(&list, binds).unwrap();
                    }
                    // TODO: Check if lambda, define or let
                    let list: Vec<_> = todo!();
                    let mut output = Vec::<SExpr>::new();
                    let mut items = list.into_iter();
                    output.push(items.next().unwrap());
                    for item in items {
                        output.push(item.expand(env, binds).await)
                    }
                    Self::List(output)
                }
                x => x,
            }
        })
    }

    fn compile_with_binds<'a>(
        self,
        env: &'a Gc<Env>,
        binds: &'a Binds<'_>,
    ) -> BoxFuture<'a, Box<dyn Eval>> {
        Box::pin(async move {
            let expr = self.expand(env, binds).await;
            todo!()
        })
    }

    pub fn compile<'a>(self, env: &'a Gc<Env>) -> BoxFuture<'a, Box<dyn Eval>> {
        Box::pin(async move {
            let binds = Binds::new(env).await;
            self.compile_with_binds(env, &binds).await
        })
    }

    pub fn parse(i: &[Lexeme<'_>]) -> Self {
        todo!()
    }
}

#[derive(Clone)]
pub struct Ident {
    macro_env: Option<Gc<Env>>,
    pub sym: String,
}

impl Ident {
    pub fn new_free(sym: String) -> Self {
        Self {
            macro_env: None,
            sym,
        }
    }

    pub fn new_macro(sym: &str, macro_env: &Gc<Env>) -> Self {
        Self {
            macro_env: Some(macro_env.clone()),
            sym: sym.to_string(),
        }
    }

    pub async fn lookup(&self, env: &Gc<Env>) -> Gc<Value> {
        // If macro env is set, use that. Otherwise, use env.
        todo!()
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.macro_env.as_ref().map(Gc::as_ptr).hash(state);
        self.sym.hash(state);
    }
}

impl PartialEq for Ident {
    fn eq(&self, rhs: &Ident) -> bool {
        self.macro_env.as_ref().map(Gc::as_ptr) == rhs.macro_env.as_ref().map(Gc::as_ptr)
            && self.sym == rhs.sym
    }
}

impl Eq for Ident {}
