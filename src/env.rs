use either::Either;
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{
    gc::{Gc, Trace},
    syntax::{Identifier, Mark},
    value::Value,
};

/// A Top level environment.
pub trait Top: Trace + Send + Sync + 'static {
    fn def_var(&mut self, name: String) -> Var;

    fn def_macro(&mut self, name: String, val: Gc<Value>);

    fn set_var(&self, name: &str, val: Value);

    fn fetch_var(&mut self, name: &str) -> Option<Global>;

    fn fetch_macro(&self, name: &str) -> Option<Gc<Value>>;
}

/// Top level environment, or compilation unit, for a Scheme library.
#[derive(Trace)]
pub struct Library {
    vars: HashMap<String, Gc<Value>>,
    macros: HashMap<String, Gc<Value>>,
}

impl Top for Library {
    fn def_var(&mut self, name: String) -> Var {
        let global = Gc::new(Value::Undefined);
        assert!(self.vars.insert(name.clone(), global.clone()).is_none());
        Var::Global(Global::new(name, global))
    }

    fn def_macro(&mut self, name: String, val: Gc<Value>) {
        self.macros.insert(name, val);
    }

    fn set_var(&self, name: &str, val: Value) {
        *self.vars.get(name).unwrap().write() = val;
    }

    fn fetch_var(&mut self, name: &str) -> Option<Global> {
        self.vars
            .get(name)
            .map(|val| Global::new(name.to_string(), val.clone()))
    }

    fn fetch_macro(&self, name: &str) -> Option<Gc<Value>> {
        self.macros.get(name).cloned()
    }
}

/// Top level environment for a Scheme program.
#[derive(Trace)]
pub struct Program {
    vars: HashMap<String, Gc<Value>>,
    macros: HashMap<String, Gc<Value>>,
}

/// Top level environment for a Scheme REPL.
#[derive(Default, Trace)]
pub struct Repl {
    vars: HashMap<String, Gc<Value>>,
    macros: HashMap<String, Gc<Value>>,
}

/// Repl's implementation of Top differs from Program and Library by assuming
/// that undefined variables are simply yet to be defined.
impl Top for Repl {
    fn def_var(&mut self, name: String) -> Var {
        let global = self
            .vars
            .entry(name.clone())
            .or_insert_with(|| Gc::new(Value::Undefined));
        Var::Global(Global::new(name, global.clone()))
    }

    fn def_macro(&mut self, name: String, val: Gc<Value>) {
        self.macros.insert(name, val);
    }

    fn set_var(&self, name: &str, val: Value) {
        *self.vars.get(name).unwrap().write() = val;
    }

    fn fetch_var(&mut self, name: &str) -> Option<Global> {
        Some(Global::new(
            name.to_string(),
            self.vars
                .entry(name.to_string())
                .or_insert_with(|| Gc::new(Value::Undefined))
                .clone(),
        ))
    }

    fn fetch_macro(&self, name: &str) -> Option<Gc<Value>> {
        self.macros.get(name).cloned()
    }
}

#[derive(Trace)]
pub struct LexicalContour<T: Trace> {
    up: Environment<T>,
    vars: HashMap<Identifier, Local>,
    macros: HashMap<Identifier, Gc<Value>>,
}

impl<T: Top> LexicalContour<T> {
    fn new(env: &Environment<T>) -> Self {
        Self {
            up: env.clone(),
            vars: Default::default(),
            macros: Default::default(),
        }
    }
}

impl<T: Top> Gc<LexicalContour<T>> {
    pub fn def_var(&self, name: Identifier) -> Local {
        let local = Local::gensym();
        self.write().vars.insert(name, local);
        local
    }

    pub fn def_macro(&self, name: Identifier, value: Gc<Value>) {
        self.write().macros.insert(name, value);
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        if let Some(local) = self.read().vars.get(name).copied() {
            return Some(Var::Local(local));
        }
        self.read().up.fetch_var(name)
    }

    pub fn fetch_macro(&self, _name: &Identifier) -> Option<(MacroSource<T>, Gc<Value>)> {
        todo!()
    }
}

#[derive(Trace)]
pub struct MacroExpansion<T: Trace> {
    up: Environment<T>,
    mark: Mark,
    source: Either<Gc<LexicalContour<T>>, Gc<T>>,
}

pub type MacroSource<T> = Either<Gc<LexicalContour<T>>, Gc<T>>;

impl<T: Top> MacroExpansion<T> {
    pub fn new(
        env: &Environment<T>,
        mark: Mark,
        source: Either<Gc<LexicalContour<T>>, Gc<T>>,
    ) -> Self {
        Self {
            up: env.clone(),
            mark,
            source,
        }
    }
}

impl<T: Top> Gc<MacroExpansion<T>> {
    pub fn def_var(&self, name: Identifier) -> Var {
        // In the case of defining variables produced from macro expansions, pass them
        // on to the next environment up.
        self.read().up.def_var(name)
    }

    pub fn def_macro(&self, name: Identifier, val: Gc<Value>) {
        self.read().up.def_macro(name, val);
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        // Attempt to check the up scope first:
        let this = self.read();
        let var = this.up.fetch_var(name);
        if var.is_some() {
            return var;
        }
        // If the current expansion context contains the mark, remove it and check the
        // expansion source scope.
        if name.marks.contains(&this.mark) {
            match this.source {
                Either::Left(ref lex) => {
                    let mut unmarked = name.clone();
                    unmarked.mark(this.mark);
                    lex.fetch_var(&unmarked)
                }
                Either::Right(ref lib) => lib.write().fetch_var(&name.name).map(Var::Global),
            }
        } else {
            None
        }
    }

    pub fn fetch_macro(&self, _name: &Identifier) -> Option<(MacroSource<T>, Gc<Value>)> {
        todo!()
    }
}

#[derive(Trace)]
pub enum Environment<T: Trace> {
    Top(Gc<T>),
    LexicalContour(Gc<LexicalContour<T>>),
    MacroExpansion(Gc<MacroExpansion<T>>),
}

impl<T: Top> Environment<T> {
    pub fn def_var(&self, name: Identifier) -> Var {
        match self {
            Self::Top(top) => top.write().def_var(name.name),
            Self::LexicalContour(lex) => Var::Local(lex.def_var(name)),
            Self::MacroExpansion(me) => me.def_var(name),
        }
    }

    pub fn def_macro(&self, name: Identifier, val: Gc<Value>) {
        match self {
            Self::Top(top) => top.write().def_macro(name.name, val),
            Self::LexicalContour(lex) => lex.def_macro(name, val),
            Self::MacroExpansion(me) => me.def_macro(name, val),
        }
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        match self {
            Self::Top(top) => top.write().fetch_var(&name.name).map(Var::Global),
            Self::LexicalContour(lex) => lex.fetch_var(name),
            Self::MacroExpansion(me) => me.fetch_var(name),
        }
    }

    pub fn fetch_macro(&self, _name: &Identifier) -> Option<(MacroSource<T>, Gc<Value>)> {
        /*
        match self {
            Self::Top(top) => top.read().fetch_macro(&name.name).map(|mac| (Either::Right(top.clone()), mac)),
            Self::LexicalContour(lex) => lex.fetch_macro(name),
            Self::MacroExpansion(me) => me.fetch_macro(name),
        }
         */
        // For now, just None
        None
    }

    pub fn is_bound(&self, name: &Identifier) -> bool {
        self.fetch_var(name).is_some()
    }

    pub fn new_lexical_contour(&self) -> Self {
        let new_lexical_contour = LexicalContour::new(self);
        Self::LexicalContour(Gc::new(new_lexical_contour))
    }

    pub fn new_macro_expansion(
        &self,
        mark: Mark,
        source: Either<Gc<LexicalContour<T>>, Gc<T>>,
    ) -> Self {
        let new_macro_expansion = MacroExpansion::new(self, mark, source);
        Self::MacroExpansion(Gc::new(new_macro_expansion))
    }
}

impl Environment<Repl> {
    pub fn new_repl() -> Self {
        Self::Top(Gc::new(Repl::default()))
    }
}

impl<T: Top> Clone for Environment<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Top(top) => Self::Top(top.clone()),
            Self::LexicalContour(lex) => Self::LexicalContour(lex.clone()),
            Self::MacroExpansion(mac) => Self::MacroExpansion(mac.clone()),
        }
    }
}

#[derive(Copy, Clone, Trace, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Local(usize);

impl Local {
    /// Create a new temporary value.
    pub fn gensym() -> Self {
        static NEXT_SYM: AtomicUsize = AtomicUsize::new(0);
        Self(NEXT_SYM.fetch_add(1, Ordering::Relaxed))
    }

    pub fn to_func_name(&self) -> String {
        format!("f{}", self.0)
    }
}

impl ToString for Local {
    fn to_string(&self) -> String {
        format!("%{}", self.0)
    }
}

impl fmt::Debug for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Clone, Trace, Hash, PartialEq, Eq)]
pub struct Global {
    name: String,
    val: Gc<Value>,
}

impl Global {
    pub fn new(name: String, val: Gc<Value>) -> Self {
        Global { name, val }
    }

    pub fn value(self) -> Gc<Value> {
        self.val
    }
}

impl fmt::Debug for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.name)
    }
}

#[derive(Clone, Trace, Hash, PartialEq, Eq)]
pub enum Var {
    Global(Global),
    Local(Local),
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Global(global) => global.fmt(f),
            Self::Local(local) => local.fmt(f),
        }
    }
}
