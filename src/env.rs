use std::{
    collections::HashMap,
    fmt,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{
    gc::{Gc, Trace},
    proc::Closure,
    syntax::{Identifier, Mark},
    value::Value,
};

/// A Top level environment.
pub trait Top: Trace + Send + Sync + 'static + Sized {
    fn def_var(&mut self, name: Identifier, value: Value) -> Var;

    fn def_macro(&mut self, name: Identifier, mac: Macro<Self>);

    fn fetch_var(&mut self, name: &Identifier) -> Option<Global>;

    fn fetch_macro(&self, name: &Identifier) -> Option<Macro<Self>>;

    fn import<'a>(&mut self, vars: impl Iterator<Item = (&'a str, &'a Gc<Value>)>);
}

/// Top level environment, or compilation unit, for a Scheme library.
#[derive(Default, Trace)]
pub struct Library {
    vars: HashMap<Identifier, Gc<Value>>,
    macros: HashMap<Identifier, Macro<Self>>,
}

impl Library {
    // TODO: Remove all identifier that have a mark.
    pub fn exported_vars<'a>(&'a self) -> impl Iterator<Item = (&'a str, &'a Gc<Value>)> {
        self.vars.iter().map(|(k, v)| (k.name.as_str(), v))
    }

    /*
    pub fn exported_macros<'a>(&'a self) -> impl Iterator<Item = (&'a str, &'a Macro<Self>)> {
        self.macros.iter().map(|(k, v)| (k.as_str(), v))
    }
    */
}

impl Top for Library {
    fn def_var(&mut self, name: Identifier, val: Value) -> Var {
        let global = Gc::new(val);
        assert!(self.vars.insert(name.clone(), global.clone()).is_none());
        Var::Global(Global::new(name, global))
    }

    fn def_macro(&mut self, name: Identifier, mac: Macro<Self>) {
        self.macros.insert(name, mac);
    }

    fn fetch_var(&mut self, name: &Identifier) -> Option<Global> {
        self.vars
            .get(name)
            .map(|val| Global::new(name.clone(), val.clone()))
    }

    fn fetch_macro(&self, name: &Identifier) -> Option<Macro<Self>> {
        self.macros.get(name).cloned()
    }

    fn import<'a>(&mut self, vars: impl Iterator<Item = (&'a str, &'a Gc<Value>)>) {
        for (name, val) in vars {
            self.vars
                .insert(Identifier::new(name.to_string()), val.clone());
        }
    }
}

/*
/// Top level environment for a Scheme program.
#[derive(Trace)]
pub struct Program {
    vars: HashMap<Identifier, Gc<Value>>,
    macros: HashMap<Identifier, Macro<Program>>,
}
*/

/// Top level environment for a Scheme REPL.
#[derive(Default, Trace)]
pub struct Repl {
    vars: HashMap<Identifier, Gc<Value>>,
    macros: HashMap<Identifier, Macro<Repl>>,
}

/// Repl's implementation of Top differs from Program and Library by assuming
/// that undefined variables are simply yet to be defined.
impl Top for Repl {
    fn def_var(&mut self, name: Identifier, val: Value) -> Var {
        let global = self
            .vars
            .entry(name.clone())
            .or_insert_with(|| Gc::new(val));
        Var::Global(Global::new(name, global.clone()))
    }

    fn def_macro(&mut self, name: Identifier, mac: Macro<Self>) {
        self.macros.insert(name, mac);
    }

    fn fetch_var(&mut self, name: &Identifier) -> Option<Global> {
        self.vars
            .get(name)
            .map(|val| Global::new(name.clone(), val.clone()))
        /*
        Some(Global::new(
            name.clone(),
            self.vars
                .entry(name.clone())
                .or_insert_with(|| Gc::new(Value::Undefined))
                .clone(),
        ))
         */
    }

    fn fetch_macro(&self, name: &Identifier) -> Option<Macro<Self>> {
        self.macros.get(name).cloned()
    }

    fn import<'a>(&mut self, vars: impl Iterator<Item = (&'a str, &'a Gc<Value>)>) {
        for (name, val) in vars {
            self.vars
                .insert(Identifier::new(name.to_string()), val.clone());
        }
    }
}

#[derive(Trace)]
pub struct LexicalContour<T: Trace> {
    up: Environment<T>,
    vars: HashMap<Identifier, Local>,
    macros: HashMap<Identifier, Closure>,
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

    pub fn def_macro(&self, name: Identifier, closure: Closure) {
        self.write().macros.insert(name, closure);
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        if let Some(local) = self.read().vars.get(name) {
            return Some(Var::Local(*local));
        }
        self.read().up.fetch_var(name)
    }

    pub fn fetch_macro(&self, name: &Identifier) -> Option<Macro<T>> {
        if let Some(trans) = self.read().macros.get(name) {
            return Some(Macro::new(MacroSource::from(self.clone()), trans.clone()));
        }
        self.read().up.fetch_macro(name)
    }
}

#[derive(Trace)]
pub struct MacroExpansion<T: Trace> {
    up: Environment<T>,
    mark: Mark,
    source: MacroSource<T>,
}

impl<T: Trace + Top> MacroExpansion<T> {
    pub fn new(env: &Environment<T>, mark: Mark, source: MacroSource<T>) -> Self {
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

    pub fn def_macro(&self, name: Identifier, closure: Closure) {
        self.read().up.def_macro(name, closure);
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
        name.marks
            .contains(&this.mark)
            .then(|| {
                let mut unmarked = name.clone();
                unmarked.mark(this.mark);
                this.source.fetch_var(&unmarked)
            })
            .flatten()
    }

    pub fn fetch_macro(&self, name: &Identifier) -> Option<Macro<T>> {
        // Attempt to check the up scope first:
        let this = self.read();
        let mac = this.up.fetch_macro(name);
        if mac.is_some() {
            return mac;
        }
        // If the current expansion context contains the mark, remove it and check the
        // expansion source scope.
        name.marks
            .contains(&this.mark)
            .then(|| {
                let mut unmarked = name.clone();
                unmarked.mark(this.mark);
                this.source.fetch_macro(&unmarked)
            })
            .flatten()
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
            Self::Top(top) => top.write().def_var(name, Value::Undefined),
            Self::LexicalContour(lex) => Var::Local(lex.def_var(name)),
            Self::MacroExpansion(me) => me.def_var(name),
        }
    }

    pub fn def_macro(&self, name: Identifier, val: Closure) {
        match self {
            Self::Top(top) => top
                .write()
                .def_macro(name, Macro::new(MacroSource::from_top(top.clone()), val)),
            Self::LexicalContour(lex) => lex.def_macro(name, val),
            Self::MacroExpansion(me) => me.def_macro(name, val),
        }
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        match self {
            Self::Top(top) => top.write().fetch_var(name).map(Var::Global),
            Self::LexicalContour(lex) => lex.fetch_var(name),
            Self::MacroExpansion(me) => me.fetch_var(name),
        }
    }

    pub fn fetch_macro(&self, name: &Identifier) -> Option<Macro<T>> {
        match self {
            Self::Top(top) => top.read().fetch_macro(&name),
            Self::LexicalContour(lex) => lex.fetch_macro(name),
            Self::MacroExpansion(me) => me.fetch_macro(name),
        }
    }

    pub fn is_bound(&self, name: &Identifier) -> bool {
        self.fetch_var(name).is_some()
    }

    pub fn new_lexical_contour(&self) -> Self {
        let new_lexical_contour = LexicalContour::new(self);
        Self::LexicalContour(Gc::new(new_lexical_contour))
    }

    pub fn new_macro_expansion(&self, mark: Mark, source: MacroSource<T>) -> Self {
        let new_macro_expansion = MacroExpansion::new(self, mark, source);
        Self::MacroExpansion(Gc::new(new_macro_expansion))
    }
}

impl Environment<Repl> {
    pub fn new_repl() -> Self {
        Self::Top(Gc::new(Repl::default()))
    }

    pub fn from_repl(repl: Repl) -> Self {
        Self::Top(Gc::new(repl))
    }
}

impl<T: Trace> Clone for Environment<T> {
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
    name: Identifier,
    val: Gc<Value>,
}

impl Global {
    pub fn new(name: Identifier, val: Gc<Value>) -> Self {
        Global { name, val }
    }

    pub fn value(self) -> Gc<Value> {
        self.val
    }
}

impl fmt::Debug for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.name.name)
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

#[derive(Trace)]
pub enum MacroSource<T: Trace> {
    /// This macro is imported from a library.
    LibraryImport(Gc<Library>),
    /// This macro comes from a lexical contour within the current top
    LexicalContour(Gc<LexicalContour<T>>),
    /// This macro comes from top level environment
    Top(Gc<T>),
}

impl<T: Trace> Clone for MacroSource<T> {
    fn clone(&self) -> Self {
        match self {
            Self::LibraryImport(lib) => Self::LibraryImport(lib.clone()),
            Self::LexicalContour(lex) => Self::LexicalContour(lex.clone()),
            Self::Top(top) => Self::Top(top.clone()),
        }
    }
}

impl<T: Trace + Top> MacroSource<T> {
    fn from_top(top: Gc<T>) -> Self {
        Self::Top(top)
    }

    fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        match self {
            Self::LibraryImport(lib) => lib.write().fetch_var(name).map(Var::Global),
            Self::LexicalContour(lex) => lex.fetch_var(name),
            Self::Top(top) => top.write().fetch_var(name).map(Var::Global),
        }
    }

    fn fetch_macro(&self, name: &Identifier) -> Option<Macro<T>> {
        match self {
            Self::LibraryImport(lib) => lib.read().fetch_macro(name).map(Macro::into_export),
            Self::LexicalContour(lex) => lex.fetch_macro(name),
            Self::Top(top) => top.read().fetch_macro(name),
        }
    }
}

impl<T: Trace> From<Gc<Library>> for MacroSource<T> {
    fn from(lib: Gc<Library>) -> Self {
        Self::LibraryImport(lib)
    }
}

impl<T: Trace> From<Gc<LexicalContour<T>>> for MacroSource<T> {
    fn from(lib: Gc<LexicalContour<T>>) -> Self {
        Self::LexicalContour(lib)
    }
}

#[derive(Trace)]
pub struct Macro<T: Trace + Top> {
    pub source_env: MacroSource<T>,
    pub transformer: Closure,
}

impl<T: Trace + Top> Clone for Macro<T> {
    fn clone(&self) -> Self {
        Self {
            source_env: self.source_env.clone(),
            transformer: self.transformer.clone(),
        }
    }
}

impl Macro<Library> {
    fn into_export<T: Trace + Top>(self) -> Macro<T> {
        let top = match self.source_env {
            MacroSource::LibraryImport(lib) => lib.clone(),
            MacroSource::Top(top) => top.clone(),
            MacroSource::LexicalContour(_lex) => todo!("Get top from lex"),
        };

        Macro::new(MacroSource::from(top), self.transformer)
    }
}

impl<T: Trace + Top> Macro<T> {
    pub fn new(source_env: MacroSource<T>, transformer: Closure) -> Self {
        Self {
            source_env,
            transformer,
        }
    }
}
