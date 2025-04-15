use std::{
    collections::{hash_map::Entry, HashMap}, fmt, hash::{Hash, Hasher}, sync::atomic::{AtomicUsize, Ordering}
};

use crate::{
    gc::{Gc, Trace},
    proc::Closure,
    syntax::{Identifier, Mark},
    value::Value,
};

/// A Top level environment
#[derive(Trace)]
pub struct Top {
    kind: TopLevelEnvKind,
    vars: HashMap<Identifier, Gc<Value>>,
    macros: HashMap<Identifier, Macro>,
}

#[derive(Trace)]
pub enum TopLevelEnvKind {
    Library,
    Program,
    Repl,
}

impl Top {
    pub fn library() -> Self {
        Self {
            kind: TopLevelEnvKind::Library,
            vars: HashMap::new(),
            macros: HashMap::new(),
        }
    }

    pub fn program() -> Self {
        Self {
            kind: TopLevelEnvKind::Program,
            vars: HashMap::new(),
            macros: HashMap::new(),
        }
    }

    pub fn repl() -> Self {
        Self {
            kind: TopLevelEnvKind::Repl,
            vars: HashMap::new(),
            macros: HashMap::new(),
        }
    }

    pub fn is_repl(&self) -> bool {
        matches!(self.kind, TopLevelEnvKind::Repl)
    }

    pub fn import(&mut self, lib: &Top) {
        for (name, val) in lib.vars.iter() {
            self.vars.insert(name.clone(), val.clone());
        }
        for (name, mac) in lib.macros.iter() {
            self.macros.insert(name.clone(), mac.clone());
        }
    }

    pub fn def_var(&mut self, name: Identifier, value: Value) -> Global {
        let global = Gc::new(value);
        match self.vars.entry(name.clone()) {
            Entry::Occupied(occup) => Global::new(name, occup.get().clone()),
            Entry::Vacant(vacant) => Global::new(name, vacant.insert(global).clone()),
        }
    }

    pub fn def_macro(&mut self, name: Identifier, mac: Macro) {
        self.macros.insert(name, mac);
    }

    pub fn fetch_var(&mut self, name: &Identifier) -> Option<Global> {
        self.vars
            .get(name)
            .map(|val| Global::new(name.clone(), val.clone()))
    }

    pub fn fetch_macro(&self, name: &Identifier) -> Option<Macro> {
        self.macros.get(name).cloned()
    }
}

#[derive(Trace)]
pub struct LexicalContour {
    up: Environment,
    vars: HashMap<Identifier, Local>,
    macros: HashMap<Identifier, Closure>,
}

impl LexicalContour {
    fn new(env: &Environment) -> Self {
        Self {
            up: env.clone(),
            vars: Default::default(),
            macros: Default::default(),
        }
    }
}

impl LexicalContour {
    pub fn def_var(&mut self, name: Identifier) -> Local {
        let local = Local::gensym();
        self.vars.insert(name, local);
        local
    }

    pub fn def_macro(&mut self, name: Identifier, closure: Closure) {
        self.macros.insert(name, closure);
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        if let Some(local) = self.vars.get(name) {
            return Some(Var::Local(*local));
        }
        self.up.fetch_var(name)
    }

    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        if let Some(local) = self.vars.get(name) {
            return Some(*local);
        }
        self.up.fetch_local(name)
    }

    pub fn fetch_top(&self) -> Gc<Top> {
        self.up.fetch_top()
    }
}

impl Gc<LexicalContour> {
    pub fn fetch_macro(&self, name: &Identifier) -> Option<Macro> {
        if let Some(trans) = self.read().macros.get(name) {
            return Some(Macro::new(
                Environment::LexicalContour(self.clone()),
                trans.clone(),
            ));
        }
        self.read().up.fetch_macro(name)
    }
}

#[derive(Trace)]
pub struct MacroExpansion {
    up: Environment,
    mark: Mark,
    source: Environment,
}

impl MacroExpansion {
    pub fn new(env: &Environment, mark: Mark, source: Environment) -> Self {
        Self {
            up: env.clone(),
            mark,
            source,
        }
    }
}

impl MacroExpansion {
    pub fn def_var(&self, name: Identifier) -> Var {
        // In the case of defining variables produced from macro expansions, pass them
        // on to the next environment up.
        self.up.def_var(name)
    }

    pub fn def_macro(&self, name: Identifier, closure: Closure) {
        self.up.def_macro(name, closure);
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        // Attempt to check the up scope first:
        let var = self.up.fetch_var(name);
        if var.is_some() {
            return var;
        }
        // If the current expansion context contains the mark, remove it and check the
        // expansion source scope.
        name.marks
            .contains(&self.mark)
            .then(|| {
                let mut unmarked = name.clone();
                unmarked.mark(self.mark);
                self.source.fetch_var(&unmarked)
            })
            .flatten()
    }

    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        // Attempt to check the up scope first:
        let var = self.up.fetch_local(name);
        if var.is_some() {
            return var;
        }
        // If the current expansion context contains the mark, remove it and check the
        // expansion source scope.
        name.marks
            .contains(&self.mark)
            .then(|| {
                let mut unmarked = name.clone();
                unmarked.mark(self.mark);
                self.source.fetch_local(&unmarked)
            })
            .flatten()
    }

    pub fn fetch_macro(&self, name: &Identifier) -> Option<Macro> {
        // Attempt to check the up scope first:
        let mac = self.up.fetch_macro(name);
        if mac.is_some() {
            return mac;
        }
        // If the current expansion context contains the mark, remove it and check the
        // expansion source scope.
        name.marks
            .contains(&self.mark)
            .then(|| {
                let mut unmarked = name.clone();
                unmarked.mark(self.mark);
                self.source.fetch_macro(&unmarked)
            })
            .flatten()
    }

    pub fn fetch_top(&self) -> Gc<Top> {
        self.up.fetch_top()
    }
}

#[derive(Trace)]
pub enum Environment {
    Top(Gc<Top>),
    LexicalContour(Gc<LexicalContour>),
    MacroExpansion(Gc<MacroExpansion>),
}

impl Environment {
    pub fn fetch_top(&self) -> Gc<Top> {
        match self {
            Self::Top(top) => top.clone(),
            Self::LexicalContour(lex) => lex.read().fetch_top(),
            Self::MacroExpansion(me) => me.read().fetch_top(),
        }
    }

    pub fn def_var(&self, name: Identifier) -> Var {
        match self {
            Self::Top(top) => Var::Global(top.write().def_var(name, Value::undefined())),
            Self::LexicalContour(lex) => Var::Local(lex.write().def_var(name)),
            Self::MacroExpansion(me) => me.read().def_var(name),
        }
    }

    pub fn def_macro(&self, name: Identifier, val: Closure) {
        match self {
            Self::Top(top) => top.write().def_macro(name, Macro::new(self.clone(), val)),
            Self::LexicalContour(lex) => lex.write().def_macro(name, val),
            Self::MacroExpansion(me) => me.read().def_macro(name, val),
        }
    }

    pub fn fetch_var(&self, name: &Identifier) -> Option<Var> {
        match self {
            Self::Top(top) => top.write().fetch_var(name).map(Var::Global),
            Self::LexicalContour(lex) => lex.read().fetch_var(name),
            Self::MacroExpansion(me) => me.read().fetch_var(name),
        }
    }

    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        match self {
            Self::Top(_) => None,
            Self::LexicalContour(lex) => lex.read().fetch_local(name),
            Self::MacroExpansion(me) => me.read().fetch_local(name),
        }
    }

    pub fn fetch_macro(&self, name: &Identifier) -> Option<Macro> {
        match self {
            Self::Top(top) => top.read().fetch_macro(name),
            Self::LexicalContour(lex) => lex.fetch_macro(name),
            Self::MacroExpansion(me) => me.read().fetch_macro(name),
        }
    }

    pub fn is_bound(&self, name: &Identifier) -> bool {
        self.fetch_var(name).is_some()
    }

    pub fn new_lexical_contour(&self) -> Self {
        let new_lexical_contour = LexicalContour::new(self);
        Self::LexicalContour(Gc::new(new_lexical_contour))
    }

    pub fn new_macro_expansion(&self, mark: Mark, source: Environment) -> Self {
        let new_macro_expansion = MacroExpansion::new(self, mark, source);
        Self::MacroExpansion(Gc::new(new_macro_expansion))
    }
}

impl From<Gc<Top>> for Environment {
    fn from(top: Gc<Top>) -> Self {
        Self::Top(top)
    }
}

impl Clone for Environment {
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

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl fmt::Debug for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

// TODO: Do we need to make this pointer eq?
#[derive(Clone, Trace)]
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

    pub fn value_ref(&self) -> &Gc<Value> {
        &self.val
    }
}

impl fmt::Debug for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.name.name)
    }
}

impl PartialEq for Global {
    fn eq(&self, rhs: &Self) -> bool {
        self.name == rhs.name && Gc::ptr_eq(&self.val, &rhs.val)
    }
}

impl Hash for Global {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.name.hash(state);
        Gc::as_ptr(&self.val).hash(state);
    }
}

impl Eq for Global {}

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

#[derive(Clone, Trace)]
pub struct Macro {
    pub source_env: Environment,
    pub transformer: Closure,
}

impl Macro {
    pub fn new(source_env: Environment, transformer: Closure) -> Self {
        Self {
            source_env,
            transformer,
        }
    }
}

#[derive(Clone, Trace)]
#[repr(align(16))]
pub struct CapturedEnv {
    pub env: Environment,
    pub captured: Vec<Local>,
}

impl CapturedEnv {
    pub fn new(env: Environment, captured: Vec<Local>) -> Self {
        Self { env, captured }
    }
}
