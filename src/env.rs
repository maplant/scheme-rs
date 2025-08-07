use std::{
    collections::{HashMap, hash_map::Entry},
    fmt,
    hash::{Hash, Hasher},
    mem::ManuallyDrop,
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
};

use futures::future::BoxFuture;

use crate::{
    exception::Condition,
    gc::{Gc, GcInner, Trace},
    proc::Closure,
    registry::Library,
    symbols::Symbol,
    syntax::{Identifier, Mark},
    value::Value,
};

/*
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

    pub fn def_keyword(&mut self, name: Identifier, mac: Macro) {
        self.macros.insert(name, mac);
    }

    pub fn fetch_var(&mut self, name: &Identifier) -> Option<Global> {
        self.vars
            .get(name)
            .map(|val| Global::new(name.clone(), val.clone()))
    }

    pub fn fetch_keyword(&self, name: &Identifier) -> Option<Macro> {
        self.macros.get(name).cloned()
    }
}

impl fmt::Debug for Top {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Top")
    }
}
*/

/*
/// A Top level environment
pub trait Top: Send + Sync + 'static {
    fn is_repl(&self) -> bool {
        false
    }

    fn def_var(&mut self, name: Identifier, value: Value) -> Global;

    fn def_keyword(&mut self, keyword: Identifier, mac: Macro);

    fn fetch_var(&self, name: &Identifier) -> BoxFuture<'static, Option<Global>>;

    fn fetch_keyword(&self, keyword: &Identifier) -> BoxFuture<'static, Option<Macro>>;
}

pub fn type_erase_top(top: Gc<impl Top>) -> Gc<dyn Top> {
    let top = ManuallyDrop::new(top);
    let top: NonNull<GcInner<dyn Top>> = top.ptr;
    Gc {
        ptr: top,
        marker: std::marker::PhantomData,
    }
}
*/

#[derive(Trace)]
pub struct LexicalContour {
    up: Environment,
    vars: HashMap<Identifier, Local>,
    macros: HashMap<Identifier, Gc<Closure>>,
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

    pub fn def_macro(&mut self, name: Identifier, closure: Gc<Closure>) {
        self.macros.insert(name, closure);
    }

    pub fn fetch_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Var>, Condition>> {
        if let Some(local) = self.vars.get(name) {
            let local = *local;
            return Box::pin(async move { Ok(Some(Var::Local(local))) });
        }
        let up = self.up.clone();
        Box::pin(async move { up.fetch_var(name).await })
    }

    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        if let Some(local) = self.vars.get(name) {
            return Some(*local);
        }
        self.up.fetch_local(name)
    }

    pub fn fetch_top(&self) -> Library {
        self.up.fetch_top()
    }

    pub fn is_bound(&self, name: &Identifier) -> bool {
        self.vars.contains_key(name) || self.up.is_bound(name)
    }
}

impl Gc<LexicalContour> {
    pub fn fetch_keyword<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Keyword>, Condition>> {
        let up = {
            let this = self.read();
            if let Some(trans) = this.macros.get(name) {
                let this_clone = self.clone();
                let trans = trans.clone();
                return Box::pin(async move {
                    Ok(Some(Keyword::new(
                        Environment::LexicalContour(this_clone),
                        trans,
                    )))
                });
            }
            this.up.clone()
        };
        Box::pin(async move { up.fetch_keyword(name).await })
    }
}

#[derive(Trace)]
pub struct LetSyntaxContour {
    up: Environment,
    macros: HashMap<Identifier, Gc<Closure>>,
    recursive: bool,
}

impl LetSyntaxContour {
    fn new(env: &Environment, recursive: bool) -> Self {
        Self {
            up: env.clone(),
            macros: Default::default(),
            recursive,
        }
    }
}

impl LetSyntaxContour {
    pub fn def_var(&self, name: Identifier) -> Var {
        self.up.def_var(name)
    }

    pub fn def_keyword(&mut self, name: Identifier, closure: Gc<Closure>) {
        self.macros.insert(name, closure);
    }

    pub fn fetch_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Var>, Condition>> {
        let up = self.up.clone();
        Box::pin(async move { up.fetch_var(name).await })
    }

    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        self.up.fetch_local(name)
    }

    pub fn fetch_top(&self) -> Library {
        self.up.fetch_top()
    }
}

impl Gc<LetSyntaxContour> {
    pub fn fetch_keyword<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Keyword>, Condition>> {
        let up = {
            let this = self.read();
            if let Some(trans) = this.macros.get(name) {
                let trans = trans.clone();
                let env = if this.recursive {
                    Environment::LetSyntaxContour(self.clone())
                } else {
                    this.up.clone()
                };
                return Box::pin(async move { Ok(Some(Keyword::new(env, trans))) });
            }
            this.up.clone()
        };
        Box::pin(async move { up.fetch_keyword(name).await })
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

    pub fn def_keyword(&self, name: Identifier, closure: Gc<Closure>) {
        self.up.def_keyword(name, closure);
    }

    pub fn fetch_var<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Var>, Condition>> {
        let up = self.up.clone();
        let source = self.source.clone();
        let mark = self.mark;
        Box::pin(async move {
            // Attempt to check the up scope first:
            let var = up.fetch_var(name).await?;
            if var.is_some() {
                return Ok(var);
            }
            // If the current expansion context contains the mark, remove it and check the
            // expansion source scope.
            if name.marks.contains(&mark) {
                let mut unmarked = name.clone();
                unmarked.mark(mark);
                source.fetch_var(&unmarked).await
            } else {
                Ok(None)
            }
        })
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

    pub fn fetch_keyword<'a>(
        &self,
        name: &'a Identifier,
    ) -> BoxFuture<'a, Result<Option<Keyword>, Condition>> {
        let up = self.up.clone();
        let source = self.source.clone();
        let mark = self.mark;
        Box::pin(async move {
            // Attempt to check the up scope first:
            let key = up.fetch_keyword(name).await?;
            if key.is_some() {
                return Ok(key);
            }
            // If the current expansion context contains the mark, remove it and check the
            // expansion source scope.
            if name.marks.contains(&mark) {
                let mut unmarked = name.clone();
                unmarked.mark(mark);
                source.fetch_keyword(&unmarked).await
            } else {
                Ok(None)
            }
        })
    }

    pub fn fetch_top(&self) -> Library {
        self.up.fetch_top()
    }

    pub fn is_bound(&self, name: &Identifier) -> bool {
        self.up.is_bound(name)
            || name.marks.contains(&self.mark) && {
                let mut unmarked = name.clone();
                unmarked.mark(self.mark);
                self.source.is_bound(&unmarked)
            }
    }
}

#[derive(Trace)]
pub enum Environment {
    Top(Library),
    LexicalContour(Gc<LexicalContour>),
    LetSyntaxContour(Gc<LetSyntaxContour>),
    MacroExpansion(Gc<MacroExpansion>),
}

impl Environment {
    pub fn fetch_top(&self) -> Library {
        match self {
            Self::Top(top) => top.clone(),
            Self::LexicalContour(lex) => lex.read().fetch_top(),
            Self::LetSyntaxContour(ls) => ls.read().fetch_top(),
            Self::MacroExpansion(me) => me.read().fetch_top(),
        }
    }

    pub fn def_var(&self, name: Identifier) -> Var {
        match self {
            Self::Top(top) => Var::Global(top.def_var(name, Value::undefined())),
            Self::LexicalContour(lex) => Var::Local(lex.write().def_var(name)),
            Self::LetSyntaxContour(ls) => ls.read().def_var(name),
            Self::MacroExpansion(me) => me.read().def_var(name),
        }
    }

    pub fn def_keyword(&self, name: Identifier, val: Gc<Closure>) {
        match self {
            Self::Top(top) => top.def_keyword(name, Keyword::new(self.clone(), val)),
            Self::LexicalContour(lex) => lex.write().def_macro(name, val),
            Self::LetSyntaxContour(ls) => ls.write().def_keyword(name, val),
            Self::MacroExpansion(me) => me.read().def_keyword(name, val),
        }
    }

    pub async fn fetch_var(&self, name: &Identifier) -> Result<Option<Var>, Condition> {
        match self {
            Self::Top(top) => Ok(top.fetch_var(name).await?.map(Var::Global)),
            Self::LexicalContour(lex) => {
                let fetch_fut = { lex.read().fetch_var(name) };
                fetch_fut.await
            }
            Self::LetSyntaxContour(ls) => {
                let fetch_fut = { ls.read().fetch_var(name) };
                fetch_fut.await
            }
            Self::MacroExpansion(me) => {
                let fetch_fut = { me.read().fetch_var(name) };
                fetch_fut.await
            }
        }
    }

    pub fn fetch_local(&self, name: &Identifier) -> Option<Local> {
        match self {
            Self::Top(_) => None,
            Self::LexicalContour(lex) => lex.read().fetch_local(name),
            Self::LetSyntaxContour(ls) => ls.read().fetch_local(name),
            Self::MacroExpansion(me) => me.read().fetch_local(name),
        }
    }

    pub async fn fetch_keyword(&self, name: &Identifier) -> Result<Option<Keyword>, Condition> {
        match self {
            Self::Top(top) => top.fetch_keyword(name).await,
            Self::LexicalContour(lex) => {
                let fetch_fut = { lex.fetch_keyword(name) };
                fetch_fut.await
            }
            Self::LetSyntaxContour(ls) => {
                let fetch_fut = { ls.fetch_keyword(name) };
                fetch_fut.await
            }
            Self::MacroExpansion(me) => {
                let fetch_fut = { me.read().fetch_keyword(name) };
                fetch_fut.await
            }
        }
    }

    pub fn is_bound(&self, name: &Identifier) -> bool {
        match self {
            Self::Top(lib) => lib.is_bound(name),
            Self::LexicalContour(lex) => lex.read().is_bound(name),
            Self::LetSyntaxContour(ls) => ls.read().up.is_bound(name),
            Self::MacroExpansion(me) => me.read().is_bound(name),
        }
    }

    pub fn new_lexical_contour(&self) -> Self {
        let new_lexical_contour = LexicalContour::new(self);
        Self::LexicalContour(Gc::new(new_lexical_contour))
    }

    pub fn new_let_syntax_contour(&self, recursive: bool) -> Self {
        let new_let_syntax_contour = LetSyntaxContour::new(self, recursive);
        Self::LetSyntaxContour(Gc::new(new_let_syntax_contour))
    }

    pub fn new_macro_expansion(&self, mark: Mark, source: Environment) -> Self {
        let new_macro_expansion = MacroExpansion::new(self, mark, source);
        Self::MacroExpansion(Gc::new(new_macro_expansion))
    }
}

impl From<Library> for Environment {
    fn from(top: Library) -> Self {
        Self::Top(top)
    }
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        match self {
            Self::Top(top) => Self::Top(top.clone()),
            Self::LexicalContour(lex) => Self::LexicalContour(lex.clone()),
            Self::LetSyntaxContour(ls) => Self::LetSyntaxContour(ls.clone()),
            Self::MacroExpansion(mac) => Self::MacroExpansion(mac.clone()),
        }
    }
}

#[derive(Copy, Clone, Trace)]
pub struct Local {
    id: usize,
    pub(crate) name: Option<Symbol>,
}

impl Hash for Local {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.id.hash(state);
    }
}

impl PartialEq for Local {
    fn eq(&self, rhs: &Self) -> bool {
        self.id == rhs.id
    }
}

impl Eq for Local {}

impl Local {
    /// Create a new temporary value.
    pub fn gensym() -> Self {
        static NEXT_SYM: AtomicUsize = AtomicUsize::new(0);
        Self {
            id: NEXT_SYM.fetch_add(1, Ordering::Relaxed),
            name: None,
        }
    }

    pub fn gensym_with_name(name: Symbol) -> Self {
        let mut sym = Self::gensym();
        sym.name = Some(name);
        sym
    }

    pub fn to_func_name(&self) -> String {
        if let Some(name) = self.name {
            format!("{name}")
        } else {
            format!("f{}", self.id)
        }
    }
}

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.name {
            write!(f, "{name}")
        } else {
            write!(f, "%{}", self.id)
        }
    }
}

impl fmt::Debug for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.name {
            write!(f, "{name}")
        } else {
            write!(f, "%{}", self.id)
        }
    }
}

// TODO: Do we need to make this pointer eq?
#[derive(Clone, Trace)]
pub struct Global {
    pub(crate) name: Identifier,
    val: Gc<Value>,
    mutable: bool,
}

impl Global {
    pub fn new(name: Identifier, val: Gc<Value>, mutable: bool) -> Self {
        Global { name, val, mutable }
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
        write!(f, "${}", self.name.sym)
    }
}

impl PartialEq for Global {
    fn eq(&self, rhs: &Self) -> bool {
        self.name == rhs.name && Gc::ptr_eq(&self.val, &rhs.val)
    }
}

impl Eq for Global {}

impl Hash for Global {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.name.hash(state);
        Gc::as_ptr(&self.val).hash(state);
    }
}

#[derive(Clone, Trace, Hash, PartialEq, Eq)]
pub enum Var {
    Global(Global),
    Local(Local),
}

impl Var {
    pub fn symbol(&self) -> Option<Symbol> {
        match self {
            Var::Global(global) => Some(global.name.sym),
            Var::Local(local) => local.name,
        }
    }
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
pub struct Keyword {
    pub source_env: Environment,
    pub transformer: Gc<Closure>,
}

impl Keyword {
    pub fn new(source_env: Environment, transformer: Gc<Closure>) -> Self {
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
