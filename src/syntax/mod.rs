//! Rust representation of S-expressions.

use crate::{
    ast::Primitive,
    env::{Binding, Environment, Scope, add_binding, resolve},
    exceptions::{CompoundCondition, Exception, Message, SyntaxViolation, Who},
    gc::{Gc, Trace},
    ports::Port,
    proc::Procedure,
    records::{RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::bridge,
    symbols::Symbol,
    syntax::parse::ParseSyntaxError,
    value::{Expect1, UnpackedValue, Value},
};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::{collections::BTreeSet, fmt, hash::Hash, io::Cursor, sync::Arc};

#[cfg(feature = "async")]
use futures::future::BoxFuture;

pub mod lex;
pub mod parse;

/// Source location for an s-expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Trace)]
pub struct Span {
    pub line: u32,
    pub column: usize,
    pub offset: usize,
    pub file: Arc<String>,
}

impl Span {
    pub fn new(file: &str) -> Self {
        Self {
            file: Arc::new(file.to_string()),
            ..Default::default()
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            line: 1,
            column: 0,
            offset: 0,
            file: Arc::new(String::new()),
        }
    }
}

impl SchemeCompatible for Span {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "span", sealed: true, opaque: true)
    }
}

/// Representation of a Scheme syntax object, or s-expression.
#[derive(Clone, Trace)]
#[repr(align(16))]
pub enum Syntax {
    /// A wrapped value.
    Wrapped {
        value: Value,
        span: Span,
    },
    /// A nested grouping of pairs. If the expression is a proper list, then the
    /// last element of expression will be Null. This vector is guaranteed to contain
    /// at least two elements.
    List {
        list: Vec<Syntax>,
        span: Span,
    },
    Vector {
        vector: Vec<Syntax>,
        span: Span,
    },
    Identifier {
        ident: Identifier,
        span: Span,
    },
}

impl Syntax {
    pub(crate) fn adjust_scope(&mut self, scope: Scope, op: fn(&mut Identifier, Scope)) {
        match self {
            Self::List { list, .. } => {
                for item in list {
                    item.adjust_scope(scope, op);
                }
            }
            Self::Vector { vector, .. } => {
                for item in vector {
                    item.adjust_scope(scope, op);
                }
            }
            Self::Identifier { ident, .. } => op(ident, scope),
            _ => (),
        }
    }

    pub fn add_scope(&mut self, scope: Scope) {
        self.adjust_scope(scope, Identifier::add_scope)
    }

    pub fn flip_scope(&mut self, scope: Scope) {
        self.adjust_scope(scope, Identifier::flip_scope)
    }

    pub fn remove_scope(&mut self, scope: Scope) {
        self.adjust_scope(scope, Identifier::remove_scope)
    }

    pub fn wrap(value: Value) -> Syntax {
        match value.unpack() {
            UnpackedValue::Pair(pair) => {
                let (car, cdr) = pair.into();
                let car = Self::wrap(car);
                let cdr = Self::wrap(cdr);
                match cdr {
                    Syntax::List { mut list, span } => {
                        list.insert(0, car);
                        Syntax::List { list, span }
                    }
                    _ => Syntax::List {
                        list: vec![car, cdr],
                        span: Span::default(),
                    },
                }
            }
            UnpackedValue::Vector(vec) => Syntax::Vector {
                vector: vec.iter().map(Syntax::wrap).collect(),
                span: Span::default(),
            },
            UnpackedValue::Syntax(syn) => syn.as_ref().clone(),
            value => Syntax::Wrapped {
                value: value.into_value(),
                span: Span::default(),
            },
        }
    }

    pub fn unwrap(self) -> Value {
        match self {
            Self::Wrapped { value, .. } => value,
            Self::List { mut list, .. } => {
                let mut cdr = Self::unwrap(list.pop().unwrap());
                for car in list.into_iter().map(Self::unwrap).rev() {
                    cdr = Value::from((car, cdr));
                }
                cdr
            }
            Self::Vector { vector, .. } => {
                Value::from(vector.into_iter().map(Syntax::unwrap).collect::<Vec<_>>())
            }
            _ => Value::from(self),
        }
    }

    pub fn datum_to_syntax(scopes: &BTreeSet<Scope>, value: Value) -> Syntax {
        match value.unpack() {
            UnpackedValue::Pair(pair) => {
                let (car, cdr) = pair.into();
                let car = Self::datum_to_syntax(scopes, car);
                let cdr = Self::datum_to_syntax(scopes, cdr);
                match cdr {
                    Syntax::List { mut list, span } => {
                        list.insert(0, car);
                        Syntax::List { list, span }
                    }
                    _ => Syntax::List {
                        list: vec![car, cdr],
                        span: Span::default(),
                    },
                }
            }
            UnpackedValue::Vector(vec) => Syntax::Vector {
                vector: vec
                    .iter()
                    .map(|value| Syntax::datum_to_syntax(scopes, value))
                    .collect(),
                span: Span::default(),
            },
            UnpackedValue::Syntax(syn) => {
                let mut syn = syn.as_ref().clone();
                for scope in scopes {
                    syn.add_scope(*scope);
                }
                syn
            }
            UnpackedValue::Symbol(sym) => Syntax::Identifier {
                ident: Identifier {
                    sym,
                    scopes: scopes.clone(),
                },
                span: Span::default(),
            },
            value => Syntax::Wrapped {
                value: value.into_value(),
                span: Span::default(),
            },
        }
    }

    pub fn syntax_to_datum(value: Value) -> Value {
        match value.unpack() {
            UnpackedValue::Pair(pair) => {
                let (car, cdr) = pair.into();
                Value::from((Self::syntax_to_datum(car), Self::syntax_to_datum(cdr)))
            }
            UnpackedValue::Vector(vec) => {
                Value::from(vec.iter().map(Self::syntax_to_datum).collect::<Vec<_>>())
            }
            UnpackedValue::Syntax(syn) => match syn.as_ref() {
                Syntax::Identifier { ident, .. } => Value::from(ident.sym),
                Syntax::Wrapped { value, .. } => value.clone(),
                syn => Syntax::syntax_to_datum(Self::unwrap(syn.clone())),
            },
            unpacked => unpacked.into_value(),
        }
    }

    #[maybe_async]
    fn apply_transformer(&self, transformer: &Procedure) -> Result<Expansion, Exception> {
        // Create a new scope for the expansion
        let intro_scope = Scope::new();

        // Apply the new scope to the input
        let mut input = self.clone();
        input.add_scope(intro_scope);

        // Call the transformer with the input:
        let transformer_output = maybe_await!(transformer.call(&[Value::from(input)]))?;

        let output: Value = transformer_output.expect1()?;
        let mut output = Syntax::wrap(output);
        output.flip_scope(intro_scope);

        Ok(Expansion::Expanded(output))
    }

    #[cfg(not(feature = "async"))]
    fn expand_once(&self, env: &Environment) -> Result<Expansion, Exception> {
        self.expand_once_inner(env)
    }

    #[cfg(feature = "async")]
    fn expand_once<'a>(
        &'a self,
        env: &'a Environment,
    ) -> BoxFuture<'a, Result<Expansion, Exception>> {
        Box::pin(self.expand_once_inner(env))
    }

    #[maybe_async]
    fn expand_once_inner(&self, env: &Environment) -> Result<Expansion, Exception> {
        match self {
            Self::List { list, .. } => {
                let ident = match list.first() {
                    Some(Self::Identifier { ident, .. }) => ident,
                    _ => return Ok(Expansion::Unexpanded),
                };
                if let Some(binding) = ident.resolve() {
                    if let Some(transformer) = maybe_await!(env.lookup_keyword(binding))? {
                        return maybe_await!(self.apply_transformer(&transformer));
                    } else if let Some(Primitive::Set) = env.lookup_primitive(binding)
                        && let [Syntax::Identifier { ident, .. }, ..] = &list.as_slice()[1..]
                    {
                        // Check for set! macro
                        // Look for a variable transformer
                        if let Some(binding) = ident.resolve()
                            && let Some(transformer) = maybe_await!(env.lookup_keyword(binding))?
                        {
                            if !transformer.is_variable_transformer() {
                                return Err(Exception::error(format!(
                                    "{} is not a variable transformer",
                                    ident.sym
                                )));
                            }
                            return maybe_await!(self.apply_transformer(&transformer));
                        }
                    }
                }
            }
            Self::Identifier { ident, .. } => {
                if let Some(binding) = ident.resolve()
                    && let Some(transformer) = maybe_await!(env.lookup_keyword(binding))?
                {
                    return maybe_await!(self.apply_transformer(&transformer));
                }
            }
            _ => (),
        }
        Ok(Expansion::Unexpanded)
    }

    /// Fully expand the outermost syntax object.
    #[maybe_async]
    pub(crate) fn expand(mut self, env: &Environment) -> Result<Syntax, Exception> {
        loop {
            match maybe_await!(self.expand_once(env)) {
                Ok(Expansion::Unexpanded) => {
                    return Ok(self);
                }
                Ok(Expansion::Expanded(syntax)) => {
                    self = syntax;
                }
                Err(condition) => {
                    return Err(condition.add_condition(SyntaxViolation::new(self, None)));
                }
            }
        }
    }

    #[cfg(not(feature = "async"))]
    pub fn from_str(s: &str, file_name: Option<&str>) -> Result<Self, ParseSyntaxError> {
        use crate::ports::{BufferMode, Transcoder};

        let file_name = file_name.unwrap_or("<unknown>");
        let bytes = Cursor::new(s.as_bytes().to_vec());
        let port = Port::new(
            file_name,
            bytes,
            BufferMode::Block,
            Some(Transcoder::native()),
        );
        port.all_sexprs(Span::new(file_name))
    }

    #[cfg(feature = "async")]
    pub fn from_str(s: &str, file_name: Option<&str>) -> Result<Self, ParseSyntaxError> {
        use crate::ports::{BufferMode, Transcoder};

        let file_name = file_name.unwrap_or("<unknown>");
        let bytes = Cursor::new(s.as_bytes().to_vec());

        // This is kind of convoluted, but convenient
        let port = Arc::into_inner(
            Port::new(
                file_name,
                bytes,
                BufferMode::Block,
                Some(Transcoder::native()),
            )
            .0,
        )
        .unwrap();
        let info = port.info;
        let mut data = port.data.into_inner();

        // This is safe since we don't need the async executor to drive anything
        // here
        futures::executor::block_on(async move {
            use crate::syntax::parse::Parser;

            let mut parser = Parser::new(&mut data, &info, Span::new(file_name));
            parser.all_sexprs().await
        })
    }

    /// Returns true if the syntax item is a list with a car that is an
    /// identifier equal to the passed argument.
    pub(crate) fn has_car(&self, car: &str) -> bool {
        matches!(self.as_list(), Some([Self::Identifier { ident, .. }, .. ]) if ident == car)
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Wrapped { span, .. } => span,
            Self::List { span, .. } => span,
            Self::Vector { span, .. } => span,
            Self::Identifier { span, .. } => span,
        }
    }

    pub fn as_ident(&self) -> Option<&Identifier> {
        if let Syntax::Identifier { ident, .. } = self {
            Some(ident)
        } else {
            None
        }
    }

    pub fn new_list(list: Vec<Syntax>, span: impl Into<Span>) -> Self {
        Self::List {
            list,
            span: span.into(),
        }
    }

    pub fn as_list(&self) -> Option<&[Syntax]> {
        if let Syntax::List { list, .. } = self {
            Some(list)
        } else {
            None
        }
    }

    pub fn as_list_mut(&mut self) -> Option<&mut [Syntax]> {
        if let Syntax::List { list, .. } = self {
            Some(list)
        } else {
            None
        }
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Self::List { .. })
    }

    pub fn new_vector(vector: Vec<Syntax>, span: impl Into<Span>) -> Self {
        Self::Vector {
            vector,
            span: span.into(),
        }
    }

    pub fn is_vector(&self) -> bool {
        matches!(self, Self::Vector { .. })
    }

    pub fn new_wrapped(value: Value, span: impl Into<Span>) -> Self {
        Self::Wrapped {
            value,
            span: span.into(),
        }
    }

    pub fn new_identifier(name: &str, span: impl Into<Span>) -> Self {
        Self::Identifier {
            ident: Identifier::new(name),
            span: span.into(),
        }
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier { .. })
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Self::Wrapped { value, .. } if value.is_null())
    }
}

impl fmt::Debug for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Syntax::List { list, .. } => {
                let proper_list = list.last().unwrap().is_null();
                let len = list.len();
                write!(f, "(")?;
                for (i, item) in list.iter().enumerate() {
                    if i == len - 1 {
                        if proper_list {
                            break;
                        } else {
                            write!(f, " . {item:?}")?;
                        }
                    } else {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{item:?}")?;
                    }
                }
                write!(f, ")")
            }
            Syntax::Wrapped { value, .. } => {
                write!(f, "{value:?}")
            }
            Syntax::Vector { vector, .. } => {
                write!(f, "#(")?;
                for (i, item) in vector.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item:?}")?;
                }
                write!(f, ")")
            }
            Syntax::Identifier { ident, .. } => {
                write!(f, "{}", ident.sym)
            }
        }
    }
}

pub(crate) enum Expansion {
    /// Syntax remained unchanged after expansion
    Unexpanded,
    /// Syntax was expanded, producing a new expansion context
    Expanded(Syntax),
}

#[derive(Clone, Trace, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub(crate) sym: Symbol,
    pub(crate) scopes: BTreeSet<Scope>,
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({:?})", self.sym, self.scopes)
    }
}

impl Identifier {
    pub fn new(name: &str) -> Self {
        Self {
            sym: Symbol::intern(name),
            scopes: BTreeSet::new(),
        }
    }

    pub fn symbol(&self) -> Symbol {
        self.sym
    }

    pub fn from_symbol(sym: Symbol, scope: Scope) -> Self {
        Self {
            sym,
            scopes: BTreeSet::from([scope]),
        }
    }

    pub fn add_scope(&mut self, scope: Scope) {
        self.scopes.insert(scope);
    }

    pub fn remove_scope(&mut self, scope: Scope) {
        self.scopes.remove(&scope);
    }

    pub fn flip_scope(&mut self, scope: Scope) {
        if self.scopes.contains(&scope) {
            self.scopes.remove(&scope);
        } else {
            self.scopes.insert(scope);
        }
    }

    pub fn free_identifier_equal(&self, rhs: &Self) -> bool {
        match (self.resolve(), rhs.resolve()) {
            (Some(lhs), Some(rhs)) => lhs == rhs,
            (None, None) => self.sym == rhs.sym,
            _ => false,
        }
    }

    pub fn resolve(&self) -> Option<Binding> {
        resolve(self)
    }

    pub(crate) fn bind(&self) -> Binding {
        if let Some(binding) = self.resolve() {
            binding
        } else {
            self.new_bind()
        }
    }

    pub(crate) fn new_bind(&self) -> Binding {
        let new_binding = Binding::new();
        add_binding(self.clone(), new_binding);
        new_binding
    }
}

impl PartialEq<str> for Identifier {
    fn eq(&self, rhs: &str) -> bool {
        self.sym.to_str().as_ref() == rhs
    }
}

#[bridge(name = "syntax->datum", lib = "(rnrs syntax-case builtins (6))")]
pub fn syntax_to_datum(value: &Value) -> Result<Vec<Value>, Exception> {
    // This is quite slow and could be improved
    Ok(vec![Syntax::syntax_to_datum(value.clone())])
}

#[bridge(name = "datum->syntax", lib = "(rnrs syntax-case builtins (6))")]
pub fn datum_to_syntax(template_id: Identifier, datum: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(Syntax::datum_to_syntax(
        &template_id.scopes,
        datum.clone(),
    ))])
}

#[bridge(name = "identifier?", lib = "(rnrs syntax-case builtins (6))")]
pub fn identifier_pred(obj: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(
        obj.cast_to_scheme_type::<Identifier>().is_some(),
    )])
}

#[bridge(name = "bound-identifier=?", lib = "(rnrs syntax-case builtins (6))")]
pub fn bound_identifier_eq_pred(id1: Identifier, id2: Identifier) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(id1 == id2)])
}

#[bridge(name = "free-identifier=?", lib = "(rnrs syntax-case builtins (6))")]
pub fn free_identifier_eq_pred(id1: Identifier, id2: Identifier) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(id1.free_identifier_equal(&id2))])
}

#[bridge(name = "generate-temporaries", lib = "(rnrs syntax-case builtins (6))")]
pub fn generate_temporaries(list: &Value) -> Result<Vec<Value>, Exception> {
    let length = if let Syntax::List { list, .. } = Syntax::wrap(list.clone())
        && list.last().unwrap().is_null()
    {
        list.len() - 1
    } else {
        return Err(Exception::error("expected proper list"));
    };

    let mut temporaries = Value::null();
    for _ in 0..length {
        let ident = Syntax::Identifier {
            ident: Identifier {
                sym: Symbol::gensym(),
                scopes: BTreeSet::new(),
            },
            span: Span::default(),
        };
        temporaries = Value::from((Value::from(ident), temporaries));
    }

    Ok(vec![temporaries])
}

#[bridge(name = "syntax-violation", lib = "(rnrs base builtins (6))")]
pub fn syntax_violation(
    who: &Value,
    message: &Value,
    form: &Value,
    subform: &[Value],
) -> Result<Vec<Value>, Exception> {
    let subform = match subform {
        [] => None,
        [subform] => Some(subform.clone()),
        _ => return Err(Exception::wrong_num_of_var_args(3..4, 3 + subform.len())),
    };
    let mut conditions = Vec::new();
    if who.is_true() {
        conditions.push(Value::from_rust_type(Who::new(who.clone())));
    } else if let Some(syntax) = form.cast_to_scheme_type::<Gc<Syntax>>() {
        let who = if let Syntax::Identifier { ident, .. } = syntax.as_ref() {
            Some(ident.sym)
        } else if let Some([Syntax::Identifier { ident, .. }, ..]) = syntax.as_list() {
            Some(ident.sym)
        } else {
            None
        };
        conditions.push(Value::from_rust_type(Who::new(Value::from(who))));
    }
    conditions.push(Value::from_rust_type(Message::new(message)));
    conditions.push(Value::from_rust_type(SyntaxViolation::new_from_values(
        form.clone(),
        subform,
    )));
    Err(Exception(Value::from(Exception::from(CompoundCondition(
        conditions,
    )))))
}
