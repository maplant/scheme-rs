//! Rust representation of S-expressions.

use crate::{
    // ast::Literal,
    ast::Primitive,
    env::{Binding, Environment, Scope, add_binding, resolve},
    exceptions::{CompoundCondition, Exception, Message, SyntaxViolation, Who},
    gc::{Gc, Trace},
    lists::list_to_vec_with_null,
    ports::Port,
    proc::Procedure,
    records::{RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::bridge,
    symbols::Symbol,
    syntax::parse::ParseSyntaxError,
    value::{Expect1, UnpackedValue, Value, ValueType},
};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt,
    hash::Hash,
    io::Cursor,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

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
    /*
    ByteVector {
        vector: Vec<u8>,
        span: Span,
    },
    */
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
                //Syntax::Vector { vector, .. } => {
                //    Value::from(vector.iter().cloned().map(Syntax::unwrap).map(Syntax::syntax_to_datum).collect::<Vec<_>>())
                //}
                //Syntax::List { list, .. } => {
                //    let mut cdr = Self::syntax_to_datum(Self::unwrap(list.last().unwrap().clone()));
                //    for car in list.iter().rev().skip(1) {
                //        cdr = Value::from((Self::syntax_to_datum(Self::unwrap(car.clone())), cdr))
                //    }
                //    cdr
                //}
                syn => Syntax::syntax_to_datum(Self::unwrap(syn.clone())), /*
                                                                               Syntax::Wrapped { value, .. } => value.clone(),
                                                                               Syntax::List { list, .. } => {
                                                                                   let mut cdr = Self::syntax_to_datum(Self::unwraplist.last().unwrap());
                                                                                   for car in list.iter().rev().skip(1) {
                                                                                       cdr = Value::from((Self::syntax_to_datum(unwrap(car.clone())), cdr))
                                                                                   }
                                                                           }
                                                                               */
            },
            unpacked => unpacked.into_value(),
        }
    }

    /*
        pub fn add_scope(&mut self, scope: Scope) {
            match self {
                Self::List { list, .. } => {
                    for item in list {
                        item.add_scope(scope);
                    }
                }
                Self::Vector { vector, .. } => {
                    for item in vector {
                        item.add_scope(scope);
                    }
                }
                Self::Identifier { ident, .. } => ident.add_scope(scope),
                _ => (),
            }
        }

        pub fn flip_scope(&mut self, scope: Scope) {
            match self {
                Self::List { list, .. } => {
                    for item in list {
                        item.flip_scope(scope);
                    }
                }
                Self::Vector { vector, .. } => {
                    for item in vector {
                        item.flip_scope(scope);
                    }
                }
                Self::Identifier { ident, .. } => ident.flip_scope(scope),
                _ => (),
            }
    }
        */

    /*
    pub fn mark(&mut self, mark: Mark) {
        match self {
            Self::List { list, .. } => {
                for item in list {
                    item.mark(mark);
                }
            }
            Self::Vector { vector, .. } => {
                for item in vector {
                    item.mark(mark);
                }
            }
            Self::Identifier { ident, .. } => ident.mark(mark),
            _ => (),
        }
    }

    pub fn mark_many(&mut self, marks: &BTreeSet<Mark>) {
        match self {
            Self::List { list, .. } => {
                for item in list {
                    item.mark_many(marks);
                }
            }
            Self::Vector { vector, .. } => {
                for item in vector {
                    item.mark_many(marks);
                }
            }
            Self::Identifier { ident, .. } => ident.mark_many(marks),
            _ => (),
        }
    }
    */

    pub(crate) fn car(&self) -> Option<&Self> {
        if let Some([car, ..]) = self.as_list() {
            Some(car)
        } else {
            None
        }
    }

    /*
    pub fn cdr(&self) -> Result<Self, Exception> {
        match self.as_list() {
            Some([_, null @ Syntax::Null { .. }]) => Ok(null.clone()),
            Some([_, cdr @ ..]) => Ok(Syntax::List {
                list: cdr.to_vec(),
                span: self.span().clone(),
            }),
            _ => Err(Exception::type_error("list", self.syntax_type())),
        }
    }
     */

    /*
    pub fn syntax_from_datum(scopes: &BTreeSet<Scope>, datum: Value) -> Result<Self, Exception> {
        // TODO: conjure up better values for Span
        match datum.unpack() {
            UnpackedValue::Boolean(b) => {
                Ok(Syntax::new_literal(Literal::Boolean(b), Span::default()))
            }
            UnpackedValue::Null => Ok(Syntax::new_null(Span::default())),
            UnpackedValue::Pair(pair) => {
                let (lhs, rhs) = pair.into();
                let mut list = Vec::new();
                list.push(lhs.clone());
                list_to_vec_with_null(&rhs, &mut list);
                let mut out_list = Vec::new();
                for item in list.iter() {
                    out_list.push(Syntax::syntax_from_datum(scopes, item.clone())?);
                }
                Ok(Syntax::new_list(out_list, Span::default()))
            }
            UnpackedValue::Syntax(syntax) => {
                let mut syntax = syntax.as_ref().clone();
                for scope in scopes {
                    syntax.add_scope(*scope);
                }
                Ok(syntax)
            }
            UnpackedValue::Vector(vec) => {
                let mut out_vec = Vec::new();
                for item in vec.0.vec.read().iter() {
                    out_vec.push(Syntax::syntax_from_datum(scopes, item.clone())?);
                }
                Ok(Syntax::new_vector(out_vec, Span::default()))
            }
            UnpackedValue::Symbol(sym) => {
                let ident = Identifier {
                    sym,
                    scopes: scopes.clone(),
                };
                Ok(Syntax::Identifier {
                    ident,
                    span: Span::default(),
                })
            }
            UnpackedValue::Number(num) => Ok(Syntax::Literal {
                literal: Literal::Number(num.clone()),
                span: Span::default(),
            }),
            UnpackedValue::String(string) => Ok(Syntax::Literal {
                literal: Literal::String(string.to_string()),
                span: Span::default(),
            }),
            datum => Err(Exception::error(format!(
                "cannot convert datum type {} to syntax object",
                datum.type_name()
            ))),
        }
    }
    */

    /*
    fn resolve_bindings<'a>(
        &'a mut self,
        env: &Environment,
        resolved_bindings: &mut HashMap<&'a Identifier, Binding>,
    ) {
        match self {
            Self::List { list, .. } => {
                for item in list {
                    item.resolve_bindings(env, resolved_bindings);
                }
            }
            Self::Vector { vector, .. } => {
                for item in vector {
                    item.resolve_bindings(env, resolved_bindings);
                }
            }
            &mut Self::Identifier {
                ref ident,
                ref mut binding,
                ..
            } => {
                *binding = Some(
                    resolved_bindings
                        .entry(ident)
                        .or_insert_with(|| env.fetch_binding(ident))
                        .clone(),
                );
            }
            _ => (),
        }
    }
     */

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
        println!("expanding: {self:?}");
        match self {
            Self::List { list, .. } => {
                // TODO: If list head is a list, do we expand this in here or in proc call?
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
            match maybe_await!(self.expand_once(&env)) {
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
            // Self::ByteVector { span, .. } => span,
            // Self::Literal { span, .. } => span,
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

    /*
    pub fn new_byte_vector(vector: Vec<u8>, span: impl Into<Span>) -> Self {
        Self::ByteVector {
            vector,
            span: span.into(),
        }
    }

    pub fn new_literal(literal: Literal, span: impl Into<Span>) -> Self {
        Self::Literal {
            literal,
            span: span.into(),
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Self::Literal { .. })
    }
    */

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
            // Syntax::Null { span, .. } => write!(f, "() @ {span}"),
            Syntax::List { list, .. } => {
                // Proper list
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
            /*
            Syntax::ByteVector { vector, .. } => {
                write!(f, "#vu8(")?;
                for (i, item) in vector.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item:x}")?;
                }
                write!(f, ")")
            }
            Syntax::Literal { literal, .. } => {
                write!(f, "{literal}")
            }
            */
            Syntax::Identifier { ident, span, .. } => {
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

/*

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Trace)]
pub struct Mark(usize);

impl Mark {
    pub fn new() -> Self {
        static NEXT_MARK: AtomicUsize = AtomicUsize::new(0);
        Self(NEXT_MARK.fetch_add(1, Ordering::Relaxed))
    }
}

impl Default for Mark {
    fn default() -> Self {
        Self::new()
    }
}

*/

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

    /*
    /// Return this identifier prefixed with the given string
    pub fn prefix(self, prefix: &str) -> Self {
        let sym = Symbol::intern(&format!("{prefix}{}", self.sym.to_str()));
        Self {
            sym,
            marks: self.marks,
        }
    }

    pub fn mark(&mut self, mark: Mark) {
        if self.marks.contains(&mark) {
            self.marks.remove(&mark);
        } else {
            self.marks.insert(mark);
        }
    }

    pub fn mark_many(&mut self, marks: &BTreeSet<Mark>) {
        self.marks = self.marks.symmetric_difference(marks).cloned().collect();
    }
    */
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

    /*
    match value.unpack() {
        UnpackedValue::Vector(vec) =>
    }
    */
    /*
    // TODO: This can certainly be done more efficiently
    Ok(vec![Value::datum_from_syntax(&Syntax::syntax_from_datum(
        &BTreeSet::default(),
        syn.clone(),
    )?)])
     */
    // todo!()
}

#[bridge(name = "datum->syntax", lib = "(rnrs syntax-case builtins (6))")]
pub fn datum_to_syntax(template_id: Identifier, datum: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(Syntax::datum_to_syntax(
        &template_id.scopes,
        datum.clone(),
    ))])
    /*
    let syntax: Arc<Syntax> = template_id.clone().try_into()?;
    let Syntax::Identifier {
        ident: template_id, ..
    } = syntax.as_ref()
    else {
        return Err(Exception::type_error("template_id", "syntax"));
    };
    Ok(vec![Value::from(Syntax::syntax_from_datum(
        &template_id.scopes,
        datum.clone(),
    )?)])
     */
    //todo!()
}

#[bridge(name = "identifier?", lib = "(rnrs syntax-case builtins (6))")]
pub fn identifier_pred(obj: &Value) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(obj.cast_to_scheme_type::<Identifier>().is_some())])
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
    let length = crate::lists::length(list)?;

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

    Ok(vec![Value::from(temporaries)])
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
