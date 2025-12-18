use crate::{
    ast::Literal,
    env::{Environment, Keyword},
    exceptions::Condition,
    gc::{Trace, Gc},
    lists::list_to_vec_with_null,
    ports::Port,
    registry::bridge,
    symbols::Symbol,
    syntax::parse::ParseSyntaxError,
    value::{UnpackedValue, Value, ValueType},
};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::{
    collections::{BTreeSet, HashMap},
    fmt,
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

/// Representation of a Scheme syntax object, or s-expression.
#[derive(Clone, Trace)]
#[repr(align(16))]
pub enum Syntax {
    /// An empty list.
    Null {
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
    ByteVector {
        vector: Vec<u8>,
        span: Span,
    },
    Literal {
        literal: Literal,
        span: Span,
    },
    Identifier {
        ident: Identifier,
        binding_env: Option<Environment>,
        span: Span,
    },
}

impl Syntax {
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

    pub fn car(&self) -> Result<Self, Condition> {
        let Some([car, ..]) = self.as_list() else {
            return Err(Condition::type_error("list", self.syntax_type()));
        };
        Ok(car.clone())
    }

    pub fn cdr(&self) -> Result<Self, Condition> {
        match self.as_list() {
            Some([_, null @ Syntax::Null { .. }]) => Ok(null.clone()),
            Some([_, cdr @ ..]) => Ok(Syntax::List {
                list: cdr.to_vec(),
                span: self.span().clone(),
            }),
            _ => Err(Condition::type_error("list", self.syntax_type())),
        }
    }

    pub fn syntax_type(&self) -> &'static str {
        "todo"
    }

    pub fn syntax_from_datum(marks: &BTreeSet<Mark>, datum: Value) -> Self {
        // TODO: conjure up better values for Span
        match datum.unpack() {
            UnpackedValue::Boolean(b) => Syntax::new_literal(Literal::Boolean(b), Span::default()),
            UnpackedValue::Null => Syntax::new_null(Span::default()),
            UnpackedValue::Pair(pair) => {
                let (lhs, rhs) = pair.into();
                let mut list = Vec::new();
                list.push(lhs.clone());
                list_to_vec_with_null(&rhs, &mut list);
                let mut out_list = Vec::new();
                for item in list.iter() {
                    out_list.push(Syntax::syntax_from_datum(marks, item.clone()));
                }
                Syntax::new_list(out_list, Span::default())
            }
            UnpackedValue::Syntax(syntax) => {
                let mut syntax = syntax.as_ref().clone();
                syntax.mark_many(marks);
                syntax
            }
            UnpackedValue::Vector(vec) => {
                let mut out_vec = Vec::new();
                for item in vec.0.vec.read().iter() {
                    out_vec.push(Syntax::syntax_from_datum(marks, item.clone()));
                }
                Syntax::new_vector(out_vec, Span::default())
            }
            UnpackedValue::Symbol(sym) => {
                let ident = Identifier {
                    sym,
                    marks: marks.clone(),
                };
                Syntax::Identifier {
                    ident,
                    binding_env: None,
                    span: Span::default(),
                }
            }
            UnpackedValue::Number(num) => Syntax::Literal {
                literal: Literal::Number(num.as_ref().clone()),
                span: Span::default(),
            },
            x => unimplemented!("{:?}", x.into_value()),
        }
    }

    fn resolve_bindings<'a>(
        &'a mut self,
        env: &Environment,
        resolved_bindings: &mut HashMap<&'a Identifier, Option<Environment>>,
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
                ref mut binding_env,
                ..
            } => {
                *binding_env = resolved_bindings
                    .entry(ident)
                    .or_insert_with(|| env.binding_env(ident))
                    .clone()
            }
            _ => (),
        }
    }

    #[maybe_async]
    fn apply_transformer(&self, env: &Environment, mac: Keyword) -> Result<Expansion, Value> {
        // Create a new mark for the expansion context
        let new_mark = Mark::new();

        // Apply the new mark to the input and resolve any bindings
        let mut input = self.clone();
        input.resolve_bindings(env, &mut HashMap::default());
        input.mark(new_mark);

        // Call the transformer with the input:
        let transformer_output = maybe_await!(mac.transformer.call(&[Value::from(input)]))?;

        // Output must be syntax:
        let output: Gc<Syntax> = transformer_output
            .first()
            .ok_or_else(|| Condition::syntax(self.clone(), None))?
            .clone()
            .try_into()?;

        // Apply the new mark to the output
        let mut output = output.as_ref().clone();
        output.mark(new_mark);

        let new_env = env.new_macro_expansion(new_mark, mac.source_env.clone());

        Ok(Expansion::new_expanded(new_env, output))
    }

    #[cfg(not(feature = "async"))]
    fn expand_once(&self, env: &Environment) -> Result<Expansion, Value> {
        self.expand_once_inner(env)
    }

    #[cfg(feature = "async")]
    fn expand_once<'a>(&'a self, env: &'a Environment) -> BoxFuture<'a, Result<Expansion, Value>> {
        Box::pin(self.expand_once_inner(env))
    }

    #[maybe_async]
    fn expand_once_inner(&self, env: &Environment) -> Result<Expansion, Value> {
        match self {
            Self::List { list, .. } => {
                // TODO: If list head is a list, do we expand this in here or in proc call?
                let ident = match list.first() {
                    Some(Self::Identifier { ident, .. }) => ident,
                    _ => return Ok(Expansion::Unexpanded),
                };
                if let Some(mac) = maybe_await!(env.fetch_keyword(ident))? {
                    return maybe_await!(self.apply_transformer(env, mac));
                }

                // Check for set! macro
                match &list.as_slice()[1..] {
                    [Syntax::Identifier { ident: var, .. }, ..] if ident == "set!" => {
                        // Look for a variable transformer:
                        if let Some(mac) = maybe_await!(env.fetch_keyword(var))? {
                            if !mac.transformer.is_variable_transformer() {
                                return Err(Condition::error(format!(
                                    "{} not a variable transformer",
                                    var.sym
                                ))
                                .into());
                            }
                            return maybe_await!(self.apply_transformer(env, mac));
                        }
                    }
                    _ => (),
                }
            }
            Self::Identifier { ident, .. } => {
                if let Some(mac) = maybe_await!(env.fetch_keyword(ident))? {
                    return maybe_await!(self.apply_transformer(env, mac));
                }
            }
            _ => (),
        }
        Ok(Expansion::Unexpanded)
    }

    /// Fully expand the outermost syntax object.
    #[maybe_async]
    pub fn expand(mut self, env: &Environment) -> Result<FullyExpanded, Value> {
        let mut curr_env = env.clone();
        loop {
            match maybe_await!(self.expand_once(&curr_env))? {
                Expansion::Unexpanded => {
                    return Ok(FullyExpanded::new(curr_env, self));
                }
                Expansion::Expanded { new_env, syntax } => {
                    curr_env = new_env;
                    self = syntax;
                }
            }
        }
    }

    #[cfg(not(feature = "async"))]
    pub fn from_str(s: &str, file_name: Option<&str>) -> Result<Vec<Self>, ParseSyntaxError> {
        use crate::ports::{BufferMode, Transcoder};

        let file_name = file_name.unwrap_or("<unknown>");
        let bytes = Cursor::new(s.as_bytes().to_vec());
        let port = Port::new(bytes, BufferMode::Block, Some(Transcoder::native()));
        port.all_sexprs(Span::new(file_name))
    }

    #[cfg(feature = "async")]
    pub fn from_str(s: &str, file_name: Option<&str>) -> Result<Vec<Self>, ParseSyntaxError> {
        use crate::ports::{BufferMode, Transcoder};

        let file_name = file_name.unwrap_or("<unknown>");
        let bytes = Cursor::new(s.as_bytes().to_vec());

        // This is kind of convoluted, but convenient
        let port =
            Arc::into_inner(Port::new(bytes, BufferMode::Block, Some(Transcoder::native())).0)
                .unwrap();
        let info = port.info;
        let mut data = port.data.into_inner();

        // This is safe since we don't need the async executor to drive anything
        // here
        futures::executor::block_on(async move {
            use crate::syntax::parse::Parser;

            let mut parser = Parser::new(&mut data, info, Span::new(file_name));
            parser.all_sexprs().await
        })
    }

    /// Returns true if the syntax item is a list with a car that is an
    /// identifier equal to the passed argument.
    pub(crate) fn has_car(&self, car: &str) -> bool {
        matches!(self.as_list(), Some([Self::Identifier { ident, .. }, .. ]) if ident == car)
    }
}

impl fmt::Debug for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Syntax::Null { .. } => write!(f, "()"),
            Syntax::List { list, .. } => {
                // Proper list
                let proper_list = matches!(list.last(), Some(Syntax::Null { .. }));
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
            Syntax::ByteVector { vector, .. } => {
                write!(f, "#u8(")?;
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
            Syntax::Identifier { ident, .. } => {
                write!(f, "{}", ident.sym)
            }
        }
    }
}

pub enum Expansion {
    /// Syntax remained unchanged after expansion
    Unexpanded,
    /// Syntax was expanded, producing a new expansion context
    Expanded {
        new_env: Environment,
        syntax: Syntax,
    },
}

impl Expansion {
    fn new_expanded(new_env: Environment, syntax: Syntax) -> Self {
        Self::Expanded { new_env, syntax }
    }
}

pub struct FullyExpanded {
    pub expansion_env: Environment,
    pub expanded: Syntax,
}

impl FullyExpanded {
    pub fn new(expansion_env: Environment, expanded: Syntax) -> Self {
        Self {
            expansion_env,
            expanded,
        }
    }
}

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

#[derive(Clone, Hash, PartialEq, Eq, Trace)]
pub struct Identifier {
    pub sym: Symbol,
    pub marks: BTreeSet<Mark>,
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.sym)
    }
}

impl Identifier {
    pub fn new(name: &str) -> Self {
        Self {
            sym: Symbol::intern(name),
            marks: BTreeSet::default(),
        }
    }

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
}

impl PartialEq<str> for Identifier {
    fn eq(&self, rhs: &str) -> bool {
        self.sym.to_str().as_ref() == rhs
    }
}

impl Syntax {
    pub fn span(&self) -> &Span {
        match self {
            Self::Null { span } => span,
            Self::List { span, .. } => span,
            Self::Vector { span, .. } => span,
            Self::ByteVector { span, .. } => span,
            Self::Literal { span, .. } => span,
            Self::Identifier { span, .. } => span,
        }
    }

    pub fn new_null(span: impl Into<Span>) -> Self {
        Self::Null { span: span.into() }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null { .. })
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

    pub fn is_list(&self) -> bool {
        matches!(self, Self::List { .. })
    }

    pub fn new_vector(vector: Vec<Syntax>, span: impl Into<Span>) -> Self {
        Self::Vector {
            vector,
            span: span.into(),
        }
    }

    pub fn new_byte_vector(vector: Vec<u8>, span: impl Into<Span>) -> Self {
        Self::ByteVector {
            vector,
            span: span.into(),
        }
    }

    pub fn is_vector(&self) -> bool {
        matches!(self, Self::Vector { .. })
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

    pub fn new_identifier(name: &str, span: impl Into<Span>) -> Self {
        Self::Identifier {
            ident: Identifier::new(name),
            span: span.into(),
            binding_env: None,
        }
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier { .. })
    }
}

#[bridge(name = "syntax->datum", lib = "(rnrs syntax-case builtins (6))")]
pub fn syntax_to_datum(syn: &Value) -> Result<Vec<Value>, Condition> {
    let syn: Gc<Syntax> = syn.clone().try_into()?;
    Ok(vec![Value::datum_from_syntax(syn.as_ref())])
}

#[bridge(name = "datum->syntax", lib = "(rnrs syntax-case builtins (6))")]
pub fn datum_to_syntax(template_id: &Value, datum: &Value) -> Result<Vec<Value>, Condition> {
    let syntax: Gc<Syntax> = template_id.clone().try_into()?;
    let Syntax::Identifier {
        ident: template_id, ..
    } = syntax.as_ref()
    else {
        return Err(Condition::type_error("template_id", "syntax"));
    };
    Ok(vec![Value::from(Syntax::syntax_from_datum(
        &template_id.marks,
        datum.clone(),
    ))])
}

#[bridge(name = "identifier?", lib = "(rnrs syntax-case builtins (6))")]
pub fn identifier_pred(obj: &Value) -> Result<Vec<Value>, Condition> {
    let Ok(syn) = Gc::<Syntax>::try_from(obj.clone()) else {
        return Ok(vec![Value::from(false)]);
    };
    Ok(vec![Value::from(syn.is_identifier())])
}

#[bridge(name = "bound-identifier=?", lib = "(rnrs syntax-case builtins (6))")]
pub fn bound_identifier_eq_pred(id1: &Value, id2: &Value) -> Result<Vec<Value>, Condition> {
    let id1: Gc<Syntax> = id1.clone().try_into()?;
    let id2: Gc<Syntax> = id2.clone().try_into()?;
    let Syntax::Identifier { ident: id1, .. } = id1.as_ref() else {
        return Err(Condition::type_error("identifier", "syntax"));
    };
    let Syntax::Identifier { ident: id2, .. } = id2.as_ref() else {
        return Err(Condition::type_error("identifier", "syntax"));
    };
    Ok(vec![Value::from(id1 == id2)])
}

#[bridge(name = "free-identifier=?", lib = "(rnrs syntax-case builtins (6))")]
pub fn free_identifier_eq_pred(id1: &Value, id2: &Value) -> Result<Vec<Value>, Condition> {
    let id1: Gc<Syntax> = id1.clone().try_into()?;
    let id2: Gc<Syntax> = id2.clone().try_into()?;
    let Syntax::Identifier {
        ident: id1,
        binding_env: bound_id1,
        ..
    } = id1.as_ref()
    else {
        return Err(Condition::type_error("identifier", "syntax"));
    };
    let Syntax::Identifier {
        ident: id2,
        binding_env: bound_id2,
        ..
    } = id2.as_ref()
    else {
        return Err(Condition::type_error("identifier", "syntax"));
    };
    Ok(vec![Value::from(
        bound_id1 == bound_id2 && id1.sym == id2.sym,
    )])
}

#[bridge(name = "generate-temporaries", lib = "(rnrs syntax-case builtins (6))")]
pub fn generate_temporaries(list: &Value) -> Result<Vec<Value>, Condition> {
    let length = match list.type_of() {
        ValueType::Pair => crate::lists::length(list)?,
        ValueType::Syntax => {
            let syntax: Gc<Syntax> = list.clone().try_into()?;
            match &*syntax {
                // TODO: Check for proper list?
                Syntax::List { list, .. } => list.len() - 1,
                _ => return Err(Condition::error("Syntax object must be a list".to_string())),
            }
        }
        _ => return Err(Condition::error("argument must be a list".to_string())),
    };

    // We can use marks to create unique temporaries
    let mark = Mark::new();
    let mut idents = (0..length)
        .map(|i| {
            let mut ident = Identifier::new(&format!("${i}"));
            ident.mark(mark);
            Syntax::Identifier {
                ident,
                binding_env: None,
                span: Span::default(),
            }
        })
        .collect::<Vec<_>>();
    idents.push(Syntax::Null {
        span: Span::default(),
    });
    Ok(vec![Value::from(Syntax::List {
        list: idents,
        span: Span::default(),
    })])
}
