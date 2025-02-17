use crate::{
    ast::Literal,
    env::{Environment, Macro},
    exception::Exception,
    gc::{Gc, Trace},
    lex::{InputSpan, Token},
    lists::list_to_vec_with_null,
    parse::ParseSyntaxError,
    registry::bridge,
    value::Value,
};
use futures::future::BoxFuture;
use std::{
    collections::{BTreeSet, HashSet},
    fmt,
    sync::Arc,
};

#[derive(Debug, Clone, PartialEq, Trace)]
pub struct Span {
    pub line: u32,
    pub column: usize,
    pub offset: usize,
    pub file: Arc<String>,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            line: 0,
            column: 0,
            offset: 0,
            file: Arc::new(String::new()),
        }
    }
}

impl From<InputSpan<'_>> for Span {
    fn from(span: InputSpan<'_>) -> Self {
        Span {
            line: span.location_line(),
            column: span.get_column(),
            offset: span.location_offset(),
            file: span.extra.clone(),
        }
    }
}

#[derive(Clone, derive_more::Debug, Trace)]
// TODO: Make cloning this struct as fast as possible.
pub enum Syntax {
    /// An empty list.
    Null {
        #[debug(skip)]
        span: Span,
    },
    /// A nested grouping of pairs. If the expression is a proper list, then the
    /// last element of expression will be Null. This vector is guaranteed to contain
    /// at least two elements.
    List {
        list: Vec<Syntax>,
        #[debug(skip)]
        span: Span,
    },
    Vector {
        vector: Vec<Syntax>,
        #[debug(skip)]
        span: Span,
    },
    Literal {
        literal: Literal,
        #[debug(skip)]
        span: Span,
    },
    Identifier {
        ident: Identifier,
        #[debug(skip)]
        bound: bool,
        #[debug(skip)]
        span: Span,
    },
}

impl Syntax {
    pub fn mark(&mut self, mark: Mark) {
        match self {
            Self::List { ref mut list, .. } => {
                for item in list {
                    item.mark(mark);
                }
            }
            Self::Vector { ref mut vector, .. } => {
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
            Self::List { ref mut list, .. } => {
                for item in list {
                    item.mark_many(marks);
                }
            }
            Self::Vector { ref mut vector, .. } => {
                for item in vector {
                    item.mark_many(marks);
                }
            }
            Self::Identifier { ident, .. } => ident.mark_many(marks),
            _ => (),
        }
    }

    // I do not like the fact that this function exists.
    pub fn normalize(self) -> Self {
        match self {
            Self::List { mut list, span } => {
                if let [Syntax::Null { .. }] = list.as_slice() {
                    list.pop().unwrap()
                } else if list.is_empty() {
                    Syntax::Null { span }
                } else {
                    Self::List { list, span }
                }
            }
            x => x,
        }
    }

    pub fn from_datum(marks: &BTreeSet<Mark>, datum: &Gc<Value>) -> Self {
        let datum = datum.read();
        // TODO: conjure up better values for Span
        match &*datum {
            Value::Null => Syntax::new_null(Span::default()),
            Value::Pair(lhs, rhs) => {
                let mut list = Vec::new();
                list.push(lhs.clone());
                list_to_vec_with_null(rhs, &mut list);
                // TODO: Use futures combinators
                let mut out_list = Vec::new();
                for item in list.iter() {
                    out_list.push(Syntax::from_datum(marks, item));
                }
                Syntax::new_list(out_list, Span::default())
            }
            Value::Syntax(syntax) => {
                let mut syntax = syntax.clone();
                syntax.mark_many(marks);
                syntax
            }
            Value::Symbol(sym) => {
                let ident = Identifier {
                    name: sym.clone(),
                    marks: marks.clone(),
                };
                Syntax::Identifier {
                    ident,
                    bound: false,
                    span: Span::default(),
                }
            }
            _ => unimplemented!(),
        }
    }

    pub fn resolve_bindings(&mut self, env: &Environment) {
        match self {
            Self::List { ref mut list, .. } => {
                for item in list {
                    item.resolve_bindings(env);
                }
            }
            Self::Vector { ref mut vector, .. } => {
                for item in vector {
                    item.resolve_bindings(env);
                }
            }
            Self::Identifier {
                ref ident,
                ref mut bound,
                ..
            } => *bound = env.is_bound(ident),
            _ => (),
        }
    }

    #[allow(dead_code)]
    async fn apply_transformer(
        &self,
        env: &Environment,
        mac: Macro,
        // cont: &Closure,
    ) -> Result<Expansion, Exception> {
        // Create a new mark for the expansion context
        let new_mark = Mark::new();

        // Apply the new mark to the input and resolve any bindings
        // TODO: Figure out a better way to do this without cloning so much
        let mut input = self.clone();
        input.resolve_bindings(env);
        input.mark(new_mark);

        // Call the transformer with the input:

        let transformer_output = mac
            .transformer
            .call(&[Gc::new(Value::Syntax(input))])
            .await?;
        let transformer_output = transformer_output[0].read();

        // Output must be syntax:
        let output: &Syntax = transformer_output.as_ref().try_into()?;

        // Apply the new mark to the output
        let mut output = output.clone();
        output.mark(new_mark);

        let new_env = env.new_macro_expansion(new_mark, mac.source_env.clone());

        Ok(Expansion::new_expanded(new_env, output))
    }

    fn expand_once<'a>(
        &'a self,
        env: &'a Environment,
        // cont: &Closure,
    ) -> BoxFuture<'a, Result<Expansion, Exception>> {
        Box::pin(async move {
            match self {
                Self::List { list, .. } => {
                    // TODO: If list head is a list, do we expand this in here or in proc call?

                    let ident = match list.first() {
                        Some(Self::Identifier { ident, .. }) => ident,
                        _ => return Ok(Expansion::Unexpanded),
                    };
                    if let Some(mac) = env.fetch_macro(ident) {
                        return self.apply_transformer(env, mac).await;
                    }

                    // Check for set! macro
                    match list.as_slice() {
                        [Syntax::Identifier { ident, .. }, ..] if ident.name == "set!" => {
                            // Look for variable transformer:
                            if let Some(mac) = env.fetch_macro(ident) {
                                /*
                                if !mac.transformer.read().is_variable_transformer() {
                                    return Err(Exception::error(
                                        "Not a variable transformer".to_string(),
                                    ));
                                }
                                */
                                return self.apply_transformer(env, mac).await;
                            }
                        }
                        _ => (),
                    }
                }
                Self::Identifier { ident, .. } => {
                    if let Some(mac) = env.fetch_macro(ident) {
                        return self.apply_transformer(env, mac).await;
                    }
                }
                _ => (),
            }
            Ok(Expansion::Unexpanded)
        })
    }

    /// Fully expand the outermost syntax object.
    pub async fn expand(
        mut self,
        env: &Environment,
        // cont: &Closure,
    ) -> Result<FullyExpanded, Exception> {
        let mut curr_env = env.clone();
        loop {
            match self.expand_once(&curr_env).await? {
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

    fn parse_fragment<'a, 'b>(
        i: &'b [Token<'a>],
    ) -> Result<(&'b [Token<'a>], Self), ParseSyntaxError<'a>> {
        let (remaining, syntax) = crate::parse::expression(i)?;
        Ok((remaining, syntax))
    }

    pub fn parse<'a>(mut i: &[Token<'a>]) -> Result<Vec<Self>, ParseSyntaxError<'a>> {
        let mut output = Vec::new();
        while !i.is_empty() {
            let (remaining, expr) = Self::parse_fragment(i)?;
            output.push(expr);
            i = remaining
        }
        Ok(output)
    }

    pub fn from_str<'a>(
        s: &'a str,
        file_name: Option<&str>,
    ) -> Result<Vec<Self>, ParseSyntaxError<'a>> {
        let tokens = Token::tokenize(s, file_name)?;
        Self::parse(&tokens)
    }

    pub fn fetch_all_identifiers(&self, idents: &mut HashSet<Identifier>) {
        match self {
            Self::List { list: syns, .. } | Self::Vector { vector: syns, .. } => {
                for item in syns {
                    item.fetch_all_identifiers(idents);
                }
            }
            Self::Identifier { ident, .. } => {
                idents.insert(ident.clone());
            }
            _ => (),
        }
    }
}

// #[derive(derive_more::Debug)]
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

/*
#[derive(derive_more::Debug, Clone)]
pub struct ExpansionCtx {
    pub mark: Mark,
    #[debug(skip)]
    pub env: Gc<Env>,
}

impl ExpansionCtx {
    fn new(mark: Mark, env: Gc<Env>) -> Self {
        Self { mark, env }
    }
}

#[derive(Debug)]
pub struct FullyExpanded {
    pub expansion_ctxs: Vec<ExpansionCtx>,
    pub expanded: Syntax,
}

impl AsRef<Syntax> for FullyExpanded {
    fn as_ref(&self) -> &Syntax {
        &self.expanded
    }
}
*/

#[derive(Debug)]
pub struct ParsedSyntax {
    pub doc_comment: Option<String>,
    pub syntax: Syntax,
}

impl ParsedSyntax {}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Trace)]
pub struct Mark(usize);

impl Mark {
    pub fn new() -> Self {
        Self(rand::random())
    }
}

impl Default for Mark {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Trace)]
pub struct Identifier {
    pub name: String,
    pub marks: BTreeSet<Mark>,
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} ({})",
            self.name,
            self.marks
                .iter()
                .map(|m| m.0.to_string() + " ")
                .collect::<String>()
        )
    }
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Self {
            name,
            marks: BTreeSet::default(),
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
        self.name == rhs
    }
}

impl Syntax {
    pub fn span(&self) -> &Span {
        match self {
            Self::Null { span } => span,
            Self::List { span, .. } => span,
            Self::Vector { span, .. } => span,
            Self::Literal { span, .. } => span,
            Self::Identifier { span, .. } => span,
        }
    }

    // There's got to be a better way:

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
            ident: Identifier::new(name.to_string()),
            span: span.into(),
            bound: false,
        }
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier { .. })
    }
}

/*
#[bridge(name = "syntax->datum", )]
pub async fn syntax_to_datum(
    _cont: &Option<Arc<Continuation>>,
    syn: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let syn = syn.read();
    let Value::Syntax(ref syn) = &*syn else {
        return Err(RuntimeError::invalid_type("syntax", syn.type_name()));
    };
    Ok(vec![Gc::new(Value::from_syntax(syn))])
}
*/

#[bridge(name = "datum->syntax", lib = "(base)")]
pub async fn datum_to_syntax(
    template_id: &Gc<Value>,
    datum: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, Exception> {
    let template_id = template_id.read();
    let Value::Syntax(Syntax::Identifier {
        ident: template_id, ..
    }) = &*template_id
    else {
        return Err(Exception::invalid_type("syntax", template_id.type_name()));
    };
    Ok(vec![Gc::new(Value::Syntax(Syntax::from_datum(
        &template_id.marks,
        datum,
    )))])
}
