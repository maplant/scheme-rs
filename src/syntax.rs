use crate::{
    ast::{self, FetchVar, Literal, MacroExpansionPoint},
    compile::{Compile, CompileError},
    continuation::{CatchContinuationCall, Continuation},
    env::Env,
    error::RuntimeError,
    eval::Eval,
    gc::{Gc, Trace},
    lex::{InputSpan, Lexeme, Token},
    lists::list_to_vec_with_null,
    parse::ParseError,
    records,
    util::RequireOne,
    value::Value,
};
use futures::future::BoxFuture;
use proc_macros::builtin;
use std::{collections::BTreeSet, fmt, sync::Arc};

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

    // TODO: Return a Cow<'a, Self> instead to avoid the clone.
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

    pub fn resolve_bindings(&mut self, env: &Env) {
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

    async fn apply_transformer(
        &self,
        curr_env: &Env,
        macro_env: Env,
        cont: &Option<Arc<Continuation>>,
        transformer: Gc<Value>,
    ) -> Result<Expansion<'static>, RuntimeError> {
        // Create a new mark for the expansion context
        let new_mark = Mark::new();
        // Apply the new mark to the input
        // TODO: Figure out a better way to do this without cloning so much
        let mut input = self.clone();
        input.resolve_bindings(curr_env);
        input.mark(new_mark);
        // Call the transformer with the input:
        let transform = transformer.read().as_callable().unwrap();
        let mut output = {
            let output = transform
                .call(vec![Gc::new(Value::Syntax(input))], cont)
                .await?
                .eval(cont)
                .await?
                .require_one()?;
            let output = output.read();
            match &*output {
                Value::Syntax(syntax) => syntax.clone(),
                _ => todo!(),
            }
        };
        /*
            let mut output = match &*transformer.read() {
                Value::Procedure(proc) => {
                    let output = proc
                        .call(vec![Gc::new(Value::Syntax(input))], cont)
                        .await?
                        .eval(cont)
                        .await?
                        .require_one()?;
                    let output = output.read();
                    match &*output {
                        Value::Syntax(syntax) => syntax.clone(),
                        _ => todo!(),
                    }
                }
                Value::Transformer(transformer) => transformer
                    .expand(&input)
                    .ok_or_else(RuntimeError::no_patterns_match)?,
                x => return Err(RuntimeError::invalid_type("procedure", x.type_name())),
        };
            */
        // Apply the new mark to the output
        output.mark(new_mark);
        Ok(Expansion::Expanded {
            mark: new_mark,
            syntax: output,
            macro_env,
        })
    }

    fn expand<'a>(
        &'a self,
        env: &'a Env,
        cont: &'a Option<Arc<Continuation>>,
    ) -> BoxFuture<'a, Result<Expansion<'a>, RuntimeError>> {
        Box::pin(async move {
            match self {
                Self::List { list, .. } => {
                    // If the head is not an identifier, we leave the expression unexpanded
                    // for now. We will expand it later in the proc call
                    let ident = match list.first() {
                        Some(Self::Identifier { ident, .. }) => ident,
                        _ => return Ok(Expansion::Unexpanded(self)),
                    };
                    if let Some((macro_env, transformer)) = env.fetch_macro(ident) {
                        return self
                            .apply_transformer(env, macro_env, cont, transformer)
                            .await;
                    }
                }
                Self::Identifier { ident, .. } => {
                    if let Some((macro_env, transformer)) = env.fetch_macro(ident) {
                        return self
                            .apply_transformer(env, macro_env, cont, transformer)
                            .await;
                    }
                }
                _ => (),
            }
            Ok(Expansion::Unexpanded(self))
        })
    }

    pub async fn compile_expanded(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Arc<dyn Eval>, CompileError> {
        match self {
            Self::Null { span } => Err(CompileError::UnexpectedEmptyList(span.clone())),
            // Special identifiers:
            Self::Identifier { ident, .. } if ident == "<undefined>" => {
                Ok(Arc::new(Value::Undefined))
            }
            // Regular identifiers:
            Self::Identifier { ident, .. } => {
                Ok(Arc::new(FetchVar::new(ident.clone())) as Arc<dyn Eval>)
            }
            Self::Literal { literal, .. } => Ok(Arc::new(literal.clone()) as Arc<dyn Eval>),
            Self::List { list: exprs, span } => match &exprs[..] {
                // Function call:
                [Self::Identifier { ident, .. }, ..] if env.is_bound(ident) => {
                    ast::Call::compile_to_expr(exprs, env, cont, span).await
                }
                // Special forms:
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "quote" => {
                    ast::Quote::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "syntax" => {
                    ast::SyntaxQuote::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "begin" => {
                    ast::Body::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "let" => {
                    ast::Let::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "lambda" => {
                    ast::Lambda::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "if" => {
                    ast::If::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "and" => {
                    ast::And::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "or" => {
                    ast::Or::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "define" => {
                    ast::Define::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "define-syntax" => {
                    ast::DefineSyntax::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "syntax-case" => {
                    ast::SyntaxCase::compile_to_expr(tail, env, cont, span).await
                }
                [Self::Identifier { ident, span, .. }, tail @ ..]
                    if ident == "define-record-type" =>
                {
                    records::DefineRecordType::compile_to_expr(tail, env, cont, span).await
                }
                // Very special form:
                [Self::Identifier { ident, span, .. }, tail @ ..] if ident == "set!" => {
                    // Check for a variable transformer
                    if let Some(Syntax::Identifier { ident, .. }) = tail.first() {
                        if let Some((macro_env, transformer)) = env.fetch_macro(ident) {
                            if !transformer.read().is_variable_transformer() {
                                return Err(CompileError::NotVariableTransformer);
                            }
                            return self
                                .apply_transformer(env, macro_env, cont, transformer)
                                .await?
                                .compile(env, cont)
                                .await;
                        }
                    }
                    ast::Set::compile_to_expr(tail, env, cont, span).await
                }
                // Special function call:
                _ => ast::Call::compile_to_expr(exprs, env, cont, span).await,
            },
            Self::Vector { vector, .. } => {
                let mut vals = Vec::new();
                for item in vector {
                    match item {
                        Self::Null { .. } => vals.push(Arc::new(Value::Null) as Arc<dyn Eval>),
                        item => vals.push(item.compile(env, cont).await?),
                    }
                }
                Ok(Arc::new(ast::Vector { vals }) as Arc<dyn Eval>)
            }
        }
    }

    pub async fn compile(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Arc<dyn Eval>, CompileError> {
        self.expand(env, cont).await?.compile(env, cont).await
    }
}

pub enum Expansion<'a> {
    /// Syntax remained unchanged after expansion
    Unexpanded(&'a Syntax),
    /// Syntax was expanded, producing a new expansion context
    Expanded {
        mark: Mark,
        macro_env: Env,
        syntax: Syntax,
    },
}

impl Expansion<'_> {
    pub fn is_expanded(&self) -> bool {
        matches!(self, Self::Expanded { .. })
    }

    pub fn is_unexpanded(&self) -> bool {
        matches!(self, Self::Unexpanded(_))
    }
}

impl<'a> Expansion<'a> {
    pub fn compile(
        self,
        env: &'a Env,
        cont: &'a Option<Arc<Continuation>>,
    ) -> BoxFuture<'a, Result<Arc<dyn Eval>, CompileError>> {
        Box::pin(async move {
            match self {
                Self::Unexpanded(syntax) => syntax.compile_expanded(env, cont).await,
                Self::Expanded {
                    mark,
                    syntax,
                    macro_env,
                } => {
                    // If the expression has been expanded, we may need to expand it again, but
                    // it must be done in a new expansion context.
                    let env =
                        Env::Expansion(Gc::new(env.new_expansion_context(mark, macro_env.clone())));
                    Ok(Arc::new(MacroExpansionPoint::new(
                        mark,
                        macro_env,
                        syntax.expand(&env, cont).await?.compile(&env, cont).await?,
                    )) as Arc<dyn Eval>)
                }
            }
        })
    }
}

#[derive(Debug)]
pub struct ParsedSyntax {
    pub doc_comment: Option<String>,
    syntax: Syntax,
}

impl ParsedSyntax {
    fn parse_fragment<'a, 'b>(
        i: &'b [Token<'a>],
    ) -> Result<(&'b [Token<'a>], Self), ParseError<'a>> {
        let (doc_comment, remaining) = if let Token {
            lexeme: Lexeme::DocComment(ref doc_comment),
            ..
        } = i[0]
        {
            (Some(doc_comment.clone()), &i[1..])
        } else {
            (None, i)
        };
        let (remaining, syntax) = crate::parse::expression(remaining)?;
        Ok((
            remaining,
            Self {
                doc_comment,
                syntax,
            },
        ))
    }

    pub fn parse<'a>(mut i: &[Token<'a>]) -> Result<Vec<Self>, ParseError<'a>> {
        let mut output = Vec::new();
        while !i.is_empty() {
            let (remaining, expr) = Self::parse_fragment(i)?;
            output.push(expr);
            i = remaining
        }
        Ok(output)
    }

    pub async fn compile(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<Arc<dyn Eval>, CompileError> {
        Ok(Arc::new(CatchContinuationCall::new(
            self.syntax.compile(env, cont).await?,
        )))
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Trace)]
pub struct Mark(usize);

impl Mark {
    pub fn new() -> Self {
        Self(rand::random())
    }

    /// Obtain a mark from a Gc pointer value.
    pub fn from_gc<T: Trace>(gc: &Gc<T>) -> Self {
        Self(unsafe { gc.as_opaque().as_ptr() as *const () as usize })
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
        write!(f, "{}", self.name)
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

    pub fn new_list(list: Vec<Syntax>, span: impl Into<Span>) -> Self {
        Self::List {
            list,
            span: span.into(),
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

#[builtin("syntax->datum")]
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

#[builtin("datum->syntax")]
pub async fn datum_to_syntax(
    _cont: &Option<Arc<Continuation>>,
    template_id: &Gc<Value>,
    datum: &Gc<Value>,
) -> Result<Vec<Gc<Value>>, RuntimeError> {
    let template_id = template_id.read();
    let Value::Syntax(Syntax::Identifier {
        ident: template_id, ..
    }) = &*template_id
    else {
        return Err(RuntimeError::invalid_type(
            "syntax",
            template_id.type_name(),
        ));
    };
    Ok(vec![Gc::new(Value::Syntax(Syntax::from_datum(
        &template_id.marks,
        datum,
    )))])
}
