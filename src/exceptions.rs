//! Exceptional situations and conditions

use scheme_rs_macros::{cps_bridge, runtime_fn};

use crate::{
    ast::ParseAstError,
    gc::{Gc, GcInner, Trace},
    proc::{Application, DynStack, DynStackElem, FuncPtr, Procedure, pop_dyn_stack},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    syntax::{Identifier, Span, Syntax, parse::ParseSyntaxError},
    value::Value,
};

pub use scheme_rs_macros::define_condition_type;

use std::{error::Error as StdError, fmt, ops::Range, sync::Arc};

#[derive(Clone, Trace)]
pub struct Exception {
    pub backtrace: Vec<Frame>,
    pub obj: Value,
}

impl Exception {
    pub fn new(backtrace: Vec<Frame>, obj: Value) -> Self {
        Self { backtrace, obj }
    }
}

impl fmt::Display for Exception {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Uncaught exception: {}", self.obj)?;
        Ok(())
    }
}

impl fmt::Debug for Exception {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const MAX_BACKTRACE_LEN: usize = 20;
        writeln!(f, "Uncaught exception: {}", self.obj)?;
        if !self.backtrace.is_empty() {
            writeln!(f, "Stack trace:")?;
            for (i, frame) in self.backtrace.iter().rev().enumerate() {
                if i >= MAX_BACKTRACE_LEN {
                    writeln!(f, "(backtrace truncated)")?;
                    break;
                }
                writeln!(f, "{i}: {frame}")?;
            }
        }
        Ok(())
    }
}

impl StdError for Exception {}

impl From<Condition> for Exception {
    fn from(cond: Condition) -> Self {
        Self {
            backtrace: Vec::new(),
            obj: cond.0,
        }
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Value as fmt::Debug>::fmt(&self.0, f)
    }
}

impl From<ParseSyntaxError> for Condition {
    fn from(_error: ParseSyntaxError) -> Self {
        todo!()
    }
}

impl From<Exception> for Condition {
    fn from(e: Exception) -> Self {
        // For now just drop the back trace:
        Self(e.obj)
    }
}

#[derive(Debug, Clone)]
pub struct Condition(pub Value);

impl Condition {
    pub fn error(message: impl fmt::Display) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((Assertion::new(), Message::new(message.to_string()))),
        )))
    }

    pub fn syntax(form: Syntax, subform: Option<Syntax>) -> Self {
        Self(Value::from(Record::from_rust_type(SyntaxViolation::new(
            form, subform,
        ))))
    }

    pub fn undefined(ident: Identifier) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((
                Undefined::new(),
                Message::new(format!("Undefined variable {}", ident.sym)),
            )),
        )))
    }

    pub fn type_error(expected: &str, provided: &str) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((
                Assertion::new(),
                Message::new(format!(
                    "Expected value of type {expected}, provided {provided}"
                )),
            )),
        )))
    }

    pub fn invalid_operator(provided: &str) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((
                Assertion::new(),
                Message::new(format!(
                    "Invalid operator, expected procedure, provided {provided}"
                )),
            )),
        )))
    }

    pub fn invalid_index(index: usize, len: usize) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((
                Assertion::new(),
                Message::new(format!(
                    "Invalid index of {index} into collection of size {len}"
                )),
            )),
        )))
    }

    pub fn invalid_range(range: Range<usize>, len: usize) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((
                Assertion::new(),
                Message::new(format!(
                    "Invalid range of {range:?} into collection of size {len}"
                )),
            )),
        )))
    }

    pub fn wrong_num_of_unicode_chars(expected: usize, provided: usize) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((
                Assertion::new(),
                Message::new(format!(
                    "Expected to receive {expected} unicode characters from transform, received {provided}"
                )),
            )),
        )))
    }

    pub fn wrong_num_of_args(expected: usize, provided: usize) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((
                Assertion::new(),
                Message::new(format!(
                    "Expected {expected} arguments, provided {provided}"
                )),
            )),
        )))
    }

    pub fn wrong_num_of_var_args(expected: Range<usize>, provided: usize) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((
                Assertion::new(),
                Message::new(format!(
                    "Expected {expected:?} arguments, provided {provided}"
                )),
            )),
        )))
    }

    /// For when we cannot convert a value into the requested type.
    ///
    /// Example: Integer to a Complex
    pub fn conversion_error(expected: &str, provided: &str) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((
                Assertion::new(),
                Message::new(format!("Could not convert {provided} into {expected}")),
            )),
        )))
    }

    /// For when we cannot represent the value into the requested type.
    ///
    /// Example: an u128 number as an u8
    pub fn not_representable(value: &str, r#type: &str) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((
                Assertion::new(),
                Message::new(format!("Could not represent '{value}' in {type} type")),
            )),
        )))
    }
}

impl From<SimpleCondition> for Condition {
    fn from(simple: SimpleCondition) -> Self {
        Self(Value::from(Record::from_rust_type(simple)))
    }
}

impl From<Warning> for Condition {
    fn from(warning: Warning) -> Self {
        Self(Value::from(Record::from_rust_type(warning)))
    }
}

impl From<Serious> for Condition {
    fn from(serious: Serious) -> Self {
        Self(Value::from(Record::from_rust_type(serious)))
    }
}

impl From<ParseAstError> for Condition {
    fn from(value: ParseAstError) -> Self {
        Condition::error(format!("Error parsing: {value:?}"))
    }
}

macro_rules! impl_into_condition_for {
    ($for:ty) => {
        impl From<$for> for Condition {
            fn from(e: $for) -> Self {
                Self::error(e.to_string())
            }
        }
    };
}

impl_into_condition_for!(Box<crate::num::ArithmeticError>);
impl_into_condition_for!(std::num::TryFromIntError);

#[derive(Copy, Clone, Default, Debug, Trace)]
pub struct SimpleCondition;

impl SimpleCondition {
    pub fn new() -> Self {
        Self
    }
}

impl SchemeCompatible for SimpleCondition {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "&condition")
    }
}

define_condition_type!(
    rust_name: Warning,
    scheme_name: "&warning",
    parent: SimpleCondition,
);

impl Warning {
    pub fn new() -> Self {
        Self {
            parent: Gc::new(SimpleCondition::new()),
        }
    }
}

define_condition_type!(
    rust_name: Serious,
    scheme_name: "&serious",
    parent: SimpleCondition,
);

impl Serious {
    pub fn new() -> Self {
        Self {
            parent: Gc::new(SimpleCondition::new()),
        }
    }
}

define_condition_type!(
    rust_name: Message,
    scheme_name: "&message",
    parent: SimpleCondition,
    fields: {
        message: String,
    },
    constructor: |message| {
        Ok(Message { parent: Gc::new(SimpleCondition::new()), message: message.to_string() })
    },
    debug: |this, f| {
        write!(f, " ")?;
        this.message.fmt(f)
    }
);

impl Message {
    pub fn new(message: impl std::fmt::Display) -> Self {
        Self {
            parent: Gc::new(SimpleCondition::new()),
            message: message.to_string(),
        }
    }
}

define_condition_type!(
    rust_name: Error,
    scheme_name: "&error",
    parent: Serious,
);

impl Error {
    pub fn new() -> Self {
        Self {
            parent: Gc::new(Serious::new()),
        }
    }
}

define_condition_type!(
    rust_name: Violation,
    scheme_name: "&violation",
    parent: Serious,
);

impl Violation {
    pub fn new() -> Self {
        Self {
            parent: Gc::new(Serious::new()),
        }
    }
}

define_condition_type!(
    rust_name: Assertion,
    scheme_name: "&assertion",
    parent: Violation
);

impl Assertion {
    pub fn new() -> Self {
        Self {
            parent: Gc::new(Violation::new()),
        }
    }
}

define_condition_type!(
    rust_name: SyntaxViolation,
    scheme_name: "&syntax",
    parent: Violation,
    fields: {
        form: Arc<Syntax>,
        subform: Option<Arc<Syntax>>,
    },
    constructor: |form, subform| {
        let form = form.try_into()?;
        let subform = if subform.is_true() { Some(subform.try_into()?) } else { None };
        Ok(SyntaxViolation { parent: Gc::new(Violation::new()), form, subform })
    },
    debug: |this, f| {
        write!(f, " form: {:?} subform: {:?}", this.form, this.subform)
    }
);

impl SyntaxViolation {
    pub fn new(form: Syntax, subform: Option<Syntax>) -> Self {
        Self {
            parent: Gc::new(Violation::new()),
            form: Arc::new(form),
            subform: subform.map(Arc::new),
        }
    }
}

define_condition_type!(
    rust_name: Undefined,
    scheme_name: "&undefined",
    parent: Violation
);

impl Undefined {
    pub fn new() -> Self {
        Self {
            parent: Gc::new(Violation::new()),
        }
    }
}

#[derive(Clone, Trace)]
pub struct CompoundCondition(Vec<Value>);

impl SchemeCompatible for CompoundCondition {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "compound-condition")
    }
}

impl fmt::Debug for CompoundCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for cond in self.0.iter() {
            write!(f, " ")?;
            cond.fmt(f)?;
        }
        Ok(())
    }
}

impl<T> From<T> for Condition
where
    CompoundCondition: From<T>,
{
    fn from(value: T) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from(value),
        )))
    }
}

impl<A, B> From<(A, B)> for CompoundCondition
where
    A: SchemeCompatible,
    B: SchemeCompatible,
{
    fn from(value: (A, B)) -> Self {
        Self(vec![
            Value::from(Record::from_rust_type(value.0)),
            Value::from(Record::from_rust_type(value.1)),
        ])
    }
}

impl<A, B, C> From<(A, B, C)> for CompoundCondition
where
    A: SchemeCompatible,
    B: SchemeCompatible,
    C: SchemeCompatible,
{
    fn from(value: (A, B, C)) -> Self {
        Self(vec![
            Value::from(Record::from_rust_type(value.0)),
            Value::from(Record::from_rust_type(value.1)),
            Value::from(Record::from_rust_type(value.2)),
        ])
    }
}

#[derive(Debug, Clone, Trace)]
pub struct Frame {
    pub proc: Symbol,
    pub call_site_span: Option<Arc<Span>>,
}

impl Frame {
    pub fn new(proc: Symbol, call_site_span: Option<Arc<Span>>) -> Self {
        Self {
            proc,
            call_site_span,
        }
    }
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref call_site) = self.call_site_span {
            write!(f, "{} at {call_site}", self.proc)
        } else {
            write!(f, "{} at (unknown)", self.proc)
        }
    }
}

#[cps_bridge(
    name = "with-exception-handler",
    lib = "(rnrs base builtins (6))",
    args = "handler thunk"
)]
pub fn with_exception_handler(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_stack: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
    let [handler, thunk] = args else {
        unreachable!();
    };

    let handler: Procedure = handler.clone().try_into()?;
    let thunk: Procedure = thunk.clone().try_into()?;

    dyn_stack.push(DynStackElem::ExceptionHandler(handler));

    let k_proc: Procedure = k.clone().try_into().unwrap();
    let (req_args, var) = k_proc.get_formals();

    let k = Procedure::new(
        runtime.clone(),
        vec![k.clone()],
        FuncPtr::Continuation(pop_dyn_stack),
        req_args,
        var,
        None,
    );

    Ok(Application::new(thunk.clone(), vec![Value::from(k)], None))
}

#[cps_bridge(name = "raise", lib = "(rnrs base builtins (6))", args = "obj")]
pub fn raise_builtin(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _dyn_stack: &mut DynStack,
    _k: Value,
) -> Result<Application, Condition> {
    Ok(raise(runtime.clone(), args[0].clone()))
}

/// Raises a non-continuable exception to the current exception handler.
pub fn raise(runtime: Runtime, raised: Value) -> Application {
    Application::new(
        Procedure::new(
            runtime,
            vec![raised],
            FuncPtr::Continuation(unwind_to_exception_handler),
            0,
            false,
            None,
        ),
        Vec::new(),
        None,
    )
}

#[runtime_fn]
unsafe extern "C" fn raise_rt(
    runtime: *mut GcInner<RuntimeInner>,
    raised: *const (),
) -> *mut Application {
    unsafe {
        let runtime = Runtime::from_raw_inc_rc(runtime);
        let raised = Value::from_raw(raised);
        Box::into_raw(Box::new(raise(runtime, raised)))
    }
}

unsafe extern "C" fn unwind_to_exception_handler(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    _args: *const Value,
    dyn_stack: *mut DynStack,
) -> *mut Application {
    unsafe {
        // env[0] is the raised value:
        let raised = env.as_ref().unwrap().clone();

        let dyn_stack = dyn_stack.as_mut().unwrap_unchecked();

        loop {
            let app = match dyn_stack.pop() {
                None => {
                    // If the stack is empty, we should return the error
                    Application::halt_err(raised)
                }
                Some(DynStackElem::Winder(winder)) => {
                    // If this is a winder, we should call the out winder while unwinding
                    Application::new(
                        winder.out_thunk,
                        vec![Value::from(Procedure::new(
                            Runtime::from_raw_inc_rc(runtime),
                            vec![raised],
                            FuncPtr::Continuation(unwind_to_exception_handler),
                            0,
                            false,
                            None,
                        ))],
                        None,
                    )
                }
                Some(DynStackElem::ExceptionHandler(handler)) => Application::new(
                    handler,
                    vec![
                        raised.clone(),
                        Value::from(Procedure::new(
                            Runtime::from_raw_inc_rc(runtime),
                            vec![raised],
                            FuncPtr::Continuation(reraise_exception),
                            0,
                            true,
                            None,
                        )),
                    ],
                    None,
                ),
                _ => continue,
            };
            return Box::into_raw(Box::new(app));
        }
    }
}

unsafe extern "C" fn reraise_exception(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    _args: *const Value,
    _dyn_stack: *mut DynStack,
) -> *mut Application {
    unsafe {
        let runtime = Runtime(Gc::from_raw_inc_rc(runtime));

        // env[0] is the exception
        let exception = env.as_ref().unwrap().clone();

        Box::into_raw(Box::new(Application::new(
            Procedure::new(
                runtime,
                Vec::new(),
                FuncPtr::Bridge(raise_builtin),
                1,
                false,
                None,
            ),
            vec![exception, Value::undefined()],
            None,
        )))
    }
}

/// Raises an exception to the current exception handler and coninues with the
/// value returned by the handler.
#[cps_bridge(
    name = "raise-continuable",
    lib = "(rnrs base builtins (6))",
    args = "obj"
)]
pub fn raise_continuable(
    _runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_stack: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
    let [condition] = args else {
        unreachable!();
    };

    let Some(handler) = dyn_stack.current_exception_handler() else {
        return Ok(Application::halt_err(condition.clone()));
    };

    Ok(Application::new(handler, vec![condition.clone(), k], None))
}
