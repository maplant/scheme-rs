//! Exceptional situations and conditions.

use crate::{
    gc::{Gc, GcInner, Trace},
    ports::{IoError, IoReadError, IoWriteError},
    proc::{Application, DynStack, DynStackElem, FuncPtr, Procedure, pop_dyn_stack},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::cps_bridge,
    runtime::{Runtime, RuntimeInner},
    syntax::{Identifier, Syntax, parse::ParseSyntaxError},
    value::{UnpackedValue, Value},
    vectors::Vector,
};
use parking_lot::RwLock;
pub use scheme_rs_macros::define_condition_type;
use scheme_rs_macros::runtime_fn;
use std::{convert::Infallible, fmt, ops::Range, sync::Arc};

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Value as fmt::Debug>::fmt(&self.0, f)
    }
}

/// A signal of some sort of erroneous condition.
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

    pub fn io_error(message: impl fmt::Display) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((IoError::new(), Assertion::new(), Message::new(message))),
        )))
    }

    pub fn io_read_error(message: impl fmt::Display) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((IoReadError::new(), Assertion::new(), Message::new(message))),
        )))
    }

    pub fn io_write_error(message: impl fmt::Display) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((IoWriteError::new(), Assertion::new(), Message::new(message))),
        )))
    }

    pub fn add_condition(self, condition: impl SchemeCompatible) -> Self {
        let mut conditions = if let Some(compound) = self.0.cast_to_rust_type::<CompoundCondition>()
        {
            compound.0.clone()
        } else {
            vec![self.0]
        };

        conditions.push(Value::from(Record::from_rust_type(condition)));

        Self(Value::from(Record::from_rust_type(CompoundCondition(
            conditions,
        ))))
    }
}

impl From<&'_ Value> for Option<Condition> {
    fn from(value: &'_ Value) -> Self {
        if let UnpackedValue::Record(record) = &*value.unpacked_ref()
            && let rtd = record.rtd()
            && (RecordTypeDescriptor::is_subtype_of(&rtd, &SimpleCondition::rtd())
                || RecordTypeDescriptor::is_subtype_of(&rtd, &CompoundCondition::rtd()))
        {
            Some(Condition(value.clone()))
        } else {
            None
        }
    }
}

impl From<std::io::Error> for Condition {
    fn from(value: std::io::Error) -> Self {
        Self::from((IoError::new(), Message::new(format!("{value:?}"))))
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

impl From<Message> for Condition {
    fn from(message: Message) -> Self {
        Self(Value::from(Record::from_rust_type(message)))
    }
}

impl From<Infallible> for Condition {
    fn from(infallible: Infallible) -> Self {
        match infallible {}
    }
}

impl From<ParseSyntaxError> for Condition {
    fn from(error: ParseSyntaxError) -> Self {
        Self::from((Lexical::new(), Message::new(error)))
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

impl Default for Warning {
    fn default() -> Self {
        Self::new()
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

impl Default for Serious {
    fn default() -> Self {
        Self::new()
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
    rust_name: StackTrace,
    scheme_name: "&trace",
    parent: SimpleCondition,
    fields: {
        trace: Vector,
    },
    constructor: |trace| {
        Ok(StackTrace {
            parent: Gc::new(SimpleCondition::new()),
            trace: trace.clone().try_into()?,
        })
    },
    debug: |this, f| {
        for trace in &*this.trace.0.vec.read() {
            write!(f, " {trace}")?;
        }
        Ok(())
    }
);

impl StackTrace {
    pub fn new(trace: Vec<Syntax>) -> Self {
        Self {
            parent: Gc::new(SimpleCondition::new()),
            trace: Vector::from(trace.into_iter().map(Value::from).collect::<Vec<_>>()),
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

impl Default for Error {
    fn default() -> Self {
        Self::new()
    }
}

define_condition_type!(
    rust_name: ImportError,
    scheme_name: "&import",
    parent: Error,
    fields: {
        library: String,
    },
    constructor: |lib| {
        Ok(ImportError {  parent: Gc::new(Error::new()), library: lib.to_string() })
    },
    debug: |this, f| {
        write!(f, " library: {}", this.library)
    }
);

impl ImportError {
    pub fn new(library: String) -> Self {
        Self {
            parent: Gc::new(Error::new()),
            library,
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

impl Default for Violation {
    fn default() -> Self {
        Self::new()
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

impl Default for Assertion {
    fn default() -> Self {
        Self::new()
    }
}

define_condition_type!(
    rust_name: Lexical,
    scheme_name: "&lexical",
    parent: Violation,
);

impl Lexical {
    pub fn new() -> Self {
        Self {
            parent: Gc::new(Violation::new()),
        }
    }
}

impl Default for Lexical {
    fn default() -> Self {
        Self::new()
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

impl Default for Undefined {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Trace)]
pub struct CompoundCondition(pub(crate) Vec<Value>);

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

#[cps_bridge(
    def = "with-exception-handler handler thunk",
    lib = "(rnrs base builtins (6))"
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
    );

    Ok(Application::new(thunk, vec![Value::from(k)]))
}

#[cps_bridge(def = "raise obj", lib = "(rnrs base builtins (6))")]
pub fn raise_builtin(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    dyn_stack: &mut DynStack,
    _k: Value,
) -> Result<Application, Condition> {
    Ok(raise(runtime.clone(), args[0].clone(), dyn_stack))
}

/// Raises a non-continuable exception to the current exception handler.
pub fn raise(runtime: Runtime, raised: Value, dyn_stack: &mut DynStack) -> Application {
    let raised = if let Some(condition) = raised.cast_to_scheme_type::<Condition>() {
        Value::from(condition.add_condition(StackTrace::new(dyn_stack.to_stack_trace())))
    } else {
        raised
    };

    Application::new(
        Procedure::new(
            runtime,
            vec![raised],
            FuncPtr::Continuation(unwind_to_exception_handler),
            0,
            false,
        ),
        Vec::new(),
    )
}

#[runtime_fn]
unsafe extern "C" fn raise_rt(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
    raised: *const (),
    dyn_stack: *mut DynStack,
) -> *mut Application {
    unsafe {
        let runtime = Runtime::from_raw_inc_rc(runtime);
        let raised = Value::from_raw(raised);
        Box::into_raw(Box::new(raise(
            runtime,
            raised,
            dyn_stack.as_mut().unwrap_unchecked(),
        )))
    }
}

unsafe extern "C" fn unwind_to_exception_handler(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
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
                        ))],
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
                        )),
                    ],
                ),
                _ => continue,
            };
            return Box::into_raw(Box::new(app));
        }
    }
}

unsafe extern "C" fn reraise_exception(
    runtime: *mut GcInner<RwLock<RuntimeInner>>,
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
            ),
            vec![exception, Value::undefined()],
        )))
    }
}

/// Raises an exception to the current exception handler and coninues with the
/// value returned by the handler.
#[cps_bridge(def = "raise-continuable obj", lib = "(rnrs base builtins (6))")]
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

    Ok(Application::new(handler, vec![condition.clone(), k]))
}
