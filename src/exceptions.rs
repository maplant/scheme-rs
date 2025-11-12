//! Exceptional situations and conditions

use scheme_rs_macros::{cps_bridge, runtime_fn};

use crate::{
    ast::ParseAstError,
    gc::{Gc, GcInner, Trace},
    proc::{Application, DynStack, DynStackElem, FuncPtr, Procedure, pop_dyn_stack},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, into_scheme_compatible, rtd},
    registry::bridge,
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    syntax::{Identifier, Span, Syntax},
    value::Value,
};
use std::{error::Error as StdError, fmt, ops::Range, sync::Arc};

#[derive(Debug, Clone, Trace)]
pub struct Exception {
    pub backtrace: Vec<Frame>,
    pub obj: Value,
}

impl Exception {
    pub fn new(backtrace: Vec<Frame>, obj: Value) -> Self {
        Self { backtrace, obj }
    }
}

// TODO: This shouldn't be the display impl for Exception, I don' t think.
impl fmt::Display for Exception {
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

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Value as fmt::Debug>::fmt(&self.0, f)
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
    pub fn error(message: String) -> Self {
        Self(Value::from(Record::from_rust_type(
            CompoundCondition::from((Assertion::new(), Message::new(message))),
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
                Message::new(format!("Could not represent '{value}' in {} type", r#type)),
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
impl_into_condition_for!(crate::num::NumberToUsizeError);
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

#[derive(Clone, Debug, Trace)]
pub struct Warning(Gc<SimpleCondition>);

impl SchemeCompatible for Warning {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "&warning", parent: SimpleCondition::rtd())
    }

    fn extract_embedded_record(
        &self,
        rtd: &Arc<RecordTypeDescriptor>,
    ) -> Option<Gc<dyn SchemeCompatible>> {
        SimpleCondition::rtd()
            .is_subtype_of(rtd)
            .then(|| into_scheme_compatible(self.0.clone()))
    }
}

#[derive(Clone, Debug, Trace)]
pub struct Serious(Gc<SimpleCondition>);

impl Serious {
    pub fn new() -> Self {
        Self(Gc::new(SimpleCondition))
    }
}

impl Default for Serious {
    fn default() -> Self {
        Self::new()
    }
}

impl SchemeCompatible for Serious {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "&serious", parent: SimpleCondition::rtd())
    }

    fn extract_embedded_record(
        &self,
        rtd: &Arc<RecordTypeDescriptor>,
    ) -> Option<Gc<dyn SchemeCompatible>> {
        SimpleCondition::rtd()
            .is_subtype_of(rtd)
            .then(|| into_scheme_compatible(self.0.clone()))
    }
}

#[derive(Clone, Trace)]
pub struct Message {
    parent: Gc<SimpleCondition>,
    message: String,
}

impl Message {
    pub fn new(message: String) -> Self {
        Self {
            parent: Gc::new(SimpleCondition::new()),
            message,
        }
    }
}

impl SchemeCompatible for Message {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            name: "&message",
            parent: SimpleCondition::rtd(),
            fields: ["msg"],
            constructor: |vals| {
                let [ msg ] = vals else {
                    unreachable!();
                };
                Ok(into_scheme_compatible(Gc::new(Message::new(msg.clone().try_into()?))))
            }
        )
    }

    fn extract_embedded_record(
        &self,
        rtd: &Arc<RecordTypeDescriptor>,
    ) -> Option<Gc<dyn SchemeCompatible>> {
        SimpleCondition::rtd()
            .is_subtype_of(rtd)
            .then(|| into_scheme_compatible(self.parent.clone()))
    }
}

impl fmt::Debug for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, " ")?;
        self.message.fmt(f)
    }
}

#[derive(Clone, Trace)]
pub struct Violation(Gc<Serious>);

impl Violation {
    pub fn new() -> Self {
        Self(Gc::new(Serious::new()))
    }
}

impl Default for Violation {
    fn default() -> Self {
        Self::new()
    }
}

impl SchemeCompatible for Violation {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "&violation", parent: Serious::rtd())
    }

    fn extract_embedded_record(
        &self,
        rtd: &Arc<RecordTypeDescriptor>,
    ) -> Option<Gc<dyn SchemeCompatible>> {
        Serious::rtd()
            .is_subtype_of(rtd)
            .then(|| into_scheme_compatible(self.0.clone()))
    }
}

#[derive(Clone, Trace)]
pub struct Assertion(Gc<Serious>);

impl Assertion {
    pub fn new() -> Self {
        Assertion(Gc::new(Serious::new()))
    }
}

impl Default for Assertion {
    fn default() -> Self {
        Self::new()
    }
}

impl SchemeCompatible for Assertion {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "&assertion", parent: Violation::rtd())
    }

    fn extract_embedded_record(
        &self,
        rtd: &Arc<RecordTypeDescriptor>,
    ) -> Option<Gc<dyn SchemeCompatible>> {
        Violation::rtd()
            .is_subtype_of(rtd)
            .then(|| into_scheme_compatible(self.0.clone()))
    }
}

#[derive(Clone, Trace)]
pub struct SyntaxViolation {
    parent: Gc<Violation>,
    form: Value,
    subform: Value,
}

impl SyntaxViolation {
    fn new(form: Syntax, subform: Option<Syntax>) -> Self {
        Self {
            parent: Gc::new(Violation::new()),
            form: Value::from(form),
            subform: subform
                .map(Value::from)
                .unwrap_or_else(|| Value::from(false)),
        }
    }
}

impl SchemeCompatible for SyntaxViolation {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(
            name: "&syntax",
            parent: Violation::rtd(),
            fields: [ "form", "subform" ],
            constructor: |vals| {
                let [ form, subform ] = vals else {
                    unreachable!();
                };
                Ok(into_scheme_compatible(Gc::new(SyntaxViolation {
                    parent: Gc::new(Violation::new()),
                    form: form.clone(),
                    subform: subform.clone(),
                })))
            }
        )
    }

    fn extract_embedded_record(
        &self,
        rtd: &Arc<RecordTypeDescriptor>,
    ) -> Option<Gc<dyn SchemeCompatible>> {
        Violation::rtd()
            .is_subtype_of(rtd)
            .then(|| into_scheme_compatible(self.parent.clone()))
    }
}

#[bridge(name = "&syntax-rtd", lib = "(rnrs conditions builtins (6))")]
pub fn syntax_rtd() -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(SyntaxViolation::rtd())])
}

impl fmt::Debug for SyntaxViolation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, " form: {} subform: {}", self.form, self.subform)
    }
}

#[derive(Clone, Trace)]
pub struct Undefined(Gc<Violation>);

impl Undefined {
    pub fn new() -> Self {
        Self(Gc::new(Violation::new()))
    }
}

impl Default for Undefined {
    fn default() -> Self {
        Self::new()
    }
}

impl SchemeCompatible for Undefined {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "&undefined", parent: Violation::rtd())
    }

    fn extract_embedded_record(
        &self,
        rtd: &Arc<RecordTypeDescriptor>,
    ) -> Option<Gc<dyn SchemeCompatible>> {
        Violation::rtd()
            .is_subtype_of(rtd)
            .then(|| into_scheme_compatible(self.0.clone()))
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

macro_rules! impl_empty_debug {
    ( $t:ty ) => {
        impl fmt::Debug for $t {
            fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
                Ok(())
            }
        }
    };
}

impl_empty_debug!(Assertion);
impl_empty_debug!(Violation);
impl_empty_debug!(Undefined);

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
    dyn_stack: &mut DynStack,
    _k: Value,
) -> Result<Application, Condition> {
    Ok(raise(runtime.clone(), args[0].clone(), dyn_stack))
}

/// Raises a non-continuable exception to the current exception handler.
pub fn raise(runtime: Runtime, raised: Value, dyn_stack: &mut DynStack) -> Application {
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
    raised: i64,
    dyn_stack: *mut DynStack,
) -> *mut Application {
    unsafe {
        let runtime = Runtime::from_raw_inc_rc(runtime);
        let raised = Value::from_raw(raised as u64);
        let dyn_stack = dyn_stack.as_mut().unwrap_unchecked();
        Box::into_raw(Box::new(raise(runtime, raised, dyn_stack)))
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
