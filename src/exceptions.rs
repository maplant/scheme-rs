//! Exceptional situations and conditions

use scheme_rs_macros::{cps_bridge, runtime_fn};

use crate::{
    ast::ParseAstError,
    gc::{Gc, GcInner, Trace},
    lists::{self, list_to_vec},
    proc::{Application, DynamicWind, FuncPtr, Procedure},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, into_scheme_compatible, rtd},
    registry::bridge,
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    syntax::{Identifier, Span, Syntax},
    value::{UnpackedValue, Value},
};
use std::{error::Error as StdError, fmt, ops::Range, ptr::null_mut, sync::Arc};

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

/// An exception handler includes the current handler - a function to call with
/// any condition that is raised - and the previous handler.
#[derive(Clone, Debug, Default, Trace)]
pub struct ExceptionHandler(pub(crate) Option<Gc<ExceptionHandlerInner>>);

#[derive(Clone, Debug, Trace)]
pub(crate) struct ExceptionHandlerInner {
    /// The previously installed handler. If the previously installed handler is
    /// None, we return the condition as an Error.
    prev_handler: ExceptionHandler,
    /// The currently installed handler.
    curr_handler: Procedure,
    /// The dynamic extent of the exception handler.
    dynamic_extent: DynamicWind,
}

impl ExceptionHandler {
    /// # Safety
    /// Exception handler must point to a valid Gc'd object.
    pub(crate) unsafe fn from_ptr(ptr: *mut GcInner<ExceptionHandlerInner>) -> Self {
        Self((!ptr.is_null()).then(|| unsafe { Gc::from_raw_inc_rc(ptr) }))
    }

    pub(crate) fn as_ptr(&self) -> *mut GcInner<ExceptionHandlerInner> {
        self.0.as_ref().map_or_else(null_mut, Gc::as_ptr)
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
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let [handler, thunk] = args else {
        unreachable!();
    };

    let handler: Procedure = handler.clone().try_into()?;
    let thunk: Procedure = thunk.clone().try_into()?;

    let exception_handler_inner = ExceptionHandlerInner {
        prev_handler: exception_handler.clone(),
        curr_handler: handler.clone(),
        dynamic_extent: dynamic_wind.clone(),
    };

    let exception_handler = ExceptionHandler(Some(Gc::new(exception_handler_inner)));

    let cont_proc: Procedure = cont.clone().try_into().unwrap();
    let (req_args, var) = {
        let cont_proc_read = cont_proc.0.read();
        (cont_proc_read.num_required_args, cont_proc_read.variadic)
    };

    let k = Procedure::new(
        runtime.clone(),
        vec![cont.clone()],
        FuncPtr::Continuation(reset_exception_handler),
        req_args,
        var,
        None,
    );

    Ok(Application::new(
        thunk.clone(),
        vec![Value::from(k)],
        exception_handler,
        dynamic_wind.clone(),
        None,
    ))
}

pub(crate) unsafe extern "C" fn reset_exception_handler(
    _runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        // env[0] is the continuation
        let k: Procedure = env.as_ref().unwrap().clone().try_into().unwrap();

        // Pop the exception handler
        let curr_handler = ExceptionHandler::from_ptr(exception_handler);
        let prev_handler = curr_handler
            .0
            .map(|handler| handler.read().prev_handler.clone())
            .unwrap_or_default();

        // Ugh:

        let mut collected_args: Vec<_> = (0..k.0.read().num_required_args)
            .map(|i| args.add(i).as_ref().unwrap().clone())
            .collect();

        if k.0.read().variadic {
            let rest_args = args
                .add(k.0.read().num_required_args)
                .as_ref()
                .unwrap()
                .clone();
            let mut vec = Vec::new();
            list_to_vec(&rest_args, &mut vec);
            collected_args.extend(vec);
        }

        let app = Application::new(
            k,
            collected_args,
            prev_handler,
            dynamic_wind.as_ref().unwrap().clone(),
            None,
        );

        Box::into_raw(Box::new(app))
    }
}

#[cps_bridge(name = "raise", lib = "(rnrs base builtins (6))", args = "obj")]
pub fn raise_builtin(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    Ok(raise(
        runtime.clone(),
        args[0].clone(),
        exception_handler.clone(),
        dynamic_wind,
    ))
}

/// Raises a non-continuable exception to the current exception handler.
pub fn raise(
    runtime: Runtime,
    raised: Value,
    exception_handler: ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Application {
    let (parent_wind, handler, parent_handler) =
        if let Some(exception_handler) = exception_handler.0 {
            let handler = exception_handler.read();
            (
                handler.dynamic_extent.clone(),
                Value::from(handler.curr_handler.clone()),
                handler.prev_handler.clone(),
            )
        } else {
            (
                DynamicWind::default(),
                Value::from(false),
                ExceptionHandler::default(),
            )
        };

    let thunks = exit_winders(dynamic_wind, &parent_wind);
    let calls = Procedure::new(
        runtime,
        vec![thunks, raised.clone(), handler],
        FuncPtr::Continuation(call_exits_and_exception_handler_reraise),
        0,
        false,
        None,
    );

    Application::new(calls, Vec::new(), parent_handler, parent_wind, None)
}

#[runtime_fn]
unsafe extern "C" fn raise_rt(
    runtime: *mut GcInner<RuntimeInner>,
    raised: i64,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        let runtime = Runtime::from_raw_inc_rc(runtime);
        let raised = Value::from_raw(raised as u64);
        let exception_handler = ExceptionHandler::from_ptr(exception_handler);
        let dynamic_wind = dynamic_wind.as_ref().unwrap();
        Box::into_raw(Box::new(raise(
            runtime,
            raised,
            exception_handler,
            dynamic_wind,
        )))
    }
}

fn exit_winders(from_extent: &DynamicWind, to_extent: &DynamicWind) -> Value {
    let mut from_winders = from_extent.winders.as_slice();
    let mut to_winders = to_extent.winders.as_slice();

    while let Some((to_first, to_remaining)) = to_winders.split_first() {
        let Some((from_first, from_remaining)) = from_winders.split_first() else {
            return Value::null();
        };

        if !Gc::ptr_eq(&from_first.1.0, &to_first.1.0) {
            break;
        }

        from_winders = from_remaining;
        to_winders = to_remaining;
    }

    let mut thunks = Value::null();
    for thunk in from_winders.iter() {
        thunks = Value::from((Value::from(thunk.1.clone()), thunks));
    }

    thunks
}

unsafe extern "C" fn call_exits_and_exception_handler_reraise(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
) -> *mut Application {
    unsafe {
        let runtime = Runtime::from_raw_inc_rc(runtime);

        // env[0] are the thunks:
        let thunks = env.as_ref().unwrap().clone();

        // env[1] is the raised value:
        let raised = env.add(1).as_ref().unwrap().clone();

        // env[2] is the next exception handler
        let curr_handler = env.add(2).as_ref().unwrap().clone();

        let app = match thunks.unpack() {
            UnpackedValue::Pair(pair) => {
                let lists::Pair(head_thunk, tail) = &*pair.read();
                let head_thunk: Procedure = head_thunk.clone().try_into().unwrap();
                let cont = Procedure::new(
                    runtime.clone(),
                    vec![tail.clone(), raised, curr_handler],
                    FuncPtr::Continuation(call_exits_and_exception_handler_reraise),
                    0,
                    false,
                    None,
                );
                Application::new(
                    head_thunk,
                    vec![Value::from(cont)],
                    ExceptionHandler::from_ptr(exception_handler),
                    dynamic_wind.as_ref().unwrap().clone(),
                    None,
                )
            }
            UnpackedValue::Null => {
                // If the exception handler is null, we want to return it as an
                // error.
                if !curr_handler.is_true() {
                    let app = Application::new(
                        Procedure::new(runtime, Vec::new(), FuncPtr::HaltError, 1, false, None),
                        vec![raised.clone()],
                        ExceptionHandler::from_ptr(exception_handler),
                        dynamic_wind.as_ref().unwrap().clone(),
                        None,
                    );
                    return Box::into_raw(Box::new(app));
                }

                let curr_handler: Procedure = curr_handler.try_into().unwrap();

                Application::new(
                    curr_handler,
                    vec![
                        raised.clone(),
                        Value::from(Procedure::new(
                            runtime,
                            vec![raised],
                            FuncPtr::Continuation(reraise_exception),
                            0,
                            true,
                            None,
                        )),
                    ],
                    ExceptionHandler::from_ptr(exception_handler),
                    dynamic_wind.as_ref().unwrap().clone(),
                    None,
                )
            }
            _ => unreachable!(),
        };

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn reraise_exception(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    _args: *const Value,
    exception_handler: *mut GcInner<ExceptionHandlerInner>,
    dynamic_wind: *const DynamicWind,
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
            ExceptionHandler::from_ptr(exception_handler),
            dynamic_wind.as_ref().unwrap().clone(),
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
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    cont: &Value,
    exception_handler: &ExceptionHandler,
    dynamic_wind: &DynamicWind,
) -> Result<Application, Condition> {
    let [condition] = args else {
        unreachable!();
    };

    let Some(handler) = &exception_handler.0 else {
        return Ok(Application::new(
            Procedure::new(
                runtime.clone(),
                Vec::new(),
                FuncPtr::HaltError,
                1,
                false,
                None,
            ),
            vec![condition.clone()],
            ExceptionHandler::default(),
            dynamic_wind.clone(),
            None,
        ));
    };

    let handler = handler.read().clone();

    Ok(Application::new(
        handler.curr_handler,
        vec![condition.clone(), cont.clone()],
        handler.prev_handler,
        dynamic_wind.clone(),
        None,
    ))
}
