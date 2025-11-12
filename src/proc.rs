//! Procedures, continuation and user, and applying values to those procedures.
//! Contains the main evaluation trampoline.

use crate::{
    env::Local,
    exceptions::{Condition, Exception, ExceptionHandler, Frame, raise},
    gc::{Gc, GcInner, Trace},
    lists::{self, Pair, list_to_vec},
    records::{Record, RecordTypeDescriptor, SchemeCompatible, rtd},
    registry::BridgeFnDebugInfo,
    runtime::{Runtime, RuntimeInner},
    symbols::Symbol,
    syntax::Span,
    value::{Cell, UnpackedValue, UnpackedValueRef, Value},
};
use scheme_rs_macros::{cps_bridge, maybe_async, maybe_await};
use std::{collections::HashMap, fmt, hash::Hash, mem::MaybeUninit, sync::Arc};

/// A function pointer to a generated continuation.
pub(crate) type ContinuationPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    params: *mut Parameters,
) -> *mut Application;

/// A function pointer to a generated user function.
pub(crate) type UserPtr = unsafe extern "C" fn(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    params: *mut Parameters,
    k: Value,
) -> *mut Application;

/// A function pointer to a sync Rust bridge function.
pub type SyncBridgePtr = for<'a> fn(
    runtime: &'a Runtime,
    env: &'a [Value],
    // TODO: Make this a Vec
    args: &'a [Value],
    rest_args: &'a [Value],
    params: &mut Parameters,
    k: Value,
) -> Application;

/// A function pointer to an async Rust bridge function.
#[cfg(feature = "async")]
pub type AsyncBridgePtr = for<'a> fn(
    runtime: &'a Runtime,
    env: &'a [Value],
    args: &'a [Value],
    rest_args: &'a [Value],
    params: &'a mut Parameters,
    k: Value,
) -> futures::future::BoxFuture<'a, Application>;

#[derive(Copy, Clone, Debug)]
pub(crate) enum FuncPtr {
    Continuation(ContinuationPtr),
    User(UserPtr),
    Bridge(SyncBridgePtr),
    #[cfg(feature = "async")]
    AsyncBridge(AsyncBridgePtr),
}

enum JitFuncPtr {
    Continuation(ContinuationPtr),
    User(UserPtr),
}

#[derive(Clone, Trace)]
#[repr(align(16))]
pub(crate) struct ProcedureInner {
    /// The runtime the Procedure is defined in. This is necessary to ensure that
    /// dropping the runtime does not de-allocate the function pointer for this
    /// procedure.
    // TODO: Do we make this optional in the case of bridge functions?
    pub(crate) runtime: Runtime,
    /// Environmental variables used by the procedure.
    pub(crate) env: Vec<Value>,
    /// Fuction pointer to the body of the procecure.
    #[trace(skip)]
    pub(crate) func: FuncPtr,
    /// Number of required arguments to this procedure.
    pub(crate) num_required_args: usize,
    /// Whether or not this is a variadic function.
    pub(crate) variadic: bool,
    /// Whether or not this function is a variable transformer.
    pub(crate) is_variable_transformer: bool,
    /// Debug information for this function. Only applicable if the function is
    /// a user function, i.e. not a continuation.
    pub(crate) debug_info: Option<Arc<FuncDebugInfo>>,
}

impl ProcedureInner {
    pub(crate) fn new(
        runtime: Runtime,
        env: Vec<Value>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<FuncDebugInfo>>,
    ) -> Self {
        Self {
            runtime,
            env,
            func,
            num_required_args,
            variadic,
            is_variable_transformer: false,
            debug_info,
        }
    }

    pub fn is_continuation(&self) -> bool {
        matches!(self.func, FuncPtr::Continuation(_))
    }

    pub fn is_user_func(&self) -> bool {
        !self.is_continuation()
    }

    pub(crate) fn prepare_args(
        &self,
        mut args: Vec<Value>,
        params: &mut Parameters,
    ) -> Result<(Vec<Value>, Option<Value>), Application> {
        // Extract the continuation, if it is required
        let cont = (!matches!(self.func, FuncPtr::Continuation(_))).then(|| args.pop().unwrap());

        // Error if the number of arguments provided is incorrect
        if args.len() < self.num_required_args {
            return Err(raise(
                self.runtime.clone(),
                Condition::wrong_num_of_args(self.num_required_args, args.len()).into(),
                params,
            ));
        }

        if !self.variadic && args.len() > self.num_required_args {
            return Err(raise(
                self.runtime.clone(),
                Condition::wrong_num_of_args(self.num_required_args, args.len()).into(),
                params,
            ));
        }

        Ok((args, cont))
    }

    #[cfg(feature = "async")]
    async fn apply_async_bridge(
        &self,
        func: AsyncBridgePtr,
        args: &[Value],
        params: &mut Parameters,
        k: Value,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(&self.runtime, &self.env, args, rest_args, params, k).await
    }

    fn apply_sync_bridge(
        &self,
        func: SyncBridgePtr,
        args: &[Value],
        params: &mut Parameters,
        k: Value,
    ) -> Application {
        let (args, rest_args) = if self.variadic {
            args.split_at(self.num_required_args)
        } else {
            (args, &[] as &[Value])
        };

        (func)(&self.runtime, &self.env, args, rest_args, params, k)
    }

    fn apply_jit(
        &self,
        func: JitFuncPtr,
        mut args: Vec<Value>,
        params: &mut Parameters,
        k: Option<Value>,
    ) -> Application {
        if self.variadic {
            let mut rest_args = Value::null();
            let extra_args = args.len() - self.num_required_args;
            for _ in 0..extra_args {
                rest_args = Value::from(Gc::new(Pair::new(args.pop().unwrap(), rest_args)));
            }
            args.push(rest_args);
        }

        let app = match func {
            JitFuncPtr::Continuation(sync_fn) => unsafe {
                (sync_fn)(
                    Gc::as_ptr(&self.runtime.0),
                    self.env.as_ptr(),
                    args.as_ptr(),
                    params as *mut Parameters,
                )
            },
            JitFuncPtr::User(sync_fn) => unsafe {
                (sync_fn)(
                    Gc::as_ptr(&self.runtime.0),
                    self.env.as_ptr(),
                    args.as_ptr(),
                    params as *mut Parameters,
                    Value::from_raw(Value::as_raw(k.as_ref().unwrap())),
                )
            },
        };

        unsafe { *Box::from_raw(app) }
    }

    #[maybe_async]
    pub fn apply(&self, args: Vec<Value>, params: &mut Parameters) -> Application {
        let (args, k) = match self.prepare_args(args, params) {
            Ok(args) => args,
            Err(raised) => return raised,
        };

        match self.func {
            FuncPtr::Continuation(k) => {
                self.apply_jit(JitFuncPtr::Continuation(k), args, params, None)
            }
            FuncPtr::User(user) => self.apply_jit(JitFuncPtr::User(user), args, params, k),
            FuncPtr::Bridge(sbridge) => self.apply_sync_bridge(sbridge, &args, params, k.unwrap()),
            #[cfg(feature = "async")]
            FuncPtr::AsyncBridge(abridge) => {
                self.apply_async_bridge(abridge, &args, params, k.unwrap())
                    .await
            }
        }
    }
}

/// The runtime representation of a Procedure, which can be either a user
/// function or a continuation. Contains a reference to all of the environmental
/// variables used in the body, along with a function pointer to the body of the
/// procedure.
#[derive(Clone, Trace)]
pub struct Procedure(pub(crate) Gc<ProcedureInner>);

impl Procedure {
    pub(crate) fn new(
        runtime: Runtime,
        env: Vec<Value>,
        func: FuncPtr,
        num_required_args: usize,
        variadic: bool,
        debug_info: Option<Arc<FuncDebugInfo>>,
    ) -> Self {
        Self(Gc::new(ProcedureInner {
            runtime,
            env,
            func,
            num_required_args,
            variadic,
            is_variable_transformer: false,
            debug_info,
        }))
    }

    pub fn get_runtime(&self) -> Runtime {
        self.0.read().runtime.clone()
    }

    pub fn get_formals(&self) -> (usize, bool) {
        let this = self.0.read();
        (this.num_required_args, this.variadic)
    }

    /// # Safety
    /// `args` must be a valid pointer and contain num_required_args + variadic entries.
    pub unsafe fn collect_args(&self, args: *const Value) -> Vec<Value> {
        // I don't really like this, but what are you gonna do?
        let (num_required_args, variadic) = self.get_formals();

        unsafe {
            let mut collected_args: Vec<_> = (0..num_required_args)
                .map(|i| args.add(i).as_ref().unwrap().clone())
                .collect();

            if variadic {
                let rest_args = args.add(num_required_args).as_ref().unwrap().clone();
                let mut vec = Vec::new();
                lists::list_to_vec(&rest_args, &mut vec);
                collected_args.extend(vec);
            }

            collected_args
        }
    }

    pub fn is_variable_transformer(&self) -> bool {
        self.0.read().is_variable_transformer
    }

    #[maybe_async]
    pub fn call(&self, args: &[Value]) -> Result<Vec<Value>, Exception> {
        unsafe extern "C" fn halt(
            _runtime: *mut GcInner<RuntimeInner>,
            _env: *const Value,
            args: *const Value,
            _params: *mut Parameters,
        ) -> *mut Application {
            unsafe { crate::runtime::halt(Value::into_raw(args.read()) as i64) }
        }

        let mut args = args.to_vec();

        // TODO: We don't need to create a new one of these every time, we should just have
        // one
        args.push(Value::from(Self(Gc::new(ProcedureInner::new(
            self.0.read().runtime.clone(),
            Vec::new(),
            FuncPtr::Continuation(halt),
            0,
            true,
            None,
        )))));

        maybe_await!(Application::new(self.clone(), args, None,).eval(&mut Parameters::default(),))
    }
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.read().fmt(f)
    }
}

impl fmt::Debug for ProcedureInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_continuation() {
            return write!(f, "continuation");
        }

        let Some(ref debug_info) = self.debug_info else {
            write!(f, "(unknown-function")?;
            for i in 0..self.num_required_args {
                write!(f, " ${i}")?;
            }
            if self.variadic {
                write!(f, " . ${}", self.num_required_args)?;
            }
            return write!(f, ")");
        };

        write!(f, "({}", debug_info.name)?;

        if let Some((last, args)) = debug_info.args.split_last() {
            for arg in args {
                write!(f, " {arg}")?;
            }
            if self.variadic {
                write!(f, " .")?;
            }
            write!(f, " {last}")?;
        }

        write!(f, ") at {}", debug_info.location)
    }
}

pub enum OpType {
    Proc(Procedure),
    HaltOk,
    HaltErr,
}

/// An application of a function to a given set of values.
pub struct Application {
    /// The operator being applied to.
    pub op: OpType,
    /// The arguments being applied to the operator.
    pub args: Vec<Value>,
    /// The call site of this application, if it exists.
    pub call_site: Option<Arc<Span>>,
}

impl Application {
    pub fn new(op: Procedure, args: Vec<Value>, call_site: Option<Arc<Span>>) -> Self {
        Self {
            // We really gotta figure out how to deal with this better
            op: OpType::Proc(op),
            args,
            call_site,
        }
    }

    pub fn halt_ok(args: Vec<Value>) -> Self {
        Self {
            op: OpType::HaltOk,
            args,
            call_site: None,
        }
    }

    pub fn halt_err(arg: Value) -> Self {
        Self {
            op: OpType::HaltErr,
            args: vec![arg],
            call_site: None,
        }
    }

    /// Evaluate the application - and all subsequent application - until all that
    /// remains are values. This is the main trampoline of the evaluation engine.
    #[maybe_async]
    pub fn eval(mut self, params: &mut Parameters) -> Result<Vec<Value>, Exception> {
        let mut stack_trace = StackTraceCollector::new();

        loop {
            let op = match self.op {
                OpType::Proc(proc) => proc,
                OpType::HaltOk => return Ok(self.args),
                OpType::HaltErr => {
                    return Err(Exception::new(
                        stack_trace.into_frames(),
                        self.args.pop().unwrap(),
                    ));
                }
            };
            let op = { op.0.read().as_ref().clone() };
            stack_trace.collect_application(op.debug_info.clone(), self.call_site);
            self = maybe_await!(op.apply(self.args, params));
        }
    }
}

/// Parameters passed through the dynamic extent of the functions
#[derive(Clone, Debug, Default, Trace)]
pub struct Parameters {
    pub(crate) exception_handler: ExceptionHandler,
    pub(crate) dynamic_wind: DynamicWind,
    // pub(crate) prompts: Prompts,
}

impl SchemeCompatible for Parameters {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "$parameters", opaque: true, sealed: true)
    }
}

#[derive(Default)]
struct StackTraceCollector {
    stack_trace: Vec<StackTrace>,
}

impl StackTraceCollector {
    fn new() -> Self {
        Self::default()
    }

    fn into_frames(self) -> Vec<Frame> {
        self.stack_trace
            .into_iter()
            .map(|trace| Frame::new(trace.debug_info.name, trace.call_site))
            .collect()
    }

    fn collect_application(
        &mut self,
        debug_info: Option<Arc<FuncDebugInfo>>,
        call_site: Option<Arc<Span>>,
    ) {
        if let Some(debug_info) = debug_info {
            // This is a user func, and therefore we should push to the
            // current stack trace.
            self.stack_trace.push(StackTrace {
                debug_info,
                call_site,
            });
        } else {
            // If this is not user func, we are returning from one via a
            // continuation and should pop the stack frame:
            self.stack_trace.pop();
        }
    }
}

#[derive(Debug)]
pub struct FuncDebugInfo {
    /// The name of the function.
    name: Symbol,
    /// Named arguments for the function.
    args: Vec<Local>,
    /// Location of the function definition
    location: Span,
}

#[derive(Debug)]
struct StackTrace {
    debug_info: Arc<FuncDebugInfo>,
    call_site: Option<Arc<Span>>,
}

impl FuncDebugInfo {
    pub fn new(name: Option<Symbol>, args: Vec<Local>, location: Span) -> Self {
        Self {
            name: name.unwrap_or_else(|| Symbol::intern("<lambda>")),
            args,
            location,
        }
    }

    pub fn from_bridge_fn(name: &'static str, debug_info: BridgeFnDebugInfo) -> Self {
        Self {
            name: Symbol::intern(name),
            args: debug_info
                .args
                .iter()
                .map(|arg| Local::gensym_with_name(Symbol::intern(arg)))
                .collect(),
            location: Span {
                line: debug_info.line,
                column: debug_info.column as usize,
                offset: debug_info.offset,
                file: std::sync::Arc::new(debug_info.file.to_string()),
            },
        }
    }
}

#[cps_bridge(name = "apply", lib = "(rnrs base builtins (6))", args = "arg1 . args")]
pub fn apply(
    _runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    _params: &mut Parameters,
    k: Value,
) -> Result<Application, Condition> {
    if rest_args.is_empty() {
        return Err(Condition::wrong_num_of_args(2, args.len()));
    }
    let op: Procedure = args[0].clone().try_into()?;
    let (last, args) = rest_args.split_last().unwrap();
    let mut args = args.to_vec();
    list_to_vec(last, &mut args);
    args.push(k);
    Ok(Application::new(op.clone(), args, None))
}

////////////////////////////////////////////////////////////////////////////////
//
// Call with current continuation
//

#[cps_bridge(
    name = "call-with-current-continuation",
    lib = "(rnrs base builtins (6))",
    args = "proc"
)]
pub fn call_with_current_continuation(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    params: &mut Parameters,
    k: Value,
) -> Result<Application, Condition> {
    let [proc] = args else { unreachable!() };
    let proc: Procedure = proc.clone().try_into()?;

    let (req_args, variadic) = {
        let k: Procedure = k.clone().try_into()?;
        let k_read = k.0.read();
        (k_read.num_required_args, k_read.variadic)
    };

    let params = Value::from(Record::from_rust_type(params.clone()));

    let escape_procedure = Procedure::new(
        runtime.clone(),
        vec![k.clone(), params],
        FuncPtr::Bridge(escape_procedure),
        req_args,
        variadic,
        None,
    );

    let app = Application::new(proc, vec![Value::from(escape_procedure), k], None);

    Ok(app)
}

/// Prepare the continuation for call/cc. Clones the continuation environment
/// and creates a closure that calls the appropriate winders.
#[cps_bridge]
fn escape_procedure(
    runtime: &Runtime,
    env: &[Value],
    args: &[Value],
    rest_args: &[Value],
    params: &mut Parameters,
    _k: Value,
) -> Result<Application, Condition> {
    // env[0] is the continuation
    let k = env[0].clone();
    // env[1] are the parameters of the continuation
    let saved_params = env[1].clone();
    let saved_params = saved_params.try_into_rust_type::<Parameters>()?;

    let thunks = params
        .dynamic_wind
        .thunks_to_extent(&saved_params.read().dynamic_wind);

    // Clone the continuation
    let k_ref = k.unpacked_ref();
    let k = maybe_clone_continuation(
        &k_ref,
        &mut StackClone::default(),
        &mut StackClone::default(),
    )
    .unwrap_or_else(|| k.clone());
    let k: Procedure = k.try_into().unwrap();

    let args = args.iter().chain(rest_args).cloned().collect::<Vec<_>>();

    let k = if thunks.is_null() {
        *params = saved_params.read().clone();
        k
    } else {
        let (req_args, variadic) = {
            let k_read = k.0.read();
            (k_read.num_required_args, k_read.variadic)
        };

        Procedure::new(
            runtime.clone(),
            vec![thunks, env[1].clone(), Value::from(k)],
            FuncPtr::Continuation(call_thunks),
            req_args,
            variadic,
            None,
        )
    };

    let app = Application::new(k, args, None);

    Ok(app)
}

fn prepare_env_variable(
    value: &Value,
    cloned_k: &mut StackClone<ContinuationPtr>,
    cloned_cells: &mut StackClone<*mut GcInner<Value>>,
) -> Value {
    let value_ref = value.unpacked_ref();
    maybe_clone_continuation(&value_ref, cloned_k, cloned_cells)
        .or_else(|| maybe_clone_cell(&value_ref, cloned_cells))
        .unwrap_or_else(|| value.clone())
}

fn maybe_clone_continuation(
    value: &UnpackedValueRef<'_>,
    cloned_k: &mut StackClone<ContinuationPtr>,
    cloned_cells: &mut StackClone<*mut GcInner<Value>>,
) -> Option<Value> {
    let UnpackedValue::Procedure(k) = value.as_ref() else {
        return None;
    };

    let func_ptr = {
        let k_read = k.0.read();
        match k_read.func {
            FuncPtr::Continuation(cont) if !k_read.env.is_empty() => cont,
            _ => return None,
        }
    };

    if let Some(k) = cloned_k.get(&func_ptr) {
        return Some(k.clone());
    }

    let new_k = {
        let k = k.0.read();
        let new_env = k
            .env
            .iter()
            .map(|var| prepare_env_variable(var, cloned_k, cloned_cells))
            .collect::<Vec<_>>();
        Procedure::new(
            k.runtime.clone(),
            new_env,
            k.func,
            k.num_required_args,
            k.variadic,
            k.debug_info.clone(),
        )
    };

    let new_k = Value::from(new_k);
    cloned_k.insert(func_ptr, new_k.clone());

    Some(new_k)
}

fn maybe_clone_cell(
    value: &UnpackedValueRef<'_>,
    cloned: &mut StackClone<*mut GcInner<Value>>,
) -> Option<Value> {
    let UnpackedValue::Cell(cell) = value.as_ref() else {
        return None;
    };

    let cell_ptr = Gc::as_ptr(&cell.0);

    if let Some(cloned) = cloned.get(&cell_ptr) {
        return Some(cloned);
    }

    let new_cell = Value::from(Cell(Gc::new(cell.0.read().clone())));
    cloned.insert(cell_ptr, new_cell.clone());
    Some(new_cell)
}

const MAX_ON_STACK: usize = 10;

enum StackClone<K> {
    Stack {
        len: usize,
        stack: [MaybeUninit<(K, Value)>; MAX_ON_STACK],
    },
    Heap(HashMap<K, Value>),
}

impl<K> Default for StackClone<K> {
    fn default() -> Self {
        StackClone::Stack {
            len: 0,
            stack: [const { MaybeUninit::uninit() }; MAX_ON_STACK],
        }
    }
}

impl<K> StackClone<K>
where
    K: PartialEq + Eq + Hash,
{
    fn get(&self, key: &K) -> Option<Value> {
        match self {
            Self::Stack { len, stack } => {
                for elem in stack.iter().take(*len) {
                    let elem = unsafe { elem.assume_init_ref() };
                    if &elem.0 == key {
                        return Some(elem.1.clone());
                    }
                }
                None
            }
            Self::Heap(heap) => heap.get(key).cloned(),
        }
    }

    fn insert(&mut self, key: K, val: Value) {
        match self {
            Self::Stack { len, stack } if *len == MAX_ON_STACK => {
                let mut heap =
                    std::mem::replace(stack, [const { MaybeUninit::uninit() }; MAX_ON_STACK])
                        .into_iter()
                        .map(|x| unsafe { x.assume_init() })
                        .collect::<HashMap<_, _>>();
                heap.insert(key, val);
                *self = Self::Heap(heap);
            }
            Self::Stack { len, stack } => {
                stack[*len] = MaybeUninit::new((key, val));
                *len += 1;
            }
            Self::Heap(heap) => {
                heap.insert(key, val);
            }
        }
    }
}

pub(crate) unsafe extern "C" fn call_thunks(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    _params: *mut Parameters,
) -> *mut Application {
    unsafe {
        // env[0] are the thunks:
        let thunks = env.as_ref().unwrap().clone();

        // env[1] are the saved params:
        let params = env.add(1).as_ref().unwrap().clone();

        // env[2] is the continuation:
        let k: Procedure = env.add(2).as_ref().unwrap().clone().try_into().unwrap();

        // k determines the number of arguments:
        let collected_args = {
            let k_read = k.0.read();
            let mut num_args = k_read.num_required_args;

            if k_read.is_user_func() {
                num_args += 1;
            }

            let mut collected_args = if k_read.variadic {
                args.add(num_args).as_ref().unwrap().clone()
            } else {
                Value::null()
            };

            for i in (0..num_args).rev() {
                let arg = args.add(i).as_ref().unwrap().clone();
                collected_args = Value::from((arg, collected_args));
            }

            collected_args
        };

        let thunks = Procedure::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![thunks, collected_args, params, Value::from(k)],
            FuncPtr::Continuation(call_thunks_pass_args),
            0,
            false,
            None,
        );

        let app = Application::new(thunks, Vec::new(), None);

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn call_thunks_pass_args(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    _args: *const Value,
    params: *mut Parameters,
) -> *mut Application {
    unsafe {
        // env[0] are the thunks:
        let thunks = env.as_ref().unwrap().clone();

        // env[1] are the collected arguments
        let args = env.add(1).as_ref().unwrap().clone();

        // env[2] are the saved params:
        let saved_params = env.add(2).as_ref().unwrap().clone();

        // env[3] is k1, the current continuation
        let k = env.add(3).as_ref().unwrap().clone();

        let app = match &*thunks.unpacked_ref() {
            UnpackedValue::Pair(pair) => {
                let lists::Pair(head, tail) = &*pair.read();
                let into_thunk: Gc<lists::Pair> = head.clone().try_into().unwrap();
                let lists::Pair(head_thunk, head_params) = &*into_thunk.read();
                let head_thunk: Procedure = head_thunk.clone().try_into().unwrap();
                *params.as_mut().unwrap() = head_params
                    .try_into_rust_type::<Parameters>()
                    .unwrap()
                    .read()
                    .clone();
                let k = Procedure::new(
                    Runtime::from_raw_inc_rc(runtime),
                    vec![tail.clone(), args, saved_params, k],
                    FuncPtr::Continuation(call_thunks_pass_args),
                    0,
                    false,
                    None,
                );
                Application::new(head_thunk.clone(), vec![Value::from(k)], None)
            }
            UnpackedValue::Null => {
                let mut collected_args = Vec::new();
                list_to_vec(&args, &mut collected_args);
                // Restore the saved params:
                *params.as_mut().unwrap() = saved_params
                    .try_into_rust_type::<Parameters>()
                    .unwrap()
                    .read()
                    .clone();
                Application::new(k.try_into().unwrap(), collected_args, None)
            }
            _ => unreachable!(),
        };

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn call_consumer_with_values(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    params: *mut Parameters,
) -> *mut Application {
    unsafe {
        // env[0] is the consumer
        let consumer = env.as_ref().unwrap().clone();
        let type_name = consumer.type_name();
        let params = params.as_mut().unwrap_unchecked();

        let consumer: Procedure = match consumer.try_into() {
            Ok(consumer) => consumer,
            _ => {
                let raised = raise(
                    Runtime::from_raw_inc_rc(runtime),
                    Condition::invalid_operator(type_name).into(),
                    params,
                );
                return Box::into_raw(Box::new(raised));
            }
        };

        // env[1] is the continuation
        let k = env.add(1).as_ref().unwrap().clone();

        let mut collected_args: Vec<_> = (0..consumer.0.read().num_required_args)
            .map(|i| args.add(i).as_ref().unwrap().clone())
            .collect();

        // I hate this constant going back and forth from variadic to list. I have
        // to figure out a way to make it consistent
        if consumer.0.read().variadic {
            let rest_args = args
                .add(consumer.0.read().num_required_args)
                .as_ref()
                .unwrap()
                .clone();
            let mut vec = Vec::new();
            list_to_vec(&rest_args, &mut vec);
            collected_args.extend(vec);
        }

        collected_args.push(k);

        Box::into_raw(Box::new(Application::new(
            consumer.clone(),
            collected_args,
            None,
        )))
    }
}

#[cps_bridge(
    name = "call-with-values",
    lib = "(rnrs base builtins (6))",
    args = "producer consumer"
)]
pub fn call_with_values(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _params: &mut Parameters,
    k: Value,
) -> Result<Application, Condition> {
    let [producer, consumer] = args else {
        return Err(Condition::wrong_num_of_args(2, args.len()));
    };

    let producer: Procedure = producer.clone().try_into()?;
    let consumer: Procedure = consumer.clone().try_into()?;

    // Get the details of the consumer:
    let (num_required_args, variadic) = {
        let consumer_read = consumer.0.read();
        (consumer_read.num_required_args, consumer_read.variadic)
    };

    let call_consumer_closure = Procedure::new(
        runtime.clone(),
        vec![Value::from(consumer), k],
        FuncPtr::Continuation(call_consumer_with_values),
        num_required_args,
        variadic,
        None,
    );

    Ok(Application::new(
        producer,
        vec![Value::from(call_consumer_closure)],
        None,
    ))
}

////////////////////////////////////////////////////////////////////////////////
//
// Dynamic wind
//

#[derive(Clone, Debug, Default, Trace)]
pub struct DynamicWind {
    pub(crate) winders: Vec<Winder>,
}

impl DynamicWind {
    pub(crate) fn thunks_to_extent(&self, to_extent: &Self) -> Value {
        let mut from_winders = self.winders.as_slice();
        let mut to_winders = to_extent.winders.as_slice();

        while let Some((to_first, to_remaining)) = to_winders.split_first() {
            let Some((from_first, from_remaining)) = from_winders.split_first() else {
                break;
            };

            if !Gc::ptr_eq(&from_first.in_thunk.0, &to_first.in_thunk.0) {
                break;
            }

            from_winders = from_remaining;
            to_winders = to_remaining;
        }

        let mut thunks = Value::null();
        for thunk in
            from_winders
                .iter()
                .map(|wind| Value::from((Value::from(wind.out_thunk.clone()), wind.params.clone())))
                .rev()
                .chain(to_winders.iter().map(|wind| {
                    Value::from((Value::from(wind.in_thunk.clone()), wind.params.clone()))
                }))
                .rev()
        {
            thunks = Value::from((thunk, thunks));
        }

        thunks
    }

    pub(crate) fn exit_winders(&self, to_extent: &Self) -> Value {
        let mut from_winders = self.winders.as_slice();
        let mut to_winders = to_extent.winders.as_slice();

        while let Some((to_first, to_remaining)) = to_winders.split_first() {
            let Some((from_first, from_remaining)) = from_winders.split_first() else {
                return Value::null();
            };

            if !Gc::ptr_eq(&from_first.out_thunk.0, &to_first.out_thunk.0) {
                break;
            }

            from_winders = from_remaining;
            to_winders = to_remaining;
        }

        let mut thunks = Value::null();
        for thunk in from_winders.iter() {
            thunks = Value::from((
                Value::from((Value::from(thunk.out_thunk.clone()), thunk.params.clone())),
                thunks,
            ));
        }

        thunks
    }
}

#[derive(Clone, Debug, Trace)]
pub struct Winder {
    pub(crate) in_thunk: Procedure,
    pub(crate) out_thunk: Procedure,
    pub(crate) params: Value,
}

#[cps_bridge(
    name = "dynamic-wind",
    lib = "(rnrs base builtins (6))",
    args = "in body out"
)]
pub fn dynamic_wind(
    runtime: &Runtime,
    _env: &[Value],
    args: &[Value],
    _rest_args: &[Value],
    _params: &mut Parameters,
    k: Value,
) -> Result<Application, Condition> {
    let [in_thunk_val, body_thunk_val, out_thunk_val] = args else {
        return Err(Condition::wrong_num_of_args(3, args.len()));
    };

    let in_thunk: Procedure = in_thunk_val.clone().try_into()?;
    let _: Procedure = body_thunk_val.clone().try_into()?;

    let call_body_thunk_cont = Procedure::new(
        runtime.clone(),
        vec![
            in_thunk_val.clone(),
            body_thunk_val.clone(),
            out_thunk_val.clone(),
            k,
        ],
        FuncPtr::Continuation(call_body_thunk),
        0,
        true,
        None,
    );

    Ok(Application::new(
        in_thunk,
        vec![Value::from(call_body_thunk_cont)],
        None,
    ))
}

pub(crate) unsafe extern "C" fn call_body_thunk(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    _args: *const Value,
    params: *mut Parameters,
) -> *mut Application {
    unsafe {
        // env[0] is the in thunk
        let in_thunk = env.as_ref().unwrap().clone();

        // env[1] is the body thunk
        let body_thunk: Procedure = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        // env[2] is the out thunk
        let out_thunk = env.add(2).as_ref().unwrap().clone();

        // env[3] is k, the continuation
        let k = env.add(3).as_ref().unwrap().clone();

        let params = params.as_mut().unwrap_unchecked();

        params.dynamic_wind.winders.push(Winder {
            in_thunk: in_thunk.clone().try_into().unwrap(),
            out_thunk: out_thunk.clone().try_into().unwrap(),
            params: Value::from(Record::from_rust_type(params.clone())),
        });

        let k = Procedure::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![out_thunk, k],
            FuncPtr::Continuation(call_out_thunks),
            0,
            true,
            None,
        );

        let app = Application::new(body_thunk, vec![Value::from(k)], None);

        Box::into_raw(Box::new(app))
    }
}

pub(crate) unsafe extern "C" fn call_out_thunks(
    runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    args: *const Value,
    params: *mut Parameters,
) -> *mut Application {
    unsafe {
        // env[0] is the out thunk
        let out_thunk: Procedure = env.as_ref().unwrap().clone().try_into().unwrap();

        // env[1] is k, the remaining continuation
        let k = env.add(1).as_ref().unwrap().clone();

        // args[0] is the result of the body thunk
        let body_thunk_res = args.as_ref().unwrap().clone();

        let params = params.as_mut().unwrap_unchecked();
        params.dynamic_wind.winders.pop();

        let cont = Procedure(Gc::new(ProcedureInner::new(
            Runtime::from_raw_inc_rc(runtime),
            vec![body_thunk_res, k],
            FuncPtr::Continuation(forward_body_thunk_result),
            0,
            true,
            None,
        )));

        let app = Application::new(out_thunk, vec![Value::from(cont)], None);

        Box::into_raw(Box::new(app))
    }
}

unsafe extern "C" fn forward_body_thunk_result(
    _runtime: *mut GcInner<RuntimeInner>,
    env: *const Value,
    _args: *const Value,
    _params: *mut Parameters,
) -> *mut Application {
    unsafe {
        // env[0] is the result of the body thunk
        let body_thunk_res = env.as_ref().unwrap().clone();
        // env[1] is k, the continuation.
        let k: Procedure = env.add(1).as_ref().unwrap().clone().try_into().unwrap();

        let mut args = Vec::new();
        list_to_vec(&body_thunk_res, &mut args);

        let app = Application::new(k, args, None);

        Box::into_raw(Box::new(app))
    }
}
