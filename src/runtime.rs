use crate::{
    cps::Cps,
    env::Local,
    exception::{Condition, ExceptionHandler},
    expand,
    gc::{init_gc, Gc, GcInner, Trace},
    lists::list_to_vec,
    num,
    proc::{
        /* clone_continuation_env, */
        Application, Closure, ClosurePtr, ContinuationPtr, DynamicWind, FuncPtr, FunctionDebugInfo,
    },
    syntax::Span,
    value::{ReflexiveValue, UnpackedValue, Value},
};
use indexmap::IndexMap;
use inkwell::{
    builder::BuilderError,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    targets::{InitializationConfig, Target},
    AddressSpace, OptimizationLevel,
};
use std::{collections::{HashMap, HashSet}, mem::ManuallyDrop, ptr::null_mut};
use tokio::sync::{mpsc, oneshot};

/// Scheme-rs Runtime
///
/// # Safety:
///
/// The runtime contains the only live references to the LLVM Context and therefore
/// modules and allocated functions in the form a Sender of compilation tasks.
///
/// When that sender's ref count is zero, it will cause the receiver to fail and the
/// compilation task will exit, allowing for a graceful shutdown.
///
/// However, this is dropping a lifetime. If we clone a closure and drop the runtime
/// from whence it was cleaved, we're left with a dangling pointer.
///
/// In order to remedy this it is vitally important the closure has a back pointer to
/// the runtime. Probably also want to make it immutable
#[derive(Trace, Clone, Debug)]
pub struct Runtime {
    compilation_buffer_tx: mpsc::Sender<CompilationTask>,
    // TODO: Make this something better than just a vec
    pub(crate) constants_pool: HashSet<ReflexiveValue>,
    pub(crate) debug_info: DebugInfo,
}

const MAX_COMPILATION_TASKS: usize = 5; // Shrug

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Runtime {
    pub fn new() -> Self {
        init_gc();
        let (compilation_buffer_tx, compilation_buffer_rx) = mpsc::channel(MAX_COMPILATION_TASKS);
        // According the inkwell (and therefore LLVM docs), one LlvmContext may be
        // present per thread. Thus, we spawn a new thread and a  new compilation
        // task for every Runtime.
        std::thread::spawn(move || compilation_task(compilation_buffer_rx));
        Runtime {
            compilation_buffer_tx,
            constants_pool: HashSet::new(),
            debug_info: DebugInfo::default(),
        }
    }
}

impl Gc<Runtime> {
    pub async fn compile_expr(&self, expr: Cps) -> Result<Gc<Closure>, BuilderError> {
        self.compile_expr_with_env(expr, IndexMap::default()).await
    }

    pub async fn compile_expr_with_env(
        &self,
        expr: Cps,
        env: IndexMap<Local, Gc<Value>>,
    ) -> Result<Gc<Closure>, BuilderError> {
        let (completion_tx, completion_rx) = oneshot::channel();
        let task = CompilationTask {
            env,
            completion_tx,
            compilation_unit: expr,
            runtime: self.clone(),
        };
        let sender = { self.read().compilation_buffer_tx.clone() };
        sender.send(task).await.unwrap();
        // Wait for the compilation task to complete:
        completion_rx.await.unwrap()
    }
}

#[derive(Trace, Clone, Debug, Default)]
pub struct DebugInfo {
    /// Location of function applications
    pub(crate) call_sites: Vec<Span>,
    /// Functions and their debug information
    pub(crate) function_debug_info: Vec<FunctionDebugInfo>,
}

pub type CallSiteId = u32;
pub type FunctionDebugInfoId = u32;

pub const IGNORE_CALL_SITE: CallSiteId = u32::MAX;
pub const IGNORE_FUNCTION: FunctionDebugInfoId = u32::MAX;

impl DebugInfo {
    pub fn new_call_site(&mut self, span: Span) -> CallSiteId {
        let id = self.call_sites.len();
        self.call_sites.push(span);
        id as CallSiteId
    }

    pub fn new_function_debug_info(
        &mut self,
        function_debug_info: FunctionDebugInfo,
    ) -> FunctionDebugInfoId {
        let id = self.function_debug_info.len();
        self.function_debug_info.push(function_debug_info);
        id as FunctionDebugInfoId
    }

    pub fn get_function_debug_info(
        &self,
        function_debug_info_id: FunctionDebugInfoId,
    ) -> Option<&FunctionDebugInfo> {
        if function_debug_info_id == IGNORE_FUNCTION {
            None
        } else {
            self.function_debug_info
                .get(function_debug_info_id as usize)
        }
    }
}

struct CompilationTask {
    env: IndexMap<Local, Gc<Value>>,
    compilation_unit: Cps,
    completion_tx: oneshot::Sender<CompilationResult>,
    /// Since Contexts are per-thread, we will only ever see the same Runtime. However,
    /// we can't cache the Runtime, as that would cause a ref cycle that would prevent
    /// the last compilation buffer sender to drop. Therefore, its lifetime is that of
    /// the compilation task
    runtime: Gc<Runtime>,
}

type CompilationResult = Result<Gc<Closure>, BuilderError>;

fn compilation_task(mut compilation_queue_rx: mpsc::Receiver<CompilationTask>) {
    Target::initialize_native(&InitializationConfig::default()).unwrap();

    // Create an LLVM context, module and execution engine. All of these should live for
    // the lifetime of the program.
    let context = Context::create();

    let mut modules = Vec::new();

    while let Some(task) = compilation_queue_rx.blocking_recv() {
        let CompilationTask {
            env,
            completion_tx,
            compilation_unit,
            runtime,
        } = task;

        // I don't really know a way to do this beyond just creating a new module every time.

        let module = context.create_module("scheme_rs");
        ExecutionEngine::link_in_mc_jit();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::default())
            .unwrap();
        let builder = context.create_builder();

        install_runtime(&context, &module, &execution_engine);

        let closure = compilation_unit
            .into_closure(runtime, env, &context, &module, &execution_engine, &builder)
            .map(Gc::new);

        modules.push(module);

        let _ = completion_tx.send(closure);
    }
}

fn install_runtime<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>, ee: &ExecutionEngine<'ctx>) {
    let i32_type = ctx.i32_type();
    let i64_type = ctx.i64_type();
    let bool_type = ctx.bool_type();
    let void_type = ctx.void_type();
    let ptr_type = ctx.ptr_type(AddressSpace::default());

    // alloc_cell:
    let sig = ptr_type.fn_type(&[], false);
    let f = module.add_function("alloc_cell", sig, None);
    ee.add_global_mapping(&f, alloc_cell as usize);

    // read_cell:
    let sig = i64_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("read_cell", sig, None);
    ee.add_global_mapping(&f, read_cell as usize);

    // drop_cells:
    let sig = void_type.fn_type(&[ptr_type.into(), i32_type.into()], false);
    let f = module.add_function("drop_cells", sig, None);
    ee.add_global_mapping(&f, drop_cells as usize);

    // drop_values:
    let sig = void_type.fn_type(&[ptr_type.into(), i32_type.into()], false);
    let f = module.add_function("drop_values", sig, None);
    ee.add_global_mapping(&f, drop_values as usize);

    // apply:
    let sig = ptr_type.fn_type(
        &[
            ptr_type.into(), // runtime
            i64_type.into(), // operator
            ptr_type.into(), // args
            i32_type.into(), // num args
            ptr_type.into(), // exception handler
            ptr_type.into(), // dynamic wind
            i32_type.into(), // call site id
        ],
        false,
    );
    let f = module.add_function("apply", sig, None);
    ee.add_global_mapping(&f, apply as usize);

    // forward:
    let sig = ptr_type.fn_type(
        &[
            i64_type.into(), // operator
            i64_type.into(), // args
            ptr_type.into(), // exception handler
            ptr_type.into(), // dynamic_wind
        ],
        false,
    );
    let f = module.add_function("forward", sig, None);
    ee.add_global_mapping(&f, forward as usize);

    // halt:
    let sig = ptr_type.fn_type(&[i64_type.into()], false);
    let f = module.add_function("halt", sig, None);
    ee.add_global_mapping(&f, halt as usize);

    // truthy:
    let sig = bool_type.fn_type(&[i64_type.into()], false);
    let f = module.add_function("truthy", sig, None);
    ee.add_global_mapping(&f, truthy as usize);

    // store:
    let sig = void_type.fn_type(&[i64_type.into(), ptr_type.into()], false);
    let f = module.add_function("store", sig, None);
    ee.add_global_mapping(&f, store as usize);

    // make_continuation
    let sig = ptr_type.fn_type(
        &[
            ptr_type.into(), // Runtime
            ptr_type.into(), // Continuation Ptr
            ptr_type.into(), // Env
            i32_type.into(), // Num envs
            ptr_type.into(), // Globals
            i32_type.into(), // Num globals
            i32_type.into(), // Num required args
            bool_type.into(), // Variadic?
        ],
        false,
    );
    let f = module.add_function("make_continuation", sig, None);
    ee.add_global_mapping(&f, make_continuation as usize);

    // make_closure:
    let sig = ptr_type.fn_type(
        &[
            ptr_type.into(), // Runtime
            ptr_type.into(), // Closure Ptr
            ptr_type.into(), // Env
            i32_type.into(), // Num envs
            ptr_type.into(), // Globals
            i32_type.into(), // Num globals
            i32_type.into(), // Num required args
            bool_type.into(), // Variadic?
            i32_type.into(), // Debug info 
        ],
        false,
    );
    let f = module.add_function("make_closure", sig, None);
    ee.add_global_mapping(&f, make_closure as usize);

    /*
    // get_call_transformer_fn:
    let sig = ptr_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("get_call_transformer_fn", sig, None);
    ee.add_global_mapping(&f, get_call_transformer_fn as usize);

    // extract_winders:
    let sig = ptr_type.fn_type(&[ptr_type.into()], false);
    let f = module.add_function("extract_winders", sig, None);
    ee.add_global_mapping(&f, extract_winders as usize);

    // prepare_continuation:
    let sig = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into(), ptr_type.into()], false);
    let f = module.add_function("prepare_continuation", sig, None);
    ee.add_global_mapping(&f, prepare_continuation as usize);
    */

    // add:
    let sig = i64_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("add", sig, None);
    ee.add_global_mapping(&f, add as usize);

    // sub:
    let sig = i64_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("sub", sig, None);
    ee.add_global_mapping(&f, sub as usize);

    // mul:
    let sig = i64_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("mul", sig, None);
    ee.add_global_mapping(&f, mul as usize);

    // div:
    let sig = i64_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("div", sig, None);
    ee.add_global_mapping(&f, div as usize);

    // equal:
    let sig = i64_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("equal", sig, None);
    ee.add_global_mapping(&f, equal as usize);

    // greater:
    let sig = i64_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("greater", sig, None);
    ee.add_global_mapping(&f, greater as usize);

    // greater_equal:
    let sig = i64_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("greater_equal", sig, None);
    ee.add_global_mapping(&f, greater_equal as usize);

    // lesser:
    let sig = i64_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("lesser", sig, None);
    ee.add_global_mapping(&f, lesser as usize);

    // lesser_equal:
    let sig = i64_type.fn_type(&[ptr_type.into(), i32_type.into(), ptr_type.into()], false);
    let f = module.add_function("lesser_equal", sig, None);
    ee.add_global_mapping(&f, lesser_equal as usize);
}

/// Allocate a new Gc with a value of undefined
unsafe extern "C" fn alloc_cell() -> *mut GcInner<Value> {
    Gc::into_raw(Gc::new(Value::undefined()))
}

/// Read the value of a Cell
unsafe extern "C" fn read_cell(cell: *mut GcInner<Value>) -> i64 {
    // We do not need to increment the reference count of the cell, it is going to
    // be decremented at the end of this function.
    let cell = ManuallyDrop::new(Gc::from_raw(cell));
    let cell_read = cell.read();
    let raw = Value::as_raw(&cell_read);
    raw as i64
}

/// Decrement the reference count of all of the cells
unsafe extern "C" fn drop_cells(cells: *const *mut GcInner<Value>, num_cells: u32) {
    for i in 0..num_cells {
        Gc::decrement_reference_count(cells.add(i as usize).read())
    }
}

/// Decrement the reference count of all of the values
unsafe extern "C" fn drop_values(vals: *const i64, num_vals: u32) {
    for i in 0..num_vals {
        drop(Value::from_raw(vals.add(i as usize).read() as u64));
    }
}

/// Create a boxed application
/// TODO: Take error handler as argument, return application with error handler
/// if operator is not a closure.
unsafe extern "C" fn apply(
    runtime: *mut GcInner<Runtime>,
    op: i64,
    args: *const i64,
    num_args: u32,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
    call_site_id: u32,
) -> *mut Result<Application, Condition> {
    
    let args: Vec<_> = (0..num_args)
        .map(|i|
            Value::from_raw_inc_rc(args.add(i as usize).read() as u64)
        )
        .collect();

    let op = match Value::from_raw_inc_rc(op as u64).unpack() {
        UnpackedValue::Closure(op) => op,
        x => {
            return Box::into_raw(Box::new(Err(Condition::invalid_operator_type(
                x.type_name(),
            ))))
        }
    };

    /*
    let call_site = (call_site_id != u32::MAX).then(|| {
        // No need to increment the ref count for runtime here, it is dropped
        // immediately.
        let runtime = ManuallyDrop::new(Gc::from_raw(runtime));
        let runtime_read = runtime.read();
        runtime_read.debug_info.call_sites[call_site_id as usize].clone()
    });
     */

    let app = Application::new(
        op,
        args,
        ExceptionHandler::from_ptr(exception_handler),
        dynamic_wind.as_ref().unwrap().clone(),
        // call_site,
        None
    );

    Box::into_raw(Box::new(Ok(app)))
}

/// Create a boxed application that forwards a list of values to the operator
unsafe extern "C" fn forward(
    op: i64,
    args: i64,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    let op = match Value::from_raw_inc_rc(op as u64).unpack() {
        UnpackedValue::Closure(op) => op,
        x => {
            return Box::into_raw(Box::new(Err(Condition::invalid_operator_type(
                x.type_name(),
            ))))
        }
    };

    // We do not need to increment to forward here, for the same reason as in
    // halt.
    let args = ManuallyDrop::new(Value::from_raw(args as u64));
    let mut flattened = Vec::new();
    list_to_vec(&args, &mut flattened);

    let app = Application::new(
        op,
        flattened,
        ExceptionHandler::from_ptr(exception_handler),
        dynamic_wind.as_ref().unwrap().clone(),
        None,
    );

    Box::into_raw(Box::new(Ok(app)))
}

/// Create a boxed application that simply returns its arguments
pub(crate) unsafe extern "C" fn halt(args: i64) -> *mut Result<Application, Condition> {
    // We do not need to increment the rc here, it will be incremented in list_to_vec
    let args = ManuallyDrop::new(Value::from_raw(args as u64));
    let mut flattened = Vec::new();
    list_to_vec(&args, &mut flattened);
    let app = Application::halt(flattened);
    Box::into_raw(Box::new(Ok(app)))
}

/// Evaluate a `Gc<Value>` as "truthy" or not, as in whether it triggers a
/// conditional.
unsafe extern "C" fn truthy(val: i64) -> bool {
    // No need to increment the reference count here:
    ManuallyDrop::new(Value::from_raw(val as u64)).is_true()
}

/// Replace the value pointed to at to with the value contained in from.
unsafe extern "C" fn store(from: i64, to: *mut GcInner<Value>) {
    // We do not need to increment the ref count for to, it is dropped
    // immediately.
    let from = Value::from_raw_inc_rc(from as u64);
    let to = ManuallyDrop::new(Gc::from_raw(to));
    *to.write() = from;
}

/// Allocate a closure
unsafe extern "C" fn make_continuation(
    runtime: *mut GcInner<Runtime>,
    fn_ptr: ContinuationPtr,
    env: *const *mut GcInner<Value>,
    num_envs: u32,
    globals: *const *mut GcInner<Value>,
    num_globals: u32,
    num_required_args: u32,
    variadic: bool,
) -> *mut GcInner<Value> {
    // Collect the environment:
    let env: Vec<_> = (0..num_envs)
        .map(|i| Gc::from_raw_inc_rc(env.add(i as usize).read()))
        .collect();

    // Collect the globals:
    let globals: Vec<_> = (0..num_globals)
        .map(|i| {
            let raw = globals.add(i as usize).read();
            Gc::from_raw_inc_rc(raw)
        })
        .collect();

    let closure = Closure::new(
        Gc::from_raw_inc_rc(runtime),
        env,
        globals,
        FuncPtr::Continuation(fn_ptr),
        num_required_args as usize,
        variadic,
        None,
    );

    Gc::into_raw(Gc::new(Value::from(closure)))
}

/// Allocate a closure for a function that takes a continuation
unsafe extern "C" fn make_closure(
    runtime: *mut GcInner<Runtime>,
    fn_ptr: ClosurePtr,
    env: *const *mut GcInner<Value>,
    num_envs: u32,
    globals: *const *mut GcInner<Value>,
    num_globals: u32,
    num_required_args: u32,
    variadic: bool,
    debug_info_id: u32,
) -> *mut GcInner<Value> {
    // Collect the environment:
    let env: Vec<_> = (0..num_envs)
        .map(|i| Gc::from_raw_inc_rc(env.add(i as usize).read()))
        .collect();

    // Collect the globals:
    let globals: Vec<_> = (0..num_globals)
        .map(|i| {
            let raw = globals.add(i as usize).read();
            Gc::from_raw_inc_rc(raw)
        })
        .collect();

    let closure = Closure::new(
        Gc::from_raw_inc_rc(runtime),
        env,
        globals,
        FuncPtr::Closure(fn_ptr),
        num_required_args as usize,
        variadic,
        // Some(debug_info_id),
        None
    );

    let raw = Gc::into_raw(Gc::new(Value::from(closure)));
    raw  
}

/// Call a transformer with the given argument and return the expansion
unsafe extern "C" fn get_call_transformer_fn(runtime: *mut GcInner<Runtime>) -> i64 {
    /*
    let closure = Closure::new(
        Gc::from_raw(runtime),
        Vec::new(),
        Vec::new(),
        FuncPtr::Bridge(expand::call_transformer),
        3,
        true,
        Some(IGNORE_FUNCTION),
    );
    ManuallyDrop::new(Gc::new(Value::Closure(closure))).as_ptr()
     */
    todo!()
}

/*
/// Create a pair of the two provided values.
unsafe extern "C" fn cons(
    head: *mut GcInner<Value>,
    tail: *mut GcInner<Value>,
) -> *mut GcInner<Value> {
    let head = Gc::from_ptr(head);
    let tail = Gc::from_ptr(tail);
    ManuallyDrop::new(Gc::new(Value::Pair(head, tail))).as_ptr()
}
*/

/// Extract the current winders from the environment and return them as a vec.
unsafe extern "C" fn extract_winders(dynamic_wind: *const DynamicWind) -> i64 {
    /*
    let dynamic_wind = dynamic_wind.as_ref().unwrap();
    let winders: Vec<_> = dynamic_wind
        .winders
        .iter()
        .cloned()
        .map(|(in_winder, out_winder)| {
            Value::Pair(
                Gc::new(Value::Closure(in_winder)),
                Gc::new(Value::Closure(out_winder)),
            )
        })
        .collect();
    ManuallyDrop::new(Gc::new(Value::Vector(winders))).as_ptr()
     */
    todo!()
}

/// Prepare the continuation for call/cc. Clones the continuation environment
/// and creates a closure that calls the appropriate winders.
///
/// Expects that the continuation and winders will be provided in the form of a
/// pair of the continuation and vector of pairs of in/out winders.
unsafe extern "C" fn prepare_continuation(
    cont: i64,
    winders: i64,
    from_dynamic_extent: *const DynamicWind,
) -> i64 {
    /*
    // Determine which winders we will need to call. This is determined as the
    // winders provided in cont_and_winders with the prefix of curr_dynamic_wind
    // removed.
    let cont = Gc::from_raw(cont);
    let winders = Gc::from_raw(winders);
    let winders = winders.read();
    let to_winders: &Vec<Value> = winders.as_ref().try_into().unwrap();
    let from_winders = from_dynamic_extent.as_ref().unwrap();

    let thunks = compute_winders(from_winders, to_winders);

    // Clone the continuation
    let cont = clone_continuation_env(&cont, &mut HashMap::default());

    let (runtime, req_args, variadic) = {
        let cont = cont.read();
        let cont: &Closure = cont.as_ref().try_into().unwrap();
        (cont.runtime.clone(), cont.num_required_args, cont.variadic)
    };

    ManuallyDrop::new(Gc::new(Value::Closure(Closure::new(
        runtime,
        vec![thunks, cont],
        Vec::new(),
        FuncPtr::Continuation(call_thunks),
        req_args,
        variadic,
        None,
    ))))
    .as_ptr()
     */
    todo!()
}

fn compute_winders(from_extent: &DynamicWind, to_extent: &[Value]) -> Value {
    /*
    let len = from_extent.winders.len().min(to_extent.len());

    let mut split_point = 0;
    #[allow(clippy::needless_range_loop)]
    for i in 0..len {
        let Value::Pair(ref to_in, _) = to_extent[i] else {
            unreachable!()
        };
        let to_in_ref = to_in.read();
        let to_in: &Closure = to_in_ref.as_ref().try_into().unwrap();
        if &from_extent.winders[i].0 == to_in {
            split_point = i + 1;
        } else {
            break;
        }
    }

    let (_, to_extent) = to_extent.split_at(split_point);
    let (_, from_extent) = from_extent
        .winders
        .split_at_checked(split_point)
        .unwrap_or((&[], &[]));

    let mut thunks = Gc::new(Value::Null);
    for thunk in from_extent
        .iter()
        .map(|(_, out)| Gc::new(Value::Closure(out.clone())))
        .chain(
            to_extent
                .iter()
                .map(|to_extent| {
                    let Value::Pair(ref to_in, _) = to_extent else {
                        unreachable!()
                    };
                    to_in.clone()
                })
                .rev(),
        )
    {
        thunks = Gc::new(Value::Pair(thunk, thunks));
    }

    thunks
     */
    todo!()
}

unsafe extern "C" fn call_thunks(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    /*
    // env[0] are the thunks:
    let thunks = Gc::from_raw(env.read());
    // env[1] is the continuation:
    let k: Closure = Gc::from_raw(env.add(1).read()).try_into().unwrap();

    // k determines the number of arguments:
    let num_args = k.num_required_args;
    let mut collected_args = if k.variadic {
        Gc::from_raw(args.add(num_args).read())
    } else {
        Gc::new(Value::Null)
    };

    for i in (0..num_args).rev() {
        collected_args = Gc::new(Value::Pair(
            Gc::from_raw(args.add(i).read()),
            collected_args,
        ));
    }

    let thunks = Closure::new(
        Gc::from_raw(runtime),
        vec![thunks, collected_args, Gc::new(Value::Closure(k))],
        Vec::new(),
        FuncPtr::Continuation(call_thunks_pass_args),
        0,
        false,
        None,
    );

    let app = Application::new(
        thunks,
        Vec::new(),
        ExceptionHandler::from_ptr(exception_handler),
        dynamic_wind.as_ref().unwrap().clone(),
        None,
    );

    Box::into_raw(Box::new(Ok(app)))
     */
    todo!()
}

unsafe extern "C" fn call_thunks_pass_args(
    runtime: *mut GcInner<Runtime>,
    env: *const *mut GcInner<Value>,
    _globals: *const *mut GcInner<Value>,
    _args: *const *mut GcInner<Value>,
    exception_handler: *mut GcInner<ExceptionHandler>,
    dynamic_wind: *const DynamicWind,
) -> *mut Result<Application, Condition> {
    /*
    // env[0] are the thunks:
    let thunks = Gc::from_raw(env.read());
    // env[1] are the collected arguments
    let args = Gc::from_raw(env.add(1).read());
    // env[2] is k1, the current continuation
    let k = Gc::from_raw(env.add(2).read());

    let thunks = thunks.read();
    let app = match thunks.as_ref() {
        Value::Pair(head_thunk, tail) => {
            let head_thunk = head_thunk.read();
            let head_thunk: &Closure = head_thunk.as_ref().try_into().unwrap();
            let cont = Closure::new(
                Gc::from_raw(runtime),
                vec![tail.clone(), args, k],
                Vec::new(),
                FuncPtr::Continuation(call_thunks_pass_args),
                0,
                false,
                None,
            );
            Application::new(
                head_thunk.clone(),
                vec![Gc::new(Value::Closure(cont))],
                ExceptionHandler::from_ptr(exception_handler),
                dynamic_wind.as_ref().unwrap().clone(),
                None,
            )
        }
        Value::Null => {
            let mut collected_args = Vec::new();
            list_to_vec(&args, &mut collected_args);
            // collected_args.push(Gc::new(Value::Null));
            Application::new(
                k.try_into().unwrap(),
                collected_args,
                ExceptionHandler::from_ptr(exception_handler),
                dynamic_wind.as_ref().unwrap().clone(),
                None,
            )
        }
        _ => unreachable!(),
    };

    Box::into_raw(Box::new(Ok(app)))
     */
    todo!()
}

unsafe extern "C" fn add(
    vals: *const i64,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> i64 {
    let vals: Vec<_> = (0..num_vals)
        // Can't easily wrap these in a ManuallyDrop, so we dec the rc.
        .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
        .collect();
    match num::add(&vals) {
        Ok(num) => Value::into_raw(Value::from(num)) as i64,
        Err(condition) => {
            error.write(Box::into_raw(Box::new(Err(condition))));
            Value::into_raw(Value::undefined()) as i64
        }
    }
}

unsafe extern "C" fn sub(
    vals: *const i64,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> i64 {
    let vals: Vec<_> = (0..num_vals)
        .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
        .collect();
    match num::sub(&vals[0], &vals[1..]) {
        Ok(num) => Value::into_raw(Value::from(num)) as i64,
        Err(condition) => {
            error.write(Box::into_raw(Box::new(Err(condition))));
            Value::into_raw(Value::undefined()) as i64
        }
    } 
}

unsafe extern "C" fn mul(
    vals: *const i64,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> i64 {
    let vals: Vec<_> = (0..num_vals)
        .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
        .collect();
    match num::mul(&vals) {
        Ok(num) => Value::into_raw(Value::from(num)) as i64,
        Err(condition) => {
            error.write(Box::into_raw(Box::new(Err(condition))));
            Value::into_raw(Value::undefined()) as i64
        }
    }
}

unsafe extern "C" fn div(
    vals: *const i64,
    num_vals: u32,
    error: *mut *mut Result<Application, Condition>,
) -> i64 {
    let vals: Vec<_> = (0..num_vals)
        .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
        .collect();
    match num::div(&vals[0], &vals[1..]) {
        Ok(num) => Value::into_raw(Value::from(num)) as i64,
        Err(condition) => {
            error.write(Box::into_raw(Box::new(Err(condition))));
            Value::into_raw(Value::undefined()) as i64
        }
    }
}

macro_rules! define_comparison_fn {
    ( $name:ident ) => {
        unsafe extern "C" fn $name(
            vals: *const i64,
            num_vals: u32,
            error: *mut *mut Result<Application, Condition>,
        ) -> i64 {
            let vals: Vec<_> = (0..num_vals)
                .map(|i| Value::from_raw_inc_rc(vals.add(i as usize).read() as u64))
                .collect();
            match num::$name(&vals) {
                Ok(res) => Value::into_raw(Value::from(res)) as i64,
                Err(condition) => {
                    error.write(Box::into_raw(Box::new(Err(condition))));
                    Value::into_raw(Value::undefined()) as i64
                }
            }
        }
    };
}

define_comparison_fn!(equal);
define_comparison_fn!(greater);
define_comparison_fn!(greater_equal);
define_comparison_fn!(lesser);
define_comparison_fn!(lesser_equal);
