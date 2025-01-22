use std::sync::{Mutex, OnceLock};

use crate::{
    cps::Cps,
    proc::{Closure, SyncFuncPtr},
};
use inkwell::{
    context::Context, execution_engine::ExecutionEngine, module::Module, OptimizationLevel,
};
use tokio::{
    sync::{mpsc, oneshot},
    task::JoinHandle,
};

struct CompilationBuffer {
    compilation_buffer_tx: mpsc::Sender<CompilationTask>,
    compilation_buffer_rx: Mutex<Option<mpsc::Receiver<CompilationTask>>>,
}

impl Default for CompilationBuffer {
    fn default() -> Self {
        todo!()
    }
}

struct CompilationTask {
    completion_tx: oneshot::Sender<CompilationResult>,
    compilation_unit: Cps,
}

type CompilationResult = Result<Closure, CompilationError>;

enum CompilationError {}

static COMPILATION_QUEUE: OnceLock<CompilationBuffer> = OnceLock::new();
static COMPILATION_TASK: OnceLock<JoinHandle<()>> = OnceLock::new();

async fn compilation_task() {
    let mut compilation_queue_rx = COMPILATION_QUEUE
        .get_or_init(CompilationBuffer::default)
        .compilation_buffer_rx
        .lock()
        .unwrap()
        .take()
        .unwrap();

    // Create an LLVM context, module and execution engine. All of these should live for
    // the lifetime of the program.
    //
    // As far as I can tell to begin we only need one module. I haven't done extensive
    // research into the pros and cons of having multiple.
    let context = Context::create();
    let module = context.create_module("scheme_rs");
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::default())
        .unwrap();
    let builder = context.create_builder();

    install_runtime(&module, &execution_engine);

    while let Some(task) = compilation_queue_rx.recv().await {
        let CompilationTask {
            completion_tx,
            compilation_unit,
        } = task;

        let closure = compilation_unit.into_closure(&context, &module, &builder);

        let _ = completion_tx.send(Ok(closure));
    }
}

fn install_runtime(module: &Module<'_>, ee: &ExecutionEngine<'_>) {
    let f = module.add_function("gc_alloc_undef_val", todo!(), None);
    ee.add_global_mapping(&f, todo!() as usize)
}
