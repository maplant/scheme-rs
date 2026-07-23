#![cfg(feature = "plugins")]

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::{Arc, LazyLock, Mutex};

use scheme_rs_plugin_api::HostFnTable;

use crate::ast::LibraryName;
use crate::env::{TOP_LEVEL_BINDINGS, TopLevelBinding};
use crate::exceptions::{Exception, raise};
use crate::proc::{Application, BridgePtr, ContBarrier, Procedure};
use crate::records::{Field, Record, RecordTypeDescriptor};
use crate::runtime::Runtime;
use crate::symbols::Symbol;
use crate::value::Value as HostValue;

type PluginValue = scheme_rs_plugin_api::Value;

#[inline]
unsafe fn plugin_to_host(v: PluginValue) -> HostValue {
    unsafe { std::mem::transmute(v) }
}

#[inline]
unsafe fn host_to_plugin(v: HostValue) -> PluginValue {
    unsafe { std::mem::transmute(v) }
}

#[inline]
fn plugin_ref_to_host(v: *const PluginValue) -> *const HostValue {
    v as *const HostValue
}

// ── Value construction ─────────────────────────────────────────────────────

unsafe extern "C" fn host_make_integer(n: i64) -> PluginValue {
    let v = HostValue::from(n);
    unsafe { host_to_plugin(v) }
}

unsafe extern "C" fn host_make_float(n: f64) -> PluginValue {
    let v = HostValue::from(n);
    unsafe { host_to_plugin(v) }
}

unsafe extern "C" fn host_make_string(ptr: *const u8, len: usize) -> PluginValue {
    let s = unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr, len)) };
    let v = HostValue::from(s.to_string());
    unsafe { host_to_plugin(v) }
}

unsafe extern "C" fn host_make_symbol(ptr: *const u8, len: usize) -> PluginValue {
    let s = unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr, len)) };
    let sym = crate::symbols::Symbol::intern(s);
    let v = HostValue::from(sym);
    unsafe { host_to_plugin(v) }
}

unsafe extern "C" fn host_cons(car: PluginValue, cdr: PluginValue) -> PluginValue {
    let car = unsafe { plugin_to_host(car) };
    let cdr = unsafe { plugin_to_host(cdr) };
    let pair = crate::lists::Pair::immutable(car, cdr);
    unsafe { host_to_plugin(HostValue::from(pair)) }
}

unsafe extern "C" fn host_make_vector(elems: *const PluginValue, len: usize) -> PluginValue {
    let slice = unsafe { std::slice::from_raw_parts(elems as *const HostValue, len) };
    let vec: Vec<HostValue> = slice.iter().map(|v| v.clone()).collect();
    unsafe { host_to_plugin(HostValue::from(vec)) }
}

// ── Value extraction ───────────────────────────────────────────────────────

unsafe extern "C" fn host_to_integer(v: *const PluginValue) -> i64 {
    let v = unsafe { &*plugin_ref_to_host(v) };
    i64::try_from(v).unwrap_or(0)
}

unsafe extern "C" fn host_to_float(v: *const PluginValue) -> f64 {
    let v = unsafe { &*plugin_ref_to_host(v) };
    f64::try_from(v).unwrap_or(0.0)
}

unsafe extern "C" fn host_to_string_copy(
    v: *const PluginValue,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) {
    let v = unsafe { &*plugin_ref_to_host(v) };
    let wide: crate::strings::WideString = match v.clone().try_into() {
        Ok(w) => w,
        Err(_) => {
            unsafe {
                *out_ptr = std::ptr::null_mut();
                *out_len = 0;
            }
            return;
        }
    };
    let s: String = wide.into();
    let bytes = s.into_bytes().into_boxed_slice();
    let len = bytes.len();
    let ptr = Box::into_raw(bytes) as *mut u8;
    unsafe {
        *out_ptr = ptr;
        *out_len = len;
    }
}

// ── Pair ops ───────────────────────────────────────────────────────────────

unsafe extern "C" fn host_car(v: *const PluginValue) -> PluginValue {
    let v = unsafe { &*plugin_ref_to_host(v) };
    let pair: crate::lists::Pair = v.clone().try_into().expect("car: not a pair");
    unsafe { host_to_plugin(pair.car()) }
}

unsafe extern "C" fn host_cdr(v: *const PluginValue) -> PluginValue {
    let v = unsafe { &*plugin_ref_to_host(v) };
    let pair: crate::lists::Pair = v.clone().try_into().expect("cdr: not a pair");
    unsafe { host_to_plugin(pair.cdr()) }
}

// ── Refcounting ────────────────────────────────────────────────────────────

unsafe extern "C" fn host_value_retain(raw: usize) {
    let v = unsafe { HostValue::from_raw_inc_rc(raw as *const ()) };
    std::mem::forget(v);
}

unsafe extern "C" fn host_value_release(raw: usize) {
    let _ = unsafe { HostValue::from_raw(raw as *const ()) };
}

// ── Plugin bridge wrapper ──────────────────────────────────────────────────

/// Static bridge function that dispatches to a plugin's SimpleBridgeFn.
/// env[0] holds the plugin function pointer as a raw i64.
fn plugin_bridge_wrapper(
    runtime: &Runtime,
    env: &[HostValue],
    k: Procedure,
    args: &[HostValue],
    rest_args: &[HostValue],
    barrier: &mut ContBarrier,
) -> Application {
    let fn_ptr_raw: i64 = (&env[0]).try_into().unwrap_or(0);
    let plugin_fn: scheme_rs_plugin_api::SimpleBridgeFn =
        unsafe { std::mem::transmute(fn_ptr_raw as usize) };

    let all_args: Vec<HostValue> = args.iter().chain(rest_args).cloned().collect();

    let result = unsafe {
        plugin_fn(
            all_args.as_ptr() as *const PluginValue,
            all_args.len(),
        )
    };

    let raw_value = result.value.as_raw();
    let error_ptr = result.error;
    let error_len = result.error_len;
    std::mem::forget(result);

    if error_ptr.is_null() {
        let host_val = unsafe { HostValue::from_raw(raw_value as *const ()) };
        Application::new(k, None, vec![host_val])
    } else {
        let err_str = unsafe {
            let slice = std::slice::from_raw_parts(error_ptr, error_len);
            let s = String::from_utf8_lossy(slice).into_owned();
            drop(Box::from_raw(std::slice::from_raw_parts_mut(
                error_ptr as *mut u8,
                error_len,
            )));
            s
        };
        raise(runtime.clone(), Exception::error(&err_str).into(), barrier)
    }
}

// ── Pending bridge registration ────────────────────────────────────────────

pub(crate) struct PendingBridge {
    pub name: String,
    pub lib_name: String,
    pub num_args: usize,
    pub variadic: bool,
    pub func_ptr: usize,
}

pub(crate) struct PendingDefine {
    pub name: String,
    pub lib_name: String,
    pub value: HostValue,
}

thread_local! {
    static PENDING_BRIDGES: RefCell<Vec<PendingBridge>> = RefCell::new(Vec::new());
    static PENDING_DEFINES: RefCell<Vec<PendingDefine>> = RefCell::new(Vec::new());
    static LOADING_RUNTIME: RefCell<Option<Runtime>> = RefCell::new(None);
    static CURRENT_LIB_NAME: RefCell<Option<String>> = RefCell::new(None);
}

pub(crate) fn set_loading_runtime(rt: &Runtime) {
    LOADING_RUNTIME.with(|cell| *cell.borrow_mut() = Some(rt.clone()));
}

pub(crate) fn clear_loading_runtime() {
    LOADING_RUNTIME.with(|cell| *cell.borrow_mut() = None);
    CURRENT_LIB_NAME.with(|cell| *cell.borrow_mut() = None);
}

pub(crate) fn take_pending_bridges() -> Vec<PendingBridge> {
    PENDING_BRIDGES.with(|pb| std::mem::take(&mut *pb.borrow_mut()))
}

pub(crate) fn take_pending_defines() -> Vec<PendingDefine> {
    PENDING_DEFINES.with(|pd| std::mem::take(&mut *pd.borrow_mut()))
}

unsafe extern "C" fn host_register_bridge(
    spec: *const scheme_rs_plugin_api::BridgeSpec,
) -> bool {
    let spec = unsafe { &*spec };
    let name = unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(spec.name_ptr, spec.name_len))
    };
    let lib_name = unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(spec.lib_ptr, spec.lib_len))
    };
    let func = match spec.func {
        Some(f) => f as usize,
        None => return false,
    };

    let lib_name_owned = lib_name.to_string();
    CURRENT_LIB_NAME.with(|cell| *cell.borrow_mut() = Some(lib_name_owned.clone()));
    PENDING_BRIDGES.with(|pb| {
        pb.borrow_mut().push(PendingBridge {
            name: name.to_string(),
            lib_name: lib_name_owned,
            num_args: spec.num_args,
            variadic: spec.variadic,
            func_ptr: func,
        });
    });
    true
}

/// Create a host Procedure wrapping a plugin's SimpleBridgeFn.
pub(crate) fn make_plugin_procedure(
    rt: &Runtime,
    fn_ptr: usize,
    num_args: usize,
    variadic: bool,
) -> Procedure {
    let env = vec![HostValue::from(fn_ptr as i64)];
    Procedure::new(
        rt.clone(),
        env,
        plugin_bridge_wrapper as BridgePtr,
        num_args,
        variadic,
    )
}

// ── host_call ──────────────────────────────────────────────────────────────

unsafe extern "C" fn host_call(
    proc: *const PluginValue,
    args: *const PluginValue,
    argc: usize,
    err_ptr: *mut *mut u8,
    err_len: *mut usize,
) -> PluginValue {
    let proc_val = unsafe { &*plugin_ref_to_host(proc) };
    let procedure: Procedure = match proc_val.clone().try_into() {
        Ok(p) => p,
        Err(e) => {
            let msg = format!("{e}");
            let bytes = msg.into_bytes().into_boxed_slice();
            let len = bytes.len();
            let ptr = Box::into_raw(bytes) as *mut u8;
            unsafe {
                *err_ptr = ptr;
                *err_len = len;
            }
            return PluginValue::undefined();
        }
    };

    let args_slice = unsafe { std::slice::from_raw_parts(plugin_ref_to_host(args), argc) };
    let host_args: Vec<HostValue> = args_slice.iter().map(|v| v.clone()).collect();

    let mut barrier = ContBarrier::new();

    #[cfg(feature = "async")]
    let result = procedure.call_sync(&host_args, &mut barrier);
    #[cfg(not(feature = "async"))]
    let result = procedure.call(&host_args, &mut barrier);

    match result {
        Ok(vals) => {
            unsafe {
                *err_ptr = std::ptr::null_mut();
                *err_len = 0;
            }
            let host_val = vals.into_iter().next().unwrap_or_else(HostValue::null);
            unsafe { host_to_plugin(host_val) }
        }
        Err(e) => {
            let msg = format!("{e}");
            let bytes = msg.into_bytes().into_boxed_slice();
            let len = bytes.len();
            let ptr = Box::into_raw(bytes) as *mut u8;
            unsafe {
                *err_ptr = ptr;
                *err_len = len;
            }
            PluginValue::undefined()
        }
    }
}

// ── Stubs (future tasks) ───────────────────────────────────────────────────

unsafe extern "C" fn host_apply_continuation(
    k: *const PluginValue,
    args: *const PluginValue,
    argc: usize,
) -> scheme_rs_plugin_api::ApplicationResult {
    let k_val = unsafe { &*plugin_ref_to_host(k) }.clone();
    let k_proc: Procedure = k_val.try_into().expect("apply_continuation: k is not a procedure");
    let args_slice = unsafe { std::slice::from_raw_parts(plugin_ref_to_host(args), argc) };
    let args_vec: Vec<HostValue> = args_slice.iter().cloned().collect();
    let app = Application::new(k_proc, None, args_vec);
    unsafe { std::mem::transmute(Box::into_raw(Box::new(app)) as *mut c_void) }
}

unsafe extern "C" fn host_make_application(
    proc_ptr: *const PluginValue,
    k: *const PluginValue,
    args: *const PluginValue,
    argc: usize,
) -> scheme_rs_plugin_api::ApplicationResult {
    let proc_val = unsafe { &*plugin_ref_to_host(proc_ptr) }.clone();
    let proc: Procedure = proc_val.try_into().expect("make_application: proc is not a procedure");
    let k_val = unsafe { &*plugin_ref_to_host(k) }.clone();
    let k_proc: Procedure = k_val.try_into().expect("make_application: k is not a procedure");
    let args_slice = unsafe { std::slice::from_raw_parts(plugin_ref_to_host(args), argc) };
    let args_vec: Vec<HostValue> = args_slice.iter().cloned().collect();
    let app = Application::new(proc, Some(k_proc), args_vec);
    unsafe { std::mem::transmute(Box::into_raw(Box::new(app)) as *mut c_void) }
}

unsafe extern "C" fn host_raise_error(
    msg: *const u8,
    len: usize,
    _barrier: *mut c_void,
) -> scheme_rs_plugin_api::ApplicationResult {
    let err_str = unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(msg, len))
    };
    let exception = Exception::error(err_str);
    let app = Application::halt_err(exception.into());
    unsafe { std::mem::transmute(Box::into_raw(Box::new(app)) as *mut c_void) }
}

static FOREIGN_FINALIZERS: LazyLock<Mutex<HashMap<usize, unsafe extern "C" fn(*mut c_void)>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

unsafe extern "C" fn host_register_type(
    name: *const u8,
    name_len: usize,
    finalizer: Option<unsafe extern "C" fn(*mut c_void)>,
) -> usize {
    let name_str =
        unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(name, name_len)) };
    let sym = Symbol::intern(name_str);
    let data_sym = Symbol::intern("data");

    let rtd = Arc::new(RecordTypeDescriptor {
        name: sym,
        sealed: true,
        opaque: true,
        uid: Some(sym),
        embedded_vtable: None,
        embedded_constructor: None,
        inherits: indexmap::IndexSet::new(),
        num_inherited_fields: 0,
        fields: vec![Field::Immutable(data_sym)],
    });

    let ptr = Arc::into_raw(rtd);
    let handle = ptr as usize;

    if let Some(f) = finalizer {
        FOREIGN_FINALIZERS.lock().unwrap().insert(handle, f);
    }

    handle
}

unsafe extern "C" fn host_make_foreign(type_handle: usize, data: *mut c_void) -> PluginValue {
    let rtd_ptr = type_handle as *const RecordTypeDescriptor;
    let rtd = unsafe { Arc::from_raw(rtd_ptr) };
    let rtd_clone = rtd.clone();
    std::mem::forget(rtd);

    let record = Record::new_plain(
        rtd_clone,
        vec![HostValue::from(data as usize as i64)],
    );

    unsafe { host_to_plugin(HostValue::from(record)) }
}

unsafe extern "C" fn host_get_foreign(
    type_handle: usize,
    value: *const PluginValue,
) -> *mut c_void {
    let v = unsafe { &*plugin_ref_to_host(value) };
    let record: Record = match v.clone().try_into() {
        Ok(r) => r,
        Err(_) => return std::ptr::null_mut(),
    };

    let rtd_ptr = type_handle as *const RecordTypeDescriptor;
    let expected_rtd = unsafe { Arc::from_raw(rtd_ptr) };
    let matches = Arc::ptr_eq(&record.rtd(), &expected_rtd);
    std::mem::forget(expected_rtd);

    if !matches {
        return std::ptr::null_mut();
    }

    let data_val = record.0.fields()[0].clone();
    let ptr: i64 = match (&data_val).try_into() {
        Ok(n) => n,
        Err(_) => return std::ptr::null_mut(),
    };

    ptr as usize as *mut c_void
}

unsafe extern "C" fn host_define(name_ptr: *const u8, name_len: usize, value: PluginValue) {
    let name = unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(name_ptr, name_len))
    };
    let lib_name = CURRENT_LIB_NAME.with(|cell| cell.borrow().clone());
    let Some(lib_name) = lib_name else {
        panic!("host_define called without a current library (call register_bridge first)");
    };
    let host_val = unsafe { plugin_to_host(value) };
    PENDING_DEFINES.with(|pd| {
        pd.borrow_mut().push(PendingDefine {
            name: name.to_string(),
            lib_name,
            value: host_val,
        });
    });
}

unsafe extern "C" fn host_lookup(
    module_ptr: *const u8,
    module_len: usize,
    name_ptr: *const u8,
    name_len: usize,
) -> PluginValue {
    let module_str = unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(module_ptr, module_len))
    };
    let name_str = unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(name_ptr, name_len))
    };

    let rt = LOADING_RUNTIME.with(|cell| cell.borrow().clone());
    let Some(rt) = rt else {
        panic!("host_lookup called outside of plugin loading context");
    };

    let lib_name = match LibraryName::from_str(module_str, None) {
        Ok(ln) => ln,
        Err(_) => return PluginValue::undefined(),
    };

    let registry = rt.get_registry();
    let registry_inner = registry.0.read();
    let Some(lib) = registry_inner.libs.get(&lib_name.name) else {
        return PluginValue::undefined();
    };

    let sym = Symbol::intern(name_str);
    let lib_inner = lib.0.read();
    let Some(export) = lib_inner.exports.get(&sym) else {
        return PluginValue::undefined();
    };

    let binding = export.binding;
    drop(lib_inner);
    drop(registry_inner);

    let top_level = TOP_LEVEL_BINDINGS.lock();
    match top_level.get(&binding) {
        Some(TopLevelBinding::Global(global)) => {
            let val = global.read();
            unsafe { host_to_plugin(val) }
        }
        _ => PluginValue::undefined(),
    }
}

// ── Table construction ─────────────────────────────────────────────────────

pub fn build_host_fn_table() -> &'static HostFnTable {
    let table = HostFnTable {
        make_integer: host_make_integer,
        make_float: host_make_float,
        make_string: host_make_string,
        make_symbol: host_make_symbol,
        cons: host_cons,
        make_vector: host_make_vector,
        to_integer: host_to_integer,
        to_float: host_to_float,
        to_string_copy: host_to_string_copy,
        car: host_car,
        cdr: host_cdr,
        call: host_call,
        apply_continuation: host_apply_continuation,
        make_application: host_make_application,
        raise_error: host_raise_error,
        register_type: host_register_type,
        make_foreign: host_make_foreign,
        get_foreign: host_get_foreign,
        register_bridge: host_register_bridge,
        value_retain: host_value_retain,
        value_release: host_value_release,
        define: host_define,
        lookup: host_lookup,
    };
    Box::leak(Box::new(table))
}
