use std::ffi::c_void;
use std::sync::OnceLock;

use crate::types::{
    ApplicationResult, Barrier, BridgeSpec, Continuation, PluginError, TypeHandle,
};
use crate::value::Value;

// ── Host function table ────────────────────────────────────────────────────

#[repr(C)]
pub struct HostFnTable {
    // Value construction
    pub make_integer: unsafe extern "C" fn(i64) -> Value,
    pub make_float: unsafe extern "C" fn(f64) -> Value,
    pub make_string: unsafe extern "C" fn(*const u8, usize) -> Value,
    pub make_symbol: unsafe extern "C" fn(*const u8, usize) -> Value,
    pub cons: unsafe extern "C" fn(Value, Value) -> Value,
    pub make_vector: unsafe extern "C" fn(*const Value, usize) -> Value,

    // Extraction
    pub to_integer: unsafe extern "C" fn(*const Value, *mut i64) -> bool,
    pub to_float: unsafe extern "C" fn(*const Value) -> f64,
    pub to_string_copy: unsafe extern "C" fn(*const Value, *mut *mut u8, *mut usize),

    // Pair ops
    pub car: unsafe extern "C" fn(*const Value) -> Value,
    pub cdr: unsafe extern "C" fn(*const Value) -> Value,

    // Procedure calls
    pub call:
        unsafe extern "C" fn(*const Value, *const Value, usize, *mut *mut u8, *mut usize) -> Value,

    // CPS primitives
    pub apply_continuation:
        unsafe extern "C" fn(*const Value, *const Value, usize) -> ApplicationResult,
    pub make_application:
        unsafe extern "C" fn(*const Value, *const Value, *const Value, usize) -> ApplicationResult,
    pub raise_error: unsafe extern "C" fn(*const u8, usize, *mut c_void) -> ApplicationResult,

    // Foreign types
    pub register_type:
        unsafe extern "C" fn(*const u8, usize, Option<unsafe extern "C" fn(*mut c_void)>) -> usize,
    pub make_foreign: unsafe extern "C" fn(usize, *mut c_void) -> Value,
    pub get_foreign: unsafe extern "C" fn(usize, *const Value) -> *mut c_void,

    // Bridge registration
    pub register_bridge: unsafe extern "C" fn(*const BridgeSpec) -> bool,

    // Refcounting
    pub value_retain: unsafe extern "C" fn(usize),
    pub value_release: unsafe extern "C" fn(usize),

    // Module interaction
    pub define: unsafe extern "C" fn(*const u8, usize, Value),
    pub lookup: unsafe extern "C" fn(*const u8, usize, *const u8, usize) -> Value,
}

// ── Global state ───────────────────────────────────────────────────────────

static HOST: OnceLock<&'static HostFnTable> = OnceLock::new();

pub(crate) fn host() -> &'static HostFnTable {
    HOST.get()
        .copied()
        .expect("host function table not initialized")
}

/// Initialize the host function table. Called from generated plugin init code.
///
/// # Safety
/// `table` must point to a valid `HostFnTable` with `'static` lifetime.
pub unsafe fn _init_host(table: *const ()) {
    let table_ref: &'static HostFnTable = unsafe { &*(table as *const HostFnTable) };

    HOST.set(table_ref).ok();

    crate::value::HOST
        .set(crate::value::HostFns {
            retain: table_ref.value_retain,
            release: table_ref.value_release,
        })
        .ok();
}

// ── Public wrapper functions ───────────────────────────────────────────────

pub fn make_integer(n: i64) -> Value {
    unsafe { (host().make_integer)(n) }
}

pub fn make_float(n: f64) -> Value {
    unsafe { (host().make_float)(n) }
}

pub fn make_string(s: &str) -> Value {
    unsafe { (host().make_string)(s.as_ptr(), s.len()) }
}

pub fn make_symbol(s: &str) -> Value {
    unsafe { (host().make_symbol)(s.as_ptr(), s.len()) }
}

pub fn to_integer(v: &Value) -> Option<i64> {
    let mut out: i64 = 0;
    let ok = unsafe { (host().to_integer)(v, &mut out) };
    if ok { Some(out) } else { None }
}

pub fn to_float(v: &Value) -> f64 {
    unsafe { (host().to_float)(v) }
}

pub fn to_string_copy(v: &Value) -> Option<String> {
    let mut ptr: *mut u8 = std::ptr::null_mut();
    let mut len: usize = 0;
    unsafe {
        (host().to_string_copy)(v, &mut ptr, &mut len);
        if ptr.is_null() {
            return None;
        }
        let slice = std::slice::from_raw_parts_mut(ptr, len);
        let s = String::from_utf8_lossy(slice).into_owned();
        drop(Box::from_raw(slice));
        Some(s)
    }
}

pub fn cons(car: Value, cdr: Value) -> Value {
    unsafe { (host().cons)(car, cdr) }
}

pub fn car(pair: &Value) -> Value {
    unsafe { (host().car)(pair) }
}

pub fn cdr(pair: &Value) -> Value {
    unsafe { (host().cdr)(pair) }
}

pub fn call(proc: &Value, args: &[Value]) -> Result<Value, PluginError> {
    let mut err_ptr: *mut u8 = std::ptr::null_mut();
    let mut err_len: usize = 0;
    let result =
        unsafe { (host().call)(proc, args.as_ptr(), args.len(), &mut err_ptr, &mut err_len) };
    if err_ptr.is_null() {
        Ok(result)
    } else {
        let msg = unsafe {
            let slice = std::slice::from_raw_parts(err_ptr, err_len);
            let s = String::from_utf8_lossy(slice).into_owned();
            drop(Box::from_raw(std::slice::from_raw_parts_mut(
                err_ptr, err_len,
            )));
            s
        };
        Err(PluginError::new(msg))
    }
}

pub fn apply_continuation(k: &Continuation, args: &[Value]) -> ApplicationResult {
    unsafe { (host().apply_continuation)(k.as_value(), args.as_ptr(), args.len()) }
}

pub fn make_application(
    proc: &Value,
    k: &Continuation,
    args: &[Value],
) -> ApplicationResult {
    unsafe { (host().make_application)(proc, k.as_value(), args.as_ptr(), args.len()) }
}

pub fn raise_error(msg: &str, barrier: &Barrier) -> ApplicationResult {
    unsafe { (host().raise_error)(msg.as_ptr(), msg.len(), barrier.as_ptr()) }
}

pub fn register_type(
    name: &str,
    finalizer: Option<unsafe extern "C" fn(*mut c_void)>,
) -> TypeHandle {
    let raw = unsafe { (host().register_type)(name.as_ptr(), name.len(), finalizer) };
    TypeHandle::from_raw(raw)
}

pub fn make_foreign(handle: TypeHandle, data: *mut c_void) -> Value {
    unsafe { (host().make_foreign)(handle.as_raw(), data) }
}

pub fn get_foreign(handle: TypeHandle, value: &Value) -> *mut c_void {
    unsafe { (host().get_foreign)(handle.as_raw(), value) }
}

pub fn define(name: &str, value: Value) {
    unsafe { (host().define)(name.as_ptr(), name.len(), value) }
}

pub fn lookup(module: &str, name: &str) -> Value {
    unsafe {
        (host().lookup)(
            module.as_ptr(),
            module.len(),
            name.as_ptr(),
            name.len(),
        )
    }
}

pub fn register_bridge(spec: &BridgeSpec) -> bool {
    unsafe { (host().register_bridge)(spec) }
}
