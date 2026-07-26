use std::ffi::c_void;
use std::fmt;
use std::marker::PhantomData;

use crate::value::Value;

// ── Opaque handles ─────────────────────────────────────────────────────────

#[repr(transparent)]
pub struct Continuation(Value);

impl Continuation {
    pub(crate) fn from_value(v: Value) -> Self {
        Self(v)
    }

    /// Construct a `Continuation` from a raw plugin `Value`.
    ///
    /// # Safety
    /// The caller must ensure `v` is a valid continuation value received
    /// from the host (i.e. the `k` pointer in a `CpsBridgeFn`).
    pub unsafe fn from_raw(v: *const Value) -> Self {
        Self(unsafe { (*v).clone() })
    }

    pub fn as_value(&self) -> &Value {
        &self.0
    }
}

#[repr(C)]
pub struct Barrier {
    ptr: *mut c_void,
    _not_send: PhantomData<*mut ()>,
}

impl Barrier {
    /// Construct a `Barrier` from a raw pointer.
    ///
    /// # Safety
    /// The caller must ensure `ptr` is a valid barrier pointer received
    /// from the host (i.e. the barrier argument in a `CpsBridgeFn`).
    pub unsafe fn from_raw(ptr: *mut c_void) -> Self {
        Self {
            ptr,
            _not_send: PhantomData,
        }
    }

    pub(crate) fn as_ptr(&self) -> *mut c_void {
        self.ptr
    }
}

#[repr(transparent)]
pub struct ApplicationResult(*mut c_void);

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeHandle(usize);

impl TypeHandle {
    pub(crate) fn from_raw(raw: usize) -> Self {
        Self(raw)
    }

    pub fn as_raw(&self) -> usize {
        self.0
    }
}

// ── Error type ─────────────────────────────────────────────────────────────

pub struct PluginError {
    message: String,
}

impl PluginError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }

    pub fn type_error(msg: impl Into<String>) -> Self {
        Self {
            message: format!("type error: {}", msg.into()),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for PluginError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl fmt::Debug for PluginError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PluginError")
            .field("message", &self.message)
            .finish()
    }
}

impl std::error::Error for PluginError {}

impl From<std::io::Error> for PluginError {
    fn from(e: std::io::Error) -> Self {
        Self::new(e.to_string())
    }
}

impl From<fmt::Error> for PluginError {
    fn from(e: fmt::Error) -> Self {
        Self::new(e.to_string())
    }
}

// ── Bridge function types ──────────────────────────────────────────────────

pub type SimpleBridgeFn = unsafe extern "C" fn(*const Value, usize) -> BridgeReturn;

pub type CpsBridgeFn =
    unsafe extern "C" fn(*const Value, *const Value, usize, *mut c_void) -> ApplicationResult;

#[repr(C)]
pub struct BridgeReturn {
    pub value: Value,
    pub error: *const u8,
    pub error_len: usize,
}

impl BridgeReturn {
    pub fn ok(value: Value) -> Self {
        Self {
            value,
            error: std::ptr::null(),
            error_len: 0,
        }
    }

    pub fn err(msg: &str) -> Self {
        let leaked = Box::leak(msg.to_owned().into_boxed_str());
        Self {
            value: Value::undefined(),
            error: leaked.as_ptr(),
            error_len: leaked.len(),
        }
    }
}

// ── Bridge registration ────────────────────────────────────────────────────

#[repr(C)]
pub struct BridgeSpec {
    pub name_ptr: *const u8,
    pub name_len: usize,
    pub lib_ptr: *const u8,
    pub lib_len: usize,
    pub num_args: usize,
    pub variadic: bool,
    pub func: Option<SimpleBridgeFn>,
    pub cps_func: Option<CpsBridgeFn>,
    pub blocking: bool,
}
