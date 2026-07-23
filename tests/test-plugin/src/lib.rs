use std::ffi::c_void;
use std::sync::OnceLock;

use scheme_rs_plugin_api::*;

#[plugin_bridge(name = "test-plugin-add", lib = "(test plugin)")]
fn add(a: i64, b: i64) -> Result<i64, PluginError> {
    Ok(a + b)
}

#[plugin_bridge(name = "test-plugin-greeting", lib = "(test plugin)")]
fn greeting() -> Result<String, PluginError> {
    Ok("hello from plugin".to_string())
}

struct Counter {
    count: i64,
}

static COUNTER_TYPE: OnceLock<TypeHandle> = OnceLock::new();

#[plugin_bridge(name = "make-counter", lib = "(test plugin)")]
fn make_counter(initial: i64) -> Result<Value, PluginError> {
    let counter = Box::new(Counter { count: initial });
    let handle = *COUNTER_TYPE.get().unwrap();
    Ok(make_foreign(handle, Box::into_raw(counter) as *mut c_void))
}

#[plugin_bridge(name = "counter-value", lib = "(test plugin)")]
fn counter_value(v: Value) -> Result<i64, PluginError> {
    let handle = *COUNTER_TYPE.get().unwrap();
    let ptr = get_foreign(handle, &v);
    if ptr.is_null() {
        return Err(PluginError::type_error("expected counter"));
    }
    let counter = unsafe { &*(ptr as *const Counter) };
    Ok(counter.count)
}

#[unsafe(no_mangle)]
pub extern "C" fn scheme_rs_plugin_abi_version() -> u32 {
    1
}

#[unsafe(no_mangle)]
pub extern "C" fn scheme_rs_plugin_init(host_table: *const ()) {
    unsafe { scheme_rs_plugin_api::host::_init_host(host_table) };

    COUNTER_TYPE.set(register_type("counter", None)).ok();

    register_bridge(&__plugin_bridge_spec_add());
    register_bridge(&__plugin_bridge_spec_greeting());
    register_bridge(&__plugin_bridge_spec_make_counter());
    register_bridge(&__plugin_bridge_spec_counter_value());
}
