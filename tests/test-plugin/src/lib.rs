use scheme_rs::exceptions::Exception;
use scheme_rs::registry::bridge;
use scheme_rs::value::Value;

#[bridge(name = "test-plugin-add", lib = "(test plugin)")]
fn test_plugin_add(a: i64, b: i64) -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from(a + b)])
}

#[bridge(name = "test-plugin-greeting", lib = "(test plugin)")]
fn test_plugin_greeting() -> Result<Vec<Value>, Exception> {
    Ok(vec![Value::from("hello from plugin".to_string())])
}
