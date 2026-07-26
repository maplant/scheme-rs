use crate::host;
use crate::types::PluginError;
use crate::value::Value;

pub trait FromScheme: Sized {
    fn from_scheme(v: &Value) -> Result<Self, PluginError>;
}

pub trait IntoScheme {
    fn into_scheme(self) -> Value;
}

impl FromScheme for i64 {
    fn from_scheme(v: &Value) -> Result<Self, PluginError> {
        host::to_integer(v)
            .ok_or_else(|| PluginError::type_error("expected integer"))
    }
}

impl FromScheme for f64 {
    fn from_scheme(v: &Value) -> Result<Self, PluginError> {
        if !v.is_number() {
            return Err(PluginError::type_error("expected number"));
        }
        Ok(host::to_float(v))
    }
}

impl FromScheme for bool {
    fn from_scheme(v: &Value) -> Result<Self, PluginError> {
        v.to_bool()
            .ok_or_else(|| PluginError::type_error("expected boolean"))
    }
}

impl FromScheme for String {
    fn from_scheme(v: &Value) -> Result<Self, PluginError> {
        host::to_string_copy(v)
            .ok_or_else(|| PluginError::type_error("expected string"))
    }
}

impl FromScheme for Value {
    fn from_scheme(v: &Value) -> Result<Self, PluginError> {
        Ok(v.clone())
    }
}

impl IntoScheme for i64 {
    fn into_scheme(self) -> Value {
        host::make_integer(self)
    }
}

impl IntoScheme for f64 {
    fn into_scheme(self) -> Value {
        host::make_float(self)
    }
}

impl IntoScheme for bool {
    fn into_scheme(self) -> Value {
        Value::from_bool(self)
    }
}

impl IntoScheme for String {
    fn into_scheme(self) -> Value {
        host::make_string(&self)
    }
}

impl IntoScheme for &str {
    fn into_scheme(self) -> Value {
        host::make_string(self)
    }
}

impl IntoScheme for Value {
    fn into_scheme(self) -> Value {
        self
    }
}

impl IntoScheme for () {
    fn into_scheme(self) -> Value {
        Value::undefined()
    }
}
