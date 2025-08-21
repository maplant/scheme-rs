/// Implements a string builder, in its api similar to
/// [strings.Builder](https://pkg.go.dev/strings#Builder)
#[derive(Default)]
pub struct Builder {
    buffer: Vec<u8>,
}

impl Builder {
    pub fn new() -> Self {
        Self::with_capacity(64)
    }

    pub fn with_capacity(cap: usize) -> Self {
        Builder {
            buffer: Vec::with_capacity(cap),
        }
    }

    pub fn write_char(&mut self, char: char) {
        self.buffer.push(char as u8);
    }

    pub fn write_byte(&mut self, byte: u8) {
        self.buffer.push(byte);
    }

    pub fn write_str(&mut self, str: &str) {
        self.buffer.append(&mut str.as_bytes().to_vec());
    }

    pub fn write_string(&mut self, string: String) {
        self.buffer.append(&mut string.into_bytes())
    }

    pub fn write_buf(&mut self, buf: Vec<u8>) {
        let mut b = buf;
        self.buffer.append(&mut b)
    }

    // This is slow, but probably fine for fancy error reporting, DO NOT USE in any hotpaths
    pub fn write_int<T: std::fmt::Display>(&mut self, i: T) {
        self.write_string(i.to_string());
    }

    pub fn reset(&mut self) {
        self.buffer.clear();
    }

    pub fn to_string(self) -> String {
        match String::from_utf8(self.buffer) {
            Ok(string) => string,
            Err(_) => String::from("<failed to stringify Builder::buffer>"),
        }
    }
}

impl std::fmt::Display for Builder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = std::str::from_utf8(&self.buffer).map_err(|_| std::fmt::Error)?;
        write!(f, "{s}")
    }
}
