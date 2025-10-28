//! Input and Output handling

use rustyline::{Editor, error::ReadlineError};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::sync::Arc;

#[cfg(not(feature = "async"))]
use std::{io::Read, sync::Mutex};

#[cfg(feature = "tokio")]
use tokio::{
    io::{self, AsyncRead, AsyncReadExt},
    sync::Mutex,
};

#[derive(Default)]
pub struct Utf8Buffer {
    buff: [u8; 4],
    len: u8,
}

impl Utf8Buffer {
    fn push_and_decode(&mut self, byte: u8) -> Option<char> {
        self.buff[self.len as usize] = byte;
        match str::from_utf8(&self.buff[..(self.len as usize + 1)]) {
            Ok(s) => {
                self.len = 0;
                s.chars().next()
            }
            Err(err) if err.error_len().is_none() => {
                self.len += 1;
                None
            }
            Err(_) => {
                self.len = 0;
                Some('\u{FFFD}')
            }
        }
    }
}

// This is pretty poorly optimized, but we'll get to that eventually.
#[derive(Default)]
pub struct CharBuffer {
    buff: Vec<char>,
    byte_decoder: Utf8Buffer,
}

#[cfg(feature = "async")]
type Reader = std::pin::Pin<Box<dyn AsyncRead + Send + Sync + 'static>>;

#[cfg(not(feature = "async"))]
type Reader = Box<dyn Read + Send + Sync + 'static>;

impl CharBuffer {
    #[cfg(not(feature = "async"))]
    pub fn peekn(&mut self, idx: usize, reader: &mut Reader) -> Result<char, ReadError> {
        while self.buff.len() <= idx {
            let mut buff = [0_u8; 1024];
            let num_read = reader.read(&mut buff[..]).map_err(ReadError::Io)?;
            if num_read == 0 {
                return Err(ReadError::Eof);
            }
            for byte in buff.into_iter().take(num_read) {
                if let Some(next_chr) = self.byte_decoder.push_and_decode(byte) {
                    self.buff.push(next_chr);
                }
            }
        }

        Ok(self.buff[idx])
    }

    #[cfg(feature = "async")]
    pub async fn peekn(&mut self, idx: usize, reader: &mut Reader) -> Result<char, ReadError> {
        while self.buff.len() <= idx {
            let next_byte = match reader.read_u8().await {
                Ok(chr) => chr,
                Err(err) if err.kind() == io::ErrorKind::UnexpectedEof => {
                    return Err(ReadError::Eof);
                }
                Err(err) => return Err(ReadError::Io(err)),
            };
            if let Some(next_chr) = self.byte_decoder.push_and_decode(next_byte) {
                self.buff.push(next_chr);
            }
        }

        Ok(self.buff[idx])
    }

    pub fn skip(&mut self, n: usize) {
        self.buff = self.buff.split_off(n);
    }
}

pub enum InputPort {
    Reader { buffer: CharBuffer, reader: Reader },
    Prompt { prompt: Prompt },
}

impl InputPort {
    /*
    pub fn from_reader(reader: impl AsyncRead + Send + Sync + 'static) -> Self {
        Self::Reader {
            buffer: CharBuffer::default(),
            reader: Box::pin(reader),
        }
    }
    */

    #[maybe_async]
    pub fn read_char(&mut self) -> Result<char, ReadError> {
        match self {
            Self::Reader { buffer, reader } => {
                let chr = maybe_await!(buffer.peekn(0, reader))?;
                buffer.skip(1);
                Ok(chr)
            }
            Self::Prompt { prompt } => maybe_await!(prompt.read_char()),
        }
    }

    #[maybe_async]
    pub fn peekn(&mut self, idx: usize) -> Result<char, ReadError> {
        match self {
            Self::Reader { buffer, reader } => maybe_await!(buffer.peekn(idx, reader)),
            Self::Prompt { prompt } => maybe_await!(prompt.peekn(idx)),
        }
    }
}

#[derive(Debug)]
pub enum ReadError {
    /// End of file reached
    Eof,
    /// IO error
    Io(std::io::Error),
    /// Rustyline error
    Prompt,
}

pub struct Port {
    pub(crate) input_port: Option<Arc<Mutex<InputPort>>>,
}

impl Port {
    // TODO: Error handling
    /*
    pub async fn read_file(file: &Path) -> Self {
        let name = file.file_name().unwrap().to_string_lossy().into_owned();
        let reader = BufReader::new(File::open(file).await.unwrap());
        Self {
            name,
            inner: Arc::new(Mutex::new(PortInner::InputPort(InputPort::from_reader(
                reader,
            )))),
        }
    }
    */

    #[cfg(not(feature = "async"))]
    pub fn from_reader(reader: impl Read + Sync + Send + 'static) -> Self {
        Self {
            input_port: Some(Arc::new(Mutex::new(InputPort::Reader {
                buffer: CharBuffer::default(),
                reader: Box::new(reader),
            }))),
        }
    }

    #[cfg(feature = "async")]
    pub fn from_reader(reader: impl AsyncRead + Sync + Send + 'static) -> Self {
        Self {
            input_port: Some(Arc::new(Mutex::new(InputPort::Reader {
                buffer: CharBuffer::default(),
                reader: Box::pin(reader),
            }))),
        }
    }

    pub fn from_prompt(editor: impl Readline) -> Self {
        Self {
            input_port: Some(Arc::new(Mutex::new(InputPort::Prompt {
                prompt: Prompt::new(editor),
            }))),
        }
    }

    pub fn get_input_port(&self) -> Option<Arc<Mutex<InputPort>>> {
        self.input_port.clone()
    }

    /*
    #[cfg(feature = "async")]
    pub async fn try_lock_input_port(&self) -> Option<MutexGuard<'_, InputPort>> {
        if let Some(input_port) = self.input_port {
            Some(input_port.lock().await)
        } else {
            None
        }
    }
    */
}

pub trait Readline: Send + 'static {
    fn readline(&mut self, prompt: &str) -> rustyline::Result<String>;
}

impl<H, I> Readline for Editor<H, I>
where
    H: rustyline::Helper + Send + 'static,
    I: rustyline::history::History + Send + 'static,
{
    fn readline(&mut self, prompt: &str) -> rustyline::Result<String> {
        self.readline(prompt)
    }
}

#[cfg(not(feature = "async"))]
mod prompt {
    use super::*;

    pub struct Prompt {
        buffer: Vec<char>,
        pos: usize,
        editor: Box<dyn Readline>,
    }

    impl Prompt {
        pub(super) fn new(editor: impl Readline) -> Self {
            Self {
                buffer: Vec::new(),
                pos: 0,
                editor: Box::new(editor),
            }
        }

        pub(super) fn read_char(&mut self) -> Result<char, ReadError> {
            let chr = self.peekn(0)?;
            self.pos += 1;
            Ok(chr)
        }

        pub(super) fn peekn(&mut self, idx: usize) -> Result<char, ReadError> {
            while self.pos + idx >= self.buffer.len() {
                self.buffer.extend(
                    self.editor
                        .readline("> ")
                        .map_err(|err| match err {
                            ReadlineError::Eof => ReadError::Eof,
                            ReadlineError::Io(io) => ReadError::Io(io),
                            _ => ReadError::Prompt,
                        })?
                        .chars(),
                );
            }
            let chr = self.buffer[self.pos + idx];
            Ok(chr)
        }
    }
}

#[cfg(feature = "tokio")]
mod prompt {
    use super::*;
    use tokio::{
        sync::mpsc::{Receiver, Sender},
        task::JoinHandle,
    };

    pub struct Prompt {
        buffer: Vec<char>,
        pos: usize,
        editor: Arc<std::sync::Mutex<dyn Readline>>,
    }

    impl Prompt {
        pub(super) fn new(editor: impl Readline) -> Self {
            let editor = Arc::new(std::sync::Mutex::new(editor));
            Self {
                buffer: Vec::new(),
                pos: 0,
                editor,
            }
        }

        pub(super) async fn read_char(&mut self) -> Result<char, ReadError> {
            let chr = self.peekn(0).await?;
            self.pos += 1;
            Ok(chr)
        }

        pub(super) async fn peekn(&mut self, idx: usize) -> Result<char, ReadError> {
            while self.pos + idx >= self.buffer.len() {
                /*

                // Some garbage collection:
                if self.pos > self.buffer.len() {
                    self.pos -= self.buffer.len();
                    self.buffer.clear();
                }

                 */
                let (tx, rx) = tokio::sync::oneshot::channel();
                PROMPT_TASK
                    .tx
                    .send(InputRequest {
                        prompt: "> ".to_string(),
                        editor: self.editor.clone(),
                        tx,
                    })
                    .await
                    .unwrap();
                self.buffer.extend(
                    rx.await
                        .unwrap()
                        .map_err(|err| match err {
                            ReadlineError::Eof => ReadError::Eof,
                            ReadlineError::Io(io) => ReadError::Io(io),
                            _ => ReadError::Prompt,
                        })?
                        .chars(),
                );
                self.buffer.push('\n');
                self.pos = 0;
            }
            let chr = self.buffer[self.pos + idx];
            Ok(chr)
        }
    }

    pub struct InputRequest {
        prompt: String,
        editor: Arc<std::sync::Mutex<dyn Readline>>,
        tx: tokio::sync::oneshot::Sender<rustyline::Result<String>>,
    }

    struct PromptTask {
        tx: Sender<InputRequest>,
        _task: JoinHandle<()>,
    }

    static PROMPT_TASK: std::sync::LazyLock<PromptTask> = std::sync::LazyLock::new(|| {
        let (tx, rx) = tokio::sync::mpsc::channel(1);
        let _task = tokio::spawn(async move { prompt(rx).await });
        PromptTask { tx, _task }
    });

    async fn prompt(mut rx: Receiver<InputRequest>) {
        while let Some(InputRequest { prompt, editor, tx }) = rx.recv().await {
            let input =
                tokio::task::spawn_blocking(move || editor.lock().unwrap().readline(&prompt))
                    .await
                    .unwrap();
            if tx.send(input).is_err() {
                panic!("Failed to send prompt");
            }
        }
    }
}

use prompt::*;
