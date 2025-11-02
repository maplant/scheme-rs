//! Input and Output handling

use rustyline::{Editor, error::ReadlineError};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::{any::Any, io::{Seek, SeekFrom, Write}, sync::Arc};

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

/*
pub enum InputPort {
    Reader { buffer: CharBuffer, reader: Reader },
    Prompt { prompt: Prompt },
}
*/

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
}

pub enum WriteError {
    Io(std::io::Error),
}

pub enum SeekError {
    Io(std::io::Error),
}

#[cfg(not(feature = "async"))]
pub type ReadFn = fn(&mut dyn Any, &mut [u8]) -> Result<usize, ReadError>;
#[cfg(not(feature = "async"))]
pub type WriteFn = fn(&mut dyn Any, &[u8]) -> Result<usize, WriteError>;
#[cfg(not(feature = "async"))]
pub type SeekFn = fn(&mut dyn Any, u64) -> Result<u64, SeekError>;

#[cfg(feature = "async")]
pub type ReadFn =
    for<'a> fn(&'a mut dyn Any, &'a mut [u8]) -> BoxFuture<'a, Result<usize, ReadError>>;
#[cfg(feature = "async")]
pub type WriteFn =
    for<'a> fn(&'a mut dyn Any, &'a [u8]) -> BoxFuture<'a, Result<usize, WriteError>>;

struct PortInner {
    port: Arc<Mutex<dyn Any + Send + Sync + 'static>>,
    read: Option<ReadFn>,
    write: Option<WriteFn>,
    seek: Option<SeekFn>,
}

fn read_fn<T>() -> ReadFn
where
    T: Read + Any + Sync + Send + 'static,
{
    |any, buff| {
        let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
        concrete.read(buff).map_err(ReadError::Io)
    }
}

fn write_fn<T>() -> WriteFn
where
    T: Write + Any + Sync + Send + 'static,
{
    |any, buff| {
        let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
        concrete.write(buff).map_err(WriteError::Io)
    }
}

fn seek_fn<T>() -> SeekFn
where
    T: Seek + Any + Sync + Send + 'static,
{
    |any, pos| {
        let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
        concrete.seek(SeekFrom::Start(pos)).map_err(SeekError::Io)
    }
}

impl PortInner {
    fn new<P>(port: P) -> Self
    where
        P: IntoPort,
    {
        Self {
            port: port.into_port(),
            read: P::read_fn(),
            write: P::write_fn(),
            seek: P::seek_fn(),
        }
    }
    
    #[cfg(not(feature = "async"))]
    fn lock_port<R>(
        &self,
        callback: impl FnOnce(&mut dyn Any, Option<ReadFn>, Option<WriteFn>) -> R,
    ) -> R {
        let mut locked_port = self.port.lock().unwrap();
        callback(&mut *locked_port, self.read, self.write)
    }

    #[cfg(feature = "async")]
    async fn lock_port<R>(
        &self,
        callback: impl AsyncFnOnce(&mut dyn Any, Option<ReaderFn>, Option<WriterFn>) -> R,
    ) -> R {
        let mut locked_port = self.port.lock().unwrap();
        callback(&mut *locked_port, self.reader, self.writer).await
    }
}

pub trait IntoPort: Any + Send + Sync + 'static + Sized {
    fn into_port(self) -> Arc<Mutex<dyn Any + Send + Sync + 'static>> {
        Arc::new(Mutex::new(self))
    }

    fn read_fn() -> Option<ReadFn> {
        None
    }

    fn write_fn() -> Option<WriteFn> {
        None
    }

    fn seek_fn() -> Option<SeekFn> {
        None
    }
}

pub struct Port(pub(crate) Arc<PortInner>);

impl Port {
    /*
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
    */
}

/*
pub trait Readline: Send + Sync + 'static {
    fn readline(&mut self, prompt: &str) -> rustyline::Result<String>;
}

impl<H, I> Readline for Editor<H, I>
where
    H: rustyline::Helper + Send + Sync + 'static,
    I: rustyline::history::History + Send + Sync + 'static,
{
    fn readline(&mut self, prompt: &str) -> rustyline::Result<String> {
        self.readline(prompt)
    }
}
*/

#[cfg(not(feature = "async"))]
mod prompt {
    use super::*;

    pub struct Prompt<H, I>
    where
        H: rustyline::Helper + Send + Sync + 'static,
        I: rustyline::history::History + Send + Sync + 'static,
    {
        leftover: Vec<u8>,
        editor: Editor<H, I>,
    }

    impl<H, I> Prompt<H, I>
    where
        H: rustyline::Helper + Send + Sync + 'static,
        I: rustyline::history::History + Send + Sync + 'static,
    {
        pub(super) fn new(editor: Editor<H, I>) -> Self {
            Self {
                leftover: Vec::new(),
                editor,
            }
        }

        /*
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
                            _ => todo!(),
                        })?
                        .chars(),
                );
            }
            let chr = self.buffer[self.pos + idx];
            Ok(chr)
        }
        */
    }

    impl<H, I> IntoPort for Prompt<H, I>
    where
        H: rustyline::Helper + Send + Sync + 'static,
        I: rustyline::history::History + Send + Sync + 'static,
    {
        fn read_fn() -> Option<ReadFn> {
            Some(|any, buff| {
                let concrete = unsafe { any.downcast_mut::<Self>().unwrap_unchecked() };
                let input = concrete.editor.readline("> ");
                todo!()
            })
        }
    }
}

pub enum Codec {
    Latin1,
    Utf8,
    Utf16,
}

pub enum EolStyle {
    /// Linefeed
    Lf,
    /// Carriage return
    Cr,
    /// Carriage return linefeed
    Crlf,
    /// Next line
    Nel,
    /// Carriage return next line
    Crnel,
    /// Line separator
    Ls
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
