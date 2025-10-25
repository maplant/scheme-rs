//! Input and Output handling

use rustyline::{Editor, error::ReadlineError};
use std::{
    pin::Pin,
    sync::{Arc, LazyLock},
};
use tokio::{
    io::{self, AsyncRead, AsyncReadExt},
    sync::{
        MappedMutexGuard, Mutex, MutexGuard,
        mpsc::{Receiver, Sender},
    },
    task::JoinHandle,
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
    byte_decoder: Utf8Buffer,
    buff: Vec<char>,
}

type Reader = Pin<Box<dyn AsyncRead + Send + Sync + 'static>>;

impl CharBuffer {
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
    pub fn from_reader(reader: impl AsyncRead + Send + Sync + 'static) -> Self {
        Self::Reader {
            buffer: CharBuffer::default(),
            reader: Box::pin(reader),
        }
    }

    pub async fn read_char(&mut self) -> Result<char, ReadError> {
        match self {
            Self::Reader { buffer, reader } => {
                let chr = buffer.peekn(0, reader).await?;
                buffer.skip(1);
                Ok(chr)
            }
            Self::Prompt { prompt } => prompt.read_char().await,
        }
    }

    pub async fn peekn(&mut self, idx: usize) -> Result<char, ReadError> {
        match self {
            Self::Reader { buffer, reader } => buffer.peekn(idx, reader).await,
            Self::Prompt { prompt } => prompt.peekn(idx).await,
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

#[non_exhaustive]
pub(crate) enum PortInner {
    InputPort(InputPort),
}

pub struct Port {
    pub(crate) inner: Arc<Mutex<PortInner>>,
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
    pub fn from_reader(reader: impl AsyncRead + Send + Sync + 'static) -> Self {
        Self {
            inner: Arc::new(Mutex::new(PortInner::InputPort(InputPort::from_reader(
                reader,
            )))),
        }
    }

    pub fn from_prompt(editor: impl Readline) -> Self {
        Self {
            inner: Arc::new(Mutex::new(PortInner::InputPort(InputPort::Prompt {
                prompt: Prompt::new(editor),
            }))),
        }
    }

    pub async fn try_lock_input_port(&self) -> Option<MappedMutexGuard<'_, InputPort>> {
        MutexGuard::try_map(self.inner.lock().await, |port| match port {
            PortInner::InputPort(input) => Some(input),
        })
        .ok()
    }
}

pub struct Prompt {
    buffer: Vec<char>,
    pos: usize,
    editor: Arc<std::sync::Mutex<dyn Readline>>,
}

impl Prompt {
    fn new(editor: impl Readline) -> Self {
        let editor = Arc::new(std::sync::Mutex::new(editor));
        Self {
            buffer: Vec::new(),
            pos: 0,
            editor,
        }
    }

    async fn read_char(&mut self) -> Result<char, ReadError> {
        let chr = self.peekn(0).await?;
        self.pos += 1;
        Ok(chr)
    }

    async fn peekn(&mut self, idx: usize) -> Result<char, ReadError> {
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

struct PromptTask {
    tx: Sender<InputRequest>,
    _task: JoinHandle<()>,
}

static PROMPT_TASK: LazyLock<PromptTask> = LazyLock::new(|| {
    let (tx, rx) = tokio::sync::mpsc::channel(1);
    let _task = tokio::spawn(async move { prompt(rx).await });
    PromptTask { tx, _task }
});

async fn prompt(mut rx: Receiver<InputRequest>) {
    while let Some(InputRequest { prompt, editor, tx }) = rx.recv().await {
        let input = tokio::task::spawn_blocking(move || editor.lock().unwrap().readline(&prompt))
            .await
            .unwrap();
        if tx.send(input).is_err() {
            panic!("Failed to send prompt");
        }
    }
}
