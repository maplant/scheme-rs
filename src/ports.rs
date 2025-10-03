//! Input and Output handling

use rustyline::Editor;
use std::{
    io::Cursor,
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

// use crate::gc::Trace;

pub struct Utf8Buffer {
    buff: [u8; 4],
    len: u8,
}

impl Utf8Buffer {
    fn push_and_decode(&mut self, byte: u8) -> Option<char> {
        self.buff[self.len as usize] = byte;
        match str::from_utf8(&self.buff[..(self.len as usize)]) {
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
pub struct CharBuffer {
    byte_decoder: Utf8Buffer,
    buff: Vec<char>,
}

type Reader = Pin<Box<dyn AsyncRead + Send + Sync + 'static>>;

impl CharBuffer {
    pub async fn peekn(&mut self, idx: usize, reader: &mut Reader) -> io::Result<char> {
        while self.buff.len() <= idx {
            let next_byte = reader.read_u8().await?;
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
    pub async fn read_char(&mut self) -> io::Result<char> {
        match self {
            Self::Reader { buffer, reader } => {
                let chr = buffer.peekn(0, reader).await?;
                buffer.skip(1);
                Ok(chr)
            }
            Self::Prompt { prompt } => {
                todo!()
            }
        }
    }

    pub async fn peekn(&mut self, idx: usize) -> io::Result<char> {
        match self {
            Self::Reader { buffer, reader } => buffer.peekn(idx, reader).await,
            Self::Prompt { prompt } => {
                todo!()
            }
        }
    }
}

pub(crate) enum PortInner {
    InputPort(InputPort),
    OutputPort(()),
}

pub struct Port(pub(crate) Arc<Mutex<PortInner>>);

impl Port {
    pub async fn try_lock_input_port(&self) -> Option<MappedMutexGuard<'_, InputPort>> {
        MutexGuard::try_map(self.0.lock().await, |port| match port {
            PortInner::InputPort(input) => Some(input),
            _ => None,
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
    async fn read_char(&mut self) -> rustyline::Result<char> {
        if self.pos >= self.buffer.len() {
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
            self.buffer = rx.await.unwrap()?.chars().collect();
            self.pos = 0;
        }
        let chr = self.buffer[self.pos];
        self.pos += 1;
        Ok(chr)
    }
}

pub struct InputRequest {
    prompt: String,
    editor: Arc<std::sync::Mutex<dyn Readline>>,
    tx: tokio::sync::oneshot::Sender<rustyline::Result<String>>,
}

pub trait Readline: Send {
    fn readline(&mut self, prompt: &str) -> rustyline::Result<String>;
}

impl<H, I> Readline for Editor<H, I>
where
    H: rustyline::Helper + Send,
    I: rustyline::history::History + Send,
{
    fn readline(&mut self, prompt: &str) -> rustyline::Result<String> {
        self.readline(prompt)
    }
}

struct PromptTask {
    tx: Sender<InputRequest>,
    task: JoinHandle<()>,
}

static PROMPT_TASK: LazyLock<PromptTask> = LazyLock::new(|| {
    let (tx, rx) = tokio::sync::mpsc::channel(1);
    let task = tokio::spawn(async move { prompt(rx).await });
    PromptTask { tx, task }
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
