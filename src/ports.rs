//! Input and Output handling

use either::Either;
use memchr::{memchr, memmem};
use rustyline::{Editor, error::ReadlineError};
use scheme_rs_macros::{maybe_async, maybe_await};
use std::{
    any::Any,
    io::{Cursor, Seek, SeekFrom, Write},
    iter::Peekable,
    slice,
    sync::Arc,
};

#[cfg(not(feature = "async"))]
use std::{
    io::{self, Read},
    sync::Mutex,
};

#[cfg(feature = "tokio")]
use tokio::{
    io::{self, AsyncRead, AsyncReadExt},
    sync::Mutex,
};

#[derive(Clone, Debug)]
pub enum ReadError {
    Io(Arc<io::Error>),
    NotAnInputPort,
}

impl From<io::Error> for ReadError {
    fn from(error: io::Error) -> Self {
        Self::Io(Arc::new(error))
    }
}

#[derive(Clone, Debug)]
pub enum WriteError {
    Io(Arc<io::Error>),
    NotAnOutputPort,
}

impl From<io::Error> for WriteError {
    fn from(error: io::Error) -> Self {
        Self::Io(Arc::new(error))
    }
}

pub struct Utf8Buffer {
    buff: [u8; 4],
    len: u8,
    error_mode: ErrorHandlingMode,
}

impl Utf8Buffer {
    fn new(error_mode: ErrorHandlingMode) -> Self {
        Self {
            buff: [0; 4],
            len: 0,
            error_mode,
        }
    }
}

impl Decode for Utf8Buffer {
    fn push_and_decode(&mut self, byte: u8) -> Result<Option<char>, ReadError> {
        self.buff[self.len as usize] = byte;
        match str::from_utf8(&self.buff[..(self.len as usize + 1)]) {
            Ok(s) => {
                self.len = 0;
                Ok(s.chars().next())
            }
            Err(err) if err.error_len().is_none() => {
                self.len += 1;
                Ok(None)
            }
            Err(_) => {
                self.len = 0;
                match self.error_mode {
                    ErrorHandlingMode::Ignore => Ok(None),
                    ErrorHandlingMode::Replace => Ok(Some('\u{FFFD}')),
                    ErrorHandlingMode::Raise => panic!("raise error here"),
                }
            }
        }
    }
}

pub struct Utf16Buffer {
    buff: [u8; 4],
    len: u8,
    endianness: Endianness,
    error_mode: ErrorHandlingMode,
}

#[derive(Copy, Clone)]
enum Endianness {
    Le,
    Be,
}

impl Utf16Buffer {
    fn new(error_mode: ErrorHandlingMode, endianness: Endianness) -> Self {
        Self {
            buff: [0; 4],
            len: 0,
            endianness,
            error_mode,
        }
    }
}

impl Decode for Utf16Buffer {
    fn push_and_decode(&mut self, byte: u8) -> Result<Option<char>, ReadError> {
        self.buff[self.len as usize] = byte;
        self.len += 1;
        if self.len == 1 || self.len == 3 {
            return Ok(None);
        }

        let chars = char::decode_utf16(self.buff.chunks(2).map(|bytes| {
            let [a, b] = bytes else { unreachable!() };
            match self.endianness {
                Endianness::Le => u16::from_le_bytes([*a, *b]),
                Endianness::Be => u16::from_be_bytes([*a, *b]),
            }
        }))
        .collect::<Vec<_>>();

        match chars.as_slice() {
            [Ok(chr), ..] => {
                self.buff[0] = self.buff[2];
                self.buff[1] = self.buff[3];
                self.len = 0;
                Ok(Some(*chr))
            }
            [Err(_), _, ..] => {
                self.buff[0] = self.buff[2];
                self.buff[1] = self.buff[3];
                self.len = 0;
                match self.error_mode {
                    ErrorHandlingMode::Ignore => Ok(None),
                    ErrorHandlingMode::Replace => Ok(Some('\u{FFFD}')),
                    ErrorHandlingMode::Raise => panic!("raise error here"),
                }
            }
            [Err(_)] => Ok(None),
            [] => unreachable!(),
        }
    }
}

trait Decode {
    fn push_and_decode(&mut self, byte: u8) -> Result<Option<char>, ReadError>;
}

pub struct Decoder<'a, D> {
    port: &'a mut PortInner,
    decode: D,
    pos: Either<usize, ReadError>,
}

impl<'a, D> Decoder<'a, D> {
    fn new(port: &'a mut PortInner, decode: D) -> Self {
        Self {
            port,
            decode,
            pos: Either::Left(0),
        }
    }
}

impl<D> Iterator for Decoder<'_, D>
where
    D: Decode,
{
    type Item = Result<(usize, char), ReadError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut pos = match self.pos {
            Either::Left(pos) => pos,
            Either::Right(ref err) => return Some(Err(err.clone())),
        };
        loop {
            match self
                .port
                .peekn_bytes(pos)
                .transpose()?
                .and_then(|byte| self.decode.push_and_decode(byte))
            {
                Ok(Some(chr)) => {
                    self.pos = Either::Left(pos + 1);
                    return Some(Ok((pos, chr)));
                }
                Ok(None) => {
                    pos += 1;
                    self.pos = Either::Left(pos);
                }
                Err(err) => {
                    self.pos = Either::Right(err.clone());
                    return Some(Err(err));
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum BufferMode {
    None,
    Line,
    Block,
}

impl BufferMode {
    fn new_input_buffer(&self, text: bool, is_input_port: bool) -> Box<[u8]> {
        if !is_input_port {
            return Box::from(Vec::new());
        }
        match self {
            Self::None if text => Box::from(vec![0u8; 4]),
            Self::None => Box::from(vec![0]),
            Self::Line | Self::Block => Box::from(vec![0u8; BUFFER_SIZE]),
        }
    }

    fn new_output_buffer(&self, is_output_port: bool) -> Vec<u8> {
        if !is_output_port {
            return Vec::new();
        }
        match self {
            Self::None => Vec::new(),
            Self::Line | Self::Block => Vec::with_capacity(BUFFER_SIZE),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Transcoder {
    codec: Codec,
    eol_type: EolStyle,
    error_handling_mode: ErrorHandlingMode,
}

impl Transcoder {
    pub fn native() -> Self {
        Self {
            codec: Codec::Utf8,
            eol_type: EolStyle::Lf,
            error_handling_mode: ErrorHandlingMode::Replace,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Codec {
    Latin1,
    Utf8,
    Utf16,
}

impl Codec {
    fn byte_len(&self, chr: char) -> usize {
        match self {
            Self::Latin1 => 1,
            Self::Utf8 => chr.len_utf8(),
            Self::Utf16 => chr.len_utf16(),
        }
    }

    fn ls_needle(&self, utf16_endianness: Option<Endianness>) -> &'static [u8] {
        match self {
            Self::Latin1 => &[],
            Self::Utf8 => "\u{2028}".as_bytes(),
            Self::Utf16 => match utf16_endianness {
                Some(Endianness::Le) => &[0x20, 0x28],
                Some(Endianness::Be) => &[0x28, 0x20],
                None => &[],
            },
        }
    }
}

#[derive(Copy, Clone, Debug)]
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
    Ls,
}

impl EolStyle {
    fn convert_eol_style(
        self,
        mut iter: Peekable<impl Iterator<Item = Result<(usize, char), ReadError>>>,
    ) -> impl Iterator<Item = Result<(usize, char), ReadError>> {
        std::iter::from_fn(move || {
            let next_chr = iter.next()?;
            match (self, next_chr) {
                (Self::Lf, x) => Some(x),
                (Self::Cr, Ok((idx, '\r'))) => Some(Ok((idx, '\n'))),
                (Self::Crlf, Ok((idx, '\r'))) => {
                    if let Some(Ok((idx, '\n'))) = iter.peek() {
                        Some(Ok((*idx, '\n')))
                    } else {
                        Some(Ok((idx, '\r')))
                    }
                }
                (Self::Nel, Ok((idx, '\u{0085}'))) => Some(Ok((idx, '\n'))),
                (Self::Crnel, Ok((idx, '\r'))) => {
                    if let Some(Ok((idx, '\u{0085}'))) = iter.peek() {
                        Some(Ok((*idx, '\n')))
                    } else {
                        Some(Ok((idx, '\r')))
                    }
                }
                (Self::Ls, Ok((idx, '\u{2028}'))) => Some(Ok((idx, '\n'))),
                (_, err) => Some(err),
            }
        })
    }

    /// Finds the index past the end of line, if it exists
    fn contains_eol(&self, ls_needle: &[u8], bytes: &[u8]) -> bool {
        match self {
            Self::Lf => memchr(b'\n', bytes).is_some(),
            Self::Cr => memchr(b'\r', bytes).is_some(),
            Self::Crlf => memmem::find(bytes, b"\r\n").is_some(),
            Self::Nel => memchr(b'\x85', bytes).is_some(),
            Self::Crnel => memmem::find(bytes, b"\r\x85").is_some(),
            Self::Ls if !ls_needle.is_empty() => memmem::find(bytes, ls_needle).is_some(),
            Self::Ls => false,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ErrorHandlingMode {
    Ignore,
    Raise,
    Replace,
}

#[cfg(not(feature = "async"))]
pub type ReadFn = fn(&mut dyn Any, &mut [u8]) -> io::Result<usize>;
#[cfg(not(feature = "async"))]
pub type WriteFn = fn(&mut dyn Any, &[u8]) -> io::Result<()>;
#[cfg(not(feature = "async"))]
pub type SeekFn = fn(&mut dyn Any, SeekFrom) -> io::Result<u64>;

#[cfg(feature = "async")]
pub type ReadFn =
    for<'a> fn(&'a mut dyn Any, &'a mut [u8]) -> BoxFuture<'a, Result<usize, ReadError>>;
#[cfg(feature = "async")]
pub type WriteFn =
    for<'a> fn(&'a mut dyn Any, &'a [u8]) -> BoxFuture<'a, Result<usize, WriteError>>;

pub(crate) struct PortInner {
    // TODO: make this optional to indicate closing
    port: Box<dyn Any + Send + 'static>,
    read: Option<ReadFn>,
    write: Option<WriteFn>,
    seek: Option<SeekFn>,
    text: bool,
    buffer_mode: BufferMode,
    transcoder: Transcoder,
    input_pos: usize,
    bytes_read: usize,
    input_buffer: Box<[u8]>,
    output_buffer: Vec<u8>,
    utf16_endianness: Option<Endianness>,
}

fn read_fn<T>() -> ReadFn
where
    T: Read + Any + Send + 'static,
{
    |any, buff| {
        let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
        concrete.read(buff)
    }
}

fn write_fn<T>() -> WriteFn
where
    T: Write + Any + Send + 'static,
{
    |any, buff| {
        let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
        concrete.write_all(buff)?;
        concrete.flush()
    }
}

fn seek_fn<T>() -> SeekFn
where
    T: Seek + Any + Send + 'static,
{
    |any, pos| {
        let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
        concrete.seek(pos)
    }
}

pub const BUFFER_SIZE: usize = 8192;

impl PortInner {
    fn new<P>(port: P, text: bool, buffer_mode: BufferMode, transcoder: Transcoder) -> Self
    where
        P: IntoPort,
    {
        let read = P::read_fn();
        let write = P::write_fn();
        let seek = P::seek_fn();

        Self {
            port: port.into_port(),
            read,
            write,
            seek,
            text,
            buffer_mode,
            transcoder,
            input_pos: 0,
            bytes_read: 0,
            input_buffer: buffer_mode.new_input_buffer(text, read.is_some()),
            output_buffer: buffer_mode.new_output_buffer(write.is_some()),
            utf16_endianness: None,
        }
    }

    fn read_byte(&mut self) -> Result<Option<u8>, ReadError> {
        let next_byte = self.peekn_bytes(0)?;
        self.consume_bytes(1)?;
        Ok(next_byte)
    }

    pub(crate) fn read_char(&mut self) -> Result<Option<char>, ReadError> {
        let Some(next_char) = self.peekn_chars(0)? else {
            return Ok(None);
        };
        let byte_len = self.transcoder.codec.byte_len(next_char);
        self.consume_bytes(byte_len)?;

        Ok(Some(next_char))
    }

    fn peekn_bytes(&mut self, n: usize) -> Result<Option<u8>, ReadError> {
        // TODO: If we have a non-empty output buffer, flush that first before reading.

        let Some(read) = self.read else {
            todo!();
        };

        if n + self.input_pos > self.input_buffer.len() {
            panic!("attempt to lookahead further than the buffer allows");
        }

        while self.bytes_read <= n + self.input_pos {
            match self.buffer_mode {
                BufferMode::None => {
                    let read = (read)(
                        self.port.as_mut(),
                        &mut self.input_buffer[self.bytes_read..],
                    )?;
                    if read == 1 {
                        self.bytes_read += 1;
                    } else {
                        return Ok(None);
                    }
                }
                BufferMode::Line => {
                    loop {
                        let read = (read)(
                            self.port.as_mut(),
                            &mut self.input_buffer[self.bytes_read..],
                        )?;
                        if read == 0 {
                            return Ok(None);
                        }
                        // Attempt to find the line ending:
                        if self.transcoder.eol_type.contains_eol(
                            self.transcoder.codec.ls_needle(self.utf16_endianness),
                            &self.input_buffer[self.bytes_read..],
                        ) {
                            self.bytes_read += read;
                            break;
                        } else {
                            self.bytes_read += read;
                            // If we can't find it, we need to extend the buffer.
                            // I don't really like this, but I'm not sure how else
                            // to go about it. Will probably just comment this out
                            // for the time being.
                            let mut buffer = std::mem::replace(
                                &mut self.input_buffer,
                                Vec::new().into_boxed_slice(),
                            )
                            .into_vec();
                            buffer.extend(std::iter::repeat(0u8).take(BUFFER_SIZE));
                            self.input_buffer = buffer.into_boxed_slice();
                        }
                    }
                }
                BufferMode::Block => {
                    let read = (read)(
                        self.port.as_mut(),
                        &mut self.input_buffer[self.bytes_read..],
                    )?;
                    if read == 0 {
                        return Ok(None);
                    }
                    self.bytes_read += read;
                }
            }
        }

        Ok(Some(self.input_buffer[n + self.input_pos]))
    }

    fn transcode(&mut self) -> impl Iterator<Item = Result<(usize, char), ReadError>> {
        let eol_type = self.transcoder.eol_type;
        let decoder = match self.transcoder.codec {
            Codec::Latin1 => {
                let mut i = 0;
                Box::new(std::iter::from_fn(move || match self.peekn_bytes(i) {
                    Ok(Some(byte)) => {
                        let res = (i, char::from(byte));
                        i += 1;
                        Some(Ok(res))
                    }
                    Ok(None) => None,
                    Err(err) => Some(Err(err)),
                })) as Box<dyn Iterator<Item = Result<(usize, char), ReadError>>>
            }
            Codec::Utf16 => Box::new(Decoder::new(
                self,
                Utf16Buffer::new(
                    self.transcoder.error_handling_mode,
                    self.utf16_endianness.unwrap(),
                ),
            )),
            Codec::Utf8 => Box::new(Decoder::new(
                self,
                Utf8Buffer::new(self.transcoder.error_handling_mode),
            )),
        };
        eol_type.convert_eol_style(decoder.peekable())
    }

    pub(crate) fn peekn_chars(&mut self, n: usize) -> Result<Option<char>, ReadError> {
        // TODO: If this a binary port, throw an error.

        // If this is a utf16 port and we have not assigned endiannes, check for
        // the BOM
        if matches!(self.transcoder.codec, Codec::Utf16) && self.utf16_endianness.is_none() {
            let b1 = self.peekn_bytes(0)?;
            let b2 = self.peekn_bytes(1)?;
            self.utf16_endianness = match (b1, b2) {
                (Some(b'\xFF'), Some(b'\xFE')) => {
                    self.consume_bytes(2)?;
                    Some(Endianness::Le)
                }
                (Some(b'\xFE'), Some(b'\xFF')) => {
                    self.consume_bytes(2)?;
                    Some(Endianness::Be)
                }
                _ => Some(Endianness::Le),
            };
        }

        Ok(self.transcode().nth(n).transpose()?.map(|(_, chr)| chr))
    }

    fn consume_bytes(&mut self, n: usize) -> Result<(), ReadError> {
        if self.bytes_read < self.input_pos + n {
            let _ = self.peekn_bytes(n - self.bytes_read)?;
        }

        self.input_pos += n;

        if self.input_pos >= self.input_buffer.len() {
            self.input_pos -= self.input_buffer.len();
            self.bytes_read = 0;
        }

        Ok(())
    }

    pub(crate) fn consume_chars(&mut self, n: usize) -> Result<(), ReadError> {
        for _ in 0..n {
            self.read_char()?;
        }
        Ok(())
        /*
        let Some((bytes_to_skip, last_char)) = self.transcode().take(n).last().transpose()? else {
            // println!("early return");
            return Ok(());
        };
        self.consume_bytes(dbg!(bytes_to_skip + self.transcoder.codec.byte_len(last_char)))
        */
    }

    fn put_bytes(&mut self, mut bytes: &[u8]) -> Result<(), WriteError> {
        let Some(write) = self.write else {
            todo!();
        };

        match self.buffer_mode {
            BufferMode::None => write(self.port.as_mut(), bytes)?,
            BufferMode::Line => loop {
                if let Some(line_end) = memchr(b'\n', bytes) {
                    self.output_buffer.extend_from_slice(&bytes[..line_end + 1]);
                    bytes = &bytes[line_end + 1..];
                    write(self.port.as_mut(), &self.output_buffer[..])?;
                    self.output_buffer.clear();
                } else {
                    self.output_buffer.extend_from_slice(bytes);
                    break;
                }
            },
            BufferMode::Block => loop {
                if bytes.len() + self.output_buffer.len() >= BUFFER_SIZE {
                    let num_bytes_to_buffer = BUFFER_SIZE - &self.output_buffer.len();
                    self.output_buffer
                        .extend_from_slice(&bytes[..num_bytes_to_buffer]);
                    bytes = &bytes[num_bytes_to_buffer..];
                    write(self.port.as_mut(), &self.output_buffer[..])?;
                    self.output_buffer.clear();
                } else {
                    self.output_buffer.extend_from_slice(bytes);
                    break;
                }
            },
        }

        Ok(())
    }

    /*
    #[cfg(not(feature = "async"))]
    fn lock_port<R>(
        &self,
        callback: impl FnOnce(&mut dyn Any, Option<ReadFn>, Option<WriteFn>, Option<SeekFn>) -> R,
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
    */
}

pub trait IntoPort: Any + Send + 'static + Sized {
    fn into_port(self) -> Box<dyn Any + Send + 'static> {
        Box::new(self)
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

impl IntoPort for Cursor<Vec<u8>> {
    fn read_fn() -> Option<ReadFn> {
        Some(read_fn::<Self>())
    }

    fn write_fn() -> Option<WriteFn> {
        Some(write_fn::<Self>())
    }

    fn seek_fn() -> Option<SeekFn> {
        Some(seek_fn::<Self>())
    }    
}

#[cfg(not(feature = "async"))]
impl IntoPort for std::fs::File {
    fn read_fn() -> Option<ReadFn> {
        Some(read_fn::<Self>())
    }

    fn write_fn() -> Option<WriteFn> {
        Some(write_fn::<Self>())
    }

    fn seek_fn() -> Option<SeekFn> {
        Some(seek_fn::<Self>())
    }
}

pub struct Port(pub(crate) Arc<Mutex<PortInner>>);

impl Port {
    pub fn new<P>(port: P, text: bool, buffer_mode: BufferMode, transcoder: Transcoder) -> Self
    where
        P: IntoPort,
    {
        Self(Arc::new(Mutex::new(PortInner::new(
            port,
            text,
            buffer_mode,
            transcoder,
        ))))
    }

    #[maybe_async]
    pub fn get_sexpr(&self) -> Result<Option<Syntax>, ParseSyntaxError> {
        #[cfg(not(feature = "async"))]
        let mut port = self.0.lock().unwrap();

        #[cfg(feature = "async")]
        let mut port = self.0.lock().await;

        let mut parser = Parser::new("<todo>", &mut port);

        Ok(maybe_await!(parser.get_sexpr_or_eof())?)
    }

    #[maybe_async]
    pub fn all_sexprs(&self) -> Result<Vec<Syntax>, ParseSyntaxError> {
        #[cfg(not(feature = "async"))]
        let mut port = self.0.lock().unwrap();

        #[cfg(feature = "async")]
        let mut port = self.0.lock().await;

        let mut parser = Parser::new("<todo>", &mut port);

        Ok(maybe_await!(parser.all_sexprs())?)
    }

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
        H: rustyline::Helper + Send + 'static,
        I: rustyline::history::History + Send + Sync + 'static,
    {
        leftover: Vec<u8>,
        editor: Editor<H, I>,
        closed: bool,
    }

    impl<H, I> Prompt<H, I>
    where
        H: rustyline::Helper + Send + 'static,
        I: rustyline::history::History + Send + Sync + 'static,
    {
        pub fn new(editor: Editor<H, I>) -> Self {
            Self {
                leftover: Vec::new(),
                editor,
                closed: false,
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
        H: rustyline::Helper + Send + 'static,
        I: rustyline::history::History + Send + Sync + 'static,
    {
        fn read_fn() -> Option<ReadFn> {
            Some(|any, buff| {
                use std::cmp::Ordering;
                let concrete = unsafe { any.downcast_mut::<Self>().unwrap_unchecked() };
                if concrete.closed {
                    return Ok(0);
                }
                let mut line = if concrete.leftover.is_empty() {
                    if let Ok(line) = concrete.editor.readline("> ") {
                        let mut line = line.into_bytes();
                        line.push(b'\n');
                        line
                    } else {
                        concrete.closed = true;
                        return Ok(0);
                    }
                } else {
                    std::mem::take(&mut concrete.leftover)
                };
                match line.len().cmp(&buff.len()) {
                    Ordering::Less => {
                        buff[..line.len()].copy_from_slice(line.as_slice());
                    }
                    Ordering::Greater => {
                        concrete.leftover = line.split_off(buff.len());
                        buff.copy_from_slice(line.as_slice());
                    }
                    Ordering::Equal => {
                        buff.copy_from_slice(line.as_slice());
                    }
                }
                Ok(line.len())
            })
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

pub use prompt::*;

use crate::{
    exceptions::Condition,
    syntax::{parse::{ParseSyntaxError, Parser}, Syntax},
};
