//! Input and Output handling

use either::Either;
use memchr::{memchr, memmem};
use rustyline::Editor;
use scheme_rs_macros::{bridge, define_condition_type, maybe_async, maybe_await, rtd};
use std::{any::Any, borrow::Cow, fmt, io::Cursor, sync::{Arc, LazyLock, OnceLock}};

use crate::{
    exceptions::{self, Condition, Error}, gc::{Gc, Trace}, records::{Record, RecordTypeDescriptor, SchemeCompatible}, syntax::{
        parse::{ParseSyntaxError, Parser}, Span, Syntax
    }, value::{Value, ValueType}
};

#[derive(Clone, Debug)]
pub enum ReadError {
    Io(Arc<io::Error>),
    NotAnInputPort,
    PortIsClosed,
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
    PortIsClosed,
}

impl From<io::Error> for WriteError {
    fn from(error: io::Error) -> Self {
        Self::Io(Arc::new(error))
    }
}

#[derive(Clone, Debug)]
pub enum SeekError {
    Io(Arc<io::Error>),
    NotSeekable,
    PortIsClosed,
}

impl From<io::Error> for SeekError {
    fn from(error: io::Error) -> Self {
        Self::Io(Arc::new(error))
    }
}

pub(crate) struct Utf8Buffer {
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

struct Decoder<'a, D> {
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

impl<D> Decoder<'_, D>
where
    D: Decode,
{
    #[maybe_async]
    fn decode_next(&mut self) -> Option<Result<(usize, char), ReadError>> {
        let mut pos = match self.pos {
            Either::Left(pos) => pos,
            Either::Right(ref err) => return Some(Err(err.clone())),
        };
        loop {
            match maybe_await!(self.port.peekn_bytes(pos))
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
    #[maybe_async]
    fn convert_eol_style_to_linefeed_inner(
        self,
        iter: &mut Peekable<impl MaybeStream<Item = Result<(usize, char), ReadError>>>,
    ) -> Option<Result<(usize, char), ReadError>> {
        #[cfg(feature = "async")]
        let mut iter: std::pin::Pin<&mut Peekable<_>> = std::pin::pin!(iter);
        let next_chr = maybe_await!(iter.next())?;
        match (self, next_chr) {
            (Self::Lf, x) => Some(x),
            (Self::Cr, Ok((idx, '\r'))) => Some(Ok((idx, '\n'))),
            (Self::Crlf, Ok((idx, '\r'))) => {
                if let Some(Ok((idx, '\n'))) = maybe_await!(iter.peek()) {
                    Some(Ok((*idx, '\n')))
                } else {
                    Some(Ok((idx, '\r')))
                }
            }
            (Self::Nel, Ok((idx, '\u{0085}'))) => Some(Ok((idx, '\n'))),
            (Self::Crnel, Ok((idx, '\r'))) => {
                if let Some(Ok((idx, '\u{0085}'))) = maybe_await!(iter.peek()) {
                    Some(Ok((*idx, '\n')))
                } else {
                    Some(Ok((idx, '\r')))
                }
            }
            (Self::Ls, Ok((idx, '\u{2028}'))) => Some(Ok((idx, '\n'))),
            (_, err) => Some(err),
        }
    }

    #[cfg(not(feature = "async"))]
    fn convert_eol_style_to_linefeed(
        self,
        mut iter: Peekable<impl Iterator<Item = Result<(usize, char), ReadError>>>,
    ) -> impl Iterator<Item = Result<(usize, char), ReadError>> {
        std::iter::from_fn(move || self.convert_eol_style_to_linefeed_inner(&mut iter))
    }

    #[cfg(feature = "async")]
    fn convert_eol_style_to_linefeed(
        self,
        mut iter: Peekable<impl MaybeStream<Item = Result<(usize, char), ReadError>>>,
    ) -> impl futures::stream::Stream<Item = Result<(usize, char), ReadError>> {
        async_stream::stream! {
            while let Some(val) = self.convert_eol_style_to_linefeed_inner(&mut iter).await {
                yield val;
            }
        }
    }

    fn convert_linefeeds_to_eol_style(
        self,
        iter: impl Iterator<Item = char>,
    ) -> impl Iterator<Item = char> {
        iter.flat_map(move |chr| {
            if chr == '\n' {
                match self {
                    Self::Lf => [Some('\n'), None],
                    Self::Cr => [Some('\r'), None],
                    Self::Crlf => [Some('\r'), Some('\n')],
                    Self::Nel => [Some('\u{0085}'), None],
                    Self::Crnel => [Some('\r'), Some('\u{0085}')],
                    Self::Ls => [Some('\u{2028}'), None],
                }
            } else {
                [Some(chr), None]
            }
        })
        .flatten()
    }

    /// Finds the index past the end of line, if it exists
    fn find_next_line(&self, ls_needle: &[u8], bytes: &[u8]) -> Option<usize> {
        match self {
            Self::Lf => memchr(b'\n', bytes).map(|i| i + 1),
            Self::Cr => memchr(b'\r', bytes).map(|i| i + 1),
            Self::Crlf => memmem::find(bytes, b"\r\n").map(|i| i + 2),
            Self::Nel => memchr(b'\x85', bytes).map(|i| i + 1),
            Self::Crnel => memmem::find(bytes, b"\r\x85").map(|i| i + 2),
            Self::Ls if !ls_needle.is_empty() => {
                memmem::find(bytes, ls_needle).map(|i| i + ls_needle.len())
            }
            Self::Ls => None,
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
mod __impl {
    pub(super) use std::{
        io::{self, Read, Seek, SeekFrom, Write},
        iter::{Iterator as MaybeStream, Peekable},
        sync::Mutex,
    };

    use super::*;

    pub(super) type ReadFn = fn(&mut dyn Any, &mut [u8]) -> io::Result<usize>;
    pub(super) type WriteFn = fn(&mut dyn Any, &[u8]) -> io::Result<()>;
    pub(super) type SeekFn = fn(&mut dyn Any, SeekFrom) -> io::Result<u64>;

    pub(super) fn read_fn<T>() -> ReadFn
    where
        T: Read + Any + Send + 'static,
    {
        |any, buff| {
            let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
            concrete.read(buff)
        }
    }

    pub(super) fn write_fn<T>() -> WriteFn
    where
        T: Write + Any + Send + 'static,
    {
        |any, buff| {
            let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
            concrete.write_all(buff)?;
            concrete.flush()?;
            Ok(())
        }
    }

    pub(super) fn seek_fn<T>() -> SeekFn
    where
        T: Seek + Any + Send + 'static,
    {
        |any, pos| {
            let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
            concrete.seek(pos)
        }
    }

    impl<D> Iterator for Decoder<'_, D>
    where
        D: Decode,
    {
        type Item = Result<(usize, char), ReadError>;

        fn next(&mut self) -> Option<Self::Item> {
            self.decode_next()
        }
    }

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

    impl IntoPort for std::io::Stdout {
        fn write_fn() -> Option<WriteFn> {
            Some(write_fn::<Self>())
        }
    }

    impl IntoPort for std::io::Stderr {
        fn write_fn() -> Option<WriteFn> {
            Some(write_fn::<Self>())
        }
    }
}

#[cfg(feature = "async")]
mod __impl {
    use futures::future::BoxFuture;
    pub(super) use futures::stream::{Peekable, Stream as MaybeStream, StreamExt};
    use std::pin::pin;
    pub(super) use std::{
        io::{self, SeekFrom},
        pin::Pin,
    };
    use tokio::io::{AsyncRead, AsyncReadExt, AsyncSeekExt, AsyncWrite, AsyncWriteExt};
    #[cfg(feature = "tokio")]
    pub(super) use tokio::sync::Mutex;

    use super::*;

    pub type ReadFn =
        for<'a> fn(&'a mut (dyn Any + Send), &'a mut [u8]) -> BoxFuture<'a, io::Result<usize>>;
    pub type WriteFn =
        for<'a> fn(&'a mut (dyn Any + Send), &'a [u8]) -> BoxFuture<'a, io::Result<()>>;
    pub type SeekFn =
        for<'a> fn(&'a mut (dyn Any + Send), SeekFrom) -> BoxFuture<'a, io::Result<u64>>;

    pub(super) fn read_fn<T>() -> ReadFn
    where
        T: AsyncRead + Any + Send + 'static,
    {
        |any, buff| {
            Box::pin(async move {
                let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
                let mut concrete: Pin<&mut T> = pin!(concrete);
                concrete.read(buff).await
            })
        }
    }

    pub(super) fn write_fn<T>() -> WriteFn
    where
        T: AsyncWrite + Any + Send + 'static,
    {
        |any, buff| {
            Box::pin(async move {
                let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
                let mut concrete: Pin<&mut T> = pin!(concrete);
                concrete.write_all(buff).await?;
                concrete.flush().await?;
                Ok(())
            })
        }
    }

    pub(super) fn seek_fn<T>() -> SeekFn
    where
        T: AsyncSeekExt + Any + Send + 'static,
    {
        |any, pos| {
            Box::pin(async move {
                let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
                let mut concrete: Pin<&mut T> = pin!(concrete);
                concrete.seek(pos).await
            })
        }
    }

    #[cfg(feature = "tokio")]
    impl IntoPort for tokio::fs::File {
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

    #[cfg(feature = "tokio")]
    impl IntoPort for tokio::io::Stdout {
        fn write_fn() -> Option<WriteFn> {
            Some(write_fn::<Self>())
        }
    }

    #[cfg(feature = "tokio")]
    impl IntoPort for tokio::io::Stderr {
        fn write_fn() -> Option<WriteFn> {
            Some(write_fn::<Self>())
        }
    }

    pub(super) trait StreamExtExt {
        type Item;

        async fn last(self) -> Option<Self::Item>;

        async fn nth(self, n: usize) -> Option<Self::Item>;
    }

    impl<T> StreamExtExt for T
    where
        T: StreamExt,
    {
        type Item = T::Item;

        async fn last(self) -> Option<Self::Item> {
            self.fold(None, |_, x| async move { Some(x) }).await
        }

        async fn nth(self, n: usize) -> Option<Self::Item> {
            let mut this = std::pin::pin!(self);
            for _ in 0..n {
                let _ = this.next().await?;
            }
            this.next().await
        }
    }
}

use __impl::*;

pub(crate) struct PortInner {
    port: Option<Box<dyn Any + Send + 'static>>,
    read: Option<ReadFn>,
    write: Option<WriteFn>,
    seek: Option<SeekFn>,
    #[allow(unused)]
    text: bool,
    buffer_mode: BufferMode,
    transcoder: Transcoder,
    input_pos: usize,
    bytes_read: usize,
    input_buffer: Box<[u8]>,
    output_buffer: Vec<u8>,
    utf16_endianness: Option<Endianness>,
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
            port: Some(port.into_port()),
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

    #[allow(unused)]
    #[maybe_async]
    fn read_byte(&mut self) -> Result<Option<u8>, ReadError> {
        let next_byte = maybe_await!(self.peekn_bytes(0))?;
        maybe_await!(self.consume_bytes(1))?;
        Ok(next_byte)
    }

    #[maybe_async]
    pub(crate) fn read_char(&mut self) -> Result<Option<char>, ReadError> {
        let Some(next_char) = maybe_await!(self.peekn_chars(0))? else {
            return Ok(None);
        };
        let byte_len = self.transcoder.codec.byte_len(next_char);
        maybe_await!(self.consume_bytes(byte_len))?;

        Ok(Some(next_char))
    }

    #[maybe_async]
    fn peekn_bytes(&mut self, n: usize) -> Result<Option<u8>, ReadError> {
        let Some(read) = self.read else {
            return Err(ReadError::NotAnInputPort);
        };

        let Some(port) = self.port.as_deref_mut() else {
            return Err(ReadError::PortIsClosed);
        };

        if let Some(write) = self.write
            && !self.output_buffer.is_empty()
        {
            maybe_await!(write(port, &self.output_buffer[..]))?;
            self.output_buffer.clear();
        }

        if n + self.input_pos > self.input_buffer.len() {
            panic!("attempt to lookahead further than the buffer allows");
        }

        while self.bytes_read <= n + self.input_pos {
            match self.buffer_mode {
                BufferMode::None => {
                    let read = maybe_await!((read)(
                        port,
                        &mut self.input_buffer[self.bytes_read..],
                    ))?;
                    if read == 1 {
                        self.bytes_read += 1;
                    } else {
                        return Ok(None);
                    }
                }
                BufferMode::Line => {
                    loop {
                        let read = maybe_await!((read)(
                            port,
                            &mut self.input_buffer[self.bytes_read..],
                        ))?;
                        if read == 0 {
                            return Ok(None);
                        }
                        // Attempt to find the line ending:
                        if self
                            .transcoder
                            .eol_type
                            .find_next_line(
                                self.transcoder.codec.ls_needle(self.utf16_endianness),
                                &self.input_buffer[self.bytes_read..],
                            )
                            .is_some()
                        {
                            self.bytes_read += read;
                            break;
                        } else {
                            self.bytes_read += read;
                            // If we can't find it, we need to extend the
                            // buffer. I don't really like this, but I'm not
                            // sure how else to go about it. Will probably just
                            // end up commenting this out.
                            let mut buffer = std::mem::replace(
                                &mut self.input_buffer,
                                Vec::new().into_boxed_slice(),
                            )
                            .into_vec();
                            buffer.extend(std::iter::repeat_n(0u8, BUFFER_SIZE));
                            self.input_buffer = buffer.into_boxed_slice();
                        }
                    }
                }
                BufferMode::Block => {
                    let read = maybe_await!((read)(
                        port,
                        &mut self.input_buffer[self.bytes_read..],
                    ))?;
                    if read == 0 {
                        return Ok(None);
                    }
                    self.bytes_read += read;
                }
            }
        }

        Ok(Some(self.input_buffer[n + self.input_pos]))
    }

    #[cfg(not(feature = "async"))]
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
        eol_type.convert_eol_style_to_linefeed(decoder.peekable())
    }

    #[cfg(feature = "async")]
    fn transcode(
        &mut self,
    ) -> impl futures::stream::Stream<Item = Result<(usize, char), ReadError>> {
        let eol_type = self.transcoder.eol_type;
        let decoder = match self.transcoder.codec {
            Codec::Latin1 => async_stream::stream! {
                let mut i = 0;
                loop {
                    match self.peekn_bytes(i).await {
                        Ok(Some(byte)) => {
                            let res = (i, char::from(byte));
                            i += 1;
                            yield Ok(res)
                        }
                        Ok(None) => break,
                        Err(err) => yield Err(err),
                    }
                }
            }
            .boxed(),
            Codec::Utf16 => async_stream::stream! {
                let mut decoder = Decoder::new(
                    self,
                    Utf16Buffer::new(
                        self.transcoder.error_handling_mode,
                        self.utf16_endianness.unwrap(),
                    ),
                );
                while let Some(decoded) = decoder.decode_next().await {
                    yield decoded;
                }
            }
            .boxed(),
            Codec::Utf8 => async_stream::stream! {
                let mut decoder = Decoder::new(
                    self,
                    Utf8Buffer::new(self.transcoder.error_handling_mode),
                );
                while let Some(decoded) = decoder.decode_next().await {
                    yield decoded;
                }
            }
            .boxed(),
        };
        eol_type.convert_eol_style_to_linefeed(decoder.peekable())
    }

    #[maybe_async]
    pub(crate) fn peekn_chars(&mut self, n: usize) -> Result<Option<char>, ReadError> {
        // TODO: If this a binary port, throw an error.

        // If this is a utf16 port and we have not assigned endiannes, check for
        // the BOM
        if matches!(self.transcoder.codec, Codec::Utf16) && self.utf16_endianness.is_none() {
            let b1 = maybe_await!(self.peekn_bytes(0))?;
            let b2 = maybe_await!(self.peekn_bytes(1))?;
            self.utf16_endianness = match (b1, b2) {
                (Some(b'\xFF'), Some(b'\xFE')) => {
                    maybe_await!(self.consume_bytes(2))?;
                    Some(Endianness::Le)
                }
                (Some(b'\xFE'), Some(b'\xFF')) => {
                    maybe_await!(self.consume_bytes(2))?;
                    Some(Endianness::Be)
                }
                _ => Some(Endianness::Le),
            };
        }

        Ok(maybe_await!(self.transcode().nth(n))
            .transpose()?
            .map(|(_, chr)| chr))
    }

    #[maybe_async]
    fn consume_bytes(&mut self, n: usize) -> Result<(), ReadError> {
        if self.bytes_read < self.input_pos + n {
            let _ = maybe_await!(self.peekn_bytes(n - self.bytes_read))?;
        }

        self.input_pos += n;

        if self.input_pos >= self.input_buffer.len() {
            self.input_pos -= self.input_buffer.len();
            self.bytes_read = 0;
        }

        Ok(())
    }

    #[maybe_async]
    pub(crate) fn consume_chars(&mut self, n: usize) -> Result<(), ReadError> {
        let Some((bytes_to_skip, last_char)) =
            maybe_await!(self.transcode().take(n).last()).transpose()?
        else {
            return Ok(());
        };
        maybe_await!(self.consume_bytes(bytes_to_skip + self.transcoder.codec.byte_len(last_char)))
    }

    #[maybe_async]
    fn put_bytes(&mut self, mut bytes: &[u8]) -> Result<(), WriteError> {
        let Some(write) = self.write else {
            return Err(WriteError::NotAnOutputPort);
        };

        let Some(port) = self.port.as_deref_mut() else {
            return Err(WriteError::PortIsClosed);
        };

        // If we can, seek back
        if let Some(seek) = self.seek
            && self.bytes_read > 0
        {
            let seek_to = (self.bytes_read - self.input_pos) as i64;
            maybe_await!(seek(port, SeekFrom::Current(-seek_to)))?;
            self.bytes_read = 0;
            self.input_pos = 0;
        }

        match self.buffer_mode {
            BufferMode::None => maybe_await!(write(port, bytes))?,
            BufferMode::Line => loop {
                if let Some(next_line) = self.transcoder.eol_type.find_next_line(
                    self.transcoder.codec.ls_needle(self.utf16_endianness),
                    bytes,
                ) {
                    self.output_buffer.extend_from_slice(&bytes[..next_line]);
                    bytes = &bytes[next_line..];
                    maybe_await!(write(port, &self.output_buffer[..]))?;
                    self.output_buffer.clear();
                } else {
                    self.output_buffer.extend_from_slice(bytes);
                    break;
                }
            },
            BufferMode::Block => loop {
                if bytes.len() + self.output_buffer.len() >= BUFFER_SIZE {
                    let num_bytes_to_buffer = BUFFER_SIZE - self.output_buffer.len();
                    self.output_buffer
                        .extend_from_slice(&bytes[..num_bytes_to_buffer]);
                    bytes = &bytes[num_bytes_to_buffer..];
                    maybe_await!(write(port, &self.output_buffer[..]))?;
                    self.output_buffer.clear();
                } else {
                    self.output_buffer.extend_from_slice(bytes);
                    break;
                }
            },
        }

        Ok(())
    }

    #[maybe_async]
    fn put_str(&mut self, s: &str) -> Result<(), WriteError> {
        let s = if matches!(self.transcoder.eol_type, EolStyle::Lf) {
            Cow::Borrowed(s)
        } else {
            Cow::Owned(
                self.transcoder
                    .eol_type
                    .convert_linefeeds_to_eol_style(s.chars())
                    .collect::<String>(),
            )
        };
        match self.transcoder.codec {
            Codec::Latin1 | Codec::Utf8 => {
                // Probably should do a check here to ensure the string is ascii
                // if our codec is latin1
                maybe_await!(self.put_bytes(s.as_bytes()))?;
            }
            Codec::Utf16 => {
                let endianness = self.utf16_endianness.unwrap_or(Endianness::Le);
                let bytes = s
                    .encode_utf16()
                    .flat_map(|codepoint| match endianness {
                        Endianness::Le => codepoint.to_le_bytes(),
                        Endianness::Be => codepoint.to_be_bytes(),
                    })
                    .collect::<Vec<_>>();
                maybe_await!(self.put_bytes(&bytes))?;
            }
        }
        Ok(())
    }

    #[maybe_async]
    fn flush(&mut self) -> Result<(), WriteError> {
        let Some(write) = self.write else {
            return Err(WriteError::NotAnOutputPort);
        };

        let Some(port) = self.port.as_deref_mut() else {
            return Err(WriteError::PortIsClosed);
        };

        maybe_await!(write(port, &self.output_buffer[..]))?;
        self.output_buffer.clear();

        Ok(())
    }

    #[allow(unused)]
    #[maybe_async]
    fn seek(&mut self, pos: u64) -> Result<u64, SeekError> {
        let Some(seek) = self.seek else {
            return Err(SeekError::NotSeekable);
        };

        let Some(port) = self.port.as_deref_mut() else {
            return Err(SeekError::PortIsClosed);
        };

        let pos = maybe_await!(seek(port, SeekFrom::Start(pos)))?;

        Ok(pos)
    }
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

#[derive(Trace, Clone)]
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
    pub fn get_sexpr(&self, span: Span) -> Result<Option<(Syntax, Span)>, ParseSyntaxError> {
        #[cfg(not(feature = "async"))]
        let mut port = self.0.lock().unwrap();

        #[cfg(feature = "async")]
        let mut port = self.0.lock().await;

        let mut parser = Parser::new(&mut port, span);

        let sexpr_or_eof = maybe_await!(parser.get_sexpr_or_eof())?;
        let ending_span = parser.curr_span();

        Ok(sexpr_or_eof.map(|sexpr| (sexpr, ending_span)))
    }

    #[maybe_async]
    pub fn all_sexprs(&self, span: Span) -> Result<Vec<Syntax>, ParseSyntaxError> {
        #[cfg(not(feature = "async"))]
        let mut port = self.0.lock().unwrap();

        #[cfg(feature = "async")]
        let mut port = self.0.lock().await;

        let mut parser = Parser::new(&mut port, span);

        Ok(maybe_await!(parser.all_sexprs())?)
    }

    #[maybe_async]
    pub fn put_str(&self, s: &str) -> Result<(), WriteError> {
        #[cfg(not(feature = "async"))]
        let mut port = self.0.lock().unwrap();

        #[cfg(feature = "async")]
        let mut port = self.0.lock().await;

        maybe_await!(port.put_str(s))
    }

    #[maybe_async]
    pub fn flush(&self) -> Result<(), WriteError> {
        #[cfg(not(feature = "async"))]
        let mut port = self.0.lock().unwrap();

        #[cfg(feature = "async")]
        let mut port = self.0.lock().await;

        maybe_await!(port.flush())
    }
}

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

    trait Readline: Send + 'static {
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

    pub struct Prompt {
        leftover: Vec<u8>,
        closed: bool,
        editor: Arc<std::sync::Mutex<dyn Readline>>,
    }

    impl Prompt {
        #[allow(private_bounds)]
        pub fn new(editor: impl Readline) -> Self {
            let editor = Arc::new(std::sync::Mutex::new(editor));
            Self {
                leftover: Vec::new(),
                closed: false,
                editor,
            }
        }
    }

    impl IntoPort for Prompt {
        fn read_fn() -> Option<ReadFn> {
            Some(|any, buff| {
                Box::pin(async move {
                    use std::cmp::Ordering;

                    let concrete = unsafe { any.downcast_mut::<Self>().unwrap_unchecked() };
                    let mut concrete: Pin<&mut Self> = std::pin::pin!(concrete);

                    // TODO: Figure out how to de-duplicate this code
                    if concrete.closed {
                        return Ok(0);
                    }
                    let mut line = if concrete.leftover.is_empty() {
                        if let Ok(line) = {
                            let (tx, rx) = tokio::sync::oneshot::channel();
                            PROMPT_TASK
                                .tx
                                .send(InputRequest {
                                    prompt: "> ".to_string(),
                                    editor: concrete.editor.clone(),
                                    tx,
                                })
                                .await
                                .unwrap();

                            rx.await.unwrap()
                        } {
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
            })
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

// Conditions:

define_condition_type!(
    rust_name: IoError,
    scheme_name: "&i/o",
    parent: Error
);

impl IoError {
    pub fn new() -> Self {
        Self {
            parent: Gc::new(Error::new()),
        }
    }
}

define_condition_type!(
    rust_name: IoReadError,
    scheme_name: "&i/o-read",
    parent: IoError,
);

define_condition_type!(
    rust_name: IoWriteError,
    scheme_name: "&i/o-write",
    parent: IoError,
);

define_condition_type!(
    rust_name: IoInvalidPositionError,
    scheme_name: "&i/o-invalid-position",
    parent: IoError,
    fields: {
        position: usize,
    },
    constructor: |position| {
        Ok(IoInvalidPositionError {
            parent: Gc::new(IoError::new()),
            position: todo!(),
        })
    },
    debug: |this, f| {
        write!(f, " position: {}", this.position)
    }
);

define_condition_type!(
    rust_name: IoFilenameError,
    scheme_name: "&i/o-filename",
    parent: IoError,
    fields: {
        filename: String,
    },
    constructor: |filename| {
        Ok(IoFilenameError {
            parent: Gc::new(IoError::new()),
            filename: filename.to_string(),
        })
    },
    debug: |this, f| {
        write!(f, " filename: {}", this.filename)
    }
);

impl IoFilenameError {
    pub fn new(filename: String) -> Self {
        Self {
            parent: Gc::new(IoError::new()),
            filename: filename.to_string(),
        }
    }
}

define_condition_type!(
    rust_name: IoFileProtectionError,
    scheme_name: "&i/o-file-protection",
    parent: IoFilenameError,
    constructor: |filename| {
        Ok(IoFileProtectionError {
            parent: Gc::new(IoFilenameError::new(filename.to_string()))
        })
    },
    debug: |this, f| {
        this.parent.read().fmt(f)
    }
);

impl IoFileProtectionError {
    pub fn new(filename: String) -> Self {
        Self {
            parent: Gc::new(IoFilenameError::new(filename))
        }
    }
}

define_condition_type!(
    rust_name: IoFileIsReadOnlyError,
    scheme_name: "&i/o-file-is-read-only",
    parent: IoFileProtectionError,
    constructor: |filename| {
        Ok(IoFileIsReadOnlyError {
            parent: Gc::new(IoFileProtectionError::new(filename.to_string()))
        })
    },
    debug: |this, f| {
        this.parent.read().fmt(f)
    }
);

define_condition_type!(
    rust_name: IoFileAlreadyExistsError,
    scheme_name: "&i/o-file-already-exists",
    parent: IoFilenameError,
    constructor: |filename| {
        Ok(IoFileAlreadyExistsError {
            parent: Gc::new(IoFilenameError::new(filename.to_string()))
        })
    },
    debug: |this, f| {
        this.parent.read().fmt(f)
    }
);

define_condition_type!(
    rust_name: IoFileDoesNotExistError,
    scheme_name: "&i/o-file-does-not-exist",
    parent: IoFilenameError,
    constructor: |filename| {
        Ok(IoFileDoesNotExistError {
            parent: Gc::new(IoFilenameError::new(filename.to_string()))
        })
    },
    debug: |this, f| {
        this.parent.read().fmt(f)
    }
);

define_condition_type!(
    rust_name: IoPortError,
    scheme_name: "&i/o-port",
    parent: IoError,
    fields: {
        port: Port,
    },
    constructor: |port| {
        Ok(IoPortError {
            parent: Gc::new(IoError::new()),
            port: port.try_into()?,
        })
    },
);

#[derive(Copy, Clone, Trace)]
pub struct EofObject;

impl SchemeCompatible for EofObject {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "!eof", opaque: true, sealed: true)
    }
}

impl fmt::Debug for EofObject {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

static EOF_OBJECT: LazyLock<Value> = LazyLock::new(|| {
    Value::from(Record::from_rust_type(EofObject))
});

#[bridge(name = "eof-object", lib = "(rnrs io ports (6))")]
pub fn eof_object() -> Result<Vec<Value>, Condition> {
    Ok(vec![EOF_OBJECT.clone()])
}

#[bridge(name = "port?", lib = "(rnrs io ports (6))")]
pub fn port_pred(obj: &Value) ->Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(obj.type_of() == ValueType::Port)])
}

// TODO: port-transcoder

// TODO: textual-port?
// TODO: binary-port?

// TODO: transcoded-port

// TODO: has-port-position?
// TODO: port-position

// TODO: port-has-set-position!?
// TODO: set-port-position!


#[bridge(name = "standard-output-port", lib = "(rnrs io ports (6))")]
pub fn standard_output_port() -> Result<Vec<Value>, Condition> {
    let port = Port::new(
        #[cfg(not(feature = "async"))]
        std::io::stdout(),
        #[cfg(feature = "tokio")]
        tokio::io::stdout(),
        true,
        BufferMode::None,
        Transcoder::native(),
    );
    Ok(vec![Value::from(port)])
}

#[maybe_async]
#[bridge(name = "put-datum", lib = "(rnrs io ports (6))")]
pub fn put_datum(port: &Value, datum: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = port.clone().try_into()?;
    let datum = format!("{datum}");
    maybe_await!(port.put_str(&datum))?;
    Ok(Vec::new())
}
