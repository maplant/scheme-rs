//! Input and Output handling.

use either::Either;
use memchr::{memchr, memmem};
use rustyline::Editor;
use scheme_rs_macros::{bridge, cps_bridge, define_condition_type, maybe_async, maybe_await, rtd};
use std::{
    any::Any,
    borrow::Cow,
    fmt,
    io::{Cursor, ErrorKind},
    sync::{Arc, LazyLock},
};

use crate::{
    enumerations::{EnumerationSet, EnumerationType},
    exceptions::{Assertion, Condition, Error},
    gc::{Gc, Trace},
    proc::{Application, DynStack, Procedure},
    records::{Record, RecordTypeDescriptor, SchemeCompatible},
    runtime::Runtime,
    strings::WideString,
    symbols::Symbol,
    syntax::{
        Span, Syntax,
        parse::{ParseSyntaxError, Parser},
    },
    value::{Value, ValueType},
    vectors::ByteVector,
};

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
    fn push_and_decode(&mut self, byte: u8) -> Result<Option<char>, Condition> {
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
            Err(err) => {
                self.len = 0;
                match self.error_mode {
                    ErrorHandlingMode::Ignore => Ok(None),
                    ErrorHandlingMode::Replace => Ok(Some('\u{FFFD}')),
                    ErrorHandlingMode::Raise => Err(Condition::io_read_error(format!("{err:?}"))),
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
    fn push_and_decode(&mut self, byte: u8) -> Result<Option<char>, Condition> {
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
            [Err(err), _, ..] => {
                self.buff[0] = self.buff[2];
                self.buff[1] = self.buff[3];
                self.len = 0;
                match self.error_mode {
                    ErrorHandlingMode::Ignore => Ok(None),
                    ErrorHandlingMode::Replace => Ok(Some('\u{FFFD}')),
                    ErrorHandlingMode::Raise => Err(Condition::io_read_error(format!("{err:?}"))),
                }
            }
            [Err(_)] => Ok(None),
            [] => unreachable!(),
        }
    }
}

trait Decode {
    fn push_and_decode(&mut self, byte: u8) -> Result<Option<char>, Condition>;
}

struct Decoder<'a, D> {
    data: &'a mut BinaryPortData,
    info: &'a BinaryPortInfo,
    decode: D,
    char_idx: usize,
    pos: Either<usize, Condition>,
}

impl<'a, D> Decoder<'a, D> {
    fn new(data: &'a mut BinaryPortData, info: &'a BinaryPortInfo, decode: D) -> Self {
        Self {
            data,
            info,
            decode,
            char_idx: 0,
            pos: Either::Left(0),
        }
    }
}

impl<D> Decoder<'_, D>
where
    D: Decode,
{
    #[maybe_async]
    fn decode_next(&mut self) -> Option<Result<(usize, char), Condition>> {
        let mut pos = match self.pos {
            Either::Left(pos) => pos,
            Either::Right(ref err) => return Some(Err(err.clone())),
        };
        loop {
            match maybe_await!(self.data.peekn_bytes(self.info, pos))
                .transpose()?
                .and_then(|byte| self.decode.push_and_decode(byte))
            {
                Ok(Some(chr)) => {
                    let last_char_idx = std::mem::replace(&mut self.char_idx, pos + 1);
                    self.pos = Either::Left(pos + 1);
                    return Some(Ok((last_char_idx, chr)));
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

#[derive(Copy, Clone, Debug, Trace)]
pub enum BufferMode {
    None,
    Line,
    Block,
}

impl BufferMode {
    fn new_input_buffer(&self, text: bool, is_input_port: bool) -> ByteVector {
        if !is_input_port {
            return ByteVector::new(Vec::new());
        }
        match self {
            Self::None if text => ByteVector::new(vec![0u8; 4]),
            Self::None => ByteVector::new(vec![0]),
            Self::Line | Self::Block => ByteVector::new(vec![0u8; BUFFER_SIZE]),
        }
    }

    fn new_output_buffer(&self, is_output_port: bool) -> ByteVector {
        if !is_output_port {
            return ByteVector::new(Vec::new());
        }
        match self {
            Self::None => ByteVector::new(Vec::new()),
            Self::Line | Self::Block => ByteVector::new(Vec::with_capacity(BUFFER_SIZE)),
        }
    }

    fn to_sym(self) -> Symbol {
        match self {
            Self::None => Symbol::intern("none"),
            Self::Line => Symbol::intern("line"),
            Self::Block => Symbol::intern("block"),
        }
    }
}

impl SchemeCompatible for BufferMode {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "buffer-mode", sealed: true, opaque: true)
    }
}

#[bridge(name = "buffer-mode", lib = "(rnrs io ports (6))")]
pub fn buffer_mode(mode: &Value) -> Result<Vec<Value>, Condition> {
    let sym: Symbol = mode.clone().try_into()?;
    let mode = match &*sym.to_str() {
        "line" => BufferMode::Line,
        "block" => BufferMode::Block,
        _ => BufferMode::None,
    };
    Ok(vec![Value::from(Record::from_rust_type(mode))])
}

#[derive(Copy, Clone, Trace)]
pub struct Transcoder {
    codec: Codec,
    eol_type: EolStyle,
    error_handling_mode: ErrorHandlingMode,
}

impl fmt::Debug for Transcoder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            " {:?} {:?} {:?}",
            self.codec, self.eol_type, self.error_handling_mode
        )
    }
}

impl Transcoder {
    pub fn native() -> Self {
        Self {
            codec: Codec::Utf8,
            eol_type: EolStyle::None,
            error_handling_mode: ErrorHandlingMode::Replace,
        }
    }
}

impl SchemeCompatible for Transcoder {
    fn rtd() -> Arc<RecordTypeDescriptor> {
        rtd!(name: "transcoder", opaque: true, sealed: true)
    }
}

#[bridge(name = "native-transcoder", lib = "(rnrs io ports (6))")]
pub fn native_transcoder() -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(Record::from_rust_type(
        Transcoder::native(),
    ))])
}

#[derive(Copy, Clone, Trace)]
pub enum Codec {
    Latin1,
    Utf8,
    Utf16,
}

impl fmt::Debug for Codec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Latin1 => write!(f, "latin-1"),
            Self::Utf8 => write!(f, "utf-8"),
            Self::Utf16 => write!(f, "utf-16"),
        }
    }
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

#[derive(Copy, Clone, Trace)]
pub enum EolStyle {
    /// None
    None,
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

impl fmt::Debug for EolStyle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Lf => write!(f, "lf"),
            Self::Cr => write!(f, "cr"),
            Self::Crlf => write!(f, "crlf"),
            Self::Nel => write!(f, "nel"),
            Self::Crnel => write!(f, "crnel"),
            Self::Ls => write!(f, "ls"),
        }
    }
}

impl EolStyle {
    #[maybe_async]
    fn convert_eol_style_to_linefeed_inner(
        self,
        iter: &mut Peekable<impl MaybeStream<Item = Result<(usize, char), Condition>>>,
    ) -> Option<Result<(usize, char), Condition>> {
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
        mut iter: Peekable<impl Iterator<Item = Result<(usize, char), Condition>>>,
    ) -> impl Iterator<Item = Result<(usize, char), Condition>> {
        std::iter::from_fn(move || self.convert_eol_style_to_linefeed_inner(&mut iter))
    }

    #[cfg(feature = "async")]
    fn convert_eol_style_to_linefeed(
        self,
        mut iter: Peekable<impl MaybeStream<Item = Result<(usize, char), Condition>>>,
    ) -> impl futures::stream::Stream<Item = Result<(usize, char), Condition>> {
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
                    Self::None => [Some(chr), None],
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
            Self::None | Self::Ls => None,
        }
    }
}

#[derive(Copy, Clone, Trace)]
pub enum ErrorHandlingMode {
    Ignore,
    Raise,
    Replace,
}

impl fmt::Debug for ErrorHandlingMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ignore => write!(f, "ignore"),
            Self::Raise => write!(f, "raise"),
            Self::Replace => write!(f, "replace"),
        }
    }
}

#[cfg(not(feature = "async"))]
mod __impl {
    pub(super) use std::{
        io::{Read, Seek, SeekFrom, Write},
        iter::{Iterator as MaybeStream, Peekable},
        sync::Mutex,
    };

    use super::*;

    pub(super) type ReadFn = Box<
        dyn Fn(&mut dyn Any, &ByteVector, usize, usize) -> Result<usize, Condition> + Send + Sync,
    >;
    pub(super) type WriteFn =
        Box<dyn Fn(&mut dyn Any, &ByteVector, usize, usize) -> Result<(), Condition> + Send + Sync>;
    pub(super) type GetPosFn = Box<dyn Fn(&mut dyn Any) -> Result<u64, Condition> + Send + Sync>;
    pub(super) type SetPosFn =
        Box<dyn Fn(&mut dyn Any, u64) -> Result<(), Condition> + Send + Sync>;
    pub(super) type CloseFn = Box<dyn Fn(&mut dyn Any) -> Result<(), Condition> + Send + Sync>;

    pub(super) fn read_fn<T>() -> ReadFn
    where
        T: Read + Any + Send + 'static,
    {
        Box::new(|any, buff, start, count| {
            let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
            let mut buff = buff.as_mut_slice();
            concrete
                .read(&mut buff[start..(start + count)])
                .map_err(|err| Condition::io_read_error(format!("{err:?}")))
        })
    }

    pub(super) fn write_fn<T>() -> WriteFn
    where
        T: Write + Any + Send + 'static,
    {
        Box::new(|any, buff, start, count| {
            let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
            let buff = buff.as_slice();
            concrete
                .write_all(&buff[start..(start + count)])
                .and_then(|()| concrete.flush())
                .map_err(|err| Condition::io_write_error(format!("{err:?}")))?;
            Ok(())
        })
    }

    pub(super) fn get_pos_fn<T>() -> GetPosFn
    where
        T: Seek + Any + Send + 'static,
    {
        Box::new(|any| {
            let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
            concrete
                .stream_position()
                .map_err(|err| Condition::io_error(format!("{err:?}")))
        })
    }

    pub(super) fn set_pos_fn<T>() -> SetPosFn
    where
        T: Seek + Any + Send + 'static,
    {
        Box::new(|any, pos| {
            let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
            let _ = concrete
                .seek(SeekFrom::Start(pos))
                .map_err(|err| Condition::io_error(format!("{err:?}")))?;
            Ok(())
        })
    }

    pub(super) fn proc_to_read_fn(read: Procedure) -> ReadFn {
        Box::new(move |_, buff, start, count| {
            let [read] = read
                .call(&[
                    Value::from(buff.clone()),
                    Value::from(start),
                    Value::from(count),
                ])
                .map_err(|err| err.into_inner().add_condition(IoReadError::new()))?
                .try_into()
                .map_err(|_| {
                    Condition::io_read_error(
                        "invalid number of values returned from read procedure",
                    )
                })?;
            let read: usize = read.try_into().map_err(|_| {
                Condition::io_read_error("could not convert read procedure return value to usize")
            })?;
            Ok(read)
        })
    }

    pub(super) fn proc_to_get_pos_fn(get_pos: Procedure) -> GetPosFn {
        Box::new(move |_| {
            let [pos] = get_pos
                .call(&[])
                .map_err(|err| err.into_inner().add_condition(IoError::new()))?
                .try_into()
                .map_err(|_| {
                    Condition::io_error("invalid number of values returned get-pos procedure")
                })?;
            let pos: u64 = pos.try_into().map_err(|_| {
                Condition::io_read_error("could not convert get-pos procedure return value to u64")
            })?;
            Ok(pos)
        })
    }

    pub(super) fn proc_to_set_pos_fn(set_pos: Procedure) -> SetPosFn {
        Box::new(move |_, pos| {
            let _ = set_pos
                .call(&[Value::from(pos)])
                .map_err(|err| err.into_inner().add_condition(IoError::new()))?;
            Ok(())
        })
    }

    pub(super) fn proc_to_close_fn(close: Procedure) -> CloseFn {
        Box::new(move |_| {
            let _ = close
                .call(&[])
                .map_err(|err| err.into_inner().add_condition(IoError::new()))?;
            Ok(())
        })
    }

    impl<D> Iterator for Decoder<'_, D>
    where
        D: Decode,
    {
        type Item = Result<(usize, char), Condition>;

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

        fn seek_fns() -> Option<(GetPosFn, SetPosFn)> {
            Some((get_pos_fn::<Self>(), set_pos_fn::<Self>()))
        }
    }

    impl IntoPort for std::io::Stdin {
        fn read_fn() -> Option<ReadFn> {
            Some(read_fn::<Self>())
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
    pub(super) use std::{io::SeekFrom, pin::Pin};
    use tokio::io::{AsyncRead, AsyncReadExt, AsyncSeek, AsyncSeekExt, AsyncWrite, AsyncWriteExt};
    #[cfg(feature = "tokio")]
    pub(super) use tokio::sync::Mutex;

    use super::*;

    pub(super) type ReadFn = Box<
        dyn for<'a> Fn(
                &'a mut (dyn Any + Send),
                &'a ByteVector,
                usize,
                usize,
            ) -> BoxFuture<'a, Result<usize, Condition>>
            + Send
            + Sync,
    >;
    pub(super) type WriteFn = Box<
        dyn for<'a> Fn(
                &'a mut (dyn Any + Send),
                &'a ByteVector,
                usize,
                usize,
            ) -> BoxFuture<'a, Result<(), Condition>>
            + Send
            + Sync,
    >;
    pub(super) type GetPosFn = Box<
        dyn for<'a> Fn(&'a mut (dyn Any + Send)) -> BoxFuture<'a, Result<u64, Condition>>
            + Send
            + Sync,
    >;
    pub(super) type SetPosFn = Box<
        dyn for<'a> Fn(&'a mut (dyn Any + Send), u64) -> BoxFuture<'a, Result<(), Condition>>
            + Send
            + Sync,
    >;
    pub(super) type CloseFn = Box<
        dyn for<'a> Fn(&'a mut (dyn Any + Send)) -> BoxFuture<'a, Result<(), Condition>>
            + Send
            + Sync,
    >;

    // Annoyingly, we need to double up our buffers here because we put
    // Bytevectors behind a non-async compatible rwlock. We _could_ put them
    // behind an async compatible one, but I'm not sure that's worthwhile.

    pub(super) fn read_fn<T>() -> ReadFn
    where
        T: AsyncRead + Any + Send + 'static,
    {
        // let mut local_buff = Vec::with_capacity(1024);
        Box::new(move |any, buff, start, count| {
            Box::pin(async move {
                let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
                let mut concrete: Pin<&mut T> = pin!(concrete);
                let mut local_buff = vec![0u8; count];
                let read = concrete
                    .read(&mut local_buff) //  buff[start..(start + count)])
                    .await
                    .map_err(|err| Condition::io_read_error(format!("{err:?}")))?;
                buff.as_mut_slice()[start..(start + count)].copy_from_slice(&local_buff);

                Ok(read)
            })
        })
    }

    pub(super) fn write_fn<T>() -> WriteFn
    where
        T: AsyncWrite + Any + Send + 'static,
    {
        Box::new(|any, buff, start, count| {
            Box::pin(async move {
                let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
                let mut concrete: Pin<&mut T> = pin!(concrete);
                let local_buff = buff.as_slice()[start..(start + count)].to_vec();
                concrete
                    .write_all(&local_buff)
                    .await
                    .map_err(|err| Condition::io_write_error(format!("{err:?}")))?;
                concrete
                    .flush()
                    .await
                    .map_err(|err| Condition::io_write_error(format!("{err:?}")))?;
                Ok(())
            })
        })
    }

    pub(super) fn get_pos_fn<T>() -> GetPosFn
    where
        T: AsyncSeek + Any + Send + 'static,
    {
        Box::new(|any| {
            Box::pin(async move {
                let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
                let mut concrete: Pin<&mut T> = pin!(concrete);
                concrete
                    .stream_position()
                    .await
                    .map_err(|err| Condition::io_error(format!("{err:?}")))
            })
        })
    }

    pub(super) fn set_pos_fn<T>() -> SetPosFn
    where
        T: AsyncSeek + Any + Send + 'static,
    {
        Box::new(|any, pos| {
            Box::pin(async move {
                let concrete = unsafe { any.downcast_mut::<T>().unwrap_unchecked() };
                let mut concrete: Pin<&mut T> = pin!(concrete);
                let _ = concrete
                    .seek(SeekFrom::Start(pos))
                    .await
                    .map_err(|err| Condition::io_error(format!("{err:?}")))?;
                Ok(())
            })
        })
    }

    pub(super) fn proc_to_read_fn(read: Procedure) -> ReadFn {
        Box::new(move |_, buff, start, count| {
            let read = read.clone();
            Box::pin(async move {
                let [read] = read
                    .call(&[
                        Value::from(buff.clone()),
                        Value::from(start),
                        Value::from(count),
                    ])
                    .await
                    .map_err(|err| err.into_inner().add_condition(IoReadError::new()))?
                    .try_into()
                    .map_err(|_| {
                        Condition::io_read_error(
                            "invalid number of values returned from read procedure",
                        )
                    })?;
                let read: usize = read.try_into().map_err(|_| {
                    Condition::io_read_error(
                        "could not convert read procedure return value to usize",
                    )
                })?;
                Ok(read)
            })
        })
    }

    pub(super) fn proc_to_get_pos_fn(get_pos: Procedure) -> GetPosFn {
        Box::new(move |_| {
            let get_pos = get_pos.clone();
            Box::pin(async move {
                let [pos] = get_pos
                    .call(&[])
                    .await
                    .map_err(|err| err.into_inner().add_condition(IoError::new()))?
                    .try_into()
                    .map_err(|_| {
                        Condition::io_error("invalid number of values returned get-pos procedure")
                    })?;
                let pos: u64 = pos.try_into().map_err(|_| {
                    Condition::io_read_error(
                        "could not convert get-pos procedure return value to u64",
                    )
                })?;
                Ok(pos)
            })
        })
    }

    pub(super) fn proc_to_set_pos_fn(set_pos: Procedure) -> SetPosFn {
        Box::new(move |_, pos| {
            let set_pos = set_pos.clone();
            Box::pin(async move {
                let _ = set_pos
                    .call(&[Value::from(pos)])
                    .await
                    .map_err(|err| err.into_inner().add_condition(IoError::new()))?;
                Ok(())
            })
        })
    }

    pub(super) fn proc_to_close_fn(close: Procedure) -> CloseFn {
        Box::new(move |_| {
            let close = close.clone();
            Box::pin(async move {
                let _ = close
                    .call(&[])
                    .await
                    .map_err(|err| err.into_inner().add_condition(IoError::new()))?;
                Ok(())
            })
        })
    }

    #[cfg(feature = "tokio")]
    impl IntoPort for tokio::fs::File {
        fn read_fn() -> Option<ReadFn> {
            Some(read_fn::<Self>())
        }

        fn write_fn() -> Option<WriteFn> {
            Some(write_fn::<Self>())
        }

        fn seek_fns() -> Option<(GetPosFn, SetPosFn)> {
            Some((get_pos_fn::<Self>(), set_pos_fn::<Self>()))
        }
    }

    #[cfg(feature = "tokio")]
    impl IntoPort for tokio::io::Stdin {
        fn read_fn() -> Option<ReadFn> {
            Some(read_fn::<Self>())
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
    pub(crate) info: PortInfo,
    pub(crate) data: Mutex<PortData>,
}

impl PortInner {
    fn new<D, P>(id: D, port: P, buffer_mode: BufferMode, transcoder: Option<Transcoder>) -> Self
    where
        D: fmt::Display,
        P: IntoPort,
    {
        let read = P::read_fn();
        let write = P::write_fn();
        let seek = P::seek_fns();
        let close = P::close_fn();

        Self::new_with_fns(
            id,
            port.into_port(),
            read,
            write,
            seek,
            close,
            buffer_mode,
            transcoder,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn new_with_fns<D>(
        id: D,
        port: PortBox,
        read: Option<ReadFn>,
        write: Option<WriteFn>,
        seek: Option<(GetPosFn, SetPosFn)>,
        close: Option<CloseFn>,
        buffer_mode: BufferMode,
        transcoder: Option<Transcoder>,
    ) -> Self
    where
        D: fmt::Display,
    {
        let is_read = read.is_some();
        let is_write = write.is_some();

        Self {
            info: PortInfo::BinaryPort(BinaryPortInfo {
                id: id.to_string(),
                read,
                write,
                seek,
                close,
                buffer_mode,
                transcoder,
            }),
            data: Mutex::new(PortData::BinaryPort(BinaryPortData {
                port: Some(port),
                input_pos: 0,
                bytes_read: 0,
                input_buffer: buffer_mode.new_input_buffer(transcoder.is_some(), is_read),
                output_buffer: buffer_mode.new_output_buffer(is_write),
                utf16_endianness: None,
            })),
        }
    }
}

#[cfg(not(feature = "async"))]
type PortBox = Box<dyn Any + 'static>;

#[cfg(feature = "async")]
type PortBox = Box<dyn Any + Send + Sync + 'static>;

/// Immutable data describing the binary port.
pub(crate) struct BinaryPortInfo {
    id: String,
    read: Option<ReadFn>,
    write: Option<WriteFn>,
    seek: Option<(GetPosFn, SetPosFn)>,
    close: Option<CloseFn>,
    buffer_mode: BufferMode,
    transcoder: Option<Transcoder>,
}

/// Mutable data contained in the binary port.
pub(crate) struct BinaryPortData {
    port: Option<PortBox>,
    input_pos: usize,
    bytes_read: usize,
    input_buffer: ByteVector,
    output_buffer: ByteVector,
    utf16_endianness: Option<Endianness>,
}

pub const BUFFER_SIZE: usize = 8192;

impl BinaryPortData {
    #[maybe_async]
    fn read_byte(&mut self, port_info: &BinaryPortInfo) -> Result<Option<u8>, Condition> {
        let next_byte = maybe_await!(self.peekn_bytes(port_info, 0))?;
        maybe_await!(self.consume_bytes(port_info, 1))?;
        Ok(next_byte)
    }

    #[maybe_async]
    fn read_char(&mut self, port_info: &BinaryPortInfo) -> Result<Option<char>, Condition> {
        let Some(next_char) = maybe_await!(self.peekn_chars(port_info, 0))? else {
            return Ok(None);
        };

        let Some(transcoder) = port_info.transcoder else {
            return Err(Condition::io_read_error("not a text port"));
        };

        let byte_len = transcoder.codec.byte_len(next_char);
        maybe_await!(self.consume_bytes(port_info, byte_len))?;

        Ok(Some(next_char))
    }

    #[maybe_async]
    fn peekn_bytes(
        &mut self,
        port_info: &BinaryPortInfo,
        n: usize,
    ) -> Result<Option<u8>, Condition> {
        let Some(read) = port_info.read.as_ref() else {
            return Err(Condition::io_read_error("not an input port"));
        };

        let Some(port) = self.port.as_deref_mut() else {
            return Err(Condition::io_read_error("port is closed"));
        };

        if let Some(write) = port_info.write.as_ref()
            && let len = self.output_buffer.len()
            && len != 0
        {
            maybe_await!(write(port, &self.output_buffer, 0, len))?;
            self.output_buffer.clear();
        }

        if n + self.input_pos > self.input_buffer.len() {
            panic!("attempt to lookahead further than the buffer allows");
        }

        while self.bytes_read <= n + self.input_pos {
            match (port_info.buffer_mode, port_info.transcoder) {
                (BufferMode::None, _) => {
                    let read = maybe_await!((read)(port, &self.input_buffer, self.bytes_read, 1))?;
                    if read == 1 {
                        self.bytes_read += 1;
                    } else {
                        return Ok(None);
                    }
                }
                (BufferMode::Line, Some(transcoder)) => {
                    loop {
                        let count = self.input_buffer.len() - self.bytes_read;
                        let read =
                            maybe_await!((read)(port, &self.input_buffer, self.bytes_read, count))?;
                        if read == 0 {
                            return Ok(None);
                        }
                        // Attempt to find the line ending:
                        if transcoder
                            .eol_type
                            .find_next_line(
                                transcoder.codec.ls_needle(self.utf16_endianness),
                                &self.input_buffer.as_slice()
                                    [self.bytes_read..(self.bytes_read + read)],
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
                            self.input_buffer
                                .0
                                .vec
                                .write()
                                .extend(std::iter::repeat_n(0u8, BUFFER_SIZE));
                        }
                    }
                }
                (BufferMode::Line | BufferMode::Block, _) => {
                    let count = self.input_buffer.len() - self.bytes_read;
                    let read =
                        maybe_await!((read)(port, &self.input_buffer, self.bytes_read, count))?;
                    if read == 0 {
                        return Ok(None);
                    }
                    self.bytes_read += read;
                }
            }
        }

        Ok(self.input_buffer.get(n + self.input_pos))
    }

    #[cfg(not(feature = "async"))]
    fn transcode<'a>(
        &'a mut self,
        port_info: &'a BinaryPortInfo,
        transcoder: Transcoder,
    ) -> impl Iterator<Item = Result<(usize, char), Condition>> + use<'a> {
        let eol_type = transcoder.eol_type;
        let decoder = match transcoder.codec {
            Codec::Latin1 => {
                let mut i = 0;
                Box::new(std::iter::from_fn(move || {
                    match self.peekn_bytes(port_info, i) {
                        Ok(Some(byte)) => {
                            let res = (i, char::from(byte));
                            i += 1;
                            Some(Ok(res))
                        }
                        Ok(None) => None,
                        Err(err) => Some(Err(err)),
                    }
                })) as Box<dyn Iterator<Item = Result<(usize, char), Condition>>>
            }
            Codec::Utf16 => Box::new(Decoder::new(
                self,
                port_info,
                Utf16Buffer::new(
                    transcoder.error_handling_mode,
                    self.utf16_endianness.unwrap(),
                ),
            )),
            Codec::Utf8 => Box::new(Decoder::new(
                self,
                port_info,
                Utf8Buffer::new(transcoder.error_handling_mode),
            )),
        };
        eol_type.convert_eol_style_to_linefeed(decoder.peekable())
    }

    #[cfg(feature = "async")]
    fn transcode<'a>(
        &'a mut self,
        port_info: &'a BinaryPortInfo,
        transcoder: Transcoder,
    ) -> impl futures::stream::Stream<Item = Result<(usize, char), Condition>> + use<'a> {
        let eol_type = transcoder.eol_type;
        let decoder = match transcoder.codec {
            Codec::Latin1 => async_stream::stream! {
                let mut i = 0;
                loop {
                    match self.peekn_bytes(port_info, i).await {
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
                    port_info,
                    Utf16Buffer::new(
                        transcoder.error_handling_mode,
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
                    port_info,
                    Utf8Buffer::new(transcoder.error_handling_mode),
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
    pub(crate) fn peekn_chars(
        &mut self,
        port_info: &BinaryPortInfo,
        n: usize,
    ) -> Result<Option<char>, Condition> {
        let Some(transcoder) = port_info.transcoder else {
            return Err(Condition::io_read_error("not a text port"));
        };

        // If this is a utf16 port and we have not assigned endiannes, check for
        // the BOM
        if matches!(transcoder.codec, Codec::Utf16) && self.utf16_endianness.is_none() {
            let b1 = maybe_await!(self.peekn_bytes(port_info, 0))?;
            let b2 = maybe_await!(self.peekn_bytes(port_info, 1))?;
            self.utf16_endianness = match (b1, b2) {
                (Some(b'\xFF'), Some(b'\xFE')) => {
                    maybe_await!(self.consume_bytes(port_info, 2))?;
                    Some(Endianness::Le)
                }
                (Some(b'\xFE'), Some(b'\xFF')) => {
                    maybe_await!(self.consume_bytes(port_info, 2))?;
                    Some(Endianness::Be)
                }
                _ => Some(Endianness::Le),
            };
        }

        Ok(maybe_await!(self.transcode(port_info, transcoder).nth(n))
            .transpose()?
            .map(|(_, chr)| chr))
    }

    #[maybe_async]
    fn consume_bytes(&mut self, port_info: &BinaryPortInfo, n: usize) -> Result<(), Condition> {
        if self.bytes_read < self.input_pos + n {
            let _ =
                maybe_await!(self.peekn_bytes(port_info, self.input_pos + n - self.bytes_read))?;
        }

        self.input_pos += n;

        if self.input_pos >= self.input_buffer.len() {
            self.input_pos -= self.input_buffer.len();
            self.bytes_read = 0;
        }

        Ok(())
    }

    #[maybe_async]
    pub(crate) fn consume_chars(
        &mut self,
        port_info: &BinaryPortInfo,
        n: usize,
    ) -> Result<(), Condition> {
        let Some(transcoder) = port_info.transcoder else {
            return Err(Condition::io_read_error("not a text port"));
        };

        let Some((bytes_to_skip, last_char)) =
            maybe_await!(self.transcode(port_info, transcoder).take(n).last()).transpose()?
        else {
            return Ok(());
        };

        maybe_await!(self.consume_bytes(
            port_info,
            bytes_to_skip + transcoder.codec.byte_len(last_char)
        ))?;

        if self.input_buffer.len() - self.input_pos < 4 {
            self.input_buffer
                .as_mut_slice()
                .copy_within(self.input_pos.., 0);
            self.bytes_read -= self.input_pos;
            self.input_pos = 0;
        }
        Ok(())
    }

    #[maybe_async]
    fn put_bytes(&mut self, port_info: &BinaryPortInfo, mut bytes: &[u8]) -> Result<(), Condition> {
        let Some(write) = port_info.write.as_ref() else {
            return Err(Condition::io_write_error("not an output port"));
        };

        let Some(port) = self.port.as_deref_mut() else {
            return Err(Condition::io_write_error("port is closed"));
        };

        // If we can, seek back
        if let Some((get_pos, set_pos)) = port_info.seek.as_ref()
            && self.bytes_read > 0
        {
            let curr_pos = maybe_await!(get_pos(port))
                .map_err(|err| err.add_condition(IoWriteError::new()))?;
            let seek_to = curr_pos - (self.bytes_read as u64 - self.input_pos as u64);
            maybe_await!(set_pos(port, seek_to))
                .map_err(|err| err.add_condition(IoWriteError::new()))?;
            self.bytes_read = 0;
            self.input_pos = 0;
        }

        match (port_info.buffer_mode, port_info.transcoder) {
            (BufferMode::None, _) => {
                for byte in bytes {
                    {
                        self.output_buffer.as_mut_slice()[0] = *byte;
                    }
                    maybe_await!(write(port, &self.output_buffer, 0, 1))?;
                    self.output_buffer.as_mut_vec().clear();
                }
            }
            (BufferMode::Line, Some(transcoder)) => loop {
                if let Some(next_line) = transcoder
                    .eol_type
                    .find_next_line(transcoder.codec.ls_needle(self.utf16_endianness), bytes)
                {
                    self.output_buffer
                        .as_mut_vec()
                        .extend_from_slice(&bytes[..next_line]);
                    bytes = &bytes[next_line..];
                    maybe_await!(write(
                        port,
                        &self.output_buffer,
                        0,
                        self.output_buffer.len()
                    ))?;
                    self.output_buffer.clear();
                } else {
                    self.output_buffer.as_mut_vec().extend_from_slice(bytes);
                    break;
                }
            },
            (BufferMode::Line | BufferMode::Block, _) => loop {
                if bytes.len() + self.output_buffer.len() >= BUFFER_SIZE {
                    let num_bytes_to_buffer = BUFFER_SIZE - self.output_buffer.len();
                    self.output_buffer
                        .as_mut_vec()
                        .extend_from_slice(&bytes[..num_bytes_to_buffer]);
                    bytes = &bytes[num_bytes_to_buffer..];
                    maybe_await!(write(
                        port,
                        &self.output_buffer,
                        0,
                        self.output_buffer.len()
                    ))?;
                    self.output_buffer.clear();
                } else {
                    self.output_buffer.as_mut_vec().extend_from_slice(bytes);
                    break;
                }
            },
        }

        Ok(())
    }

    #[maybe_async]
    fn put_str(&mut self, port_info: &BinaryPortInfo, s: &str) -> Result<(), Condition> {
        let Some(transcoder) = port_info.transcoder else {
            return Err(Condition::io_write_error("not a text port"));
        };

        let s = if matches!(transcoder.eol_type, EolStyle::Lf) {
            Cow::Borrowed(s)
        } else {
            Cow::Owned(
                transcoder
                    .eol_type
                    .convert_linefeeds_to_eol_style(s.chars())
                    .collect::<String>(),
            )
        };
        match transcoder.codec {
            Codec::Latin1 | Codec::Utf8 => {
                // Probably should do a check here to ensure the string is ascii
                // if our codec is latin1
                maybe_await!(self.put_bytes(port_info, s.as_bytes()))?;
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
                maybe_await!(self.put_bytes(port_info, &bytes))?;
            }
        }
        Ok(())
    }

    #[maybe_async]
    fn flush(&mut self, port_info: &BinaryPortInfo) -> Result<(), Condition> {
        let Some(write) = port_info.write.as_ref() else {
            return Err(Condition::io_write_error("not an output port"));
        };

        let Some(port) = self.port.as_deref_mut() else {
            return Err(Condition::io_write_error("port is closed"));
        };

        maybe_await!(write(
            port,
            &self.output_buffer,
            0,
            self.output_buffer.len()
        ))?;
        self.output_buffer.clear();

        Ok(())
    }

    #[maybe_async]
    fn get_pos(&mut self, port_info: &BinaryPortInfo) -> Result<u64, Condition> {
        let Some((get_pos, _)) = port_info.seek.as_ref() else {
            return Err(Condition::io_error("port does not support get-pos"));
        };

        let Some(port) = self.port.as_deref_mut() else {
            return Err(Condition::io_error("port is closed"));
        };

        maybe_await!(get_pos(port))
    }

    #[maybe_async]
    fn set_pos(&mut self, port_info: &BinaryPortInfo, pos: u64) -> Result<(), Condition> {
        let Some((_, set_pos)) = port_info.seek.as_ref() else {
            return Err(Condition::io_error("port does not support set-pos!"));
        };

        let Some(port) = self.port.as_deref_mut() else {
            return Err(Condition::io_error("port is closed"));
        };

        maybe_await!(set_pos(port, pos))
    }

    #[maybe_async]
    fn close(&mut self, port_info: &BinaryPortInfo) -> Result<(), Condition> {
        let port = self.port.take();

        if let Some(mut port) = port {
            if let Some(write) = port_info.write.as_ref() {
                maybe_await!(write(
                    &mut port,
                    &self.output_buffer,
                    0,
                    self.output_buffer.len()
                ))?;
            }

            if let Some(close) = port_info.close.as_ref() {
                maybe_await!((close)(&mut port))?;
            }
        }

        Ok(())
    }
}

/// Immutable data describing a CustomStringPort
pub(crate) struct CustomTextualPortInfo {
    id: String,
    read: Option<Procedure>,
    write: Option<Procedure>,
    seek: Option<(Procedure, Procedure)>,
    #[expect(unused)]
    close: Option<Procedure>,
    buffer_mode: BufferMode,
}

/// Mutable data contained in a CustomStringPort
pub(crate) struct CustomTextualPortData {
    input_pos: usize,
    chars_read: usize,
    input_buffer: WideString,
    output_buffer: WideString,
}

impl CustomTextualPortData {
    #[maybe_async]
    fn peekn_chars(
        &mut self,
        port_info: &CustomTextualPortInfo,
        n: usize,
    ) -> Result<Option<char>, Condition> {
        let Some(read) = port_info.read.as_ref() else {
            return Err(Condition::io_read_error("not an input port"));
        };

        if let Some(write) = port_info.write.as_ref()
            && let len = self.output_buffer.len()
            && len != 0
        {
            maybe_await!(write.call(&[
                Value::from(self.output_buffer.clone()),
                Value::from(0usize),
                Value::from(len)
            ]))?;
            self.output_buffer.clear();
        }

        if n + self.input_pos > self.input_buffer.len() {
            panic!("attempt to lookahead further than the buffer allows")
        }

        while self.chars_read <= n + self.input_pos {
            let (start, count) = match port_info.buffer_mode {
                BufferMode::None => (0, 1),
                BufferMode::Line | BufferMode::Block => {
                    (self.chars_read, self.input_buffer.len() - self.chars_read)
                }
            };
            let read: usize = maybe_await!(read.call(&[
                Value::from(self.input_buffer.clone()),
                Value::from(start),
                Value::from(count)
            ]))?[0]
                .clone()
                .try_into()?;

            if read == 0 {
                return Ok(None);
            }
            self.chars_read += read;
        }

        Ok(self.input_buffer.get(n + self.input_pos))
    }

    #[maybe_async]
    fn read_char(&mut self, port_info: &CustomTextualPortInfo) -> Result<Option<char>, Condition> {
        let next_chr = maybe_await!(self.peekn_chars(port_info, 0))?;
        maybe_await!(self.consume_chars(port_info, 1))?;
        Ok(next_chr)
    }

    #[maybe_async]
    fn consume_chars(
        &mut self,
        port_info: &CustomTextualPortInfo,
        n: usize,
    ) -> Result<(), Condition> {
        if self.chars_read < self.input_pos + n {
            let _ =
                maybe_await!(self.peekn_chars(port_info, self.input_pos + n - self.chars_read))?;
        }

        self.input_pos += n;

        if self.input_pos >= self.input_buffer.len() {
            self.input_pos -= self.input_buffer.len();
            self.chars_read = 0;
        }

        Ok(())
    }

    #[maybe_async]
    fn put_str(&mut self, port_info: &CustomTextualPortInfo, s: &str) -> Result<(), Condition> {
        let Some(write) = port_info.write.as_ref() else {
            return Err(Condition::io_write_error("not an output port"));
        };

        // If we can, seek back
        if let Some((get_pos, set_pos)) = port_info.seek.as_ref()
            && self.chars_read > 0
        {
            let curr_pos: u64 = maybe_await!(get_pos.call(&[]))?[0]
                .clone()
                .try_into()
                .map_err(|err: Condition| err.add_condition(IoWriteError::new()))?;
            let seek_to = curr_pos - (self.chars_read as u64 - self.input_pos as u64);
            maybe_await!(set_pos.call(&[Value::from(seek_to)]))?;
            self.chars_read = 0;
            self.input_pos = 0;
        }

        match port_info.buffer_mode {
            BufferMode::None => {
                for chr in s.chars() {
                    {
                        self.output_buffer.0.chars.write()[0] = chr;
                    }
                    maybe_await!(write.call(&[
                        Value::from(self.output_buffer.clone()),
                        Value::from(0usize),
                        Value::from(1usize)
                    ]))?;
                }
            }
            BufferMode::Line => {
                let mut lines = s.lines().peekable();
                while let Some(line) = lines.next() {
                    {
                        let mut output_buffer = self.output_buffer.0.chars.write();
                        output_buffer.extend(line.chars());
                        if lines.peek().is_some() {
                            output_buffer.push('\n');
                        }
                        if !output_buffer.ends_with(&['\n']) {
                            break;
                        }
                    }
                    let len = self.output_buffer.len();
                    maybe_await!(write.call(&[
                        Value::from(self.output_buffer.clone()),
                        Value::from(0usize),
                        Value::from(len)
                    ]))?;
                    self.output_buffer.clear();
                }
            }
            BufferMode::Block => {
                for chr in s.chars() {
                    let len = self.output_buffer.len();
                    if len >= BUFFER_SIZE {
                        maybe_await!(write.call(&[
                            Value::from(self.output_buffer.clone()),
                            Value::from(0usize),
                            Value::from(len)
                        ]))?;
                        self.output_buffer.clear();
                    }
                    self.output_buffer.0.chars.write().push(chr);
                }
            }
        }

        Ok(())
    }
}

pub(crate) enum PortInfo {
    BinaryPort(BinaryPortInfo),
    #[expect(unused)]
    CustomTextualPort(CustomTextualPortInfo),
}

pub(crate) enum PortData {
    BinaryPort(BinaryPortData),
    #[expect(unused)]
    CustomTextualPort(CustomTextualPortData),
}

impl PortData {
    #[allow(dead_code)]
    #[maybe_async]
    fn read_byte(&mut self, port_info: &PortInfo) -> Result<Option<u8>, Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.read_byte(port_info))
            }
            (Self::CustomTextualPort(_), PortInfo::CustomTextualPort(_)) => {
                Err(Condition::io_read_error("not a binary port"))
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    pub(crate) fn read_char(&mut self, port_info: &PortInfo) -> Result<Option<char>, Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.read_char(port_info))
            }
            (Self::CustomTextualPort(port_data), PortInfo::CustomTextualPort(port_info)) => {
                maybe_await!(port_data.read_char(port_info))
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    fn peekn_bytes(&mut self, port_info: &PortInfo, n: usize) -> Result<Option<u8>, Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.peekn_bytes(port_info, n))
            }
            (Self::CustomTextualPort(_), PortInfo::CustomTextualPort(_)) => {
                Err(Condition::io_read_error("not a binary port"))
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    pub(crate) fn peekn_chars(
        &mut self,
        port_info: &PortInfo,
        n: usize,
    ) -> Result<Option<char>, Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.peekn_chars(port_info, n))
            }
            (Self::CustomTextualPort(port_data), PortInfo::CustomTextualPort(port_info)) => {
                maybe_await!(port_data.peekn_chars(port_info, n))
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    fn consume_bytes(&mut self, port_info: &PortInfo, n: usize) -> Result<(), Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.consume_bytes(port_info, n))
            }
            (Self::CustomTextualPort(_), PortInfo::CustomTextualPort(_)) => {
                Err(Condition::io_read_error("not a binary port"))
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    pub(crate) fn consume_chars(
        &mut self,
        port_info: &PortInfo,
        n: usize,
    ) -> Result<(), Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.consume_chars(port_info, n))
            }
            (Self::CustomTextualPort(port_data), PortInfo::CustomTextualPort(port_info)) => {
                maybe_await!(port_data.consume_chars(port_info, n))
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    fn put_bytes(&mut self, port_info: &PortInfo, bytes: &[u8]) -> Result<(), Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.put_bytes(port_info, bytes))
            }
            (Self::CustomTextualPort(_), PortInfo::CustomTextualPort(_)) => {
                Err(Condition::io_read_error("not a binary port"))
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    fn put_str(&mut self, port_info: &PortInfo, s: &str) -> Result<(), Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.put_str(port_info, s))
            }
            (Self::CustomTextualPort(port_data), PortInfo::CustomTextualPort(port_info)) => {
                maybe_await!(port_data.put_str(port_info, s))
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    fn flush(&mut self, port_info: &PortInfo) -> Result<(), Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.flush(port_info))
            }
            (Self::CustomTextualPort(_port_data), PortInfo::CustomTextualPort(_port_info)) => {
                todo!()
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    fn get_pos(&mut self, port_info: &PortInfo) -> Result<u64, Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.get_pos(port_info))
            }
            (Self::CustomTextualPort(_port_data), PortInfo::CustomTextualPort(_port_info)) => {
                todo!()
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    fn set_pos(&mut self, port_info: &PortInfo, pos: u64) -> Result<(), Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.set_pos(port_info, pos))
            }
            (Self::CustomTextualPort(_port_data), PortInfo::CustomTextualPort(_port_info)) => {
                todo!()
            }
            _ => unreachable!(),
        }
    }

    #[maybe_async]
    fn close(&mut self, port_info: &PortInfo) -> Result<(), Condition> {
        match (self, port_info) {
            (Self::BinaryPort(port_data), PortInfo::BinaryPort(port_info)) => {
                maybe_await!(port_data.close(port_info))
            }
            (Self::CustomTextualPort(_port_data), PortInfo::CustomTextualPort(_port_info)) => {
                todo!()
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(not(feature = "async"))]
#[doc(hidden)]
pub trait IntoPortReqs: Sized + 'static {}

#[cfg(feature = "async")]
#[doc(hidden)]
pub trait IntoPortReqs: Send + Sync + Sized + 'static {}

#[cfg(not(feature = "async"))]
impl<T> IntoPortReqs for T where T: Sized + 'static {}

#[cfg(feature = "async")]
impl<T> IntoPortReqs for T where T: Send + Sync + Sized + 'static {}

pub trait IntoPort: IntoPortReqs {
    fn into_port(self) -> PortBox {
        Box::new(self)
    }

    fn read_fn() -> Option<ReadFn> {
        None
    }

    fn write_fn() -> Option<WriteFn> {
        None
    }

    fn seek_fns() -> Option<(GetPosFn, SetPosFn)> {
        None
    }

    fn close_fn() -> Option<CloseFn> {
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

    fn seek_fns() -> Option<(GetPosFn, SetPosFn)> {
        Some((get_pos_fn::<Self>(), set_pos_fn::<Self>()))
    }
}

#[derive(Trace, Clone)]
pub struct Port(pub(crate) Arc<PortInner>);

impl Port {
    pub fn new<D, P>(
        id: D,
        port: P,
        buffer_mode: BufferMode,
        transcoder: Option<Transcoder>,
    ) -> Self
    where
        D: fmt::Display,
        P: IntoPort,
    {
        Self(Arc::new(PortInner::new(id, port, buffer_mode, transcoder)))
    }

    #[allow(clippy::too_many_arguments)]
    fn new_with_fns<D>(
        id: D,
        port: PortBox,
        read: Option<ReadFn>,
        write: Option<WriteFn>,
        seek: Option<(GetPosFn, SetPosFn)>,
        close: Option<CloseFn>,
        buffer_mode: BufferMode,
        transcoder: Option<Transcoder>,
    ) -> Self
    where
        D: fmt::Display,
    {
        Self(Arc::new(PortInner::new_with_fns(
            id,
            port,
            read,
            write,
            seek,
            close,
            buffer_mode,
            transcoder,
        )))
    }

    pub fn id(&self) -> &str {
        match &self.0.info {
            PortInfo::BinaryPort(BinaryPortInfo { id, .. }) => id.as_str(),
            PortInfo::CustomTextualPort(CustomTextualPortInfo { id, .. }) => id.as_str(),
        }
    }

    pub fn transcoder(&self) -> Option<Transcoder> {
        match self.0.info {
            PortInfo::BinaryPort(BinaryPortInfo { transcoder, .. }) => transcoder,
            PortInfo::CustomTextualPort(_) => None,
        }
    }

    pub fn buffer_mode(&self) -> BufferMode {
        match self.0.info {
            PortInfo::BinaryPort(BinaryPortInfo { buffer_mode, .. }) => buffer_mode,
            PortInfo::CustomTextualPort(CustomTextualPortInfo { buffer_mode, .. }) => buffer_mode,
        }
    }

    pub fn is_textual_port(&self) -> bool {
        matches!(
            self.0.info,
            PortInfo::BinaryPort(BinaryPortInfo {
                transcoder: Some(_),
                ..
            }) | PortInfo::CustomTextualPort(_)
        )
    }

    pub fn is_input_port(&self) -> bool {
        matches!(
            self.0.info,
            PortInfo::BinaryPort(BinaryPortInfo { read: Some(_), .. })
                | PortInfo::CustomTextualPort(CustomTextualPortInfo { read: Some(_), .. })
        )
    }

    pub fn is_output_port(&self) -> bool {
        matches!(
            self.0.info,
            PortInfo::BinaryPort(BinaryPortInfo { write: Some(_), .. })
                | PortInfo::CustomTextualPort(CustomTextualPortInfo { write: Some(_), .. })
        )
    }

    #[maybe_async]
    pub fn get_u8(&self) -> Result<Option<u8>, Condition> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        // TODO: Ensure this is a binary port
        if let Some(byte) = maybe_await!(data.peekn_bytes(&self.0.info, 0))? {
            maybe_await!(data.consume_bytes(&self.0.info, 1))?;
            Ok(Some(byte))
        } else {
            Ok(None)
        }
    }

    #[maybe_async]
    pub fn lookahead_u8(&self) -> Result<Option<u8>, Condition> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        // TODO: Ensure this is a binary port
        maybe_await!(data.peekn_bytes(&self.0.info, 0))
    }

    #[maybe_async]
    pub fn get_char(&self) -> Result<Option<char>, Condition> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        if let Some(chr) = maybe_await!(data.peekn_chars(&self.0.info, 0))? {
            maybe_await!(data.consume_chars(&self.0.info, 1))?;
            Ok(Some(chr))
        } else {
            Ok(None)
        }
    }

    #[maybe_async]
    pub fn lookahead_char(&self) -> Result<Option<char>, Condition> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        maybe_await!(data.peekn_chars(&self.0.info, 0))
    }

    #[maybe_async]
    pub fn get_line(&self) -> Result<Option<String>, Condition> {
        let mut out = String::new();
        loop {
            match maybe_await!(self.get_char())? {
                Some('\n') => return Ok(Some(out)),
                Some(chr) => out.push(chr),
                None if out.is_empty() => return Ok(None),
                None => return Ok(Some(out)),
            }
        }
    }

    #[maybe_async]
    pub fn get_sexpr(&self, span: Span) -> Result<Option<(Syntax, Span)>, ParseSyntaxError> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        let mut parser = Parser::new(&mut data, &self.0.info, span);

        let sexpr_or_eof = maybe_await!(parser.get_sexpr_or_eof())?;
        let ending_span = parser.curr_span();

        Ok(sexpr_or_eof.map(|sexpr| (sexpr, ending_span)))
    }

    #[maybe_async]
    pub fn all_sexprs(&self, span: Span) -> Result<Vec<Syntax>, ParseSyntaxError> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        let mut parser = Parser::new(&mut data, &self.0.info, span);

        Ok(maybe_await!(parser.all_sexprs())?)
    }

    #[maybe_async]
    pub fn put_u8(&self, byte: u8) -> Result<(), Condition> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        // TODO: ensure this is not a textual port

        maybe_await!(data.put_bytes(&self.0.info, &[byte]))
    }

    #[maybe_async]
    pub fn put_char(&self, chr: char) -> Result<(), Condition> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        let mut buf: [u8; 4] = [0; 4];
        let s = chr.encode_utf8(&mut buf);

        maybe_await!(data.put_str(&self.0.info, s))
    }

    #[maybe_async]
    pub fn put_str(&self, s: &str) -> Result<(), Condition> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        maybe_await!(data.put_str(&self.0.info, s))
    }

    #[maybe_async]
    pub fn flush(&self) -> Result<(), Condition> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        maybe_await!(data.flush(&self.0.info))
    }

    #[maybe_async]
    pub fn get_pos(&self) -> Result<u64, Condition> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        maybe_await!(data.get_pos(&self.0.info))
    }

    #[maybe_async]
    pub fn set_pos(&self, pos: u64) -> Result<(), Condition> {
        #[cfg(not(feature = "async"))]
        let mut data = self.0.data.lock().unwrap();

        #[cfg(feature = "async")]
        let mut data = self.0.data.lock().await;

        maybe_await!(data.set_pos(&self.0.info, pos))
    }
}

impl fmt::Debug for Port {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl PartialEq for Port {
    fn eq(&self, rhs: &Self) -> bool {
        Arc::ptr_eq(&self.0, &rhs.0)
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
            Some(Box::new(|any, buff, start, count| {
                use std::cmp::Ordering;

                let buff = &mut buff.as_mut_slice()[start..(start + count)];
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
            }))
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
            Some(Box::new(|any, buff, start, count| {
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

                    let buff = &mut buff.as_mut_slice()[start..(start + count)];
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
            }))
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

impl Default for IoError {
    fn default() -> Self {
        Self::new()
    }
}

define_condition_type!(
    rust_name: IoReadError,
    scheme_name: "&i/o-read",
    parent: IoError,
);

impl IoReadError {
    pub fn new() -> Self {
        Self {
            parent: Gc::new(IoError::new()),
        }
    }
}

impl Default for IoReadError {
    fn default() -> Self {
        Self::new()
    }
}

define_condition_type!(
    rust_name: IoWriteError,
    scheme_name: "&i/o-write",
    parent: IoError,
);

impl IoWriteError {
    pub fn new() -> Self {
        Self {
            parent: Gc::new(IoError::new()),
        }
    }
}

impl Default for IoWriteError {
    fn default() -> Self {
        Self::new()
    }
}

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
            position: position.try_into()?,
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
        this.parent.fmt(f)
    }
);

impl IoFileProtectionError {
    pub fn new(filename: impl fmt::Display) -> Self {
        Self {
            parent: Gc::new(IoFilenameError::new(filename.to_string())),
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
        this.parent.fmt(f)
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
        this.parent.fmt(f)
    }
);

impl IoFileAlreadyExistsError {
    pub fn new(filename: impl fmt::Display) -> Self {
        Self {
            parent: Gc::new(IoFilenameError::new(filename.to_string())),
        }
    }
}

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
        this.parent.fmt(f)
    }
);

impl IoFileDoesNotExistError {
    pub fn new(filename: impl fmt::Display) -> Self {
        Self {
            parent: Gc::new(IoFilenameError::new(filename.to_string())),
        }
    }
}

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

static EOF_OBJECT: LazyLock<Value> =
    LazyLock::new(|| Value::from(Record::from_rust_type(EofObject)));

static FILE_OPTIONS: LazyLock<Gc<EnumerationType>> = LazyLock::new(|| {
    Gc::new(EnumerationType::new([
        Symbol::intern("append"),
        Symbol::intern("no-create"),
        Symbol::intern("no-fail"),
        Symbol::intern("no-truncate"),
    ]))
});

fn default_file_options() -> EnumerationSet {
    EnumerationSet::new(&FILE_OPTIONS, [])
}

#[derive(Copy, Clone, Debug)]
enum PortKind {
    Read,
    Write,
    ReadWrite,
}

impl PortKind {
    fn read(self) -> bool {
        matches!(self, Self::Read | Self::ReadWrite)
    }

    fn write(self) -> bool {
        matches!(self, Self::Write | Self::ReadWrite)
    }
}

#[maybe_async]
fn open_file_port(
    filename: &Value,
    rest_args: &[Value],
    kind: PortKind,
) -> Result<Port, Condition> {
    #[cfg(not(feature = "async"))]
    use std::fs::File;

    #[cfg(feature = "tokio")]
    use tokio::fs::File;

    if rest_args.len() > 3 {
        return Err(Condition::wrong_num_of_var_args(1..4, rest_args.len() + 1));
    }

    // We don't actually use file options for anything in the input case.
    let (file_options, rest_args) = if let [file_options, rest @ ..] = rest_args {
        let file_options = file_options
            .clone()
            .try_into_rust_type::<EnumerationSet>()?;
        file_options.type_check(&FILE_OPTIONS)?;
        (file_options, rest)
    } else {
        (Gc::new(default_file_options()), &[] as &[Value])
    };

    let (buffer_mode, rest_args) = if let [buffer_mode, rest @ ..] = rest_args {
        let buffer_mode = buffer_mode.clone().try_into_rust_type::<BufferMode>()?;
        (*buffer_mode, rest)
    } else {
        (BufferMode::Block, &[] as &[Value])
    };

    let transcoder = if let [transcoder] = rest_args {
        if transcoder.is_true() {
            let transcoder = transcoder.clone().try_into_rust_type::<Transcoder>()?;
            Some(*transcoder)
        } else {
            None
        }
    } else {
        None
    };

    let filename = filename.to_string();
    let file = maybe_await!(
        File::options()
            .read(kind.read())
            .write(kind.write())
            .create(!file_options.contains("no-create"))
            .append(file_options.contains("append"))
            .truncate(!file_options.contains("no-truncate"))
            .open(&filename)
    )
    .map_err(|err| map_io_error_to_condition(&filename, err))?;

    Ok(Port::new_with_fns(
        filename,
        file.into_port(),
        None,
        File::write_fn(),
        transcoder.is_some().then(File::seek_fns).flatten(),
        File::close_fn(),
        buffer_mode,
        transcoder,
    ))
}

fn map_io_error_to_condition(filename: &str, err: std::io::Error) -> Condition {
    match err.kind() {
        ErrorKind::NotFound => {
            Condition::from((Assertion::new(), IoFileDoesNotExistError::new(filename)))
        }
        ErrorKind::AlreadyExists => {
            Condition::from((Assertion::new(), IoFileAlreadyExistsError::new(filename)))
        }
        ErrorKind::PermissionDenied => {
            Condition::from((Assertion::new(), IoFileProtectionError::new(filename)))
        }
        // TODO: All the rest
        _ => Condition::io_error(format!("{err:?}")),
    }
}

#[bridge(name = "default-file-options", lib = "(rnrs io ports (6))")]
pub fn default_file_options_scm() -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(Record::from_rust_type(
        default_file_options(),
    ))])
}

#[bridge(name = "eof-object", lib = "(rnrs io ports (6))")]
pub fn eof_object() -> Result<Vec<Value>, Condition> {
    Ok(vec![EOF_OBJECT.clone()])
}

#[bridge(name = "port?", lib = "(rnrs io ports (6))")]
pub fn port_pred(obj: &Value) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(obj.type_of() == ValueType::Port)])
}

#[bridge(name = "port-transcoder", lib = "(rnrs io ports (6))")]
pub fn port_transcoder(port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = port.clone().try_into()?;
    if let Some(transcoder) = port.transcoder() {
        let transcoder = Value::from(Record::from_rust_type(transcoder));
        Ok(vec![transcoder])
    } else {
        Ok(vec![Value::from(false)])
    }
}

#[bridge(name = "textual-port?", lib = "(rnrs io ports (6))")]
pub fn textual_port_pred(port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = port.clone().try_into()?;
    Ok(vec![Value::from(port.is_textual_port())])
}

#[bridge(name = "binary-port?", lib = "(rnrs io ports (6))")]
pub fn binary_port_pred(port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = port.clone().try_into()?;
    Ok(vec![Value::from(!port.is_textual_port())])
}

// TODO: transcoded-port

// TODO: has-port-position?
// TODO: port-position

// TODO: port-has-set-position!?
// TODO: set-port-position!

#[maybe_async]
#[bridge(name = "close-port", lib = "(rnrs io ports (6))")]
pub fn close_port(port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = port.clone().try_into()?;

    #[cfg(not(feature = "async"))]
    let mut data = port.0.data.lock().unwrap();

    #[cfg(feature = "tokio")]
    let mut data = port.0.data.lock().await;

    maybe_await!(data.close(&port.0.info))?;

    Ok(Vec::new())
}

// TODO: call-with-port

#[bridge(name = "input-port?", lib = "(rnrs io ports (6))")]
pub fn input_port_pred(obj: &Value) -> Result<Vec<Value>, Condition> {
    let Ok(port) = Port::try_from(obj.clone()) else {
        return Ok(vec![Value::from(false)]);
    };

    Ok(vec![Value::from(port.is_input_port())])
}

#[maybe_async]
#[bridge(name = "port-eof?", lib = "(rnrs io ports (6))")]
pub fn port_eof_pred(input_port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = input_port.clone().try_into()?;

    #[cfg(not(feature = "async"))]
    let mut data = port.0.data.lock().unwrap();

    #[cfg(feature = "tokio")]
    let mut data = port.0.data.lock().await;

    Ok(vec![Value::from(
        maybe_await!(data.peekn_bytes(&port.0.info, 0))?.is_none(),
    )])
}

#[maybe_async]
#[bridge(name = "open-file-input-port", lib = "(rnrs io ports (6))")]
pub fn open_file_input_port(
    filename: &Value,
    rest_args: &[Value],
) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(maybe_await!(open_file_port(
        filename,
        rest_args,
        PortKind::Read
    ))?)])
}

#[bridge(name = "make-custom-binary-input-port", lib = "(rnrs io ports (6))")]
pub fn make_custom_binary_input_port(
    id: &Value,
    read: &Value,
    get_position: &Value,
    set_position: &Value,
    close: &Value,
) -> Result<Vec<Value>, Condition> {
    let read: Procedure = read.clone().try_into()?;

    let seek = if get_position.is_true() && set_position.is_true() {
        let get_pos: Procedure = get_position.clone().try_into()?;
        let set_pos: Procedure = set_position.clone().try_into()?;
        Some((proc_to_get_pos_fn(get_pos), proc_to_set_pos_fn(set_pos)))
    } else {
        None
    };

    let close = if close.is_true() {
        let close: Procedure = close.clone().try_into()?;
        Some(proc_to_close_fn(close))
    } else {
        None
    };

    let port = Port::new_with_fns(
        id.to_string(),
        Box::new(()),
        Some(proc_to_read_fn(read)),
        None,
        seek,
        close,
        BufferMode::Block,
        None,
    );

    Ok(vec![Value::from(port)])
}

#[bridge(name = "standard-input-port", lib = "(rnrs io ports (6))")]
pub fn standard_input_port() -> Result<Vec<Value>, Condition> {
    let port = Port::new(
        "<stdin>",
        #[cfg(not(feature = "async"))]
        std::io::stdin(),
        #[cfg(feature = "tokio")]
        tokio::io::stdin(),
        BufferMode::None,
        None,
    );
    Ok(vec![Value::from(port)])
}

#[cps_bridge(def = "current-input-port", lib = "(rnrs base builtins (6))")]
pub fn current_input_port(
    _runtime: &Runtime,
    _env: &[Value],
    _args: &[Value],
    _rest_args: &[Value],
    dyn_stack: &mut DynStack,
    k: Value,
) -> Result<Application, Condition> {
    let current_input_port = dyn_stack.current_input_port().unwrap_or_else(|| {
        Port::new(
            "<stdin>",
            #[cfg(not(feature = "async"))]
            std::io::stdin(),
            #[cfg(feature = "tokio")]
            tokio::io::stdin(),
            BufferMode::None,
            Some(Transcoder::native()),
        )
    });
    Ok(Application::new(
        k.try_into().unwrap(),
        vec![Value::from(current_input_port)],
        None,
    ))
}

// 8.2.8. Binary input

#[maybe_async]
#[bridge(name = "get-u8", lib = "(rnrs io ports (6))")]
pub fn get_u8(binary_input_port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = binary_input_port.clone().try_into()?;
    if let Some(byte) = maybe_await!(port.get_u8())? {
        Ok(vec![Value::from(byte)])
    } else {
        Ok(vec![EOF_OBJECT.clone()])
    }
}

#[maybe_async]
#[bridge(name = "lookahead-u8", lib = "(rnrs io ports (6))")]
pub fn lookahead_u8(binary_input_port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = binary_input_port.clone().try_into()?;
    if let Some(byte) = maybe_await!(port.lookahead_u8())? {
        Ok(vec![Value::from(byte)])
    } else {
        Ok(vec![EOF_OBJECT.clone()])
    }
}

// 8.2.9. Textual input

#[maybe_async]
#[bridge(name = "get-char", lib = "(rnrs io ports (6))")]
pub fn get_char(textual_input_port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = textual_input_port.clone().try_into()?;
    if let Some(chr) = maybe_await!(port.get_char())? {
        Ok(vec![Value::from(chr)])
    } else {
        Ok(vec![EOF_OBJECT.clone()])
    }
}

#[maybe_async]
#[bridge(name = "lookahead-char", lib = "(rnrs io ports (6))")]
pub fn lookahead_char(textual_input_port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = textual_input_port.clone().try_into()?;
    if let Some(chr) = maybe_await!(port.lookahead_char())? {
        Ok(vec![Value::from(chr)])
    } else {
        Ok(vec![EOF_OBJECT.clone()])
    }
}

#[maybe_async]
#[bridge(name = "get-line", lib = "(rnrs io ports (6))")]
pub fn get_line(textual_input_port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = textual_input_port.clone().try_into()?;
    if let Some(line) = maybe_await!(port.get_line())? {
        Ok(vec![Value::from(line)])
    } else {
        Ok(vec![EOF_OBJECT.clone()])
    }
}

#[maybe_async]
#[bridge(name = "get-datum", lib = "(rnrs io ports (6))")]
pub fn get_datum(textual_input_port: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = textual_input_port.clone().try_into()?;
    if let Some((syntax, _)) = maybe_await!(port.get_sexpr(Span::default()))? {
        Ok(vec![Value::datum_from_syntax(&syntax)])
    } else {
        Ok(vec![EOF_OBJECT.clone()])
    }
}

// 8.2.10. Output ports

#[bridge(name = "standard-output-port", lib = "(rnrs io ports (6))")]
pub fn standard_output_port() -> Result<Vec<Value>, Condition> {
    let port = Port::new(
        "<stdout>",
        #[cfg(not(feature = "async"))]
        std::io::stdout(),
        #[cfg(feature = "tokio")]
        tokio::io::stdout(),
        BufferMode::None,
        None,
    );
    Ok(vec![Value::from(port)])
}

// 8.2.10. Output ports

#[bridge(name = "output-port?", lib = "(rnrs io ports (6))")]
pub fn output_port_pred(obj: &Value) -> Result<Vec<Value>, Condition> {
    let Ok(port) = Port::try_from(obj.clone()) else {
        return Ok(vec![Value::from(false)]);
    };

    Ok(vec![Value::from(port.is_output_port())])
}

#[maybe_async]
#[bridge(name = "flush-output-port", lib = "(rnrs io ports (6))")]
pub fn flush_output_port(obj: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = obj.clone().try_into()?;
    maybe_await!(port.flush())?;
    Ok(Vec::new())
}

#[bridge(name = "output-port-buffer-mode", lib = "(rnrs io ports (6))")]
pub fn output_port_buffer_mode(output_port: &Value) -> Result<Vec<Value>, Condition> {
    let output_port: Port = output_port.clone().try_into()?;
    Ok(vec![Value::from(output_port.buffer_mode().to_sym())])
}

#[maybe_async]
#[bridge(name = "open-file-output-port", lib = "(rnrs io ports (6))")]
pub fn open_file_output_port(
    filename: &Value,
    rest_args: &[Value],
) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(maybe_await!(open_file_port(
        filename,
        rest_args,
        PortKind::Write
    ))?)])
}

// 8.2.11. Binary output

#[maybe_async]
#[bridge(name = "put-u8", lib = "(rnrs io ports (6))")]
pub fn put_u8(binary_output_port: &Value, octet: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = binary_output_port.clone().try_into()?;
    let octet: u8 = octet.clone().try_into()?;
    maybe_await!(port.put_u8(octet))?;
    Ok(Vec::new())
}

// 8.2.12. Textual output

#[maybe_async]
#[bridge(name = "put-char", lib = "(rnrs io ports (6))")]
pub fn put_char(textual_output_port: &Value, chr: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = textual_output_port.clone().try_into()?;
    let chr: char = chr.clone().try_into()?;
    maybe_await!(port.put_char(chr))?;
    Ok(Vec::new())
}

#[maybe_async]
#[bridge(name = "put-datum", lib = "(rnrs io ports (6))")]
pub fn put_datum(textual_output_port: &Value, datum: &Value) -> Result<Vec<Value>, Condition> {
    let port: Port = textual_output_port.clone().try_into()?;
    let str_rep = format!("{datum:?}");
    maybe_await!(port.put_str(&str_rep))?;
    Ok(Vec::new())
}

// 8.2.13. Input/output ports

#[maybe_async]
#[bridge(name = "open-file-input/output-port", lib = "(rnrs io ports (6))")]
pub fn open_file_input_output_port(
    filename: &Value,
    rest_args: &[Value],
) -> Result<Vec<Value>, Condition> {
    Ok(vec![Value::from(maybe_await!(open_file_port(
        filename,
        rest_args,
        PortKind::ReadWrite
    ))?)])
}
