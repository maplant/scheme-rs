//! τJIT: a typed JIT tier on top of the baseline JIT.
//!
//! Every user lambda gets a [`LambdaCode`], shared by all closures allocated
//! from it. It counts calls; once a lambda is hot, the types of its arguments
//! and returns are sampled for a few calls.
//!
//! Environment variables:
//! - `SCHEME_RS_TEOJIT_TRACE`: print tier transitions.

use crate::{
    proc::{Args, MAX_DIRECT_ARGS, UserPtr},
    symbols::Symbol,
    syntax::Span,
    value::{Tag, Value},
};
use std::{
    fmt,
    ptr::NonNull,
    sync::{
        LazyLock,
        atomic::{AtomicPtr, AtomicU16, AtomicU64, AtomicUsize, Ordering},
    },
};

/// Number of calls after which a lambda is considered hot.
///
/// TODO: this value is a placeholder and must be tuned with benchmarks once
/// the specialized tier exists: too low and we spend compile time on code that
/// barely runs, too high and hot code stays on the baseline for longer.
const HOT_THRESHOLD: u64 = 1000;

/// Number of calls after becoming hot whose argument types are sampled.
///
/// TODO: placeholder, tune with benchmarks. Sampled calls take the slow path
/// through [`Procedure::apply`], so this should stay small. A type that is
/// missed here only costs a deopt later, it never affects correctness.
const SAMPLE_COUNT: u64 = 64;

/// Number of calls after which a lambda is done being profiled.
const PROFILED: u64 = HOT_THRESHOLD + SAMPLE_COUNT;

/// Bitset of the pointer tags observed for an argument, one bit per [`Tag`].
#[derive(Copy, Clone, Default, PartialEq, Eq)]
pub(crate) struct TypeSet(u16);

impl TypeSet {
    const TAGS: [Tag; 9] = [
        Tag::FixNum,
        Tag::Pair,
        Tag::SmallValue,
        Tag::Number,
        Tag::Procedure,
        Tag::Record,
        Tag::RecordTypeDescriptor,
        Tag::Cell,
        Tag::ReturnAddress,
    ];

    fn bit(tag: Tag) -> u16 {
        1 << tag as u64
    }

    /// Set for returns of zero or more than one value.
    const MULTIPLE_VALUES: Self = Self(1 << 15);

    pub(crate) fn of(value: &Value) -> Self {
        Self(Self::bit(value.tag()))
    }

    /// The type of a return, given its first two values.
    fn of_return(ret1: &Value, ret2: &Value) -> Self {
        if ret1.is_undefined() || !ret2.is_undefined() {
            Self::MULTIPLE_VALUES
        } else {
            Self::of(ret1)
        }
    }
}

impl fmt::Debug for TypeSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut names = Self::TAGS
            .iter()
            .filter(|tag| self.0 & Self::bit(**tag) != 0)
            .map(|tag| format!("{tag:?}"))
            .collect::<Vec<_>>();
        if self.0 & Self::MULTIPLE_VALUES.0 != 0 {
            names.push("Values".to_string());
        }
        match names.is_empty() {
            true => write!(f, "none"),
            false => write!(f, "{}", names.join("|")),
        }
    }
}

/// Whether the typed JIT prints what it is doing. All of its output goes
/// through [`teojit_trace!`].
pub(crate) static TEOJIT_TRACE: LazyLock<bool> =
    LazyLock::new(|| std::env::var("SCHEME_RS_TEOJIT_TRACE").is_ok());

macro_rules! teojit_trace {
    ($($arg:tt)*) => {
        if *$crate::teojit::TEOJIT_TRACE {
            eprintln!("[τJIT] {}", format_args!($($arg)*));
        }
    };
}
#[allow(unused_imports)]
pub(crate) use teojit_trace;

static NEXT_LAMBDA_ID: AtomicUsize = AtomicUsize::new(0);

/// JIT state shared by every closure allocated from the same lambda.
pub(crate) struct LambdaCode {
    /// Unique id of the lambda, to tell apart lambdas in trace output.
    id: usize,
    name: Option<Symbol>,
    loc: Option<Span>,
    /// Number of times a closure of this lambda has been entered. Updated
    /// without synchronization, so concurrent calls may lose counts.
    calls: AtomicU64,
    /// Types observed for each direct argument while sampling.
    ///
    /// TODO: could be packed into a single AtomicU64 so a sample is one
    /// load-or-store and the whole tuple can be read at once.
    arg_types: [AtomicU16; MAX_DIRECT_ARGS],
    /// Types returned while sampling.
    ret_types: AtomicU16,
    /// Entry every call should go to, or null while the lambda is still being
    /// profiled.
    entry: AtomicPtr<u8>,
}

impl LambdaCode {
    /// Allocate the state for a lambda. It lives as long as the JIT compiled
    /// code referencing it, which is never freed.
    pub(crate) fn alloc(name: Option<Symbol>, loc: Option<Span>) -> &'static Self {
        let code = Box::leak(Box::new(Self {
            id: NEXT_LAMBDA_ID.fetch_add(1, Ordering::Relaxed),
            name,
            loc,
            calls: AtomicU64::new(0),
            arg_types: Default::default(),
            ret_types: AtomicU16::new(0),
            entry: AtomicPtr::new(std::ptr::null_mut()),
        }));
        teojit_trace!("{code}: -> baseline");
        code
    }

    /// Whether calls are currently routed through [`Procedure::apply`] so
    /// their argument types can be sampled.
    pub(crate) fn is_sampling(&self) -> bool {
        self.entry().is_none() && self.calls.load(Ordering::Relaxed) >= HOT_THRESHOLD
    }

    /// The entry for a direct tail call to `baseline` from JIT code, or None
    /// while sampling, so the call goes through [`Procedure::apply`] instead.
    #[inline]
    pub(crate) fn tail_call_entry(&'static self, baseline: UserPtr) -> Option<UserPtr> {
        if let Some(entry) = self.entry() {
            return Some(entry);
        }
        let calls = self.calls.load(Ordering::Relaxed);
        if calls >= HOT_THRESHOLD {
            None
        } else {
            self.count_call(calls, baseline);
            Some(baseline)
        }
    }

    /// The entry for a call to `baseline` through [`Procedure::apply`].
    #[inline]
    pub(crate) fn apply_entry(
        &'static self,
        baseline: UserPtr,
        args: &Args,
        num_args: usize,
    ) -> UserPtr {
        if let Some(entry) = self.entry() {
            return entry;
        }
        let calls = self.calls.load(Ordering::Relaxed);
        if calls >= HOT_THRESHOLD {
            self.record_args(args, num_args);
        }
        self.count_call(calls, baseline);
        baseline
    }

    /// The entry every call should go to, once profiling is done.
    #[inline]
    fn entry(&self) -> Option<UserPtr> {
        NonNull::new(self.entry.load(Ordering::Acquire)).map(UserPtr)
    }

    /// Count a call, given the number of calls before it.
    #[inline]
    fn count_call(&'static self, calls: u64, baseline: UserPtr) {
        let calls = calls + 1;
        self.calls.store(calls, Ordering::Relaxed);
        if calls == HOT_THRESHOLD {
            teojit_trace!("{self}: baseline -> sampling (hot after {calls} calls)");
        } else if calls == PROFILED {
            self.finish_sampling(baseline);
        }
    }

    #[cold]
    fn finish_sampling(&'static self, baseline: UserPtr) {
        self.entry.store(baseline.0.as_ptr(), Ordering::Release);
        teojit_trace!(
            "{self}: sampling -> baseline (sampled {:?} -> {:?} over {SAMPLE_COUNT} calls)",
            self.arg_types(),
            self.ret_types()
        );
    }

    fn record_args(&self, args: &Args, num_args: usize) {
        for (arg, types) in args.0.iter().zip(&self.arg_types).take(num_args) {
            let observed = types.load(Ordering::Relaxed) | TypeSet::of(arg).0;
            types.store(observed, Ordering::Relaxed);
        }
    }

    pub(crate) fn record_return(&self, ret1: &Value, ret2: &Value) {
        let observed = self.ret_types.load(Ordering::Relaxed) | TypeSet::of_return(ret1, ret2).0;
        self.ret_types.store(observed, Ordering::Relaxed);
    }

    pub(crate) fn ret_types(&self) -> TypeSet {
        TypeSet(self.ret_types.load(Ordering::Relaxed))
    }

    pub(crate) fn arg_types(&self) -> [TypeSet; MAX_DIRECT_ARGS] {
        std::array::from_fn(|i| TypeSet(self.arg_types[i].load(Ordering::Relaxed)))
    }
}

impl fmt::Debug for LambdaCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for LambdaCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.name {
            Some(name) => write!(f, "#{} {name}", self.id)?,
            None => write!(f, "#{} <lambda>", self.id)?,
        }
        if let Some(ref loc) = self.loc {
            write!(f, " at {loc}")?;
        }
        Ok(())
    }
}
