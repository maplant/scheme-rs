use std::sync::OnceLock;

// ── Tag encoding (matches host src/value.rs exactly) ───────────────────────
//
// 4 low bits encode the type tag. Even tags identify heap-allocated types
// (except Pair/Record with null pointer, which are null/undefined).
// Odd values (bit 0 = 1) are inline fixnums — the upper 63 bits hold the
// integer value directly.

const ALIGNMENT: usize = 16;
const TAG_BITS: usize = ALIGNMENT.ilog2() as usize; // 4
const TAG_MASK: usize = 0b1111;

const TAG_PAIR: usize = 0;
const TAG_BOOLEAN: usize = 1 << 1;            // 2
const TAG_CHAR_OR_SYMBOL: usize = 2 << 1;     // 4
const TAG_NUMBER: usize = 3 << 1;             // 6
const TAG_PROCEDURE: usize = 4 << 1;          // 8
const TAG_RECORD: usize = 5 << 1;             // 10
const TAG_RTD: usize = 6 << 1;                // 12
const TAG_CELL: usize = 7 << 1;               // 14

const SYMBOL_CHAR: u32 = 0x110000;

// ── Well-known constants ────────────────────────────────────────────────────

const NULL_VALUE: usize = TAG_PAIR;            // 0 — Pair tag with null pointer
const TRUE_VALUE: usize = TAG_BOOLEAN | (1 << TAG_BITS); // 18
const FALSE_VALUE: usize = TAG_BOOLEAN;        // 2
const UNDEFINED_VALUE: usize = TAG_RECORD;     // 10 — Record tag with null pointer

// ── Host function table (populated by the host at load time) ────────────────

type RetainFn = unsafe extern "C" fn(usize);
type ReleaseFn = unsafe extern "C" fn(usize);

pub(crate) struct HostFns {
    pub retain: RetainFn,
    pub release: ReleaseFn,
}

pub(crate) static HOST: OnceLock<HostFns> = OnceLock::new();

// ── Value ───────────────────────────────────────────────────────────────────

/// A Scheme value with identical layout to the host's `Value(*const ())`.
///
/// Uses tagged pointers: 4 low bits encode the type tag, upper bits hold either
/// an aligned heap pointer or an inline immediate (booleans, characters, symbols,
/// null, undefined, fixnums).
#[repr(transparent)]
pub struct Value(usize);

unsafe impl Send for Value {}
unsafe impl Sync for Value {}

impl Value {
    // ── Tag introspection ───────────────────────────────────────────────

    #[inline]
    fn tag(&self) -> usize {
        self.0 & TAG_MASK
    }

    /// True when no reference-count management is needed.
    ///
    /// Immediate values are: fixnums, booleans, characters, symbols,
    /// null, and undefined.
    #[inline]
    pub fn is_immediate(&self) -> bool {
        if self.0 & 1 == 1 {
            return true; // FixNum
        }
        match self.tag() {
            TAG_PAIR => self.is_null(),
            TAG_BOOLEAN | TAG_CHAR_OR_SYMBOL => true,
            TAG_RECORD => self.is_undefined(),
            _ => false,
        }
    }

    #[inline]
    pub fn is_null(&self) -> bool {
        self.0 == NULL_VALUE
    }

    #[inline]
    pub fn is_undefined(&self) -> bool {
        self.0 == UNDEFINED_VALUE
    }

    #[inline]
    pub fn is_boolean(&self) -> bool {
        self.tag() == TAG_BOOLEAN
    }

    #[inline]
    pub fn is_pair(&self) -> bool {
        self.tag() == TAG_PAIR && !self.is_null()
    }

    #[inline]
    pub fn is_procedure(&self) -> bool {
        self.tag() == TAG_PROCEDURE
    }

    #[inline]
    pub fn is_record(&self) -> bool {
        self.tag() == TAG_RECORD && !self.is_undefined()
    }

    #[inline]
    pub fn is_symbol(&self) -> bool {
        if self.tag() != TAG_CHAR_OR_SYMBOL {
            return false;
        }
        let untagged = self.0 & !TAG_MASK;
        ((untagged as u32) >> TAG_BITS) == SYMBOL_CHAR
    }

    #[inline]
    pub fn is_number(&self) -> bool {
        (self.0 & 1 == 1) || self.tag() == TAG_NUMBER
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        // In the current host encoding, strings are records.
        // The host's extraction function handles the actual type check.
        self.is_record()
    }

    #[inline]
    pub fn is_character(&self) -> bool {
        if self.tag() != TAG_CHAR_OR_SYMBOL {
            return false;
        }
        let untagged = self.0 & !TAG_MASK;
        ((untagged as u32) >> TAG_BITS) != SYMBOL_CHAR
    }

    // ── Constructors for immediates ─────────────────────────────────────

    #[inline]
    pub fn null() -> Self {
        Self(NULL_VALUE)
    }

    #[inline]
    pub fn undefined() -> Self {
        Self(UNDEFINED_VALUE)
    }

    #[inline]
    pub fn from_bool(b: bool) -> Self {
        Self(TAG_BOOLEAN | ((b as usize) << TAG_BITS))
    }

    // ── Extractors ──────────────────────────────────────────────────────

    #[inline]
    pub fn to_bool(&self) -> Option<bool> {
        if self.is_boolean() {
            Some(self.0 != FALSE_VALUE)
        } else {
            None
        }
    }

    // ── Raw access ──────────────────────────────────────────────────────

    #[inline]
    pub fn as_raw(&self) -> usize {
        self.0
    }

    /// Reconstruct a `Value` from a raw `usize` previously obtained via
    /// [`as_raw`](Value::as_raw).
    ///
    /// # Safety
    /// The caller must ensure `raw` was produced by `as_raw` (or the host
    /// equivalent `Value::into_raw`) and that ownership semantics are
    /// respected (i.e. the refcount has been incremented if a second owner
    /// is being created).
    #[inline]
    pub unsafe fn from_raw(raw: usize) -> Self {
        Self(raw)
    }
}

// ── Clone / Drop ────────────────────────────────────────────────────────────

impl Clone for Value {
    fn clone(&self) -> Self {
        if self.is_immediate() {
            return Self(self.0);
        }
        let host = HOST
            .get()
            .expect("cannot clone heap Value: host not initialized");
        unsafe { (host.retain)(self.0) };
        Self(self.0)
    }
}

impl Drop for Value {
    fn drop(&mut self) {
        if self.is_immediate() {
            return;
        }
        let host = HOST
            .get()
            .expect("cannot drop heap Value: host not initialized");
        unsafe { (host.release)(self.0) };
    }
}

// ── Debug ───────────────────────────────────────────────────────────────────

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 & 1 == 1 {
            let n = (self.0 as i64) >> 1;
            return write!(f, "Value::fixnum({n})");
        }
        match self.tag() {
            TAG_PAIR if self.is_null() => write!(f, "Value::null"),
            TAG_PAIR => write!(f, "Value::pair({:#x})", self.0),
            TAG_BOOLEAN => write!(f, "Value::bool({})", self.0 == TRUE_VALUE),
            TAG_CHAR_OR_SYMBOL if self.is_symbol() => {
                write!(f, "Value::symbol({:#x})", self.0 >> 32)
            }
            TAG_CHAR_OR_SYMBOL => {
                let cp = ((self.0 & !TAG_MASK) as u32) >> TAG_BITS;
                write!(f, "Value::char({cp:#x})")
            }
            TAG_NUMBER => write!(f, "Value::number({:#x})", self.0),
            TAG_PROCEDURE => write!(f, "Value::procedure({:#x})", self.0),
            TAG_RECORD if self.is_undefined() => write!(f, "Value::undefined"),
            TAG_RECORD => write!(f, "Value::record({:#x})", self.0),
            TAG_RTD => write!(f, "Value::rtd({:#x})", self.0),
            TAG_CELL => write!(f, "Value::cell({:#x})", self.0),
            tag => write!(f, "Value({:#x}, tag={tag})", self.0),
        }
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn null_value() {
        let v = Value::null();
        assert!(v.is_null());
        assert!(v.is_immediate());
        assert!(!v.is_undefined());
        assert!(!v.is_boolean());
        assert!(!v.is_pair());
    }

    #[test]
    fn undefined_value() {
        let v = Value::undefined();
        assert!(v.is_undefined());
        assert!(v.is_immediate());
        assert!(!v.is_null());
        assert!(!v.is_boolean());
    }

    #[test]
    fn boolean_true() {
        let v = Value::from_bool(true);
        assert!(v.is_boolean());
        assert!(v.is_immediate());
        assert_eq!(v.to_bool(), Some(true));
        assert_eq!(v.as_raw(), TRUE_VALUE);
    }

    #[test]
    fn boolean_false() {
        let v = Value::from_bool(false);
        assert!(v.is_boolean());
        assert!(v.is_immediate());
        assert_eq!(v.to_bool(), Some(false));
        assert_eq!(v.as_raw(), FALSE_VALUE);
    }

    #[test]
    fn non_boolean_to_bool_is_none() {
        assert_eq!(Value::null().to_bool(), None);
        assert_eq!(Value::undefined().to_bool(), None);
    }

    #[test]
    fn raw_roundtrip() {
        let v = Value::from_bool(true);
        let raw = v.as_raw();
        let v2 = unsafe { Value::from_raw(raw) };
        assert_eq!(v2.as_raw(), raw);
        assert!(v2.is_boolean());
        assert_eq!(v2.to_bool(), Some(true));
    }

    #[test]
    fn constants_match_host() {
        assert_eq!(NULL_VALUE, 0);  // Pair tag = 0
        assert_eq!(FALSE_VALUE, 2); // Boolean tag
        assert_eq!(TRUE_VALUE, 18); // Boolean tag | (1 << 4)
        assert_eq!(UNDEFINED_VALUE, 10); // Record tag = 10
    }

    #[test]
    fn tag_bits_layout() {
        assert_eq!(TAG_BITS, 4);
        assert_eq!(TAG_MASK, 0b1111);
    }

    #[test]
    fn immediate_clone_no_panic() {
        let vals = [
            Value::null(),
            Value::undefined(),
            Value::from_bool(true),
            Value::from_bool(false),
        ];
        for v in &vals {
            let _cloned = v.clone();
        }
    }

    #[test]
    fn immediate_drop_no_panic() {
        let _ = Value::null();
        let _ = Value::undefined();
        let _ = Value::from_bool(true);
        let _ = Value::from_bool(false);
    }

    #[test]
    fn size_and_alignment() {
        assert_eq!(size_of::<Value>(), size_of::<usize>());
        assert_eq!(size_of::<Value>(), size_of::<*const ()>());
    }

    #[test]
    fn tag_discrimination() {
        let null = Value::null();
        let undef = Value::undefined();
        let t = Value::from_bool(true);

        assert!(!null.is_procedure());
        assert!(!null.is_record());
        assert!(!undef.is_pair());
        assert!(!t.is_symbol());
        assert!(!t.is_character());
    }

    #[test]
    fn fixnum_is_number() {
        // A fixnum has bit 0 = 1, upper bits hold the value
        let fixnum = Value((42 << 1) | 1);
        assert!(fixnum.is_number());
        assert!(fixnum.is_immediate());
        assert!(!fixnum.is_boolean());
        assert!(!fixnum.is_pair());
    }
}
