use crate::sync::{AtomicU64, Ordering};

#[inline]
pub(crate) const fn bits_through(bit: usize) -> u64 {
    u64::MAX >> (63 - bit)
}

/// The index of the highest set bit at or before `bit`.
pub(crate) fn last_set_at_or_before(words: &[AtomicU64], bit: usize) -> Option<usize> {
    let mut mask = bits_through(bit % 64);
    for w in (0..=bit / 64).rev() {
        let bits = words[w].load(Ordering::Relaxed) & mask;
        if bits != 0 {
            return Some(w * 64 + bits.ilog2() as usize);
        }
        mask = u64::MAX;
    }
    None
}
