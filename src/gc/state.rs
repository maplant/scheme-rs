//! Packed GC header state word: reference count, color, and flags in a
//! single atomic word. Single-word RMWs give mutator/collector visibility
//! via per-location modification order (design doc §1, "Scenario 2").

pub(crate) const RC_BITS: u32 = 48;
pub const RC_MASK: usize = (1 << RC_BITS) - 1;
pub(crate) const COLOR_SHIFT: u32 = RC_BITS;
pub(crate) const COLOR_MASK: usize = 0b111 << COLOR_SHIFT;
pub const BUFFERED: usize = 1 << 51;
pub const INC_EVENT: usize = 1 << 52;
/// Attention-list membership claim (Phase 1b+). Distinct from BUFFERED,
/// which the epoch scan owns until Phase 2 unifies them.
pub const ATTN_CLAIM: usize = 1 << 53;
/// Finalized by the scan while claimed; header memory awaits the drain
/// that removes its attention-list entry (dealloc deferral).
pub const ATTN_DEAD: usize = 1 << 54;
/// A drain observed rc==0 once (phase 2 zero-aging). JIT-compiled frames
/// hold raw pointers and re-materialize counts via `from_raw_inc_rc`, so a
/// single zero sighting can be a transient resurrection window, not death;
/// only a *second consecutive* sighting with rc still 0 is safe to free.
pub const ZERO_PENDING: usize = 1 << 55;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Color {
    /// In use or free
    Black = 0,
    /// Possible member of a cycle
    Gray = 1,
    /// Member of a garbage cycle
    White = 2,
    /// Possible root of cycle
    Purple = 3,
    /// Candidate cycle undergoing Σ-computation
    Red = 4,
    /// Candidate cycle awaiting epoch boundary
    Orange = 5,
}

impl From<u8> for Color {
    fn from(value: u8) -> Self {
        match value {
            0 => Self::Black,
            1 => Self::Gray,
            2 => Self::White,
            3 => Self::Purple,
            4 => Self::Red,
            5 => Self::Orange,
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct GcState(pub usize);

impl GcState {
    /// rc = 1, Black, unclaimed (phase 2: newborns touch no global structure
    /// until their first event).
    pub(crate) fn new_initial() -> Self {
        GcState(1)
    }

    pub fn rc(self) -> usize {
        self.0 & RC_MASK
    }

    pub fn color(self) -> Color {
        Color::from(((self.0 & COLOR_MASK) >> COLOR_SHIFT) as u8)
    }

    pub fn with_color(self, color: Color) -> Self {
        GcState((self.0 & !COLOR_MASK) | ((color as usize) << COLOR_SHIFT))
    }

    pub fn buffered(self) -> bool {
        self.0 & BUFFERED != 0
    }

    pub fn inc_event(self) -> bool {
        self.0 & INC_EVENT != 0
    }

    pub fn attn_claimed(self) -> bool {
        self.0 & ATTN_CLAIM != 0
    }

    pub fn attn_dead(self) -> bool {
        self.0 & ATTN_DEAD != 0
    }

    pub fn zero_pending(self) -> bool {
        self.0 & ZERO_PENDING != 0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn initial_state() {
        let s = GcState::new_initial();
        assert_eq!(s.rc(), 1);
        assert_eq!(s.color(), Color::Black);
        assert!(
            !s.buffered(),
            "phase 2: newborns touch no global structure until their first event"
        );
        assert!(!s.inc_event());
    }

    #[test]
    fn rc_arithmetic_preserves_color_and_flags() {
        let s = GcState(GcState::new_initial().0 | BUFFERED).with_color(Color::Orange);
        let bumped = GcState(s.0 + 1);
        assert_eq!(bumped.rc(), 2);
        assert_eq!(bumped.color(), Color::Orange);
        assert!(bumped.buffered());
        let dropped = GcState(bumped.0 - 2);
        assert_eq!(dropped.rc(), 0);
        assert_eq!(dropped.color(), Color::Orange);
    }

    #[test]
    fn color_roundtrip_preserves_rc_and_flags() {
        let mut s = GcState(RC_MASK & 0xdead_beef);
        for c in [
            Color::Black,
            Color::Gray,
            Color::White,
            Color::Purple,
            Color::Red,
            Color::Orange,
        ] {
            s = s.with_color(c);
            assert_eq!(s.color(), c);
            assert_eq!(s.rc(), RC_MASK & 0xdead_beef);
            assert!(!s.buffered());
        }
    }

    #[test]
    fn flag_masks_are_disjoint_from_rc_and_color() {
        assert_eq!(RC_MASK & COLOR_MASK, 0);
        assert_eq!((RC_MASK | COLOR_MASK) & BUFFERED, 0);
        assert_eq!((RC_MASK | COLOR_MASK | BUFFERED) & INC_EVENT, 0);
    }

    #[test]
    fn attn_bits_disjoint_and_roundtrip() {
        assert_eq!((RC_MASK | COLOR_MASK | BUFFERED | INC_EVENT) & ATTN_CLAIM, 0);
        assert_eq!(
            (RC_MASK | COLOR_MASK | BUFFERED | INC_EVENT | ATTN_CLAIM) & ATTN_DEAD,
            0
        );
        assert_eq!(
            (RC_MASK | COLOR_MASK | BUFFERED | INC_EVENT | ATTN_CLAIM | ATTN_DEAD)
                & ZERO_PENDING,
            0
        );

        let s = GcState::new_initial();
        assert!(!s.attn_claimed(), "newborns have no pending attention event");
        assert!(!s.attn_dead());
        assert!(!s.buffered(), "phase 2: newborns are unbuffered");
        assert!(!s.zero_pending());

        let claimed = GcState(s.0 | ATTN_CLAIM);
        assert!(claimed.attn_claimed());
        assert_eq!(claimed.rc(), 1);
        assert_eq!(claimed.color(), Color::Black);

        let buffered_and_claimed = GcState(s.0 | BUFFERED | ATTN_CLAIM);
        assert!(
            buffered_and_claimed.buffered(),
            "claim must not disturb the buffered bit"
        );
        assert!(buffered_and_claimed.attn_claimed());

        let dead = GcState(claimed.0 | ATTN_DEAD);
        assert!(dead.attn_dead());
        assert!(dead.attn_claimed());

        let zero_pending_and_claimed = GcState(s.0 | ZERO_PENDING | ATTN_CLAIM);
        assert!(zero_pending_and_claimed.zero_pending());
        assert!(zero_pending_and_claimed.attn_claimed());
        assert_eq!(zero_pending_and_claimed.rc(), 1);
        assert_eq!(zero_pending_and_claimed.color(), Color::Black);
    }
}
