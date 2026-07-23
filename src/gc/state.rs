//! Packed GC header state word: reference count, color, and flags in a
//! single atomic word. Single-word RMWs give mutator/collector visibility
//! via per-location modification order (design doc §1, "Scenario 2").

pub(crate) const RC_BITS: u32 = 48;
pub(crate) const RC_MASK: usize = (1 << RC_BITS) - 1;
pub(crate) const COLOR_SHIFT: u32 = RC_BITS;
pub(crate) const COLOR_MASK: usize = 0b111 << COLOR_SHIFT;
pub(crate) const BUFFERED: usize = 1 << 51;
pub(crate) const INC_EVENT: usize = 1 << 52;
/// Attention-list membership claim (Phase 1b+). Distinct from BUFFERED,
/// which the epoch scan owns until Phase 2 unifies them.
pub(crate) const ATTN_CLAIM: usize = 1 << 53;
/// Finalized by the scan while claimed; header memory awaits the drain
/// that removes its attention-list entry (dealloc deferral).
pub(crate) const ATTN_DEAD: usize = 1 << 54;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum Color {
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
pub(crate) struct GcState(pub(crate) usize);

impl GcState {
    /// rc = 1, Black, buffered (matches the current header's birth state:
    /// `prev` carried the buffered bit set at construction).
    pub(crate) fn new_initial() -> Self {
        GcState(1 | BUFFERED)
    }

    pub(crate) fn rc(self) -> usize {
        self.0 & RC_MASK
    }

    pub(crate) fn color(self) -> Color {
        Color::from(((self.0 & COLOR_MASK) >> COLOR_SHIFT) as u8)
    }

    pub(crate) fn with_color(self, color: Color) -> Self {
        GcState((self.0 & !COLOR_MASK) | ((color as usize) << COLOR_SHIFT))
    }

    pub(crate) fn buffered(self) -> bool {
        self.0 & BUFFERED != 0
    }

    pub(crate) fn inc_event(self) -> bool {
        self.0 & INC_EVENT != 0
    }

    pub(crate) fn attn_claimed(self) -> bool {
        self.0 & ATTN_CLAIM != 0
    }

    pub(crate) fn attn_dead(self) -> bool {
        self.0 & ATTN_DEAD != 0
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
        assert!(s.buffered(), "objects are born buffered (pre-first-scan)");
        assert!(!s.inc_event());
    }

    #[test]
    fn rc_arithmetic_preserves_color_and_flags() {
        let s = GcState::new_initial().with_color(Color::Orange);
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

        let s = GcState::new_initial();
        assert!(!s.attn_claimed(), "newborns have no pending attention event");
        assert!(!s.attn_dead());

        let claimed = GcState(s.0 | ATTN_CLAIM);
        assert!(claimed.attn_claimed());
        assert_eq!(claimed.rc(), 1);
        assert_eq!(claimed.color(), Color::Black);
        assert!(claimed.buffered(), "claim must not disturb the scan's bit");

        let dead = GcState(claimed.0 | ATTN_DEAD);
        assert!(dead.attn_dead());
        assert!(dead.attn_claimed());
    }
}
