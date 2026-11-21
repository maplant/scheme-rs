use std::sync::LazyLock;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

use crate::registry::define;

#[define(lib = "(scheme time (1))")]
pub fn current_second() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs_f64()
}

#[define(lib = "(scheme time (1))")]
pub fn current_jiffy() -> u128 {
    static JIFFY_EPOCH: LazyLock<Instant> = LazyLock::new(Instant::now);
    JIFFY_EPOCH.elapsed().as_nanos()
}

#[define(lib = "(scheme time (1))")]
pub fn jiffies_per_second() -> u64 {
    1_000_000_000
}
