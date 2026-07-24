use std::time::{SystemTime, UNIX_EPOCH};

use scheme_rs_macros::bridge;


#[bridge(name = "current-second", lib = "(scheme time (1))")]
pub fn current_second() -> f64 {
    SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64()
}

#[bridge(name = "current-jiffy", lib = "(scheme time (1))")]
pub fn current_jiffy() -> u128 {
    SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos()
}

#[bridge(name = "jiffies-per-second", lib = "(scheme time (1))")]
pub fn jiffies_per_second() -> u64 {
    1_000_000_000
}
