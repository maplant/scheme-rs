use crate::{assert_file, assert_failure};

assert_file!(r7rs);

assert_failure!(byte_overflow, "#u8(9001)");
