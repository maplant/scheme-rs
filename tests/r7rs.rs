mod common;

common::run_test!(r7rs);

// TODO: Move this into r7rs, or somewhere else
// assert_failure!(byte_overflow, "#u8(9001)");
