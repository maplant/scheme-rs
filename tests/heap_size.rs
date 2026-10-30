use scheme_rs::gc::{Gc, budget, set_heap_size};

const BLOCK_SIZE: usize = 32 * 1024;

#[test]
fn set_heap_size_applies_before_first_use_and_fails_after() {
    set_heap_size(4 * BLOCK_SIZE).unwrap();
    let _ = Gc::new(0u64);
    assert_eq!(budget(), 4 * BLOCK_SIZE);
    assert!(set_heap_size(8 * BLOCK_SIZE).is_err());
}
