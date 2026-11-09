use crate::{Collector, Heap, ObjectHeader, for_each_threads_words, stop_the_world};

pub(crate) struct LxrCollector {}

#[allow(dead_code)]
pub(crate) struct LxrHeader {}

unsafe impl ObjectHeader for LxrHeader {
    type HeapState = ();
    fn layout(&self) -> std::alloc::Layout {
        todo!()
    }
}

impl Collector for LxrCollector {
    type Header = LxrHeader;

    fn new(_heap: &'static Heap<LxrHeader>) -> Self {
        LxrCollector {}
    }

    fn collect(&mut self) {
        let stopped = stop_the_world();
        for_each_threads_words(&stopped, LxrCollector::check_word);
    }

    fn shutdown(&mut self) {
        todo!()
    }
}

impl LxrCollector {
    #[allow(unused_variables)]
    fn check_word(word: usize) {
        todo!()
    }
}
