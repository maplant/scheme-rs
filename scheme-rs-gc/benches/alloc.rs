use core::alloc::Layout;

use criterion::{Criterion, criterion_group, criterion_main};
use scheme_rs_gc::{Heap, ObjectModel};

struct Model;

unsafe impl ObjectModel for Model {
    type Header = Layout;

    fn layout(header: &Layout) -> Layout {
        *header
    }
}

const N: usize = 1000;
const OBJ: Layout = match Layout::from_size_align(64, 16) {
    Ok(layout) => layout,
    Err(_) => panic!(),
};

fn churn(c: &mut Criterion) {
    let heap = Heap::<Model>::new(64 << 20).unwrap();
    c.bench_function("heap: 1000 x 64B alloc, free, sweep", |b| {
        b.iter(|| {
            let mut m = heap.mutator();
            let objs: Vec<_> = (0..N)
                .map(|_| {
                    let obj = m.alloc(OBJ).unwrap();
                    unsafe { obj.cast::<Layout>().write(OBJ) };
                    obj
                })
                .collect();
            drop(m);
            let mut collector = heap.collector();
            for obj in objs {
                unsafe { collector.free(obj) };
            }
            collector.sweep();
        })
    });
    c.bench_function("box: 1000 x 64B alloc, free", |b| {
        b.iter(|| {
            let objs: Vec<Box<[u8; 64]>> = (0..N).map(|_| Box::new([0; 64])).collect();
            drop(objs);
        })
    });
}

criterion_group!(benches, churn);
criterion_main!(benches);
