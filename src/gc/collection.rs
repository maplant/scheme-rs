use scheme_rs_gc::{GcHeader, collect_garbage as collect_garbage_sync};
use scheme_rs_macros::{maybe_async, maybe_await};
#[cfg(feature = "tokio")]
use tokio::task::spawn_blocking;

use crate::registry::bridge;

#[bridge(name = "gc-header-size", lib = "(runtime (1))")]
pub fn gc_header_size() -> usize {
    size_of::<GcHeader>()
}

/// Force a garbage collection pause.
#[cfg(not(feature = "async"))]
pub fn collect_garbage() {
    collect_garbage_sync();
}

#[cfg(feature = "tokio")]
pub async fn collect_garbage() {
    spawn_blocking(collect_garbage_sync).await.unwrap();
}

#[maybe_async]
#[bridge(name = "collect-garbage", lib = "(runtime (1))")]
pub fn collect_garbage_bridge() {
    maybe_await!(collect_garbage());
}

#[cfg(test)]
mod test {
    use super::collect_garbage_sync;
    use crate::gc::*;
    use parking_lot::{Mutex, RwLock};
    use std::sync::Arc;

    #[test]
    fn cycles() {
        init_gc();

        #[derive(Default, Trace)]
        struct Cyclic {
            next: Option<Gc<RwLock<Cyclic>>>,
            out: Option<Arc<()>>,
        }

        let out_ptr = Arc::new(());

        let a = Gc::new(RwLock::new(Cyclic::default()));
        let b = Gc::new(RwLock::new(Cyclic::default()));
        let c = Gc::new(RwLock::new(Cyclic::default()));

        // a -> b -> c -
        // ^----------/
        a.write().next = Some(b.clone());
        b.write().next = Some(c.clone());
        b.write().out = Some(out_ptr.clone());
        c.write().next = Some(a.clone());

        assert_eq!(Arc::strong_count(&out_ptr), 2);

        drop(a);
        drop(b);
        drop(c);

        collect_garbage_sync();
        collect_garbage_sync();
        collect_garbage_sync();

        assert_eq!(Arc::strong_count(&out_ptr), 1);
    }

    static FINALIZE_ORDER: Mutex<Vec<usize>> = Mutex::new(Vec::new());

    struct Tag(usize);

    unsafe impl Trace for Tag {
        unsafe fn visit_children(&self, _visitor: &mut dyn FnMut(OpaqueGcPtr)) {}

        unsafe fn finalize(&mut self) {
            FINALIZE_ORDER.lock().push(self.0);
        }
    }

    #[derive(Trace)]
    struct TreeNode {
        tag: Tag,
        kids: Vec<Gc<RwLock<TreeNode>>>,
    }

    fn tree_node(tag: usize, kids: Vec<Gc<RwLock<TreeNode>>>) -> Gc<RwLock<TreeNode>> {
        Gc::new(RwLock::new(TreeNode {
            tag: Tag(tag),
            kids,
        }))
    }

    #[test]
    fn release_cascade_finalizes_children_before_parents() {
        init_gc();

        // 0 -> (1 -> (3, 4), 2 -> 5)
        let root = tree_node(
            0,
            vec![
                tree_node(1, vec![tree_node(3, Vec::new()), tree_node(4, Vec::new())]),
                tree_node(2, vec![tree_node(5, Vec::new())]),
            ],
        );

        collect_garbage_sync();
        collect_garbage_sync();
        FINALIZE_ORDER.lock().clear();

        drop(root);
        collect_garbage_sync();
        collect_garbage_sync();

        let order = FINALIZE_ORDER.lock().clone();
        assert_eq!(order.len(), 6, "not every node finalized: {order:?}");
        let at = |tag| order.iter().position(|&t| t == tag).unwrap();
        for (child, parent) in [(3, 1), (4, 1), (5, 2), (1, 0), (2, 0)] {
            assert!(at(child) < at(parent), "{child} finalized after {parent}");
        }
    }

    /// Deep enough to exhaust the collector's 2 MiB stack in any build profile.
    const DEEP: usize = 200_000;

    #[derive(Default, Trace)]
    struct DeepNode {
        next: Option<Gc<RwLock<DeepNode>>>,
        chain: Option<Gc<RwLock<DeepNode>>>,
        out: Option<Arc<()>>,
    }

    fn deep_chain(len: usize, out: &Arc<()>) -> Gc<RwLock<DeepNode>> {
        let mut head = Gc::new(RwLock::new(DeepNode {
            out: Some(out.clone()),
            ..Default::default()
        }));
        for _ in 1..len {
            head = Gc::new(RwLock::new(DeepNode {
                next: Some(head),
                ..Default::default()
            }));
        }
        head
    }

    #[test]
    fn deep_chain_drop_releases_fully() {
        init_gc();

        let out = Arc::new(());
        let head = deep_chain(DEEP, &out);
        assert_eq!(Arc::strong_count(&out), 2);

        // Do not remove these. Until every node has been through an epoch its
        // buffered flag stops the cascade after a single node, so without them
        // the drop below never recurses deeply and this test passes even
        // against the unfixed recursive cascade.
        collect_garbage_sync();
        collect_garbage_sync();

        drop(head);
        collect_garbage_sync();
        collect_garbage_sync();
        assert_eq!(Arc::strong_count(&out), 1, "deep chain not fully released");
    }

    /// Covers the other entry point: `free_cycle` reaching the cascade through
    /// `cyclic_decrement`, rather than the rc-zero sweep.
    #[test]
    fn deep_chain_behind_freed_cycle_releases_fully() {
        init_gc();

        let out = Arc::new(());
        let chain = deep_chain(DEEP, &out);
        // Do not remove these; same reason as the previous test.
        collect_garbage_sync();
        collect_garbage_sync();

        let a = Gc::new(RwLock::new(DeepNode::default()));
        let b = Gc::new(RwLock::new(DeepNode::default()));
        a.write().next = Some(b.clone());
        b.write().next = Some(a.clone());
        b.write().chain = Some(chain.clone());

        drop(a);
        drop(b);
        // Trial-deletes the a/b cycle.
        collect_garbage_sync();

        // Frees a/b, whose chain edge then decrements the head to zero.
        drop(chain);
        collect_garbage_sync();
        collect_garbage_sync();
        assert_eq!(
            Arc::strong_count(&out),
            1,
            "chain behind freed cycle not released"
        );
    }
}
