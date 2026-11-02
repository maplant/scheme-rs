use std::{
    alloc::Layout,
    cell::UnsafeCell,
    fmt::{self, Debug, Formatter},
    ptr::NonNull,
    sync::atomic::AtomicUsize,
};

#[cfg(not(feature = "lxr"))]
use crate::cycle_collector::CycleHeader;
#[cfg(feature = "lxr")]
use crate::lxr_collector::LxrHeader;
use crate::{AllocError, Mutator, region::MAX_ALIGN};

#[derive(Debug)]
pub struct VTable {
    /// Type-erased visitor function
    pub visit_children: unsafe fn(this: *const (), visitor: &mut dyn FnMut(HeapObject)),
    /// Type-erased finalizer function
    pub finalize: unsafe fn(this: *mut ()),
}

#[cfg(not(feature = "lxr"))]
pub type Header = CycleHeader;

#[cfg(feature = "lxr")]
pub type Header = LxrHeader; // todo: use the LXR header.

pub const HEADER_SIZE: usize = size_of::<Header>();

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct HeapObject {
    /// Object header
    pub(crate) header: NonNull<UnsafeCell<Header>>,
}

impl Debug for HeapObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.header.as_ptr())
    }
}

impl HeapObject {
    /// Allocates an object of `data_layout` behind a header, in the heap of
    /// `mutator`.
    ///
    /// # Safety
    ///
    /// The memory returned is uninitialized.
    pub unsafe fn alloc_layout_in(
        mutator: &mut Mutator<'_, Header>,
        data_layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        let layout = Self::object_layout(data_layout);
        Ok(unsafe { Self::init_header(mutator.alloc(layout)?, layout) })
    }

    fn object_layout(data_layout: Layout) -> Layout {
        assert!(data_layout.align() <= MAX_ALIGN);
        let data_layout = Layout::from_size_align(data_layout.size().max(1), data_layout.align())
            .expect("Layout creation failed in alloc_layout");

        let (unpadded_layout, _) = Layout::new::<Header>().extend(data_layout).unwrap();
        unpadded_layout.pad_to_align()
    }

    unsafe fn init_header(ptr: NonNull<u8>, layout: Layout) -> NonNull<u8> {
        let header_ptr = ptr.cast::<Header>();
        unsafe {
            header_ptr.write(Header::new(layout));
            header_ptr.byte_add(HEADER_SIZE).cast()
        }
    }

    /// # Safety
    ///
    /// The object is live.
    pub unsafe fn rc(&self) -> &AtomicUsize {
        unsafe { (*self.header.as_ref().get()).rc() }
    }

    pub(crate) unsafe fn from_header(header: *mut Header) -> Option<Self> {
        NonNull::new(header).map(|h| Self { header: h.cast() })
    }

    /// # Safety
    ///
    /// data was returned by one of HeapObjects alloc functions,
    /// and the object is live.
    pub unsafe fn from_data<T: ?Sized>(data: NonNull<T>) -> Self {
        let header: NonNull<Header> = unsafe { data.byte_sub(HEADER_SIZE) }.cast::<Header>();

        Self {
            header: header.cast::<UnsafeCell<Header>>(),
        }
    }

    pub(crate) unsafe fn as_ptr(&self) -> *mut Header {
        self.header.as_ptr() as *mut Header
    }

    /// # Safety
    ///
    /// The object is live.
    pub unsafe fn data(&self) -> *const () {
        unsafe { self.header.as_ptr().byte_add(HEADER_SIZE) }.cast()
    }

    /// # Safety
    ///
    /// The object is live.
    pub unsafe fn data_mut(&self) -> *mut () {
        unsafe { self.header.as_ptr().byte_add(HEADER_SIZE) }.cast()
    }
}

unsafe impl Send for HeapObject {}
unsafe impl Sync for HeapObject {}
