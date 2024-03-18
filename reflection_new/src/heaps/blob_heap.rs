use std::collections::HashMap;
use std::cell::RefCell;
use bumpalo::Bump;

pub struct BlobHeap<'l> {
    buf: &'l Bump,
    map: RefCell<HashMap<&'l [u8], &'l [u8]>>,
}

pub struct BlobHeapScope<'l> {
    heap: &'l BlobHeap<'l>,
    map: RefCell<HashMap<&'l [u8], usize>>,
    vec: RefCell<Vec<&'l [u8]>>,
}

impl<'l> BlobHeap<'l> {
    pub fn new(bump: &'l Bump) -> Self {
        Self { buf: bump, map: RefCell::default() }
    }

    pub fn intern_blob(&self, blob: &[u8]) -> &'l [u8] {
        let mut map = self.map.borrow_mut();

        if let Some(blob) = map.get(blob) {
            return blob;
        }

        let blob = self.buf.alloc_slice_copy(blob);
        map.insert(blob, blob);
        blob
    }

    pub fn make_scope(&'l self) -> BlobHeapScope<'l> {
        BlobHeapScope {
            heap: self,
            map: RefCell::default(),
            vec: RefCell::default(),
        }
    }
}

impl<'l> BlobHeapScope<'l> {
    pub fn intern_blob(&self, str: &[u8]) -> (&'l [u8], usize) {
        let mut map = self.map.borrow_mut();
        let mut vec = self.vec.borrow_mut();

        if let Some(idx) = map.get(str) {
            return (vec[*idx], *idx);
        }

        let blob = self.heap.intern_blob(str);
        let idx = vec.len();

        vec.push(blob);
        map.insert(blob, idx);
        (blob, idx)
    }
}

#[cfg(feature = "write")]
mod write {
    use std::io::Error;
    use crate::write::Write;
    use crate::heaps::blob_heap::BlobHeapScope;

    impl Write<'_> for BlobHeapScope<'_> {
        type Requirements = ();
        fn write<T: std::io::Write>(&'_ self, stream: &mut T, _: ()) -> Result<(), Error> {
            let blobs = self.vec.borrow();
            blobs.len().write(stream, ())?;

            for blob in blobs.iter() {
                blob.len().write(stream, ())?;
                stream.write_all(blob)?;
            }

            Ok(())
        }
    }
}
