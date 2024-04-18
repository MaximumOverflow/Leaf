use crate::heaps::blob_heap::intern::Intern;
use std::collections::HashMap;
use std::cell::RefCell;
use bumpalo::Bump;

pub struct BlobHeap<'l> {
	buf: &'l Bump,
	map: RefCell<HashMap<&'l [u8], &'l [u8]>>,
}

pub struct BlobHeapScope<'l> {
	heap: &'l BlobHeap<'l>,
	vec: RefCell<Vec<&'l [u8]>>,
	map: RefCell<HashMap<&'l [u8], usize>>,
}

impl<'l> BlobHeap<'l> {
	pub fn new(bump: &'l Bump) -> Self {
		Self {
			buf: bump,
			map: RefCell::default(),
		}
	}

	pub fn intern<T: Intern<'l>>(&self, data: T) -> T::Interned {
		data.intern(self).0
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
	pub fn intern<T: Intern<'l>>(&self, data: T) -> (T::Interned, usize) {
		data.intern_in_scope(self)
	}
}

mod intern {
	use crate::heaps::{BlobHeap, BlobHeapScope};

	pub trait Intern<'l> {
		type Interned: 'l;
		fn intern(self, heap: &BlobHeap<'l>) -> (Self::Interned, bool);
		fn intern_in_scope(self, heap: &BlobHeapScope<'l>) -> (Self::Interned, usize);
	}

	impl<'l> Intern<'l> for &[u8] {
		type Interned = &'l [u8];
		fn intern(self, heap: &BlobHeap<'l>) -> (Self::Interned, bool) {
			let mut map = heap.map.borrow_mut();

			if let Some(blob) = map.get(self) {
				return (blob, false);
			}

			let blob = heap.buf.alloc_slice_copy(self);
			map.insert(blob, blob);
			(blob, true)
		}

		fn intern_in_scope(self, scope: &BlobHeapScope<'l>) -> (Self::Interned, usize) {
			let mut map = scope.map.borrow_mut();
			let mut vec = scope.vec.borrow_mut();

			if let Some(idx) = map.get(self) {
				return (vec[*idx], *idx);
			}

			let blob = scope.heap.intern(self);
			let idx = vec.len();

			vec.push(blob);
			map.insert(blob, idx);
			(blob, idx)
		}
	}

	impl<'l> Intern<'l> for &str {
		type Interned = &'l str;
		fn intern(self, heap: &BlobHeap<'l>) -> (Self::Interned, bool) {
			self.to_string().intern(heap)
		}

		fn intern_in_scope(self, heap: &BlobHeapScope<'l>) -> (Self::Interned, usize) {
			self.to_string().intern_in_scope(heap)
		}
	}

	impl<'l> Intern<'l> for String {
		type Interned = &'l str;
		fn intern(mut self, heap: &BlobHeap<'l>) -> (Self::Interned, bool) {
			if self.as_bytes().last() != Some(&b'\0') {
				self.push('\0');
			}
			let (blob, added) = self.as_bytes().intern(heap);
			(unsafe { std::str::from_utf8_unchecked(&blob[..blob.len() - 1]) }, added)
		}

		fn intern_in_scope(mut self, heap: &BlobHeapScope<'l>) -> (Self::Interned, usize) {
			if self.as_bytes().last() != Some(&b'\0') {
				self.push('\0');
			}
			let (blob, idx) = self.as_bytes().intern_in_scope(heap);
			(unsafe { std::str::from_utf8_unchecked(&blob[..blob.len() - 1]) }, idx)
		}
	}

	impl<'l> Intern<'l> for Vec<u8> {
		type Interned = &'l [u8];
		fn intern(self, heap: &BlobHeap<'l>) -> (Self::Interned, bool) {
			self.as_slice().intern(heap)
		}

		fn intern_in_scope(self, heap: &BlobHeapScope<'l>) -> (Self::Interned, usize) {
			self.as_slice().intern_in_scope(heap)
		}
	}
}

#[cfg(feature = "write")]
mod write {
	use std::io::Error;
	use crate::write::Write;
	use crate::heaps::blob_heap::BlobHeapScope;

	impl Write<'_, '_> for BlobHeapScope<'_> {
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
