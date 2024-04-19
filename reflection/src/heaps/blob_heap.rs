use std::collections::HashMap;
use std::sync::RwLock;

use bumpalo::Bump;
use fxhash::FxHashMap;
use nohash_hasher::BuildNoHashHasher;

use crate::heaps::blob_heap::intern::Intern;

pub struct BlobHeap<'l> {
	buf: &'l Bump,
	map: RwLock<FxHashMap<&'l [u8], &'l [u8]>>,
}

pub struct BlobHeapScope<'l> {
	heap: &'l BlobHeap<'l>,
	vec: RwLock<Vec<&'l [u8]>>,
	map: RwLock<FxHashMap<&'l [u8], usize>>,
	ptr: RwLock<HashMap<*const u8, usize, BuildNoHashHasher<usize>>>,
}

impl<'l> BlobHeap<'l> {
	pub fn new(bump: &'l Bump) -> Self {
		Self {
			buf: bump,
			map: RwLock::default(),
		}
	}

	#[tracing::instrument(skip_all)]
	pub fn intern<T: Intern<'l>>(&self, data: T) -> T::Interned {
		data.intern(self).0
	}

	pub fn make_scope(&'l self) -> BlobHeapScope<'l> {
		BlobHeapScope {
			heap: self,
			map: RwLock::default(),
			vec: RwLock::default(),
			ptr: RwLock::default(),
		}
	}
}

impl<'l> BlobHeapScope<'l> {
	#[tracing::instrument(skip_all)]
	pub fn intern<T: Intern<'l>>(&self, data: T) -> (T::Interned, usize) {
		data.intern_in_scope(self)
	}

	pub fn get_blob_index(&self, blob: &[u8]) -> Option<usize> {
		self.map.read().unwrap().get(blob).cloned()
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
			{
				let map = heap.map.read().unwrap();
				if let Some(blob) = map.get(self) {
					return (blob, false);
				}
			}

			let mut map = heap.map.write().unwrap();
			let blob = heap.buf.alloc_slice_copy(self);
			map.insert(blob, blob);
			(blob, true)
		}

		fn intern_in_scope(self, scope: &BlobHeapScope<'l>) -> (Self::Interned, usize) {
			{
				let ptr = scope.ptr.read().unwrap();
				if let Some(idx) = ptr.get(&self.as_ptr()) {
					let mut vec = scope.vec.read().unwrap();
					return (vec[*idx], *idx);
				}
			}
			{
				let map = scope.map.read().unwrap();
				if let Some(idx) = map.get(self) {
					let vec = scope.vec.read().unwrap();
					return (vec[*idx], *idx);
				}
			}

			let blob = scope.heap.intern(self);

			let mut vec = scope.vec.write().unwrap();
			let idx = vec.len();
			vec.push(blob);
			drop(vec);

			let mut map = scope.map.write().unwrap();
			map.insert(blob, idx);
			drop(map);

			let mut ptr = scope.ptr.write().unwrap();
			ptr.insert(blob.as_ptr(), idx);
			drop(ptr);

			(blob, idx)
		}
	}

	impl<'l> Intern<'l> for &str {
		type Interned = &'l str;
		fn intern(self, heap: &BlobHeap<'l>) -> (Self::Interned, bool) {
			let (blob, added) = self.as_bytes().intern(heap);
			(unsafe { std::str::from_utf8_unchecked(&blob) }, added)
		}

		fn intern_in_scope(self, heap: &BlobHeapScope<'l>) -> (Self::Interned, usize) {
			let (blob, idx) = self.as_bytes().intern_in_scope(heap);
			(unsafe { std::str::from_utf8_unchecked(&blob) }, idx)
		}
	}

	impl<'l> Intern<'l> for String {
		type Interned = &'l str;
		fn intern(mut self, heap: &BlobHeap<'l>) -> (Self::Interned, bool) {
			let (blob, added) = self.as_bytes().intern(heap);
			(unsafe { std::str::from_utf8_unchecked(&blob) }, added)
		}

		fn intern_in_scope(mut self, heap: &BlobHeapScope<'l>) -> (Self::Interned, usize) {
			let (blob, idx) = self.as_bytes().intern_in_scope(heap);
			(unsafe { std::str::from_utf8_unchecked(&blob) }, idx)
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
