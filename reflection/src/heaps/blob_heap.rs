use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, RwLock};

use fxhash::FxHashMap;
use nohash_hasher::BuildNoHashHasher;

#[allow(unused_imports)]
use std::io::{Read, Write, Error, ErrorKind};

use crate::heaps::ArenaAllocator;
use crate::heaps::blob_heap::intern::Intern;
use crate::Type;

pub struct BlobHeap<'l> {
	buf: &'l ArenaAllocator,
	map: RwLock<FxHashMap<&'l [u8], &'l [u8]>>,
}

impl<'l> BlobHeap<'l> {
	pub fn new(bump: &'l ArenaAllocator) -> Self {
		Self {
			buf: bump,
			map: RwLock::default(),
		}
	}

	#[tracing::instrument(skip_all)]
	pub fn intern<T: Intern<'l>>(&self, data: T) -> T::Interned {
		data.intern(self).0
	}

	pub fn make_scope(heap: &Arc<BlobHeap<'l>>) -> Arc<BlobHeapScope<'l>> {
		let scope = BlobHeapScope {
			heap: heap.clone(),
			map: RwLock::default(),
			vec: RwLock::default(),
			ptr: RwLock::default(),
		};
		scope.intern("");
		scope.intern([Type::Void.discriminant()].as_slice());
		scope.intern([Type::Char.discriminant()].as_slice());
		scope.intern([Type::Bool.discriminant()].as_slice());
		scope.intern([Type::Int8.discriminant()].as_slice());
		scope.intern([Type::Int16.discriminant()].as_slice());
		scope.intern([Type::Int32.discriminant()].as_slice());
		scope.intern([Type::Int64.discriminant()].as_slice());
		scope.intern([Type::UInt8.discriminant()].as_slice());
		scope.intern([Type::UInt16.discriminant()].as_slice());
		scope.intern([Type::UInt32.discriminant()].as_slice());
		scope.intern([Type::UInt64.discriminant()].as_slice());
		scope.intern([Type::Float16.discriminant()].as_slice());
		scope.intern([Type::Float32.discriminant()].as_slice());
		scope.intern([Type::Float64.discriminant()].as_slice());
		Arc::new(scope)
	}
}

pub struct BlobHeapScope<'l> {
	heap: Arc<BlobHeap<'l>>,
	vec: RwLock<Vec<&'l [u8]>>,
	map: RwLock<FxHashMap<&'l [u8], usize>>,
	ptr: RwLock<HashMap<*const u8, usize, BuildNoHashHasher<usize>>>,
}

impl<'l> BlobHeapScope<'l> {
	#[tracing::instrument(skip_all)]
	pub fn intern<T: Intern<'l>>(&self, data: T) -> (T::Interned, usize) {
		data.intern_in_scope(self)
	}

	pub fn get_blob_index(&self, blob: &[u8]) -> Option<usize> {
		self.map.read().unwrap().get(blob).cloned()
	}

	pub fn get_blob_at_index(&self, idx: usize) -> Option<&'l [u8]> {
		self.vec.read().unwrap().get(idx).cloned()
	}

	pub fn get_str_at_index(&self, idx: usize) -> Option<&'l str> {
		std::str::from_utf8(self.vec.read().unwrap().get(idx)?).ok()
	}
}

impl Debug for BlobHeapScope<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use std::fmt::Write;
		let mut dbg = f.debug_struct("BlobHeapScope");
		let vec = self.vec.read().unwrap();
		let mut name = String::new();
		for (i, blob) in vec.iter().enumerate() {
			name.clear();
			write!(name, "{i}").unwrap();
			dbg.field(
				&name,
				&format_args!("[u8; {}] @ {:#?}", blob.len(), blob.as_ptr()),
			);
		}
		dbg.finish()
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
					let vec = scope.vec.read().unwrap();
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
		fn intern(self, heap: &BlobHeap<'l>) -> (Self::Interned, bool) {
			self.as_str().intern(heap)
		}

		fn intern_in_scope(self, heap: &BlobHeapScope<'l>) -> (Self::Interned, usize) {
			self.as_str().intern_in_scope(heap)
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

#[cfg(feature = "read")]
impl<'val: 'req, 'req> crate::serialization::MetadataRead<'val, 'req> for Arc<BlobHeapScope<'val>> {
	type Requirements = &'req Arc<BlobHeap<'val>>;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		let count = usize::read(stream, ())?;
		let mut buffer = vec![];
		let heap = BlobHeap::make_scope(req.into());
		for _ in 0..count {
			let len = usize::read(stream, ())?;
			unsafe {
				buffer.set_len(0);
				buffer.reserve_exact(len);
				buffer.set_len(len);
			}
			stream.read_exact(&mut buffer)?;
			heap.intern(buffer.as_slice());
		}
		Ok(heap)
	}
}

#[cfg(feature = "write")]
impl<'val> crate::serialization::MetadataWrite<'val, '_> for BlobHeapScope<'val> {
	type Requirements = ();
	fn write<S: Write>(
		&self,
		stream: &mut S,
		_: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		let buffers = self.vec.read().unwrap();
		buffers.len().write(stream, ())?;
		for buf in buffers.iter() {
			buf.len().write(stream, ())?;
			stream.write_all(buf)?;
		}
		Ok(())
	}
}
