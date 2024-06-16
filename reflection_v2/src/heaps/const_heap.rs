use std::borrow::Borrow;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::io::{Result as IoResult, Write as IoWrite};
use crate::heaps::const_heap::intern::FromInterned;
use owning_ref::{ArcRef, OwningRef};
use crate::serialization::Write;
use parking_lot::{Mutex, MutexGuard};
use std::marker::PhantomData;
use std::sync::{Arc, Weak};
use petgraph::dot::Dot;
use fxhash::FxHashMap;
use petgraph::Graph;
use intern::Intern;
use bumpalo::Bump;

macro_rules! decl_constant_kinds {
    ($($ty: ty: $kind: ident,)*) => {
		#[repr(u8)]
		#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
		pub enum ConstKind {
			$($kind),*
		}

		#[derive(Clone)]
		pub enum AnyConstRef {
			$($kind(ConstRef<$ty>)),*
		}

		$(
			impl From<ConstRef<$ty>> for AnyConstRef {
				fn from(value: ConstRef<$ty>) -> Self {
					Self::$kind(value)
				}
			}

			impl TryFrom<AnyConstRef> for ConstRef<$ty> {
				type Error = ();
				fn try_from(value: AnyConstRef) -> Result<Self, Self::Error> {
					match value {
						AnyConstRef::$kind(v) => Ok(v),
						_ => Err(()),
					}
				}
			}
		)*

		impl AnyConstRef {
			fn as_bytes(&self) -> &[u8] {
				match self {
					$(
						AnyConstRef::$kind(v) => &v.val,
					)*
				}
			}
		}

		impl Debug for AnyConstRef {
			fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
				#[allow(unreachable_patterns)]
				match self {
					AnyConstRef::Blob(v) => {
						struct HiddenForBrevity;
						impl Debug for HiddenForBrevity {
							fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
								write!(f, "[..]")
							}
						}
						let slice = v.get();
						let name = format!("ConstRef<[u8; {}]>", slice.len());
						let mut dbg = f.debug_tuple(&name);
						if slice.len() <= 4 {
							dbg.field(&slice);
						} else {
							dbg.field(&HiddenForBrevity);
						}
						dbg.finish()
					},
					$(
						AnyConstRef::$kind(v) => {
							const NAME: &str = concat!("ConstRef<", stringify!($ty), ">");
							let mut dbg = f.debug_tuple(NAME);
							dbg.field(&v.get());
							dbg.finish()
						},
					)*
				}
			}
		}

		impl ConstHeap {
			pub fn get(&self, idx: usize) -> Option<AnyConstRef> {
				let inner = self.inner.lock();
				let entry = inner.entry_vec.get(idx)?;
				let self_ref = self.self_ref.upgrade().unwrap();
				let const_ref = OwningRef::new(self_ref);
				let result = match entry.kind {
					$(
						ConstKind::$kind => AnyConstRef::$kind(ConstRef {
							idx,
							pmd: PhantomData,
							val: const_ref.map(|_| unsafe { std::slice::from_raw_parts(entry.ptr, entry.len) }),
						}),
					)*
				};
				Some(result)
			}
		}

		impl Iterator for Iter<'_> {
			type Item = AnyConstRef;
			fn next(&mut self) -> Option<Self::Item> {
				let entry = self.guard.entry_vec.get(self.idx)?;
				let const_ref = OwningRef::new(self.self_ref.clone());
				let result = match entry.kind {
					$(
						ConstKind::$kind => AnyConstRef::$kind(ConstRef {
							idx: self.idx,
							pmd: PhantomData,
							val: const_ref.map(|_| unsafe { std::slice::from_raw_parts(entry.ptr, entry.len) }),
						}),
					)*
				};

				self.idx += 1;
				Some(result)
			}
		}
	};
}

decl_constant_kinds! {
	[u8]: Blob,
	str: String,

	i8: Int8,
	i16: Int16,
	i32: Int32,
	i64: Int64,
	isize: NInt,

	u8: UInt8,
	u16: UInt16,
	u32: UInt32,
	u64: UInt64,
	usize: NUInt,

	f32: Float32,
	f64: Float64,
}

pub struct ConstRef<T: ?Sized> {
	idx: usize,
	pmd: PhantomData<T>,
	val: ArcRef<ConstHeap, [u8]>,
}

impl<T: ?Sized> Clone for ConstRef<T> {
	#[inline]
	fn clone(&self) -> Self {
		Self {
			idx: self.idx,
			pmd: PhantomData,
			val: self.val.clone(),
		}
	}
}

impl<T: ?Sized> ConstRef<T> {
	#[inline]
	pub fn idx(&self) -> usize {
		self.idx
	}

	#[inline]
	pub fn as_bytes(&self) -> &[u8] {
		&self.val
	}
}

impl<'l, T: ?Sized + FromInterned<'l>> ConstRef<T> {
	#[inline]
	pub fn get(&'l self) -> T::Type {
		T::from_interned(&self.val)
	}
}

impl<T: ?Sized + for<'l> FromInterned<'l>> Debug for ConstRef<T>
where
	for<'l> <T as FromInterned<'l>>::Type: Debug,
{
	#[inline]
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		self.get().fmt(f)
	}
}

impl<T: ?Sized + for<'l> FromInterned<'l>> Display for ConstRef<T>
where
	for<'l> <T as FromInterned<'l>>::Type: Display,
{
	#[inline]
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		self.get().fmt(f)
	}
}

impl<T: ?Sized + for<'l> FromInterned<'l>> Eq for ConstRef<T> where
	for<'l> <T as FromInterned<'l>>::Type: Eq
{
}

impl<T: ?Sized + for<'l> FromInterned<'l>> PartialEq for ConstRef<T>
where
	for<'l> <T as FromInterned<'l>>::Type: PartialEq,
{
	#[inline]
	fn eq(&self, other: &Self) -> bool {
		self.get() == other.get()
	}

	#[inline]
	fn ne(&self, other: &Self) -> bool {
		self.get() != other.get()
	}
}

impl<T: ?Sized + for<'l> FromInterned<'l>> Hash for ConstRef<T>
where
	for<'l> <T as FromInterned<'l>>::Type: Hash,
{
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.get().hash(state)
	}
}

#[derive(Copy, Clone)]
struct Entry {
	len: usize,
	ptr: *const u8,
	kind: ConstKind,
}
unsafe impl Send for Entry {}

impl Eq for Entry {}

impl PartialEq for Entry {
	#[inline]
	fn eq(&self, other: &Self) -> bool {
		let a = unsafe { std::slice::from_raw_parts(self.ptr, self.len) };
		let b = unsafe { std::slice::from_raw_parts(other.ptr, other.len) };
		self.kind == other.kind && a == b
	}
}

impl Hash for Entry {
	fn hash<H: Hasher>(&self, state: &mut H) {
		unsafe { std::slice::from_raw_parts(self.ptr, self.len) }.hash(state)
	}
}

#[derive(Copy, Clone)]
struct Blob {
	len: usize,
	ptr: *const u8,
}
unsafe impl Send for Blob {}

impl Borrow<[u8]> for Blob {
	fn borrow(&self) -> &[u8] {
		unsafe { std::slice::from_raw_parts(self.ptr, self.len) }
	}
}

impl Eq for Blob {}

impl PartialEq for Blob {
	#[inline]
	fn eq(&self, other: &Self) -> bool {
		let a: &[u8] = self.borrow();
		let b: &[u8] = other.borrow();
		a == b
	}
}

impl Hash for Blob {
	#[inline]
	fn hash<H: Hasher>(&self, state: &mut H) {
		let bytes: &[u8] = self.borrow();
		bytes.hash(state)
	}
}

struct Inner {
	mem: Bump,
	entry_vec: Vec<Entry>,
	entry_map: FxHashMap<Entry, usize>,

	tmp: Bump,
	blob_map: FxHashMap<Blob, Blob>,
}

pub struct ConstHeap {
	inner: Mutex<Inner>,
	self_ref: Weak<ConstHeap>,
}

impl ConstHeap {
	pub fn new() -> Arc<Self> {
		Arc::new_cyclic(|self_ref| Self {
			self_ref: self_ref.clone(),
			inner: Mutex::new(Inner {
				mem: Bump::new(),
				tmp: Bump::new(),
				blob_map: Default::default(),
				entry_vec: Default::default(),
				entry_map: Default::default(),
			}),
		})
	}

	pub fn intern<T: Intern>(&self, v: T) -> ConstRef<T::Interned> {
		let mut idx = 0;
		let self_ref = self.self_ref.upgrade().unwrap();
		let const_ref = OwningRef::new(self_ref);
		let val = const_ref.map(|heap| {
			let mut inner = heap.inner.lock();
			let Inner {
				mem,
				entry_vec,
				entry_map,
				tmp,
				blob_map,
			} = &mut *inner;

			let bytes = v.intern(tmp);
			let blob = match blob_map.get(bytes) {
				Some(blob) => *blob,
				None => {
					let interned = mem.alloc_slice_copy(bytes);
					let blob = Blob {
						len: interned.len(),
						ptr: interned.as_ptr(),
					};
					blob_map.insert(blob, blob);
					blob
				},
			};
			tmp.reset();

			let entry = Entry {
				len: blob.len,
				ptr: blob.ptr,
				kind: T::KIND,
			};

			idx = *entry_map.entry(entry).or_insert_with(|| {
				let idx = entry_vec.len();
				entry_vec.push(entry);
				idx
			});

			unsafe { std::slice::from_raw_parts(entry.ptr, entry.len) }
		});
		ConstRef {
			idx,
			val,
			pmd: PhantomData,
		}
	}

	#[inline]
	pub fn iter(&self) -> Iter {
		Iter {
			idx: 0,
			guard: self.inner.lock(),
			self_ref: self.self_ref.upgrade().unwrap(),
		}
	}

	pub fn as_graph(&self) -> String {
		struct Nothing;
		impl Display for Nothing {
			fn fmt(&self, _: &mut Formatter<'_>) -> std::fmt::Result {
				Ok(())
			}
		}

		let inner = self.inner.lock();
		let Inner {
			blob_map: blobs, ..
		} = &*inner;

		let mut blob_nodes = vec![];
		let mut graph = Graph::new();

		let mut blob_map = FxHashMap::default();
		for blob in blobs.values() {
			let idx = blob_map.len();
			blob_map.insert(blob.ptr, idx);
			let node = graph.add_node(format!("Blob {idx}"));
			blob_nodes.push(node);
		}

		drop(inner);
		for const_ref in self.iter() {
			let idx = blob_map[&const_ref.as_bytes().as_ptr()];
			let blob = blob_nodes[idx];
			let node = graph.add_node(format!("{:?}", const_ref));
			graph.add_edge(node, blob, Nothing);
		}

		format!("{}", Dot::new(&graph))
	}
}

impl Write for ConstHeap {
	fn write<Stream: IoWrite>(&self, stream: &mut Stream) -> IoResult<()> {
		let inner = self.inner.lock();
		let Inner {
			entry_vec: entries,
			blob_map: blobs,
			..
		} = &*inner;

		let mut blob_vec = vec![];
		let mut blob_map = FxHashMap::default();
		for blob in blobs.values() {
			let idx = blob_map.len();
			blob_map.insert(blob.ptr, idx);
			blob_vec.push(*blob);
		}

		entries.len().write(stream)?;
		for entry in entries {
			stream.write_all(&[entry.kind as u8])?;
			blob_map[&entry.ptr].write(stream)?;
		}

		blob_vec.len().write(stream)?;
		for blob in blob_vec {
			unsafe { std::slice::from_raw_parts(blob.ptr, blob.len) }.write(stream)?;
		}

		Ok(())
	}
}

pub struct Iter<'l> {
	idx: usize,
	self_ref: Arc<ConstHeap>,
	guard: MutexGuard<'l, Inner>,
}

mod intern {
	use std::io::Cursor;
	use bumpalo::Bump;
	use crate::serialization::{Read, Write};
	use super::ConstKind;

	pub trait Intern {
		type Interned: ?Sized;
		const KIND: ConstKind;
		fn intern<'a>(&self, bump: &'a Bump) -> &'a [u8];
	}

	pub trait FromInterned<'l> {
		type Type: 'l;
		fn from_interned(v: &'l [u8]) -> Self::Type;
	}

	impl Intern for &[u8] {
		type Interned = [u8];
		const KIND: ConstKind = ConstKind::Blob;
		#[inline]
		fn intern<'a>(&self, bump: &'a Bump) -> &'a [u8] {
			bump.alloc_slice_copy(self)
		}
	}

	impl<'l> FromInterned<'l> for [u8] {
		type Type = &'l [u8];
		#[inline]
		fn from_interned(bytes: &'l [u8]) -> Self::Type {
			bytes
		}
	}

	impl Intern for &str {
		type Interned = str;
		const KIND: ConstKind = ConstKind::String;
		#[inline]
		fn intern<'a>(&self, bump: &'a Bump) -> &'a [u8] {
			let bytes = bump.alloc_slice_fill_default(self.len() + 1);
			let [str @ .., 0] = bytes else { unreachable!() };
			str.copy_from_slice(self.as_bytes());
			bytes
		}
	}

	impl<'l> FromInterned<'l> for str {
		type Type = &'l str;

		#[inline]
		fn from_interned(bytes: &'l [u8]) -> Self::Type {
			let [str @ .., 0] = bytes else { unreachable!() };
			unsafe { std::str::from_utf8_unchecked(str) }
		}
	}

	macro_rules! impl_primitive {
		($($ty: ty: $kind: ident,)*) => {
			$(
				impl Intern for $ty {
					type Interned = Self;
					const KIND: ConstKind = ConstKind::$kind;
					#[inline]
					fn intern<'a>(&self, bump: &'a Bump) -> &'a [u8] {
						bump.alloc_slice_copy(&self.to_le_bytes())
					}
				}

				impl FromInterned<'_> for $ty {
					type Type = Self;
					#[inline]
					fn from_interned(v: &'_ [u8]) -> Self::Type {
						Self::from_le_bytes(v.try_into().unwrap())
					}
				}
			)*
		};
	}

	macro_rules! impl_coded {
		($($ty: ty: $kind: ident,)*) => {
			$(
				impl Intern for $ty {
					type Interned = Self;
					const KIND: ConstKind = ConstKind::$kind;
					fn intern<'a>(&self, bump: &'a Bump) -> &'a [u8] {
						let mut bytes: [u8; 32] = [0u8; 32];
						let mut cursor = Cursor::new(bytes.as_mut_slice());
						(*self).write(&mut cursor).unwrap();
						let len = cursor.position() as usize;
						bump.alloc_slice_copy(&bytes[..len])
					}
				}

				impl FromInterned<'_> for $ty {
					type Type = Self;
					fn from_interned(v: &'_ [u8]) -> Self::Type {
						let mut cursor = Cursor::new(v);
						Self::read(&mut cursor, &()).unwrap()
					}
				}
			)*
		};
	}

	impl_primitive! {
		i8: Int8,
		i16: Int16,
		i32: Int32,
		i64: Int64,
		u8: UInt8,
		u16: UInt16,
		u32: UInt32,
		u64: UInt64,
		f32: Float32,
		f64: Float64,
	}

	impl_coded! {
		isize: NInt,
		usize: NUInt,
	}
}
