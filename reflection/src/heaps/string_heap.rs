use std::collections::HashMap;
use std::cell::RefCell;
use bumpalo::Bump;

pub struct StringHeap<'l> {
	buf: &'l Bump,
	map: RefCell<HashMap<&'l str, &'l str>>,
}

pub struct StringHeapScope<'l> {
	heap: &'l StringHeap<'l>,
	map: RefCell<HashMap<&'l str, usize>>,
	vec: RefCell<Vec<&'l str>>,
}

impl<'l> StringHeap<'l> {
	pub fn new(bump: &'l Bump) -> Self {
		let heap = Self {
			buf: bump,
			map: RefCell::default(),
		};
		heap.intern_str("");
		heap
	}

	pub fn intern_str(&self, str: &str) -> &'l str {
		let mut map = self.map.borrow_mut();

		if let Some(str) = map.get(str) {
			return str;
		}

		let str = self.buf.alloc_str(str);
		map.insert(str, str);
		str
	}

	pub fn make_scope(&'l self) -> StringHeapScope<'l> {
		StringHeapScope {
			heap: self,
			map: RefCell::default(),
			vec: RefCell::default(),
		}
	}
}

impl<'l> StringHeapScope<'l> {
	pub fn intern_str(&self, str: &str) -> (&'l str, usize) {
		let mut map = self.map.borrow_mut();
		let mut vec = self.vec.borrow_mut();

		if let Some(idx) = map.get(str) {
			return (vec[*idx], *idx);
		}

		let str = self.heap.intern_str(str);
		let idx = vec.len();

		vec.push(str);
		map.insert(str, idx);
		(str, idx)
	}
}

#[cfg(feature = "write")]
mod write {
	use std::io::Error;
	use crate::write::Write;
	use crate::heaps::StringHeapScope;

	impl Write<'_> for StringHeapScope<'_> {
		type Requirements = ();
		fn write<T: std::io::Write>(&'_ self, stream: &mut T, _: ()) -> Result<(), Error> {
			let strings = self.vec.borrow();
			strings.len().write(stream, ())?;

			for str in strings.iter() {
				(str.len() + 1).write(stream, ())?;
				stream.write_all(str.as_bytes())?;
				stream.write_all(&[0])?;
			}

			Ok(())
		}
	}
}
