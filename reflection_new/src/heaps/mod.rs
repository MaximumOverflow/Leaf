mod string_heap;
mod blob_heap;
mod type_heap;

use bumpalo::Bump;
pub use type_heap::*;
pub use blob_heap::*;
pub use string_heap::*;

pub struct Heaps<'l> {
	type_heap: TypeHeap<'l>,
	blob_heap: BlobHeap<'l>,
	string_heap: StringHeap<'l>,
}

impl<'l> Heaps<'l> {
	pub fn new(bump: &'l Bump) -> Self {
		Self {
			type_heap: TypeHeap::new(bump),
			blob_heap: BlobHeap::new(bump),
			string_heap: StringHeap::new(bump),
		}
	}

	pub fn type_heap(&self) -> &TypeHeap<'l> {
		&self.type_heap
	}

	pub fn blob_heap(&self) -> &BlobHeap<'l> {
		&self.blob_heap
	}

	pub fn string_heap(&self) -> &StringHeap<'l> {
		&self.string_heap
	}
}
