mod string_heap;
mod blob_heap;
mod type_heap;

pub use type_heap::*;
pub use blob_heap::*;
pub use string_heap::*;
pub use bumpalo::Bump;

pub struct Heaps<'l> {
	bump: &'l Bump,
	type_heap: TypeHeap<'l>,
	blob_heap: BlobHeap<'l>,
	string_heap: StringHeap<'l>,
}

impl<'l> Heaps<'l> {
	pub fn new(bump: &'l Bump) -> Self {
		Self {
			bump,
			type_heap: TypeHeap::new(bump),
			blob_heap: BlobHeap::new(bump),
			string_heap: StringHeap::new(bump),
		}
	}

	pub fn bump(&self) -> &'l Bump {
		self.bump
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

#[derive(Copy, Clone)]
pub struct HeapScopeRefs<'l> {
	blob_heap: &'l BlobHeapScope<'l>,
	string_heap: &'l StringHeapScope<'l>,
}

impl<'l> HeapScopeRefs<'l> {
	pub fn new(blob_heap: &'l BlobHeapScope<'l>, string_heap: &'l StringHeapScope<'l>) -> Self {
		Self {
			blob_heap,
			string_heap,
		}
	}

	pub fn blob_heap(&self) -> &BlobHeapScope<'l> {
		self.blob_heap
	}

	pub fn string_heap(&self) -> &StringHeapScope<'l> {
		self.string_heap
	}
}
