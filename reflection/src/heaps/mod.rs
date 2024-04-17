mod string_heap;
mod blob_heap;
mod type_heap;

use std::sync::Arc;
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

#[derive(Clone)]
pub struct HeapScopes<'l> {
	bump: &'l Bump,
	blob_heap: Arc<BlobHeapScope<'l>>,
	string_heap: Arc<StringHeapScope<'l>>,
}

impl<'l> HeapScopes<'l> {
	pub fn new(
		bump: &'l Bump,
		blob_heap: Arc<BlobHeapScope<'l>>,
		string_heap: Arc<StringHeapScope<'l>>,
	) -> Self {
		Self {
			bump,
			blob_heap,
			string_heap,
		}
	}

	pub fn bump(&self) -> &'l Bump {
		self.bump
	}

	pub fn blob_heap(&self) -> &BlobHeapScope<'l> {
		&self.blob_heap
	}

	pub fn string_heap(&self) -> &StringHeapScope<'l> {
		&self.string_heap
	}
}
