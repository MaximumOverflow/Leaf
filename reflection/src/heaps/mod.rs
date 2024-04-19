mod blob_heap;
mod type_heap;
mod general_purpose_heap;

use std::sync::Arc;
pub use type_heap::*;
pub use blob_heap::*;
pub use general_purpose_heap::*;

pub struct Heaps<'l> {
	type_heap: TypeHeap<'l>,
	blob_heap: BlobHeap<'l>,
	bump: &'l ArenaAllocator,
}

impl<'l> Heaps<'l> {
	pub fn new(bump: &'l ArenaAllocator) -> Self {
		Self {
			bump,
			type_heap: TypeHeap::new(bump),
			blob_heap: BlobHeap::new(bump),
		}
	}

	pub fn type_heap(&self) -> &TypeHeap<'l> {
		&self.type_heap
	}

	pub fn blob_heap(&self) -> &BlobHeap<'l> {
		&self.blob_heap
	}

	pub fn general_purpose_heap(&self) -> &'l ArenaAllocator {
		self.bump
	}
}

#[derive(Clone)]
pub struct HeapScopes<'l> {
	bump: &'l ArenaAllocator,
	blob_heap: Arc<BlobHeapScope<'l>>,
}

impl<'l> HeapScopes<'l> {
	pub fn new(bump: &'l ArenaAllocator, blob_heap: Arc<BlobHeapScope<'l>>) -> Self {
		Self { bump, blob_heap }
	}

	pub fn bump(&self) -> &'l ArenaAllocator {
		self.bump
	}

	pub fn blob_heap(&self) -> &BlobHeapScope<'l> {
		&self.blob_heap
	}
}
