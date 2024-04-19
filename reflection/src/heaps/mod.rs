mod blob_heap;
mod type_heap;

use std::sync::Arc;
pub use type_heap::*;
pub use blob_heap::*;
pub use bumpalo::Bump;

pub struct Heaps<'l> {
	bump: &'l Bump,
	type_heap: TypeHeap<'l>,
	blob_heap: BlobHeap<'l>,
}

impl<'l> Heaps<'l> {
	pub fn new(bump: &'l Bump) -> Self {
		Self {
			bump,
			type_heap: TypeHeap::new(bump),
			blob_heap: BlobHeap::new(bump),
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
}

#[derive(Clone)]
pub struct HeapScopes<'l> {
	bump: &'l Bump,
	blob_heap: Arc<BlobHeapScope<'l>>,
}

impl<'l> HeapScopes<'l> {
	pub fn new(bump: &'l Bump, blob_heap: Arc<BlobHeapScope<'l>>) -> Self {
		Self { bump, blob_heap }
	}

	pub fn bump(&self) -> &'l Bump {
		self.bump
	}

	pub fn blob_heap(&self) -> &BlobHeapScope<'l> {
		&self.blob_heap
	}
}
