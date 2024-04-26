mod blob_heap;
mod type_heap;
mod general_purpose_heap;

use std::sync::Arc;
pub use type_heap::*;
pub use blob_heap::*;
pub use general_purpose_heap::*;

#[derive(Clone)]
pub struct Heaps<'l> {
	bump: &'l ArenaAllocator,
	type_heap: Arc<TypeHeap<'l>>,
	blob_heap: Arc<BlobHeap<'l>>,
}

impl<'l> Heaps<'l> {
	pub fn new(bump: &'l ArenaAllocator) -> Self {
		Self {
			bump,
			type_heap: Arc::new(TypeHeap::new(bump)),
			blob_heap: Arc::new(BlobHeap::new(bump)),
		}
	}

	pub fn type_heap(&self) -> &Arc<TypeHeap<'l>> {
		&self.type_heap
	}

	pub fn blob_heap(&self) -> &Arc<BlobHeap<'l>> {
		&self.blob_heap
	}

	pub fn general_purpose_heap(&self) -> &'l ArenaAllocator {
		self.bump
	}
}

#[derive(Clone)]
pub struct HeapScopes<'l> {
	bump: &'l ArenaAllocator,
	type_heap: Arc<TypeHeap<'l>>,
	blob_heap: Arc<BlobHeapScope<'l>>,
}

impl<'l> HeapScopes<'l> {
	pub fn new(
		bump: &'l ArenaAllocator,
		type_heap: Arc<TypeHeap<'l>>,
		blob_heap: Arc<BlobHeapScope<'l>>,
	) -> Self {
		Self {
			bump,
			type_heap,
			blob_heap,
		}
	}

	pub fn bump(&self) -> &'l ArenaAllocator {
		self.bump
	}

	pub fn blob_heap(&self) -> &Arc<BlobHeapScope<'l>> {
		&self.blob_heap
	}

	pub fn type_heap(&self) -> &Arc<TypeHeap<'l>> {
		&self.type_heap
	}
}
