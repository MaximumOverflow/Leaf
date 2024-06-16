use std::time::SystemTime;
use petgraph::dot::Dot;

use leaf_reflection_v2::heaps::{ConstHeap, TypeHeap};
use leaf_reflection_v2::metadata::types::{BuildType, DeriveType, Type};
use leaf_reflection_v2::metadata::UniqueIdentifier;

fn main() {
	let const_heap = ConstHeap::new();
	let type_heap = TypeHeap::new();

	let start = SystemTime::now();
	let slice = type_heap.new_struct(UniqueIdentifier::new(
		const_heap.intern("leaf::mem"),
		const_heap.intern("Slice"),
	));
	let _ = slice.set_fields_from_iter([
		(const_heap.intern("ptr"), Type::void().as_ptr()),
		(const_heap.intern("len"), Type::usize()),
	]);

	let vec = type_heap.new_struct(UniqueIdentifier::new(
		const_heap.intern("leaf::collections"),
		const_heap.intern("Vec"),
	));
	let _ = vec.set_fields_from_iter([
		(const_heap.intern("mem"), slice),
		(const_heap.intern("len"), Type::usize()),
	]);

	let range = type_heap.new_struct(UniqueIdentifier::new(
		const_heap.intern("leaf::iterators"),
		const_heap.intern("Range"),
	));
	let _ = range.set_fields_from_iter([
		(const_heap.intern("start"), Type::usize()),
		(const_heap.intern("end"), Type::usize()),
	]);
	println!("{:?}", start.elapsed().unwrap());

	let graph = type_heap.as_graph();
	println!("{}", Dot::new(&*graph));
}
