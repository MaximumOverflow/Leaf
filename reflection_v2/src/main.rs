use std::time::SystemTime;
use leaf_reflection_v2::heaps::ConstHeap;
use leaf_reflection_v2::serialization::Write;

fn main() {
	let time = SystemTime::now();
	let heap = ConstHeap::new();
	heap.intern(42i32);
	heap.intern(42u32);
	heap.intern([0x2Au8, 0, 0, 0].as_slice());

	heap.intern(42.0f32);
	heap.intern(42.0f64);

	let mut buffer = vec![];
	heap.write(&mut buffer).unwrap();
	println!("{} {:X?}", buffer.len(), buffer);
	println!("{:?}", time.elapsed().unwrap());

	println!("{}", heap.as_dot_graph())
}
