use parking_lot::Mutex;
use std::sync::Weak;
use bumpalo::Bump;

struct Inner {
	mem: Bump,
}

pub struct TypeHeap {
	inner: Mutex<Inner>,
	self_ref: Weak<TypeHeap>,
}
