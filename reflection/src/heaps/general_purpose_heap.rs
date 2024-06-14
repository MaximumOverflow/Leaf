use std::mem::{needs_drop, transmute};
use std::ptr::drop_in_place;
use std::slice::{from_raw_parts_mut, from_ref};

use bumpalo::Bump;
use parking_lot::Mutex;

#[derive(Default)]
pub struct ArenaAllocator {
	bump: Mutex<Bump>,
	drops: Mutex<Vec<(AllocPtr, usize, fn(AllocPtr, usize))>>,
}

#[derive(Copy, Clone)]
struct AllocPtr(*mut u8);
unsafe impl Send for AllocPtr {}

impl ArenaAllocator {
	pub fn alloc<T: Sized + Send>(&self, val: T) -> &mut T {
		unsafe {
			let bump = self.bump.lock();
			let val = bump.alloc(val);
			self.push_drop(from_ref(val));
			transmute(val)
		}
	}

	pub fn alloc_str(&self, str: &str) -> &str {
		unsafe {
			let bump = self.bump.lock();
			let str = bump.alloc_str(str);
			transmute(str)
		}
	}

	pub fn alloc_slice_copy<T: Copy + Sized>(&self, slice: &[T]) -> &[T] {
		unsafe {
			let bump = self.bump.lock();
			let slice = bump.alloc_slice_copy(slice);
			self.push_drop(slice);
			transmute(slice)
		}
	}

	pub fn alloc_slice_clone<T: Clone + Sized>(&self, slice: &[T]) -> &[T] {
		unsafe {
			let bump = self.bump.lock();
			let slice = bump.alloc_slice_clone(slice);
			self.push_drop(slice);
			transmute(slice)
		}
	}

	fn push_drop<T>(&self, slice: &[T]) {
		unsafe {
			if needs_drop::<T>() {
				let drop =
					|p: AllocPtr, l: usize| drop_in_place(from_raw_parts_mut(p.0 as *mut T, l));
				let mut slice_drops = self.drops.lock();
				let ptr = AllocPtr(slice.as_ptr() as *mut u8);
				slice_drops.push((ptr, slice.len(), drop));
			}
		}
	}
}

impl Drop for ArenaAllocator {
	fn drop(&mut self) {
		let drops = self.drops.lock();
		for (ptr, len, drop) in drops.iter() {
			drop(*ptr, *len)
		}
	}
}
