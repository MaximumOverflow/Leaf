use std::cell::RefCell;
use std::mem::{needs_drop, transmute};
use std::ptr::drop_in_place;
use std::slice::{from_raw_parts_mut, from_ref};

use bumpalo::Bump;

#[derive(Default)]
pub struct ArenaAllocator {
	bump: Bump,
	drops: RefCell<Vec<(*mut u8, usize, fn(*mut u8, usize))>>,
}

impl ArenaAllocator {
	pub fn alloc<T: Sized>(&self, val: T) -> &mut T {
		unsafe {
			let val = self.bump.alloc(val);
			self.push_drop(from_ref(val));
			transmute(val)
		}
	}

	pub fn alloc_str(&self, str: &str) -> &str {
		unsafe {
			let str = self.bump.alloc_str(str);
			transmute(str)
		}
	}

	pub fn alloc_slice_copy<T: Copy + Sized>(&self, slice: &[T]) -> &[T] {
		unsafe {
			let slice = self.bump.alloc_slice_copy(slice);
			self.push_drop(slice);
			transmute(slice)
		}
	}

	pub fn alloc_slice_clone<T: Clone + Sized>(&self, slice: &[T]) -> &[T] {
		unsafe {
			let slice = self.bump.alloc_slice_clone(slice);
			self.push_drop(slice);
			transmute(slice)
		}
	}

	fn push_drop<T>(&self, slice: &[T]) {
		unsafe {
			if needs_drop::<T>() {
				let drop = |p: *mut u8, l: usize| drop_in_place(from_raw_parts_mut(p as *mut T, l));
				let mut slice_drops = self.drops.borrow_mut();
				let ptr = slice.as_ptr() as *mut u8;
				slice_drops.push((ptr, slice.len(), drop));
			}
		}
	}
}

impl Drop for ArenaAllocator {
	fn drop(&mut self) {
		let drops = self.drops.borrow_mut();
		for (ptr, len, drop) in drops.iter() {
			drop(*ptr, *len)
		}
	}
}
