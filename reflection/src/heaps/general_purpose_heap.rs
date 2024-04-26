use std::mem::{needs_drop, transmute};
use std::slice::{from_raw_parts_mut, from_ref};
use std::ptr::drop_in_place;
use std::sync::Mutex;
use bumpalo::Bump;

#[derive(Default)]
pub struct ArenaAllocator {
	bump: Mutex<Bump>,
	drops: Mutex<Vec<(*mut u8, usize, fn(*mut u8, usize))>>,
}

impl ArenaAllocator {
	pub fn alloc<T: Sized + Send>(&self, val: T) -> &T {
		unsafe {
			let bump = self.bump.lock().unwrap();
			let val = bump.alloc(val);
			self.push_drop(from_ref(val));
			transmute(val)
		}
	}

	pub fn alloc_str(&self, str: &str) -> &str {
		unsafe {
			let bump = self.bump.lock().unwrap();
			let str = bump.alloc_str(str);
			transmute(str)
		}
	}

	pub fn alloc_slice_copy<T: Copy + Sized + Send>(&self, slice: &[T]) -> &[T] {
		unsafe {
			let bump = self.bump.lock().unwrap();
			let slice = bump.alloc_slice_copy(slice);
			self.push_drop(slice);
			transmute(slice)
		}
	}

	pub fn alloc_slice_clone<T: Clone + Sized + Send>(&self, slice: &[T]) -> &[T] {
		unsafe {
			let bump = self.bump.lock().unwrap();
			let slice = bump.alloc_slice_clone(slice);
			self.push_drop(slice);
			transmute(slice)
		}
	}

	fn push_drop<T>(&self, slice: &[T]) {
		unsafe {
			if needs_drop::<T>() {
				let drop = |p: *mut u8, l: usize| drop_in_place(from_raw_parts_mut(p as *mut T, l));
				let mut slice_drops = self.drops.lock().unwrap();
				let ptr = slice.as_ptr() as *mut u8;
				slice_drops.push((ptr, slice.len(), drop));
			}
		}
	}
}

impl Drop for ArenaAllocator {
	fn drop(&mut self) {
		let drops = self.drops.lock().unwrap();
		for (ptr, len, drop) in drops.iter() {
			drop(*ptr, *len)
		}
	}
}
