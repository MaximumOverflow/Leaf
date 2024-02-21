use std::collections::HashMap;
use core::marker::PhantomData;
use crate::{Encoded, SliceRef};
use std::rc::Rc;

#[derive(Default)]
pub struct StringHeapBuilder {
	offset: u64,
	map: HashMap<Rc<str>, u64>,
	rev_map: HashMap<u64, Rc<str>>,
}

#[allow(unused)]
impl StringHeapBuilder {
	pub fn len(&self) -> usize {
		self.offset as usize
	}

	pub fn is_empty(&self) -> bool {
		self.offset == 0
	}

	pub fn alloc(&mut self, str: &str) -> SliceRef<str> {
		match self.map.get(str) {
			Some(offset) => SliceRef {
				offset: Encoded(*offset),
				len: Encoded(str.len() as u64),
				ph: PhantomData,
			},
			None => {
				let str: Rc<str> = Rc::from(str);
				let str_ref = SliceRef {
					offset: Encoded(self.offset),
					len: Encoded(str.len() as u64),
					ph: PhantomData,
				};

				self.offset += str_ref.len.0 + 1;
				self.map.insert(str.clone(), str_ref.offset.0);
				self.rev_map.insert(str_ref.offset.0, str);
				str_ref
			},
		}
	}

	pub fn get_str(&self, str_ref: SliceRef<str>) -> Option<&str> {
		let str = self.rev_map.get(&str_ref.offset)?;
		match str.len() == str_ref.len.0 as usize {
			false => None,
			true => Some(str),
		}
	}

	pub fn get_str_ref(&self, str: &str) -> Option<SliceRef<str>> {
		let offset = *self.map.get(str)?;
		Some(SliceRef {
			offset: Encoded(offset),
			len: Encoded(str.len() as u64),
			ph: PhantomData,
		})
	}

	pub fn build(self) -> Vec<u8> {
		let mut buffer = vec![];
		self.build_into(&mut buffer);
		buffer
	}

	pub fn build_into(self, buffer: &mut Vec<u8>) {
		let buffer = unsafe {
			let start = buffer.len();
			let offset = self.offset as usize;

			buffer.reserve_exact(offset);
			buffer.set_len(start + offset);
			&mut buffer[start..]
		};

		for (str, offset) in self.map {
			let offset = offset as usize;
			let slice = &mut buffer[offset..offset + str.len()];
			slice.copy_from_slice(str.as_bytes());
			buffer[offset + str.len()] = 0;
		}
	}
}
