use std::collections::HashMap;
use crate::SliceRef;
use std::rc::Rc;

#[derive(Default)]
pub struct StringHeapBuilder {
	offset: u32,
	map: HashMap<Rc<str>, u32>,
	rev_map: HashMap<u32, Rc<str>>,
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
				offset: *offset,
				len: str.len() as u32,
				ph: Default::default(),
			},
			None => {
				let str: Rc<str> = Rc::from(str);
				let str_ref = SliceRef {
					offset: self.offset,
					len: str.len() as u32,
					ph: Default::default(),
				};

				self.offset += str_ref.len + 1;
				self.map.insert(str.clone(), str_ref.offset);
				self.rev_map.insert(str_ref.offset, str);
				str_ref
			},
		}
	}

	pub fn get_str(&self, str_ref: SliceRef<str>) -> Option<&str> {
		let str = self.rev_map.get(&str_ref.offset)?;
		match str.len() == str_ref.len as usize {
			false => None,
			true => Some(str),
		}
	}

	pub fn get_str_ref(&self, str: &str) -> Option<SliceRef<str>> {
		let offset = *self.map.get(str)?;
		Some(SliceRef {
			offset,
			len: str.len() as _,
			ph: Default::default(),
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
