use std::collections::HashMap;
use crate::SliceRef;

#[derive(Default)]
pub struct BlobHeapBuilder {
	offset: u32,
	map: HashMap<Box<[u8]>, u32>,
}

#[allow(unused)]
impl BlobHeapBuilder {
	pub fn len(&self) -> usize {
		self.offset as usize
	}

	pub fn is_empty(&self) -> bool {
		self.offset == 0
	}

	pub fn alloc(&mut self, bytes: &[u8]) -> SliceRef<[u8]> {
		match self.map.get(bytes) {
			Some(offset) => SliceRef {
				offset: *offset,
				len: bytes.len() as u32,
				ph: Default::default(),
			},
			None => {
				let buf: Box<[u8]> = Box::from(bytes);
				let buf_ref = SliceRef {
					offset: self.offset,
					len: buf.len() as u32,
					ph: Default::default(),
				};

				self.offset += buf_ref.len;
				self.map.insert(buf, buf_ref.offset);
				buf_ref
			},
		}
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

		for (buf, offset) in self.map {
			let offset = offset as usize;
			let slice = &mut buffer[offset..offset + buf.len()];
			slice.copy_from_slice(buf.as_ref());
		}
	}
}
