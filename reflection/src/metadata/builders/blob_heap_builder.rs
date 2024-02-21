use crate::{Encoded, MetadataRead, SliceRef};
use core::marker::PhantomData;
use std::collections::HashMap;
use std::io::Cursor;
use std::sync::Arc;

#[derive(Default)]
pub struct BlobHeapBuilder {
	offset: u64,
	map: HashMap<Arc<[u8]>, (u64, Arc<[u8]>)>,
	rev_map: HashMap<u64, Arc<[u8]>>,
}

#[allow(unused)]
impl BlobHeapBuilder {
	pub fn len(&self) -> usize {
		self.offset as usize
	}

	pub fn is_empty(&self) -> bool {
		self.offset == 0
	}

	pub fn alloc(&mut self, bytes: &[u8]) -> (SliceRef<[u8]>, Arc<[u8]>) {
		match self.map.get(bytes) {
			Some((offset, buf)) => {
				let buf_ref = SliceRef {
					offset: Encoded(*offset),
					len: Encoded(bytes.len() as u64),
					ph: PhantomData,
				};
				(buf_ref, buf.clone())
			},
			None => {
				let buf: Arc<[u8]> = Arc::from(bytes);
				let buf_ref = SliceRef {
					offset: Encoded(self.offset),
					len: Encoded(buf.len() as u64),
					ph: PhantomData,
				};

				self.offset += buf_ref.len.0;
				self.map.insert(buf.clone(), (buf_ref.offset.0, buf.clone()));
				self.rev_map.insert(buf_ref.offset.0, buf.clone());
				(buf_ref, buf)
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

		for (buf, (offset, _)) in self.map {
			let offset = offset as usize;
			let slice = &mut buffer[offset..offset + buf.len()];
			slice.copy_from_slice(buf.as_ref());
		}
	}

	pub fn get_blob(&self, blob_ref: SliceRef<[u8]>) -> Option<&[u8]> {
		let blob = self.rev_map.get(&blob_ref.offset)?;
		match blob.len() == blob_ref.len.0 as usize {
			false => None,
			true => Some(blob),
		}
	}

	pub fn get_blob_items<T: MetadataRead>(&self, blob_ref: SliceRef<T>) -> Option<impl Iterator<Item=T> + '_> {
		let blob = self.rev_map.get(&blob_ref.offset.0)?;
		let mut cursor = Cursor::new(&**blob);
		let iter = (0..blob_ref.len.0).map(move |i| {
			T::read(&mut cursor).unwrap()
		});

		Some(iter)
	}
}
