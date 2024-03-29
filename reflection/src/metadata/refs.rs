use std::hash::{Hash, Hasher};
use std::io::{Error, Read, Write};
use std::marker::PhantomData;
use crate::{Encoded, MetadataRead, MetadataWrite};
use std::str::Utf8Error;

#[derive(Debug)]
pub struct ElementRef<T: ?Sized> {
	pub(crate) offset: Encoded<u64>,
	pub(crate) ph: PhantomData<T>,
}

impl<T: ?Sized> Copy for ElementRef<T> {}
impl<T: ?Sized> Clone for ElementRef<T> {
	fn clone(&self) -> Self {
		*self
	}
}
impl<T: ?Sized> Eq for ElementRef<T> {}

impl<T: ?Sized> PartialEq<Self> for ElementRef<T> {
	fn eq(&self, other: &Self) -> bool {
		self.offset == other.offset
	}
}

impl<T: ?Sized> Default for ElementRef<T> {
	fn default() -> Self {
		Self {
			offset: Encoded(0),
			ph: PhantomData,
		}
	}
}

impl<T: ?Sized> MetadataRead for ElementRef<T> {
	fn read<S: Read>(stream: &mut S) -> Result<Self, Error> {
		Ok(Self { offset: Encoded::read(stream)?, ph: PhantomData })
	}
}

impl<T: ?Sized> MetadataWrite for ElementRef<T> {
	fn write<S: Write>(&self, stream: &mut S) -> Result<(), Error> {
		Encoded::write(&self.offset, stream)
	}
}

impl<T: ?Sized> Hash for ElementRef<T> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write(bytemuck::bytes_of(&self.offset.0))
	}
}

impl<T: Sized> ElementRef<T> {
	pub fn get<'l>(&self, elements: &'l [T]) -> Option<&'l T> {
		let offset = self.offset.0 as usize;
		elements.get(offset)
	}

	pub fn offset(&self) -> usize {
		self.offset.0 as usize
	}
}

impl ElementRef<str> {
	pub fn get_str<'l>(&self, pool: &'l [u8]) -> Result<&'l str, Utf8Error> {
		let offset = self.offset.0 as usize;
		let len = pool[offset..].iter().take_while(|c| **c != b'\0').count();
		std::str::from_utf8(&pool[offset..offset + len])
	}

	/// # Safety
	/// The byte slice must contain valid UTF-8
	pub unsafe fn get_str_unchecked<'l>(&self, pool: &'l [u8]) -> &'l str {
		let offset = self.offset.0 as usize;
		let len = pool[offset..].iter().take_while(|c| **c != b'\0').count();
		std::str::from_utf8_unchecked(&pool[offset..offset + len])
	}
}

impl From<SliceRef<str>> for ElementRef<str> {
	fn from(value: SliceRef<str>) -> Self {
		Self {
			offset: value.offset,
			ph: PhantomData,
		}
	}
}

#[derive(Debug)]
pub struct SliceRef<T: ?Sized> {
	pub(crate) offset: Encoded<u64>,
	pub(crate) len: Encoded<u64>,
	pub(crate) ph: PhantomData<T>,
}
impl<T: ?Sized> Copy for SliceRef<T> {}
impl<T: ?Sized> Clone for SliceRef<T> {
	fn clone(&self) -> Self {
		*self
	}
}
impl<T: ?Sized> Eq for SliceRef<T> {}

impl<T: ?Sized> PartialEq<Self> for SliceRef<T> {
	fn eq(&self, other: &Self) -> bool {
		self.offset == other.offset && self.len == other.len
	}
}

impl<T: ?Sized> Default for SliceRef<T> {
	fn default() -> Self {
		Self {
			offset: Encoded(0),
			len: Encoded(0),

			ph: PhantomData,
		}
	}
}

impl<T: ?Sized> Hash for SliceRef<T> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write_u64(self.offset.0);
		state.write_u64(self.len.0);
	}
}

impl<T: ?Sized> MetadataRead for SliceRef<T> {
	fn read<S: Read>(stream: &mut S) -> Result<Self, Error> {
		Ok(Self { offset: Encoded::read(stream)?, len: Encoded::read(stream)?, ph: PhantomData })
	}
}

impl<T: ?Sized> MetadataWrite for SliceRef<T> {
	fn write<S: Write>(&self, stream: &mut S) -> Result<(), Error> {
		Encoded::write(&self.offset, stream)?;
		Encoded::write(&self.len, stream)
	}
}

impl<T: ?Sized> SliceRef<T> {
	pub fn null() -> SliceRef<T> {
		SliceRef {
			offset: Encoded(0),
			len: Encoded(0),
			ph: PhantomData,
		}
	}
}

impl<T: Sized> SliceRef<T> {
	pub fn get<'l>(&self, elements: &'l [T]) -> Option<&'l [T]> {
		let len = self.len.0 as usize;
		let offset = self.offset.0 as usize;
		elements.get(offset..offset + len)
	}
}

impl SliceRef<str> {
	pub fn get_str<'l>(&self, pool: &'l [u8]) -> Result<&'l str, Utf8Error> {
		let len = self.len.0 as usize;
		let offset = self.offset.0 as usize;
		std::str::from_utf8(&pool[offset..offset + len])
	}

	/// # Safety
	/// The byte slice must contain valid UTF-8
	pub unsafe fn get_str_unchecked<'l>(&self, pool: &'l [u8]) -> &'l str {
		let len = self.len.0 as usize;
		let offset = self.offset.0 as usize;
		std::str::from_utf8_unchecked(&pool[offset..offset + len])
	}
}

impl SliceRef<[u8]> {
	pub fn get_slice<'l>(&self, pool: &'l [u8]) -> &'l [u8] {
		let len = self.len.0 as usize;
		let offset = self.offset.0 as usize;
		&pool[offset..offset + len]
	}
}
