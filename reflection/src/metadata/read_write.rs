use crate::utilities::{read_compressed_integer, write_compressed_integer};
use std::io::{Error, Read, Write, ErrorKind};
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use bytemuck::Pod;

pub trait MetadataRead
where
	Self: Sized,
{
	fn read<T: Read>(stream: &mut T) -> Result<Self, Error>;
}

pub trait MetadataWrite {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error>;
}

#[repr(transparent)]
#[derive(Debug, Default, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Encoded<T: Pod>(pub T);

impl<T: Pod> Deref for Encoded<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl<T: Pod> DerefMut for Encoded<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl<T: Pod> From<T> for Encoded<T> {
	fn from(value: T) -> Self {
		Self(value)
	}
}

impl<T: Into<usize> + Pod> Into<usize> for Encoded<T> {
	fn into(self) -> usize {
		self.0.into()
	}
}

impl<T: Display + Pod> Display for Encoded<T> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

const CONV_ERR: &str = "Could not convert integer to the destination type.";

macro_rules! impl_encoded_unsigned_integers {
    ($($ty: ty),*) => {
		$(
			impl MetadataRead for Encoded<$ty> {
				fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
					let int = read_compressed_integer(stream)?
						.try_into()
						.map_err(|_| Error::new(ErrorKind::InvalidData, CONV_ERR))?;
					Ok(Self(int))
				}
			}

			impl MetadataWrite for Encoded<$ty> {
				fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
					let int = self.0
					.try_into()
					.map_err(|_| Error::new(ErrorKind::InvalidData, CONV_ERR))?;

					write_compressed_integer(stream, int)
				}
			}
		)*
	};
}

macro_rules! impl_encoded_signed_integers {
    ($($ty: ty),*) => {
		$(
			impl MetadataRead for Encoded<$ty> {
				fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
					let int = (read_compressed_integer(stream)? as i64)
						.try_into()
						.map_err(|_| Error::new(ErrorKind::InvalidData, CONV_ERR))?;
					Ok(Self(int))
				}
			}

			impl MetadataWrite for Encoded<$ty> {
				fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
					let int = (self.0 as i64)
					.try_into()
					.map_err(|_| Error::new(ErrorKind::InvalidData, CONV_ERR))?;

					write_compressed_integer(stream, int)
				}
			}
		)*
	};
}

macro_rules! impl_values {
    ($($ty: ty),*) => {
		$(
			impl MetadataRead for $ty {
				fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
					let mut value = Default::default();
					stream.read_exact(bytemuck::bytes_of_mut(&mut value))?;
					Ok(value)
				}
			}

			impl MetadataWrite for $ty {
				fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
					stream.write_all(bytemuck::bytes_of(self))
				}
			}
		)*
	};
}

impl_encoded_signed_integers!(i16, i32, i64, isize);
impl_encoded_unsigned_integers!(u16, u32, u64, usize);

impl_values!(i8, i16, i32, i64, isize, u8, u16, u32, u64, usize, f32, f64);

impl MetadataRead for bool {
	fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
		let mut value = 0u8;
		stream.read_exact(bytemuck::bytes_of_mut(&mut value))?;
		Ok(value != 0)
	}
}

impl MetadataWrite for bool {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		let value = *self as u8;
		stream.write_all(bytemuck::bytes_of(&value))
	}
}

impl<T: ?Sized> MetadataRead for PhantomData<T> {
	fn read<S: Read>(_: &mut S) -> Result<Self, Error> {
		Ok(PhantomData::default())
	}
}

impl<T: ?Sized> MetadataWrite for PhantomData<T> {
	fn write<S: Write>(&self, _: &mut S) -> Result<(), Error> {
		Ok(())
	}
}
