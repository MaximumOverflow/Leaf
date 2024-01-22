use crate::utilities::{read_compressed_integer, write_compressed_integer};
use std::io::{Error, Read, Write, ErrorKind};
use std::marker::PhantomData;

pub trait MetadataRead
where
	Self: Sized,
{
	fn read<T: Read>(stream: &mut T) -> Result<Self, Error>;
}

pub trait MetadataWrite {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error>;
}

const CONV_ERR: &str = "Could not convert integer to the destination type.";

macro_rules! impl_integers {
    ($($ty: ty),*) => {
		$(
			impl MetadataRead for $ty {
				fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
					read_compressed_integer(stream)?
						.try_into()
						.map_err(|_| Error::new(ErrorKind::InvalidData, CONV_ERR))
				}
			}

			impl MetadataWrite for $ty {
				fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
					let int = (*self)
					.try_into()
					.map_err(|_| Error::new(ErrorKind::InvalidData, CONV_ERR))?;

					write_compressed_integer(stream, int)
				}
			}
		)*
	};
}

impl_integers!(u8, u16, u32, u64, usize);

impl MetadataRead for f32 {
	fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
		let mut value: f32 = 0.0;
		stream.read_exact(bytemuck::bytes_of_mut(&mut value))?;
		Ok(value)
	}
}

impl MetadataWrite for f32 {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		let bytes = bytemuck::bytes_of(self);
		stream.write_all(bytes)
	}
}

impl MetadataRead for f64 {
	fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
		let mut value: f64 = 0.0;
		stream.read_exact(bytemuck::bytes_of_mut(&mut value))?;
		Ok(value)
	}
}

impl MetadataWrite for f64 {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		let bytes = bytemuck::bytes_of(self);
		stream.write_all(bytes)
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
