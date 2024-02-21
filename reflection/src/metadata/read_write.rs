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

macro_rules! impl_encoded {
    ($base: ty; $($ty: ty),*) => {
		$(
			impl MetadataRead for Encoded<$ty> {
				fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
					let value = Encoded::<$base>::read(stream)?.0;
					let value = value.try_into().map_err(|_| Error::new(ErrorKind::InvalidData, CONV_ERR))?;
					Ok(Encoded(value))
				}
			}

			impl MetadataWrite for Encoded<$ty> {
				fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
					let value = Encoded(self.0 as $base);
					value.write(stream)
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

impl_encoded!(u64; u16, u32, usize);
impl_encoded!(i64; i16, i32, isize);
impl_values!(i8, i16, i32, i64, isize, u8, u16, u32, u64, usize, f32, f64);

impl MetadataRead for Encoded<u64> {
	fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
		let mut result: u64 = 0;
		let mut shift = 0;

		let mut success = false;
		loop {
			let mut b = 0u8;
			stream.read_exact(bytemuck::bytes_of_mut(&mut b))?;
			let msb_dropped = b & 0x7F;
			result |= (msb_dropped as u64) << shift;
			shift += 7;

			if b & 0x80 == 0 || shift > (9 * 7) {
				success = b & 0x80 == 0;
				break;
			}
		}

		match success {
			true => Ok(Self(result)),
			false => Err(Error::new(ErrorKind::InvalidData, CONV_ERR))
		}
	}
}

impl MetadataWrite for Encoded<u64> {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		let mut n = self.0;
		while n >= 0x80 {
			stream.write_all(&[0x80 | (n as u8)])?;
			n >>= 7;
		}
		stream.write_all(&[n as u8])
	}
}

impl MetadataRead for Encoded<i64> {
	fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
		let value = Encoded::<u64>::read(stream)?;
		Ok(Encoded(bytemuck::pod_read_unaligned(bytemuck::bytes_of(&value.0))))
	}
}

impl MetadataWrite for Encoded<i64> {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		let value: u64 = bytemuck::pod_read_unaligned(bytemuck::bytes_of(&self.0));
		Encoded(value).write(stream)
	}
}

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
