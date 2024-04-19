#![allow(unused_imports)]

use std::io::{Error, ErrorKind, Read, Write};
use std::io::{Seek, SeekFrom};
use std::marker::PhantomData;
use std::sync::OnceLock;

#[cfg(feature = "read")]
pub trait MetadataRead<'val, 'req>
	where
		Self: Sized,
{
	type Requirements;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error>;

	fn size<S: Read + Seek>(
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<u64, Error> {
		let start = stream.stream_position()?;
		let result = Self::read(stream, req);
		let end = stream.stream_position()?;
		stream.seek(SeekFrom::Start(start))?;
		let _ = result?;
		Ok(end - start)
	}
}

#[cfg(feature = "write")]
pub trait MetadataWrite<'val, 'req>
	where
		Self: Sized,
{
	type Requirements;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error>;
}

macro_rules! impl_metadata {
    (raw: $($ty: ty),*) => {
		$(
			#[cfg(feature = "read")]
			impl MetadataRead<'_, '_> for $ty {
				type Requirements = ();
				fn read<S: Read>(stream: &mut S, _: impl Into<Self::Requirements>) -> Result<Self, Error> {
					let mut bytes = [0; std::mem::size_of::<Self>()];
					stream.read_exact(&mut bytes)?;
					Ok(Self::from_le_bytes(bytes))
				}
			}

			#[cfg(feature = "write")]
			impl MetadataWrite<'_, '_> for $ty {
				type Requirements = ();
				fn write<S: Write>(&self, stream: &mut S, _: impl Into<Self::Requirements>) -> Result<(), Error> {
					stream.write_all(&self.to_le_bytes())
				}
			}
		)*
	};
    ($($ty: ty),*) => {
		$(
			#[cfg(feature = "read")]
			impl MetadataRead<'_, '_> for $ty {
				type Requirements = ();
				fn read<S: Read>(stream: &mut S, _: impl Into<Self::Requirements>) -> Result<Self, Error> {
					let mut result = <$ty>::default();
					let mut shift = 0;

					loop {
						let mut b = 0u8;
						stream.read_exact(bytemuck::bytes_of_mut(&mut b))?;
						let msb_dropped = b & 0x7F;
						result |= (msb_dropped as $ty) << shift;
						shift += 7;

						if b & 0x80 == 0 || shift > (9 * 7) {
							return match b & 0x80 == 0 {
								true => Ok(result),
								false => Err(Error::from(ErrorKind::InvalidData)),
							};
						}
					}
				}
			}

			#[cfg(feature = "write")]
			impl MetadataWrite<'_, '_> for $ty {
				type Requirements = ();
				fn write<S: Write>(&self, stream: &mut S, _: impl Into<Self::Requirements>) -> Result<(), Error> {
					let mut n = *self;
					while n >= 0x80 {
						stream.write_all(&[0x80 | (n as u8)])?;
						n >>= 7;
					}
					stream.write_all(&[n as u8])
				}
			}
		)*
	};
}

impl_metadata!(raw: i8, u8, f32, f64);
impl_metadata!(isize, usize, i16, u16, i32, u32, i64, u64);

#[cfg(feature = "read")]
impl MetadataRead<'_, '_> for bool {
	type Requirements = ();
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		Ok(<u8 as MetadataRead>::read(stream, req)? != 0)
	}
}

#[cfg(feature = "write")]
impl MetadataWrite<'_, '_> for bool {
	type Requirements = ();
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		<u8 as MetadataWrite>::write(&(*self as u8), stream, req)
	}
}

#[cfg(feature = "read")]
impl<'val, 'req, T: MetadataRead<'val, 'req>> MetadataRead<'val, 'req> for Option<T> {
	type Requirements = T::Requirements;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		match <u8 as MetadataRead>::read(stream, ())? {
			0u8 => Ok(None),
			1u8 => Ok(Some(<T as MetadataRead>::read(stream, req)?)),
			_ => Err(Error::new(
				ErrorKind::InvalidData,
				"Invalid Option<T> enum tag",
			)),
		}
	}
}

#[cfg(feature = "write")]
impl<'val, 'req, T: MetadataWrite<'val, 'req>> MetadataWrite<'val, 'req> for Option<T> {
	type Requirements = T::Requirements;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		match self {
			None => <u8 as MetadataWrite>::write(&0u8, stream, ()),
			Some(value) => {
				<u8 as MetadataWrite>::write(&1u8, stream, ())?;
				<T as MetadataWrite>::write(value, stream, req)
			}
		}
	}
}

#[cfg(feature = "read")]
impl<'val, 'req, T: MetadataRead<'val, 'req>> MetadataRead<'val, 'req> for OnceLock<T> {
	type Requirements = T::Requirements;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		let lock = OnceLock::new();
		let value = <T as MetadataRead>::read(stream, req)?;
		let Ok(_) = lock.set(value) else {
			unreachable!()
		};
		Ok(lock)
	}
}

#[cfg(feature = "write")]
impl<'val, 'req, T: MetadataWrite<'val, 'req>> MetadataWrite<'val, 'req> for OnceLock<T> {
	type Requirements = T::Requirements;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		match self.get() {
			Some(value) => <T as MetadataWrite>::write(value, stream, req),
			None => Err(Error::new(
				ErrorKind::NotFound,
				"OnceLock was not initialized",
			)),
		}
	}
}

#[cfg(feature = "read")]
impl<'val, 'req, T: MetadataRead<'val, 'req>> MetadataRead<'val, 'req> for Vec<T>
	where
		T::Requirements: Clone,
{
	type Requirements = T::Requirements;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		let req = req.into();
		let len = <usize as MetadataRead>::read(stream, ())?;
		let mut vec = Vec::with_capacity(len);
		for _ in 0..len {
			vec.push(<T as MetadataRead>::read(stream, req.clone())?)
		}
		Ok(vec)
	}
}

#[cfg(feature = "write")]
impl<'val, 'req, T: MetadataWrite<'val, 'req>> MetadataWrite<'val, 'req> for Vec<T>
	where
		T::Requirements: Clone,
{
	type Requirements = T::Requirements;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		let req = req.into();
		<usize as MetadataWrite>::write(&self.len(), stream, ())?;
		for element in self.iter() {
			<T as MetadataWrite>::write(element, stream, req.clone())?
		}
		Ok(())
	}
}

#[cfg(feature = "read")]
impl<'val: 'req, 'req> MetadataRead<'val, 'req> for &[u8] {
	type Requirements = &'req ReadDependencies<'val>;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		unimplemented!()
	}
}

#[cfg(feature = "write")]
impl<'val: 'req, 'req> MetadataWrite<'val, 'req> for &[u8] {
	type Requirements = &'req WriteDependencies<'val>;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		unimplemented!()
	}
}

#[cfg(feature = "read")]
impl<'val: 'req, 'req> MetadataRead<'val, 'req> for &str {
	type Requirements = &'req ReadDependencies<'val>;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		unimplemented!()
	}
}

#[cfg(feature = "write")]
impl<'val: 'req, 'req> MetadataWrite<'val, 'req> for &str {
	type Requirements = &'req WriteDependencies<'val>;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		unimplemented!()
	}
}

#[cfg(feature = "read")]
pub struct ReadDependencies<'l> {
	ph: PhantomData<&'l ()>,
}

impl Into<()> for &ReadDependencies<'_> {
	fn into(self) {}
}

#[cfg(feature = "write")]
pub struct WriteDependencies<'l> {
	ph: PhantomData<&'l ()>,
}

impl Into<()> for &WriteDependencies<'_> {
	fn into(self) {}
}
