#[cfg(feature = "write")]
pub mod write {
	use std::io::Error;

	use crate::heaps::{BlobHeapScope, StringHeapScope};

	pub type Heaps<'l> = (&'l BlobHeapScope<'l>, &'l StringHeapScope<'l>);

	pub trait Write<'l> {
		type Requirements;
		fn write<T: std::io::Write>(
			&'l self, stream: &mut T, req: Self::Requirements,
		) -> Result<(), Error>;
	}

	macro_rules! impl_write {
		(as_byte: $($ty: ty),*) => {
			$(
				impl Write<'_> for $ty {
					type Requirements = ();
					fn write<T: std::io::Write>(
						&self, stream: &mut T, _: Self::Requirements,
					) -> Result<(), Error> {
						stream.write_all(&[*self as u8])
					}
				}
			)*
		};
		($($ty: ty),*) => {
			$(
				impl Write<'_> for $ty {
					type Requirements = ();
					fn write<T: std::io::Write>(
						&self, stream: &mut T, _: Self::Requirements,
					) -> Result<(), Error> {
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

	impl_write!(as_byte: bool, i8, u8);
	impl_write!(isize, usize, i16, u16, i32, u32, i64, u64);
}
