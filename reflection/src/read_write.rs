#[cfg(feature = "write")]
pub mod write {
	use std::io::Error;

	// TODO add support for lifetimes in requirements
	pub trait Write<'l, 'r> {
		type Requirements;
		fn write<T: std::io::Write>(
			&'l self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error>;
	}

	macro_rules! impl_write {
		(raw: $($ty: ty),*) => {
			$(
				impl Write<'_, '_> for $ty {
					type Requirements = ();
					fn write<T: std::io::Write>(
						&self, stream: &mut T, _: Self::Requirements,
					) -> Result<(), Error> {
						stream.write_all(&self.to_le_bytes())
					}
				}
			)*
		};
		($($ty: ty),*) => {
			$(
				impl Write<'_, '_> for $ty {
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

	impl Write<'_, '_> for bool {
		type Requirements = ();
		fn write<T: std::io::Write>(
			&self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error> {
			(*self as u8).write(stream, req)
		}
	}

	impl_write!(raw: i8, u8, f32, f64);
	impl_write!(isize, usize, i16, u16, i32, u32, i64, u64);
}
