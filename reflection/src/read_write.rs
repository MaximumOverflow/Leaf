#[cfg(feature = "write")]
pub mod write {
	use std::io::Error;

	pub trait Write<'l> {
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
				impl Write<'_> for $ty {
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

	impl Write<'_> for bool {
		type Requirements = ();
		fn write<T: std::io::Write>(
			&'_ self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error> {
			(*self as u8).write(stream, req)
		}
	}

	impl_write!(raw: i8, u8, f32, f64);
	impl_write!(isize, usize, i16, u16, i32, u32, i64, u64);

	impl<'l, R, T: Write<'l, Requirements = R> + 'l> Write<'l> for &T {
		type Requirements = R;
		fn write<S: std::io::Write>(
			&'l self,
			stream: &mut S,
			req: Self::Requirements,
		) -> Result<(), Error> {
			Write::write(*self, stream, req)
		}
	}

	impl<'l, R, T: Write<'l, Requirements = R> + 'l> Write<'l> for &mut T {
		type Requirements = R;
		fn write<S: std::io::Write>(
			&'l self,
			stream: &mut S,
			req: Self::Requirements,
		) -> Result<(), Error> {
			Write::write(*self, stream, req)
		}
	}

	impl<'l, R: Copy, T: Write<'l, Requirements = R> + 'l> Write<'l> for Option<T> {
		type Requirements = R;
		fn write<S: std::io::Write>(
			&'l self,
			stream: &mut S,
			req: Self::Requirements,
		) -> Result<(), Error> {
			match self {
				None => false.write(stream, ()),
				Some(value) => {
					true.write(stream, ())?;
					value.write(stream, req)
				},
			}
		}
	}

	impl<'l, R: Copy, T: Write<'l, Requirements = R> + 'l> Write<'l> for Vec<T> {
		type Requirements = R;
		fn write<S: std::io::Write>(
			&'l self,
			stream: &mut S,
			req: Self::Requirements,
		) -> Result<(), Error> {
			self.len().write(stream, ())?;
			for i in self {
				Write::write(i, stream, req)?;
			}
			Ok(())
		}
	}
}
