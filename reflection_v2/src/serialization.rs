use std::io::{
	Read as IoRead, Write as IoWrite, Error as IoError, ErrorKind as IoErrorKind,
	Result as IoResult,
};

pub trait Read: Sized {
	type Context;
	fn read<Stream: IoRead>(stream: &mut Stream, context: &Self::Context) -> IoResult<Self>;
}

pub trait Write: Sized {
	fn write<Stream: IoWrite>(&self, stream: &mut Stream) -> IoResult<()>;
}

impl Read for usize {
	type Context = ();
	fn read<Stream: IoRead>(stream: &mut Stream, _: &Self::Context) -> IoResult<Self> {
		let mut result = 0usize;
		let mut shift = 0;

		loop {
			let mut b = 0u8;
			stream.read_exact(std::slice::from_mut(&mut b))?;
			let msb_dropped = b & 0x7F;
			result |= (msb_dropped as usize) << shift;
			shift += 7;

			if b & 0x80 == 0 || shift > (9 * 7) {
				return match b & 0x80 == 0 {
					true => Ok(result),
					false => Err(IoError::from(IoErrorKind::InvalidData)),
				};
			}
		}
	}
}

impl Write for usize {
	fn write<Stream: IoWrite>(&self, stream: &mut Stream) -> IoResult<()> {
		let mut i = 0;
		let mut bytes = [0; 32];
		let mut n = *self;
		while n >= 0x80 {
			bytes[i] = 0x80 | (n as u8);
			n >>= 7;
			i += 1;
		}
		bytes[i] = n as u8;
		stream.write_all(&bytes[..i + 1])
	}
}

impl Read for isize {
	type Context = ();
	#[inline]
	fn read<Stream: IoRead>(stream: &mut Stream, context: &Self::Context) -> IoResult<Self> {
		Ok(usize::read(stream, context)? as isize)
	}
}

impl Write for isize {
	#[inline]
	fn write<Stream: IoWrite>(&self, stream: &mut Stream) -> IoResult<()> {
		(*self as usize).write(stream)
	}
}

impl Write for &[u8] {
	fn write<Stream: IoWrite>(&self, stream: &mut Stream) -> IoResult<()> {
		self.len().write(stream)?;
		stream.write_all(self)
	}
}
