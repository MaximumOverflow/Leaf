use std::io::{ErrorKind, Read, Write};

pub fn compress_integer(num: u64) -> Option<([u8; 8], usize)> {
	if num <= 0x1F {
		Some(([num as u8, 0, 0, 0, 0, 0, 0, 0], 1))
	} else if num <= 0x1FFF {
		let [b0, b1] = u16::to_be_bytes(num as u16 | 0x2000);
		Some(([b0, b1, 0, 0, 0, 0, 0, 0], 2))
	} else if num <= 0x1FFF_FFFF {
		let [b0, b1, b2, b3] = u32::to_be_bytes(num as u32 | 0x6000_0000);
		Some(([b0, b1, b2, b3, 0, 0, 0, 0], 4))
	} else if num <= 0x1FFF_FFFF_FFFF_FFFF {
		Some((u64::to_be_bytes(num | 0xE000_0000_0000_0000), 8))
	} else {
		None
	}
}

pub fn uncompress_integer(bytes: &[u8]) -> Option<u64> {
	let first = *bytes.first()?;
	match first & 0xE0 {
		0x00 => Some(first as u64),
		0x20 => {
			let bytes: [u8; 2] = bytes.get(0..2)?.try_into().unwrap();
			let value = u16::from_be_bytes(bytes) & 0x1FFF;
			Some(value as u64)
		},
		0x60 => {
			let bytes: [u8; 4] = bytes.get(0..4)?.try_into().unwrap();
			let value = u32::from_be_bytes(bytes) & 0x1FFF_FFFF;
			Some(value as u64)
		},
		0xE0 => {
			let bytes: [u8; 8] = bytes.get(0..8)?.try_into().unwrap();
			let value = u64::from_be_bytes(bytes) & 0x1FFF_FFFF_FFFF_FFFF;
			Some(value as u64)
		},
		_ => None,
	}
}

pub fn get_compressed_integer_length(first_byte: u8) -> Option<usize> {
	match first_byte & 0xE0 {
		0x00 => Some(1),
		0x20 => Some(2),
		0x60 => Some(3),
		0xE0 => Some(4),
		_ => None,
	}
}

pub fn read_compressed_integer<T: Read>(stream: &mut T) -> Result<u64, std::io::Error> {
	const ERR: &str = "Invalid integer compression flags.";
	let mut bytes = [0; 8];
	stream.read_exact(&mut bytes[..1])?;
	let len = match get_compressed_integer_length(bytes[0]) {
		Some(len) => len,
		None => return Err(std::io::Error::new(ErrorKind::InvalidData, ERR)),
	};
	stream.read_exact(&mut bytes[1..len])?;
	match uncompress_integer(&bytes) {
		Some(int) => Ok(int),
		None => Err(std::io::Error::new(ErrorKind::InvalidData, ERR)),
	}
}

pub fn write_compressed_integer<T: Write>(stream: &mut T, num: u64) -> Result<(), std::io::Error> {
	const ERR: &str = "Integer is too big to be compressed.";
	match compress_integer(num) {
		None => Err(std::io::Error::new(ErrorKind::InvalidData, ERR)),
		Some((bytes, len)) => stream.write_all(&bytes[..len]),
	}
}


