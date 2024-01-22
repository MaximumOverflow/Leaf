use std::io::{Error, ErrorKind, Read, Write};
use crate::{MetadataRead, MetadataWrite};
use std::mem::Discriminant;

#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub enum Opcode {
	#[default]
	Nop = 0x00,
	Ret = 0x01,

	Add = 0xA0,
	Sub = 0xA1,
	Mul = 0xA2,
	Div = 0xA3,
	Mod = 0xA4,
	Shl = 0xA5,
	Shr = 0xA6,
	Or =  0xA7,
	And = 0xA8,
	Xor = 0xA9,
	Not = 0xAA,
	Neg = 0xAB,

	PushInt(i64) = 0xB0,
	PushUInt(u64) = 0xB1,
	PushDecimal(f64) = 0xB2,

	PushLocal(usize) = 0xBA,
	PushLocalA(usize) = 0xBB,
	StoreLocal(usize) = 0xBC,
}

impl MetadataWrite for Opcode {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		match self {
			| Opcode::Nop
			| Opcode::Ret
			| Opcode::Add
			| Opcode::Sub
			| Opcode::Mul
			| Opcode::Div
			| Opcode::Mod
			| Opcode::Shl
			| Opcode::Shr
			| Opcode::Or
			| Opcode::And
			| Opcode::Xor
			| Opcode::Not
			| Opcode::Neg => std::mem::discriminant(self).write(stream),

			Opcode::PushInt(i) => {
				let i = *i as u64;
				std::mem::discriminant(self).write(stream)?;
				i.write(stream)
			},

			Opcode::PushUInt(i) => {
				std::mem::discriminant(self).write(stream)?;
				i.write(stream)
			},

			Opcode::PushDecimal(f) => {
				std::mem::discriminant(self).write(stream)?;
				f.write(stream)
			},

			| Opcode::PushLocal(l) | Opcode::PushLocalA(l) | Opcode::StoreLocal(l) => {
				std::mem::discriminant(self).write(stream)?;
				l.write(stream)
			},
		}
	}
}

impl MetadataWrite for &[Opcode] {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		for opcode in self.iter() {
			opcode.write(stream)?
		}
		Ok(())
	}
}

impl MetadataRead for Discriminant<Opcode> {
	fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
		unsafe {
			let mut value: u8 = 0;
			stream.read_exact(std::slice::from_mut(&mut value))?;
			Ok(std::mem::transmute(value))
		}
	}
}

impl MetadataWrite for Discriminant<Opcode> {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		unsafe {
			let value: u8 = std::mem::transmute(*self);
			value.write(stream)
		}
	}
}

impl MetadataRead for Option<Opcode> {
	fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
		let discriminant = match u8::read(stream) {
			Ok(disc) => disc,
			Err(err) => return match err.kind() {
				ErrorKind::UnexpectedEof => Ok(None),
				_ => Err(err),
			}
		};

		let opcode = match discriminant {
			0x00 => Opcode::Nop,
			0x01 => Opcode::Ret,

			0xA0 => Opcode::Add,
			0xA1 => Opcode::Sub,
			0xA2 => Opcode::Mul,
			0xA3 => Opcode::Div,
			0xA4 => Opcode::Mod,
			0xA5 => Opcode::Shl,
			0xA6 => Opcode::Shr,
			0xA7 => Opcode::Or ,
			0xA8 => Opcode::And,
			0xA9 => Opcode::Xor,
			0xAA => Opcode::Not,
			0xAB => Opcode::Neg,

			0xB0 => Opcode::PushInt(i64::read(stream)?),
			0xB1 => Opcode::PushUInt(u64::read(stream)?),
			0xB2 => Opcode::PushDecimal(f64::read(stream)?),

			0xBA => Opcode::PushLocal(usize::read(stream)?),
			0xBB => Opcode::PushLocalA(usize::read(stream)?),
			0xBC => Opcode::StoreLocal(usize::read(stream)?),
			_ => return Err(Error::new(ErrorKind::InvalidData, format!("Unimplemented opcode 0x{:X}.", discriminant))),
		};

		Ok(Some(opcode))
	}
}

impl MetadataRead for Opcode {
	fn read<T: Read>(stream: &mut T) -> Result<Self, Error> {
		match Option::<Opcode>::read(stream)? {
			Some(opcode) => Ok(opcode),
			None => Err(Error::new(ErrorKind::UnexpectedEof, "Unexpected end of stream.")),
		}
	}
}
