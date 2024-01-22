use std::io::{Error, Write};
use std::mem::Discriminant;
use crate::MetadataWrite;

#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub enum Opcode {
	#[default]
	Nop = 0x0,
	Ret = 0x1,

	Add = 0xA0,
	Sub = 0xA1,
	Mul = 0xA2,
	Div = 0xA3,
	Mod = 0xA4,
	Shl = 0xA5,
	Shr = 0xA6,
	Or = 0xA7,
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

impl MetadataWrite for Discriminant<Opcode> {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		unsafe {
			let value: u8 = std::mem::transmute(*self);
			value.write(stream)
		}
	}
}
