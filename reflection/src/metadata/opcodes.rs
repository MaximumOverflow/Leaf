use leaf_derive::{MetadataRead, MetadataWrite};
use crate::{MetadataRead, MetadataWrite};
use std::io::{Error, Write};

#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, PartialEq, MetadataRead, MetadataWrite)]
pub enum Opcode {
	#[default]
	Nop = 0x00,
	Ret = 0x01,
	Swap = 0x02,

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

	PushInt8(i8) = 0xB0,
	PushInt16(i16) = 0xB1,
	PushInt32(i32) = 0xB2,
	PushInt64(i64) = 0xB3,

	PushUInt8(u8) = 0xB4,
	PushUInt16(u16) = 0xB5,
	PushUInt32(u32) = 0xB6,
	PushUInt64(u64) = 0xB7,

	PushDecimal16(f32) = 0xB8,
	PushDecimal32(f32) = 0xB9,
	PushDecimal64(f64) = 0xBA,

	PushLocal(usize) = 0xC0,
	PushLocalA(usize) = 0xC1,
	StoreLocal(usize) = 0xC2,

	PushParam(usize) = 0xC3,
	PushParamA(usize) = 0xC4,
	StoreParam(usize) = 0xC5,

	PushField(usize) = 0xC6,
	PushFieldA(usize) = 0xC7,
	StoreField(usize) = 0xC8,

	SignExtend(u32) = 0xCA,
	ConvDecimal(u32) = 0xCB,
}

impl MetadataWrite for &[Opcode] {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		for opcode in self.iter() {
			opcode.write(stream)?
		}
		Ok(())
	}
}
