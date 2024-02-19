use leaf_derive::{MetadataRead, MetadataWrite};
use std::io::{Error, Write};
use crate::MetadataWrite;

#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, PartialEq, MetadataRead, MetadataWrite)]
pub enum Opcode {
	#[default]
	Nop = 0x00,
	Ret = 0x01,
	Swap = 0x02,

	Add = 0x10,
	Sub = 0x11,
	Mul = 0x12,
	Div = 0x13,
	Mod = 0x14,
	Shl = 0x15,
	Shr = 0x16,
	Or =  0x17,
	And = 0x18,
	Xor = 0x19,
	Not = 0x1A,
	Neg = 0x1B,

	PushInt8(i8) = 0xA0,
	PushInt16(i16) = 0xA1,
	PushInt32(i32) = 0xA2,
	PushInt64(i64) = 0xA3,

	PushUInt8(u8) = 0xA4,
	PushUInt16(u16) = 0xA5,
	PushUInt32(u32) = 0xA6,
	PushUInt64(u64) = 0xA7,

	PushDecimal16(f32) = 0xA8,
	PushDecimal32(f32) = 0xA9,
	PushDecimal64(f64) = 0xAA,

	SignExtend(u32) = 0xAB,
	ConvDecimal(u32) = 0xAC,

	PushLocal0 = 0xB0,
	PushLocal1 = 0xB1,
	PushLocal2 = 0xB2,
	PushLocal3 = 0xB3,
	PushLocal4 = 0xB4,
	PushLocal5 = 0xB5,
	PushLocal6 = 0xB6,
	PushLocal(usize) = 0xB7,

	PushLocalA0 = 0xB8,
	PushLocalA1 = 0xB9,
	PushLocalA2 = 0xBA,
	PushLocalA3 = 0xBB,
	PushLocalA4 = 0xBC,
	PushLocalA5 = 0xBD,
	PushLocalA6 = 0xBE,
	PushLocalA(usize) = 0xBF,

	PushParam0 = 0xC0,
	PushParam1 = 0xC1,
	PushParam2 = 0xC2,
	PushParam3 = 0xC3,
	PushParam4 = 0xC4,
	PushParam5 = 0xC5,
	PushParam6 = 0xC6,
	PushParam(usize) = 0xC7,

	PushParamA0 = 0xC8,
	PushParamA1 = 0xC9,
	PushParamA2 = 0xCA,
	PushParamA3 = 0xCB,
	PushParamA4 = 0xCC,
	PushParamA5 = 0xCD,
	PushParamA6 = 0xCE,
	PushParamA(usize) = 0xCF,

	StoreLocal0 = 0xD0,
	StoreLocal1 = 0xD1,
	StoreLocal2 = 0xD2,
	StoreLocal3 = 0xD3,
	StoreLocal4 = 0xD4,
	StoreLocal5 = 0xD5,
	StoreLocal6 = 0xD6,
	StoreLocal(usize) = 0xD7,

	StoreParam0 = 0xD8,
	StoreParam1 = 0xD9,
	StoreParam2 = 0xDA,
	StoreParam3 = 0xDB,
	StoreParam4 = 0xDC,
	StoreParam5 = 0xDD,
	StoreParam6 = 0xDE,
	StoreParam(usize) = 0xDF,

	PushField0 = 0xE0,
	PushField1 = 0xE1,
	PushField2 = 0xE2,
	PushField3 = 0xE3,
	PushField4 = 0xE4,
	PushField5 = 0xE5,
	PushField6 = 0xE6,
	PushField(usize) = 0xE7,

	PushFieldA0 = 0xE8,
	PushFieldA1 = 0xE9,
	PushFieldA2 = 0xEA,
	PushFieldA3 = 0xEB,
	PushFieldA4 = 0xEC,
	PushFieldA5 = 0xED,
	PushFieldA6 = 0xEE,
	PushFieldA(usize) = 0xEF,

	StoreField0 = 0xF0,
	StoreField1 = 0xF1,
	StoreField2 = 0xF2,
	StoreField3 = 0xF3,
	StoreField4 = 0xF4,
	StoreField5 = 0xF5,
	StoreField6 = 0xF6,
	StoreField(usize) = 0xF7,
}

impl MetadataWrite for &[Opcode] {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		for opcode in self.iter() {
			opcode.write(stream)?
		}
		Ok(())
	}
}
