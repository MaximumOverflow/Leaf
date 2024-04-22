use std::fmt::Formatter;

use derivative::Derivative;

use leaf_derive::Metadata;

use crate::Type;

#[repr(u8)]
#[rustfmt::skip]
#[derive(Debug, Default, Copy, Clone, Metadata)]
#[metadata(lifetimes(val = "l"))]
pub enum OpCode<'l> {
	#[default]
	Nop = 0x00,
	Alloca { ty: &'l Type<'l> } = 0x01,
	Const { ty: &'l Type<'l>, value: &'l [u8] } = 0x02,

	Add { lhs: ValueIndex, rhs: ValueIndex } = 0x10,
	Sub { lhs: ValueIndex, rhs: ValueIndex } = 0x11,
	Mul { lhs: ValueIndex, rhs: ValueIndex } = 0x12,
	Div { lhs: ValueIndex, rhs: ValueIndex } = 0x13,
	Rem { lhs: ValueIndex, rhs: ValueIndex } = 0x14,
	Cmp { lhs: ValueIndex, rhs: ValueIndex, cmp: Comparison } = 0x15,
	SExt { ty: &'l Type<'l>, value: ValueIndex } = 0x1E,
	ZExt { ty: &'l Type<'l>, value: ValueIndex } = 0x1F,

	Load { value: ValueIndex } = 0x20,
	Store { val: ValueIndex, dst: ValueIndex } = 0x21,

	Ret { value: Option<ValueIndex> } = 0x30,
	Jp { target: ValueIndex } = 0x31,
	Br {
		condition: ValueIndex,
		true_case: ValueIndex,
		false_case: ValueIndex,
	} = 0x32,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Metadata)]
pub struct ValueIndex(pub usize);

#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, Metadata)]
pub enum Comparison {
	#[default]
	Eq = 0x0,
	Ne = 0x1,
	Lt = 0x2,
	Gt = 0x3,
	Le = 0x4,
	Ge = 0x5,
}

#[derive(Derivative, Metadata)]
#[metadata(lifetimes(val = "l"))]
#[derivative(Debug)]
pub struct SSAData<'l> {
	#[derivative(Debug(format_with = "debug_opcodes"))]
	pub(super) opcodes: Vec<OpCode<'l>>,
}

fn debug_opcodes(v: &Vec<OpCode>, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	let mut list = fmt.debug_list();
	for (i, opcode) in v.iter().enumerate() {
		list.entry(&format_args!("{i}: {:?}", opcode));
	}
	list.finish()
}
