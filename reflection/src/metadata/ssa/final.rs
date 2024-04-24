use std::fmt::{Debug, Formatter};
#[allow(unused_imports)]
use std::io::{Error, ErrorKind, Read, Write};
use std::usize;

use derivative::Derivative;

use leaf_derive::Metadata;

use crate::{Function, Type};

#[repr(u8)]
#[rustfmt::skip]
#[derive(Default, Copy, Clone, Derivative, Metadata)]
#[derivative(Debug)]
#[metadata(lifetimes(val = "l"))]
pub enum OpCode<'l> {
	#[default]
	Nop = 0x00,
	Alloca { ty: &'l Type<'l> } = 0x01,
	Const { ty: &'l Type<'l>, value: &'l [u8] } = 0x02,
	Func {
		#[derivative(Debug(format_with="debug_func_ref"))]
		func: &'l Function<'l>,
	} = 0x03,

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

	Call {
		#[derivative(Debug(format_with="debug_func_ref"))]
		func: &'l Function<'l>,
		params: &'l [ValueIndex],
	} = 0x22,
	CallInd { func: ValueIndex, params: &'l [ValueIndex] } = 0x23,

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

#[cfg(feature = "read")]
impl<'val: 'req, 'req> crate::serialization::MetadataRead<'val, 'req> for &'val [ValueIndex] {
	type Requirements = &'req crate::serialization::ReadRequirements<'val>;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		let req = req.into();
		let len = usize::read(stream, ())?;
		let mut vec = Vec::with_capacity(len);
		for _ in 0..len {
			vec.push(ValueIndex::read(stream, req)?);
		}
		Ok(req.blobs.arena_allocator().alloc_slice_copy(&vec))
	}
}

#[cfg(feature = "write")]
impl<'val: 'req, 'req> crate::serialization::MetadataWrite<'val, 'req> for &'val [ValueIndex] {
	type Requirements = &'req crate::serialization::WriteRequirements<'val>;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		let req = req.into();
		self.len().write(stream, ())?;
		for element in self.iter() {
			element.write(stream, req)?;
		}
		Ok(())
	}
}

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

fn debug_func_ref(func: &Function, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	write!(fmt, "`{}`", func.id())
}

fn debug_opcodes(v: &Vec<OpCode>, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	let mut list = fmt.debug_list();
	for (i, opcode) in v.iter().enumerate() {
		list.entry(&format_args!("{i}: {:?}", opcode));
	}
	list.finish()
}
