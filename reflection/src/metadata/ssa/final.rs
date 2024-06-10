#[allow(unused_imports)]
use std::io::{Error, ErrorKind, Read, Write};
use std::fmt::{Debug, Formatter};
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
	SizeOf { ty: &'l Type<'l> } = 0x2,

	Add { lhs: ValueIndex, rhs: ValueIndex } = 0x10,
	Sub { lhs: ValueIndex, rhs: ValueIndex } = 0x11,
	Mul { lhs: ValueIndex, rhs: ValueIndex } = 0x12,
	Div { lhs: ValueIndex, rhs: ValueIndex } = 0x13,
	Rem { lhs: ValueIndex, rhs: ValueIndex } = 0x14,
	Cmp { lhs: ValueIndex, rhs: ValueIndex, cmp: Comparison } = 0x15,
	Not { val: ValueIndex } = 0x16,
	SExt { ty: &'l Type<'l>, value: ValueIndex } = 0x1D,
	ZExt { ty: &'l Type<'l>, value: ValueIndex } = 0x1E,
	Trunc { ty: &'l Type<'l>, value: ValueIndex } = 0x1F,

	Load { value: ValueIndex } = 0x20,
	Store { val: ValueIndex, dst: ValueIndex } = 0x21,
	GEP { val: ValueIndex, idx: ValueIndex } = 0x22,

	Call {
		func: ValueIndex,
		params: &'l [ValueIndex],
	} = 0x2A,
	CallInd { func: ValueIndex, params: &'l [ValueIndex] } = 0x2B,

	Ret { value: Option<ValueIndex> } = 0x30,
	Jp { target: ValueIndex } = 0x31,
	Br {
		condition: ValueIndex,
		true_case: ValueIndex,
		false_case: ValueIndex,
	} = 0x32,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Metadata)]
pub struct ValueIndex(u64);

#[repr(usize)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ValueIndexKind {
	Local = 0b00,
	Parameter = 0b01,
	Constant = 0b10,
	Function = 0b11,
}

impl ValueIndex {
	#[inline(always)]
	pub fn local(idx: usize) -> Self {
		Self(ValueIndexKind::Local.as_tag() | ((idx as u64) << 2))
	}
	#[inline(always)]
	pub fn constant(idx: usize) -> Self {
		Self(ValueIndexKind::Constant.as_tag() | ((idx as u64) << 2))
	}
	#[inline(always)]
	pub fn function(idx: usize) -> Self {
		Self(ValueIndexKind::Function.as_tag() | ((idx as u64) << 2))
	}
	#[inline(always)]
	pub fn parameter(idx: usize) -> Self {
		Self(ValueIndexKind::Parameter.as_tag() | ((idx as u64) << 2))
	}
	#[inline(always)]
	pub fn kind(&self) -> ValueIndexKind {
		unsafe { std::mem::transmute((self.0 & 0x03) as usize) }
	}
	#[inline(always)]
	pub fn index(&self) -> usize {
		self.0.overflowing_shr(2).0 as usize
	}
}

impl ValueIndexKind {
	#[inline(always)]
	fn as_tag(self) -> u64 {
		self as u64
	}
}

impl Debug for ValueIndex {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "ValueIndex::{:?}({})", self.kind(), self.index())
	}
}

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

#[derive(Copy, Clone, Derivative, Metadata)]
#[metadata(lifetimes(val = "l"))]
#[derivative(Debug)]
pub struct ConstantRef<'l> {
	#[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
	pub(super) ty: &'l Type<'l>,
	#[derivative(Debug(format_with = "debug_slice_ref"))]
	pub(super) data: &'l [u8],
}

impl<'l> ConstantRef<'l> {
	#[inline(always)]
	pub fn ty(&self) -> &'l Type<'l> {
		self.ty
	}
	#[inline(always)]
	pub fn data(&self) -> &'l [u8] {
		self.data
	}
}

#[derive(Derivative, Metadata)]
#[metadata(lifetimes(val = "l"))]
#[derivative(Debug)]
pub struct SSAData<'l> {
	#[derivative(Debug(format_with = "debug_opcodes"))]
	pub(super) opcodes: Vec<OpCode<'l>>,
	#[derivative(Debug(format_with = "debug_functions"))]
	pub(super) referenced_functions: Vec<&'l Function<'l>>,
	#[derivative(Debug(format_with = "debug_constants"))]
	pub(super) referenced_constants: Vec<ConstantRef<'l>>,
}

impl<'l> SSAData<'l> {
	#[inline(always)]
	pub fn opcodes(&self) -> &[OpCode<'l>] {
		&self.opcodes
	}
	#[inline(always)]
	pub fn referenced_constants(&self) -> &[ConstantRef<'l>] {
		&self.referenced_constants
	}
	#[inline(always)]
	pub fn referenced_functions(&self) -> &[&'l Function<'l>] {
		&self.referenced_functions
	}
}

#[inline]
fn debug_slice_ref(data: &[u8], fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	write!(fmt, "[u8; {}] @ {:#?}", data.len(), data.as_ptr())
}

fn debug_opcodes(v: &[OpCode], fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	let mut list = fmt.debug_list();
	for (i, opcode) in v.iter().enumerate() {
		list.entry(&format_args!("{i}: {:?}", opcode));
	}
	list.finish()
}

fn debug_constants(v: &[ConstantRef], fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	let mut list = fmt.debug_list();
	for (i, ConstantRef { ty, data }) in v.iter().enumerate() {
		list.entry(&format_args!(
			"{i}: {ty} | [u8; {}] @ {:#?}",
			data.len(),
			data.as_ptr(),
		));
	}
	list.finish()
}

fn debug_functions(v: &[&Function], fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	let mut list = fmt.debug_list();
	for (i, function) in v.iter().enumerate() {
		list.entry(&format_args!("{i}: {}", function.id()));
	}
	list.finish()
}
