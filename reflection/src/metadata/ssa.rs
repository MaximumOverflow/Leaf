use crate::{Function, Pointer, Type};
use leaf_derive::Write;

#[repr(u8)]
#[derive(Debug, Default, Clone)]
pub enum Opcode<'l> {
	#[default]
	Nop = 0x00,
	Jp(usize) = 0x01,
	Br(ValueIdx, usize, usize) = 0x02,
	Ret(Option<ValueIdx>) = 0x03,
	Call(&'l Function<'l>, Vec<ValueIdx>,ValueIdx) = 0x04,
	Store(ValueIdx, ValueIdx) = 0x10,
	SCmp(ValueIdx, ValueIdx, ValueIdx, Comparison) = 0x20,
	SAdd(ValueIdx, ValueIdx, ValueIdx) = 0x21,
}

#[repr(u8)]
#[derive(Debug, Default, Clone)]
#[cfg_attr(feature = "write", derive(Write))]
pub enum Comparison {
	#[default]
	Eq = 0x0,
	Ne = 0x1,
	Lt = 0x2,
	Gt = 0x3,
	Le = 0x4,
	Ge = 0x5,
}

#[cfg_attr(feature = "write", derive(Write))]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ValueIdx {
	Local(usize),
	Const(usize),
}

#[repr(u8)]
#[derive(Debug, PartialEq)]
pub enum Const<'l> {
	U8(u8),
	U16(u16),
	U32(u32),
	U64(u64),
	I8(i8),
	I16(i16),
	I32(i32),
	I64(i64),
	F32(f32),
	F64(f64),
	Str(&'l str),
	Any(SSAContext<'l>),
}

#[derive(Debug, Default)]
pub struct SSAContext<'l> {
	opcodes: Vec<Opcode<'l>>,
	locals: Vec<&'l Type<'l>>,
	constants: Vec<Const<'l>>,
}

impl<'l> SSAContext<'l> {
	pub fn opcodes(&self) -> &[Opcode<'l>] {
		&self.opcodes
	}

	pub fn locals(&self) -> &[&'l Type<'l>] {
		&self.locals
	}

	pub fn constants(&self) -> &[Const<'l>] {
		&self.constants
	}

	pub fn value_type(&self, value: ValueIdx) -> Option<&'l Type<'l>> {
		value_type(&self.locals, &self.constants, value)
	}
}

impl PartialEq for SSAContext<'_> {
	fn eq(&self, _: &Self) -> bool {
		false
	}
}

#[cfg(feature = "build")]
mod build {
	use leaf_derive::Write;
	use crate::{Opcode, Type, value_type};
	use crate::metadata::ssa::{Const, SSAContext, ValueIdx};

	macro_rules! assert_valid {
		($cond: expr, $err: literal) => {
			if !$cond {
				return Err($err);
			}
		};
	}

	#[repr(transparent)]
	#[cfg_attr(feature = "write", derive(Write))]
	#[derive(Debug, Copy, Clone, Eq, PartialEq)]
	pub struct BlockIndex(usize);

	struct Block<'l> {
		opcodes: Vec<Opcode<'l>>,
	}

	pub struct SSAContextBuilder<'l> {
		locals: Vec<&'l Type<'l>>,
		constants: Vec<Const<'l>>,
		insert_block: BlockIndex,
		blocks: Vec<Block<'l>>,
	}

	impl<'l> SSAContextBuilder<'l> {
		pub fn new() -> Self {
			Self {
				locals: vec![],
				constants: vec![],
				insert_block: BlockIndex(0),
				blocks: vec![Block { opcodes: vec![] }],
			}
		}

		pub fn current_block(&self) -> BlockIndex {
			self.insert_block
		}

		pub fn set_current_block(&mut self, block: BlockIndex) -> Result<BlockIndex, BlockIndex> {
			match block.0 >= self.blocks.len() {
				true => Err(block),
				false => Ok(std::mem::replace(&mut self.insert_block, block)),
			}
		}

		pub fn create_block(&mut self) -> BlockIndex {
			let idx = BlockIndex(self.blocks.len());
			self.blocks.push(Block { opcodes: vec![] });
			idx
		}

		pub fn push_opcode(&mut self, opcode: Opcode<'l>) {
			self.blocks[self.insert_block.0].opcodes.push(opcode);
		}

		pub fn push_jp(&mut self, block: BlockIndex) -> Result<(), &'static str> {
			assert_valid!(block.0 < self.blocks.len(), "Invalid block");
			Ok(self.push_opcode(Opcode::Jp(block.0)))
		}

		pub fn push_br(
			&mut self, value: ValueIdx, true_case: BlockIndex, false_case: BlockIndex,
		) -> Result<(), &'static str> {
			assert_valid!(
				self.value_type(value) == Some(&Type::Bool),
				"Value is not of type `bool`"
			);
			assert_valid!(true_case.0 < self.blocks.len(), "Invalid block `true_case`");
			assert_valid!(
				false_case.0 < self.blocks.len(),
				"Invalid block `false_case`"
			);
			Ok(self.push_opcode(Opcode::Br(value, true_case.0, false_case.0)))
		}

		pub fn push_local(&mut self, ty: &'l Type<'l>) -> ValueIdx {
			let idx = ValueIdx::Local(self.locals.len());
			self.locals.push(ty);
			idx
		}

		pub fn push_constant(&mut self, val: Const<'l>) -> ValueIdx {
			let idx = ValueIdx::Const(self.constants.len());
			self.constants.push(val);
			idx
		}

		pub fn value_type(&self, value: ValueIdx) -> Option<&'l Type<'l>> {
			value_type(&self.locals, &self.constants, value)
		}

		pub fn build(self) -> SSAContext<'l> {
			let mut offset = 0;
			let mut block_offsets = vec![0; self.blocks.len()];
			for (i, block) in self.blocks.iter().enumerate() {
				block_offsets[i] = offset;
				offset += block.opcodes.len();
			}

			let mut opcodes = vec![];
			for block in self.blocks {
				for opcode in block.opcodes {
					match &opcode {
						Opcode::Jp(block) => {
							opcodes.push(Opcode::Jp(block_offsets[*block]));
						},
						Opcode::Br(cond, true_case, false_case) => opcodes.push(Opcode::Br(
							*cond,
							block_offsets[*true_case],
							block_offsets[*false_case],
						)),
						_ => opcodes.push(opcode),
					}
				}
			}

			SSAContext {
				opcodes,
				locals: self.locals,
				constants: self.constants,
			}
		}
	}
}

pub use build::*;

#[cfg(feature = "write")]
mod write {
	use std::io::Error;
	use std::mem::{discriminant, transmute};

	use crate::heaps::HeapScopeRefs;
	use crate::metadata::ssa::{Const, SSAContext};
	use crate::Opcode;
	use crate::write::Write;

	impl<'l> Write<'l> for Opcode<'l> {
		type Requirements = HeapScopeRefs<'l>;
		fn write<T: std::io::Write>(
			&'_ self, stream: &mut T, req: Self::Requirements,
		) -> Result<(), Error> {
			let discriminant: u8 = unsafe { transmute(discriminant(self)) };
			stream.write(&[discriminant])?;

			match self {
				Opcode::Nop => {},
				Opcode::Jp(target) => {
					target.write(stream, ())?;
				},
				Opcode::Br(cond, true_case, false_case) => {
					cond.write(stream, ())?;
					true_case.write(stream, ())?;
					false_case.write(stream, ())?;
				},
				Opcode::Ret(val) => {
					val.write(stream, ())?;
				},
				Opcode::Store(src, dst) => {
					src.write(stream, ())?;
					dst.write(stream, ())?;
				},
				Opcode::Call(func, args, res) => {
					let id = format!("{}/{}", func.namespace(), func.name());
					req.string_heap().intern_str(&id).1.write(stream, ())?;
					args.write(stream, ())?;
					res.write(stream, ())?;
				}
				Opcode::SAdd(lhs, rhs, dst) => {
					lhs.write(stream, ())?;
					rhs.write(stream, ())?;
					dst.write(stream, ())?;
				},
				Opcode::SCmp(lhs, rhs, dst, cmp) => {
					lhs.write(stream, ())?;
					rhs.write(stream, ())?;
					dst.write(stream, ())?;
					cmp.write(stream, ())?;
				},
			}
			Ok(())
		}
	}

	impl<'l> Write<'l> for &[Opcode<'l>] {
		type Requirements = HeapScopeRefs<'l>;
		fn write<T: std::io::Write>(
			&'l self, stream: &mut T, req: Self::Requirements,
		) -> Result<(), Error> {
			for opcode in self.iter() {
				opcode.write(stream, req)?
			}
			Ok(())
		}
	}

	impl<'l> Write<'l> for Const<'l> {
		type Requirements = HeapScopeRefs<'l>;
		fn write<T: std::io::Write>(
			&'l self, stream: &mut T, req: Self::Requirements,
		) -> Result<(), Error> {
			let discriminant: u8 = unsafe { transmute(discriminant(self)) };
			stream.write(&[discriminant])?;

			match self {
				Const::U8(v) => v.write(stream, ()),
				Const::U16(v) => v.write(stream, ()),
				Const::U32(v) => v.write(stream, ()),
				Const::U64(v) => v.write(stream, ()),
				Const::I8(v) => v.write(stream, ()),
				Const::I16(v) => v.write(stream, ()),
				Const::I32(v) => v.write(stream, ()),
				Const::I64(v) => v.write(stream, ()),
				Const::F32(v) => v.write(stream, ()),
				Const::F64(v) => v.write(stream, ()),
				Const::Str(v) => req.string_heap().intern_str(v).1.write(stream, ()),
				Const::Any(body) => body.write(stream, req),
			}
		}
	}

	impl<'l> Write<'l> for SSAContext<'l> {
		type Requirements = HeapScopeRefs<'l>;
		fn write<T: std::io::Write>(
			&'l self, stream: &mut T, req: Self::Requirements,
		) -> Result<(), Error> {
			self.locals.write(stream, req)?;
			self.constants.write(stream, req)?;
			self.opcodes.write(stream, req)
		}
	}
}

pub fn value_type<'l>(
	locals: &[&'l Type<'l>], constants: &[Const<'l>], value: ValueIdx,
) -> Option<&'l Type<'l>> {
	match value {
		ValueIdx::Local(i) => Some(locals.get(i)?),
		ValueIdx::Const(i) => Some(match constants.get(i)? {
			Const::U8(_) => &Type::UInt8,
			Const::U16(_) => &Type::UInt16,
			Const::U32(_) => &Type::UInt32,
			Const::U64(_) => &Type::UInt64,
			Const::I8(_) => &Type::Int8,
			Const::I16(_) => &Type::Int16,
			Const::I32(_) => &Type::Int32,
			Const::I64(_) => &Type::Int64,
			Const::F32(_) => &Type::Float32,
			Const::F64(_) => &Type::Float64,
			Const::Str(_) => &Type::Pointer(Pointer {
				mutable: false,
				ty: &Type::Int8,
			}),
			_ => unimplemented!(),
		}),
	}
}
