pub use build::*;
use leaf_derive::Write;

use crate::{Function, Type};

#[repr(u8)]
#[derive(Debug, Default, Clone)]
pub enum Opcode<'l> {
	#[default]
	Nop = 0x00,
	Jp(usize) = 0x01,
	Br(ValueIdx, usize, usize) = 0x02,
	Ret(Option<ValueIdx>) = 0x03,
	Call(&'l Function<'l>, Vec<ValueIdx>, Option<ValueIdx>) = 0x04,

	Load(ValueIdx, ValueIdx) = 0x10,
	Store(ValueIdx, ValueIdx) = 0x11,
	StoreA(ValueIdx, ValueIdx) = 0x12,
	Aggregate(Vec<ValueIdx>, ValueIdx) = 0x1A,

	SAdd(ValueIdx, ValueIdx, ValueIdx) = 0x20,
	SSub(ValueIdx, ValueIdx, ValueIdx) = 0x21,
	SMul(ValueIdx, ValueIdx, ValueIdx) = 0x22,
	SDiv(ValueIdx, ValueIdx, ValueIdx) = 0x23,
	SMod(ValueIdx, ValueIdx, ValueIdx) = 0x24,
	SCmp(ValueIdx, ValueIdx, ValueIdx, Comparison) = 0x25,

	UAdd(ValueIdx, ValueIdx, ValueIdx) = 0x26,
	USub(ValueIdx, ValueIdx, ValueIdx) = 0x27,
	UMul(ValueIdx, ValueIdx, ValueIdx) = 0x28,
	UDiv(ValueIdx, ValueIdx, ValueIdx) = 0x29,
	UMod(ValueIdx, ValueIdx, ValueIdx) = 0x2A,
	UCmp(ValueIdx, ValueIdx, ValueIdx, Comparison) = 0x2B,

	SConv(ValueIdx, ValueIdx, usize) = 0x2C,
	UConv(ValueIdx, ValueIdx, usize) = 0x2D,

	LNot(ValueIdx, ValueIdx) = 0x30,
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
pub struct ValueIdx(pub usize);

#[derive(Debug, Default)]
pub struct SSAContext<'l> {
	values: Vec<Value<'l>>,
	opcodes: Vec<Opcode<'l>>,
}

#[derive(Debug, Copy, Clone)]
pub struct Value<'l> {
	pub ty: &'l Type<'l>,
	pub const_data: Option<&'l [u8]>,
}

impl<'l> SSAContext<'l> {
	pub fn opcodes(&self) -> &[Opcode<'l>] {
		&self.opcodes
	}

	pub fn values(&'l self) -> &'l [Value<'l>] {
		&self.values
	}

	pub fn value_type(&self, value: ValueIdx) -> Option<&'l Type<'l>> {
		match self.values.get(value.0) {
			None => None,
			Some(v) => Some(v.ty),
		}
	}
}

impl PartialEq for SSAContext<'_> {
	fn eq(&self, _: &Self) -> bool {
		false
	}
}

#[cfg(feature = "build")]
mod build {
	use std::collections::HashMap;

	use leaf_derive::Write;

	use crate::{Opcode, Pointer, Type, Value};
	use crate::heaps::HeapScopes;
	use crate::metadata::ssa::{SSAContext, ValueIdx};

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
		values: Vec<Value<'l>>,
		insert_block: BlockIndex,
		blocks: Vec<Block<'l>>,
		heaps: HeapScopes<'l>,
		consts: HashMap<(&'l Type<'l>, usize), ValueIdx>,
	}

	impl<'l> SSAContextBuilder<'l> {
		pub fn new(heaps: HeapScopes<'l>) -> Self {
			Self {
				heaps,
				values: vec![],
				insert_block: BlockIndex(0),
				blocks: vec![Block { opcodes: vec![] }],
				consts: Default::default(),
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
			&mut self,
			value: ValueIdx,
			true_case: BlockIndex,
			false_case: BlockIndex,
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

		pub fn alloca(&mut self, ty: &'l Type<'l>) -> ValueIdx {
			let idx = ValueIdx(self.values.len());
			self.values.push(Value {
				ty,
				const_data: None,
			});
			idx
		}

		#[allow(private_bounds)]
		pub fn use_const<T: UseConst>(&mut self, value: T) -> ValueIdx {
			value.use_const(self)
		}

		pub fn values(&self) -> &[Value<'l>] {
			&self.values
		}

		pub fn value_type(&self, value: ValueIdx) -> Option<&'l Type<'l>> {
			match self.values.get(value.0) {
				None => None,
				Some(v) => Some(v.ty),
			}
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
				values: self.values,
			}
		}
	}

	trait UseConst {
		fn use_const(self, builder: &mut SSAContextBuilder) -> ValueIdx;
	}

	macro_rules! impl_const {
		($([$ty: ty; $ty_enum: ident]),*) => {$(
			impl UseConst for $ty {
				fn use_const(self, builder: &mut SSAContextBuilder) -> ValueIdx {
					let ty = &Type::$ty_enum;
					let bytes = self.to_le_bytes();
					let (data, id) = builder.heaps.blob_heap().intern_blob(bytes.as_ref());
					*builder.consts.entry((ty, id)).or_insert_with(|| {
						let idx = ValueIdx(builder.values.len());
						builder.values.push(Value { ty, const_data: Some(data) });
						idx
					})
				}
			}
		)*};
	}

	impl_const! {
		[i8; Int8],
		[i16; Int16],
		[i32; Int32],
		[i64; Int64],
		[u8; UInt8],
		[u16; UInt16],
		[u32; UInt32],
		[u64; UInt64],
		[f32; Float32],
		[f64; Float64]
	}

	impl UseConst for bool {
		fn use_const(self, builder: &mut SSAContextBuilder) -> ValueIdx {
			(self as u8).use_const(builder)
		}
	}

	impl UseConst for &str {
		fn use_const(self, builder: &mut SSAContextBuilder) -> ValueIdx {
			self.to_string().use_const(builder)
		}
	}

	impl UseConst for String {
		fn use_const(mut self, builder: &mut SSAContextBuilder) -> ValueIdx {
			if self.as_bytes().last() != Some(&b'\0') {
				self.push('\0');
			}
			self.into_bytes().use_const(builder)
		}
	}

	impl UseConst for Vec<u8> {
		fn use_const(self, builder: &mut SSAContextBuilder) -> ValueIdx {
			let ty = &Type::Pointer(Pointer {
				mutable: false,
				ty: &Type::UInt8,
			});
			let (data, id) = builder.heaps.blob_heap().intern_blob(&self);
			*builder.consts.entry((ty, id)).or_insert_with(|| {
				let idx = ValueIdx(builder.values.len());
				builder.values.push(Value {
					ty,
					const_data: Some(data),
				});
				idx
			})
		}
	}
}

#[cfg(feature = "write")]
mod write {
	use std::io::Error;
	use std::mem::{discriminant, transmute};

	use crate::{Opcode, Value};
	use crate::heaps::HeapScopes;
	use crate::metadata::ssa::SSAContext;
	use crate::write::Write;

	impl<'l> Write<'l> for Opcode<'l> {
		type Requirements = HeapScopes<'l>;
		fn write<T: std::io::Write>(
			&'_ self,
			stream: &mut T,
			req: Self::Requirements,
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
				| Opcode::Load(src, dst) | Opcode::Store(src, dst) | Opcode::StoreA(src, dst) => {
					src.write(stream, ())?;
					dst.write(stream, ())?;
				},
				Opcode::Aggregate(values, dst) => {
					values.write(stream, ())?;
					dst.write(stream, ())?;
				}
				Opcode::Call(func, args, res) => {
					let id = format!("{}/{}", func.namespace(), func.name());
					req.string_heap().intern_str(&id).1.write(stream, ())?;
					args.write(stream, ())?;
					res.write(stream, ())?;
				},
				| Opcode::SAdd(lhs, rhs, dst)
				| Opcode::SSub(lhs, rhs, dst)
				| Opcode::SMul(lhs, rhs, dst)
				| Opcode::SDiv(lhs, rhs, dst)
				| Opcode::SMod(lhs, rhs, dst)
				| Opcode::UAdd(lhs, rhs, dst)
				| Opcode::USub(lhs, rhs, dst)
				| Opcode::UMul(lhs, rhs, dst)
				| Opcode::UDiv(lhs, rhs, dst)
				| Opcode::UMod(lhs, rhs, dst) => {
					lhs.write(stream, ())?;
					rhs.write(stream, ())?;
					dst.write(stream, ())?;
				},
				| Opcode::SCmp(lhs, rhs, dst, cmp) | Opcode::UCmp(lhs, rhs, dst, cmp) => {
					lhs.write(stream, ())?;
					rhs.write(stream, ())?;
					dst.write(stream, ())?;
					cmp.write(stream, ())?;
				},
				| Opcode::SConv(val, dst, bytes) | Opcode::UConv(val, dst, bytes) => {
					val.write(stream, ())?;
					dst.write(stream, ())?;
					bytes.write(stream, ())?;
				},
				Opcode::LNot(val, dst) => {
					val.write(stream, ())?;
					dst.write(stream, ())?;
				},
			}
			Ok(())
		}
	}

	impl<'l> Write<'l> for &[Opcode<'l>] {
		type Requirements = HeapScopes<'l>;
		fn write<T: std::io::Write>(
			&'l self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error> {
			for opcode in self.iter() {
				opcode.write(stream, req.clone())?
			}
			Ok(())
		}
	}

	impl<'l> Write<'l> for Value<'l> {
		type Requirements = HeapScopes<'l>;
		fn write<T: std::io::Write>(
			&'l self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error> {
			self.ty.write(stream, req.clone())?;
			let discriminant: usize = unsafe { transmute(discriminant(&self.const_data)) };
			discriminant.write(stream, ())?;
			if let Some(data) = self.const_data {
				let (_, blob_id) = req.blob_heap().intern_blob(data);
				blob_id.write(stream, ())?;
			}
			Ok(())
		}
	}

	impl<'l> Write<'l> for SSAContext<'l> {
		type Requirements = HeapScopes<'l>;
		fn write<T: std::io::Write>(
			&'l self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error> {
			self.values.write(stream, req.clone())?;
			self.opcodes.write(stream, req)
		}
	}
}
