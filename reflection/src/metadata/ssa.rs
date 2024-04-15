use crate::{Function, Type};
use leaf_derive::Write;

#[repr(u8)]
#[derive(Debug, Default, Clone)]
pub enum Opcode<'l> {
	#[default]
	Nop,
	Ret(Option<ValueIdx>),
	Store(ValueIdx, ValueIdx),
	Call(&'l Function<'l>, Vec<ValueIdx>),
}

#[cfg_attr(feature = "write", derive(Write))]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ValueIdx {
	Param(usize),
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
	Any(SSAContext<'l>)
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
}

impl PartialEq for SSAContext<'_> {
	fn eq(&self, _: &Self) -> bool {
		false
	}
}

#[cfg(feature = "build")]
mod build {
	use crate::{Opcode, Type};
	use crate::metadata::ssa::{Const, SSAContext, ValueIdx};

	impl<'l> SSAContext<'l> {
		pub fn push_opcode(&mut self, opcode: Opcode<'l>) -> usize {
			let offset = self.opcodes.len();
			self.opcodes.push(opcode);
			offset
		}

		pub fn push_local(&mut self, ty: &'l Type<'l>) -> ValueIdx {
			let idx = ValueIdx::Local(self.locals.len());
			self.locals.push(ty);
			idx
		}

		pub fn push_constant(&mut self, val: Const<'l>) -> ValueIdx {
			let idx = ValueIdx::Const(self.locals.len());
			self.constants.push(val);
			idx
		}
	}
}

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
		fn write<T: std::io::Write>(&'_ self, stream: &mut T, req: Self::Requirements) -> Result<(), Error> {
			let discriminant: u8 = unsafe { transmute(discriminant(self)) };
			stream.write(&[discriminant])?;

			match self {
				Opcode::Nop => {}
				Opcode::Ret(val) => {
					val.write(stream, ())?;
				},
				Opcode::Store(src, dst) => {
					src.write(stream, ())?;
					dst.write(stream, ())?;
				}
				Opcode::Call(func, args) => {
					let id = format!("{}/{}", func.namespace(), func.name());
					req.string_heap().intern_str(&id).1.write(stream, ())?;
					args.write(stream, ())?;
				}
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
		fn write<T: std::io::Write>(&'l self, stream: &mut T, req: Self::Requirements) -> Result<(), Error> {
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
				Const::Any(body) => body.write(stream, req)
			}
		}
	}

	impl<'l> Write<'l> for SSAContext<'l> {
		type Requirements = HeapScopeRefs<'l>;
		fn write<T: std::io::Write>(&'l self, stream: &mut T, req: Self::Requirements) -> Result<(), Error> {
			self.locals.write(stream, req)?;
			self.constants.write(stream, req)?;
			self.opcodes.write(stream, req)
		}
	}
}
