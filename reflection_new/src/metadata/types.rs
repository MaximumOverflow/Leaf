use std::sync::{Arc, OnceLock};

use derivative::Derivative;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Type<'l> {
	Void = 0x00,
	Char = 0x01,
	Bool = 0x02,

	Int8 = 0x10,
	Int16 = 0x11,
	Int32 = 0x12,
	Int64 = 0x13,
	UInt8 = 0x14,
	UInt16 = 0x15,
	UInt32 = 0x16,
	UInt64 = 0x17,

	Float16 = 0x20,
	Float32 = 0x21,
	Float64 = 0x23,

	Array(Array<'l>) = 0x30,
	Pointer(Pointer<'l>) = 0x31,
	Struct(Arc<Struct<'l>>) = 0x32,
}

impl Eq for Type<'_> {}

impl PartialEq<Self> for Type<'_> {
	fn eq(&self, other: &Self) -> bool {
		if std::ptr::eq(self, other) {
			return true;
		}

		if std::mem::discriminant(self) != std::mem::discriminant(other) {
			return false;
		}

		match (self, other) {
			(Type::Array(a), Type::Array(b)) => a == b,
			(Type::Pointer(a), Type::Pointer(b)) => a == b,
			(Type::Struct(a), Type::Struct(b)) => a == b,
			_ => false,
		}
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Array<'l> {
	count: usize,
	ty: &'l Type<'l>,
}

impl<'l> Into<Type<'l>> for Array<'l> {
	fn into(self) -> Type<'l> {
		Type::Array(self)
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Pointer<'l> {
	mutable: bool,
	ty: &'l Type<'l>,
}

impl<'l> Into<Type<'l>> for Pointer<'l> {
	fn into(self) -> Type<'l> {
		Type::Pointer(self)
	}
}

#[derive(Derivative)]
#[derivative(Debug, Eq, PartialEq)]
pub struct Struct<'l> {
	name: &'l str,
	namespace: &'l str,
	fields: OnceLock<Vec<Field<'l>>>,
}

impl<'l> Into<Type<'l>> for Arc<Struct<'l>> {
	fn into(self) -> Type<'l> {
		Type::Struct(self)
	}
}

impl<'l> Struct<'l> {
	pub fn name(&self) -> &str {
		&self.name
	}

	pub fn namespace(&self) -> &'l str {
		&self.namespace
	}

	pub fn fields(&self) -> &[Field<'l>] {
		match self.fields.get() {
			None => &[],
			Some(fields) => fields.as_slice(),
		}
	}
}

#[derive(Debug, Eq, PartialEq)]
pub struct Field<'l> {
	name: &'l str,
	ty: &'l Type<'l>,
}

impl<'l> Field<'l> {
	pub fn name(&self) -> &'l str {
		self.name
	}

	pub fn ty(&self) -> &'l Type<'l> {
		self.ty
	}
}

#[cfg(feature = "build")]
mod build {
	use crate::metadata::types::*;

	impl<'l> Array<'l> {
		pub fn new(ty: &'l Type<'l>, count: usize) -> Self {
			Self { ty, count }
		}
	}

	impl<'l> Pointer<'l> {
		pub fn new(ty: &'l Type<'l>, mutable: bool) -> Self {
			Self { ty, mutable }
		}
	}

	impl<'l> Field<'l> {
		pub fn new(name: &'l str, ty: &'l Type<'l>) -> Self {
			Self { name, ty }
		}
	}

	impl<'l> Struct<'l> {
		pub(crate) fn new(namespace: &'l str, name: &'l str) -> Self {
			Self {
				name,
				namespace,
				fields: OnceLock::new(),
			}
		}

		pub fn set_fields(&self, fields: Vec<Field<'l>>) -> Result<(), Vec<Field<'l>>> {
			self.fields.set(fields)
		}
	}
}

#[cfg(feature = "write")]
mod write {
	use std::io::{Cursor, Error};

	use crate::{Field, Struct};
	use crate::metadata::types::Type;
	use crate::write::{Heaps, Write};

	impl<'l> Type<'l> {
		fn write_recursive<T: std::io::Write>(
			&self, stream: &mut T, req: Heaps<'l>,
		) -> Result<(), Error> {
			let (_, string_heap) = req;

			let discriminant: u8 = unsafe { std::mem::transmute(std::mem::discriminant(self)) };
			stream.write_all(&[discriminant])?;

			match self {
				| Type::Void
				| Type::Char
				| Type::Bool
				| Type::Int8
				| Type::Int16
				| Type::Int32
				| Type::Int64
				| Type::UInt8
				| Type::UInt16
				| Type::UInt32
				| Type::UInt64
				| Type::Float16
				| Type::Float32
				| Type::Float64 => Ok(()),

				Type::Array(data) => {
					data.ty.write_recursive(stream, req)?;
					data.count.write(stream, ())
				},

				Type::Pointer(data) => {
					data.ty.write_recursive(stream, req)?;
					data.mutable.write(stream, ())
				},

				Type::Struct(data) => {
					string_heap.intern_str(&data.namespace).1.write(stream, ())?;
					string_heap.intern_str(&data.name).1.write(stream, ())?;
					Ok(())
				},
			}
		}
	}

	impl<'l> Write<'l> for Type<'l> {
		type Requirements = Heaps<'l>;
		fn write<T: std::io::Write>(&self, stream: &mut T, req: Heaps<'l>) -> Result<(), Error> {
			let (blob_heap, _) = req;

			let mut buffer = vec![];
			let mut buffer_stream = Cursor::new(&mut buffer);

			self.write_recursive(&mut buffer_stream, req)?;

			blob_heap.intern_blob(&buffer).1.write(stream, ())
		}
	}

	impl<'l> Write<'l> for Struct<'l> {
		type Requirements = Heaps<'l>;
		fn write<T: std::io::Write>(&'l self, stream: &mut T, req: Heaps<'l>) -> Result<(), Error> {
			let (_, string_heap) = req;
			string_heap.intern_str(self.namespace).1.write(stream, ())?;
			string_heap.intern_str(self.name).1.write(stream, ())?;
			for field in self.fields() {
				field.write(stream, req)?;
			}
			Ok(())
		}
	}

	impl<'l> Write<'l> for Field<'l> {
		type Requirements = Heaps<'l>;
		fn write<'a, T: std::io::Write>(
			&self, stream: &mut T, req: Heaps<'l>,
		) -> Result<(), Error> {
			let (_, string_heap) = req;
			string_heap.intern_str(self.name).1.write(stream, ())?;
			self.ty.write(stream, req)
		}
	}
}
