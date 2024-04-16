use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::OnceLock;

use derivative::Derivative;

#[repr(u8)]
#[derive(Debug, Clone, Hash)]
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
	Struct(&'l Struct<'l>) = 0x32,
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
			_ => std::mem::discriminant(self) == std::mem::discriminant(other),
		}
	}
}

impl Display for Type<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Type::Void => f.write_str("void"),
			Type::Char => f.write_str("char"),
			Type::Bool => f.write_str("bool"),
			Type::Int8 => f.write_str("i8"),
			Type::Int16 => f.write_str("i16"),
			Type::Int32 => f.write_str("i32"),
			Type::Int64 => f.write_str("i64"),
			Type::UInt8 => f.write_str("u8"),
			Type::UInt16 => f.write_str("u16"),
			Type::UInt32 => f.write_str("u32"),
			Type::UInt64 => f.write_str("u64"),
			Type::Float16 => f.write_str("f16"),
			Type::Float32 => f.write_str("f32"),
			Type::Float64 => f.write_str("f64"),
			Type::Array(data) => write!(f, "[{}; {}]", data.ty, data.count),
			Type::Pointer(Pointer { mutable, ty }) => match *mutable {
				false => write!(f, "*{}", ty),
				true => write!(f, "*mut {}", ty),
			},
			Type::Struct(base) => write!(f, "{}::{}", base.namespace, base.name),
		}
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Array<'l> {
	pub count: usize,
	pub ty: &'l Type<'l>,
}

impl<'l> Array<'l> {
	pub fn count(&self) -> usize {
		self.count
	}

	pub fn ty(&self) -> &'l Type<'l> {
		self.ty
	}
}

impl<'l> Into<Type<'l>> for Array<'l> {
	fn into(self) -> Type<'l> {
		Type::Array(self)
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Pointer<'l> {
	pub mutable: bool,
	pub ty: &'l Type<'l>,
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

impl<'l> Into<Type<'l>> for &'l Struct<'l> {
	fn into(self) -> Type<'l> {
		Type::Struct(self)
	}
}

impl Hash for Struct<'_> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write(self.namespace.as_bytes());
		state.write(self.name.as_bytes());
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
	use crate::heaps::HeapScopeRefs;
	use crate::metadata::types::Type;
	use crate::write::Write;

	impl<'l> Type<'l> {
		fn write_recursive<T: std::io::Write>(
			&self,
			stream: &mut T,
			req: HeapScopeRefs<'l>,
		) -> Result<(), Error> {
			let string_heap = req.string_heap();

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
		type Requirements = HeapScopeRefs<'l>;
		fn write<T: std::io::Write>(
			&self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error> {
			let blob_heap = req.blob_heap();

			let mut buffer = vec![];
			let mut buffer_stream = Cursor::new(&mut buffer);

			self.write_recursive(&mut buffer_stream, req)?;

			blob_heap.intern_blob(&buffer).1.write(stream, ())
		}
	}

	impl<'l> Write<'l> for Struct<'l> {
		type Requirements = HeapScopeRefs<'l>;
		fn write<T: std::io::Write>(
			&'l self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error> {
			let string_heap = req.string_heap();
			string_heap.intern_str(self.namespace).1.write(stream, ())?;
			string_heap.intern_str(self.name).1.write(stream, ())?;
			for field in self.fields() {
				field.write(stream, req)?;
			}
			Ok(())
		}
	}

	impl<'l> Write<'l> for Field<'l> {
		type Requirements = HeapScopeRefs<'l>;
		fn write<'a, T: std::io::Write>(
			&self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error> {
			let string_heap = req.string_heap();
			string_heap.intern_str(self.name).1.write(stream, ())?;
			self.ty.write(stream, req)
		}
	}
}
