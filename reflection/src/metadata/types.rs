use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::OnceLock;

use derivative::Derivative;
use leaf_derive::Metadata;

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

	Struct(&'l Struct<'l>) = 0x32,
	Array { count: usize, ty: &'l Type<'l> } = 0x30,
	Pointer { mutable: bool, ty: &'l Type<'l> } = 0x31,
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
			(Type::Array { count: ca, ty: ta }, Type::Array { count: cb, ty: tb }) => {
				ca == cb && ta == tb
			}
			(
				Type::Pointer {
					mutable: ma,
					ty: ta,
				},
				Type::Pointer {
					mutable: mb,
					ty: tb,
				},
			) => ma == mb && ta == tb,
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
			Type::Array { count, ty } => write!(f, "[{}; {}]", ty, count),
			Type::Pointer { mutable, ty } => match *mutable {
				false => write!(f, "*{}", ty),
				true => write!(f, "*mut {}", ty),
			},
			Type::Struct(base) => write!(f, "{}::{}", base.namespace(), base.name()),
		}
	}
}

#[derive(Derivative, Metadata)]
#[derivative(Debug, Eq, PartialEq)]
#[metadata(lifetimes(val = "l"))]
pub struct Struct<'l> {
	id: &'l str,
	name: &'l str,
	fields: OnceLock<Vec<Field<'l>>>,
}

impl<'l> Struct<'l> {
	pub fn id(&self) -> &'l str {
		self.id
	}

	pub fn name(&self) -> &str {
		&self.name
	}

	pub fn namespace(&self) -> &'l str {
		match self.id.rsplit_once('/') {
			None => "",
			Some((ns, _)) => ns,
		}
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
		state.write(self.id.as_bytes());
	}
}

#[derive(Debug, Eq, PartialEq, Metadata)]
#[metadata(lifetimes(val = "l"))]
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

	impl<'l> Field<'l> {
		pub fn new(name: &'l str, ty: &'l Type<'l>) -> Self {
			Self { name, ty }
		}
	}

	impl<'l> Struct<'l> {
		pub(crate) fn new(id: &'l str, name: &'l str) -> Self {
			Self {
				id,
				name,
				fields: OnceLock::new(),
			}
		}

		pub fn set_fields(&self, fields: Vec<Field<'l>>) -> Result<(), Vec<Field<'l>>> {
			self.fields.set(fields)
		}
	}
}

#[cfg(feature = "read")]
impl<'val: 'req, 'req> crate::serialization::MetadataRead<'val, 'req> for &'val Type<'val> {
	type Requirements = &'req crate::serialization::ReadDependencies<'val>;
	fn read<S: std::io::Read>(
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<Self, std::io::Error> {
		todo!()
	}
}

#[cfg(feature = "write")]
impl<'val: 'req, 'req> crate::serialization::MetadataWrite<'val, 'req> for &'val Type<'val> {
	type Requirements = &'req crate::serialization::WriteDependencies<'val>;
	fn write<S: std::io::Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), std::io::Error> {
		todo!()
	}
}
