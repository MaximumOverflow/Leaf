use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::OnceLock;

use derivative::Derivative;
use leaf_derive::Metadata;
use crate::UniqueIdentifier;

#[allow(unused_imports)]
use std::io::{Read, Write, Error, ErrorKind};
use std::io::Cursor;

#[repr(u8)]
#[derive(Clone, Hash, Metadata)]
#[metadata(lifetimes(val = "l"))]
pub enum Type<'l> {
	Uninit = 0x00,
	Void = 0x01,
	Char = 0x02,
	Bool = 0x03,

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
	Float64 = 0x22,

	Struct(&'l Struct<'l>) = 0x40,
	Array {
		count: usize,
		ty: &'l Type<'l>,
	} = 0x41,
	Pointer {
		mutable: bool,
		ty: &'l Type<'l>,
	} = 0x42,
	Reference {
		mutable: bool,
		ty: &'l Type<'l>,
	} = 0x43,
	FunctionPointer {
		ret_ty: &'l Type<'l>,
		param_tys: &'l [&'l Type<'l>],
	} = 0x44,
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
			},
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
			(Type::Struct(a), Type::Struct(b)) => std::ptr::eq(*a, *b),
			_ => std::mem::discriminant(self) == std::mem::discriminant(other),
		}
	}
}

impl Display for Type<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Type::Uninit => write!(f, "?"),
			Type::Void => write!(f, "void"),
			Type::Char => write!(f, "char"),
			Type::Bool => write!(f, "bool"),
			Type::Int8 => write!(f, "i8"),
			Type::Int16 => write!(f, "i16"),
			Type::Int32 => write!(f, "i32"),
			Type::Int64 => write!(f, "i64"),
			Type::UInt8 => write!(f, "u8"),
			Type::UInt16 => write!(f, "u16"),
			Type::UInt32 => write!(f, "u32"),
			Type::UInt64 => write!(f, "u64"),
			Type::Float16 => write!(f, "f16"),
			Type::Float32 => write!(f, "f32"),
			Type::Float64 => write!(f, "f64"),
			Type::Struct(base) => Display::fmt(&base.id, f),
			Type::Array { count, ty } => write!(f, "[{}; {}]", ty, count),
			Type::Pointer { mutable, ty } => match *mutable {
				false => write!(f, "*{}", ty),
				true => write!(f, "*mut {}", ty),
			},
			Type::Reference { mutable, ty } => match *mutable {
				false => write!(f, "&{}", ty),
				true => write!(f, "&mut {}", ty),
			},
			Type::FunctionPointer { ret_ty, param_tys } => {
				let mut separator = "";
				write!(f, "(")?;
				for param in *param_tys {
					write!(f, "{separator}{param}")?;
					separator = ", ";
				}
				write!(f, ") -> {ret_ty}")
			},
		}
	}
}

impl Debug for Type<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Type::Struct(s) => Debug::fmt(s, f),
			_ => Display::fmt(self, f),
		}
	}
}

impl Type<'_> {
	#[inline(always)]
	pub(crate) fn discriminant(&self) -> u8 {
		unsafe { std::mem::transmute(std::mem::discriminant(self)) }
	}

	#[inline(always)]
	pub fn id(&self) -> UniqueIdentifier {
		match self {
			Type::Void => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "void",
			},
			Type::Char => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "char",
			},
			Type::Bool => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "bool",
			},
			Type::Int8 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "i8",
			},
			Type::Int16 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "i16",
			},
			Type::Int32 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "i32",
			},
			Type::Int64 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "i64",
			},
			Type::UInt8 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "u8",
			},
			Type::UInt16 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "u16",
			},
			Type::UInt32 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "u32",
			},
			Type::UInt64 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "u64",
			},
			Type::Float16 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "f16",
			},
			Type::Float32 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "f32",
			},
			Type::Float64 => UniqueIdentifier {
				namespace: "core::intrinsics::types",
				name: "f64",
			},
			Type::Struct(data) => data.id,
			Type::Uninit => unreachable!(),
			Type::Array { .. } => unreachable!(),
			Type::Pointer { .. } => unreachable!(),
			Type::Reference { .. } => unreachable!(),
			Type::FunctionPointer { .. } => unreachable!(),
		}
	}

	#[inline(always)]
	pub fn is_integer(&self) -> bool {
		self.discriminant() & 0x10 != 0
	}

	#[inline(always)]
	pub fn is_float(&self) -> bool {
		self.discriminant() & 0x20 != 0
	}

	#[inline(always)]
	pub fn is_intrinsic(&self) -> bool {
		self.discriminant() & 0x40 == 0
	}

	#[inline(always)]
	pub fn is_signed_integer(&self) -> Option<bool> {
		match self {
			Type::Int8 | Type::Int16 | Type::Int32 | Type::Int64 => Some(true),
			Type::UInt8 | Type::UInt16 | Type::UInt32 | Type::UInt64 => Some(false),
			_ => None,
		}
	}

	#[inline(always)]
	pub fn integer_size(&self) -> Option<usize> {
		match self {
			Type::Int8 | Type::UInt8 => Some(1),
			Type::Int16 | Type::UInt16 => Some(2),
			Type::Int32 | Type::UInt32 => Some(4),
			Type::Int64 | Type::UInt64 => Some(8),
			_ => None,
		}
	}

	#[inline(always)]
	pub fn float_size(&self) -> Option<usize> {
		match self {
			Type::Float16 => Some(2),
			Type::Float32 => Some(4),
			Type::Float64 => Some(8),
			_ => None,
		}
	}
}

#[derive(Derivative, Metadata)]
#[derivative(Debug)]
#[metadata(lifetimes(val = "l"))]
pub struct Struct<'l> {
	// Should always be the first field
	#[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
	id: UniqueIdentifier<'l>,
	name: &'l str,
	#[derivative(Debug(format_with = "debug_fields"))]
	fields: OnceLock<Vec<Field<'l>>>,
}

impl<'l> Struct<'l> {
	pub fn id(&self) -> UniqueIdentifier<'l> {
		self.id
	}

	pub fn name(&self) -> &str {
		&self.name
	}

	pub fn namespace(&self) -> &'l str {
		self.id.namespace
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
		self.id.hash(state)
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
		pub(crate) fn new(id: UniqueIdentifier<'l>, name: &'l str) -> Self {
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
impl<'val: 'req, 'req> crate::serialization::MetadataRead<'val, 'req> for &'val Struct<'val> {
	type Requirements = &'req crate::serialization::ReadRequirements<'val>;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		let req = req.into();
		unsafe {
			assert!(!req.structs.is_null());
			let structs = &*req.structs;
			let id = UniqueIdentifier::read(stream, req)?;
			match structs.get(&id) {
				Some(cell) => Ok(&*cell.get()),
				None => Err(Error::new(
					ErrorKind::NotFound,
					format!("Could not retrieve type `{id}`"),
				)),
			}
		}
	}
}

#[cfg(feature = "write")]
impl<'val: 'req, 'req> crate::serialization::MetadataWrite<'val, 'req> for &'val Struct<'val> {
	type Requirements = &'req crate::serialization::WriteRequirements<'val>;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		self.id.write(stream, req)
	}
}

#[cfg(feature = "read")]
impl<'val: 'req, 'req> crate::serialization::MetadataRead<'val, 'req> for &'val Type<'val> {
	type Requirements = &'req crate::serialization::ReadRequirements<'val>;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		let req = req.into();
		let blob: &'val [u8] = crate::serialization::MetadataRead::read(stream, req)?;
		let ty: Type<'val> = Type::read(&mut Cursor::new(blob), req)?;
		match ty {
			Type::Uninit => Ok(&Type::Uninit),
			Type::Void => Ok(&Type::Void),
			Type::Char => Ok(&Type::Char),
			Type::Bool => Ok(&Type::Bool),
			Type::Int8 => Ok(&Type::Int8),
			Type::Int16 => Ok(&Type::Int16),
			Type::Int32 => Ok(&Type::Int32),
			Type::Int64 => Ok(&Type::Int64),
			Type::UInt8 => Ok(&Type::UInt8),
			Type::UInt16 => Ok(&Type::UInt16),
			Type::UInt32 => Ok(&Type::UInt32),
			Type::UInt64 => Ok(&Type::UInt64),
			Type::Float16 => Ok(&Type::Float16),
			Type::Float32 => Ok(&Type::Float32),
			Type::Float64 => Ok(&Type::Float64),
			Type::Struct(data) => Ok(req.type_heap.struct_ref(data)),
			Type::Array { ty, count } => Ok(req.type_heap.array(ty, count)),
			Type::Pointer { mutable, ty } => Ok(req.type_heap.pointer(ty, mutable)),
			Type::Reference { mutable, ty } => Ok(req.type_heap.reference(ty, mutable)),
			Type::FunctionPointer { .. } => unimplemented!(),
		}
	}
}

#[cfg(feature = "write")]
impl<'val: 'req, 'req> crate::serialization::MetadataWrite<'val, 'req> for &'val Type<'val> {
	type Requirements = &'req crate::serialization::WriteRequirements<'val>;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		let req = req.into();
		let mut buffer = vec![];
		Type::write(self, &mut buffer, req)?;
		let Some(idx) = req.blobs.get_blob_index(&buffer) else {
			return Err(Error::new(
				ErrorKind::NotFound,
				format!("Type `{self}` was not previously interned"),
			));
		};
		idx.write(stream, ())
	}
}

#[cfg(feature = "read")]
impl<'val: 'req, 'req> crate::serialization::MetadataRead<'val, 'req>
	for &'val [&'val Type<'val>]
{
	type Requirements = &'req crate::serialization::ReadRequirements<'val>;
	fn read<S: Read>(stream: &mut S, req: impl Into<Self::Requirements>) -> Result<Self, Error> {
		let req = req.into();
		let len = usize::read(stream, ())?;
		let mut types = Vec::with_capacity(len);
		for _ in 0..len {
			let ty = <&'val Type<'val>>::read(stream, req)?;
			types.push(ty);
		}
		Ok(req.blobs.arena_allocator().alloc_slice_copy(&types))
	}
}

#[cfg(feature = "write")]
impl<'val: 'req, 'req> crate::serialization::MetadataWrite<'val, 'req>
	for &'val [&'val Type<'val>]
{
	type Requirements = &'req crate::serialization::WriteRequirements<'val>;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		let req = req.into();
		self.len().write(stream, ())?;
		for ty in self.iter() {
			<&'val Type<'val>>::write(ty, stream, req)?;
		}
		Ok(())
	}
}

fn debug_fields(v: &OnceLock<Vec<Field>>, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	match v.get() {
		None => fmt.write_str("?"),
		Some(fields) => {
			let mut list = fmt.debug_set();
			for field in fields {
				list.entry(&format_args!("{}: {}", field.name, field.ty));
			}
			list.finish()
		},
	}
}
