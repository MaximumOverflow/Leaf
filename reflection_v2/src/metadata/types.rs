use crate::heaps::{ConstRef, TypeHeap};
use crate::metadata::UniqueIdentifier;
use crate::heaps::type_heap::{TypeRef, TypeRelationship};
use std::fmt::{Display, Formatter};
use std::sync::{Arc, OnceLock};
use fxhash::FxHashMap;
use private::Private;

#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub enum TypeKind {
	#[default]
	Void = 0x00,

	// Integer types
	NInt = 0x10,
	Int8 = 0x11,
	Int16 = 0x12,
	Int32 = 0x13,
	Int64 = 0x14,
	NUInt = 0x15,
	UInt8 = 0x16,
	UInt16 = 0x17,
	UInt32 = 0x18,
	UInt64 = 0x19,
	Char = 0x1A,
	Bool = 0x1B,

	// Floating point types
	Float16 = 0x20,
	Float32 = 0x21,
	Float64 = 0x22,

	// User defined types
	Struct = 0x40,
	Enum = 0x41,
	Union = 0x42,
	Interface = 0x43,

	// Derivative types
	Array = 0x80,
	Pointer = 0x81,
	Reference = 0x82,
	FunctionPointer = 0x83,
}

pub type FieldMap = FxHashMap<ConstRef<str>, TypeRef>;

pub struct Type {
	kind: TypeKind,
	base: Option<TypeRef>,
	fields: OnceLock<FieldMap>,
	ident: Option<UniqueIdentifier>,

	ptr_ty: OnceLock<TypeRef>,
	ref_ty: OnceLock<TypeRef>,
}

impl Type {
	#[inline]
	pub fn kind(&self) -> TypeKind {
		self.kind
	}

	#[inline]
	pub fn base(&self) -> Option<TypeRef> {
		self.base.clone()
	}

	#[inline]
	pub fn fields(&self) -> Option<&FieldMap> {
		self.fields.get()
	}

	#[inline]
	pub fn void() -> TypeRef {
		static TYPE: OnceLock<TypeRef> = OnceLock::new();
		TYPE.get_or_init(|| Self::default_heap().intern(Self::new_intrinsic(TypeKind::Void)))
			.clone()
	}

	#[inline]
	pub fn usize() -> TypeRef {
		static TYPE: OnceLock<TypeRef> = OnceLock::new();
		TYPE.get_or_init(|| Self::default_heap().intern(Self::new_intrinsic(TypeKind::NUInt)))
			.clone()
	}

	pub(crate) fn new_intrinsic(kind: TypeKind) -> Type {
		Type {
			kind,
			base: None,
			ident: None,
			fields: Default::default(),
			ptr_ty: Default::default(),
			ref_ty: Default::default(),
		}
	}

	pub(crate) fn new_struct(ident: UniqueIdentifier) -> Type {
		Type {
			kind: TypeKind::Struct,
			base: None,
			fields: Default::default(),
			ident: Some(ident),
			ptr_ty: Default::default(),
			ref_ty: Default::default(),
		}
	}

	fn default_heap() -> &'static Arc<TypeHeap> {
		static DEFAULT_HEAP: OnceLock<Arc<TypeHeap>> = OnceLock::new();
		DEFAULT_HEAP.get_or_init(|| TypeHeap::new())
	}
}

pub trait BuildType: Private {
	fn set_fields(&self, fields: FieldMap) -> Result<(), FieldMap>;
	fn set_fields_from_iter(
		&self,
		fields: impl IntoIterator<Item = (ConstRef<str>, TypeRef)>,
	) -> Result<(), FieldMap> {
		let fields: FieldMap = fields.into_iter().collect();
		self.set_fields(fields)
	}
}

impl BuildType for TypeRef {
	fn set_fields(&self, mut fields: FieldMap) -> Result<(), FieldMap> {
		let ty = self.get();
		match ty.kind {
			TypeKind::Struct => {
				let heap = self.heap();
				heap.intern_all(fields.values_mut());
				heap.add_edges(self, fields.values(), TypeRelationship::Field);
				ty.fields.set(fields)
			},
			_ => Err(fields),
		}
	}
}

pub trait DeriveType: Private {
	fn as_ptr(&self) -> TypeRef;
	fn as_ref(&self) -> TypeRef;
	fn as_array(&self, size: usize) -> TypeRef;
}

impl DeriveType for TypeRef {
	fn as_ptr(&self) -> TypeRef {
		self.get()
			.ptr_ty
			.get_or_init(|| {
				self.heap().intern(Type {
					kind: TypeKind::Pointer,
					base: Some(self.clone()),
					fields: Default::default(),
					ident: None,
					ptr_ty: Default::default(),
					ref_ty: Default::default(),
				})
			})
			.clone()
	}

	fn as_ref(&self) -> TypeRef {
		self.get()
			.ref_ty
			.get_or_init(|| {
				self.heap().intern(Type {
					kind: TypeKind::Reference,
					base: Some(self.clone()),
					fields: Default::default(),
					ident: None,
					ptr_ty: Default::default(),
					ref_ty: Default::default(),
				})
			})
			.clone()
	}

	fn as_array(&self, _size: usize) -> TypeRef {
		todo!()
	}
}

impl Display for Type {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self.kind {
			TypeKind::Void => write!(f, "void"),
			TypeKind::NInt => write!(f, "isize"),
			TypeKind::Int8 => write!(f, "i8"),
			TypeKind::Int16 => write!(f, "i16"),
			TypeKind::Int32 => write!(f, "i32"),
			TypeKind::Int64 => write!(f, "i64"),
			TypeKind::NUInt => write!(f, "usize"),
			TypeKind::UInt8 => write!(f, "u8"),
			TypeKind::UInt16 => write!(f, "u16"),
			TypeKind::UInt32 => write!(f, "u32"),
			TypeKind::UInt64 => write!(f, "u64"),
			TypeKind::Char => write!(f, "char"),
			TypeKind::Bool => write!(f, "bool"),
			TypeKind::Float16 => write!(f, "f16"),
			TypeKind::Float32 => write!(f, "f32"),
			TypeKind::Float64 => write!(f, "f64"),
			TypeKind::Pointer => write!(f, "*{}", self.base.as_ref().unwrap()),
			TypeKind::Struct | TypeKind::Enum | TypeKind::Union | TypeKind::Interface => {
				write!(f, "{}", self.ident.as_ref().unwrap())
			},
			_ => unimplemented!(),
		}
	}
}

mod private {
	use crate::heaps::type_heap::TypeRef;

	pub trait Private {}
	impl Private for TypeRef {}
}
