use crate::{ElementRef, MetadataWrite, SliceRef};
use leaf_derive::MetadataWrite;
use bytemuck::{Pod, Zeroable};
use std::io::{Error, Write};

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum TypeSignatureTag {
	TypeDef = 0x00,
	TypeRef = 0x01,

	Array = 0x02,
	Pointer = 0x03,
	MutPointer = 0x04,

	Void = 0x05,
	Char = 0x06,
	Bool = 0x07,
	Decimal = 0x08,
	Integer = 0x09,
	UInteger = 0x0A,

	Decimal16 = 0x0B,
	Decimal32 = 0x0C,
	Decimal64 = 0x0D,

	Integer8 = 0x0E,
	Integer16 = 0x0F,
	Integer32 = 0x10,
	Integer64 = 0x11,

	UInteger8 = 0x12,
	UInteger16 = 0x13,
	UInteger32 = 0x14,
	UInteger64 = 0x15,
}
unsafe impl Pod for TypeSignatureTag {}
unsafe impl Zeroable for TypeSignatureTag {}

pub type TypeSignature = [u8];

#[repr(u32)]
#[derive(Default, Copy, Clone)]
pub enum TypeKind {
	#[default]
	Struct,
	Enum,
	Union,
	Interface,
}
unsafe impl Pod for TypeKind {}
unsafe impl Zeroable for TypeKind {}

impl MetadataWrite for TypeKind {
	fn write<T: Write>(&self, stream: &mut T) -> Result<(), Error> {
		let value = *self as u32;
		value.write(stream)
	}
}

#[repr(C)]
#[derive(Default, Copy, Clone, MetadataWrite)]
pub struct TypeDef {
	pub(crate) kind: TypeKind,
	pub(crate) name: ElementRef<str>,
	pub(crate) namespace: ElementRef<str>,
	pub(crate) fields: SliceRef<FieldDef>,
}

impl TypeDef {
	pub fn kind(&self) -> TypeKind {
		self.kind
	}
	pub fn name(&self) -> ElementRef<str> {
		self.name
	}
	pub fn namespace(&self) -> ElementRef<str> {
		self.namespace
	}
	pub fn fields(&self) -> SliceRef<FieldDef> {
		self.fields
	}
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct TypeRef {}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct TypeDefOrRef {
	tag: u32,
	idx: ElementRef<()>,
}

impl From<ElementRef<TypeDef>> for TypeDefOrRef {
	fn from(value: ElementRef<TypeDef>) -> Self {
		Self {
			tag: 0,
			idx: ElementRef {
				offset: value.offset,
				ph: Default::default(),
			},
		}
	}
}

impl TypeDefOrRef {
	pub fn as_type_def(&self) -> Option<ElementRef<TypeDef>> {
		match self.tag == 0 {
			false => None,
			true => Some(ElementRef {
				offset: self.idx.offset,
				ph: Default::default(),
			}),
		}
	}

	pub fn as_type_ref(&self) -> Option<ElementRef<TypeRef>> {
		match self.tag == 1 {
			false => None,
			true => Some(ElementRef {
				offset: self.idx.offset,
				ph: Default::default(),
			}),
		}
	}
}

#[repr(C)]
#[derive(Default, Copy, Clone, MetadataWrite)]
pub struct FieldDef {
	pub(crate) name: ElementRef<str>,
	pub(crate) signature: SliceRef<TypeSignature>,
}
