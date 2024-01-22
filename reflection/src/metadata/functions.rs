use crate::{SliceRef, TypeSignature};
use leaf_derive::MetadataWrite;

#[repr(C)]
#[derive(Default, Copy, Clone, MetadataWrite)]
pub struct FunctionDef {
	pub(crate) name: SliceRef<str>,
	pub(crate) namespace: SliceRef<str>,
	pub(crate) params: SliceRef<ParameterDef>,
	pub(crate) return_ty: SliceRef<TypeSignature>,
	pub(crate) body: FunctionBody,
}

impl FunctionDef {
	pub fn name(&self) -> SliceRef<str> {
		self.name
	}
	pub fn namespace(&self) -> SliceRef<str> {
		self.namespace
	}
	pub fn params(&self) -> SliceRef<ParameterDef> {
		self.params
	}
	pub fn return_ty(&self) -> SliceRef<TypeSignature> {
		self.return_ty
	}
	pub fn body(&self) -> FunctionBody {
		self.body
	}
}

#[repr(C)]
#[derive(Default, Copy, Clone, MetadataWrite)]
pub struct MethodDef {
	pub(crate) name: SliceRef<str>,
	pub(crate) parameters: SliceRef<ParameterDef>,
}

#[repr(C)]
#[derive(Default, Copy, Clone, MetadataWrite)]
pub struct ParameterDef {
	pub(crate) name: SliceRef<str>,
	pub(crate) signature: SliceRef<TypeSignature>,
}

#[repr(C)]
#[derive(Default, Copy, Clone, MetadataWrite)]
pub struct FunctionBody {
	pub(crate) locals: SliceRef<SliceRef<TypeSignature>>,
	pub(crate) opcodes: SliceRef<[u8]>,
}
