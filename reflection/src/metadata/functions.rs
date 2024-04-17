use std::sync::OnceLock;
use bitflags::bitflags;

use crate::metadata::ssa::SSAContext;
use crate::Type;

#[derive(Debug)]
pub struct Function<'l> {
	id: &'l str,
	namespace: &'l str,
	name: &'l str,
	ret_ty: OnceLock<&'l Type<'l>>,
	params: OnceLock<Vec<Parameter<'l>>>,
	body: OnceLock<FunctionBody<'l>>,
}

impl<'l> Function<'l> {
	pub fn id(&self) -> &str {
		self.id
	}

	pub fn namespace(&self) -> &str {
		self.namespace
	}

	pub fn name(&self) -> &str {
		self.name
	}

	pub fn ret_ty(&self) -> &'l Type<'l> {
		self.ret_ty.get().unwrap()
	}

	pub fn params(&self) -> &[Parameter<'l>] {
		self.params.get().unwrap().as_slice()
	}

	pub fn body(&self) -> Option<&FunctionBody<'l>> {
		self.body.get()
	}
}

#[derive(Debug)]
pub struct Parameter<'l> {
	name: &'l str,
	ty: &'l Type<'l>,
}

pub type FunctionBody<'l> = SSAContext<'l>;

bitflags! {
	pub struct FunctionFlags: u32 {
		const HAS_BODY	= 0b0000000000000001;
		const EXTERNAL	= 0b0000000000000010;
		const INTRINSIC	= 0b0000000000000100;
	}
}

#[cfg(feature = "build")]
mod build {
	use std::sync::OnceLock;

	use crate::metadata::functions::{Function, Parameter};
	use crate::{FunctionBody, Type};

	impl<'l> Function<'l> {
		pub(crate) fn new(id: &'l str, namespace: &'l str, name: &'l str) -> Self {
			Self {
				id,
				name,
				namespace,
				body: OnceLock::new(),
				ret_ty: OnceLock::new(),
				params: OnceLock::new(),
			}
		}

		pub fn set_return_type(&self, ty: &'l Type<'l>) -> Result<(), &'l Type<'l>> {
			self.ret_ty.set(ty)
		}

		pub fn set_params(&self, params: Vec<Parameter<'l>>) -> Result<(), Vec<Parameter<'l>>> {
			self.params.set(params)
		}

		pub fn set_body(&self, body: FunctionBody<'l>) -> Result<(), FunctionBody<'l>> {
			self.body.set(body)
		}
	}

	impl<'l> Parameter<'l> {
		pub fn new(name: &'l str, ty: &'l Type<'l>) -> Self {
			Self { name, ty }
		}

		pub fn name(&self) -> &'l str {
			self.name
		}

		pub fn ty(&self) -> &'l Type<'l> {
			self.ty
		}
	}
}

#[cfg(feature = "write")]
mod write {
	use std::io::{Error, ErrorKind};
	use crate::FunctionFlags;

	use crate::heaps::HeapScopes;
	use crate::metadata::functions::{Function, Parameter};
	use crate::write::Write;

	impl<'l> Write<'l> for Function<'l> {
		type Requirements = HeapScopes<'l>;
		fn write<T: std::io::Write>(
			&'l self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error> {
			let Some(ret_ty) = self.ret_ty.get() else {
				return Err(Error::new(
					ErrorKind::AddrNotAvailable,
					"Return type has not been set",
				));
			};
			let Some(params) = self.params.get() else {
				return Err(Error::new(
					ErrorKind::AddrNotAvailable,
					"Params vec has not been set",
				));
			};

			let body = self.body.get();

			let mut flags = FunctionFlags::empty();
			flags.set(FunctionFlags::HAS_BODY, body.is_some());
			// flags.set(FunctionFlags::EXTERNAL, )

			stream.write_all(&flags.bits().to_le_bytes())?;

			let string_heap = req.string_heap();
			string_heap.intern_str(self.namespace).1.write(stream, ())?;
			string_heap.intern_str(self.name).1.write(stream, ())?;

			params.len().write(stream, ())?;
			for param in params {
				param.write(stream, req.clone())?;
			}

			ret_ty.write(stream, req.clone())?;

			if let Some(body) = body {
				body.write(stream, req)?;
			}

			Ok(())
		}
	}

	impl<'l> Write<'l> for Parameter<'l> {
		type Requirements = HeapScopes<'l>;
		fn write<T: std::io::Write>(
			&'l self,
			stream: &mut T,
			req: Self::Requirements,
		) -> Result<(), Error> {
			let string_heap = req.string_heap();
			string_heap.intern_str(self.name).1.write(stream, ())?;
			self.ty.write(stream, req)
		}
	}
}
