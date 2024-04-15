use std::sync::OnceLock;

use crate::Type;

#[derive(Debug)]
pub struct Function<'l> {
	namespace: &'l str,
	name: &'l str,
	ret_ty: OnceLock<&'l Type<'l>>,
	params: OnceLock<Vec<Parameter<'l>>>,
}

#[derive(Debug)]
pub struct Parameter<'l> {
	name: &'l str,
	ty: &'l Type<'l>,
}

#[cfg(feature = "build")]
mod build {
	use std::sync::OnceLock;
	use crate::metadata::functions::{Function, Parameter};
	use crate::Type;

	impl<'l> Function<'l> {
		pub(crate) fn new(namespace: &'l str, name: &'l str) -> Self {
			Self {
				name,
				namespace,
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

	use crate::metadata::functions::{Function, Parameter};
	use crate::write::{Heaps, Write};

	impl<'l> Write<'l> for Function<'l> {
		type Requirements = Heaps<'l>;
		fn write<T: std::io::Write>(&'l self, stream: &mut T, req: Heaps<'l>) -> Result<(), Error> {
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

			let (_, string_heap) = req;
			string_heap.intern_str(self.namespace).1.write(stream, ())?;
			string_heap.intern_str(self.name).1.write(stream, ())?;

			params.len().write(stream, ())?;
			for param in params {
				param.write(stream, req)?;
			}

			ret_ty.write(stream, req)
		}
	}

	impl<'l> Write<'l> for Parameter<'l> {
		type Requirements = Heaps<'l>;
		fn write<T: std::io::Write>(&'l self, stream: &mut T, req: Heaps<'l>) -> Result<(), Error> {
			let (_, string_heap) = req;
			string_heap.intern_str(self.name).1.write(stream, ())?;
			self.ty.write(stream, req)
		}
	}
}
