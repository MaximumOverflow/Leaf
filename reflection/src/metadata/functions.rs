use std::fmt::{Debug, Display, Formatter};
use std::sync::OnceLock;
use bitflags::bitflags;

use crate::metadata::ssa::SSAContext;
use crate::{Type, UniqueIdentifier};

#[allow(unused_imports)]
use std::io::{Read, Write, Error, ErrorKind};
use derivative::Derivative;
use leaf_derive::Metadata;

#[derive(Derivative, Metadata)]
#[metadata(lifetimes(val = "l"))]
#[derivative(Debug)]
pub struct Function<'l> {
	// Should always be the first field
	#[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
	id: UniqueIdentifier<'l>,
	name: &'l str,
	#[derivative(Debug(format_with = "debug_ret_ty"))]
	ret_ty: OnceLock<&'l Type<'l>>,
	#[derivative(Debug(format_with = "debug_params"))]
	params: OnceLock<Vec<Parameter<'l>>>,
	#[derivative(Debug(format_with = "debug_body"))]
	body: OnceLock<Option<FunctionBody<'l>>>,
}

impl<'l> Function<'l> {
	pub fn id(&self) -> UniqueIdentifier<'l> {
		self.id
	}

	pub fn namespace(&self) -> &'l str {
		self.id.namespace
	}

	pub fn name(&self) -> &'l str {
		self.name
	}

	pub fn ret_ty(&self) -> &'l Type<'l> {
		self.ret_ty.get().unwrap()
	}

	pub fn params(&self) -> &[Parameter<'l>] {
		self.params.get().unwrap().as_slice()
	}

	pub fn body(&self) -> Option<&FunctionBody<'l>> {
		self.body.get()?.as_ref()
	}
}

impl Display for Function<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "`{}`", self.id)
	}
}

#[derive(Metadata, Derivative)]
#[metadata(lifetimes(val = "l"))]
#[derivative(Debug)]
pub struct Parameter<'l> {
	name: &'l str,
	#[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
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
	use crate::{FunctionBody, Type, UniqueIdentifier};

	impl<'l> Function<'l> {
		pub(crate) fn new(id: UniqueIdentifier<'l>, name: &'l str) -> Self {
			Self {
				id,
				name,
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

		pub fn set_body(&self, body: Option<FunctionBody<'l>>) -> Result<(), Option<FunctionBody<'l>>> {
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

#[cfg(feature = "read")]
impl<'val: 'req, 'req> crate::serialization::MetadataRead<'val, 'req> for &'val Function<'val> {
	type Requirements = &'req crate::serialization::ReadRequirements<'val>;
	fn read<S: Read>(
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<Self, Error> {
		let req = req.into();
		unsafe {
			assert!(!req.functions.is_null());
			let structs = &*req.functions;
			let id = UniqueIdentifier::read(stream, req)?;
			match structs.get(&id) {
				Some(cell) => Ok(&*cell.get()),
				None => Err(Error::new(ErrorKind::NotFound, format!("Could not retrieve function `{id}`"))),
			}
		}
	}
}

#[cfg(feature = "write")]
impl<'val: 'req, 'req> crate::serialization::MetadataWrite<'val, 'req> for &'val Function<'val> {
	type Requirements = &'req crate::serialization::WriteRequirements<'val>;
	fn write<S: Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		self.id.write(stream, req)
	}
}

fn debug_ret_ty(v: &OnceLock<&Type>, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	match v.get() {
		None => fmt.write_str("?"),
		Some(ty) => write!(fmt, "{}", ty),
	}
}

fn debug_body(v: &OnceLock<Option<SSAContext>>, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	match v.get() {
		None => fmt.write_str("?"),
		Some(None) => fmt.write_str("None"),
		Some(Some(body)) => Debug::fmt(body, fmt),
	}
}

fn debug_params(v: &OnceLock<Vec<Parameter>>, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	match v.get() {
		None => fmt.write_str("?"),
		Some(params) => {
			let mut list = fmt.debug_set();
			for param in params {
				list.entry(&format_args!("{}: {}", param.name, param.ty));
			}
			list.finish()
		}
	}
}
