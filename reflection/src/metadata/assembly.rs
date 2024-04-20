use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
#[allow(unused_imports)]
use std::io::{Error, ErrorKind, Read, Write};
use std::ptr::null;
use std::sync::Arc;
use derivative::Derivative;

use crate::{Struct, Type, UniqueIdentifier};
use crate::heaps::{ArenaAllocator, BlobHeapScope, Heaps, TypeHeap};
use crate::metadata::functions::Function;
use crate::serialization::{ReadRequirements, WriteRequirements};

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Assembly<'l> {
	name: String,
	assembly_version: Version,
	blob_heap: Arc<BlobHeapScope<'l>>,
	#[derivative(Debug(format_with = "debug_types"))]
	types: HashMap<UniqueIdentifier<'l>, &'l Type<'l>>,
	#[derivative(Debug(format_with = "debug_functions"))]
	functions: HashMap<UniqueIdentifier<'l>, &'l Function<'l>>,

	#[derivative(Debug = "ignore")]
	bump: &'l ArenaAllocator,
	#[derivative(Debug = "ignore")]
	type_heap: Arc<TypeHeap<'l>>,
}

impl<'l> Assembly<'l> {
	pub fn functions(&'l self) -> impl Iterator<Item=&'l Function<'l>> {
		self.functions.values().map(move |f| &**f)
	}
}

#[derive(Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Version {
	pub major: u16,
	pub minor: u16,
	pub patch: u16,
}

impl Version {
	pub fn format_version() -> Option<Self> {
		Some(Self {
			major: std::env::var("CARGO_PKG_VERSION_MAJOR").ok()?.parse().ok()?,
			minor: std::env::var("CARGO_PKG_VERSION_MINOR").ok()?.parse().ok()?,
			patch: std::env::var("CARGO_PKG_VERSION_PATCH").ok()?.parse().ok()?,
		})
	}
}

#[cfg(feature = "build")]
mod build {
	use std::collections::HashMap;

	use crate::{Struct, Type, UniqueIdentifier, Version};
	use crate::heaps::{BlobHeap, Heaps, HeapScopes};
	use crate::metadata::assembly::Assembly;
	use crate::metadata::functions::Function;

	impl<'l> Assembly<'l> {
		pub fn new(name: &str, version: Version, heaps: &'l Heaps<'l>) -> Self {
			let assembly = Assembly {
				bump: heaps.general_purpose_heap(),
				name: name.to_string(),
				type_heap: heaps.type_heap().clone(),
				blob_heap: BlobHeap::make_scope(heaps.blob_heap()),
				assembly_version: version,
				types: HashMap::new(),
				functions: HashMap::new(),
			};
			assembly
		}

		pub fn create_struct(
			&mut self,
			namespace: &str,
			name: &str,
		) -> Result<&'l Type<'l>, &'static str> {
			if namespace.contains('/') {
				return Err("Namespace shall not contain '/' characters");
			}
			if name.contains('/') {
				return Err("Name shall not contain '/' characters");
			}

			let name = self.blob_heap.intern(name).0;
			let namespace = self.blob_heap.intern(namespace.replace("::", "/")).0;
			let id = UniqueIdentifier::new(name, namespace);

			if self.types.contains_key(&id) {
				return Err("Type already exists");
			}

			let ty = self.type_heap.r#struct(Struct::new(id, name));
			self.types.insert(id, ty);
			Ok(ty)
		}

		pub fn create_function(
			&mut self,
			namespace: &str,
			name: &str,
		) -> Result<&'l Function<'l>, &'static str> {
			if namespace.contains('/') {
				return Err("Namespace shall not contain '/' characters");
			}
			if name.contains('/') {
				return Err("Name shall not contain '/' characters");
			}

			let name = self.blob_heap.intern(name).0;
			let namespace = self.blob_heap.intern(namespace.replace("::", "/")).0;
			let id = UniqueIdentifier::new(name, namespace);

			let name = self.blob_heap.intern(name).0;

			if self.functions.contains_key(&id) {
				return Err("Function already exists");
			}

			let func = self.bump.alloc(Function::new(id, name));
			self.functions.insert(id, func);
			Ok(func)
		}

		pub fn heaps(&self) -> HeapScopes<'l> {
			HeapScopes::new(self.bump, self.type_heap.clone(), self.blob_heap.clone())
		}
	}
}

#[cfg(feature = "read")]
impl<'val: 'req, 'req> crate::serialization::MetadataRead<'val, 'req> for Assembly<'val> {
	type Requirements = Heaps<'val>;
	#[tracing::instrument(skip_all)]
	fn read<S: Read>(
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<Self, Error> {
		let req = req.into();
		let mut magic = [0; 4];
		stream.read_exact(&mut magic)?;
		if magic.as_slice() != b"LEAF" {
			return Err(Error::new(ErrorKind::InvalidData, format! {
				"Expected magic value {:#?}, found {:#?}",
				u32::from_ne_bytes(*b"LEAF"),
				u32::from_ne_bytes(magic),
			}));
		}

		let _format_version = Version::read(stream, ())?;
		let assembly_version = Version::read(stream, ())?;
		let assembly_name = String::read(stream, ())?;

		let blob_heap = Arc::<BlobHeapScope>::read(
			stream, req.blob_heap(),
		)?;

		let mut requirements = ReadRequirements {
			blobs: blob_heap.clone(),
			type_heap: req.type_heap().clone(),
			structs: null(),
			functions: null(),
		};

		let struct_ids = Vec::<UniqueIdentifier>::read(stream, &requirements)?;
		let function_ids = Vec::<UniqueIdentifier>::read(stream, &requirements)?;

		let structs: HashMap<UniqueIdentifier<'val>, &'val UnsafeCell<Struct<'val>>> = HashMap::from_iter(
			struct_ids.iter().map(|id| {
				(*id, req.type_heap().struct_cell(Struct::new(*id, "")))
			})
		);
		let functions: HashMap<UniqueIdentifier<'val>, &'val UnsafeCell<Function<'val>>> = HashMap::from_iter(
			function_ids.iter().map(|id| {
				(*id, &*req.general_purpose_heap().alloc(UnsafeCell::new(Function::new(*id, ""))))
			})
		);
		requirements.structs = &structs;
		requirements.functions = &functions;

		unsafe {
			for id in struct_ids {
				let ty = Struct::read(stream, &requirements)?;
				let ty_ref = structs[&id];
				drop(std::ptr::read(ty_ref.get()));
				std::ptr::write(ty_ref.get(), ty);
			}

			for id in function_ids {
				let func = Function::read(stream, &requirements)?;
				let func_ref = functions[&id];
				drop(std::ptr::read(func_ref.get()));
				std::ptr::write(func_ref.get(), func);
			}

			Ok(Self {
				name: assembly_name,
				assembly_version,
				types: structs.into_iter().map(|(id, ty)| {
					(id, req.type_heap().struct_ref(&*ty.get()))
				}).collect(),
				functions: functions.into_iter().map(|(id, func)| {
					(id, &*func.get())
				}).collect(),
				bump: req.general_purpose_heap(),
				type_heap: req.type_heap().clone(),
				blob_heap,
			})
		}
	}
}

#[cfg(feature = "write")]
impl<'val> crate::serialization::MetadataWrite<'val, '_> for Assembly<'val> {
	type Requirements = ();
	#[tracing::instrument(skip_all)]
	fn write<S: Write>(
		&self,
		stream: &mut S,
		_: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		stream.write_all(b"LEAF")?;
		Version::format_version().unwrap_or_default().write(stream, ())?;
		self.assembly_version.write(stream, ())?;
		self.name.write(stream, ())?;

		self.blob_heap.write(stream, ())?;

		let requirements = WriteRequirements {
			blobs: self.blob_heap.clone(),
		};

		// Struct IDs
		self.types.len().write(stream, ())?;
		for ty in self.types.values() {
			UniqueIdentifier::write(&ty.id(), stream, &requirements)?;
		}

		// Function IDs
		self.functions.len().write(stream, ())?;
		for func in self.functions.values() {
			UniqueIdentifier::write(&func.id(), stream, &requirements)?;
		}

		// Structs
		for ty in self.types.values() {
			let Type::Struct(ty) = ty else { unreachable!() };
			Struct::write(ty, stream, &requirements)?;
		}
		// Functions
		for func in self.functions.values() {
			Function::write(func, stream, &requirements)?;
		}

		Ok(())
	}
}

#[cfg(feature = "read")]
impl crate::serialization::MetadataRead<'_, '_> for Version {
	type Requirements = ();
	fn read<S: Read>(
		stream: &mut S,
		_: impl Into<Self::Requirements>,
	) -> Result<Self, Error> {
		let mut bytes = [0; 2];
		stream.read_exact(&mut bytes)?;
		let major = u16::from_le_bytes(bytes);
		stream.read_exact(&mut bytes)?;
		let minor = u16::from_le_bytes(bytes);
		stream.read_exact(&mut bytes)?;
		let patch = u16::from_le_bytes(bytes);
		Ok(Self {
			major,
			minor,
			patch,
		})
	}
}

#[cfg(feature = "write")]
impl crate::serialization::MetadataWrite<'_, '_> for Version {
	type Requirements = ();
	fn write<S: Write>(
		&self,
		stream: &mut S,
		_: impl Into<Self::Requirements>,
	) -> Result<(), Error> {
		stream.write_all(&self.major.to_le_bytes())?;
		stream.write_all(&self.minor.to_le_bytes())?;
		stream.write_all(&self.patch.to_le_bytes())?;
		Ok(())
	}
}

fn debug_types(v: &HashMap<UniqueIdentifier, &Type>, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	let mut list = fmt.debug_list();
	for ty in v.values() {
		list.entry(ty);
	}
	list.finish()
}

fn debug_functions(v: &HashMap<UniqueIdentifier, &Function>, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
	let mut list = fmt.debug_list();
	for ty in v.values() {
		list.entry(ty);
	}
	list.finish()
}
