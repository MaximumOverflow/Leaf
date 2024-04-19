use std::collections::HashMap;
use std::sync::Arc;

use bumpalo::Bump;

use crate::heaps::BlobHeapScope;
use crate::metadata::functions::Function;
use crate::Struct;

pub struct Assembly<'l> {
	name: &'l str,
	assembly_version: Version,
	structs: HashMap<&'l str, &'l Struct<'l>>,
	functions: HashMap<&'l str, &'l Function<'l>>,

	bump: &'l Bump,
	blob_heap: Arc<BlobHeapScope<'l>>,
}

impl<'l> Assembly<'l> {
	pub fn functions(&'l self) -> impl Iterator<Item = &'l Function<'l>> {
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
	use std::sync::Arc;

	use crate::{Struct, Version};
	use crate::heaps::{Heaps, HeapScopes};
	use crate::metadata::assembly::Assembly;
	use crate::metadata::functions::Function;

	impl<'l> Assembly<'l> {
		pub fn new(name: &'l str, version: Version, heaps: &'l Heaps<'l>) -> Self {
			let assembly = Assembly {
				name,
				bump: heaps.bump(),
				blob_heap: Arc::new(heaps.blob_heap().make_scope()),
				assembly_version: version,
				structs: HashMap::new(),
				functions: HashMap::new(),
			};
			assembly
		}

		pub fn create_struct(
			&mut self,
			namespace: &str,
			name: &str,
		) -> Result<&'l Struct<'l>, &'static str> {
			if namespace.contains('/') {
				return Err("Namespace shall not contain '/' characters");
			}
			if name.contains('/') {
				return Err("Name shall not contain '/' characters");
			}

			let id = self
				.blob_heap
				.intern(format! {
					"{}/{}",
					namespace.replace("::", "/"),
					name
				})
				.0;

			let name = self.blob_heap.intern(name).0;

			if self.structs.contains_key(&id) {
				return Err("Type already exists");
			}

			let ty = self.bump.alloc(Struct::new(id, name));
			self.structs.insert(id, ty);
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

			let id = self
				.blob_heap
				.intern(format! {
					"{}/{}",
					namespace.replace("::", "/"),
					name
				})
				.0;

			let name = self.blob_heap.intern(name).0;

			if self.functions.contains_key(id) {
				return Err("Function already exists");
			}

			let func = self.bump.alloc(Function::new(id, name));
			self.functions.insert(id, func);
			Ok(func)
		}

		pub fn heaps(&self) -> HeapScopes<'l> {
			HeapScopes::new(self.bump, self.blob_heap.clone())
		}
	}
}

#[cfg(feature = "read")]
impl crate::serialization::MetadataRead<'_, '_> for Version {
	type Requirements = ();
	fn read<S: std::io::Read>(
		stream: &mut S,
		_: impl Into<Self::Requirements>,
	) -> Result<Self, std::io::Error> {
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
	fn write<S: std::io::Write>(
		&self,
		stream: &mut S,
		req: impl Into<Self::Requirements>,
	) -> Result<(), std::io::Error> {
		stream.write_all(bytemuck::bytes_of(&self.major))?;
		stream.write_all(bytemuck::bytes_of(&self.minor))?;
		stream.write_all(bytemuck::bytes_of(&self.patch))?;
		Ok(())
	}
}
