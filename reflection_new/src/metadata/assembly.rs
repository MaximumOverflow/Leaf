use std::collections::HashMap;
use std::sync::Arc;

use crate::heaps::{BlobHeapScope, StringHeapScope};
use crate::metadata::functions::Function;
use crate::Struct;

pub struct Assembly<'l> {
	name: &'l str,
	assembly_version: Version,
	structs: HashMap<String, Arc<Struct<'l>>>,
	functions: HashMap<String, Arc<Function<'l>>>,

	blob_heap: Arc<BlobHeapScope<'l>>,
	string_heap: Arc<StringHeapScope<'l>>,
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

	use crate::heaps::Heaps;
	use crate::{Struct, Version};
	use crate::metadata::assembly::Assembly;
	use crate::metadata::functions::Function;

	impl<'l> Assembly<'l> {
		pub fn new(name: &'l str, version: Version, heaps: &'l Heaps<'l>) -> Self {
			let assembly = Assembly {
				name,
				blob_heap: Arc::new(heaps.blob_heap().make_scope()),
				string_heap: Arc::new(heaps.string_heap().make_scope()),
				assembly_version: version,
				structs: HashMap::new(),
				functions: HashMap::new(),
			};
			assembly
		}

		pub fn create_struct(
			&mut self, namespace: &str, name: &str,
		) -> Result<Arc<Struct<'l>>, &'static str> {
			if namespace.contains('/') {
				return Err("Namespace shall not contain '/' characters");
			}
			if name.contains('/') {
				return Err("Name shall not contain '/' characters");
			}

			let namespace = self.string_heap.intern_str(&namespace.replace("::", "/")).0;
			let name = self.string_heap.intern_str(name).0;

			let id = format!("{}/{}", namespace, name);
			if self.structs.contains_key(&id) {
				return Err("Type already exists");
			}

			let ty = Arc::new(Struct::new(namespace, name));
			self.structs.insert(id, ty.clone());
			Ok(ty)
		}

		pub fn create_function(
			&mut self, namespace: &str, name: &str,
		) -> Result<Arc<Function<'l>>, &'static str> {
			if namespace.contains('/') {
				return Err("Namespace shall not contain '/' characters");
			}
			if name.contains('/') {
				return Err("Name shall not contain '/' characters");
			}

			let namespace = self.string_heap.intern_str(&namespace.replace("::", "/")).0;
			let name = self.string_heap.intern_str(name).0;

			let id = format!("{}/{}", namespace, name);
			if self.functions.contains_key(&id) {
				return Err("Function already exists");
			}

			let func = Arc::new(Function::new(namespace, name));
			self.functions.insert(id, func.clone());
			Ok(func)
		}
	}
}

#[cfg(feature = "write")]
mod write {
	use std::io::{Cursor, Error, Write as IoWrite};

	use bytemuck::bytes_of;

	use crate::metadata::assembly::Assembly;
	use crate::Version;
	use crate::write::Write;

	impl<'l> Write<'l> for Assembly<'l> {
		type Requirements = ();
		fn write<T: IoWrite>(&'l self, stream: &mut T, _: Self::Requirements) -> Result<(), Error> {
			write!(stream, "LEAF")?;
			Version::format_version().unwrap_or_default().write(stream, ())?;
			self.assembly_version.write(stream, ())?;

			(self.name.len() + 1).write(stream, ())?;
			stream.write_all(self.name.as_bytes())?;
			stream.write_all(&[0])?;

			let mut tmp = vec![];
			let mut tmp_stream = Cursor::new(&mut tmp);

			self.structs.len().write(&mut tmp_stream, ())?;
			for ty in self.structs.values() {
				ty.write(&mut tmp_stream, (&self.blob_heap, &self.string_heap))?;
			}

			self.functions.len().write(&mut tmp_stream, ())?;
			for func in self.functions.values() {
				func.write(&mut tmp_stream, (&self.blob_heap, &self.string_heap))?;
			}

			self.string_heap.write(stream, ())?;
			self.blob_heap.write(stream, ())?;
			stream.write_all(&tmp)?;

			Ok(())
		}
	}

	impl Write<'_> for Version {
		type Requirements = ();
		fn write<T: IoWrite>(&'_ self, stream: &mut T, _: ()) -> Result<(), Error> {
			stream.write_all(bytes_of(&self.major))?;
			stream.write_all(bytes_of(&self.minor))?;
			stream.write_all(bytes_of(&self.patch))?;
			Ok(())
		}
	}
}
