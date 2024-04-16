use std::collections::HashMap;
use std::sync::Arc;

use bumpalo::Bump;

use crate::heaps::{BlobHeapScope, StringHeapScope};
use crate::metadata::functions::Function;
use crate::Struct;

pub struct Assembly<'l> {
	name: &'l str,
	assembly_version: Version,
	structs: HashMap<String, &'l Struct<'l>>,
	functions: HashMap<&'l str, &'l Function<'l>>,

	bump: &'l Bump,
	blob_heap: Arc<BlobHeapScope<'l>>,
	string_heap: Arc<StringHeapScope<'l>>,
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
	use crate::heaps::Heaps;
	use crate::metadata::assembly::Assembly;
	use crate::metadata::functions::Function;

	impl<'l> Assembly<'l> {
		pub fn new(name: &'l str, version: Version, heaps: &'l Heaps<'l>) -> Self {
			let assembly = Assembly {
				name,
				bump: heaps.bump(),
				blob_heap: Arc::new(heaps.blob_heap().make_scope()),
				string_heap: Arc::new(heaps.string_heap().make_scope()),
				assembly_version: version,
				structs: HashMap::new(),
				functions: HashMap::new(),
			};
			assembly
		}

		pub fn intern_str(&mut self, str: &str) -> &'l str {
			self.string_heap.intern_str(str).0
		}

		pub fn create_struct(
			&mut self, namespace: &str, name: &str,
		) -> Result<&'l Struct<'l>, &'static str> {
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

			let ty = self.bump.alloc(Struct::new(namespace, name));
			self.structs.insert(id, ty);
			Ok(ty)
		}

		pub fn create_function(
			&mut self, namespace: &str, name: &str,
		) -> Result<&'l Function<'l>, &'static str> {
			if namespace.contains('/') {
				return Err("Namespace shall not contain '/' characters");
			}
			if name.contains('/') {
				return Err("Name shall not contain '/' characters");
			}

			let namespace = self.string_heap.intern_str(&namespace.replace("::", "/")).0;
			let name = self.string_heap.intern_str(name).0;

			let id = self.string_heap.intern_str(&format!("{}/{}", namespace, name)).0;
			if self.functions.contains_key(id) {
				return Err("Function already exists");
			}

			let func = self.bump.alloc(Function::new(id, namespace, name));
			self.functions.insert(id, func);
			Ok(func)
		}
	}
}

#[cfg(feature = "write")]
mod write {
	use std::io::{Cursor, Error, Write as IoWrite};

	use bytemuck::bytes_of;

	use crate::heaps::HeapScopeRefs;
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
			let heaps = HeapScopeRefs::new(&self.blob_heap, &self.string_heap);

			self.structs.len().write(&mut tmp_stream, ())?;
			for ty in self.structs.values() {
				ty.write(&mut tmp_stream, heaps)?;
			}

			self.functions.len().write(&mut tmp_stream, ())?;
			for func in self.functions.values() {
				func.write(&mut tmp_stream, heaps)?;
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
