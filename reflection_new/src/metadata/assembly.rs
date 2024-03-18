use std::collections::HashMap;
use std::sync::Arc;

use crate::heaps::{BlobHeapScope, StringHeapScope};
use crate::Struct;

pub struct Assembly<'l> {
    major_version: u16,
    minor_version: u16,
    structs: HashMap<String, Arc<Struct<'l>>>,

    blob_heap: Arc<BlobHeapScope<'l>>,
    string_heap: Arc<StringHeapScope<'l>>,
}

#[cfg(feature = "build")]
mod build {
    use std::collections::HashMap;
    use std::sync::Arc;

    use crate::heaps::{BlobHeap, StringHeap};
    use crate::metadata::assembly::Assembly;
    use crate::Struct;

    impl<'l> Assembly<'l> {
        pub fn new(blob_heap: &'l BlobHeap<'l>, string_heap: &'l StringHeap<'l>) -> Self {
            let assembly = Assembly {
                blob_heap: Arc::new(blob_heap.make_scope()),
                string_heap: Arc::new(string_heap.make_scope()),
                major_version: std::env::var("CARGO_PKG_VERSION_MAJOR").unwrap().parse().unwrap(),
                minor_version: std::env::var("CARGO_PKG_VERSION_MINOR").unwrap().parse().unwrap(),
                structs: HashMap::new(),
            };
            assembly
        }

        pub fn create_struct(&mut self, namespace: &str, name: &str) -> Result<Arc<Struct<'l>>, &'static str> {
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
    }
}

#[cfg(feature = "write")]
mod write {
    use std::io::{Cursor, Error, Write as IoWrite};

    use bytemuck::bytes_of;

    use crate::metadata::assembly::Assembly;
    use crate::write::Write;

    impl<'l> Write<'l> for Assembly<'l> {
        type Requirements = ();
        fn write<T: IoWrite>(&'l self, stream: &mut T, _: Self::Requirements) -> Result<(), Error> {
            write!(stream, "LEAF")?;
            stream.write_all(bytes_of(&self.major_version))?;
            stream.write_all(bytes_of(&self.minor_version))?;

            let mut structs = vec![];
            {
                let mut structs = Cursor::new(&mut structs);
                for ty in self.structs.values() {
                    ty.write(&mut structs, (&self.blob_heap, &self.string_heap))?;
                }
            }

            self.string_heap.write(stream, ())?;
            self.blob_heap.write(stream, ())?;

            stream.write_all(&structs)?;
            drop(structs);

            Ok(())
        }
    }
}
