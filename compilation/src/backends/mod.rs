use leaf_reflection::Assembly;
use object::write::Object;
use std::error::Error;

#[cfg(feature = "backend-x86_64")]
pub mod x86_64;

pub trait CompilationBackend {
	fn compile<'l>(&self, assembly: &'l Assembly<'l>) -> Result<Object, Box<dyn Error>>;
}
