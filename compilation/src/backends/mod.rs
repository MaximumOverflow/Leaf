use std::error::Error;

use leaf_reflection::Assembly;

#[cfg(feature = "backend-x86_64")]
pub mod x86_64;

#[cfg(feature = "backend-llvm")]
pub mod llvm;

pub trait CompilationBackend<'l, 'a> {
	type Output;
	fn compile(&self, assembly: &'a Assembly<'a>) -> Result<Self::Output, Box<dyn Error>>;
}
