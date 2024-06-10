use leaf_parsing::ast::CompilationUnit as Ast;
use crate::frontend::reports::FrontEndReport;

pub struct CompilationUnit<'l> {
	ast: Ast<'l>,
}

impl<'l> CompilationUnit<'l> {
	pub fn new(ast: Ast<'l>) -> Self {
		Self { ast }
	}

	pub fn compile_types(&mut self) -> Result<(), Vec<FrontEndReport>> {
		Ok(())
	}

	pub fn compile_functions(&mut self) -> Result<(), Vec<FrontEndReport>> {
		Ok(())
	}
}
