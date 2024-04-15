use std::sync::OnceLock;

use anyhow::Error;

use leaf_parsing::ast::CompilationUnit as Ast;
use leaf_parsing::parser::CompilationUnitParser as AstParser;
use leaf_reflection::Assembly;

pub struct CompilationUnit<'a, 'l> {
	ast: Ast<'a>,
	assembly: &'a mut Assembly<'l>,
}

impl<'a, 'l> CompilationUnit<'a, 'l> {
	pub fn compile(assembly: &'a mut Assembly<'l>, code: &'a str) -> anyhow::Result<()> {
		let unit = Self::new(assembly, code)?;
		Ok(())
	}

	fn new(assembly: &'a mut Assembly<'l>, code: &'a str) -> anyhow::Result<Self> {
		static PARSER: OnceLock<AstParser> = OnceLock::new();
		let parser = PARSER.get_or_init(AstParser::new);

		let ast = match parser.parse(code) {
			Ok(root) => root,
			Err(err) => return Err(Error::msg(err.to_string())),
		};

		Ok(
			Self {
				ast,
				assembly,
			}
		)
	}
}
