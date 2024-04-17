use std::collections::HashMap;
use std::path::Path;
use std::sync::OnceLock;

use anyhow::Error;

use leaf_parsing::ast::{CompilationUnit as Ast, Function as FunctionAst, Symbol};
use leaf_parsing::parser::CompilationUnitParser as AstParser;
use leaf_reflection::{Assembly, Function, Parameter, SSAContextBuilder, Type};
use tracing::{debug, error, info, Level, span, trace};
use leaf_reflection::heaps::{Heaps, HeapScopes};
use crate::frontend::block::Block;
use crate::frontend::types::{TypeCache, TypeResolver};

pub struct CompilationUnit<'a, 'l> {
	heaps: HeapScopes<'l>,
	type_cache: &'a TypeCache<'l>,
	assembly: &'a mut Assembly<'l>,
	types: HashMap<&'l str, &'l Type<'l>>,
	functions: HashMap<&'l str, &'l Function<'l>>,
}

impl<'a, 'l> CompilationUnit<'a, 'l> {
	pub fn compile_file(
		type_cache: &'a TypeCache<'l>,
		assembly: &'a mut Assembly<'l>,
		path: &impl AsRef<Path>,
	) -> anyhow::Result<()> {
		let path = path.as_ref();
		let absolute_path = path
			.canonicalize()
			.unwrap()
			.to_str()
			.unwrap()
			.replace("\\\\?\\", "")
			.replace('\\', "/");

		span!(Level::DEBUG, "compile_file", file = absolute_path).in_scope(|| {
			info!("Compiling file `{}`", absolute_path);
			let code = std::fs::read_to_string(path)?;
			Self::compile_internal(type_cache, assembly, &code)?;
			info!("File `{}` compiled successfully", absolute_path);
			Ok(())
		})
	}

	pub fn compile_code(
		type_cache: &'a TypeCache<'l>,
		assembly: &'a mut Assembly<'l>,
		code: &str,
	) -> anyhow::Result<()> {
		span!(Level::DEBUG, "compile_code").in_scope(|| {
			info!("Compiling code");
			Self::compile_internal(type_cache, assembly, code)?;
			info!("Code compiled successfully");
			Ok(())
		})
	}

	fn compile_internal(
		type_cache: &'a TypeCache<'l>,
		assembly: &'a mut Assembly<'l>,
		code: &str,
	) -> anyhow::Result<()> {
		static PARSER: OnceLock<AstParser> = OnceLock::new();
		let parser = PARSER.get_or_init(AstParser::new);

		let ast = match parser.parse(code) {
			Ok(root) => root,
			Err(err) => return Err(Error::msg(err.to_string())),
		};

		let mut unit = Self::new(type_cache, assembly);
		let funcs = unit.create_functions(&ast)?;
		unit.compile_functions(funcs)?;
		Ok(())
	}

	fn new(
		type_cache: &'a TypeCache<'l>,
		assembly: &'a mut Assembly<'l>,
	) -> Self {
		Self {
			type_cache,
			heaps: assembly.heaps(),
			assembly,
			types: HashMap::new(),
			functions: HashMap::new(),
		}
	}

	fn create_functions<'c>(
		&mut self,
		ast: &'c Ast<'c>,
	) -> anyhow::Result<Vec<(&'l Function<'l>, &'c FunctionAst<'c>)>> {
		let mut funcs = vec![];
		for decl in &ast.declarations {
			let name = decl.name;
			let Symbol::Function(decl) = &decl.symbol else {
				continue;
			};

			let func = self.assembly.create_function(ast.namespace, name).unwrap();

			let span = span!(Level::DEBUG, "create_function", id = func.id());
			let _span = span.enter();
			debug!("Creating function");

			trace!("Resolving return type");
			let ret_ty = self.resolve_type(&decl.return_ty)?;
			func.set_return_type(ret_ty).unwrap();

			trace!("Resolving parameter types");
			let mut params = Vec::with_capacity(decl.params.len());
			for p in &decl.params {
				let name = self.assembly.intern_str(p.name);
				params.push(Parameter::new(name, self.resolve_type(&p.ty)?));
			}
			func.set_params(params).unwrap();

			if self.functions.contains_key(name) {
				error!("Duplicate declaration");
				return Err(Error::msg(format!(
					"Duplicate declaration of {}::{}",
					ast.namespace, name
				)));
			}

			self.functions.insert(func.name(), func);

			if decl.block.is_some() {
				funcs.push((func, decl));
				debug!("Function `{}` declared successfully", func.id());
			} else {
				info!("External function `{}` declared successfully", func.id());
			}
		}

		Ok(funcs)
	}

	fn compile_functions(
		&mut self,
		funcs: Vec<(&'l Function<'l>, &FunctionAst)>,
	) -> anyhow::Result<()> {
		for (func, decl) in funcs {
			let span = span!(Level::DEBUG, "compile_function", id = func.id());
			let _span = span.enter();
			debug!("Compiling function `{}`", func.id());

			let mut body = SSAContextBuilder::new(self.heaps.clone());
			let mut block = Block {
				func,
				heaps: self.heaps.clone(),
				type_cache: self.type_cache,
				values: HashMap::new(),
				types: self.types(),
				functions: &self.functions,
			};

			for i in func.params() {
				let idx = body.alloca(i.ty());
				block.values.insert(i.name(), (idx, false));
			}

			block.compile(decl.block.as_ref().unwrap(), &mut body)?;

			func.set_body(body.build()).unwrap();
			info!("Function `{}` compiled successfully", func.id());
		}
		Ok(())
	}
}

impl<'l> TypeResolver<'l> for CompilationUnit<'_, 'l> {
	fn type_cache(&self) -> &TypeCache<'l> {
		self.type_cache
	}
	fn types(&self) -> &HashMap<&'l str, &'l Type<'l>> {
		&self.types
	}
}
