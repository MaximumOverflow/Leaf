use std::collections::HashMap;
use std::path::Path;
use std::sync::OnceLock;

use anyhow::Error;

use leaf_parsing::ast::{
	Symbol, CompilationUnit as Ast, Function as FunctionAst, Struct as StructAst, Enum as EnumAst,
	SymbolDeclaration,
};
use leaf_parsing::parser::CompilationUnitParser as AstParser;
use leaf_reflection::{Assembly, Field, Function, Parameter, SSAContextBuilder, Struct, Type};
use tracing::{debug, error, info, instrument, Level, span, trace};
use leaf_reflection::heaps::HeapScopes;
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
		let symbols = unit.declare_symbols(&ast)?;
		unit.compile_types(symbols.structs)?;
		unit.compile_functions(symbols.functions)?;
		Ok(())
	}

	fn new(type_cache: &'a TypeCache<'l>, assembly: &'a mut Assembly<'l>) -> Self {
		Self {
			type_cache,
			heaps: assembly.heaps(),
			assembly,
			types: HashMap::new(),
			functions: HashMap::new(),
		}
	}

	#[instrument(skip_all)]
	fn declare_symbols<'c>(
		&mut self,
		ast: &'c Ast<'c>,
	) -> anyhow::Result<SymbolDeclarations<'l, 'c>> {
		let mut symbols = SymbolDeclarations::default();
		for decl in &ast.declarations {
			let name = decl.name;
			match &decl.symbol {
				Symbol::Struct(decl) => {
					let span = span!(Level::DEBUG, "create_struct", name);
					let _span = span.enter();
					debug!("Creating struct");

					let r#struct = self.assembly.create_struct(ast.namespace, name).unwrap();
					let ty = self.heaps.bump().alloc(Type::Struct(r#struct));
					self.types.insert(r#struct.name(), ty);
					symbols.structs.push((r#struct, decl));
				},
				_ => {},
			}
		}

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
				let name = self.heaps.blob_heap().intern(p.name).0;
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
				symbols.functions.push((func, decl));
				debug!("Function `{}` declared successfully", func.id());
			} else {
				info!("External function `{}` declared successfully", func.id());
			}
		}

		Ok(symbols)
	}

	fn compile_types(&mut self, structs: Vec<(&'l Struct<'l>, &StructAst)>) -> anyhow::Result<()> {
		for (r#struct, decl) in structs {
			let span = span!(Level::DEBUG, "compile_struct", id = r#struct.id());
			let _span = span.enter();
			debug!("Compiling function `{}`", r#struct.id());

			let mut members = vec![];
			for member in &decl.members {
				let ty = self.resolve_type(&member.ty)?;
				let name = self.heaps.blob_heap().intern(member.name).0;
				members.push(Field::new(name, ty));
			}

			r#struct.set_fields(members).unwrap();
			info!("Type `{}` compiled successfully", r#struct.id());
		}
		Ok(())
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

#[derive(Default)]
struct SymbolDeclarations<'l, 'a> {
	structs: Vec<(&'l Struct<'l>, &'a StructAst<'a>)>,
	functions: Vec<(&'l Function<'l>, &'a FunctionAst<'a>)>,
}
