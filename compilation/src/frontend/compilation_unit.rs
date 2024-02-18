use leaf_reflection::structured::{Type, Function};
use leaf_parsing::parser::CompilationUnitParser;
use std::sync::{Arc, OnceLock};
use std::collections::HashMap;
use anyhow::{Context, Error};
use leaf_reflection::structured::assembly::AssemblyBuilder;
use leaf_reflection::structured::functions::FunctionBodyBuilder;
use leaf_reflection::structured::types::StructBuilder;
use crate::BUG_ERR;
use crate::frontend::block::{Block, BlockRequirements};
use crate::frontend::expressions::Value;
use crate::frontend::types::TypeResolver;

pub struct CompilationUnit<'l> {
	namespace: Arc<str>,
	assembly: &'l mut AssemblyBuilder,
	available_types: HashMap<Arc<str>, Arc<Type>>,
	available_functions: HashMap<Arc<str>, Arc<Function>>,
}

impl<'l> CompilationUnit<'l> {
	pub fn new(assembly_builder: &'l mut AssemblyBuilder, code: &str) -> anyhow::Result<Self> {
		static PARSER: OnceLock<CompilationUnitParser> = OnceLock::new();
		let parser = PARSER.get_or_init(CompilationUnitParser::new);

		let root = match parser.parse(code) {
			Ok(root) => root,
			Err(err) => return Err(Error::msg(err.to_string())),
		};

		let mut unit = Self {
			assembly: assembly_builder,
			namespace: Arc::from(root.namespace),
			available_types: Default::default(),
			available_functions: Default::default(),
		};

		let structs = unit.define_structs(&root);
		unit.populate_structs(structs);

		let functions = unit.define_functions(&root);
		unit.compile_functions(functions);

		Ok(unit)
	}

	fn define_structs<'a>(
		&mut self,
		ast: &'a leaf_parsing::ast::CompilationUnit<'a>,
	) -> Vec<(StructBuilder, &'a leaf_parsing::ast::Struct<'a>)> {
		let mut decls = vec![];
		for decl in &ast.declarations {
			if let leaf_parsing::ast::SymbolDeclaration::Struct(decl) = decl {
				let id = decl.id.unwrap_or_default();
				let builder = self.assembly.define_struct(id, &self.namespace);

				let ty = builder.as_ref();
				let id = ty.name_arc().unwrap();
				self.available_types.insert(id.clone(), ty.clone());
				decls.push((builder, decl));
			}
		}
		decls
	}

	fn populate_structs(
		&mut self,
		structs: Vec<(StructBuilder, &leaf_parsing::ast::Struct)>
	) {
		for (mut builder, decl) in structs {
			for member in &decl.members {
				let ty = self.resolve_type(&member.ty).unwrap();
				builder.define_field(member.id, &ty).unwrap();
			}
			builder.build();
		}
	}

	fn define_functions<'a>(
		&mut self,
		ast: &'a leaf_parsing::ast::CompilationUnit<'a>,
	) -> Vec<(FunctionBodyBuilder, &'a leaf_parsing::ast::Function<'a>)> {
		let mut decls = vec![];
		for decl in &ast.declarations {
			if let leaf_parsing::ast::SymbolDeclaration::Function(decl) = decl {
				let id = decl.signature.id.unwrap_or_default();

				let mut params = vec![];
				for (name, ty) in &decl.signature.par {
					let ty = self.resolve_type(ty).unwrap();
					params.push((*name, ty));
				}
				let ret_ty = self.resolve_type(&decl.signature.ret_ty).unwrap();

				let mut builder = self.assembly.define_function(id, &self.namespace);

				builder.set_return_type(&ret_ty);
				for (name, ty) in params {
					builder.define_parameter(name, &ty).unwrap();
				}

				let (func, builder) = builder.declare();
				self.available_functions.insert(func.name_arc().clone(), func);
				decls.push((builder, decl));
			}
		}
		decls
	}

	fn compile_functions(
		&mut self,
		functions: Vec<(FunctionBodyBuilder, &leaf_parsing::ast::Function)>
	) {
		struct Data<'l> {
			return_type: Arc<Type>,
			values: HashMap<Arc<str>, Value>,
			compilation_unit: &'l CompilationUnit<'l>,
		}

		impl<'l> BlockRequirements for Data<'l> {
			fn expected_type(&self) -> &Arc<Type> {
				&self.return_type
			}
			fn values(&self) -> &HashMap<Arc<str>, Value> {
				&self.values
			}
			fn values_mut(&mut self) -> &mut HashMap<Arc<str>, Value> {
				&mut self.values
			}
		}

		for (mut builder, decl) in functions {
			let func = builder.as_ref();
			let mut values = HashMap::with_capacity(func.parameters().len());
			for (i, param) in func.parameters().iter().enumerate() {
				values.insert(param.name_arc().clone(), Value::Param(param.ty().clone(), i));
			}

			let mut data = Data {
				values,
				compilation_unit: self,
				return_type: func.return_ty().clone(),
			};

			let block = Block::new(&mut data);
			let func_name = func.name_arc().clone();
			block.compile(&decl.block, &mut builder).with_context(|| format!("Could not compile {:?}", func_name)).unwrap();
			builder.define();
		}
	}
}

impl TypeResolver for CompilationUnit<'_> {
	fn types(&self) -> &HashMap<Arc<str>, Arc<Type>> {
		&self.available_types
	}
}
