use leaf_reflection::{ElementRef, FunctionDef, TypeDef, TypeKind};
use leaf_parsing::ast::{SymbolDeclaration, Function};
use crate::frontend::type_resolver::TypeResolver;
use leaf_parsing::parser::CompilationUnitParser;
use leaf_reflection::builders::MetadataBuilder;
use crate::frontend::block::Block;
use std::collections::HashMap;
use lazy_static::lazy_static;
use anyhow::Error;
use crate::BUG_ERR;

pub struct CompilationUnit<'l> {
	pub(super) metadata_builder: &'l mut MetadataBuilder,

	pub(super) types_vec: Vec<ElementRef<TypeDef>>,
	pub(super) types_map: HashMap<ElementRef<str>, ElementRef<TypeDef>>,

	pub(super) functions_vec: Vec<ElementRef<FunctionDef>>,
	pub(super) functions_map: HashMap<ElementRef<str>, ElementRef<FunctionDef>>,
}

impl<'l> CompilationUnit<'l> {
	pub fn compile(mb: &mut MetadataBuilder, code: &str) -> anyhow::Result<()> {
		lazy_static! {
			static ref PARSER: CompilationUnitParser = CompilationUnitParser::new();
		}

		let root = match PARSER.parse(code) {
			Ok(root) => root,
			Err(err) => return Err(Error::msg(err.to_string())),
		};

		let mut unit = CompilationUnit {
			metadata_builder: mb,
			types_vec: vec![],
			functions_vec: vec![],
			types_map: Default::default(),
			functions_map: Default::default(),
		};

		unit.declare_types(&root);
		unit.declare_functions(&root);
		unit.define_types(&root)?;
		unit.define_functions(&root)?;

		Ok(())
	}

	fn declare_types(&mut self, root: &leaf_parsing::ast::CompilationUnit) {
		for decl in &root.declarations {
			match decl {
				| SymbolDeclaration::Alias(_) | SymbolDeclaration::Function(_) => {},
				_ => {
					self.declare_type(decl, root.namespace);
				},
			}
		}
	}

	fn define_types(&mut self, root: &leaf_parsing::ast::CompilationUnit) -> anyhow::Result<()> {
		let types = self.types_vec.clone();
		let mut types = types.iter();

		for decl in &root.declarations {
			match decl {
				| SymbolDeclaration::Alias(_) 
				| SymbolDeclaration::Function(_) => {},
				_ => {
					let declared_ty = *types.next().expect(BUG_ERR);
					self.define_type(declared_ty, decl)?
				},
			}
		}

		Ok(())
	}

	fn declare_type(&mut self, decl: &SymbolDeclaration, namespace: &str) -> ElementRef<TypeDef> {
		let ty = match decl {
			SymbolDeclaration::Enum(decl) => self.metadata_builder.declare_type(
				TypeKind::Enum,
				decl.id.unwrap_or_default(),
				namespace,
			),
			SymbolDeclaration::Union(decl) => self.metadata_builder.declare_type(
				TypeKind::Union,
				decl.id.unwrap_or_default(),
				namespace,
			),
			SymbolDeclaration::Struct(decl) => self.metadata_builder.declare_type(
				TypeKind::Struct,
				decl.id.unwrap_or_default(),
				namespace,
			),
			SymbolDeclaration::Interface(decl) => self.metadata_builder.declare_type(
				TypeKind::Interface,
				decl.id.unwrap_or_default(),
				namespace,
			),
			_ => unreachable!("{}", BUG_ERR),
		};

		let name = self.metadata_builder.get_type(ty).unwrap().name();
		self.types_vec.push(ty);
		self.types_map.insert(name, ty);
		ty
	}

	fn define_type(
		&mut self, ty: ElementRef<TypeDef>, decl: &SymbolDeclaration,
	) -> anyhow::Result<()> {
		match decl {
			SymbolDeclaration::Enum(_decl) => unimplemented!(),
			SymbolDeclaration::Union(_decl) => unimplemented!(),
			SymbolDeclaration::Struct(decl) => {
				let mut fields = Vec::with_capacity(decl.members.len());
				for member in &decl.members {
					let signature = self.create_type_signature(&member.ty)?;
					fields.push(self.metadata_builder.create_field(member.id, &signature));
				}
				self.metadata_builder.set_fields(ty, fields);
			},
			SymbolDeclaration::Interface(_decl) => unimplemented!(),
			_ => unreachable!("{}", BUG_ERR),
		}

		Ok(())
	}

	fn declare_functions(&mut self, root: &leaf_parsing::ast::CompilationUnit) {
		for decl in &root.declarations {
			let SymbolDeclaration::Function(decl) = decl else {
				continue;
			};

			self.declare_function(decl, root.namespace);
		}
	}

	fn define_functions(
		&mut self, root: &leaf_parsing::ast::CompilationUnit,
	) -> anyhow::Result<()> {
		let functions = self.functions_vec.clone();
		let mut functions = functions.iter();

		for decl in &root.declarations {
			let SymbolDeclaration::Function(decl) = decl else {
				continue;
			};

			let declared_fn = *functions.next().expect(BUG_ERR);
			self.define_function(declared_fn, decl)?;
		}

		Ok(())
	}

	fn declare_function(&mut self, decl: &Function, namespace: &str) {
		let sig = &decl.signature;
		let func = self
			.metadata_builder
			.declare_function(sig.id.unwrap_or_default(), namespace);
		let name = self.metadata_builder.get_fn(func).unwrap().name().into();
		self.functions_vec.push(func);
		self.functions_map.insert(name, func);
	}

	fn define_function(
		&mut self, func: ElementRef<FunctionDef>, decl: &Function,
	) -> anyhow::Result<()> {
		let mut params = vec![];
		for (name, ty) in &decl.signature.par {
			let ty = self.create_type_signature(ty)?;
			params.push(self.metadata_builder.create_parameter(name, &ty));
		}

		let ret_ty = self.create_type_signature(&decl.signature.ret_ty)?;
		self.metadata_builder.set_parameters(func, params);
		self.metadata_builder.set_return_type(func, &ret_ty);

		let locals = Block::generate_local_signatures(&decl.block, self)?;
		let opcodes = Block::generate_opcodes(&decl.block, self)?;
		println!("{:?}", opcodes);

		self.metadata_builder.set_locals(func, &locals);
		self.metadata_builder.set_instructions(func, &opcodes);

		Ok(())
	}
}

impl TypeResolver for CompilationUnit<'_> {
	fn metadata_builder(&self) -> &MetadataBuilder {
		&self.metadata_builder
	}

	fn types(&self) -> &HashMap<ElementRef<str>, ElementRef<TypeDef>> {
		&self.types_map
	}
}
