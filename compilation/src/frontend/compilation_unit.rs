use leaf_reflection::{MetadataWrite, TypeKind, TypeSignatureTag};
use leaf_parsing::ast::{SymbolDeclaration, Function};
use leaf_parsing::parser::CompilationUnitParser;
use leaf_reflection::builders::MetadataBuilder;
use std::sync::{Arc, OnceLock};
use std::collections::HashMap;
use std::io::Cursor;
use anyhow::{Context, Error};
use crate::BUG_ERR;
use crate::frontend::block::{Block, BlockRequirements};
use crate::frontend::expressions::Value;

use crate::frontend::functions::{FunctionDef, Parameter};
use crate::frontend::types::{Field, StructType, Type, TypeDef, TypeResolver, TypeSignatureBlob, TypeSignatureBuilder};

static EMPTY_STR: OnceLock<Arc<str>> = OnceLock::new();

pub struct CompilationUnit<'l> {
	namespace: Arc<str>,
	metadata_builder: &'l mut MetadataBuilder,

	defined_types: Vec<TypeDef>,
	available_types: HashMap<Arc<str>, Type>,

	defined_functions: Vec<FunctionDef>,
}

impl<'l> CompilationUnit<'l> {
	pub fn new(metadata_builder: &'l mut MetadataBuilder, code: &str) -> anyhow::Result<Self> {
		static PARSER: OnceLock<CompilationUnitParser> = OnceLock::new();
		let parser = PARSER.get_or_init(CompilationUnitParser::new);

		let root = match parser.parse(code) {
			Ok(root) => root,
			Err(err) => return Err(Error::msg(err.to_string())),
		};

		let mut unit = Self {
			metadata_builder,
			defined_types: vec![],
			defined_functions: vec![],
			available_types: Default::default(),
			namespace: Arc::from(root.namespace),
		};

		let types = unit.declare_types(&root)?;
		let functions = unit.declare_functions(&root)?;
		unit.define_types(&types)?;
		unit.define_functions(&functions)?;

		Ok(unit)
	}

	pub fn defined_types(&self) -> &[TypeDef] {
		&self.defined_types
	}

	pub fn defined_functions(&self) -> &[FunctionDef] {
		&self.defined_functions
	}

	fn declare_types<'a>(&mut self, root: &'a leaf_parsing::ast::CompilationUnit<'a>) -> anyhow::Result<Vec<&'a SymbolDeclaration<'a>>> {
		let mut decls = vec![];
		for decl in &root.declarations {
			match decl {
				SymbolDeclaration::Struct(_) => {
					let ty = self.declare_type(decl, root.namespace);
					self.defined_types.push(ty.clone());
					self.available_types.insert(ty.name().clone(), ty.into());
					decls.push(decl);
				},
				_ => {}
			}
		}
		Ok(decls)
	}

	fn define_types(&mut self, types: &[&SymbolDeclaration]) -> anyhow::Result<()> {
		for (i, decl) in types.iter().enumerate() {
			match decl {
				SymbolDeclaration::Struct(s_decl) => {
					let mut members = Vec::with_capacity(s_decl.members.len());

					for member in &s_decl.members {
						let name: Arc<str> = Arc::from(member.id);
						let ty = self.resolve_type(&member.ty)?;
						members.push(Field { name, r#type: ty });
					}

					let mut members_metadata = Vec::with_capacity(s_decl.members.len());
					for member in &members {
						let signature = self.get_or_create_type_signature(&member.r#type);
						let metadata = self.metadata_builder.create_field(&member.name, signature);
						members_metadata.push(metadata);
					}

					let TypeDef::Struct(ty) = &self.defined_types[i] else {
						unreachable!("{}", BUG_ERR);
					};
					ty.fields.set(members).unwrap();
					self.metadata_builder.set_fields(ty.metadata, members_metadata);
				}
				_ => {}
			}
		}
		Ok(())
	}

	fn declare_type(&mut self, decl: &SymbolDeclaration, namespace: &str) -> TypeDef {
		match decl {
			SymbolDeclaration::Struct(decl) => {
				let name: Arc<str> = match decl.id {
					Some(id) => Arc::from(id),
					None => EMPTY_STR.get_or_init(|| Arc::from("")).clone(),
				};
				let metadata = self.metadata_builder.declare_type(
					TypeKind::Struct, &name, namespace
				);

				let ty = StructType {
					metadata,
					name: name.clone(),
					fields: Default::default(),
					namespace: self.namespace.clone(),
				};

				let type_def = TypeDef::Struct(Arc::new(ty));
				self.available_types.insert(name, (&type_def).into());
				type_def
			}
			_ => unimplemented!(),
		}
	}

	fn declare_functions<'a>(&mut self, root: &'a leaf_parsing::ast::CompilationUnit<'a>) -> anyhow::Result<Vec<&'a Function<'a>>> {
		let mut decls = vec![];
		for decl in &root.declarations {
			match decl {
				SymbolDeclaration::Function(decl) => {
					self.declare_function(decl, root.namespace)?;
					decls.push(decl);
				},
				_ => {}
			}
		}
		Ok(decls)
	}

	fn define_functions(&mut self, functions: &[&Function]) -> anyhow::Result<()> {
		let defined_functions = std::mem::take(&mut self.defined_functions);
		for (i, decl) in functions.iter().enumerate() {
			let function = &defined_functions[i];
			self.define_function(function, decl)?;
		}
		self.defined_functions = defined_functions;
		Ok(())
	}

	fn declare_function(&mut self, decl: &Function, namespace: &str) -> anyhow::Result<()> {
		let name: Arc<str> = match decl.signature.id {
			Some(id) => Arc::from(id),
			None => EMPTY_STR.get_or_init(|| Arc::from("")).clone(),
		};

		let err = || format!("Could not compile {:?}", name);

		let return_ty = self.resolve_type(&decl.signature.ret_ty).with_context(err)?;
		let mut parameters = HashMap::with_capacity(decl.signature.par.len());
		for (i, (name, ty)) in decl.signature.par.iter().enumerate() {
			let name: Arc<str> = Arc::from(*name);
			let ty = self.resolve_type(ty).with_context(err)?;
			parameters.insert(name.clone(), Parameter { index: i, r#type: ty, name });
		}

		let metadata = self.metadata_builder.declare_function(&name, namespace);
		{
			let parameters: Vec<_> = parameters.values().map(|i| {
				let ty = self.get_or_create_type_signature(&i.r#type);
				self.metadata_builder.create_parameter(&i.name, ty)
			}).collect();
			let return_ty_signature = self.get_or_create_type_signature(&return_ty);
			self.metadata_builder.set_return_type(metadata, return_ty_signature.as_ref());
			self.metadata_builder.set_parameters(metadata, parameters);
		}

		let func = FunctionDef {
			name,
			metadata,
			return_ty,
			parameters,
			locals: Default::default(),
			instructions: Default::default(),
			namespace: self.namespace.clone(),
		};
		self.defined_functions.push(func);
		Ok(())
	}

	fn define_function(&mut self, func: &FunctionDef, decl: &Function) -> anyhow::Result<()> {
		struct Data<'l> {
			return_type: Type,
			values: HashMap<Arc<str>, Value>,
			compilation_unit: &'l CompilationUnit<'l>,
		}

		impl<'l> BlockRequirements for Data<'l> {
			fn expected_type(&self) -> Type {
				self.return_type.clone()
			}
			fn values(&self) -> &HashMap<Arc<str>, Value> {
				&self.values
			}
			fn values_mut(&mut self) -> &mut HashMap<Arc<str>, Value> {
				&mut self.values
			}
		}

		let mut values = HashMap::with_capacity(func.parameters.len());
		for (i, param) in func.parameters.values().enumerate() {
			values.insert(param.name.clone(), Value::Param(param.r#type.clone(), i));
		}

		let mut data = Data {
			values,
			compilation_unit: self,
			return_type: func.return_ty.clone(),
		};

		let mut opcodes = vec![];
		let block = Block::new(&mut data);
		block.compile(&decl.block, &mut opcodes).with_context(|| format!("Could not compile {:?}", func.name))?;
		let instructions = self.metadata_builder.set_instructions(func.metadata, &opcodes);
		func.instructions.set(instructions).unwrap();
		Ok(())
	}
}

impl TypeResolver for CompilationUnit<'_> {
	type Type = Type;
	fn types(&self) -> &HashMap<Arc<str>, Self::Type> {
		&self.available_types
	}
}

impl TypeSignatureBuilder for CompilationUnit<'_> {
	fn get_or_create_type_signature(&mut self, ty: &Type) -> TypeSignatureBlob {
		match ty {
			Type::Void => {
				TypeSignatureBlob::Static(&[TypeSignatureTag::Void as u8])
			}
			Type::Decimal(size) => {
				match *size {
					2 => TypeSignatureBlob::Static(&[TypeSignatureTag::Decimal16 as u8]),
					4 => TypeSignatureBlob::Static(&[TypeSignatureTag::Decimal32 as u8]),
					8 => TypeSignatureBlob::Static(&[TypeSignatureTag::Decimal64 as u8]),
					_ => unreachable!(),
				}
			}
			Type::Integer(size) => {
				match *size {
					1 => TypeSignatureBlob::Static(&[TypeSignatureTag::Integer8 as u8]),
					2 => TypeSignatureBlob::Static(&[TypeSignatureTag::Integer16 as u8]),
					4 => TypeSignatureBlob::Static(&[TypeSignatureTag::Integer32 as u8]),
					8 => TypeSignatureBlob::Static(&[TypeSignatureTag::Integer64 as u8]),
					_ => unreachable!(),
				}
			}
			Type::UInteger(size) => {
				match *size {
					1 => TypeSignatureBlob::Static(&[TypeSignatureTag::UInteger8 as u8]),
					2 => TypeSignatureBlob::Static(&[TypeSignatureTag::UInteger16 as u8]),
					4 => TypeSignatureBlob::Static(&[TypeSignatureTag::UInteger32 as u8]),
					8 => TypeSignatureBlob::Static(&[TypeSignatureTag::UInteger64 as u8]),
					_ => unreachable!(),
				}
			}
			Type::Struct(data) => {
				let data = data.upgrade().unwrap();

				let mut buffer = [0u8; std::mem::size_of::<usize>()];
				let mut cursor = Cursor::new(buffer.as_mut_slice());
				TypeSignatureTag::TypeDef.write(&mut cursor).unwrap();
				data.metadata.write(&mut cursor).unwrap();

				let len = cursor.position() as usize;
				TypeSignatureBlob::Small(buffer, len)
			}
		}
	}
}
