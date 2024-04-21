use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use anyhow::{anyhow, Error};
use fxhash::FxHashMap;

use leaf_parsing::ast::{Symbol, CompilationUnit as Ast, Function as FunctionAst, Struct as StructAst};
use leaf_reflection::{Assembly, Field, Function, Parameter, SSAContextBuilder, Type};
use tracing::{debug, debug_span, error, info, instrument, Level, span, trace};
use leaf_parsing::ErrMode;
use leaf_parsing::parser::{Parse, Token, TokenStream};
use leaf_reflection::heaps::{BlobHeapScope, HeapScopes};
use crate::frontend::block::Block;
use crate::frontend::types::{TypeCache, TypeResolver};

pub struct CompilationUnit<'a, 'l> {
	heaps: HeapScopes<'l>,
	type_cache: &'a TypeCache<'l>,
	assembly: &'a mut Assembly<'l>,
	types: FxHashMap<&'l str, &'l Type<'l>>,
	functions: FxHashMap<&'l str, &'l Function<'l>>,
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

		debug_span!("compile_file", file = absolute_path).in_scope(|| {
			info!("Compiling file `{}`", absolute_path);
			let code = debug_span!("read_file").in_scope(|| std::fs::read_to_string(path))?;
			Self::compile_internal(type_cache, assembly, &code, &absolute_path)?;
			debug!("File `{}` compiled successfully", absolute_path);
			Ok(())
		})
	}

	pub fn compile_code(
		type_cache: &'a TypeCache<'l>,
		assembly: &'a mut Assembly<'l>,
		code: &str,
	) -> anyhow::Result<()> {
		debug_span!("compile_code").in_scope(|| {
			info!("Compiling code");
			Self::compile_internal(type_cache, assembly, code, "<dynamic_code>")?;
			debug!("Code compiled successfully");
			Ok(())
		})
	}

	fn compile_internal(
		type_cache: &'a TypeCache<'l>,
		assembly: &'a mut Assembly<'l>,
		code: &str,
		file: &str,
	) -> anyhow::Result<()> {
		let span = debug_span!("parse_code");
		let parse_code_span = span.enter();

		let span = debug_span!("lex");
		let lex_span = span.enter();
		debug!("Lexing tokens");
		let tokens = match Token::lex_all(code) {
			Ok(tokens) => tokens,
			Err(err) => {
				let mut str = vec![];
				err.to_report(file)
					.write((file, ariadne::Source::from(code)), &mut str)
					.unwrap();
				return Err(Error::msg(String::from_utf8(str).unwrap()));
			},
		};
		let mut stream = TokenStream::new(file, &tokens);
		drop(lex_span);

		let span = debug_span!("parse");
		let parse_span = span.enter();
		debug!("Parsing AST");
		let ast = match Ast::parse(&mut stream) {
			Ok(tokens) => tokens,
			Err(ErrMode::Cut(err) | ErrMode::Backtrack(err)) => {
				let mut str = vec![];
				err.to_report(file)
					.write((file, ariadne::Source::from(code)), &mut str)
					.unwrap();
				return Err(Error::msg(String::from_utf8(str).unwrap()));
			},
			_ => return Err(anyhow!("Unknown parser error")),
		};
		drop(parse_span);
		drop(parse_code_span);

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
			types: HashMap::default(),
			functions: HashMap::default(),
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

					let ty = self.assembly.create_struct(ast.namespace, name).unwrap();
					let Type::Struct(s) = ty else { unreachable!() };
					self.types.insert(s.name(), ty);
					symbols.structs.push((ty, decl));
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

			let span = span!(Level::DEBUG, "create_function", id = func.name());
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
				let Ok(_) = func.set_body(None) else {
					unreachable!()
				};
				debug!("External function `{}` declared successfully", func.id());
			}
		}

		Ok(symbols)
	}

	fn compile_types(&mut self, types: Vec<(&'l Type<'l>, &StructAst)>) -> anyhow::Result<()> {
		for (ty, decl) in types {
			let span = span!(Level::DEBUG, "compile_type", name = ty.id().name());
			let _span = span.enter();
			debug!("Compiling type `{}`", ty.id());

			match *ty {
				Type::Struct(ty) => {
					let mut members = vec![];
					for member in &decl.members {
						let ty = self.resolve_type(&member.ty)?;
						let name = self.heaps.blob_heap().intern(member.name).0;
						members.push(Field::new(name, ty));
					}
					ty.set_fields(members).unwrap();
				},
				_ => unreachable!(),
			}
			debug!("Type `{}` compiled successfully", ty.id());
		}
		Ok(())
	}

	fn compile_functions(
		&mut self,
		funcs: Vec<(&'l Function<'l>, &FunctionAst)>,
	) -> anyhow::Result<()> {
		for (func, decl) in funcs {
			let span = span!(Level::DEBUG, "compile_function", id = func.name());
			let _span = span.enter();
			debug!("Compiling function `{}`", func.id());

			let mut body = SSAContextBuilder::new(self.heaps.clone());
			let mut block = Block {
				func,
				heaps: self.heaps.clone(),
				type_cache: self.type_cache,
				values: HashMap::default(),
				types: self.types(),
				functions: &self.functions,
			};

			for i in func.params() {
				let idx = body.alloca(i.ty());
				block.values.insert(i.name(), (idx, false));
			}

			block.compile(decl.block.as_ref().unwrap(), &mut body)?;

			let Ok(_) = func.set_body(Some(body.build())) else {
				unreachable!()
			};
			debug!("Function `{}` compiled successfully", func.id());
		}
		Ok(())
	}
}

impl<'l> TypeResolver<'l> for CompilationUnit<'_, 'l> {
	fn type_cache(&self) -> &TypeCache<'l> {
		self.type_cache
	}

	fn blob_heap(&self) -> &Arc<BlobHeapScope<'l>> {
		self.heaps.blob_heap()
	}

	fn types(&self) -> &FxHashMap<&'l str, &'l Type<'l>> {
		&self.types
	}
}

#[derive(Default)]
struct SymbolDeclarations<'l, 'a> {
	structs: Vec<(&'l Type<'l>, &'a StructAst<'a>)>,
	functions: Vec<(&'l Function<'l>, &'a FunctionAst<'a>)>,
}
