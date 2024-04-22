use std::collections::HashMap;
use fxhash::FxHashMap;
use std::path::Path;
use std::sync::Arc;

use leaf_parsing::ast::{Symbol, CompilationUnit as Ast, Function as FunctionAst, Struct as StructAst};
use leaf_reflection::{Assembly, Field, Function, Parameter, SSAContextBuilder, Type};
use tracing::{debug, debug_span, error, info, instrument, Level, span, trace};
use leaf_parsing::ErrMode;
use leaf_parsing::lexer::{Token, TokenStream};
use leaf_parsing::parser::Parse;
use leaf_reflection::heaps::{BlobHeapScope, HeapScopes};
use crate::frontend::block::Block;
use crate::frontend::reports::*;
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
	) -> Result<(), FrontEndError> {
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
			let code = debug_span!("read_file").in_scope(|| {
				std::fs::read_to_string(path).map_err(|_| COULD_NOT_RETRIEVE_SOURCE)
			})?;
			Self::compile_internal(type_cache, assembly, &code, &absolute_path)?;
			debug!("File `{}` compiled successfully", absolute_path);
			Ok(())
		})
	}

	pub fn compile_code(
		type_cache: &'a TypeCache<'l>,
		assembly: &'a mut Assembly<'l>,
		code: &str,
	) -> Result<(), FrontEndError> {
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
	) -> Result<(), FrontEndError> {
		let span = debug_span!("parse_code");
		let parse_code_span = span.enter();

		let span = debug_span!("lex");
		let lex_span = span.enter();
		debug!("Lexing tokens");
		let tokens = match Token::lex_all(code) {
			Ok(tokens) => tokens,
			Err(err) => {
				dump_report(err.to_report(file), file, code);
				return Err(LEXER_ERROR);
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
				dump_report(err.to_report(file), file, code);
				return Err(PARSER_ERROR);
			},
			_ => return Err(PARSER_ERROR),
		};
		drop(parse_span);
		drop(parse_code_span);

		let mut reports = ReportData::new(file);

		let mut unit = Self::new(type_cache, assembly);
		let symbols = match unit.declare_symbols(&ast, &mut reports) {
			Ok(symbols) => symbols,
			Err(err) => {
				generate_and_dump_report(reports.errors, file, code, err);
				return Err(err);
			},
		};
		if let Err(err) = unit.compile_types(symbols.structs, &mut reports) {
			generate_and_dump_report(reports.errors, file, code, err);
			return Err(err);
		}
		if let Err(err) = unit.compile_functions(symbols.functions, &mut reports) {
			generate_and_dump_report(reports.errors, file, code, err);
			return Err(err);
		}
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
		reports: &mut ReportData,
	) -> Result<SymbolDeclarations<'l, 'c>, FrontEndError> {
		let mut symbols = SymbolDeclarations::default();
		for decl in &ast.declarations {
			let name = decl.name.value;
			match &decl.symbol {
				Symbol::Struct(struct_decl) => {
					let span = span!(Level::DEBUG, "create_struct", name);
					let _span = span.enter();
					debug!("Creating struct");

					if self.types.contains_key(name) {
						error!("Duplicate declaration");
						reports.add_error_label(
							decl.name.range.clone(),
							format! {
								"Duplicate declaration of `{}::{}`",
								ast.namespace, name
							},
						);
						return Err(DUPLICATE_DECLARATION);
					}

					let ty = match self.assembly.create_struct(ast.namespace, name) {
						Ok(func) => func,
						Err(err) => {
							error!("{}", err);
							reports.add_error_label(decl.name.range.clone(), err);
							return Err(DECLARATION_ERROR);
						},
					};

					let Type::Struct(s) = ty else { unreachable!() };
					self.types.insert(s.name(), ty);
					symbols.structs.push((ty, struct_decl));
				},
				_ => {},
			}
		}

		for decl in &ast.declarations {
			let name = decl.name.value;
			let Symbol::Function(func_decl) = &decl.symbol else {
				continue;
			};

			if self.functions.contains_key(name) {
				error!("Duplicate declaration");
				reports.add_error_label(
					decl.name.range.clone(),
					format! {
						"Duplicate declaration of `{}::{}`",
						ast.namespace, name
					},
				);
				return Err(DUPLICATE_DECLARATION);
			}

			let func = match self.assembly.create_function(ast.namespace, name) {
				Ok(func) => func,
				Err(err) => {
					error!("{}", err);
					reports.add_error_label(decl.name.range.clone(), err);
					return Err(DECLARATION_ERROR);
				},
			};

			let span = span!(Level::DEBUG, "create_function", id = func.name());
			let _span = span.enter();
			debug!("Creating function");

			trace!("Resolving return type");
			let ret_ty = self.resolve_type(&func_decl.return_ty, reports)?;
			func.set_return_type(ret_ty).unwrap();

			trace!("Resolving parameter types");
			let mut params = Vec::with_capacity(func_decl.params.len());
			for p in &func_decl.params {
				let name = self.heaps.blob_heap().intern(p.name.value).0;
				params.push(Parameter::new(name, self.resolve_type(&p.ty, reports)?));
			}
			func.set_params(params).unwrap();

			self.functions.insert(func.name(), func);

			if func_decl.block.is_some() {
				symbols.functions.push((func, func_decl));
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

	fn compile_types(
		&mut self,
		types: Vec<(&'l Type<'l>, &StructAst)>,
		reports: &mut ReportData,
	) -> Result<(), FrontEndError> {
		for (ty, decl) in types {
			let span = span!(Level::DEBUG, "compile_type", name = ty.id().name());
			let _span = span.enter();
			debug!("Compiling type `{}`", ty.id());

			match *ty {
				Type::Struct(ty) => {
					let mut members = vec![];
					for member in &decl.members {
						let ty = self.resolve_type(&member.ty, reports)?;
						let name = self.heaps.blob_heap().intern(member.name.value).0;
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
		reports: &mut ReportData,
	) -> Result<(), FrontEndError> {
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

			block.compile(decl.block.as_ref().unwrap(), &mut body, reports)?;

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
