use std::sync::Arc;
use ariadne::Color;
use fxhash::FxHashMap;

use leaf_parsing::ast::{CompilationUnit as Ast, Symbol};
use leaf_reflection::{Field, Function, Type};
use leaf_reflection::heaps::{BlobHeapScope, HeapScopes, TypeHeap};

use crate::frontend::compilation_context::{CompilationCallbacks, CompilationProgress};
use crate::frontend::reports::{FrontEndError, ReportCache, ReportData};
use crate::frontend::symbols::{Namespace, SymbolResolver, SymbolResolverHint};
use crate::frontend::types::TypeResolver;

pub struct CompilationUnit<'l, 'a> {
	ast: Ast<'l>,
	heaps: HeapScopes<'a>,
	namespace: &'l Namespace<'a>,
	types: FxHashMap<&'a str, &'a Type<'a>>,
	functions: FxHashMap<&'a str, &'a Function<'a>>,

	// Diagnostics
	reports: ReportData,
	total_symbols: usize,
	callbacks: Arc<CompilationCallbacks>,
}

impl<'l, 'a> CompilationUnit<'l, 'a> {
	pub fn new(
		ast: Ast<'l>,
		file: Arc<str>,
		heaps: HeapScopes<'a>,
		root: &'l Namespace<'a>,
		callbacks: Arc<CompilationCallbacks>,
		total_symbols: usize,
		report_cache: ReportCache,
	) -> Self {
		let Some(namespace) = root.get_child(ast.namespace) else {
			unreachable!("Namespace `{}` was not declared.", ast.namespace);
		};
		Self {
			ast,
			heaps,
			reports: ReportData::new(file.clone(), report_cache, callbacks.report_sink.clone()),
			callbacks,
			total_symbols,
			namespace,
			types: namespace.types().clone(),
			functions: namespace.functions().clone(),
		}
	}

	#[tracing::instrument(skip_all)]
	pub fn compile_types(&mut self) -> Result<(), Vec<FrontEndError>> {
		let mut errors = vec![];
		for decl in &self.ast.declarations {
			match &decl.symbol {
				Symbol::Enum(_) => {},
				Symbol::Struct(data) => {
					let Some(Type::Struct(ty)) = self.types.get(decl.name.value) else {
						unreachable!("Type `{}` was not declared.", decl.name.value);
					};
					let mut members = vec![];
					for member in &data.members {
						let ty = match self.resolve_type(&member.ty, &self.reports) {
							Ok(ty) => ty,
							Err((err, mut report)) => {
								report.add_label(
									decl.name.range.clone(),
									Some(Color::Red),
									format! {
										"Could not compile struct type `{}`", decl.name.value
									},
								);
								report.send();
								errors.push(err);
								continue;
							},
						};
						let name = self.heaps.blob_heap().intern(member.name.value).0;
						members.push(Field::new(name, ty));
					}

					// TODO make this a hard error and pre-filter types with no errors
					if let Ok(_) = ty.set_fields(members) {
						self.increment_progress();
					}
				},
				_ => continue,
			}
		}
		match errors.is_empty() {
			true => Ok(()),
			false => Err(errors),
		}
	}

	#[tracing::instrument(skip_all)]
	pub fn compile_functions(&mut self) -> Result<(), Vec<FrontEndError>> {
		let errors = vec![];
		for decl in &self.ast.declarations {
			match &decl.symbol {
				Symbol::Function(_) => {},
				_ => continue,
			}
			self.increment_progress();
		}
		match errors.is_empty() {
			true => Ok(()),
			false => Err(errors),
		}
	}

	fn increment_progress(&self) {
		if let Some(sink) = &self.callbacks.progress_event_sink {
			let _ = sink.send(CompilationProgress::CompilingSymbols(self.total_symbols));
		}
	}
}

impl<'l, 'a> SymbolResolver<'a> for CompilationUnit<'l, 'a> {
	fn get_symbol(
		&'a self,
		name: &str,
		hint: SymbolResolverHint,
	) -> Option<crate::frontend::symbols::Symbol<'a>> {
		match hint {
			SymbolResolverHint::Field => None,
			SymbolResolverHint::Type => self.types.get(name).cloned().map(Into::into),
			SymbolResolverHint::Function => self.functions.get(name).cloned().map(Into::into),
			SymbolResolverHint::Namespace => self.namespace.get_symbol(name, hint).map(Into::into),
			SymbolResolverHint::None => {
				if let Some(symbol) = self.types.get(name) {
					return Some((*symbol).into());
				}
				if let Some(symbol) = self.functions.get(name) {
					return Some((*symbol).into());
				}
				None
			},
		}
	}
}

impl<'l, 'a> TypeResolver<'a> for CompilationUnit<'l, 'a> {
	fn type_heap(&self) -> &TypeHeap<'a> {
		self.heaps.type_heap()
	}

	fn blob_heap(&self) -> &Arc<BlobHeapScope<'a>> {
		self.heaps.blob_heap()
	}

	fn types(&self) -> &FxHashMap<&'a str, &'a Type<'a>> {
		&self.types
	}
}
