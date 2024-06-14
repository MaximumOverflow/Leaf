use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use fxhash::FxHashMap;
use crate::frontend::compilation_context::{CompilationCallbacks, CompilationProgress};
use leaf_parsing::ast::{CompilationUnit as Ast, Symbol};
use leaf_reflection::heaps::{BlobHeapScope, Heaps, TypeHeap};
use leaf_reflection::Type;
use crate::frontend::reports::FrontEndReport;
use crate::frontend::symbols::Namespace;
use crate::frontend::types::TypeResolver;

pub struct CompilationUnit<'l, 'a> {
	ast: Ast<'l>,
	heaps: &'a Heaps<'a>,
	progress: Arc<ProgressInfo>,
	callbacks: Arc<CompilationCallbacks>,
	types: FxHashMap<&'a str, &'a Type<'a>>,
}

pub struct ProgressInfo {
	pub total_symbols: usize,
	pub compiled_symbols: AtomicUsize,
}

impl<'l, 'a> CompilationUnit<'l, 'a> {
	pub fn new(
		ast: Ast<'l>,
		heaps: &'a Heaps<'a>,
		namespace: &'l Namespace<'a>,
		callbacks: Arc<CompilationCallbacks>,
		progress: Arc<ProgressInfo>,
	) -> Self {
		Self {
			ast,
			heaps,
			callbacks,
			progress,
			types: namespace.types().clone(),
		}
	}

	#[tracing::instrument(skip_all)]
	pub fn compile_types(&mut self) -> Result<(), Vec<FrontEndReport>> {
		for decl in &self.ast.declarations {
			match &decl.symbol {
				Symbol::Enum(_) => {},
				Symbol::Struct(_) => {},
				_ => continue,
			}
			self.increment_progress();
		}
		Ok(())
	}

	#[tracing::instrument(skip_all)]
	pub fn compile_functions(&mut self) -> Result<(), Vec<FrontEndReport>> {
		for decl in &self.ast.declarations {
			match &decl.symbol {
				Symbol::Function(_) => {},
				_ => continue,
			}
			self.increment_progress();
		}
		Ok(())
	}

	fn increment_progress(&self) {
		if let Some(sink) = &self.callbacks.progress_events_sink {
			let i = self.progress.compiled_symbols.fetch_add(1, Ordering::AcqRel);
			let _ = sink.send(CompilationProgress::CompilingSymbols(
				i,
				self.progress.total_symbols,
			));
		}
	}
}

impl<'l, 'a> TypeResolver<'a> for CompilationUnit<'l, 'a> {
	fn type_heap(&self) -> &TypeHeap<'a> {
		todo!()
	}

	fn blob_heap(&self) -> &Arc<BlobHeapScope<'a>> {
		todo!()
	}

	fn types(&self) -> &FxHashMap<&'a str, &'a Type<'a>> {
		todo!()
	}
}
