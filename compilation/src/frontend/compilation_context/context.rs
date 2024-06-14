use std::path::Path;
use std::sync::Arc;
use ariadne::{Label, ReportKind, Source};
use fxhash::FxHashMap;
use rayon::prelude::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use rust_search::SearchBuilder;
use tracing::{debug_span, error};
use leaf_parsing::ErrMode;
use leaf_reflection::{Assembly, Version};
use leaf_parsing::ast::{CompilationUnit as Ast, Symbol};
use leaf_parsing::lexer::{Token, TokenData, TokenStream};
use leaf_parsing::parser::{Parse, ParserError};
use leaf_reflection::heaps::{ArenaAllocator, Heaps};
use crate::frontend::compilation_context::callbacks::CompilationCallbacks;
use crate::frontend::compilation_context::CompilationProgress;
use crate::frontend::compilation_context::config::CompilationConfig;
use crate::frontend::compilation_context::unit::{CompilationUnit, ProgressInfo};

use crate::frontend::reports::*;
use crate::frontend::symbols::Namespace;

pub struct CompilationContext<'l> {
	heaps: &'l Heaps<'l>,
	config: CompilationConfig,
	root_namespace: Namespace<'l>,
	assemblies: FxHashMap<(&'l str, Version), &'l Assembly<'l>>,
	callbacks: Arc<CompilationCallbacks>,
}

impl<'l> CompilationContext<'l> {
	pub fn new(config: CompilationConfig, heaps: &'l Heaps<'l>) -> Self {
		Self::new_with_callbacks(config, heaps, Default::default())
	}

	pub fn new_with_callbacks(
		config: CompilationConfig,
		heaps: &'l Heaps<'l>,
		callbacks: CompilationCallbacks,
	) -> Self {
		Self {
			config,
			heaps,
			assemblies: Default::default(),
			root_namespace: Namespace::new(""),
			callbacks: Arc::new(callbacks),
		}
	}

	pub fn compile(mut self) -> Result<&'l Assembly<'l>, Vec<u8>> {
		for (name, dependency) in &self.config.dependencies {
			unimplemented!("Dependencies are not implemented");
		}

		let files: Vec<_> = SearchBuilder::default()
			.location(&self.config.directories.src_dir)
			.ext("leaf")
			.build()
			.collect();

		let mut contents = vec![];
		let mut errors = vec![];
		let mut report_cache = ReportCache::default();

		let file_count = files.len();
		for (i, file) in files.into_iter().enumerate() {
			if let Some(sink) = &self.callbacks.progress_events_sink {
				let _ = sink.send(CompilationProgress::ParsingFiles(i, file_count));
			}

			match std::fs::read_to_string(&file) {
				Ok(code) => contents.push((code, file)),
				Err(err) => {
					let id: Arc<str> = Arc::from(file.as_str());
					let mut report = FrontEndReport::build(ReportKind::Error, id.clone(), 0);
					report = report.with_message(err.to_string());
					errors.push(report.finish());

					report_cache.0.entry(id.clone()).or_insert_with(|| Source::from(""));
				},
			}
		}

		for (code, file) in &contents {
			let id: Arc<str> = Arc::from(file.as_str());
			report_cache
				.0
				.entry(id.clone())
				.or_insert_with(|| Source::from(code.as_str()));
		}

		let mut tokens = Vec::with_capacity(contents.len());
		for (i, (code, path)) in contents.iter().enumerate() {
			if let Some(sink) = &self.callbacks.progress_events_sink {
				let _ = sink.send(CompilationProgress::ParsingFiles(i, file_count));
			}
			match debug_span!("lex_files").in_scope(|| Token::lex_all(code)) {
				Ok(res) => tokens.push((res, path.clone())),
				Err(err) => {
					errors.push(err.to_report(Arc::from(path.as_str())));
					continue;
				},
			}
		}

		let mut streams: Vec<_> = tokens
			.iter()
			.map(|(tokens, path)| TokenStream::new(path, tokens))
			.collect();

		let mut asts = Vec::with_capacity(streams.len());
		for (i, stream) in streams.iter_mut().enumerate() {
			if let Some(sink) = &self.callbacks.progress_events_sink {
				let _ = sink.send(CompilationProgress::ParsingFiles(i, file_count));
			}
			match debug_span!("parse_files").in_scope(|| Ast::parse(stream)) {
				Ok(parsed) => asts.push(parsed),
				Err(err) => match err {
					ErrMode::Backtrack(err) | ErrMode::Cut(err) => {
						errors.push(err.to_report(stream.file().clone()))
					},
					_ => unreachable!(),
				},
			};
		}

		let assembly = self.heaps.general_purpose_heap().alloc(Assembly::new(
			&self.config.package.name,
			self.config.package.version.into(),
			&self.heaps,
		));

		// Recursively instantiate namespaces and declare symbols
		let mut symbol_i = 0..;
		let symbol_count = asts.iter().flat_map(|a| &a.declarations).count();
		for ast in &asts {
			let ns = self.heaps.blob_heap().intern(ast.namespace);
			let namespace = self.root_namespace.get_or_add_child(ns);
			for decl in &ast.declarations {
				let i = symbol_i.next().unwrap();
				if let Some(sink) = &self.callbacks.progress_events_sink {
					let _ = sink.send(CompilationProgress::DeclaringSymbols(i, symbol_count));
				}
				match decl.symbol {
					Symbol::Enum(_) => unimplemented!(),
					Symbol::Struct(_) => {
						let ty = assembly.create_struct(ast.namespace, decl.name.value).unwrap();
						namespace.add_type(ty);
					},
					Symbol::Function(_) => {
						let func =
							assembly.create_function(ast.namespace, decl.name.value).unwrap();
						namespace.add_fn(func);
					},
				}
			}
		}

		let progress_info = Arc::new(ProgressInfo {
			total_symbols: symbol_count,
			compiled_symbols: Default::default(),
		});

		let mut results = vec![];
		asts.into_par_iter()
			.map(|ast| {
				let mut unit = CompilationUnit::new(
					ast,
					self.heaps,
					&self.root_namespace,
					self.callbacks.clone(),
					progress_info.clone(),
				);
				if let Err(err) = unit.compile_types() {
					return Err(err);
				}
				if let Err(err) = unit.compile_functions() {
					return Err(err);
				}
				Ok(())
			})
			.collect_into_vec(&mut results);

		if !errors.is_empty() {
			let mut report = vec![];
			for err in errors {
				err.write_for_stdout(&mut report_cache, &mut report).unwrap();
			}
			return Err(report);
		}

		self.assemblies.insert((assembly.name(), assembly.version()), assembly);

		if let Some(sink) = &self.callbacks.progress_events_sink {
			let _ = sink.send(CompilationProgress::Finished);
		}

		Ok(assembly)
	}
}
