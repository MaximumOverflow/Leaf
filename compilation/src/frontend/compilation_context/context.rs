use std::sync::Arc;
use ariadne::{Color, ReportKind, Source};
use fxhash::FxHashMap;
use pathdiff::diff_paths;
use rayon::prelude::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use rust_search::SearchBuilder;
use tracing::debug_span;
use leaf_parsing::ErrMode;
use leaf_reflection::{Assembly, Type, Version};
use leaf_parsing::ast::{CompilationUnit as Ast, Symbol};
use leaf_parsing::lexer::{Token, TokenStream};
use leaf_parsing::parser::{Parse};
use leaf_reflection::heaps::{Heaps};
use crate::frontend::compilation_context::callbacks::CompilationCallbacks;
use crate::frontend::compilation_context::CompilationProgress;
use crate::frontend::compilation_context::config::CompilationConfig;
use crate::frontend::compilation_context::unit::{CompilationUnit};

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

	pub fn compile(mut self) -> Result<&'l Assembly<'l>, Vec<FrontEndError>> {
		for (_name, _dependency) in &self.config.dependencies {
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
		let empty_file: Arc<str> = Arc::from("");
		for file in files {
			match std::fs::read_to_string(&file) {
				Ok(code) => {
					let relative_path: Arc<str> = Arc::from(
						diff_paths(file, &self.config.directories.src_dir)
							.unwrap()
							.to_string_lossy(),
					);
					contents.push((Arc::<str>::from(code), relative_path));
					if let Some(sink) = &self.callbacks.progress_event_sink {
						let _ = sink.send(CompilationProgress::ReadingFiles(file_count));
					}
				},
				Err(err) => {
					let relative_path = diff_paths(file, &self.config.directories.src_dir)
						.unwrap()
						.to_string_lossy()
						.to_string();

					let id: Arc<str> = Arc::from(relative_path);
					errors.push(COULD_NOT_RETRIEVE_SOURCE);

					let cache = Arc::get_mut(&mut report_cache.0).unwrap();
					cache.entry(id.clone()).or_insert_with(|| Source::from(empty_file.clone()));

					let report = FrontEndReport::build(ReportKind::Error, id.clone(), 0)
						.with_message(err.to_string())
						.finish();

					if let Some(sink) = &self.callbacks.report_sink {
						let mut buffer = vec![];
						report.write_for_stdout(report_cache.clone(), &mut buffer).unwrap();
						sink.send((ReportKind::Error, buffer)).unwrap();
					}
				},
			}
		}

		for (code, file) in &contents {
			let cache = Arc::get_mut(&mut report_cache.0).unwrap();
			cache.entry(file.clone()).or_insert_with(|| Source::from(code.clone()));
		}

		let mut tokens = Vec::with_capacity(contents.len());
		for (code, path) in &contents {
			match debug_span!("lex_files").in_scope(|| Token::lex_all(code)) {
				Ok(res) => {
					tokens.push((res, path.clone()));
					if let Some(sink) = &self.callbacks.progress_event_sink {
						let _ = sink.send(CompilationProgress::LexingFiles(file_count));
					}
				},
				Err(err) => {
					errors.push(FrontEndError::from(&err));
					if let Some(sink) = &self.callbacks.report_sink {
						let mut buffer = vec![];
						let report = err.to_report(path.clone());
						report.write_for_stdout(report_cache.clone(), &mut buffer).unwrap();
						sink.send((ReportKind::Error, buffer)).unwrap();
					}
					continue;
				},
			}
		}

		let mut streams: Vec<_> = tokens
			.iter()
			.map(|(tokens, path)| TokenStream::new(path, tokens))
			.collect();

		let mut asts = Vec::with_capacity(streams.len());
		for stream in &mut streams {
			match debug_span!("parse_files").in_scope(|| Ast::parse(stream)) {
				Ok(parsed) => {
					asts.push((parsed, stream.file().clone()));
					if let Some(sink) = &self.callbacks.progress_event_sink {
						let _ = sink.send(CompilationProgress::ParsingFiles(file_count));
					}
				},
				Err(err) => match err {
					ErrMode::Backtrack(err) | ErrMode::Cut(err) => {
						errors.push(FrontEndError::from(&err));
						if let Some(sink) = &self.callbacks.report_sink {
							let mut buffer = vec![];
							let report = err.to_report(stream.file().clone());
							report.write_for_stdout(report_cache.clone(), &mut buffer).unwrap();
							sink.send((ReportKind::Error, buffer)).unwrap();
						}
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
		let symbol_count = asts.iter().flat_map(|a| &a.0.declarations).count();
		for (ast, file) in &asts {
			let report_data = ReportData::new(
				file.clone(),
				report_cache.clone(),
				self.callbacks.report_sink.clone(),
			);
			let ns = self.heaps.blob_heap().intern(ast.namespace);
			let namespace = self.root_namespace.get_or_add_child(ns);
			for decl in &ast.declarations {
				match decl.symbol {
					Symbol::Enum(_) => unimplemented!(),
					Symbol::Struct(_) => {
						let ty = match assembly.create_struct(ast.namespace, decl.name.value) {
							Ok(ty) => ty,
							Err(err) => {
								let mut report = report_data.new_error(decl.name.range.start);
								report.add_label(decl.name.range.clone(), Some(Color::Red), err);
								report.send();
								continue;
							},
						};
						namespace.add_type(ty);
						if let Some(sink) = &self.callbacks.progress_event_sink {
							let _ = sink.send(CompilationProgress::DeclaringSymbols(symbol_count));
						}
					},
					Symbol::Function(_) => {
						let func = match assembly.create_function(ast.namespace, decl.name.value) {
							Ok(ty) => ty,
							Err(err) => {
								let mut report = report_data.new_error(decl.name.range.start);
								report.add_label(decl.name.range.clone(), Some(Color::Red), err);
								report.send();
								continue;
							},
						};
						namespace.add_fn(func);
						if let Some(sink) = &self.callbacks.progress_event_sink {
							let _ = sink.send(CompilationProgress::DeclaringSymbols(symbol_count));
						}
					},
				}
			}
		}
		let mut results = vec![];
		asts.into_par_iter()
			.map(|(ast, file)| {
				let mut unit = CompilationUnit::new(
					ast,
					file.clone(),
					assembly.heaps(),
					&self.root_namespace,
					self.callbacks.clone(),
					symbol_count,
					report_cache.clone(),
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
			return Err(errors);
		}

		self.assemblies.insert((assembly.name(), assembly.version()), assembly);

		if let Some(sink) = &self.callbacks.progress_event_sink {
			let _ = sink.send(CompilationProgress::Finished);
		}

		Ok(assembly)
	}
}
