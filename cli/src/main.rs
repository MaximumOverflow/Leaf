#![allow(unused_imports)]

use std::alloc::Layout;
use std::collections::HashMap;
use std::ffi::c_char;
use std::fs::File;
use std::io::{BufWriter, Cursor, Write};
use std::ops::Range;
use std::path::PathBuf;
use std::process::Stdio;
use std::sync::Arc;
use std::time::SystemTime;

use clap::Parser;
use indicatif::{ProgressBar, ProgressState, ProgressStyle};
use tracing::{debug, info, trace, Level};
use tracing_flame::FlameLayer;
use tracing_subscriber::fmt::writer::MakeWriterExt;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::Registry;
use leaf_compilation::backends::CompilationBackend;
use leaf_compilation::backends::llvm::{LLVM_Backend, LLVMContext, OptimizationLevel};

use leaf_compilation::frontend::{CompilationUnit};
use leaf_compilation::frontend::compilation_context::{
	CompilationCallbacks, CompilationContext, CompilationProgress,
};
use leaf_compilation::frontend::compilation_context::CompilationConfig;
use leaf_compilation::reflection::{Assembly, Version};
use leaf_compilation::reflection::heaps::{ArenaAllocator, Heaps};
use leaf_compilation::reflection::serialization::{MetadataRead, MetadataWrite};

#[derive(Debug, clap::Parser)]
struct Args {
	#[arg(long = "trace")]
	trace: bool,
	#[arg(short = 'v', long = "verbose")]
	verbose: bool,
	#[arg(long = "no_progress")]
	no_progress: bool,
	#[command(subcommand)]
	command: Command,
}

#[derive(Debug, clap::Subcommand)]
enum Command {
	Build,
}

#[cfg(not(any(feature = "miri_interpret", feature = "miri_compile")))]
fn main() {
	let args = Args::parse();

	let (non_blocking, _guard) = tracing_appender::non_blocking(std::io::stderr());

	let fmt_layer = tracing_subscriber::fmt::layer()
		.compact()
		.with_writer(non_blocking)
		.with_file(false)
		.with_target(false)
		.with_level(true)
		.with_writer(std::io::stderr.with_max_level(match args.verbose {
			true => Level::TRACE,
			false => Level::INFO,
		}));

	let trace = match args.trace {
		false => None,
		true => File::create("./trace.folded").ok(),
	};

	let _guard = match trace {
		None => {
			let registry = Registry::default().with(fmt_layer);
			tracing::subscriber::set_global_default(registry).unwrap();
			None
		},
		Some(file) => {
			let flame_layer = FlameLayer::new(BufWriter::new(file)).with_file_and_line(false);
			let guard = flame_layer.flush_on_drop();
			let registry = Registry::default().with(fmt_layer).with(flame_layer);
			tracing::subscriber::set_global_default(registry).unwrap();
			Some(guard)
		},
	};

	let (progress_sender, progress_thread) = match args.no_progress {
		true => (None, None),
		false => {
			let (progress_sender, progress_receiver) = std::sync::mpsc::channel();
			let progress_sender = Arc::new(progress_sender);

			let progress_thread = std::thread::spawn(move || unsafe {
				let mut progress_bar = ProgressBar::new(0);
				progress_bar.finish_and_clear();

				let mut current = -1;
				let mut progress_i = 0;
				let progress_bar_style = ProgressStyle::with_template(
					"{spinner:.green} {msg} [{elapsed_precise}] [{wide_bar:.cyan/blue}] {pos}/{len}",
				)
					.unwrap()
					.progress_chars("=>-");

				while let Ok(progress) = progress_receiver.recv() {
					let phase: isize = std::mem::transmute(std::mem::discriminant(&progress));
					let count: isize =
						std::mem::transmute(std::mem::discriminant(&CompilationProgress::Finished));
					if current != phase {
						current = phase;
						if !progress_bar.is_finished() {
							progress_bar.finish();
							progress_i = 0;
						}
						progress_bar = ProgressBar::new(0).with_style(progress_bar_style.clone());
						match progress {
							CompilationProgress::ParsingFiles(_, _) => {
								progress_bar.set_message(format!(
									"[{}/{}] Parsing files...",
									current + 1,
									count
								));
							},
							CompilationProgress::DeclaringSymbols(_, _) => {
								progress_bar.set_message(format!(
									"[{}/{}] Declaring symbols...",
									current + 1,
									count
								));
							},
							CompilationProgress::CompilingSymbols(_, _) => {
								progress_bar.set_message(format!(
									"[{}/{}] Compiling symbols...",
									current + 1,
									count
								));
							},
							CompilationProgress::Finished => {},
						}
					}
					match progress {
						CompilationProgress::ParsingFiles(i, len) => {
							progress_bar.set_length(len as u64);
							progress_bar.set_position(i as u64 + 1);
						},
						| CompilationProgress::DeclaringSymbols(i, len)
						| CompilationProgress::CompilingSymbols(i, len) => {
							progress_i = progress_i.max(i);
							progress_bar.set_length(len as u64);
							progress_bar.set_position(progress_i as u64 + 1);
						},
						CompilationProgress::Finished => {},
					}
				}
			});
			(Some(progress_sender), Some(progress_thread))
		},
	};

	match args.command {
		Command::Build => {
			let mut config = std::env::current_dir().unwrap();
			config.push("Leaf.toml");

			let config = std::fs::read_to_string(config).unwrap();
			let config: CompilationConfig = toml::from_str(&config).unwrap();

			let allocator = ArenaAllocator::default();
			let heaps = Heaps::new(&allocator);
			let context = CompilationContext::new_with_callbacks(
				config,
				&heaps,
				CompilationCallbacks {
					progress_events_sink: progress_sender,
				},
			);

			let start = SystemTime::now();
			let result = context.compile();
			if let Some(thread) = progress_thread {
				let _ = thread.join();
			}
			if let Err(err) = result {
				std::io::stderr().lock().write_all(&err).unwrap();
			} else {
				eprintln!("Compilation completed in {:?}", start.elapsed().unwrap())
			}
		},
	}
}
