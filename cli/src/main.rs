#![allow(unused_imports)]

mod progress;

use std::alloc::Layout;
use std::collections::HashMap;
use std::ffi::c_char;
use std::fs::File;
use std::io::{BufWriter, Cursor, Write};
use std::ops::{ControlFlow, Range};
use std::path::PathBuf;
use std::process::Stdio;
use std::sync::Arc;
use std::sync::mpsc::TryRecvError;
use std::time::{Duration, SystemTime};

use clap::Parser;
use indicatif::{ProgressBar, ProgressState, ProgressStyle};
use tracing::{debug, info, trace, Level};
use tracing_flame::FlameLayer;
use tracing_subscriber::fmt::writer::MakeWriterExt;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::Registry;
use leaf_compilation::backends::CompilationBackend;
use leaf_compilation::backends::llvm::{LLVM_Backend, LLVMContext, OptimizationLevel};

use leaf_compilation::frontend::compilation_context::{
	CompilationCallbacks, CompilationContext, CompilationProgress, ReportEventData,
};
use leaf_compilation::frontend::compilation_context::CompilationConfig;
use leaf_compilation::frontend::reports::{ReportData, ReportKind};
use leaf_compilation::reflection::{Assembly, Version};
use leaf_compilation::reflection::heaps::{ArenaAllocator, Heaps};
use leaf_compilation::reflection::serialization::{MetadataRead, MetadataWrite};
use crate::progress::Progress;

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

	let (progress_event_sink, progress) = match args.no_progress {
		true => (None, None),
		false => {
			let (sender, receiver) = std::sync::mpsc::channel();
			let progress = Progress::new(receiver);
			(Some(Arc::new(sender)), Some(progress))
		},
	};

	let (report_sink, reports) = std::sync::mpsc::channel::<ReportEventData>();

	let diagnostics_thread = std::thread::spawn(move || {
		let mut progress = progress;
		let mut errors = vec![];
		loop {
			let mut received = false;
			let mut should_break = false;
			if let Some(progress) = &mut progress {
				match progress.update() {
					ControlFlow::Break(_) => should_break = true,
					ControlFlow::Continue(v) => received = v,
				}
			}

			loop {
				match reports.try_recv() {
					Ok((kind, report)) => {
						received = true;
						match kind {
							ReportKind::Error => errors.extend(report),
							_ => match &progress {
								Some(p) => p.suspend(|| {
									std::io::stderr().lock().write_all(&report).unwrap()
								}),
								None => std::io::stderr().lock().write_all(&report).unwrap(),
							},
						}
					},
					Err(err) => match err {
						TryRecvError::Empty => break,
						TryRecvError::Disconnected => {
							should_break = true;
							break;
						},
					},
				}
			}

			if should_break {
				break;
			}

			if !received {
				std::thread::sleep(Duration::from_millis(100));
			}
		}

		drop(progress);
		std::io::stderr().lock().write_all(&errors).unwrap();
	});

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
					progress_event_sink,
					report_sink: Some(Arc::new(report_sink)),
				},
			);

			let start = SystemTime::now();
			let result = context.compile();

			let elapsed = start.elapsed().unwrap();
			diagnostics_thread.join().unwrap();

			if let Ok(_) = result {
				eprintln!("Compilation completed in {:?}", elapsed);
			}
		},
	}
}
