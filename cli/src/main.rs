use std::alloc::Layout;
use std::ffi::c_char;
use std::fs::File;
use std::io::BufWriter;
use std::path::PathBuf;
use std::time::SystemTime;

use clap::Parser;
use tracing::{debug, info, Level};
use tracing_flame::FlameLayer;
use tracing_subscriber::fmt::writer::MakeWriterExt;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::Registry;

use leaf_compilation::frontend::{CompilationUnit, TypeCache};
use leaf_compilation::reflection::{Assembly, Version};
use leaf_compilation::reflection::heaps::{Bump, Heaps};

use crate::interpreter::Interpreter;

mod interpreter;

#[derive(Debug, clap::Parser)]
enum Args {
	#[command(short_flag = 'c')]
	Compile(CompileArgs),
	#[command(short_flag = 'i')]
	Interpret(InterpretArgs),
}

#[derive(Debug, clap::Args)]
struct CompileArgs {
	file: PathBuf,
	out: Option<PathBuf>,
	#[arg(long = "trace")]
	trace: bool,
}

#[derive(Debug, clap::Args)]
struct InterpretArgs {
	file: PathBuf,
	#[arg(long = "trace")]
	trace: bool,
}

fn main() {
	let args = Args::parse();

	let trace = match &args {
		Args::Compile(CompileArgs { trace, .. }) => *trace,
		Args::Interpret(InterpretArgs { trace, .. }) => *trace,
	};

	let fmt_layer = tracing_subscriber::fmt::layer()
		.compact()
		.with_file(false)
		.with_target(false)
		.with_level(true)
		.with_writer(std::io::stderr.with_max_level(match trace {
			true => Level::TRACE,
			false => Level::INFO,
		}));

	let trace = match trace {
		false => None,
		true => File::create("./trace.folded").ok(),
	};

	let _flame = match trace {
		None => {
			let registry = Registry::default().with(fmt_layer);
			tracing::subscriber::set_global_default(registry).unwrap();
			None
		},

		Some(file) => {
			let flame_layer = FlameLayer::new(BufWriter::new(file));
			let guard = flame_layer.flush_on_drop();
			let registry = Registry::default().with(fmt_layer).with(flame_layer);
			tracing::subscriber::set_global_default(registry).unwrap();
			Some(guard)
		},
	};

	match args {
		Args::Interpret(InterpretArgs { file, .. }) => {
			let mut time = SystemTime::now();

			let bump = Bump::new();
			let heaps = Heaps::new(&bump);
			let type_cache = TypeCache::new(&bump);

			let mut assembly = Assembly::new("interpreter::tmp", Version::default(), &heaps);
			if let Err(err) = CompilationUnit::compile_file(&type_cache, &mut assembly, &file) {
				return println!("{:#}", err);
			}
			let comp_time = time.elapsed().unwrap();

			// println!("{:#?}", assembly);
			debug!("Compilation time: {:?}", comp_time);

			// let namespace = {
			// 	let start = code.find("namespace ").unwrap() + 10;
			// 	let end = start + code[start..].find(';').unwrap();
			// 	&code[start..end]
			// };

			let Some(main) = assembly.functions().find(|f| f.name() == "main") else {
				eprintln!("Could not find entry point 'main'");
				return;
			};

			time = SystemTime::now();
			let mut interpreter = Interpreter::new();
			unsafe {
				interpreter.register_extern_fn("core/test/print", |fmt: usize| {
					let mut len = 0;
					let mut ptr = fmt as *const c_char;
					while *ptr != 0 {
						len += 1;
						ptr = ptr.add(1);
					}
					let slice = std::slice::from_raw_parts(fmt as *const u8, len);
					let str = std::str::from_utf8(slice).unwrap();
					print!("{}", str);
				});
				interpreter.register_extern_fn("core/test/alloc", |info: [usize; 2]| {
					let [size, align] = info;
					let layout = Layout::from_size_align_unchecked(size, align);
					std::alloc::alloc(layout) as usize
				});
				interpreter.register_extern_fn("core/test/dealloc", |info: [usize; 3]| {
					let [ptr, size, align] = info;
					let layout = Layout::from_size_align_unchecked(size, align);
					std::alloc::dealloc(ptr as *mut u8, layout);
				});
			}

			match interpreter.call_as_main(main) {
				Ok(value) => {
					let interp_time = time.elapsed().unwrap();

					// println!("Stack dump: {:#?}", interpreter.stack());
					info!("Result: {:#?}", value);

					debug!("Interpretation time: {:?}", interp_time);
				},
				Err(err) => {
					// println!("Stack dump: {:#?}", interpreter.stack());
					println!("Error: {}", err);
				},
			};
		},
		Args::Compile(CompileArgs { file, .. }) => {
			let time = SystemTime::now();

			let bump = Bump::new();
			let heaps = Heaps::new(&bump);
			let type_cache = TypeCache::new(&bump);

			let mut assembly = Assembly::new("compiler::tmp", Version::default(), &heaps);
			if let Err(err) = CompilationUnit::compile_file(&type_cache, &mut assembly, &file) {
				return println!("{:#}", err);
			}
			let comp_time = time.elapsed().unwrap();

			println!("Compilation time: {:?}", comp_time);
		},
	}
}
