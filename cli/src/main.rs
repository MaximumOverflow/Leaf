#![allow(unused_imports)]

use std::alloc::Layout;
use std::ffi::c_char;
use std::fs::File;
use std::io::{BufWriter, Cursor, Write};
use std::path::PathBuf;
use std::process::Stdio;
use std::time::SystemTime;

use clap::Parser;
use tracing::{debug, info, trace, Level};
use tracing_flame::FlameLayer;
use tracing_subscriber::fmt::writer::MakeWriterExt;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::Registry;
use leaf_compilation::backends::CompilationBackend;
use leaf_compilation::backends::llvm::{LLVM_Backend, LLVMContext, OptimizationLevel};

use leaf_compilation::frontend::{CompilationUnit};
use leaf_compilation::reflection::{Assembly, Version};
use leaf_compilation::reflection::heaps::{ArenaAllocator, Heaps};
use leaf_compilation::reflection::serialization::{MetadataRead, MetadataWrite};

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
	#[arg(short = 'v', long = "verbose")]
	verbose: bool,
}

#[derive(Debug, clap::Args)]
struct InterpretArgs {
	file: PathBuf,
	#[arg(long = "trace")]
	trace: bool,
	#[arg(short = 'v', long = "verbose")]
	verbose: bool,
}

#[cfg(not(any(feature = "miri_interpret", feature = "miri_compile")))]
fn main() {
	let args = Args::parse();

	let (trace, verbose) = match &args {
		Args::Compile(CompileArgs { trace, verbose, .. }) => (*trace, *verbose),
		Args::Interpret(InterpretArgs { trace, verbose, .. }) => (*trace, *verbose),
	};

	let (non_blocking, _guard) = tracing_appender::non_blocking(std::io::stderr());

	let fmt_layer = tracing_subscriber::fmt::layer()
		.compact()
		.with_writer(non_blocking)
		.with_file(false)
		.with_target(false)
		.with_level(true)
		.with_writer(std::io::stderr.with_max_level(match verbose {
			true => Level::TRACE,
			false => Level::INFO,
		}));

	let trace = match trace {
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

	match args {
		Args::Interpret(InterpretArgs { file, .. }) => {
			let time = SystemTime::now();

			let bump = ArenaAllocator::default();
			let heaps = Heaps::new(&bump);

			let mut assembly = Assembly::new("interpreter::tmp", Version::default(), &heaps);
			if let Err(err) = CompilationUnit::compile_file(&mut assembly, &file) {
				return println!("{:#}", err.0);
			}
			let comp_time = time.elapsed().unwrap();

			info!("Compilation time: {:?}", comp_time);
			let Some(_main) = assembly.functions().find(|f| f.name() == "main") else {
				eprintln!("Could not find entry point `main`");
				return;
			};
		},
		Args::Compile(CompileArgs { file, .. }) => {
			let mut time = SystemTime::now();

			let bump = ArenaAllocator::default();
			let heaps = Heaps::new(&bump);
			let mut assembly = Assembly::new("compiler::tmp", Version::default(), &heaps);
			if let Err(err) = CompilationUnit::compile_file(&mut assembly, &file) {
				return println!("{:#}", err.0);
			}
			let mut delta = time.elapsed().unwrap();
			info!("Compilation time: {:?}", delta);

			time = SystemTime::now();
			let mut bytes: Vec<u8> = vec![];
			assembly.write(&mut bytes, ()).unwrap();
			delta = time.elapsed().unwrap();
			info!("Serialization time: {:?}", delta);
			std::fs::write("out.llib", &bytes).unwrap();

			time = SystemTime::now();
			let bump = ArenaAllocator::default();
			let heaps = Heaps::new(&bump);
			let mut cursor = Cursor::new(bytes.as_slice());
			let read_assembly = Assembly::read(&mut cursor, heaps).unwrap();
			delta = time.elapsed().unwrap();
			info!("Deserialization time: {:?}", delta);

			dbg!(&read_assembly);

			let ctx = LLVMContext::create();
			let backend = LLVM_Backend::new(&ctx, OptimizationLevel::Default);
			let module = backend.compile(&read_assembly).unwrap();
			module.print_to_stderr();

			let bitcode = module.write_bitcode_to_memory();

			let mut clang = std::process::Command::new("clang")
				.args(["-x", "ir", "-O3", "-o", "a.out", "-"])
				.stdin(Stdio::piped())
				.stdout(Stdio::piped())
				.spawn()
				.unwrap();

			let mut stdin = clang.stdin.take().unwrap();
			stdin.write_all(bitcode.as_slice()).unwrap();
			drop(stdin);

			let output = clang.wait_with_output().unwrap();
			println!("{}", String::from_utf8(output.stdout).unwrap());
		},
	}
}

#[cfg(feature = "miri_interpret")]
fn main() {
	let fmt_layer = tracing_subscriber::fmt::layer()
		.compact()
		.with_file(false)
		.with_target(false)
		.with_level(true)
		.with_writer(std::io::stderr.with_max_level(Level::TRACE));

	let registry = Registry::default().with(fmt_layer);
	tracing::subscriber::set_global_default(registry).unwrap();

	let bump = ArenaAllocator::default();
	let heaps = Heaps::new(&bump);
	let type_cache = TypeCache::new(&bump);

	let file = include_str!("../../test.leaf");
	let mut assembly = Assembly::new("interpreter::tmp", Version::default(), &heaps);
	if let Err(err) = CompilationUnit::compile_code(&type_cache, &mut assembly, file) {
		return println!("{:#}", err);
	}

	let Some(main) = assembly.functions().find(|f| f.name() == "main") else {
		eprintln!("Could not find entry point 'main'");
		return;
	};

	let mut interpreter = Interpreter::new();
	unsafe {
		interpreter.register_extern_fn("core/test/print", |fmt: *const c_char| {
			let mut len = 0;
			let mut ptr = fmt;
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
			let ptr = std::alloc::alloc(layout);
			trace!("Allocated buffer at {ptr:#?} with {layout:?}");
			ptr as usize
		});
		interpreter.register_extern_fn("core/test/dealloc", |info: (*const u8, usize, usize)| {
			let (ptr, size, align) = info;
			let layout = Layout::from_size_align_unchecked(size, align);
			let ptr = ptr as *mut u8;
			std::alloc::dealloc(ptr, layout);
			trace!("Deallocated buffer at {ptr:#?} with {layout:?}");
		});
	}

	match interpreter.call_as_main(main) {
		Ok(value) => {
			info!("Result: {:#?}", value);
		},
		Err(err) => {
			println!("Error: {}", err);
		},
	};
}

#[cfg(feature = "miri_compile")]
fn main() {
	let fmt_layer = tracing_subscriber::fmt::layer()
		.compact()
		.with_file(false)
		.with_target(false)
		.with_level(true)
		.with_writer(std::io::stderr.with_max_level(Level::TRACE));

	let registry = Registry::default().with(fmt_layer);
	tracing::subscriber::set_global_default(registry).unwrap();

	let bump = ArenaAllocator::default();
	let heaps = Heaps::new(&bump);
	let type_cache = TypeCache::new(&bump);

	let file = include_str!("../../test.leaf");
	let mut assembly = Assembly::new("interpreter::tmp", Version::default(), &heaps);
	if let Err(err) = CompilationUnit::compile_code(&type_cache, &mut assembly, file) {
		return println!("{:#}", err);
	}

	let mut bytes: Vec<u8> = vec![];
	assembly.write(&mut bytes, ()).unwrap();
	info!("Serialization completed");

	let bump = ArenaAllocator::default();
	let heaps = Heaps::new(&bump);
	let type_cache = TypeCache::new(&bump);
	let mut cursor = Cursor::new(bytes.as_slice());
	let read_assembly = Assembly::read(&mut cursor, heaps).unwrap();
	info!("Deserialization completed");

	dbg!(read_assembly);
}
