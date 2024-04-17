use std::ffi::c_char;
use std::path::PathBuf;
use std::time::SystemTime;

use clap::Parser;

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
}

#[derive(Debug, clap::Args)]
struct InterpretArgs {
	file: PathBuf,
}

fn main() {
	let args = Args::parse();

	let logger = tracing_subscriber::fmt()
		.compact()
		.with_file(false)
		.with_target(false)
		.with_level(true)
		.with_max_level(tracing::level_filters::LevelFilter::INFO)
		.finish();

	tracing::subscriber::set_global_default(logger).unwrap();

	match args {
		Args::Interpret(InterpretArgs { file }) => {
			let mut time = SystemTime::now();

			let bump = Bump::new();
			let heaps = Heaps::new(&bump);
			let type_cache = TypeCache::new(&bump);

			let mut assembly = Assembly::new("interpreter::tmp", Version::default(), &heaps);
			if let Err(err) =
				CompilationUnit::compile_file(&type_cache, &mut assembly, &file)
			{
				return println!("{:#}", err);
			}
			let comp_time = time.elapsed().unwrap();

			// println!("{:#?}", assembly);
			println!("Compilation time: {:?}", comp_time);

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
			}

			match interpreter.call_as_main(main) {
				Ok(value) => {
					let interp_time = time.elapsed().unwrap();

					// println!("Stack dump: {:#?}", interpreter.stack());
					println!("Result: {:#?}", value);

					println!("Interpretation time: {:?}", interp_time);
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
			if let Err(err) =
				CompilationUnit::compile_file(&type_cache, &mut assembly, &file)
			{
				return println!("{:#}", err);
			}
			let comp_time = time.elapsed().unwrap();

			println!("Compilation time: {:?}", comp_time);
		},
	}
}
