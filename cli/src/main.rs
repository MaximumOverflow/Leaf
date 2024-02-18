mod interpreter;

use std::path::PathBuf;
use clap::Parser;
use leaf_compilation::frontend::CompilationUnit;
use leaf_compilation::reflection::builders::MetadataBuilder;
use leaf_compilation::reflection::structured::assembly::AssemblyBuilder;
use crate::interpreter::interpret;

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
	match args {
		Args::Interpret(InterpretArgs { file }) => {
			let code = std::fs::read_to_string(&file).unwrap();

			let mut assembly_builder = AssemblyBuilder::new();
			CompilationUnit::new(&mut assembly_builder, &code).unwrap();

			// let namespace = {
			// 	let start = code.find("namespace ").unwrap() + 10;
			// 	let end = start + code[start..].find(';').unwrap();
			// 	&code[start..end]
			// };

			// let Some(main) = metadata_builder.get_fn_by_name(namespace, "main") else {
			// 	eprintln!("Could not find entry point 'main'.");
			// 	return;
			// };
			//
			// let value = interpret(&metadata_builder, main).unwrap();
			// println!("Result: {:?}", value);
		},
		Args::Compile(CompileArgs { file, out }) => {
			let code = std::fs::read_to_string(&file).unwrap();

			let mut assembly_builder = AssemblyBuilder::new();
			let unit = match CompilationUnit::new(&mut assembly_builder, &code) {
				Ok(unit) => unit,
				Err(error) => {
					println!("{:?}", error.context("Compilation failed"));
					return;
				}
			};

			let assembly = assembly_builder.build();
			println!("{:#?}", assembly);
			// let out = out.unwrap_or_else(|| {
			// 	let mut path = file.clone();
			// 	path.set_extension("llib");
			// 	path
			// });
			//
			// std::fs::write(out, metadata).unwrap();
		},
	}
}
