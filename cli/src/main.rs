mod interpreter;

use std::path::PathBuf;
use std::time::SystemTime;
use clap::Parser;
use leaf_compilation::frontend::CompilationUnit;
use leaf_compilation::reflection::structured::assembly::AssemblyBuilder;
use crate::interpreter::{Interpreter};

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
			let mut time = SystemTime::now();
			let code = std::fs::read_to_string(&file).unwrap();

			let mut assembly_builder = AssemblyBuilder::new();
			if let Err(err) = CompilationUnit::new(&mut assembly_builder, &code) {
				return println!("{:#}", err);
			}

			let assembly = assembly_builder.build();
			let comp_time = time.elapsed().unwrap();

			println!("{:#?}", assembly);
			println!("Compilation time: {:?}", comp_time);

			let namespace = {
				let start = code.find("namespace ").unwrap() + 10;
				let end = start + code[start..].find(';').unwrap();
				&code[start..end]
			};

			let Some(main) = assembly.functions().iter().find(
				|f| f.namespace() == namespace && f.name() == "main"
			) else {
				eprintln!("Could not find entry point 'main'");
				return;
			};

			time = SystemTime::now();
			let mut interpreter = Interpreter::new();

			match interpreter.interpret(main, vec![]) {
				Ok(value) => {
					let interp_time = time.elapsed().unwrap();

					println!("Stack dump: {:#?}", interpreter.stack());
					println!("Result: {:#?}", value);

					println!("Interpretation time: {:?}", interp_time);
				},
				Err(err) => {
					println!("Stack dump: {:#?}", interpreter.stack());
					println!("Error: {}", err);
				},
			};
		},
		Args::Compile(CompileArgs { file, .. }) => {
			let time = SystemTime::now();
			let code = std::fs::read_to_string(&file).unwrap();

			let mut assembly_builder = AssemblyBuilder::new();
			let _ = match CompilationUnit::new(&mut assembly_builder, &code) {
				Ok(unit) => unit,
				Err(error) => {
					println!("{:?}", error.context("Compilation failed"));
					return;
				}
			};

			let assembly = assembly_builder.build();
			let comp_time = time.elapsed().unwrap();

			println!("{:#?}", assembly);
			println!("Compilation time: {:?}", comp_time);
		},
	}
}
