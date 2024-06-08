use std::collections::HashMap;
use std::error::Error;

use iced_x86::code_asm::CodeAssembler;
use nohash_hasher::BuildNoHashHasher;
use object::write::Object;

use leaf_reflection::Assembly;

use crate::backends::CompilationBackend;

#[allow(non_camel_case_types)]
pub struct X86_64_Backend;

impl CompilationBackend for X86_64_Backend {
	fn compile<'l>(&self, assembly: &'l Assembly<'l>) -> Result<Object, Box<dyn Error>> {
		let mut asm = CodeAssembler::new(64)?;
		let mut text_symbol_map = HashMap::<_, _, BuildNoHashHasher<u64>>::default();

		for func in assembly.functions() {
			let Some(body) = func.body() else {
				continue;
			};

			let ip = asm.instructions().len();
			asm.ret().unwrap();

			let new_ip = asm.instructions().len();
			text_symbol_map.insert(ip, (format!("{}", func.id()), new_ip - ip));
		}

		unimplemented!()
	}
}
