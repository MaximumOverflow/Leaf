use std::collections::HashMap;
use crate::backends::CompilationBackend;
use iced_x86::code_asm::{CodeAssembler, eax};
use leaf_reflection::Assembly;
use object::write::{Object, Symbol, SymbolSection};
use std::error::Error;
use iced_x86::{BlockEncoder, Instruction, InstructionBlock};
use nohash_hasher::BuildNoHashHasher;
use object::{
	Architecture, BinaryFormat, Endianness, SectionKind, SymbolFlags, SymbolKind, SymbolScope,
};

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

		// let mut object = Object::new(BinaryFormat::Coff, Architecture::X86_64, Endianness::Little);
		// let text_section = object.add_section(bytes, ".text".into(), SectionKind::Text);
		//
		// for (ip, (name, len)) in text_symbol_map {
		// 	object.add_symbol(Symbol {
		// 		name: name.clone().into(),
		// 		value: ip,
		// 		size: len,
		// 		kind: SymbolKind::Text,
		// 		scope: SymbolScope::Linkage,
		// 		weak: false,
		// 		section: SymbolSection::Section(text_section),
		// 		flags: SymbolFlags::None,
		// 	});
		// }
		//
		// Ok(object)
		unimplemented!()
	}
}
