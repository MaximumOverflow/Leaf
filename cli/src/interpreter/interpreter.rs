use crate::interpreter::instruction_cache::InstructionCache;
use leaf_compilation::reflection::{Function, Opcode};
use crate::interpreter::memory::TypeLayoutCache;
use anyhow::anyhow;
use std::sync::Arc;
use std::rc::Rc;

pub struct Interpreter<'l> {
	layout_cache: Rc<TypeLayoutCache<'l>>,
	instruction_cache: InstructionCache<'l>,
}

impl<'l> Interpreter<'l> {
	pub fn new() -> Self {
		let layout_cache = Rc::new(TypeLayoutCache::default());
		let instruction_cache = InstructionCache::default();
		Self {
			layout_cache,
			instruction_cache,
		}
	}

	#[inline(never)]
	pub fn call(&mut self, function: &'l Function<'l>) -> anyhow::Result<()> {
		todo!()
	}
}
