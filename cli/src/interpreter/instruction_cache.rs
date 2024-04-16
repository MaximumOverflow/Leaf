use std::collections::HashMap;
use std::sync::Arc;

use anyhow::anyhow;

use leaf_compilation::reflection::{Function, Opcode};

#[derive(Default)]
pub struct InstructionCache<'l> {
	functions: HashMap<usize, Arc<[Opcode<'l>]>>,
}

impl<'l> InstructionCache<'l> {
	#[inline(never)]
	pub fn get_instructions(
		&mut self, function: &'l Function<'l>,
	) -> anyhow::Result<Arc<[Opcode<'l>]>> {
		let key = function as *const _ as usize;
		if let Some(opcodes) = self.functions.get(&key) {
			return Ok(opcodes.clone());
		}

		let Some(body) = function.body() else {
			return Err(anyhow!("Function has no body"));
		};

		let mut opcodes = body.opcodes().to_vec();

		let opcodes = self.functions.entry(key).or_insert(Arc::from(opcodes));
		Ok(opcodes.clone())
	}
}
